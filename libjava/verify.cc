// verify.cc - verify bytecode

/* Copyright (C) 2001, 2002, 2003, 2004, 2005  Free Software Foundation

   This file is part of libgcj.

This software is copyrighted work licensed under the terms of the
Libgcj License.  Please consult the file "LIBGCJ_LICENSE" for
details.  */

// Written by Tom Tromey <tromey@redhat.com>

// Define VERIFY_DEBUG to enable debugging output.

#include <config.h>

#include <jvm.h>
#include <gcj/cni.h>
#include <java-insns.h>
#include <java-interp.h>

// On Solaris 10/x86, <signal.h> indirectly includes <ia32/sys/reg.h>, which 
// defines PC since g++ predefines __EXTENSIONS__.  Undef here to avoid clash
// with PC member of class _Jv_BytecodeVerifier below.
#undef PC

#ifdef INTERPRETER

#include <java/lang/Class.h>
#include <java/lang/VerifyError.h>
#include <java/lang/Throwable.h>
#include <java/lang/reflect/Modifier.h>
#include <java/lang/StringBuffer.h>

#ifdef VERIFY_DEBUG
#include <stdio.h>
#endif /* VERIFY_DEBUG */


// This is used to mark states which are not scheduled for
// verification.
#define INVALID_STATE ((state *) -1)

static void debug_print (const char *fmt, ...)
  __attribute__ ((format (printf, 1, 2)));

static inline void
debug_print (MAYBE_UNUSED const char *fmt, ...)
{
#ifdef VERIFY_DEBUG
  va_list ap;
  va_start (ap, fmt);
  vfprintf (stderr, fmt, ap);
  va_end (ap);
#endif /* VERIFY_DEBUG */
}

// This started as a fairly ordinary verifier, and for the most part
// it remains so.  It works in the obvious way, by modeling the effect
// of each opcode as it is encountered.  For most opcodes, this is a
// straightforward operation.
//
// This verifier does not do type merging.  It used to, but this
// results in difficulty verifying some relatively simple code
// involving interfaces, and it pushed some verification work into the
// interpreter.
//
// Instead of merging reference types, when we reach a point where two
// flows of control merge, we simply keep the union of reference types
// from each branch.  Then, when we need to verify a fact about a
// reference on the stack (e.g., that it is compatible with the
// argument type of a method), we check to ensure that all possible
// types satisfy the requirement.
//
// Another area this verifier differs from the norm is in its handling
// of subroutines.  The JVM specification has some confusing things to
// say about subroutines.  For instance, it makes claims about not
// allowing subroutines to merge and it rejects recursive subroutines.
// For the most part these are red herrings; we used to try to follow
// these things but they lead to problems.  For example, the notion of
// "being in a subroutine" is not well-defined: is an exception
// handler in a subroutine?  If you never execute the `ret' but
// instead `goto 1' do you remain in the subroutine?
//
// For clarity on what is really required for type safety, read
// "Simple Verification Technique for Complex Java Bytecode
// Subroutines" by Alessandro Coglio.  Among other things this paper
// shows that recursive subroutines are not harmful to type safety.
// We implement something similar to what he proposes.  Note that this
// means that this verifier will accept code that is rejected by some
// other verifiers.
//
// For those not wanting to read the paper, the basic observation is
// that we can maintain split states in subroutines.  We maintain one
// state for each calling `jsr'.  In other words, we re-verify a
// subroutine once for each caller, using the exact types held by the
// callers (as opposed to the old approach of merging types and
// keeping a bitmap registering what did or did not change).  This
// approach lets us continue to verify correctly even when a
// subroutine is exited via `goto' or `athrow' and not `ret'.
//
// In some other areas the JVM specification is (mildly) incorrect,
// so we diverge.  For instance, you cannot
// violate type safety by allocating an object with `new' and then
// failing to initialize it, no matter how one branches or where one
// stores the uninitialized reference.  See "Improving the official
// specification of Java bytecode verification" by Alessandro Coglio.
//
// Note that there's no real point in enforcing that padding bytes or
// the mystery byte of invokeinterface must be 0, but we do that
// regardless.
//
// The verifier is currently neither completely lazy nor eager when it
// comes to loading classes.  It tries to represent types by name when
// possible, and then loads them when it needs to verify a fact about
// the type.  Checking types by name is valid because we only use
// names which come from the current class' constant pool.  Since all
// such names are looked up using the same class loader, there is no
// danger that we might be fooled into comparing different types with
// the same name.
//
// In the future we plan to allow for a completely lazy mode of
// operation, where the verifier will construct a list of type
// assertions to be checked later.
//
// Some test cases for the verifier live in the "verify" module of the
// Mauve test suite.  However, some of these are presently
// (2004-01-20) believed to be incorrect.  (More precisely the notion
// of "correct" is not well-defined, and this verifier differs from
// others while remaining type-safe.)  Some other tests live in the
// libgcj test suite.
class _Jv_BytecodeVerifier
{
private:

  static const int FLAG_INSN_START = 1;
  static const int FLAG_BRANCH_TARGET = 2;

  struct state;
  struct type;
  struct linked_utf8;
  struct ref_intersection;

  template<typename T>
  struct linked
  {
    T *val;
    linked<T> *next;
  };

  // The current PC.
  int PC;
  // The PC corresponding to the start of the current instruction.
  int start_PC;

  // The current state of the stack, locals, etc.
  state *current_state;

  // At each branch target we keep a linked list of all the states we
  // can process at that point.  We'll only have multiple states at a
  // given PC if they both have different return-address types in the
  // same stack or local slot.  This array is indexed by PC and holds
  // the list of all such states.
  linked<state> **states;

  // We keep a linked list of all the states which we must reverify.
  // This is the head of the list.
  state *next_verify_state;

  // We keep some flags for each instruction.  The values are the
  // FLAG_* constants defined above.  This is an array indexed by PC.
  char *flags;

  // The bytecode itself.
  unsigned char *bytecode;
  // The exceptions.
  _Jv_InterpException *exception;

  // Defining class.
  jclass current_class;
  // This method.
  _Jv_InterpMethod *current_method;

  // A linked list of utf8 objects we allocate.
  linked<_Jv_Utf8Const> *utf8_list;

  // A linked list of all ref_intersection objects we allocate.
  ref_intersection *isect_list;

  // Create a new Utf-8 constant and return it.  We do this to avoid
  // having our Utf-8 constants prematurely collected.
  _Jv_Utf8Const *make_utf8_const (char *s, int len)
  {
    linked<_Jv_Utf8Const> *lu = (linked<_Jv_Utf8Const> *)
      _Jv_Malloc (sizeof (linked<_Jv_Utf8Const>)
		  + _Jv_Utf8Const::space_needed(s, len));
    _Jv_Utf8Const *r = (_Jv_Utf8Const *) (lu + 1);
    r->init(s, len);
    lu->val = r;
    lu->next = utf8_list;
    utf8_list = lu;

    return r;
  }

  __attribute__ ((__noreturn__)) void verify_fail (char *s, jint pc = -1)
  {
    using namespace java::lang;
    StringBuffer *buf = new StringBuffer ();

    buf->append (JvNewStringLatin1 ("verification failed"));
    if (pc == -1)
      pc = start_PC;
    if (pc != -1)
      {
	buf->append (JvNewStringLatin1 (" at PC "));
	buf->append (pc);
      }

    _Jv_InterpMethod *method = current_method;
    buf->append (JvNewStringLatin1 (" in "));
    buf->append (current_class->getName());
    buf->append ((jchar) ':');
    buf->append (method->get_method()->name->toString());
    buf->append ((jchar) '(');
    buf->append (method->get_method()->signature->toString());
    buf->append ((jchar) ')');

    buf->append (JvNewStringLatin1 (": "));
    buf->append (JvNewStringLatin1 (s));
    throw new java::lang::VerifyError (buf->toString ());
  }

  // This enum holds a list of tags for all the different types we
  // need to handle.  Reference types are treated specially by the
  // type class.
  enum type_val
  {
    void_type,

    // The values for primitive types are chosen to correspond to values
    // specified to newarray.
    boolean_type = 4,
    char_type = 5,
    float_type = 6,
    double_type = 7,
    byte_type = 8,
    short_type = 9,
    int_type = 10,
    long_type = 11,

    // Used when overwriting second word of a double or long in the
    // local variables.  Also used after merging local variable states
    // to indicate an unusable value.
    unsuitable_type,
    return_address_type,
    // This is the second word of a two-word value, i.e., a double or
    // a long.
    continuation_type,

    // Everything after `reference_type' must be a reference type.
    reference_type,
    null_type,
    uninitialized_reference_type
  };

  // This represents a merged class type.  Some verifiers (including
  // earlier versions of this one) will compute the intersection of
  // two class types when merging states.  However, this loses
  // critical information about interfaces implemented by the various
  // classes.  So instead we keep track of all the actual classes that
  // have been merged.
  struct ref_intersection
  {
    // Whether or not this type has been resolved.
    bool is_resolved;

    // Actual type data.
    union
    {
      // For a resolved reference type, this is a pointer to the class.
      jclass klass;
      // For other reference types, this it the name of the class.
      _Jv_Utf8Const *name;
    } data;

    // Link to the next reference in the intersection.
    ref_intersection *ref_next;

    // This is used to keep track of all the allocated
    // ref_intersection objects, so we can free them.
    // FIXME: we should allocate these in chunks.
    ref_intersection *alloc_next;

    ref_intersection (jclass klass, _Jv_BytecodeVerifier *verifier)
      : ref_next (NULL)
    {
      is_resolved = true;
      data.klass = klass;
      alloc_next = verifier->isect_list;
      verifier->isect_list = this;
    }

    ref_intersection (_Jv_Utf8Const *name, _Jv_BytecodeVerifier *verifier)
      : ref_next (NULL)
    {
      is_resolved = false;
      data.name = name;
      alloc_next = verifier->isect_list;
      verifier->isect_list = this;
    }

    ref_intersection (ref_intersection *dup, ref_intersection *tail,
		      _Jv_BytecodeVerifier *verifier)
      : ref_next (tail)
    {
      is_resolved = dup->is_resolved;
      data = dup->data;
      alloc_next = verifier->isect_list;
      verifier->isect_list = this;
    }

    bool equals (ref_intersection *other, _Jv_BytecodeVerifier *verifier)
    {
      if (! is_resolved && ! other->is_resolved
	  && _Jv_equalUtf8Consts (data.name, other->data.name))
	return true;
      if (! is_resolved)
	resolve (verifier);
      if (! other->is_resolved)
	other->resolve (verifier);
      return data.klass == other->data.klass;
    }

    // Merge THIS type into OTHER, returning the result.  This will
    // return OTHER if all the classes in THIS already appear in
    // OTHER.
    ref_intersection *merge (ref_intersection *other,
			     _Jv_BytecodeVerifier *verifier)
    {
      ref_intersection *tail = other;
      for (ref_intersection *self = this; self != NULL; self = self->ref_next)
	{
	  bool add = true;
	  for (ref_intersection *iter = other; iter != NULL;
	       iter = iter->ref_next)
	    {
	      if (iter->equals (self, verifier))
		{
		  add = false;
		  break;
		}
	    }

	  if (add)
	    tail = new ref_intersection (self, tail, verifier);
	}
      return tail;
    }

    void resolve (_Jv_BytecodeVerifier *verifier)
    {
      if (is_resolved)
	return;

      using namespace java::lang;
      java::lang::ClassLoader *loader
	= verifier->current_class->getClassLoaderInternal();
      // We might see either kind of name.  Sigh.
      if (data.name->first() == 'L' && data.name->limit()[-1] == ';')
	data.klass = _Jv_FindClassFromSignature (data.name->chars(), loader);
      else
	data.klass = Class::forName (_Jv_NewStringUtf8Const (data.name),
				     false, loader);
      is_resolved = true;
    }

    // See if an object of type OTHER can be assigned to an object of
    // type *THIS.  This might resolve classes in one chain or the
    // other.
    bool compatible (ref_intersection *other,
		     _Jv_BytecodeVerifier *verifier)
    {
      ref_intersection *self = this;

      for (; self != NULL; self = self->ref_next)
	{
	  ref_intersection *other_iter = other;

	  for (; other_iter != NULL; other_iter = other_iter->ref_next)
	    {
	      // Avoid resolving if possible.
	      if (! self->is_resolved
		  && ! other_iter->is_resolved
		  && _Jv_equalUtf8Consts (self->data.name,
					  other_iter->data.name))
		continue;

	      if (! self->is_resolved)
		self->resolve(verifier);
	      if (! other_iter->is_resolved)
		other_iter->resolve(verifier);

	      if (! is_assignable_from_slow (self->data.klass,
					     other_iter->data.klass))
		return false;
	    }
	}

      return true;
    }

    bool isarray ()
    {
      // assert (ref_next == NULL);
      if (is_resolved)
	return data.klass->isArray ();
      else
	return data.name->first() == '[';
    }

    bool isinterface (_Jv_BytecodeVerifier *verifier)
    {
      // assert (ref_next == NULL);
      if (! is_resolved)
	resolve (verifier);
      return data.klass->isInterface ();
    }

    bool isabstract (_Jv_BytecodeVerifier *verifier)
    {
      // assert (ref_next == NULL);
      if (! is_resolved)
	resolve (verifier);
      using namespace java::lang::reflect;
      return Modifier::isAbstract (data.klass->getModifiers ());
    }

    jclass getclass (_Jv_BytecodeVerifier *verifier)
    {
      if (! is_resolved)
	resolve (verifier);
      return data.klass;
    }

    int count_dimensions ()
    {
      int ndims = 0;
      if (is_resolved)
	{
	  jclass k = data.klass;
	  while (k->isArray ())
	    {
	      k = k->getComponentType ();
	      ++ndims;
	    }
	}
      else
	{
	  char *p = data.name->chars();
	  while (*p++ == '[')
	    ++ndims;
	}
      return ndims;
    }

    void *operator new (size_t bytes)
    {
      return _Jv_Malloc (bytes);
    }

    void operator delete (void *mem)
    {
      _Jv_Free (mem);
    }
  };

  // Return the type_val corresponding to a primitive signature
  // character.  For instance `I' returns `int.class'.
  type_val get_type_val_for_signature (jchar sig)
  {
    type_val rt;
    switch (sig)
      {
      case 'Z':
	rt = boolean_type;
	break;
      case 'B':
	rt = byte_type;
	break;
      case 'C':
	rt = char_type;
	break;
      case 'S':
	rt = short_type;
	break;
      case 'I':
	rt = int_type;
	break;
      case 'J':
	rt = long_type;
	break;
      case 'F':
	rt = float_type;
	break;
      case 'D':
	rt = double_type;
	break;
      case 'V':
	rt = void_type;
	break;
      default:
	verify_fail ("invalid signature");
      }
    return rt;
  }

  // Return the type_val corresponding to a primitive class.
  type_val get_type_val_for_signature (jclass k)
  {
    return get_type_val_for_signature ((jchar) k->method_count);
  }

  // This is like _Jv_IsAssignableFrom, but it works even if SOURCE or
  // TARGET haven't been prepared.
  static bool is_assignable_from_slow (jclass target, jclass source)
  {
    // First, strip arrays.
    while (target->isArray ())
      {
	// If target is array, source must be as well.
	if (! source->isArray ())
	  return false;
	target = target->getComponentType ();
	source = source->getComponentType ();
      }

    // Quick success.
    if (target == &java::lang::Object::class$)
      return true;

    do
      {
	if (source == target)
	  return true;

	if (target->isPrimitive () || source->isPrimitive ())
	  return false;

	if (target->isInterface ())
	  {
	    for (int i = 0; i < source->interface_count; ++i)
	      {
		// We use a recursive call because we also need to
		// check superinterfaces.
		if (is_assignable_from_slow (target, source->getInterface (i)))
		  return true;
	      }
	  }
	source = source->getSuperclass ();
      }
    while (source != NULL);

    return false;
  }

  // The `type' class is used to represent a single type in the
  // verifier.
  struct type
  {
    // The type key.
    type_val key;

    // For reference types, the representation of the type.
    ref_intersection *klass;

    // This is used in two situations.
    //
    // First, when constructing a new object, it is the PC of the
    // `new' instruction which created the object.  We use the special
    // value UNINIT to mean that this is uninitialized.  The special
    // value SELF is used for the case where the current method is
    // itself the <init> method.  the special value EITHER is used
    // when we may optionally allow either an uninitialized or
    // initialized reference to match.
    //
    // Second, when the key is return_address_type, this holds the PC
    // of the instruction following the `jsr'.
    int pc;

    static const int UNINIT = -2;
    static const int SELF = -1;
    static const int EITHER = -3;

    // Basic constructor.
    type ()
    {
      key = unsuitable_type;
      klass = NULL;
      pc = UNINIT;
    }

    // Make a new instance given the type tag.  We assume a generic
    // `reference_type' means Object.
    type (type_val k)
    {
      key = k;
      // For reference_type, if KLASS==NULL then that means we are
      // looking for a generic object of any kind, including an
      // uninitialized reference.
      klass = NULL;
      pc = UNINIT;
    }

    // Make a new instance given a class.
    type (jclass k, _Jv_BytecodeVerifier *verifier)
    {
      key = reference_type;
      klass = new ref_intersection (k, verifier);
      pc = UNINIT;
    }

    // Make a new instance given the name of a class.
    type (_Jv_Utf8Const *n, _Jv_BytecodeVerifier *verifier)
    {
      key = reference_type;
      klass = new ref_intersection (n, verifier);
      pc = UNINIT;
    }

    // Copy constructor.
    type (const type &t)
    {
      key = t.key;
      klass = t.klass;
      pc = t.pc;
    }

    // These operators are required because libgcj can't link in
    // -lstdc++.
    void *operator new[] (size_t bytes)
    {
      return _Jv_Malloc (bytes);
    }

    void operator delete[] (void *mem)
    {
      _Jv_Free (mem);
    }

    type& operator= (type_val k)
    {
      key = k;
      klass = NULL;
      pc = UNINIT;
      return *this;
    }

    type& operator= (const type& t)
    {
      key = t.key;
      klass = t.klass;
      pc = t.pc;
      return *this;
    }

    // Promote a numeric type.
    type &promote ()
    {
      if (key == boolean_type || key == char_type
	  || key == byte_type || key == short_type)
	key = int_type;
      return *this;
    }

    // Mark this type as the uninitialized result of `new'.
    void set_uninitialized (int npc, _Jv_BytecodeVerifier *verifier)
    {
      if (key == reference_type)
	key = uninitialized_reference_type;
      else
	verifier->verify_fail ("internal error in type::uninitialized");
      pc = npc;
    }

    // Mark this type as now initialized.
    void set_initialized (int npc)
    {
      if (npc != UNINIT && pc == npc && key == uninitialized_reference_type)
	{
	  key = reference_type;
	  pc = UNINIT;
	}
    }

    // Mark this type as a particular return address.
    void set_return_address (int npc)
    {
      pc = npc;
    }

    // Return true if this type and type OTHER are considered
    // mergeable for the purposes of state merging.  This is related
    // to subroutine handling.  For this purpose two types are
    // considered unmergeable if they are both return-addresses but
    // have different PCs.
    bool state_mergeable_p (const type &other) const
    {
      return (key != return_address_type
	      || other.key != return_address_type
	      || pc == other.pc);
    }

    // Return true if an object of type K can be assigned to a variable
    // of type *THIS.  Handle various special cases too.  Might modify
    // *THIS or K.  Note however that this does not perform numeric
    // promotion.
    bool compatible (type &k, _Jv_BytecodeVerifier *verifier)
    {
      // Any type is compatible with the unsuitable type.
      if (key == unsuitable_type)
	return true;

      if (key < reference_type || k.key < reference_type)
	return key == k.key;

      // The `null' type is convertible to any initialized reference
      // type.
      if (key == null_type)
	return k.key != uninitialized_reference_type;
      if (k.key == null_type)
	return key != uninitialized_reference_type;

      // A special case for a generic reference.
      if (klass == NULL)
	return true;
      if (k.klass == NULL)
	verifier->verify_fail ("programmer error in type::compatible");

      // Handle the special 'EITHER' case, which is only used in a
      // special case of 'putfield'.  Note that we only need to handle
      // this on the LHS of a check.
      if (! isinitialized () && pc == EITHER)
	{
	  // If the RHS is uninitialized, it must be an uninitialized
	  // 'this'.
	  if (! k.isinitialized () && k.pc != SELF)
	    return false;
	}
      else if (isinitialized () != k.isinitialized ())
	{
	  // An initialized type and an uninitialized type are not
	  // otherwise compatible.
	  return false;
	}
      else
	{
	  // Two uninitialized objects are compatible if either:
	  // * The PCs are identical, or
	  // * One PC is UNINIT.
	  if (! isinitialized ())
	    {
	      if (pc != k.pc && pc != UNINIT && k.pc != UNINIT)
		return false;
	    }
	}

      return klass->compatible(k.klass, verifier);
    }

    bool equals (const type &other, _Jv_BytecodeVerifier *vfy)
    {
      // Only works for reference types.
      if ((key != reference_type
	   && key != uninitialized_reference_type)
	  || (other.key != reference_type
	      && other.key != uninitialized_reference_type))
	return false;
      // Only for single-valued types.
      if (klass->ref_next || other.klass->ref_next)
	return false;
      return klass->equals (other.klass, vfy);
    }

    bool isvoid () const
    {
      return key == void_type;
    }

    bool iswide () const
    {
      return key == long_type || key == double_type;
    }

    // Return number of stack or local variable slots taken by this
    // type.
    int depth () const
    {
      return iswide () ? 2 : 1;
    }

    bool isarray () const
    {
      // We treat null_type as not an array.  This is ok based on the
      // current uses of this method.
      if (key == reference_type)
	return klass->isarray ();
      return false;
    }

    bool isnull () const
    {
      return key == null_type;
    }

    bool isinterface (_Jv_BytecodeVerifier *verifier)
    {
      if (key != reference_type)
	return false;
      return klass->isinterface (verifier);
    }

    bool isabstract (_Jv_BytecodeVerifier *verifier)
    {
      if (key != reference_type)
	return false;
      return klass->isabstract (verifier);
    }

    // Return the element type of an array.
    type element_type (_Jv_BytecodeVerifier *verifier)
    {
      if (key != reference_type)
	verifier->verify_fail ("programmer error in type::element_type()", -1);

      jclass k = klass->getclass (verifier)->getComponentType ();
      if (k->isPrimitive ())
	return type (verifier->get_type_val_for_signature (k));
      return type (k, verifier);
    }

    // Return the array type corresponding to an initialized
    // reference.  We could expand this to work for other kinds of
    // types, but currently we don't need to.
    type to_array (_Jv_BytecodeVerifier *verifier)
    {
      if (key != reference_type)
	verifier->verify_fail ("internal error in type::to_array()");

      jclass k = klass->getclass (verifier);
      return type (_Jv_GetArrayClass (k, k->getClassLoaderInternal()),
		   verifier);
    }

    bool isreference () const
    {
      return key >= reference_type;
    }

    int get_pc () const
    {
      return pc;
    }

    bool isinitialized () const
    {
      return key == reference_type || key == null_type;
    }

    bool isresolved () const
    {
      return (key == reference_type
	      || key == null_type
	      || key == uninitialized_reference_type);
    }

    void verify_dimensions (int ndims, _Jv_BytecodeVerifier *verifier)
    {
      // The way this is written, we don't need to check isarray().
      if (key != reference_type)
	verifier->verify_fail ("internal error in verify_dimensions:"
			       " not a reference type");

      if (klass->count_dimensions () < ndims)
	verifier->verify_fail ("array type has fewer dimensions"
			       " than required");
    }

    // Merge OLD_TYPE into this.  On error throw exception.  Return
    // true if the merge caused a type change.
    bool merge (type& old_type, bool local_semantics,
		_Jv_BytecodeVerifier *verifier)
    {
      bool changed = false;
      bool refo = old_type.isreference ();
      bool refn = isreference ();
      if (refo && refn)
	{
	  if (old_type.key == null_type)
	    ;
	  else if (key == null_type)
	    {
	      *this = old_type;
	      changed = true;
	    }
	  else if (isinitialized () != old_type.isinitialized ())
	    verifier->verify_fail ("merging initialized and uninitialized types");
	  else
	    {
	      if (! isinitialized ())
		{
		  if (pc == UNINIT)
		    pc = old_type.pc;
		  else if (old_type.pc == UNINIT)
		    ;
		  else if (pc != old_type.pc)
		    verifier->verify_fail ("merging different uninitialized types");
		}

	      ref_intersection *merged = old_type.klass->merge (klass,
								verifier);
	      if (merged != klass)
		{
		  klass = merged;
		  changed = true;
		}
	    }
	}
      else if (refo || refn || key != old_type.key)
	{
	  if (local_semantics)
	    {
	      // If we already have an `unsuitable' type, then we
	      // don't need to change again.
	      if (key != unsuitable_type)
		{
		  key = unsuitable_type;
		  changed = true;
		}
	    }
	  else
	    verifier->verify_fail ("unmergeable type");
	}
      return changed;
    }

#ifdef VERIFY_DEBUG
    void print (void) const
    {
      char c = '?';
      switch (key)
	{
	case boolean_type: c = 'Z'; break;
	case byte_type: c = 'B'; break;
	case char_type: c = 'C'; break;
	case short_type: c = 'S'; break;
	case int_type: c = 'I'; break;
	case long_type: c = 'J'; break;
	case float_type: c = 'F'; break;
	case double_type: c = 'D'; break;
	case void_type: c = 'V'; break;
	case unsuitable_type: c = '-'; break;
	case return_address_type: c = 'r'; break;
	case continuation_type: c = '+'; break;
	case reference_type: c = 'L'; break;
	case null_type: c = '@'; break;
	case uninitialized_reference_type: c = 'U'; break;
	}
      debug_print ("%c", c);
    }
#endif /* VERIFY_DEBUG */
  };

  // This class holds all the state information we need for a given
  // location.
  struct state
  {
    // The current top of the stack, in terms of slots.
    int stacktop;
    // The current depth of the stack.  This will be larger than
    // STACKTOP when wide types are on the stack.
    int stackdepth;
    // The stack.
    type *stack;
    // The local variables.
    type *locals;
    // We keep track of the type of `this' specially.  This is used to
    // ensure that an instance initializer invokes another initializer
    // on `this' before returning.  We must keep track of this
    // specially because otherwise we might be confused by code which
    // assigns to locals[0] (overwriting `this') and then returns
    // without really initializing.
    type this_type;

    // The PC for this state.  This is only valid on states which are
    // permanently attached to a given PC.  For an object like
    // `current_state', which is used transiently, this has no
    // meaning.
    int pc;
    // We keep a linked list of all states requiring reverification.
    // If this is the special value INVALID_STATE then this state is
    // not on the list.  NULL marks the end of the linked list.
    state *next;

    // NO_NEXT is the PC value meaning that a new state must be
    // acquired from the verification list.
    static const int NO_NEXT = -1;

    state ()
      : this_type ()
    {
      stack = NULL;
      locals = NULL;
      next = INVALID_STATE;
    }

    state (int max_stack, int max_locals)
      : this_type ()
    {
      stacktop = 0;
      stackdepth = 0;
      stack = new type[max_stack];
      for (int i = 0; i < max_stack; ++i)
	stack[i] = unsuitable_type;
      locals = new type[max_locals];
      for (int i = 0; i < max_locals; ++i)
	locals[i] = unsuitable_type;
      pc = NO_NEXT;
      next = INVALID_STATE;
    }

    state (const state *orig, int max_stack, int max_locals)
    {
      stack = new type[max_stack];
      locals = new type[max_locals];
      copy (orig, max_stack, max_locals);
      pc = NO_NEXT;
      next = INVALID_STATE;
    }

    ~state ()
    {
      if (stack)
	delete[] stack;
      if (locals)
	delete[] locals;
    }

    void *operator new[] (size_t bytes)
    {
      return _Jv_Malloc (bytes);
    }

    void operator delete[] (void *mem)
    {
      _Jv_Free (mem);
    }

    void *operator new (size_t bytes)
    {
      return _Jv_Malloc (bytes);
    }

    void operator delete (void *mem)
    {
      _Jv_Free (mem);
    }

    void copy (const state *copy, int max_stack, int max_locals)
    {
      stacktop = copy->stacktop;
      stackdepth = copy->stackdepth;
      for (int i = 0; i < max_stack; ++i)
	stack[i] = copy->stack[i];
      for (int i = 0; i < max_locals; ++i)
	locals[i] = copy->locals[i];

      this_type = copy->this_type;
      // Don't modify `next' or `pc'.
    }

    // Modify this state to reflect entry to an exception handler.
    void set_exception (type t, int max_stack)
    {
      stackdepth = 1;
      stacktop = 1;
      stack[0] = t;
      for (int i = stacktop; i < max_stack; ++i)
	stack[i] = unsuitable_type;
    }

    inline int get_pc () const
    {
      return pc;
    }

    void set_pc (int npc)
    {
      pc = npc;
    }

    // Merge STATE_OLD into this state.  Destructively modifies this
    // state.  Returns true if the new state was in fact changed.
    // Will throw an exception if the states are not mergeable.
    bool merge (state *state_old, int max_locals,
		_Jv_BytecodeVerifier *verifier)
    {
      bool changed = false;

      // Special handling for `this'.  If one or the other is
      // uninitialized, then the merge is uninitialized.
      if (this_type.isinitialized ())
	this_type = state_old->this_type;

      // Merge stacks.
      if (state_old->stacktop != stacktop)  // FIXME stackdepth instead?
	verifier->verify_fail ("stack sizes differ");
      for (int i = 0; i < state_old->stacktop; ++i)
	{
	  if (stack[i].merge (state_old->stack[i], false, verifier))
	    changed = true;
	}

      // Merge local variables.
      for (int i = 0; i < max_locals; ++i)
	{
	  if (locals[i].merge (state_old->locals[i], true, verifier))
	    changed = true;
	}

      return changed;
    }

    // Ensure that `this' has been initialized.
    void check_this_initialized (_Jv_BytecodeVerifier *verifier)
    {
      if (this_type.isreference () && ! this_type.isinitialized ())
	verifier->verify_fail ("`this' is uninitialized");
    }

    // Set type of `this'.
    void set_this_type (const type &k)
    {
      this_type = k;
    }

    // Mark each `new'd object we know of that was allocated at PC as
    // initialized.
    void set_initialized (int pc, int max_locals)
    {
      for (int i = 0; i < stacktop; ++i)
	stack[i].set_initialized (pc);
      for (int i = 0; i < max_locals; ++i)
	locals[i].set_initialized (pc);
      this_type.set_initialized (pc);
    }

    // This tests to see whether two states can be considered "merge
    // compatible".  If both states have a return-address in the same
    // slot, and the return addresses are different, then they are not
    // compatible and we must not try to merge them.
    bool state_mergeable_p (state *other, int max_locals,
			    _Jv_BytecodeVerifier *verifier)
    {
      // This is tricky: if the stack sizes differ, then not only are
      // these not mergeable, but in fact we should give an error, as
      // we've found two execution paths that reach a branch target
      // with different stack depths.  FIXME stackdepth instead?
      if (stacktop != other->stacktop)
	verifier->verify_fail ("stack sizes differ");

      for (int i = 0; i < stacktop; ++i)
	if (! stack[i].state_mergeable_p (other->stack[i]))
	  return false;
      for (int i = 0; i < max_locals; ++i)
	if (! locals[i].state_mergeable_p (other->locals[i]))
	  return false;
      return true;
    }

    void reverify (_Jv_BytecodeVerifier *verifier)
    {
      if (next == INVALID_STATE)
	{
	  next = verifier->next_verify_state;
	  verifier->next_verify_state = this;
	}
    }

#ifdef VERIFY_DEBUG
    void print (const char *leader, int pc,
		int max_stack, int max_locals) const
    {
      debug_print ("%s [%4d]:   [stack] ", leader, pc);
      int i;
      for (i = 0; i < stacktop; ++i)
	stack[i].print ();
      for (; i < max_stack; ++i)
	debug_print (".");
      debug_print ("    [local] ");
      for (i = 0; i < max_locals; ++i)
	locals[i].print ();
      debug_print (" | %p\n", this);
    }
#else
    inline void print (const char *, int, int, int) const
    {
    }
#endif /* VERIFY_DEBUG */
  };

  type pop_raw ()
  {
    if (current_state->stacktop <= 0)
      verify_fail ("stack empty");
    type r = current_state->stack[--current_state->stacktop];
    current_state->stackdepth -= r.depth ();
    if (current_state->stackdepth < 0)
      verify_fail ("stack empty", start_PC);
    return r;
  }

  type pop32 ()
  {
    type r = pop_raw ();
    if (r.iswide ())
      verify_fail ("narrow pop of wide type");
    return r;
  }

  type pop_type (type match)
  {
    match.promote ();
    type t = pop_raw ();
    if (! match.compatible (t, this))
      verify_fail ("incompatible type on stack");
    return t;
  }

  // Pop a reference which is guaranteed to be initialized.  MATCH
  // doesn't have to be a reference type; in this case this acts like
  // pop_type.
  type pop_init_ref (type match)
  {
    type t = pop_raw ();
    if (t.isreference () && ! t.isinitialized ())
      verify_fail ("initialized reference required");
    else if (! match.compatible (t, this))
      verify_fail ("incompatible type on stack");
    return t;
  }

  // Pop a reference type or a return address.
  type pop_ref_or_return ()
  {
    type t = pop_raw ();
    if (! t.isreference () && t.key != return_address_type)
      verify_fail ("expected reference or return address on stack");
    return t;
  }

  void push_type (type t)
  {
    // If T is a numeric type like short, promote it to int.
    t.promote ();

    int depth = t.depth ();
    if (current_state->stackdepth + depth > current_method->max_stack)
      verify_fail ("stack overflow");
    current_state->stack[current_state->stacktop++] = t;
    current_state->stackdepth += depth;
  }

  void set_variable (int index, type t)
  {
    // If T is a numeric type like short, promote it to int.
    t.promote ();

    int depth = t.depth ();
    if (index > current_method->max_locals - depth)
      verify_fail ("invalid local variable");
    current_state->locals[index] = t;

    if (depth == 2)
      current_state->locals[index + 1] = continuation_type;
    if (index > 0 && current_state->locals[index - 1].iswide ())
      current_state->locals[index - 1] = unsuitable_type;
  }

  type get_variable (int index, type t)
  {
    int depth = t.depth ();
    if (index > current_method->max_locals - depth)
      verify_fail ("invalid local variable");
    if (! t.compatible (current_state->locals[index], this))
      verify_fail ("incompatible type in local variable");
    if (depth == 2)
      {
	type t (continuation_type);
	if (! current_state->locals[index + 1].compatible (t, this))
	  verify_fail ("invalid local variable");
      }
    return current_state->locals[index];
  }

  // Make sure ARRAY is an array type and that its elements are
  // compatible with type ELEMENT.  Returns the actual element type.
  type require_array_type (type array, type element)
  {
    // An odd case.  Here we just pretend that everything went ok.  If
    // the requested element type is some kind of reference, return
    // the null type instead.
    if (array.isnull ())
      return element.isreference () ? type (null_type) : element;

    if (! array.isarray ())
      verify_fail ("array required");

    type t = array.element_type (this);
    if (! element.compatible (t, this))
      {
	// Special case for byte arrays, which must also be boolean
	// arrays.
	bool ok = true;
	if (element.key == byte_type)
	  {
	    type e2 (boolean_type);
	    ok = e2.compatible (t, this);
	  }
	if (! ok)
	  verify_fail ("incompatible array element type");
      }

    // Return T and not ELEMENT, because T might be specialized.
    return t;
  }

  jint get_byte ()
  {
    if (PC >= current_method->code_length)
      verify_fail ("premature end of bytecode");
    return (jint) bytecode[PC++] & 0xff;
  }

  jint get_ushort ()
  {
    jint b1 = get_byte ();
    jint b2 = get_byte ();
    return (jint) ((b1 << 8) | b2) & 0xffff;
  }

  jint get_short ()
  {
    jint b1 = get_byte ();
    jint b2 = get_byte ();
    jshort s = (b1 << 8) | b2;
    return (jint) s;
  }

  jint get_int ()
  {
    jint b1 = get_byte ();
    jint b2 = get_byte ();
    jint b3 = get_byte ();
    jint b4 = get_byte ();
    return (b1 << 24) | (b2 << 16) | (b3 << 8) | b4;
  }

  int compute_jump (int offset)
  {
    int npc = start_PC + offset;
    if (npc < 0 || npc >= current_method->code_length)
      verify_fail ("branch out of range", start_PC);
    return npc;
  }

  // Add a new state to the state list at NPC.
  state *add_new_state (int npc, state *old_state)
  {
    state *new_state = new state (old_state, current_method->max_stack,
				  current_method->max_locals);
    debug_print ("== New state in add_new_state\n");
    new_state->print ("New", npc, current_method->max_stack,
		      current_method->max_locals);
    linked<state> *nlink
      = (linked<state> *) _Jv_Malloc (sizeof (linked<state>));
    nlink->val = new_state;
    nlink->next = states[npc];
    states[npc] = nlink;
    new_state->set_pc (npc);
    return new_state;
  }

  // Merge the indicated state into the state at the branch target and
  // schedule a new PC if there is a change.  NPC is the PC of the
  // branch target, and FROM_STATE is the state at the source of the
  // branch.  This method returns true if the destination state
  // changed and requires reverification, false otherwise.
  void merge_into (int npc, state *from_state)
  {
    // Iterate over all target states and merge our state into each,
    // if applicable.  FIXME one improvement we could make here is
    // "state destruction".  Merging a new state into an existing one
    // might cause a return_address_type to be merged to
    // unsuitable_type.  In this case the resulting state may now be
    // mergeable with other states currently held in parallel at this
    // location.  So in this situation we could pairwise compare and
    // reduce the number of parallel states.
    bool applicable = false;
    for (linked<state> *iter = states[npc]; iter != NULL; iter = iter->next)
      {
	state *new_state = iter->val;
	if (new_state->state_mergeable_p (from_state,
					  current_method->max_locals, this))
	  {
	    applicable = true;

	    debug_print ("== Merge states in merge_into\n");
	    from_state->print ("Frm", start_PC, current_method->max_stack,
			       current_method->max_locals);
	    new_state->print (" To", npc, current_method->max_stack,
			      current_method->max_locals);
	    bool changed = new_state->merge (from_state,
					     current_method->max_locals,
					     this);
	    new_state->print ("New", npc, current_method->max_stack,
			      current_method->max_locals);

	    if (changed)
	      new_state->reverify (this);
	  }
      }

    if (! applicable)
      {
	// Either we don't yet have a state at NPC, or we have a
	// return-address type that is in conflict with all existing
	// state.  So, we need to create a new entry.
	state *new_state = add_new_state (npc, from_state);
	// A new state added in this way must always be reverified.
	new_state->reverify (this);
      }
  }

  void push_jump (int offset)
  {
    int npc = compute_jump (offset);
    // According to the JVM Spec, we need to check for uninitialized
    // objects here.  However, this does not actually affect type
    // safety, and the Eclipse java compiler generates code that
    // violates this constraint.
    merge_into (npc, current_state);
  }

  void push_exception_jump (type t, int pc)
  {
    // According to the JVM Spec, we need to check for uninitialized
    // objects here.  However, this does not actually affect type
    // safety, and the Eclipse java compiler generates code that
    // violates this constraint.
    state s (current_state, current_method->max_stack,
	     current_method->max_locals);
    if (current_method->max_stack < 1)
      verify_fail ("stack overflow at exception handler");
    s.set_exception (t, current_method->max_stack);
    merge_into (pc, &s);
  }

  state *pop_jump ()
  {
    state *new_state = next_verify_state;
    if (new_state == INVALID_STATE)
      verify_fail ("programmer error in pop_jump");
    if (new_state != NULL)
      {
	next_verify_state = new_state->next;
	new_state->next = INVALID_STATE;
      }
    return new_state;
  }

  void invalidate_pc ()
  {
    PC = state::NO_NEXT;
  }

  void note_branch_target (int pc)
  {
    // Don't check `pc <= PC', because we've advanced PC after
    // fetching the target and we haven't yet checked the next
    // instruction.
    if (pc < PC && ! (flags[pc] & FLAG_INSN_START))
      verify_fail ("branch not to instruction start", start_PC);
    flags[pc] |= FLAG_BRANCH_TARGET;
  }

  void skip_padding ()
  {
    while ((PC % 4) > 0)
      if (get_byte () != 0)
	verify_fail ("found nonzero padding byte");
  }

  // Do the work for a `ret' instruction.  INDEX is the index into the
  // local variables.
  void handle_ret_insn (int index)
  {
    type ret_addr = get_variable (index, return_address_type);
    // It would be nice if we could do this.  However, the JVM Spec
    // doesn't say that this is what happens.  It is implied that
    // reusing a return address is invalid, but there's no actual
    // prohibition against it.
    // set_variable (index, unsuitable_type);

    int npc = ret_addr.get_pc ();
    // We might be returning to a `jsr' that is at the end of the
    // bytecode.  This is ok if we never return from the called
    // subroutine, but if we see this here it is an error.
    if (npc >= current_method->code_length)
      verify_fail ("fell off end");

    // According to the JVM Spec, we need to check for uninitialized
    // objects here.  However, this does not actually affect type
    // safety, and the Eclipse java compiler generates code that
    // violates this constraint.
    merge_into (npc, current_state);
    invalidate_pc ();
  }

  void handle_jsr_insn (int offset)
  {
    int npc = compute_jump (offset);

    // According to the JVM Spec, we need to check for uninitialized
    // objects here.  However, this does not actually affect type
    // safety, and the Eclipse java compiler generates code that
    // violates this constraint.

    // Modify our state as appropriate for entry into a subroutine.
    type ret_addr (return_address_type);
    ret_addr.set_return_address (PC);
    push_type (ret_addr);
    merge_into (npc, current_state);
    invalidate_pc ();
  }

  jclass construct_primitive_array_type (type_val prim)
  {
    jclass k = NULL;
    switch (prim)
      {
      case boolean_type:
	k = JvPrimClass (boolean);
	break;
      case char_type:
	k = JvPrimClass (char);
	break;
      case float_type:
	k = JvPrimClass (float);
	break;
      case double_type:
	k = JvPrimClass (double);
	break;
      case byte_type:
	k = JvPrimClass (byte);
	break;
      case short_type:
	k = JvPrimClass (short);
	break;
      case int_type:
	k = JvPrimClass (int);
	break;
      case long_type:
	k = JvPrimClass (long);
	break;

      // These aren't used here but we call them out to avoid
      // warnings.
      case void_type:
      case unsuitable_type:
      case return_address_type:
      case continuation_type:
      case reference_type:
      case null_type:
      case uninitialized_reference_type:
      default:
	verify_fail ("unknown type in construct_primitive_array_type");
      }
    k = _Jv_GetArrayClass (k, NULL);
    return k;
  }

  // This pass computes the location of branch targets and also
  // instruction starts.
  void branch_prepass ()
  {
    flags = (char *) _Jv_Malloc (current_method->code_length);

    for (int i = 0; i < current_method->code_length; ++i)
      flags[i] = 0;

    PC = 0;
    while (PC < current_method->code_length)
      {
	// Set `start_PC' early so that error checking can have the
	// correct value.
	start_PC = PC;
	flags[PC] |= FLAG_INSN_START;

	java_opcode opcode = (java_opcode) bytecode[PC++];
	switch (opcode)
	  {
	  case op_nop:
	  case op_aconst_null:
	  case op_iconst_m1:
	  case op_iconst_0:
	  case op_iconst_1:
	  case op_iconst_2:
	  case op_iconst_3:
	  case op_iconst_4:
	  case op_iconst_5:
	  case op_lconst_0:
	  case op_lconst_1:
	  case op_fconst_0:
	  case op_fconst_1:
	  case op_fconst_2:
	  case op_dconst_0:
	  case op_dconst_1:
	  case op_iload_0:
	  case op_iload_1:
	  case op_iload_2:
	  case op_iload_3:
	  case op_lload_0:
	  case op_lload_1:
	  case op_lload_2:
	  case op_lload_3:
	  case op_fload_0:
	  case op_fload_1:
	  case op_fload_2:
	  case op_fload_3:
	  case op_dload_0:
	  case op_dload_1:
	  case op_dload_2:
	  case op_dload_3:
	  case op_aload_0:
	  case op_aload_1:
	  case op_aload_2:
	  case op_aload_3:
	  case op_iaload:
	  case op_laload:
	  case op_faload:
	  case op_daload:
	  case op_aaload:
	  case op_baload:
	  case op_caload:
	  case op_saload:
	  case op_istore_0:
	  case op_istore_1:
	  case op_istore_2:
	  case op_istore_3:
	  case op_lstore_0:
	  case op_lstore_1:
	  case op_lstore_2:
	  case op_lstore_3:
	  case op_fstore_0:
	  case op_fstore_1:
	  case op_fstore_2:
	  case op_fstore_3:
	  case op_dstore_0:
	  case op_dstore_1:
	  case op_dstore_2:
	  case op_dstore_3:
	  case op_astore_0:
	  case op_astore_1:
	  case op_astore_2:
	  case op_astore_3:
	  case op_iastore:
	  case op_lastore:
	  case op_fastore:
	  case op_dastore:
	  case op_aastore:
	  case op_bastore:
	  case op_castore:
	  case op_sastore:
	  case op_pop:
	  case op_pop2:
	  case op_dup:
	  case op_dup_x1:
	  case op_dup_x2:
	  case op_dup2:
	  case op_dup2_x1:
	  case op_dup2_x2:
	  case op_swap:
	  case op_iadd:
	  case op_isub:
	  case op_imul:
	  case op_idiv:
	  case op_irem:
	  case op_ishl:
	  case op_ishr:
	  case op_iushr:
	  case op_iand:
	  case op_ior:
	  case op_ixor:
	  case op_ladd:
	  case op_lsub:
	  case op_lmul:
	  case op_ldiv:
	  case op_lrem:
	  case op_lshl:
	  case op_lshr:
	  case op_lushr:
	  case op_land:
	  case op_lor:
	  case op_lxor:
	  case op_fadd:
	  case op_fsub:
	  case op_fmul:
	  case op_fdiv:
	  case op_frem:
	  case op_dadd:
	  case op_dsub:
	  case op_dmul:
	  case op_ddiv:
	  case op_drem:
	  case op_ineg:
	  case op_i2b:
	  case op_i2c:
	  case op_i2s:
	  case op_lneg:
	  case op_fneg:
	  case op_dneg:
	  case op_i2l:
	  case op_i2f:
	  case op_i2d:
	  case op_l2i:
	  case op_l2f:
	  case op_l2d:
	  case op_f2i:
	  case op_f2l:
	  case op_f2d:
	  case op_d2i:
	  case op_d2l:
	  case op_d2f:
	  case op_lcmp:
	  case op_fcmpl:
	  case op_fcmpg:
	  case op_dcmpl:
	  case op_dcmpg:
	  case op_monitorenter:
	  case op_monitorexit:
	  case op_ireturn:
	  case op_lreturn:
	  case op_freturn:
	  case op_dreturn:
	  case op_areturn:
	  case op_return:
	  case op_athrow:
	  case op_arraylength:
	    break;

	  case op_bipush:
	  case op_ldc:
	  case op_iload:
	  case op_lload:
	  case op_fload:
	  case op_dload:
	  case op_aload:
	  case op_istore:
	  case op_lstore:
	  case op_fstore:
	  case op_dstore:
	  case op_astore:
	  case op_ret:
	  case op_newarray:
	    get_byte ();
	    break;

	  case op_iinc:
	  case op_sipush:
	  case op_ldc_w:
	  case op_ldc2_w:
	  case op_getstatic:
	  case op_getfield:
	  case op_putfield:
	  case op_putstatic:
	  case op_new:
	  case op_anewarray:
	  case op_instanceof:
	  case op_checkcast:
	  case op_invokespecial:
	  case op_invokestatic:
	  case op_invokevirtual:
	    get_short ();
	    break;

	  case op_multianewarray:
	    get_short ();
	    get_byte ();
	    break;

	  case op_jsr:
	  case op_ifeq:
	  case op_ifne:
	  case op_iflt:
	  case op_ifge:
	  case op_ifgt:
	  case op_ifle:
	  case op_if_icmpeq:
	  case op_if_icmpne:
	  case op_if_icmplt:
	  case op_if_icmpge:
	  case op_if_icmpgt:
	  case op_if_icmple:
	  case op_if_acmpeq:
	  case op_if_acmpne:
	  case op_ifnull:
	  case op_ifnonnull:
	  case op_goto:
	    note_branch_target (compute_jump (get_short ()));
	    break;

	  case op_tableswitch:
	    {
	      skip_padding ();
	      note_branch_target (compute_jump (get_int ()));
	      jint low = get_int ();
	      jint hi = get_int ();
	      if (low > hi)
		verify_fail ("invalid tableswitch", start_PC);
	      for (int i = low; i <= hi; ++i)
		note_branch_target (compute_jump (get_int ()));
	    }
	    break;

	  case op_lookupswitch:
	    {
	      skip_padding ();
	      note_branch_target (compute_jump (get_int ()));
	      int npairs = get_int ();
	      if (npairs < 0)
		verify_fail ("too few pairs in lookupswitch", start_PC);
	      while (npairs-- > 0)
		{
		  get_int ();
		  note_branch_target (compute_jump (get_int ()));
		}
	    }
	    break;

	  case op_invokeinterface:
	    get_short ();
	    get_byte ();
	    get_byte ();
	    break;

	  case op_wide:
	    {
	      opcode = (java_opcode) get_byte ();
	      get_short ();
	      if (opcode == op_iinc)
		get_short ();
	    }
	    break;

	  case op_jsr_w:
	  case op_goto_w:
	    note_branch_target (compute_jump (get_int ()));
	    break;

	  // These are unused here, but we call them out explicitly
	  // so that -Wswitch-enum doesn't complain.
	  case op_putfield_1:
	  case op_putfield_2:
	  case op_putfield_4:
	  case op_putfield_8:
	  case op_putfield_a:
	  case op_putstatic_1:
	  case op_putstatic_2:
	  case op_putstatic_4:
	  case op_putstatic_8:
	  case op_putstatic_a:
	  case op_getfield_1:
	  case op_getfield_2s:
	  case op_getfield_2u:
	  case op_getfield_4:
	  case op_getfield_8:
	  case op_getfield_a:
	  case op_getstatic_1:
	  case op_getstatic_2s:
	  case op_getstatic_2u:
	  case op_getstatic_4:
	  case op_getstatic_8:
	  case op_getstatic_a:
	  default:
	    verify_fail ("unrecognized instruction in branch_prepass",
			 start_PC);
	  }

	// See if any previous branch tried to branch to the middle of
	// this instruction.
	for (int pc = start_PC + 1; pc < PC; ++pc)
	  {
	    if ((flags[pc] & FLAG_BRANCH_TARGET))
	      verify_fail ("branch to middle of instruction", pc);
	  }
      }

    // Verify exception handlers.
    for (int i = 0; i < current_method->exc_count; ++i)
      {
	if (! (flags[exception[i].handler_pc.i] & FLAG_INSN_START))
	  verify_fail ("exception handler not at instruction start",
		       exception[i].handler_pc.i);
	if (! (flags[exception[i].start_pc.i] & FLAG_INSN_START))
	  verify_fail ("exception start not at instruction start",
		       exception[i].start_pc.i);
	if (exception[i].end_pc.i != current_method->code_length
	    && ! (flags[exception[i].end_pc.i] & FLAG_INSN_START))
	  verify_fail ("exception end not at instruction start",
		       exception[i].end_pc.i);

	flags[exception[i].handler_pc.i] |= FLAG_BRANCH_TARGET;
      }
  }

  void check_pool_index (int index)
  {
    if (index < 0 || index >= current_class->constants.size)
      verify_fail ("constant pool index out of range", start_PC);
  }

  type check_class_constant (int index)
  {
    check_pool_index (index);
    _Jv_Constants *pool = &current_class->constants;
    if (pool->tags[index] == JV_CONSTANT_ResolvedClass)
      return type (pool->data[index].clazz, this);
    else if (pool->tags[index] == JV_CONSTANT_Class)
      return type (pool->data[index].utf8, this);
    verify_fail ("expected class constant", start_PC);
  }

  type check_constant (int index)
  {
    check_pool_index (index);
    _Jv_Constants *pool = &current_class->constants;
    if (pool->tags[index] == JV_CONSTANT_ResolvedString
	|| pool->tags[index] == JV_CONSTANT_String)
      return type (&java::lang::String::class$, this);
    else if (pool->tags[index] == JV_CONSTANT_Integer)
      return type (int_type);
    else if (pool->tags[index] == JV_CONSTANT_Float)
      return type (float_type);
    verify_fail ("String, int, or float constant expected", start_PC);
  }

  type check_wide_constant (int index)
  {
    check_pool_index (index);
    _Jv_Constants *pool = &current_class->constants;
    if (pool->tags[index] == JV_CONSTANT_Long)
      return type (long_type);
    else if (pool->tags[index] == JV_CONSTANT_Double)
      return type (double_type);
    verify_fail ("long or double constant expected", start_PC);
  }

  // Helper for both field and method.  These are laid out the same in
  // the constant pool.
  type handle_field_or_method (int index, int expected,
			       _Jv_Utf8Const **name,
			       _Jv_Utf8Const **fmtype)
  {
    check_pool_index (index);
    _Jv_Constants *pool = &current_class->constants;
    if (pool->tags[index] != expected)
      verify_fail ("didn't see expected constant", start_PC);
    // Once we know we have a Fieldref or Methodref we assume that it
    // is correctly laid out in the constant pool.  I think the code
    // in defineclass.cc guarantees this.
    _Jv_ushort class_index, name_and_type_index;
    _Jv_loadIndexes (&pool->data[index],
		     class_index,
		     name_and_type_index);
    _Jv_ushort name_index, desc_index;
    _Jv_loadIndexes (&pool->data[name_and_type_index],
		     name_index, desc_index);

    *name = pool->data[name_index].utf8;
    *fmtype = pool->data[desc_index].utf8;

    return check_class_constant (class_index);
  }

  // Return field's type, compute class' type if requested.
  // If PUTFIELD is true, use the special 'putfield' semantics.
  type check_field_constant (int index, type *class_type = NULL,
			     bool putfield = false)
  {
    _Jv_Utf8Const *name, *field_type;
    type ct = handle_field_or_method (index,
				      JV_CONSTANT_Fieldref,
				      &name, &field_type);
    if (class_type)
      *class_type = ct;
    type result;
    if (field_type->first() == '[' || field_type->first() == 'L')
      result = type (field_type, this);
    else
      result = get_type_val_for_signature (field_type->first());

    // We have an obscure special case here: we can use `putfield' on
    // a field declared in this class, even if `this' has not yet been
    // initialized.
    if (putfield
	&& ! current_state->this_type.isinitialized ()
	&& current_state->this_type.pc == type::SELF
	&& current_state->this_type.equals (ct, this)
	// We don't look at the signature, figuring that if it is
	// wrong we will fail during linking.  FIXME?
	&& _Jv_Linker::has_field_p (current_class, name))
      // Note that we don't actually know whether we're going to match
      // against 'this' or some other object of the same type.  So,
      // here we set things up so that it doesn't matter.  This relies
      // on knowing what our caller is up to.
      class_type->set_uninitialized (type::EITHER, this);

    return result;
  }

  type check_method_constant (int index, bool is_interface,
			      _Jv_Utf8Const **method_name,
			      _Jv_Utf8Const **method_signature)
  {
    return handle_field_or_method (index,
				   (is_interface
				    ? JV_CONSTANT_InterfaceMethodref
				    : JV_CONSTANT_Methodref),
				   method_name, method_signature);
  }

  type get_one_type (char *&p)
  {
    char *start = p;

    int arraycount = 0;
    while (*p == '[')
      {
	++arraycount;
	++p;
      }

    char v = *p++;

    if (v == 'L')
      {
	while (*p != ';')
	  ++p;
	++p;
	_Jv_Utf8Const *name = make_utf8_const (start, p - start);
	return type (name, this);
      }

    // Casting to jchar here is ok since we are looking at an ASCII
    // character.
    type_val rt = get_type_val_for_signature (jchar (v));

    if (arraycount == 0)
      {
	// Callers of this function eventually push their arguments on
	// the stack.  So, promote them here.
	return type (rt).promote ();
      }

    jclass k = construct_primitive_array_type (rt);
    while (--arraycount > 0)
      k = _Jv_GetArrayClass (k, NULL);
    return type (k, this);
  }

  void compute_argument_types (_Jv_Utf8Const *signature,
			       type *types)
  {
    char *p = signature->chars();

    // Skip `('.
    ++p;

    int i = 0;
    while (*p != ')')
      types[i++] = get_one_type (p);
  }

  type compute_return_type (_Jv_Utf8Const *signature)
  {
    char *p = signature->chars();
    while (*p != ')')
      ++p;
    ++p;
    return get_one_type (p);
  }

  void check_return_type (type onstack)
  {
    type rt = compute_return_type (current_method->self->signature);
    if (! rt.compatible (onstack, this))
      verify_fail ("incompatible return type");
  }

  // Initialize the stack for the new method.  Returns true if this
  // method is an instance initializer.
  bool initialize_stack ()
  {
    int var = 0;
    bool is_init = _Jv_equalUtf8Consts (current_method->self->name,
					gcj::init_name);
    bool is_clinit = _Jv_equalUtf8Consts (current_method->self->name,
					  gcj::clinit_name);

    using namespace java::lang::reflect;
    if (! Modifier::isStatic (current_method->self->accflags))
      {
	type kurr (current_class, this);
	if (is_init)
	  {
	    kurr.set_uninitialized (type::SELF, this);
	    is_init = true;
	  }
	else if (is_clinit)
	  verify_fail ("<clinit> method must be static");
	set_variable (0, kurr);
	current_state->set_this_type (kurr);
	++var;
      }
    else
      {
	if (is_init)
	  verify_fail ("<init> method must be non-static");
      }

    // We have to handle wide arguments specially here.
    int arg_count = _Jv_count_arguments (current_method->self->signature);
    type arg_types[arg_count];
    compute_argument_types (current_method->self->signature, arg_types);
    for (int i = 0; i < arg_count; ++i)
      {
	set_variable (var, arg_types[i]);
	++var;
	if (arg_types[i].iswide ())
	  ++var;
      }

    return is_init;
  }

  void verify_instructions_0 ()
  {
    current_state = new state (current_method->max_stack,
			       current_method->max_locals);

    PC = 0;
    start_PC = 0;

    // True if we are verifying an instance initializer.
    bool this_is_init = initialize_stack ();

    states = (linked<state> **) _Jv_Malloc (sizeof (linked<state> *)
					    * current_method->code_length);
    for (int i = 0; i < current_method->code_length; ++i)
      states[i] = NULL;

    next_verify_state = NULL;

    while (true)
      {
	// If the PC was invalidated, get a new one from the work list.
	if (PC == state::NO_NEXT)
	  {
	    state *new_state = pop_jump ();
	    // If it is null, we're done.
	    if (new_state == NULL)
	      break;

	    PC = new_state->get_pc ();
	    debug_print ("== State pop from pending list\n");
	    // Set up the current state.
	    current_state->copy (new_state, current_method->max_stack,
				 current_method->max_locals);
	  }
	else
	  {
	    // We only have to do this checking in the situation where
	    // control flow falls through from the previous
	    // instruction.  Otherwise merging is done at the time we
	    // push the branch.
	    if (states[PC] != NULL)
	      {
		// We've already visited this instruction.  So merge
		// the states together.  It is simplest, but not most
		// efficient, to just always invalidate the PC here.
		merge_into (PC, current_state);
		invalidate_pc ();
		continue;
	      }
	  }

	// Control can't fall off the end of the bytecode.  We need to
	// check this in both cases, not just the fall-through case,
	// because we don't check to see whether a `jsr' appears at
	// the end of the bytecode until we process a `ret'.
	if (PC >= current_method->code_length)
	  verify_fail ("fell off end");

	// We only have to keep saved state at branch targets.  If
	// we're at a branch target and the state here hasn't been set
	// yet, we set it now.  You might notice that `ret' targets
	// won't necessarily have FLAG_BRANCH_TARGET set.  This
	// doesn't matter, since those states will be filled in by
	// merge_into.
	if (states[PC] == NULL && (flags[PC] & FLAG_BRANCH_TARGET))
	  add_new_state (PC, current_state);

	// Set this before handling exceptions so that debug output is
	// sane.
	start_PC = PC;

	// Update states for all active exception handlers.  Ordinarily
	// there are not many exception handlers.  So we simply run
	// through them all.
	for (int i = 0; i < current_method->exc_count; ++i)
	  {
	    if (PC >= exception[i].start_pc.i && PC < exception[i].end_pc.i)
	      {
		type handler (&java::lang::Throwable::class$, this);
		if (exception[i].handler_type.i != 0)
		  handler = check_class_constant (exception[i].handler_type.i);
		push_exception_jump (handler, exception[i].handler_pc.i);
	      }
	  }

	current_state->print ("   ", PC, current_method->max_stack,
			      current_method->max_locals);
	java_opcode opcode = (java_opcode) bytecode[PC++];
	switch (opcode)
	  {
	  case op_nop:
	    break;

	  case op_aconst_null:
	    push_type (null_type);
	    break;

	  case op_iconst_m1:
	  case op_iconst_0:
	  case op_iconst_1:
	  case op_iconst_2:
	  case op_iconst_3:
	  case op_iconst_4:
	  case op_iconst_5:
	    push_type (int_type);
	    break;

	  case op_lconst_0:
	  case op_lconst_1:
	    push_type (long_type);
	    break;

	  case op_fconst_0:
	  case op_fconst_1:
	  case op_fconst_2:
	    push_type (float_type);
	    break;

	  case op_dconst_0:
	  case op_dconst_1:
	    push_type (double_type);
	    break;

	  case op_bipush:
	    get_byte ();
	    push_type (int_type);
	    break;

	  case op_sipush:
	    get_short ();
	    push_type (int_type);
	    break;

	  case op_ldc:
	    push_type (check_constant (get_byte ()));
	    break;
	  case op_ldc_w:
	    push_type (check_constant (get_ushort ()));
	    break;
	  case op_ldc2_w:
	    push_type (check_wide_constant (get_ushort ()));
	    break;

	  case op_iload:
	    push_type (get_variable (get_byte (), int_type));
	    break;
	  case op_lload:
	    push_type (get_variable (get_byte (), long_type));
	    break;
	  case op_fload:
	    push_type (get_variable (get_byte (), float_type));
	    break;
	  case op_dload:
	    push_type (get_variable (get_byte (), double_type));
	    break;
	  case op_aload:
	    push_type (get_variable (get_byte (), reference_type));
	    break;

	  case op_iload_0:
	  case op_iload_1:
	  case op_iload_2:
	  case op_iload_3:
	    push_type (get_variable (opcode - op_iload_0, int_type));
	    break;
	  case op_lload_0:
	  case op_lload_1:
	  case op_lload_2:
	  case op_lload_3:
	    push_type (get_variable (opcode - op_lload_0, long_type));
	    break;
	  case op_fload_0:
	  case op_fload_1:
	  case op_fload_2:
	  case op_fload_3:
	    push_type (get_variable (opcode - op_fload_0, float_type));
	    break;
	  case op_dload_0:
	  case op_dload_1:
	  case op_dload_2:
	  case op_dload_3:
	    push_type (get_variable (opcode - op_dload_0, double_type));
	    break;
	  case op_aload_0:
	  case op_aload_1:
	  case op_aload_2:
	  case op_aload_3:
	    push_type (get_variable (opcode - op_aload_0, reference_type));
	    break;
	  case op_iaload:
	    pop_type (int_type);
	    push_type (require_array_type (pop_init_ref (reference_type),
					   int_type));
	    break;
	  case op_laload:
	    pop_type (int_type);
	    push_type (require_array_type (pop_init_ref (reference_type),
					   long_type));
	    break;
	  case op_faload:
	    pop_type (int_type);
	    push_type (require_array_type (pop_init_ref (reference_type),
					   float_type));
	    break;
	  case op_daload:
	    pop_type (int_type);
	    push_type (require_array_type (pop_init_ref (reference_type),
					   double_type));
	    break;
	  case op_aaload:
	    pop_type (int_type);
	    push_type (require_array_type (pop_init_ref (reference_type),
					   reference_type));
	    break;
	  case op_baload:
	    pop_type (int_type);
	    require_array_type (pop_init_ref (reference_type), byte_type);
	    push_type (int_type);
	    break;
	  case op_caload:
	    pop_type (int_type);
	    require_array_type (pop_init_ref (reference_type), char_type);
	    push_type (int_type);
	    break;
	  case op_saload:
	    pop_type (int_type);
	    require_array_type (pop_init_ref (reference_type), short_type);
	    push_type (int_type);
	    break;
	  case op_istore:
	    set_variable (get_byte (), pop_type (int_type));
	    break;
	  case op_lstore:
	    set_variable (get_byte (), pop_type (long_type));
	    break;
	  case op_fstore:
	    set_variable (get_byte (), pop_type (float_type));
	    break;
	  case op_dstore:
	    set_variable (get_byte (), pop_type (double_type));
	    break;
	  case op_astore:
	    set_variable (get_byte (), pop_ref_or_return ());
	    break;
	  case op_istore_0:
	  case op_istore_1:
	  case op_istore_2:
	  case op_istore_3:
	    set_variable (opcode - op_istore_0, pop_type (int_type));
	    break;
	  case op_lstore_0:
	  case op_lstore_1:
	  case op_lstore_2:
	  case op_lstore_3:
	    set_variable (opcode - op_lstore_0, pop_type (long_type));
	    break;
	  case op_fstore_0:
	  case op_fstore_1:
	  case op_fstore_2:
	  case op_fstore_3:
	    set_variable (opcode - op_fstore_0, pop_type (float_type));
	    break;
	  case op_dstore_0:
	  case op_dstore_1:
	  case op_dstore_2:
	  case op_dstore_3:
	    set_variable (opcode - op_dstore_0, pop_type (double_type));
	    break;
	  case op_astore_0:
	  case op_astore_1:
	  case op_astore_2:
	  case op_astore_3:
	    set_variable (opcode - op_astore_0, pop_ref_or_return ());
	    break;
	  case op_iastore:
	    pop_type (int_type);
	    pop_type (int_type);
	    require_array_type (pop_init_ref (reference_type), int_type);
	    break;
	  case op_lastore:
	    pop_type (long_type);
	    pop_type (int_type);
	    require_array_type (pop_init_ref (reference_type), long_type);
	    break;
	  case op_fastore:
	    pop_type (float_type);
	    pop_type (int_type);
	    require_array_type (pop_init_ref (reference_type), float_type);
	    break;
	  case op_dastore:
	    pop_type (double_type);
	    pop_type (int_type);
	    require_array_type (pop_init_ref (reference_type), double_type);
	    break;
	  case op_aastore:
	    pop_type (reference_type);
	    pop_type (int_type);
	    require_array_type (pop_init_ref (reference_type), reference_type);
	    break;
	  case op_bastore:
	    pop_type (int_type);
	    pop_type (int_type);
	    require_array_type (pop_init_ref (reference_type), byte_type);
	    break;
	  case op_castore:
	    pop_type (int_type);
	    pop_type (int_type);
	    require_array_type (pop_init_ref (reference_type), char_type);
	    break;
	  case op_sastore:
	    pop_type (int_type);
	    pop_type (int_type);
	    require_array_type (pop_init_ref (reference_type), short_type);
	    break;
	  case op_pop:
	    pop32 ();
	    break;
	  case op_pop2:
	    {
	      type t = pop_raw ();
	      if (! t.iswide ())
		pop32 ();
	    }
	    break;
	  case op_dup:
	    {
	      type t = pop32 ();
	      push_type (t);
	      push_type (t);
	    }
	    break;
	  case op_dup_x1:
	    {
	      type t1 = pop32 ();
	      type t2 = pop32 ();
	      push_type (t1);
	      push_type (t2);
	      push_type (t1);
	    }
	    break;
	  case op_dup_x2:
	    {
	      type t1 = pop32 ();
	      type t2 = pop_raw ();
	      if (! t2.iswide ())
		{
		  type t3 = pop32 ();
		  push_type (t1);
		  push_type (t3);
		}
	      else
		push_type (t1);
	      push_type (t2);
	      push_type (t1);
	    }
	    break;
	  case op_dup2:
	    {
	      type t = pop_raw ();
	      if (! t.iswide ())
		{
		  type t2 = pop32 ();
		  push_type (t2);
		  push_type (t);
		  push_type (t2);
		}
	      else
		push_type (t);
	      push_type (t);
	    }
	    break;
	  case op_dup2_x1:
	    {
	      type t1 = pop_raw ();
	      type t2 = pop32 ();
	      if (! t1.iswide ())
		{
		  type t3 = pop32 ();
		  push_type (t2);
		  push_type (t1);
		  push_type (t3);
		}
	      else
		push_type (t1);
	      push_type (t2);
	      push_type (t1);
	    }
	    break;
	  case op_dup2_x2:
	    {
	      type t1 = pop_raw ();
	      if (t1.iswide ())
		{
		  type t2 = pop_raw ();
		  if (t2.iswide ())
		    {
		      push_type (t1);
		      push_type (t2);
		    }
		  else
		    {
		      type t3 = pop32 ();
		      push_type (t1);
		      push_type (t3);
		      push_type (t2);
		    }
		  push_type (t1);
		}
	      else
		{
		  type t2 = pop32 ();
		  type t3 = pop_raw ();
		  if (t3.iswide ())
		    {
		      push_type (t2);
		      push_type (t1);
		    }
		  else
		    {
		      type t4 = pop32 ();
		      push_type (t2);
		      push_type (t1);
		      push_type (t4);
		    }
		  push_type (t3);
		  push_type (t2);
		  push_type (t1);
		}
	    }
	    break;
	  case op_swap:
	    {
	      type t1 = pop32 ();
	      type t2 = pop32 ();
	      push_type (t1);
	      push_type (t2);
	    }
	    break;
	  case op_iadd:
	  case op_isub:
	  case op_imul:
	  case op_idiv:
	  case op_irem:
	  case op_ishl:
	  case op_ishr:
	  case op_iushr:
	  case op_iand:
	  case op_ior:
	  case op_ixor:
	    pop_type (int_type);
	    push_type (pop_type (int_type));
	    break;
	  case op_ladd:
	  case op_lsub:
	  case op_lmul:
	  case op_ldiv:
	  case op_lrem:
	  case op_land:
	  case op_lor:
	  case op_lxor:
	    pop_type (long_type);
	    push_type (pop_type (long_type));
	    break;
	  case op_lshl:
	  case op_lshr:
	  case op_lushr:
	    pop_type (int_type);
	    push_type (pop_type (long_type));
	    break;
	  case op_fadd:
	  case op_fsub:
	  case op_fmul:
	  case op_fdiv:
	  case op_frem:
	    pop_type (float_type);
	    push_type (pop_type (float_type));
	    break;
	  case op_dadd:
	  case op_dsub:
	  case op_dmul:
	  case op_ddiv:
	  case op_drem:
	    pop_type (double_type);
	    push_type (pop_type (double_type));
	    break;
	  case op_ineg:
	  case op_i2b:
	  case op_i2c:
	  case op_i2s:
	    push_type (pop_type (int_type));
	    break;
	  case op_lneg:
	    push_type (pop_type (long_type));
	    break;
	  case op_fneg:
	    push_type (pop_type (float_type));
	    break;
	  case op_dneg:
	    push_type (pop_type (double_type));
	    break;
	  case op_iinc:
	    get_variable (get_byte (), int_type);
	    get_byte ();
	    break;
	  case op_i2l:
	    pop_type (int_type);
	    push_type (long_type);
	    break;
	  case op_i2f:
	    pop_type (int_type);
	    push_type (float_type);
	    break;
	  case op_i2d:
	    pop_type (int_type);
	    push_type (double_type);
	    break;
	  case op_l2i:
	    pop_type (long_type);
	    push_type (int_type);
	    break;
	  case op_l2f:
	    pop_type (long_type);
	    push_type (float_type);
	    break;
	  case op_l2d:
	    pop_type (long_type);
	    push_type (double_type);
	    break;
	  case op_f2i:
	    pop_type (float_type);
	    push_type (int_type);
	    break;
	  case op_f2l:
	    pop_type (float_type);
	    push_type (long_type);
	    break;
	  case op_f2d:
	    pop_type (float_type);
	    push_type (double_type);
	    break;
	  case op_d2i:
	    pop_type (double_type);
	    push_type (int_type);
	    break;
	  case op_d2l:
	    pop_type (double_type);
	    push_type (long_type);
	    break;
	  case op_d2f:
	    pop_type (double_type);
	    push_type (float_type);
	    break;
	  case op_lcmp:
	    pop_type (long_type);
	    pop_type (long_type);
	    push_type (int_type);
	    break;
	  case op_fcmpl:
	  case op_fcmpg:
	    pop_type (float_type);
	    pop_type (float_type);
	    push_type (int_type);
	    break;
	  case op_dcmpl:
	  case op_dcmpg:
	    pop_type (double_type);
	    pop_type (double_type);
	    push_type (int_type);
	    break;
	  case op_ifeq:
	  case op_ifne:
	  case op_iflt:
	  case op_ifge:
	  case op_ifgt:
	  case op_ifle:
	    pop_type (int_type);
	    push_jump (get_short ());
	    break;
	  case op_if_icmpeq:
	  case op_if_icmpne:
	  case op_if_icmplt:
	  case op_if_icmpge:
	  case op_if_icmpgt:
	  case op_if_icmple:
	    pop_type (int_type);
	    pop_type (int_type);
	    push_jump (get_short ());
	    break;
	  case op_if_acmpeq:
	  case op_if_acmpne:
	    pop_type (reference_type);
	    pop_type (reference_type);
	    push_jump (get_short ());
	    break;
	  case op_goto:
	    push_jump (get_short ());
	    invalidate_pc ();
	    break;
	  case op_jsr:
	    handle_jsr_insn (get_short ());
	    break;
	  case op_ret:
	    handle_ret_insn (get_byte ());
	    break;
	  case op_tableswitch:
	    {
	      pop_type (int_type);
	      skip_padding ();
	      push_jump (get_int ());
	      jint low = get_int ();
	      jint high = get_int ();
	      // Already checked LOW -vs- HIGH.
	      for (int i = low; i <= high; ++i)
		push_jump (get_int ());
	      invalidate_pc ();
	    }
	    break;

	  case op_lookupswitch:
	    {
	      pop_type (int_type);
	      skip_padding ();
	      push_jump (get_int ());
	      jint npairs = get_int ();
	      // Already checked NPAIRS >= 0.
	      jint lastkey = 0;
	      for (int i = 0; i < npairs; ++i)
		{
		  jint key = get_int ();
		  if (i > 0 && key <= lastkey)
		    verify_fail ("lookupswitch pairs unsorted", start_PC);
		  lastkey = key;
		  push_jump (get_int ());
		}
	      invalidate_pc ();
	    }
	    break;
	  case op_ireturn:
	    check_return_type (pop_type (int_type));
	    invalidate_pc ();
	    break;
	  case op_lreturn:
	    check_return_type (pop_type (long_type));
	    invalidate_pc ();
	    break;
	  case op_freturn:
	    check_return_type (pop_type (float_type));
	    invalidate_pc ();
	    break;
	  case op_dreturn:
	    check_return_type (pop_type (double_type));
	    invalidate_pc ();
	    break;
	  case op_areturn:
	    check_return_type (pop_init_ref (reference_type));
	    invalidate_pc ();
	    break;
	  case op_return:
	    // We only need to check this when the return type is
	    // void, because all instance initializers return void.
	    if (this_is_init)
	      current_state->check_this_initialized (this);
	    check_return_type (void_type);
	    invalidate_pc ();
	    break;
	  case op_getstatic:
	    push_type (check_field_constant (get_ushort ()));
	    break;
	  case op_putstatic:
	    pop_type (check_field_constant (get_ushort ()));
	    break;
	  case op_getfield:
	    {
	      type klass;
	      type field = check_field_constant (get_ushort (), &klass);
	      pop_type (klass);
	      push_type (field);
	    }
	    break;
	  case op_putfield:
	    {
	      type klass;
	      type field = check_field_constant (get_ushort (), &klass, true);
	      pop_type (field);
	      pop_type (klass);
	    }
	    break;

	  case op_invokevirtual:
	  case op_invokespecial:
	  case op_invokestatic:
	  case op_invokeinterface:
	    {
	      _Jv_Utf8Const *method_name, *method_signature;
	      type class_type
		= check_method_constant (get_ushort (),
					 opcode == op_invokeinterface,
					 &method_name,
					 &method_signature);
	      // NARGS is only used when we're processing
	      // invokeinterface.  It is simplest for us to compute it
	      // here and then verify it later.
	      int nargs = 0;
	      if (opcode == op_invokeinterface)
		{
		  nargs = get_byte ();
		  if (get_byte () != 0)
		    verify_fail ("invokeinterface dummy byte is wrong");
		}

	      bool is_init = false;
	      if (_Jv_equalUtf8Consts (method_name, gcj::init_name))
		{
		  is_init = true;
		  if (opcode != op_invokespecial)
		    verify_fail ("can't invoke <init>");
		}
	      else if (method_name->first() == '<')
		verify_fail ("can't invoke method starting with `<'");

	      // Pop arguments and check types.
	      int arg_count = _Jv_count_arguments (method_signature);
	      type arg_types[arg_count];
	      compute_argument_types (method_signature, arg_types);
	      for (int i = arg_count - 1; i >= 0; --i)
		{
		  // This is only used for verifying the byte for
		  // invokeinterface.
		  nargs -= arg_types[i].depth ();
		  pop_init_ref (arg_types[i]);
		}

	      if (opcode == op_invokeinterface
		  && nargs != 1)
		verify_fail ("wrong argument count for invokeinterface");

	      if (opcode != op_invokestatic)
		{
		  type t = class_type;
		  if (is_init)
		    {
		      // In this case the PC doesn't matter.
		      t.set_uninitialized (type::UNINIT, this);
		      // FIXME: check to make sure that the <init>
		      // call is to the right class.
		      // It must either be super or an exact class
		      // match.
		    }
		  type raw = pop_raw ();
		  if (! t.compatible (raw, this))
		    verify_fail ("incompatible type on stack");

		  if (is_init)
		    current_state->set_initialized (raw.get_pc (),
						    current_method->max_locals);
		}

	      type rt = compute_return_type (method_signature);
	      if (! rt.isvoid ())
		push_type (rt);
	    }
	    break;

	  case op_new:
	    {
	      type t = check_class_constant (get_ushort ());
	      if (t.isarray () || t.isinterface (this) || t.isabstract (this))
		verify_fail ("type is array, interface, or abstract");
	      t.set_uninitialized (start_PC, this);
	      push_type (t);
	    }
	    break;

	  case op_newarray:
	    {
	      int atype = get_byte ();
	      // We intentionally have chosen constants to make this
	      // valid.
	      if (atype < boolean_type || atype > long_type)
		verify_fail ("type not primitive", start_PC);
	      pop_type (int_type);
	      type t (construct_primitive_array_type (type_val (atype)), this);
	      push_type (t);
	    }
	    break;
	  case op_anewarray:
	    pop_type (int_type);
	    push_type (check_class_constant (get_ushort ()).to_array (this));
	    break;
	  case op_arraylength:
	    {
	      type t = pop_init_ref (reference_type);
	      if (! t.isarray () && ! t.isnull ())
		verify_fail ("array type expected");
	      push_type (int_type);
	    }
	    break;
	  case op_athrow:
	    pop_type (type (&java::lang::Throwable::class$, this));
	    invalidate_pc ();
	    break;
	  case op_checkcast:
	    pop_init_ref (reference_type);
	    push_type (check_class_constant (get_ushort ()));
	    break;
	  case op_instanceof:
	    pop_init_ref (reference_type);
	    check_class_constant (get_ushort ());
	    push_type (int_type);
	    break;
	  case op_monitorenter:
	    pop_init_ref (reference_type);
	    break;
	  case op_monitorexit:
	    pop_init_ref (reference_type);
	    break;
	  case op_wide:
	    {
	      switch (get_byte ())
		{
		case op_iload:
		  push_type (get_variable (get_ushort (), int_type));
		  break;
		case op_lload:
		  push_type (get_variable (get_ushort (), long_type));
		  break;
		case op_fload:
		  push_type (get_variable (get_ushort (), float_type));
		  break;
		case op_dload:
		  push_type (get_variable (get_ushort (), double_type));
		  break;
		case op_aload:
		  push_type (get_variable (get_ushort (), reference_type));
		  break;
		case op_istore:
		  set_variable (get_ushort (), pop_type (int_type));
		  break;
		case op_lstore:
		  set_variable (get_ushort (), pop_type (long_type));
		  break;
		case op_fstore:
		  set_variable (get_ushort (), pop_type (float_type));
		  break;
		case op_dstore:
		  set_variable (get_ushort (), pop_type (double_type));
		  break;
		case op_astore:
		  set_variable (get_ushort (), pop_init_ref (reference_type));
		  break;
		case op_ret:
		  handle_ret_insn (get_short ());
		  break;
		case op_iinc:
		  get_variable (get_ushort (), int_type);
		  get_short ();
		  break;
		default:
		  verify_fail ("unrecognized wide instruction", start_PC);
		}
	    }
	    break;
	  case op_multianewarray:
	    {
	      type atype = check_class_constant (get_ushort ());
	      int dim = get_byte ();
	      if (dim < 1)
		verify_fail ("too few dimensions to multianewarray", start_PC);
	      atype.verify_dimensions (dim, this);
	      for (int i = 0; i < dim; ++i)
		pop_type (int_type);
	      push_type (atype);
	    }
	    break;
	  case op_ifnull:
	  case op_ifnonnull:
	    pop_type (reference_type);
	    push_jump (get_short ());
	    break;
	  case op_goto_w:
	    push_jump (get_int ());
	    invalidate_pc ();
	    break;
	  case op_jsr_w:
	    handle_jsr_insn (get_int ());
	    break;

	  // These are unused here, but we call them out explicitly
	  // so that -Wswitch-enum doesn't complain.
	  case op_putfield_1:
	  case op_putfield_2:
	  case op_putfield_4:
	  case op_putfield_8:
	  case op_putfield_a:
	  case op_putstatic_1:
	  case op_putstatic_2:
	  case op_putstatic_4:
	  case op_putstatic_8:
	  case op_putstatic_a:
	  case op_getfield_1:
	  case op_getfield_2s:
	  case op_getfield_2u:
	  case op_getfield_4:
	  case op_getfield_8:
	  case op_getfield_a:
	  case op_getstatic_1:
	  case op_getstatic_2s:
	  case op_getstatic_2u:
	  case op_getstatic_4:
	  case op_getstatic_8:
	  case op_getstatic_a:
	  default:
	    // Unrecognized opcode.
	    verify_fail ("unrecognized instruction in verify_instructions_0",
			 start_PC);
	  }
      }
  }

public:

  void verify_instructions ()
  {
    branch_prepass ();
    verify_instructions_0 ();
  }

  _Jv_BytecodeVerifier (_Jv_InterpMethod *m)
  {
    // We just print the text as utf-8.  This is just for debugging
    // anyway.
    debug_print ("--------------------------------\n");
    debug_print ("-- Verifying method `%s'\n", m->self->name->chars());

    current_method = m;
    bytecode = m->bytecode ();
    exception = m->exceptions ();
    current_class = m->defining_class;

    states = NULL;
    flags = NULL;
    utf8_list = NULL;
    isect_list = NULL;
  }

  ~_Jv_BytecodeVerifier ()
  {
    if (flags)
      _Jv_Free (flags);

    while (utf8_list != NULL)
      {
	linked<_Jv_Utf8Const> *n = utf8_list->next;
	_Jv_Free (utf8_list);
	utf8_list = n;
      }

    while (isect_list != NULL)
      {
	ref_intersection *next = isect_list->alloc_next;
	delete isect_list;
	isect_list = next;
      }

    if (states)
      {
	for (int i = 0; i < current_method->code_length; ++i)
	  {
	    linked<state> *iter = states[i];
	    while (iter != NULL)
	      {
		linked<state> *next = iter->next;
		delete iter->val;
		_Jv_Free (iter);
		iter = next;
	      }
	  }
	_Jv_Free (states);
      }
  }
};

void
_Jv_VerifyMethod (_Jv_InterpMethod *meth)
{
  _Jv_BytecodeVerifier v (meth);
  v.verify_instructions ();
}

#endif	/* INTERPRETER */
