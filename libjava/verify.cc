// defineclass.cc - defining a class from .class format.

/* Copyright (C) 2001, 2002  Free Software Foundation

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

#ifdef INTERPRETER

#include <java/lang/Class.h>
#include <java/lang/VerifyError.h>
#include <java/lang/Throwable.h>
#include <java/lang/reflect/Modifier.h>
#include <java/lang/StringBuffer.h>

#ifdef VERIFY_DEBUG
#include <stdio.h>
#endif /* VERIFY_DEBUG */


static void debug_print (const char *fmt, ...)
  __attribute__ ((format (printf, 1, 2)));

static inline void
debug_print (const char *fmt, ...)
{
#ifdef VERIFY_DEBUG
  va_list ap;
  va_start (ap, fmt);
  vfprintf (stderr, fmt, ap);
  va_end (ap);
#endif /* VERIFY_DEBUG */
}

class _Jv_BytecodeVerifier
{
private:

  static const int FLAG_INSN_START = 1;
  static const int FLAG_BRANCH_TARGET = 2;

  struct state;
  struct type;
  struct subr_info;
  struct subr_entry_info;
  struct linked_utf8;

  // The current PC.
  int PC;
  // The PC corresponding to the start of the current instruction.
  int start_PC;

  // The current state of the stack, locals, etc.
  state *current_state;

  // We store the state at branch targets, for merging.  This holds
  // such states.
  state **states;

  // We keep a linked list of all the PCs which we must reverify.
  // The link is done using the PC values.  This is the head of the
  // list.
  int next_verify_pc;

  // We keep some flags for each instruction.  The values are the
  // FLAG_* constants defined above.
  char *flags;

  // We need to keep track of which instructions can call a given
  // subroutine.  FIXME: this is inefficient.  We keep a linked list
  // of all calling `jsr's at at each jsr target.
  subr_info **jsr_ptrs;

  // We keep a linked list of entries which map each `ret' instruction
  // to its unique subroutine entry point.  We expect that there won't
  // be many `ret' instructions, so a linked list is ok.
  subr_entry_info *entry_points;

  // The bytecode itself.
  unsigned char *bytecode;
  // The exceptions.
  _Jv_InterpException *exception;

  // Defining class.
  jclass current_class;
  // This method.
  _Jv_InterpMethod *current_method;

  // A linked list of utf8 objects we allocate.  This is really ugly,
  // but without this our utf8 objects would be collected.
  linked_utf8 *utf8_list;

  struct linked_utf8
  {
    _Jv_Utf8Const *val;
    linked_utf8 *next;
  };

  _Jv_Utf8Const *make_utf8_const (char *s, int len)
  {
    _Jv_Utf8Const *val = _Jv_makeUtf8Const (s, len);
    _Jv_Utf8Const *r = (_Jv_Utf8Const *) _Jv_Malloc (sizeof (_Jv_Utf8Const)
						     + val->length
						     + 1);
    r->length = val->length;
    r->hash = val->hash;
    memcpy (r->data, val->data, val->length + 1);

    linked_utf8 *lu = (linked_utf8 *) _Jv_Malloc (sizeof (linked_utf8));
    lu->val = r;
    lu->next = utf8_list;
    utf8_list = lu;

    return r;
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
    continuation_type,

    // There is an obscure special case which requires us to note when
    // a local variable has not been used by a subroutine.  See
    // push_jump_merge for more information.
    unused_by_subroutine_type,

    // Everything after `reference_type' must be a reference type.
    reference_type,
    null_type,
    unresolved_reference_type,
    uninitialized_reference_type,
    uninitialized_unresolved_reference_type
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
    // This will terminate when SOURCE==Object.
    while (true)
      {
	if (source == target)
	  return true;

	if (target->isPrimitive () || source->isPrimitive ())
	  return false;

	if (target->isArray ())
	  {
	    if (! source->isArray ())
	      return false;
	    target = target->getComponentType ();
	    source = source->getComponentType ();
	  }
	else if (target->isInterface ())
	  {
	    for (int i = 0; i < source->interface_count; ++i)
	      {
		// We use a recursive call because we also need to
		// check superinterfaces.
		if (is_assignable_from_slow (target, source->interfaces[i]))
		    return true;
	      }
	    source = source->getSuperclass ();
	    if (source == NULL)
	      return false;
	  }
	// We must do this check before we check to see if SOURCE is
	// an interface.  This way we know that any interface is
	// assignable to an Object.
	else if (target == &java::lang::Object::class$)
	  return true;
	else if (source->isInterface ())
	  {
	    for (int i = 0; i < target->interface_count; ++i)
	      {
		// We use a recursive call because we also need to
		// check superinterfaces.
		if (is_assignable_from_slow (target->interfaces[i], source))
		  return true;
	      }
	    target = target->getSuperclass ();
	    if (target == NULL)
	      return false;
	  }
	else if (source == &java::lang::Object::class$)
	  return false;
	else
	  source = source->getSuperclass ();
      }
  }

  // This is used to keep track of which `jsr's correspond to a given
  // jsr target.
  struct subr_info
  {
    // PC of the instruction just after the jsr.
    int pc;
    // Link.
    subr_info *next;
  };

  // This is used to keep track of which subroutine entry point
  // corresponds to which `ret' instruction.
  struct subr_entry_info
  {
    // PC of the subroutine entry point.
    int pc;
    // PC of the `ret' instruction.
    int ret_pc;
    // Link.
    subr_entry_info *next;
  };

  // The `type' class is used to represent a single type in the
  // verifier.
  struct type
  {
    // The type.
    type_val key;
    // Some associated data.
    union
    {
      // For a resolved reference type, this is a pointer to the class.
      jclass klass;
      // For other reference types, this it the name of the class.
      _Jv_Utf8Const *name;
    } data;
    // This is used when constructing a new object.  It is the PC of the
    // `new' instruction which created the object.  We use the special
    // value -2 to mean that this is uninitialized, and the special
    // value -1 for the case where the current method is itself the
    // <init> method.
    int pc;

    static const int UNINIT = -2;
    static const int SELF = -1;

    // Basic constructor.
    type ()
    {
      key = unsuitable_type;
      data.klass = NULL;
      pc = UNINIT;
    }

    // Make a new instance given the type tag.  We assume a generic
    // `reference_type' means Object.
    type (type_val k)
    {
      key = k;
      data.klass = NULL;
      if (key == reference_type)
	data.klass = &java::lang::Object::class$;
      pc = UNINIT;
    }

    // Make a new instance given a class.
    type (jclass klass)
    {
      key = reference_type;
      data.klass = klass;
      pc = UNINIT;
    }

    // Make a new instance given the name of a class.
    type (_Jv_Utf8Const *n)
    {
      key = unresolved_reference_type;
      data.name = n;
      pc = UNINIT;
    }

    // Copy constructor.
    type (const type &t)
    {
      key = t.key;
      data = t.data;
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
      data.klass = NULL;
      pc = UNINIT;
      return *this;
    }

    type& operator= (const type& t)
    {
      key = t.key;
      data = t.data;
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

    // If *THIS is an unresolved reference type, resolve it.
    void resolve (_Jv_BytecodeVerifier *verifier)
    {
      if (key != unresolved_reference_type
	  && key != uninitialized_unresolved_reference_type)
	return;

      using namespace java::lang;
      java::lang::ClassLoader *loader
	= verifier->current_class->getClassLoader();
      // We might see either kind of name.  Sigh.
      if (data.name->data[0] == 'L'
	  && data.name->data[data.name->length - 1] == ';')
	data.klass = _Jv_FindClassFromSignature (data.name->data, loader);
      else
	data.klass = Class::forName (_Jv_NewStringUtf8Const (data.name),
				     false, loader);
      key = (key == unresolved_reference_type
	     ? reference_type
	     : uninitialized_reference_type);
    }

    // Mark this type as the uninitialized result of `new'.
    void set_uninitialized (int npc, _Jv_BytecodeVerifier *verifier)
    {
      if (key == reference_type)
	key = uninitialized_reference_type;
      else if (key == unresolved_reference_type)
	key = uninitialized_unresolved_reference_type;
      else
	verifier->verify_fail ("internal error in type::uninitialized");
      pc = npc;
    }

    // Mark this type as now initialized.
    void set_initialized (int npc)
    {
      if (npc != UNINIT && pc == npc
	  && (key == uninitialized_reference_type
	      || key == uninitialized_unresolved_reference_type))
	{
	  key = (key == uninitialized_reference_type
		 ? reference_type
		 : unresolved_reference_type);
	  pc = UNINIT;
	}
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

      // The `null' type is convertible to any reference type.
      // FIXME: is this correct for THIS?
      if (key == null_type || k.key == null_type)
	return true;

      // Any reference type is convertible to Object.  This is a special
      // case so we don't need to unnecessarily resolve a class.
      if (key == reference_type
	  && data.klass == &java::lang::Object::class$)
	return true;

      // An initialized type and an uninitialized type are not
      // compatible.
      if (isinitialized () != k.isinitialized ())
	return false;

      // Two uninitialized objects are compatible if either:
      // * The PCs are identical, or
      // * One PC is UNINIT.
      if (! isinitialized ())
	{
	  if (pc != k.pc && pc != UNINIT && k.pc != UNINIT)
	    return false;
	}

      // Two unresolved types are equal if their names are the same.
      if (! isresolved ()
	  && ! k.isresolved ()
	  && _Jv_equalUtf8Consts (data.name, k.data.name))
	return true;

      // We must resolve both types and check assignability.
      resolve (verifier);
      k.resolve (verifier);
      return is_assignable_from_slow (data.klass, k.data.klass);
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
	return data.klass->isArray ();
      else if (key == unresolved_reference_type)
	return data.name->data[0] == '[';
      return false;
    }

    bool isnull () const
    {
      return key == null_type;
    }

    bool isinterface (_Jv_BytecodeVerifier *verifier)
    {
      resolve (verifier);
      if (key != reference_type)
	return false;
      return data.klass->isInterface ();
    }

    bool isabstract (_Jv_BytecodeVerifier *verifier)
    {
      resolve (verifier);
      if (key != reference_type)
	return false;
      using namespace java::lang::reflect;
      return Modifier::isAbstract (data.klass->getModifiers ());
    }

    // Return the element type of an array.
    type element_type (_Jv_BytecodeVerifier *verifier)
    {
      // FIXME: maybe should do string manipulation here.
      resolve (verifier);
      if (key != reference_type)
	verifier->verify_fail ("programmer error in type::element_type()", -1);

      jclass k = data.klass->getComponentType ();
      if (k->isPrimitive ())
	return type (verifier->get_type_val_for_signature (k));
      return type (k);
    }

    // Return the array type corresponding to an initialized
    // reference.  We could expand this to work for other kinds of
    // types, but currently we don't need to.
    type to_array (_Jv_BytecodeVerifier *verifier)
    {
      // Resolving isn't ideal, because it might force us to load
      // another class, but it's easy.  FIXME?
      if (key == unresolved_reference_type)
	resolve (verifier);

      if (key == reference_type)
	return type (_Jv_GetArrayClass (data.klass,
					data.klass->getClassLoader ()));
      else
	verifier->verify_fail ("internal error in type::to_array()");
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
      return (key == reference_type
	      || key == null_type
	      || key == unresolved_reference_type);
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
      if (key == reference_type)
	{
	  jclass k = data.klass;
	  while (k->isArray () && ndims > 0)
	    {
	      k = k->getComponentType ();
	      --ndims;
	    }
	}
      else
	{
	  // We know KEY == unresolved_reference_type.
	  char *p = data.name->data;
	  while (*p++ == '[' && ndims-- > 0)
	    ;
	}

      if (ndims > 0)
	verifier->verify_fail ("array type has fewer dimensions than required");
    }

    // Merge OLD_TYPE into this.  On error throw exception.
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

	      if (! isresolved ()
		  && ! old_type.isresolved ()
		  && _Jv_equalUtf8Consts (data.name, old_type.data.name))
		{
		  // Types are identical.
		}
	      else
		{
		  resolve (verifier);
		  old_type.resolve (verifier);

		  jclass k = data.klass;
		  jclass oldk = old_type.data.klass;

		  int arraycount = 0;
		  while (k->isArray () && oldk->isArray ())
		    {
		      ++arraycount;
		      k = k->getComponentType ();
		      oldk = oldk->getComponentType ();
		    }

		  // Ordinarily this terminates when we hit Object...
		  while (k != NULL)
		    {
		      if (is_assignable_from_slow (k, oldk))
			break;
		      k = k->getSuperclass ();
		      changed = true;
		    }
		  // ... but K could have been an interface, in which
		  // case we'll end up here.  We just convert this
		  // into Object.
		  if (k == NULL)
		    k = &java::lang::Object::class$;

		  if (changed)
		    {
		      while (arraycount > 0)
			{
			  java::lang::ClassLoader *loader
			    = verifier->current_class->getClassLoader();
			  k = _Jv_GetArrayClass (k, loader);
			  --arraycount;
			}
		      data.klass = k;
		    }
		}
	    }
	}
      else if (refo || refn || key != old_type.key)
	{
	  if (local_semantics)
	    {
	      // If we're merging into an "unused" slot, then we
	      // simply accept whatever we're merging from.
	      if (key == unused_by_subroutine_type)
		{
		  *this = old_type;
		  changed = true;
		}
	      else if (old_type.key == unused_by_subroutine_type)
		{
		  // Do nothing.
		}
	      // If we already have an `unsuitable' type, then we
	      // don't need to change again.
	      else if (key != unsuitable_type)
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
	case unused_by_subroutine_type: c = '_'; break;
	case reference_type: c = 'L'; break;
	case null_type: c = '@'; break;
	case unresolved_reference_type: c = 'l'; break;
	case uninitialized_reference_type: c = 'U'; break;
	case uninitialized_unresolved_reference_type: c = 'u'; break;
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
    // This is used in subroutines to keep track of which local
    // variables have been accessed.
    bool *local_changed;
    // If not 0, then we are in a subroutine.  The value is the PC of
    // the subroutine's entry point.  We can use 0 as an exceptional
    // value because PC=0 can never be a subroutine.
    int subroutine;
    // This is used to keep a linked list of all the states which
    // require re-verification.  We use the PC to keep track.
    int next;
    // We keep track of the type of `this' specially.  This is used to
    // ensure that an instance initializer invokes another initializer
    // on `this' before returning.  We must keep track of this
    // specially because otherwise we might be confused by code which
    // assigns to locals[0] (overwriting `this') and then returns
    // without really initializing.
    type this_type;

    // INVALID marks a state which is not on the linked list of states
    // requiring reverification.
    static const int INVALID = -1;
    // NO_NEXT marks the state at the end of the reverification list.
    static const int NO_NEXT = -2;

    // This is used to mark the stack depth at the instruction just
    // after a `jsr' when we haven't yet processed the corresponding
    // `ret'.  See handle_jsr_insn for more information.
    static const int NO_STACK = -1;

    state ()
      : this_type ()
    {
      stack = NULL;
      locals = NULL;
      local_changed = NULL;
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
      local_changed = (bool *) _Jv_Malloc (sizeof (bool) * max_locals);
      for (int i = 0; i < max_locals; ++i)
	{
	  locals[i] = unsuitable_type;
	  local_changed[i] = false;
	}
      next = INVALID;
      subroutine = 0;
    }

    state (const state *orig, int max_stack, int max_locals,
	   bool ret_semantics = false)
    {
      stack = new type[max_stack];
      locals = new type[max_locals];
      local_changed = (bool *) _Jv_Malloc (sizeof (bool) * max_locals);
      copy (orig, max_stack, max_locals, ret_semantics);
      next = INVALID;
    }

    ~state ()
    {
      if (stack)
	delete[] stack;
      if (locals)
	delete[] locals;
      if (local_changed)
	_Jv_Free (local_changed);
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

    void copy (const state *copy, int max_stack, int max_locals,
	       bool ret_semantics = false)
    {
      stacktop = copy->stacktop;
      stackdepth = copy->stackdepth;
      subroutine = copy->subroutine;
      for (int i = 0; i < max_stack; ++i)
	stack[i] = copy->stack[i];
      for (int i = 0; i < max_locals; ++i)
	{
	  // See push_jump_merge to understand this case.
	  if (ret_semantics)
	    locals[i] = type (copy->local_changed[i]
			      ? unsuitable_type
			      : unused_by_subroutine_type);
	  else
	    locals[i] = copy->locals[i];
	  local_changed[i] = copy->local_changed[i];
	}
      this_type = copy->this_type;
      // Don't modify `next'.
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

    // Modify this state to reflect entry into a subroutine.
    void enter_subroutine (int npc, int max_locals)
    {
      subroutine = npc;
      // Mark all items as unchanged.  Each subroutine needs to keep
      // track of its `changed' state independently.  In the case of
      // nested subroutines, this information will be merged back into
      // parent by the `ret'.
      for (int i = 0; i < max_locals; ++i)
	local_changed[i] = false;
    }

    // Merge STATE_OLD into this state.  Destructively modifies this
    // state.  Returns true if the new state was in fact changed.
    // Will throw an exception if the states are not mergeable.
    bool merge (state *state_old, bool ret_semantics,
		int max_locals, _Jv_BytecodeVerifier *verifier)
    {
      bool changed = false;

      // Special handling for `this'.  If one or the other is
      // uninitialized, then the merge is uninitialized.
      if (this_type.isinitialized ())
	this_type = state_old->this_type;

      // Merge subroutine states.  Here we just keep track of what
      // subroutine we think we're in.  We only check for a merge
      // (which is invalid) when we see a `ret'.
      if (subroutine == state_old->subroutine)
	{
	  // Nothing.
	}
      else if (subroutine == 0)
	{
	  subroutine = state_old->subroutine;
	  changed = true;
	}
      else
	{
	  // If the subroutines differ, indicate that the state
	  // changed.  This is needed to detect when subroutines have
	  // merged.
	  changed = true;
	}

      // Merge stacks.  Special handling for NO_STACK case.
      if (state_old->stacktop == NO_STACK)
	{
	  // Nothing to do in this case; we don't care about modifying
	  // the old state.
	}
      else if (stacktop == NO_STACK)
	{
	  stacktop = state_old->stacktop;
	  stackdepth = state_old->stackdepth;
	  for (int i = 0; i < stacktop; ++i)
	    stack[i] = state_old->stack[i];
	  changed = true;
	}
      else if (state_old->stacktop != stacktop)
	verifier->verify_fail ("stack sizes differ");
      else
	{
	  for (int i = 0; i < state_old->stacktop; ++i)
	    {
	      if (stack[i].merge (state_old->stack[i], false, verifier))
		changed = true;
	    }
	}

      // Merge local variables.
      for (int i = 0; i < max_locals; ++i)
	{
	  // If we're not processing a `ret', then we merge every
	  // local variable.  If we are processing a `ret', then we
	  // only merge locals which changed in the subroutine.  When
	  // processing a `ret', STATE_OLD is the state at the point
	  // of the `ret', and THIS is the state just after the `jsr'.
	  if (! ret_semantics || state_old->local_changed[i])
	    {
	      if (locals[i].merge (state_old->locals[i], true, verifier))
		{
		  // Note that we don't call `note_variable' here.
		  // This change doesn't represent a real change to a
		  // local, but rather a merge artifact.  If we're in
		  // a subroutine which is called with two
		  // incompatible types in a slot that is unused by
		  // the subroutine, then we don't want to mark that
		  // variable as having been modified.
		  changed = true;
		}
	    }

	  // If we're in a subroutine, we must compute the union of
	  // all the changed local variables.
	  if (state_old->local_changed[i])
	    note_variable (i);
	}

      return changed;
    }

    // Throw an exception if there is an uninitialized object on the
    // stack or in a local variable.  EXCEPTION_SEMANTICS controls
    // whether we're using backwards-branch or exception-handing
    // semantics.
    void check_no_uninitialized_objects (int max_locals,
					 _Jv_BytecodeVerifier *verifier,
					 bool exception_semantics = false)
    {
      if (! exception_semantics)
	{
	  for (int i = 0; i < stacktop; ++i)
	    if (stack[i].isreference () && ! stack[i].isinitialized ())
	      verifier->verify_fail ("uninitialized object on stack");
	}

      for (int i = 0; i < max_locals; ++i)
	if (locals[i].isreference () && ! locals[i].isinitialized ())
	  verifier->verify_fail ("uninitialized object in local variable");

      check_this_initialized (verifier);
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

    // Note that a local variable was modified.
    void note_variable (int index)
    {
      if (subroutine > 0)
	local_changed[index] = true;
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

    // Return true if this state is the unmerged result of a `ret'.
    bool is_unmerged_ret_state (int max_locals) const
    {
      if (stacktop == NO_STACK)
	return true;
      for (int i = 0; i < max_locals; ++i)
	{
	  if (locals[i].key == unused_by_subroutine_type)
	    return true;
	}
      return false;
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
	{
	  locals[i].print ();
	  debug_print (local_changed[i] ? "+" : " ");
	}
      if (subroutine == 0)
	debug_print ("   | None");
      else
	debug_print ("   | %4d", subroutine);
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

  type pop64 ()
  {
    type r = pop_raw ();
    if (! r.iswide ())
      verify_fail ("wide pop of narrow type");
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
    current_state->note_variable (index);

    if (depth == 2)
      {
	current_state->locals[index + 1] = continuation_type;
	current_state->note_variable (index + 1);
      }
    if (index > 0 && current_state->locals[index - 1].iswide ())
      {
	current_state->locals[index - 1] = unsuitable_type;
	// There's no need to call note_variable here.
      }
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

  // Merge the indicated state into the state at the branch target and
  // schedule a new PC if there is a change.  If RET_SEMANTICS is
  // true, then we are merging from a `ret' instruction into the
  // instruction after a `jsr'.  This is a special case with its own
  // modified semantics.
  void push_jump_merge (int npc, state *nstate, bool ret_semantics = false)
  {
    bool changed = true;
    if (states[npc] == NULL)
      {
	// There's a weird situation here.  If are examining the
	// branch that results from a `ret', and there is not yet a
	// state available at the branch target (the instruction just
	// after the `jsr'), then we have to construct a special kind
	// of state at that point for future merging.  This special
	// state has the type `unused_by_subroutine_type' in each slot
	// which was not modified by the subroutine.
	states[npc] = new state (nstate, current_method->max_stack,
				 current_method->max_locals, ret_semantics);
	debug_print ("== New state in push_jump_merge\n");
	states[npc]->print ("New", npc, current_method->max_stack,
			    current_method->max_locals);
      }
    else
      {
	debug_print ("== Merge states in push_jump_merge\n");
	nstate->print ("Frm", start_PC, current_method->max_stack,
		       current_method->max_locals);
	states[npc]->print (" To", npc, current_method->max_stack,
			    current_method->max_locals);
	changed = states[npc]->merge (nstate, ret_semantics,
				      current_method->max_locals, this);
	states[npc]->print ("New", npc, current_method->max_stack,
			    current_method->max_locals);
      }

    if (changed && states[npc]->next == state::INVALID)
      {
	// The merge changed the state, and the new PC isn't yet on our
	// list of PCs to re-verify.
	states[npc]->next = next_verify_pc;
	next_verify_pc = npc;
      }
  }

  void push_jump (int offset)
  {
    int npc = compute_jump (offset);
    if (npc < PC)
      current_state->check_no_uninitialized_objects (current_method->max_locals, this);
    push_jump_merge (npc, current_state);
  }

  void push_exception_jump (type t, int pc)
  {
    current_state->check_no_uninitialized_objects (current_method->max_locals,
						   this, true);
    state s (current_state, current_method->max_stack,
	     current_method->max_locals);
    if (current_method->max_stack < 1)
      verify_fail ("stack overflow at exception handler");
    s.set_exception (t, current_method->max_stack);
    push_jump_merge (pc, &s);
  }

  int pop_jump ()
  {
    int *prev_loc = &next_verify_pc;
    int npc = next_verify_pc;
    bool skipped = false;

    while (npc != state::NO_NEXT)
      {
	// If the next available PC is an unmerged `ret' state, then
	// we aren't yet ready to handle it.  That's because we would
	// need all kind of special cases to do so.  So instead we
	// defer this jump until after we've processed it via a
	// fall-through.  This has to happen because the instruction
	// before this one must be a `jsr'.
	if (! states[npc]->is_unmerged_ret_state (current_method->max_locals))
	  {
	    *prev_loc = states[npc]->next;
	    states[npc]->next = state::INVALID;
	    return npc;
	  }

	skipped = true;
	prev_loc = &states[npc]->next;
	npc = states[npc]->next;
      }

    // Note that we might have gotten here even when there are
    // remaining states to process.  That can happen if we find a
    // `jsr' without a `ret'.
    return state::NO_NEXT;
  }

  void invalidate_pc ()
  {
    PC = state::NO_NEXT;
  }

  void note_branch_target (int pc, bool is_jsr_target = false)
  {
    // Don't check `pc <= PC', because we've advanced PC after
    // fetching the target and we haven't yet checked the next
    // instruction.
    if (pc < PC && ! (flags[pc] & FLAG_INSN_START))
      verify_fail ("branch not to instruction start", start_PC);
    flags[pc] |= FLAG_BRANCH_TARGET;
    if (is_jsr_target)
      {
	// Record the jsr which called this instruction.
	subr_info *info = (subr_info *) _Jv_Malloc (sizeof (subr_info));
	info->pc = PC;
	info->next = jsr_ptrs[pc];
	jsr_ptrs[pc] = info;
      }
  }

  void skip_padding ()
  {
    while ((PC % 4) > 0)
      if (get_byte () != 0)
	verify_fail ("found nonzero padding byte");
  }

  // Return the subroutine to which the instruction at PC belongs.
  int get_subroutine (int pc)
  {
    if (states[pc] == NULL)
      return 0;
    return states[pc]->subroutine;
  }

  // Do the work for a `ret' instruction.  INDEX is the index into the
  // local variables.
  void handle_ret_insn (int index)
  {
    get_variable (index, return_address_type);

    int csub = current_state->subroutine;
    if (csub == 0)
      verify_fail ("no subroutine");

    // Check to see if we've merged subroutines.
    subr_entry_info *entry;
    for (entry = entry_points; entry != NULL; entry = entry->next)
      {
	if (entry->ret_pc == start_PC)
	  break;
      }
    if (entry == NULL)
      {
	entry = (subr_entry_info *) _Jv_Malloc (sizeof (subr_entry_info));
	entry->pc = csub;
	entry->ret_pc = start_PC;
	entry->next = entry_points;
	entry_points = entry;
      }
    else if (entry->pc != csub)
      verify_fail ("subroutines merged");

    for (subr_info *subr = jsr_ptrs[csub]; subr != NULL; subr = subr->next)
      {
	// Temporarily modify the current state so it looks like we're
	// in the enclosing context.
	current_state->subroutine = get_subroutine (subr->pc);
	if (subr->pc < PC)
	  current_state->check_no_uninitialized_objects (current_method->max_locals, this);
	push_jump_merge (subr->pc, current_state, true);
      }

    current_state->subroutine = csub;
    invalidate_pc ();
  }

  // We're in the subroutine SUB, calling a subroutine at DEST.  Make
  // sure this subroutine isn't already on the stack.
  void check_nonrecursive_call (int sub, int dest)
  {
    if (sub == 0)
      return;
    if (sub == dest)
      verify_fail ("recursive subroutine call");
    for (subr_info *info = jsr_ptrs[sub]; info != NULL; info = info->next)
      check_nonrecursive_call (get_subroutine (info->pc), dest);
  }

  void handle_jsr_insn (int offset)
  {
    int npc = compute_jump (offset);

    if (npc < PC)
      current_state->check_no_uninitialized_objects (current_method->max_locals, this);
    check_nonrecursive_call (current_state->subroutine, npc);

    // Modify our state as appropriate for entry into a subroutine.
    push_type (return_address_type);
    push_jump_merge (npc, current_state);
    // Clean up.
    pop_type (return_address_type);

    // On entry to the subroutine, the subroutine number must be set
    // and the locals must be marked as cleared.  We do this after
    // merging state so that we don't erroneously "notice" a variable
    // change merely on entry.
    states[npc]->enter_subroutine (npc, current_method->max_locals);

    // Indicate that we don't know the stack depth of the instruction
    // following the `jsr'.  The idea here is that we need to merge
    // the local variable state across the jsr, but the subroutine
    // might change the stack depth, so we can't make any assumptions
    // about it.  So we have yet another special case.  We know that
    // at this point PC points to the instruction after the jsr.

    // FIXME: what if we have a jsr at the end of the code, but that
    // jsr has no corresponding ret?  Is this verifiable, or is it
    // not?  If it is then we need a special case here.
    if (PC >= current_method->code_length)
      verify_fail ("fell off end");

    current_state->stacktop = state::NO_STACK;
    push_jump_merge (PC, current_state);
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
    jsr_ptrs = (subr_info **) _Jv_Malloc (sizeof (subr_info *)
					  * current_method->code_length);

    for (int i = 0; i < current_method->code_length; ++i)
      {
	flags[i] = 0;
	jsr_ptrs[i] = NULL;
      }

    bool last_was_jsr = false;

    PC = 0;
    while (PC < current_method->code_length)
      {
	// Set `start_PC' early so that error checking can have the
	// correct value.
	start_PC = PC;
	flags[PC] |= FLAG_INSN_START;

	// If the previous instruction was a jsr, then the next
	// instruction is a branch target -- the branch being the
	// corresponding `ret'.
	if (last_was_jsr)
	  note_branch_target (PC);
	last_was_jsr = false;

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
	    last_was_jsr = true;
	    // Fall through.
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
	    note_branch_target (compute_jump (get_short ()), last_was_jsr);
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
	    last_was_jsr = true;
	    // Fall through.
	  case op_goto_w:
	    note_branch_target (compute_jump (get_int ()), last_was_jsr);
	    break;

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
	if (! (flags[exception[i].handler_pc] & FLAG_INSN_START))
	  verify_fail ("exception handler not at instruction start",
		       exception[i].handler_pc);
	if (! (flags[exception[i].start_pc] & FLAG_INSN_START))
	  verify_fail ("exception start not at instruction start",
		       exception[i].start_pc);
	if (exception[i].end_pc != current_method->code_length
	    && ! (flags[exception[i].end_pc] & FLAG_INSN_START))
	  verify_fail ("exception end not at instruction start",
		       exception[i].end_pc);

	flags[exception[i].handler_pc] |= FLAG_BRANCH_TARGET;
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
      return type (pool->data[index].clazz);
    else if (pool->tags[index] == JV_CONSTANT_Class)
      return type (pool->data[index].utf8);
    verify_fail ("expected class constant", start_PC);
  }

  type check_constant (int index)
  {
    check_pool_index (index);
    _Jv_Constants *pool = &current_class->constants;
    if (pool->tags[index] == JV_CONSTANT_ResolvedString
	|| pool->tags[index] == JV_CONSTANT_String)
      return type (&java::lang::String::class$);
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
  type check_field_constant (int index, type *class_type = NULL)
  {
    _Jv_Utf8Const *name, *field_type;
    type ct = handle_field_or_method (index,
				      JV_CONSTANT_Fieldref,
				      &name, &field_type);
    if (class_type)
      *class_type = ct;
    if (field_type->data[0] == '[' || field_type->data[0] == 'L')
      return type (field_type);
    return get_type_val_for_signature (field_type->data[0]);
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
	return type (name);
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
    return type (k);
  }

  void compute_argument_types (_Jv_Utf8Const *signature,
			       type *types)
  {
    char *p = signature->data;
    // Skip `('.
    ++p;

    int i = 0;
    while (*p != ')')
      types[i++] = get_one_type (p);
  }

  type compute_return_type (_Jv_Utf8Const *signature)
  {
    char *p = signature->data;
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
    bool is_init = false;

    using namespace java::lang::reflect;
    if (! Modifier::isStatic (current_method->self->accflags))
      {
	type kurr (current_class);
	if (_Jv_equalUtf8Consts (current_method->self->name, gcj::init_name))
	  {
	    kurr.set_uninitialized (type::SELF, this);
	    is_init = true;
	  }
	set_variable (0, kurr);
	current_state->set_this_type (kurr);
	++var;
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

    states = (state **) _Jv_Malloc (sizeof (state *)
				    * current_method->code_length);
    for (int i = 0; i < current_method->code_length; ++i)
      states[i] = NULL;

    next_verify_pc = state::NO_NEXT;

    while (true)
      {
	// If the PC was invalidated, get a new one from the work list.
	if (PC == state::NO_NEXT)
	  {
	    PC = pop_jump ();
	    if (PC == state::INVALID)
	      verify_fail ("can't happen: saw state::INVALID");
	    if (PC == state::NO_NEXT)
	      break;
	    debug_print ("== State pop from pending list\n");
	    // Set up the current state.
	    current_state->copy (states[PC], current_method->max_stack,
				 current_method->max_locals);
	  }
	else
	  {
	    // Control can't fall off the end of the bytecode.  We
	    // only need to check this in the fall-through case,
	    // because branch bounds are checked when they are
	    // pushed.
	    if (PC >= current_method->code_length)
	      verify_fail ("fell off end");

	    // We only have to do this checking in the situation where
	    // control flow falls through from the previous
	    // instruction.  Otherwise merging is done at the time we
	    // push the branch.
	    if (states[PC] != NULL)
	      {
		// We've already visited this instruction.  So merge
		// the states together.  If this yields no change then
		// we don't have to re-verify.  However, if the new
		// state is an the result of an unmerged `ret', we
		// must continue through it.
		debug_print ("== Fall through merge\n");
		states[PC]->print ("Old", PC, current_method->max_stack,
				   current_method->max_locals);
		current_state->print ("Cur", PC, current_method->max_stack,
				      current_method->max_locals);
		if (! current_state->merge (states[PC], false,
					    current_method->max_locals, this)
		    && ! states[PC]->is_unmerged_ret_state (current_method->max_locals))
		  {
		    debug_print ("== Fall through optimization\n");
		    invalidate_pc ();
		    continue;
		  }
		// Save a copy of it for later.
		states[PC]->copy (current_state, current_method->max_stack,
				  current_method->max_locals);
		current_state->print ("New", PC, current_method->max_stack,
				      current_method->max_locals);
	      }
	  }

	// We only have to keep saved state at branch targets.  If
	// we're at a branch target and the state here hasn't been set
	// yet, we set it now.
	if (states[PC] == NULL && (flags[PC] & FLAG_BRANCH_TARGET))
	  {
	    states[PC] = new state (current_state, current_method->max_stack,
				    current_method->max_locals);
	  }

	// Set this before handling exceptions so that debug output is
	// sane.
	start_PC = PC;

	// Update states for all active exception handlers.  Ordinarily
	// there are not many exception handlers.  So we simply run
	// through them all.
	for (int i = 0; i < current_method->exc_count; ++i)
	  {
	    if (PC >= exception[i].start_pc && PC < exception[i].end_pc)
	      {
		type handler (&java::lang::Throwable::class$);
		if (exception[i].handler_type != 0)
		  handler = check_class_constant (exception[i].handler_type);
		push_exception_jump (handler, exception[i].handler_pc);
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
	    push_type (require_array_type (pop_type (reference_type),
					   int_type));
	    break;
	  case op_laload:
	    pop_type (int_type);
	    push_type (require_array_type (pop_type (reference_type),
					   long_type));
	    break;
	  case op_faload:
	    pop_type (int_type);
	    push_type (require_array_type (pop_type (reference_type),
					   float_type));
	    break;
	  case op_daload:
	    pop_type (int_type);
	    push_type (require_array_type (pop_type (reference_type),
					   double_type));
	    break;
	  case op_aaload:
	    pop_type (int_type);
	    push_type (require_array_type (pop_type (reference_type),
					   reference_type));
	    break;
	  case op_baload:
	    pop_type (int_type);
	    require_array_type (pop_type (reference_type), byte_type);
	    push_type (int_type);
	    break;
	  case op_caload:
	    pop_type (int_type);
	    require_array_type (pop_type (reference_type), char_type);
	    push_type (int_type);
	    break;
	  case op_saload:
	    pop_type (int_type);
	    require_array_type (pop_type (reference_type), short_type);
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
	    require_array_type (pop_type (reference_type), int_type);
	    break;
	  case op_lastore:
	    pop_type (long_type);
	    pop_type (int_type);
	    require_array_type (pop_type (reference_type), long_type);
	    break;
	  case op_fastore:
	    pop_type (float_type);
	    pop_type (int_type);
	    require_array_type (pop_type (reference_type), float_type);
	    break;
	  case op_dastore:
	    pop_type (double_type);
	    pop_type (int_type);
	    require_array_type (pop_type (reference_type), double_type);
	    break;
	  case op_aastore:
	    pop_type (reference_type);
	    pop_type (int_type);
	    require_array_type (pop_type (reference_type), reference_type);
	    break;
	  case op_bastore:
	    pop_type (int_type);
	    pop_type (int_type);
	    require_array_type (pop_type (reference_type), byte_type);
	    break;
	  case op_castore:
	    pop_type (int_type);
	    pop_type (int_type);
	    require_array_type (pop_type (reference_type), char_type);
	    break;
	  case op_sastore:
	    pop_type (int_type);
	    pop_type (int_type);
	    require_array_type (pop_type (reference_type), short_type);
	    break;
	  case op_pop:
	    pop32 ();
	    break;
	  case op_pop2:
	    pop64 ();
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
	    check_return_type (pop_type (reference_type));
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
	      type field = check_field_constant (get_ushort (), &klass);
	      pop_type (field);

	      // We have an obscure special case here: we can use
	      // `putfield' on a field declared in this class, even if
	      // `this' has not yet been initialized.
	      if (! current_state->this_type.isinitialized ()
		  && current_state->this_type.pc == type::SELF)
		klass.set_uninitialized (type::SELF, this);
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
	      else if (method_name->data[0] == '<')
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
		  pop_type (arg_types[i]);
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
		    }
		  type raw = pop_raw ();
		  bool ok = false;
		  if (t.compatible (raw, this))
		    {
		      ok = true;
		    }
		  else if (opcode == op_invokeinterface)
		    {
		      // This is a hack.  We might have merged two
		      // items and gotten `Object'.  This can happen
		      // because we don't keep track of where merges
		      // come from.  This is safe as long as the
		      // interpreter checks interfaces at runtime.
		      type obj (&java::lang::Object::class$);
		      ok = raw.compatible (obj, this);
		    }

		  if (! ok)
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
	      push_type (construct_primitive_array_type (type_val (atype)));
	    }
	    break;
	  case op_anewarray:
	    pop_type (int_type);
	    push_type (check_class_constant (get_ushort ()).to_array (this));
	    break;
	  case op_arraylength:
	    {
	      type t = pop_type (reference_type);
	      if (! t.isarray () && ! t.isnull ())
		verify_fail ("array type expected");
	      push_type (int_type);
	    }
	    break;
	  case op_athrow:
	    pop_type (type (&java::lang::Throwable::class$));
	    invalidate_pc ();
	    break;
	  case op_checkcast:
	    pop_type (reference_type);
	    push_type (check_class_constant (get_ushort ()));
	    break;
	  case op_instanceof:
	    pop_type (reference_type);
	    check_class_constant (get_ushort ());
	    push_type (int_type);
	    break;
	  case op_monitorenter:
	    pop_type (reference_type);
	    break;
	  case op_monitorexit:
	    pop_type (reference_type);
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
		  set_variable (get_ushort (), pop_type (reference_type));
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

	  default:
	    // Unrecognized opcode.
	    verify_fail ("unrecognized instruction in verify_instructions_0",
			 start_PC);
	  }
      }
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
    buf->append (JvNewStringUTF (method->get_method()->name->data));
    buf->append ((jchar) '(');
    buf->append (JvNewStringUTF (method->get_method()->signature->data));
    buf->append ((jchar) ')');

    buf->append (JvNewStringLatin1 (": "));
    buf->append (JvNewStringLatin1 (s));
    throw new java::lang::VerifyError (buf->toString ());
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
    debug_print ("-- Verifying method `%s'\n", m->self->name->data);

    current_method = m;
    bytecode = m->bytecode ();
    exception = m->exceptions ();
    current_class = m->defining_class;

    states = NULL;
    flags = NULL;
    jsr_ptrs = NULL;
    utf8_list = NULL;
    entry_points = NULL;
  }

  ~_Jv_BytecodeVerifier ()
  {
    if (states)
      _Jv_Free (states);
    if (flags)
      _Jv_Free (flags);

    if (jsr_ptrs)
      {
	for (int i = 0; i < current_method->code_length; ++i)
	  {
	    if (jsr_ptrs[i] != NULL)
	      {
		subr_info *info = jsr_ptrs[i];
		while (info != NULL)
		  {
		    subr_info *next = info->next;
		    _Jv_Free (info);
		    info = next;
		  }
	      }
	  }
	_Jv_Free (jsr_ptrs);
      }

    while (utf8_list != NULL)
      {
	linked_utf8 *n = utf8_list->next;
	_Jv_Free (utf8_list->val);
	_Jv_Free (utf8_list);
	utf8_list = n;
      }

    while (entry_points != NULL)
      {
	subr_entry_info *next = entry_points->next;
	_Jv_Free (entry_points);
	entry_points = next;
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
