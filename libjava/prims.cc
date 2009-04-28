// prims.cc - Code for core of runtime environment.

/* Copyright (C) 1998, 1999, 2000, 2001, 2002, 2003, 2004, 2005, 2006, 2007  Free Software Foundation

   This file is part of libgcj.

This software is copyrighted work licensed under the terms of the
Libgcj License.  Please consult the file "LIBGCJ_LICENSE" for
details.  */

#include <config.h>
#include <platform.h>

#include <stdlib.h>
#include <stdarg.h>
#include <stdio.h>
#include <string.h>
#include <signal.h>

#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif

#include <gcj/cni.h>
#include <jvm.h>
#include <java-signal.h>
#include <java-threads.h>
#include <java-interp.h>

#ifdef ENABLE_JVMPI
#include <jvmpi.h>
#include <java/lang/ThreadGroup.h>
#endif

#ifdef INTERPRETER
#include <jvmti.h>
#include "jvmti-int.h"
#endif

#ifndef DISABLE_GETENV_PROPERTIES
#include <ctype.h>
#include <java-props.h>
#define PROCESS_GCJ_PROPERTIES process_gcj_properties()
#else
#define PROCESS_GCJ_PROPERTIES
#endif // DISABLE_GETENV_PROPERTIES

#include <java/lang/Class.h>
#include <java/lang/ClassLoader.h>
#include <java/lang/Runtime.h>
#include <java/lang/String.h>
#include <java/lang/Thread.h>
#include <java/lang/ThreadGroup.h>
#include <java/lang/ArrayIndexOutOfBoundsException.h>
#include <java/lang/ArithmeticException.h>
#include <java/lang/ClassFormatError.h>
#include <java/lang/ClassNotFoundException.h>
#include <java/lang/InternalError.h>
#include <java/lang/NegativeArraySizeException.h>
#include <java/lang/NoClassDefFoundError.h>
#include <java/lang/NullPointerException.h>
#include <java/lang/OutOfMemoryError.h>
#include <java/lang/System.h>
#include <java/lang/VMClassLoader.h>
#include <java/lang/reflect/Modifier.h>
#include <java/io/PrintStream.h>
#include <java/lang/UnsatisfiedLinkError.h>
#include <java/lang/VirtualMachineError.h>
#include <gnu/gcj/runtime/ExtensionClassLoader.h>
#include <gnu/gcj/runtime/FinalizerThread.h>
#include <execution.h>

#ifdef INTERPRETER
#include <gnu/classpath/jdwp/Jdwp.h>
#include <gnu/classpath/jdwp/VMVirtualMachine.h>
#endif // INTERPRETER

#include <gnu/java/lang/MainThread.h>

#ifdef USE_LTDL
#include <ltdl.h>
#endif

// Execution engine for compiled code.
_Jv_CompiledEngine _Jv_soleCompiledEngine;

// Execution engine for code compiled with -findirect-classes
_Jv_IndirectCompiledEngine _Jv_soleIndirectCompiledEngine;

// We allocate a single OutOfMemoryError exception which we keep
// around for use if we run out of memory.
static java::lang::OutOfMemoryError *no_memory;

// Number of bytes in largest array object we create.  This could be
// increased to the largest size_t value, so long as the appropriate
// functions are changed to take a size_t argument instead of jint.
#define MAX_OBJECT_SIZE (((size_t)1<<31) - 1)

// Properties set at compile time.
const char **_Jv_Compiler_Properties = NULL;
int _Jv_Properties_Count = 0;

#ifndef DISABLE_GETENV_PROPERTIES
// Property key/value pairs.
property_pair *_Jv_Environment_Properties;
#endif

// Stash the argv pointer to benefit native libraries that need it.
const char **_Jv_argv;
int _Jv_argc;

// Debugging options
static bool remoteDebug = false;
#ifdef INTERPRETER
static char defaultJdwpOptions[] = "";
static char *jdwpOptions = defaultJdwpOptions;

// Typedefs for JVMTI agent functions.
typedef jint jvmti_agent_onload_func (JavaVM *vm, char *options,
                                      void *reserved);
typedef jint jvmti_agent_onunload_func (JavaVM *vm);

// JVMTI agent function pointers.
static jvmti_agent_onload_func *jvmti_agentonload = NULL;
static jvmti_agent_onunload_func *jvmti_agentonunload = NULL;
static char *jvmti_agent_opts;
#endif // INTERPRETER

// Argument support.
int
_Jv_GetNbArgs (void)
{
  // _Jv_argc is 0 if not explicitly initialized.
  return _Jv_argc;
}

const char *
_Jv_GetSafeArg (int index)
{
  if (index >=0 && index < _Jv_GetNbArgs ())
    return _Jv_argv[index];
  else
    return "";
}

void
_Jv_SetArgs (int argc, const char **argv)
{
  _Jv_argc = argc;
  _Jv_argv = argv;
}

#ifdef ENABLE_JVMPI
// Pointer to JVMPI notification functions.
void (*_Jv_JVMPI_Notify_OBJECT_ALLOC) (JVMPI_Event *event);
void (*_Jv_JVMPI_Notify_THREAD_START) (JVMPI_Event *event);
void (*_Jv_JVMPI_Notify_THREAD_END) (JVMPI_Event *event);
#endif


#if defined (HANDLE_SEGV) || defined(HANDLE_FPE)
/* Unblock a signal.  Unless we do this, the signal may only be sent
   once.  */
static void 
unblock_signal (int signum __attribute__ ((__unused__)))
{
#ifdef _POSIX_VERSION
  sigset_t sigs;

  sigemptyset (&sigs);
  sigaddset (&sigs, signum);
  sigprocmask (SIG_UNBLOCK, &sigs, NULL);
#endif
}
#endif

#ifdef HANDLE_SEGV
SIGNAL_HANDLER (catch_segv)
{
  unblock_signal (SIGSEGV);
  MAKE_THROW_FRAME (nullp);
  java::lang::NullPointerException *nullp 
    = new java::lang::NullPointerException;
  throw nullp;
}
#endif

#ifdef HANDLE_FPE
SIGNAL_HANDLER (catch_fpe)
{
  unblock_signal (SIGFPE);
#ifdef HANDLE_DIVIDE_OVERFLOW
  HANDLE_DIVIDE_OVERFLOW;
#else
  MAKE_THROW_FRAME (arithexception);
#endif
  java::lang::ArithmeticException *arithexception 
    = new java::lang::ArithmeticException (JvNewStringLatin1 ("/ by zero"));
  throw arithexception;
}
#endif


jboolean
_Jv_equalUtf8Consts (const Utf8Const* a, const Utf8Const *b)
{
  int len;
  const _Jv_ushort *aptr, *bptr;
  if (a == b)
    return true;
  if (a->hash != b->hash)
    return false;
  len = a->length;
  if (b->length != len)
    return false;
  aptr = (const _Jv_ushort *)a->data;
  bptr = (const _Jv_ushort *)b->data;
  len = (len + 1) >> 1;
  while (--len >= 0)
    if (*aptr++ != *bptr++)
      return false;
  return true;
}

/* True iff A is equal to STR.
   HASH is STR->hashCode().  
*/

jboolean
_Jv_equal (Utf8Const* a, jstring str, jint hash)
{
  if (a->hash != (_Jv_ushort) hash)
    return false;
  jint len = str->length();
  jint i = 0;
  jchar *sptr = _Jv_GetStringChars (str);
  unsigned char* ptr = (unsigned char*) a->data;
  unsigned char* limit = ptr + a->length;
  for (;; i++, sptr++)
    {
      int ch = UTF8_GET (ptr, limit);
      if (i == len)
	return ch < 0;
      if (ch != *sptr)
	return false;
    }
  return true;
}

/* Like _Jv_equal, but stop after N characters.  */
jboolean
_Jv_equaln (Utf8Const *a, jstring str, jint n)
{
  jint len = str->length();
  jint i = 0;
  jchar *sptr = _Jv_GetStringChars (str);
  unsigned char* ptr = (unsigned char*) a->data;
  unsigned char* limit = ptr + a->length;
  for (; n-- > 0; i++, sptr++)
    {
      int ch = UTF8_GET (ptr, limit);
      if (i == len)
	return ch < 0;
      if (ch != *sptr)
	return false;
    }
  return true;
}

// Determines whether the given Utf8Const object contains
// a type which is primitive or some derived form of it, eg.
// an array or multi-dimensional array variant.
jboolean
_Jv_isPrimitiveOrDerived(const Utf8Const *a)
{
  unsigned char *aptr = (unsigned char *) a->data;
  unsigned char *alimit = aptr + a->length;
  int ac = UTF8_GET(aptr, alimit);

  // Skips any leading array marks.
  while (ac == '[')
    ac = UTF8_GET(aptr, alimit);

  // There should not be another character. This implies that
  // the type name is only one character long.
  if (UTF8_GET(aptr, alimit) == -1)
    switch ( ac )
      {
        case 'Z':
        case 'B':
        case 'C':
        case 'S':
        case 'I':
        case 'J':
        case 'F':
        case 'D':
          return true;
        default:
          break;
       }

   return false;
}

// Find out whether two _Jv_Utf8Const candidates contain the same
// classname.
// The method is written to handle the different formats of classnames.
// Eg. "Ljava/lang/Class;", "Ljava.lang.Class;", "java/lang/Class" and
// "java.lang.Class" will be seen as equal.
// Warning: This function is not smart enough to declare "Z" and "boolean"
// and similar cases as equal (and is not meant to be used this way)!
jboolean
_Jv_equalUtf8Classnames (const Utf8Const *a, const Utf8Const *b)
{
  // If the class name's length differs by two characters
  // it is possible that we have candidates which are given
  // in the two different formats ("Lp1/p2/cn;" vs. "p1/p2/cn")
  switch (a->length - b->length)
    {
      case -2:
      case 0:
      case 2:
        break;
      default:
        return false;
    }

  unsigned char *aptr = (unsigned char *) a->data;
  unsigned char *alimit = aptr + a->length;
  unsigned char *bptr = (unsigned char *) b->data;
  unsigned char *blimit = bptr + b->length;

  if (alimit[-1] == ';')
    alimit--;

  if (blimit[-1] == ';')
    blimit--;

  int ac = UTF8_GET(aptr, alimit);
  int bc = UTF8_GET(bptr, blimit);

  // Checks whether both strings have the same amount of leading [ characters.
  while (ac == '[')
    {
      if (bc == '[')
        {
          ac = UTF8_GET(aptr, alimit);
          bc = UTF8_GET(bptr, blimit);
          continue;
        }

      return false;
    }

  // Skips leading L character.
  if (ac == 'L')
    ac = UTF8_GET(aptr, alimit);
        
  if (bc == 'L')
    bc = UTF8_GET(bptr, blimit);

  // Compares the remaining characters.
  while (ac != -1 && bc != -1)
    {
      // Replaces package separating dots with slashes.
      if (ac == '.')
        ac = '/';

      if (bc == '.')
        bc = '/';
      
      // Now classnames differ if there is at least one non-matching
      // character.
      if (ac != bc)
        return false;

      ac = UTF8_GET(aptr, alimit);
      bc = UTF8_GET(bptr, blimit);
    }

  return (ac == bc);
}

/* Count the number of Unicode chars encoded in a given Ut8 string. */
int
_Jv_strLengthUtf8(const char* str, int len)
{
  unsigned char* ptr;
  unsigned char* limit;
  int str_length;

  ptr = (unsigned char*) str;
  limit = ptr + len;
  str_length = 0;
  for (; ptr < limit; str_length++)
    {
      if (UTF8_GET (ptr, limit) < 0)
	return (-1);
    }
  return (str_length);
}

/* Calculate a hash value for a string encoded in Utf8 format.
 * This returns the same hash value as specified or java.lang.String.hashCode.
 */
jint
_Jv_hashUtf8String (const char* str, int len)
{
  unsigned char* ptr = (unsigned char*) str;
  unsigned char* limit = ptr + len;
  jint hash = 0;

  for (; ptr < limit;)
    {
      int ch = UTF8_GET (ptr, limit);
      /* Updated specification from
	 http://www.javasoft.com/docs/books/jls/clarify.html. */
      hash = (31 * hash) + ch;
    }
  return hash;
}

void
_Jv_Utf8Const::init(const char *s, int len)
{
  ::memcpy (data, s, len);
  data[len] = 0;
  length = len;
  hash = _Jv_hashUtf8String (s, len) & 0xFFFF;
}

_Jv_Utf8Const *
_Jv_makeUtf8Const (const char* s, int len)
{
  if (len < 0)
    len = strlen (s);
  Utf8Const* m
    = (Utf8Const*) _Jv_AllocBytes (_Jv_Utf8Const::space_needed(s, len));
  m->init(s, len);
  return m;
}

_Jv_Utf8Const *
_Jv_makeUtf8Const (jstring string)
{
  jint hash = string->hashCode ();
  jint len = _Jv_GetStringUTFLength (string);

  Utf8Const* m = (Utf8Const*)
    _Jv_AllocBytes (sizeof(Utf8Const) + len + 1);

  m->hash = hash;
  m->length = len;

  _Jv_GetStringUTFRegion (string, 0, string->length (), m->data);
  m->data[len] = 0;
  
  return m;
}



#ifdef __GCJ_DEBUG
void
_Jv_Abort (const char *function, const char *file, int line,
	   const char *message)
#else
void
_Jv_Abort (const char *, const char *, int, const char *message)
#endif
{
#ifdef __GCJ_DEBUG
  fprintf (stderr,
	   "libgcj failure: %s\n   in function %s, file %s, line %d\n",
	   message, function, file, line);
#else
  fprintf (stderr, "libgcj failure: %s\n", message);
#endif
  fflush (stderr);
  abort ();
}

static void
fail_on_finalization (jobject)
{
  JvFail ("object was finalized");
}

void
_Jv_GCWatch (jobject obj)
{
  _Jv_RegisterFinalizer (obj, fail_on_finalization);
}

void
_Jv_ThrowBadArrayIndex(jint bad_index)
{
  throw new java::lang::ArrayIndexOutOfBoundsException
    (java::lang::String::valueOf (bad_index));
}

void
_Jv_ThrowNullPointerException ()
{
  throw new java::lang::NullPointerException;
}

// Resolve an entry in the constant pool and return the target
// address.
void *
_Jv_ResolvePoolEntry (jclass this_class, jint index)
{
  _Jv_Constants *pool = &this_class->constants;

  if ((pool->tags[index] & JV_CONSTANT_ResolvedFlag) != 0)
    return pool->data[index].field->u.addr;

  JvSynchronize sync (this_class);
  return (_Jv_Linker::resolve_pool_entry (this_class, index))
    .field->u.addr;
}


// Explicitly throw a no memory exception.
// The collector calls this when it encounters an out-of-memory condition.
void _Jv_ThrowNoMemory()
{
  throw no_memory;
}

#ifdef ENABLE_JVMPI
# define JVMPI_NOTIFY_ALLOC(klass,size,obj) \
    if (__builtin_expect (_Jv_JVMPI_Notify_OBJECT_ALLOC != 0, false)) \
      jvmpi_notify_alloc(klass,size,obj);
static void
jvmpi_notify_alloc(jclass klass, jint size, jobject obj)
{
  // Service JVMPI allocation request.
  JVMPI_Event event;

  event.event_type = JVMPI_EVENT_OBJECT_ALLOC;
  event.env_id = NULL;
  event.u.obj_alloc.arena_id = 0;
  event.u.obj_alloc.class_id = (jobjectID) klass;
  event.u.obj_alloc.is_array = 0;
  event.u.obj_alloc.size = size;
  event.u.obj_alloc.obj_id = (jobjectID) obj;

  // FIXME:  This doesn't look right for the Boehm GC.  A GC may
  // already be in progress.  _Jv_DisableGC () doesn't wait for it.
  // More importantly, I don't see the need for disabling GC, since we
  // blatantly have a pointer to obj on our stack, ensuring that the
  // object can't be collected.  Even for a nonconservative collector,
  // it appears to me that this must be true, since we are about to
  // return obj. Isn't this whole approach way too intrusive for
  // a useful profiling interface?			- HB
  _Jv_DisableGC ();
  (*_Jv_JVMPI_Notify_OBJECT_ALLOC) (&event);
  _Jv_EnableGC ();
}
#else /* !ENABLE_JVMPI */
# define JVMPI_NOTIFY_ALLOC(klass,size,obj) /* do nothing */
#endif

// Allocate a new object of class KLASS.
// First a version that assumes that we have no finalizer, and that
// the class is already initialized.
// If we know that JVMPI is disabled, this can be replaced by a direct call
// to the allocator for the appropriate GC.
jobject
_Jv_AllocObjectNoInitNoFinalizer (jclass klass)
{
  jint size = klass->size ();
  jobject obj = (jobject) _Jv_AllocObj (size, klass);
  JVMPI_NOTIFY_ALLOC (klass, size, obj);
  return obj;
}

// And now a version that initializes if necessary.
jobject
_Jv_AllocObjectNoFinalizer (jclass klass)
{
  if (_Jv_IsPhantomClass(klass) )
    throw new java::lang::NoClassDefFoundError(klass->getName());

  _Jv_InitClass (klass);
  jint size = klass->size ();
  jobject obj = (jobject) _Jv_AllocObj (size, klass);
  JVMPI_NOTIFY_ALLOC (klass, size, obj);
  return obj;
}

// And now the general version that registers a finalizer if necessary.
jobject
_Jv_AllocObject (jclass klass)
{
  jobject obj = _Jv_AllocObjectNoFinalizer (klass);
  
  // We assume that the compiler only generates calls to this routine
  // if there really is an interesting finalizer.
  // Unfortunately, we still have to the dynamic test, since there may
  // be cni calls to this routine.
  // Note that on IA64 get_finalizer() returns the starting address of the
  // function, not a function pointer.  Thus this still works.
  if (klass->vtable->get_finalizer ()
      != java::lang::Object::class$.vtable->get_finalizer ())
    _Jv_RegisterFinalizer (obj, _Jv_FinalizeObject);
  return obj;
}

// Allocate a String, including variable length storage.
jstring
_Jv_AllocString(jsize len)
{
  using namespace java::lang;

  jsize sz = sizeof(java::lang::String) + len * sizeof(jchar);

  // We assert that for strings allocated this way, the data field
  // will always point to the object itself.  Thus there is no reason
  // for the garbage collector to scan any of it.
  // Furthermore, we're about to overwrite the string data, so
  // initialization of the object is not an issue.

  // String needs no initialization, and there is no finalizer, so
  // we can go directly to the collector's allocator interface.
  jstring obj = (jstring) _Jv_AllocPtrFreeObj(sz, &String::class$);

  obj->data = obj;
  obj->boffset = sizeof(java::lang::String);
  obj->count = len;
  obj->cachedHashCode = 0;

  JVMPI_NOTIFY_ALLOC (&String::class$, sz, obj);
  
  return obj;
}

// A version of the above that assumes the object contains no pointers,
// and requires no finalization.  This can't happen if we need pointers
// to locks.
#ifdef JV_HASH_SYNCHRONIZATION
jobject
_Jv_AllocPtrFreeObject (jclass klass)
{
  _Jv_InitClass (klass);
  jint size = klass->size ();

  jobject obj = (jobject) _Jv_AllocPtrFreeObj (size, klass);

  JVMPI_NOTIFY_ALLOC (klass, size, obj);

  return obj;
}
#endif /* JV_HASH_SYNCHRONIZATION */


// Allocate a new array of Java objects.  Each object is of type
// `elementClass'.  `init' is used to initialize each slot in the
// array.
jobjectArray
_Jv_NewObjectArray (jsize count, jclass elementClass, jobject init)
{
  // Creating an array of an unresolved type is impossible. So we throw
  // the NoClassDefFoundError.
  if ( _Jv_IsPhantomClass(elementClass) )
    throw new java::lang::NoClassDefFoundError(elementClass->getName());

  if (__builtin_expect (count < 0, false))
    throw new java::lang::NegativeArraySizeException;

  JvAssert (! elementClass->isPrimitive ());

  // Ensure that elements pointer is properly aligned.
  jobjectArray obj = NULL;
  size_t size = (size_t) elements (obj);
  // Check for overflow.
  if (__builtin_expect ((size_t) count > 
			(MAX_OBJECT_SIZE - 1 - size) / sizeof (jobject), false))
    throw no_memory;

  size += count * sizeof (jobject);

  jclass klass = _Jv_GetArrayClass (elementClass,
				    elementClass->getClassLoaderInternal());

  obj = (jobjectArray) _Jv_AllocArray (size, klass);
  // Cast away const.
  jsize *lp = const_cast<jsize *> (&obj->length);
  *lp = count;
  // We know the allocator returns zeroed memory.  So don't bother
  // zeroing it again.
  if (init)
    {
      jobject *ptr = elements(obj);
      while (--count >= 0)
	*ptr++ = init;
    }
  return obj;
}

// Allocate a new array of primitives.  ELTYPE is the type of the
// element, COUNT is the size of the array.
jobject
_Jv_NewPrimArray (jclass eltype, jint count)
{
  int elsize = eltype->size();
  if (__builtin_expect (count < 0, false))
    throw new java::lang::NegativeArraySizeException;

  JvAssert (eltype->isPrimitive ());
  jobject dummy = NULL;
  size_t size = (size_t) _Jv_GetArrayElementFromElementType (dummy, eltype);

  // Check for overflow.
  if (__builtin_expect ((size_t) count > 
			(MAX_OBJECT_SIZE - size) / elsize, false))
    throw no_memory;

  jclass klass = _Jv_GetArrayClass (eltype, 0);

# ifdef JV_HASH_SYNCHRONIZATION
  // Since the vtable is always statically allocated,
  // these are completely pointerfree!  Make sure the GC doesn't touch them.
  __JArray *arr =
    (__JArray*) _Jv_AllocPtrFreeObj (size + elsize * count, klass);
  memset((char *)arr + size, 0, elsize * count);
# else
  __JArray *arr = (__JArray*) _Jv_AllocObj (size + elsize * count, klass);
  // Note that we assume we are given zeroed memory by the allocator.
# endif
  // Cast away const.
  jsize *lp = const_cast<jsize *> (&arr->length);
  *lp = count;

  return arr;
}

jobject
_Jv_NewArray (jint type, jint size)
{
  switch (type)
    {
      case  4:  return JvNewBooleanArray (size);
      case  5:  return JvNewCharArray (size);
      case  6:  return JvNewFloatArray (size);
      case  7:  return JvNewDoubleArray (size);
      case  8:  return JvNewByteArray (size);
      case  9:  return JvNewShortArray (size);
      case 10:  return JvNewIntArray (size);
      case 11:  return JvNewLongArray (size);
    }
  throw new java::lang::InternalError
    (JvNewStringLatin1 ("invalid type code in _Jv_NewArray"));
}

// Allocate a possibly multi-dimensional array but don't check that
// any array length is <0.
static jobject
_Jv_NewMultiArrayUnchecked (jclass type, jint dimensions, jint *sizes)
{
  JvAssert (type->isArray());
  jclass element_type = type->getComponentType();
  jobject result;
  if (element_type->isPrimitive())
    result = _Jv_NewPrimArray (element_type, sizes[0]);
  else
    result = _Jv_NewObjectArray (sizes[0], element_type, NULL);

  if (dimensions > 1)
    {
      JvAssert (! element_type->isPrimitive());
      JvAssert (element_type->isArray());
      jobject *contents = elements ((jobjectArray) result);
      for (int i = 0; i < sizes[0]; ++i)
	contents[i] = _Jv_NewMultiArrayUnchecked (element_type, dimensions - 1,
						  sizes + 1);
    }

  return result;
}

jobject
_Jv_NewMultiArray (jclass type, jint dimensions, jint *sizes)
{
  for (int i = 0; i < dimensions; ++i)
    if (sizes[i] < 0)
      throw new java::lang::NegativeArraySizeException;

  return _Jv_NewMultiArrayUnchecked (type, dimensions, sizes);
}

jobject
_Jv_NewMultiArray (jclass array_type, jint dimensions, ...)
{
  // Creating an array of an unresolved type is impossible. So we throw
  // the NoClassDefFoundError.
  if (_Jv_IsPhantomClass(array_type))
    throw new java::lang::NoClassDefFoundError(array_type->getName());

  va_list args;
  jint sizes[dimensions];
  va_start (args, dimensions);
  for (int i = 0; i < dimensions; ++i)
    {
      jint size = va_arg (args, jint);
      if (size < 0)
	throw new java::lang::NegativeArraySizeException;
      sizes[i] = size;
    }
  va_end (args);

  return _Jv_NewMultiArrayUnchecked (array_type, dimensions, sizes);
}



// Ensure 8-byte alignment, for hash synchronization.
#define DECLARE_PRIM_TYPE(NAME)			\
  java::lang::Class _Jv_##NAME##Class __attribute__ ((aligned (8)));

DECLARE_PRIM_TYPE(byte)
DECLARE_PRIM_TYPE(short)
DECLARE_PRIM_TYPE(int)
DECLARE_PRIM_TYPE(long)
DECLARE_PRIM_TYPE(boolean)
DECLARE_PRIM_TYPE(char)
DECLARE_PRIM_TYPE(float)
DECLARE_PRIM_TYPE(double)
DECLARE_PRIM_TYPE(void)

void
_Jv_InitPrimClass (jclass cl, const char *cname, char sig, int len)
{    
  using namespace java::lang::reflect;

  // We must set the vtable for the class; the Java constructor
  // doesn't do this.
  (*(_Jv_VTable **) cl) = java::lang::Class::class$.vtable;

  // Initialize the fields we care about.  We do this in the same
  // order they are declared in Class.h.
  cl->name = _Jv_makeUtf8Const ((char *) cname, -1);
  cl->accflags = Modifier::PUBLIC | Modifier::FINAL | Modifier::ABSTRACT;
  cl->method_count = sig;
  cl->size_in_bytes = len;
  cl->vtable = JV_PRIMITIVE_VTABLE;
  cl->state = JV_STATE_DONE;
  cl->depth = -1;
}

jclass
_Jv_FindClassFromSignature (char *sig, java::lang::ClassLoader *loader,
			    char **endp)
{
  // First count arrays.
  int array_count = 0;
  while (*sig == '[')
    {
      ++sig;
      ++array_count;
    }

  jclass result = NULL;
  switch (*sig)
    {
    case 'B':
      result = JvPrimClass (byte);
      break;
    case 'S':
      result = JvPrimClass (short);
      break;
    case 'I':
      result = JvPrimClass (int);
      break;
    case 'J':
      result = JvPrimClass (long);
      break;
    case 'Z':
      result = JvPrimClass (boolean);
      break;
    case 'C':
      result = JvPrimClass (char);
      break;
    case 'F':
      result = JvPrimClass (float);
      break;
    case 'D':
      result = JvPrimClass (double);
      break;
    case 'V':
      result = JvPrimClass (void);
      break;
    case 'L':
      {
	char *save = ++sig;
	while (*sig && *sig != ';')
	  ++sig;
	// Do nothing if signature appears to be malformed.
	if (*sig == ';')
	  {
	    _Jv_Utf8Const *name = _Jv_makeUtf8Const (save, sig - save);
	    result = _Jv_FindClass (name, loader);
	  }
	break;
      }
    default:
      // Do nothing -- bad signature.
      break;
    }

  if (endp)
    {
      // Not really the "end", but the last valid character that we
      // looked at.
      *endp = sig;
    }

  if (! result)
    return NULL;

  // Find arrays.
  while (array_count-- > 0)
    result = _Jv_GetArrayClass (result, loader);
  return result;
}


jclass
_Jv_FindClassFromSignatureNoException (char *sig, java::lang::ClassLoader *loader,
                                       char **endp)
{
  jclass klass;

  try
    {
      klass = _Jv_FindClassFromSignature(sig, loader, endp);
    }
  catch (java::lang::NoClassDefFoundError *ncdfe)
    {
      return NULL;
    }
  catch (java::lang::ClassNotFoundException *cnfe)
    {
      return NULL;
    }

  return klass;
}

JArray<jstring> *
JvConvertArgv (int argc, const char **argv)
{
  if (argc < 0)
    argc = 0;
  jobjectArray ar = JvNewObjectArray(argc, &java::lang::String::class$, NULL);
  jobject *ptr = elements(ar);
  jbyteArray bytes = NULL;
  for (int i = 0;  i < argc;  i++)
    {
      const char *arg = argv[i];
      int len = strlen (arg);
      if (bytes == NULL || bytes->length < len)
	bytes = JvNewByteArray (len);
      jbyte *bytePtr = elements (bytes);
      // We assume jbyte == char.
      memcpy (bytePtr, arg, len);

      // Now convert using the default encoding.
      *ptr++ = new java::lang::String (bytes, 0, len);
    }
  return (JArray<jstring>*) ar;
}

// FIXME: These variables are static so that they will be
// automatically scanned by the Boehm collector.  This is needed
// because with qthreads the collector won't scan the initial stack --
// it will only scan the qthreads stacks.

// Command line arguments.
static JArray<jstring> *arg_vec;

// The primary thread.
static java::lang::Thread *main_thread;

#ifndef DISABLE_GETENV_PROPERTIES

static char *
next_property_key (char *s, size_t *length)
{
  size_t l = 0;

  JvAssert (s);

  // Skip over whitespace
  while (isspace (*s))
    s++;

  // If we've reached the end, return NULL.  Also return NULL if for
  // some reason we've come across a malformed property string.
  if (*s == 0
      || *s == ':'
      || *s == '=')
    return NULL;

  // Determine the length of the property key.
  while (s[l] != 0
	 && ! isspace (s[l])
	 && s[l] != ':'
	 && s[l] != '=')
    {
      if (s[l] == '\\'
	  && s[l+1] != 0)
	l++;
      l++;
    }

  *length = l;

  return s;
}

static char *
next_property_value (char *s, size_t *length)
{
  size_t l = 0;

  JvAssert (s);

  while (isspace (*s))
    s++;

  if (*s == ':'
      || *s == '=')
    s++;

  while (isspace (*s))
    s++;

  // Determine the length of the property value.
  while (s[l] != 0
	 && ! isspace (s[l])
	 && s[l] != ':'
	 && s[l] != '=')
    {
      if (s[l] == '\\'
	  && s[l+1] != 0)
	l += 2;
      else
	l++;
    }

  *length = l;

  return s;
}

static void
process_gcj_properties ()
{
  char *props = getenv("GCJ_PROPERTIES");

  if (NULL == props)
    return;

  // Later on we will write \0s into this string.  It is simplest to
  // just duplicate it here.
  props = strdup (props);

  char *p = props;
  size_t length;
  size_t property_count = 0;

  // Whip through props quickly in order to count the number of
  // property values.
  while (p && (p = next_property_key (p, &length)))
    {
      // Skip to the end of the key
      p += length;

      p = next_property_value (p, &length);
      if (p)
	p += length;
      
      property_count++;
    }

  // Allocate an array of property value/key pairs.
  _Jv_Environment_Properties = 
    (property_pair *) malloc (sizeof(property_pair) 
			      * (property_count + 1));

  // Go through the properties again, initializing _Jv_Properties
  // along the way.
  p = props;
  property_count = 0;
  while (p && (p = next_property_key (p, &length)))
    {
      _Jv_Environment_Properties[property_count].key = p;
      _Jv_Environment_Properties[property_count].key_length = length;

      // Skip to the end of the key
      p += length;

      p = next_property_value (p, &length);
      
      _Jv_Environment_Properties[property_count].value = p;
      _Jv_Environment_Properties[property_count].value_length = length;

      if (p)
	p += length;

      property_count++;
    }
  memset ((void *) &_Jv_Environment_Properties[property_count], 
	  0, sizeof (property_pair));

  // Null terminate the strings.
  for (property_pair *prop = &_Jv_Environment_Properties[0];
       prop->key != NULL;
       prop++)
    {
      prop->key[prop->key_length] = 0;
      prop->value[prop->value_length] = 0;
    }
}
#endif // DISABLE_GETENV_PROPERTIES

namespace gcj
{
  _Jv_Utf8Const *void_signature;
  _Jv_Utf8Const *clinit_name;
  _Jv_Utf8Const *init_name;
  _Jv_Utf8Const *finit_name;
  
  bool runtimeInitialized = false;
  
  // When true, print debugging information about class loading.
  bool verbose_class_flag;
  
  // When true, enable the bytecode verifier and BC-ABI type verification. 
  bool verifyClasses = true;

  // Thread stack size specified by the -Xss runtime argument.
  size_t stack_size = 0;

  // Start time of the VM
  jlong startTime = 0;

  // Arguments passed to the VM
  JArray<jstring>* vmArgs;

  // Currently loaded classes
  jint loadedClasses = 0;

  // Unloaded classes
  jlong unloadedClasses = 0;
}

// We accept all non-standard options accepted by Sun's java command,
// for compatibility with existing application launch scripts.
static jint
parse_x_arg (char* option_string)
{
  if (strlen (option_string) <= 0)
    return -1;

  if (! strcmp (option_string, "int"))
    {
      // FIXME: this should cause the vm to never load shared objects
    }
  else if (! strcmp (option_string, "mixed"))
    {
      // FIXME: allow interpreted and native code
    }
  else if (! strcmp (option_string, "batch"))
    {
      // FIXME: disable background JIT'ing
    }
  else if (! strcmp (option_string, "debug"))
    {
      remoteDebug = true;
    }
#ifdef INTERPRETER
  else if (! strncmp (option_string, "runjdwp:", 8))
    {
      if (strlen (option_string) > 8)
	  jdwpOptions = &option_string[8];
      else
	{
	  fprintf (stderr,
		   "libgcj: argument required for JDWP options");
	  return -1;
	}
    }
#endif // INTERPRETER
  else if (! strncmp (option_string, "bootclasspath:", 14))
    {
      // FIXME: add a parse_bootclasspath_arg function
    }
  else if (! strncmp (option_string, "bootclasspath/a:", 16))
    {
    }
  else if (! strncmp (option_string, "bootclasspath/p:", 16))
    {
    }
  else if (! strcmp (option_string, "check:jni"))
    {
      // FIXME: enable strict JNI checking
    }
  else if (! strcmp (option_string, "future"))
    {
      // FIXME: enable strict class file format checks
    }
  else if (! strcmp (option_string, "noclassgc"))
    {
      // FIXME: disable garbage collection for classes
    }
  else if (! strcmp (option_string, "incgc"))
    {
      // FIXME: incremental garbage collection
    }
  else if (! strncmp (option_string, "loggc:", 6))
    {
      if (option_string[6] == '\0')
        {
          fprintf (stderr,
                   "libgcj: filename argument expected for loggc option\n");
          return -1;
        }
      // FIXME: set gc logging filename
    }
  else if (! strncmp (option_string, "ms", 2))
    {
      // FIXME: ignore this option until PR 20699 is fixed.
      // _Jv_SetInitialHeapSize (option_string + 2);
    }
  else if (! strncmp (option_string, "mx", 2))
    _Jv_SetMaximumHeapSize (option_string + 2);
  else if (! strcmp (option_string, "prof"))
    {
      // FIXME: enable profiling of program running in vm
    }
  else if (! strncmp (option_string, "runhprof:", 9))
    {
      // FIXME: enable specific type of vm profiling.  add a
      // parse_runhprof_arg function
    }
  else if (! strcmp (option_string, "rs"))
    {
      // FIXME: reduced system signal usage.  disable thread dumps,
      // only terminate in response to user-initiated calls,
      // e.g. System.exit()
    }
  else if (! strncmp (option_string, "ss", 2))
    {
      _Jv_SetStackSize (option_string + 2);
    }
  else if (! strcmp (option_string, "X:+UseAltSigs"))
    {
      // FIXME: use signals other than SIGUSR1 and SIGUSR2
    }
  else if (! strcmp (option_string, "share:off"))
    {
      // FIXME: don't share class data
    }
  else if (! strcmp (option_string, "share:auto"))
    {
      // FIXME: share class data where possible
    }
  else if (! strcmp (option_string, "share:on"))
    {
      // FIXME: fail if impossible to share class data
    }
  else
    {
      // Unrecognized.
      return -1;
    }
  return 0;
}

static jint
parse_verbose_args (char* option_string,
                    bool ignore_unrecognized)
{
  size_t len = sizeof ("-verbose") - 1;

  if (strlen (option_string) < len)
    return -1;

  if (option_string[len] == ':'
      && option_string[len + 1] != '\0')
    {
      char* verbose_args = option_string + len + 1;

      do
	{
	  if (! strncmp (verbose_args,
			 "gc", sizeof ("gc") - 1))
            {
              if (verbose_args[sizeof ("gc") - 1] == '\0'
                  || verbose_args[sizeof ("gc") - 1] == ',')
                {
                  // FIXME: we should add functions to boehm-gc that
                  // toggle GC_print_stats, GC_PRINT_ADDRESS_MAP and
                  // GC_print_back_height.
                  verbose_args += sizeof ("gc") - 1;
                }
              else
                {
                verbose_arg_err:
                  fprintf (stderr, "libgcj: unknown verbose option: %s\n",
                           option_string);
                  return -1;
                }
            }
	  else if (! strncmp (verbose_args,
			      "class",
			      sizeof ("class") - 1))
            {
              if (verbose_args[sizeof ("class") - 1] == '\0'
                  || verbose_args[sizeof ("class") - 1] == ',')
                {
                  gcj::verbose_class_flag = true;
                  verbose_args += sizeof ("class") - 1;
                }
              else
                goto verbose_arg_err;
            }
	  else if (! strncmp (verbose_args, "jni",
			      sizeof ("jni") - 1))
            {
              if (verbose_args[sizeof ("jni") - 1] == '\0'
                  || verbose_args[sizeof ("jni") - 1] == ',')
                {
                  // FIXME: enable JNI messages.
                  verbose_args += sizeof ("jni") - 1;
                }
              else
                goto verbose_arg_err;
            }
	  else if (ignore_unrecognized
		   && verbose_args[0] == 'X')
	    {
	      // ignore unrecognized non-standard verbose option
	      while (verbose_args[0] != '\0'
		     && verbose_args[0] != ',')
                verbose_args++;
	    }
          else if (verbose_args[0] == ',')
            {
              verbose_args++;
            }
          else
            goto verbose_arg_err;

          if (verbose_args[0] == ',')
            verbose_args++;
	}
      while (verbose_args[0] != '\0');
    }
  else if (option_string[len] == 'g'
	   && option_string[len + 1] == 'c'
	   && option_string[len + 2] == '\0')
    {
      // FIXME: we should add functions to boehm-gc that
      // toggle GC_print_stats, GC_PRINT_ADDRESS_MAP and
      // GC_print_back_height.
      return 0;
    }
  else if (option_string[len] == '\0')
    {
      gcj::verbose_class_flag = true;
      return 0;
    }
  else
    {
      // unrecognized option beginning with -verbose
      return -1;
    }
  return 0;
}

#ifdef INTERPRETER
// This function loads the agent functions for JVMTI from the library indicated
// by name.  It returns a negative value on failure, the value of which
// indicates where ltdl failed, it also prints an error message.
static jint
load_jvmti_agent (const char *name)
{
#ifdef USE_LTDL
  if (lt_dlinit ())
    {
      fprintf (stderr, 
              "libgcj: Error in ltdl init while loading agent library.\n");
      return -1;
    }
 
  lt_dlhandle lib = lt_dlopenext (name);
  if (!lib)
    {
      fprintf (stderr, 
               "libgcj: Error opening agent library.\n");
      return -2;
    }

  if (lib)
    {
      jvmti_agentonload 
        = (jvmti_agent_onload_func *) lt_dlsym (lib, "Agent_OnLoad");
 
      if (!jvmti_agentonload)
        {
          fprintf (stderr, 
                   "libgcj: Error finding agent function in library %s.\n",
                   name);
          lt_dlclose (lib);
          lib = NULL;
          return -4;
        }
      else
        {
          jvmti_agentonunload
            = (jvmti_agent_onunload_func *) lt_dlsym (lib, "Agent_OnUnload");
	   
          return 0;
        }
    }
  else
    {
      fprintf (stderr, "libgcj: Library %s not found in library path.\n", name);
      return -3;
    }

#endif /* USE_LTDL */

  // If LTDL cannot be used, return an error code indicating this.
  return -99;
}
#endif // INTERPRETER

static jint
parse_init_args (JvVMInitArgs* vm_args)
{
  // if _Jv_Compiler_Properties is non-NULL then it needs to be
  // re-allocated dynamically.
  if (_Jv_Compiler_Properties)
    {
      const char** props = _Jv_Compiler_Properties;
      _Jv_Compiler_Properties = NULL;

      for (int i = 0; props[i]; i++)
	{
	  _Jv_Compiler_Properties = (const char**) _Jv_Realloc
	    (_Jv_Compiler_Properties,
	     (_Jv_Properties_Count + 1) * sizeof (const char*));
	  _Jv_Compiler_Properties[_Jv_Properties_Count++] = props[i];
	}
    }

  if (vm_args == NULL)
    return 0;

  for (int i = 0; i < vm_args->nOptions; ++i)
    {
      char* option_string = vm_args->options[i].optionString;
      
      if (! strcmp (option_string, "vfprintf")
	  || ! strcmp (option_string, "exit")
	  || ! strcmp (option_string, "abort"))
	{
	  // FIXME: we are required to recognize these, but for
	  // now we don't handle them in any way.
	  continue;
	}
      else if (! strncmp (option_string,
			  "-verbose", sizeof ("-verbose") - 1))
	{
	  jint result = parse_verbose_args (option_string,
                                            vm_args->ignoreUnrecognized);
	  if (result < 0)
	    return result;
	}
      else if (! strncmp (option_string, "-D", 2))
	{
	  _Jv_Compiler_Properties = (const char**) _Jv_Realloc
	    (_Jv_Compiler_Properties,
	     (_Jv_Properties_Count + 1) * sizeof (char*));

	  _Jv_Compiler_Properties[_Jv_Properties_Count++] =
	    strdup (option_string + 2);

	  continue;
	}
#ifdef INTERPRETER
      else if (! strncmp (option_string, "-agentlib", sizeof ("-agentlib") - 1))
	{
          char *strPtr;
	                                              
          if (strlen(option_string) > (sizeof ("-agentlib:") - 1))
            strPtr = &option_string[sizeof ("-agentlib:") - 1];
          else
            {
              fprintf (stderr,
                "libgcj: Malformed agentlib argument %s: expected lib name\n",
                option_string);
              return -1;
            }

          // These are optional arguments to pass to the agent library.
          jvmti_agent_opts = strchr (strPtr, '=');
   
          if (! strncmp (strPtr, "jdwp", 4))
            {    	
              // We want to run JDWP here so set the correct variables.
              remoteDebug = true;
              jdwpOptions = ++jvmti_agent_opts;
            }
          else
            {
              jint nameLength;
   
              if (jvmti_agent_opts == NULL)
                nameLength = strlen (strPtr);
              else
                {
                  nameLength = jvmti_agent_opts - strPtr;
                  jvmti_agent_opts++;
                }
               
              char lib_name[nameLength + 3 + 1];
              strcpy (lib_name, "lib");
              strncat (lib_name, strPtr, nameLength);
      
              jint result = load_jvmti_agent (lib_name);
      
              if (result < 0)
	        {
	          return -1;
	        }

	      // Mark JVMTI active
	      JVMTI::enabled = true;
            }
    
          continue;
	}
      else if (! strncmp (option_string, "-agentpath:", 
                          sizeof ("-agentpath:") - 1))
	{
          char *strPtr;
	                                              
          if (strlen(option_string) > 10)
            strPtr = &option_string[10];
          else
            {
              fprintf (stderr,
                "libgcj: Malformed agentlib argument %s: expected lib path\n",
                option_string);
              return -1;
            }
		
          // These are optional arguments to pass to the agent library.
          jvmti_agent_opts = strchr (strPtr, '=');
    
          jint nameLength;
   
          if (jvmti_agent_opts == NULL)
            nameLength = strlen (strPtr);
          else
            {
              nameLength = jvmti_agent_opts - strPtr;
              jvmti_agent_opts++;
            }
    
          char lib_name[nameLength + 3 + 1];
          strcpy (lib_name, "lib");
          strncat (lib_name, strPtr, nameLength);
          jint result = load_jvmti_agent (strPtr);

          if (result < 0)
            {
              return -1;
            }
	
	  // Mark JVMTI active
	  JVMTI::enabled = true;
          continue;
	}
#endif // INTERPRETER
      else
        {
	  int r = -1;
          if (option_string[0] == '_')
	    r = parse_x_arg (option_string + 1);
	  else if (! strncmp (option_string, "-X", 2))
	    r = parse_x_arg (option_string + 2);

	  if (r == -1 && ! vm_args->ignoreUnrecognized)
            {
              fprintf (stderr, "libgcj: unknown option: %s\n", option_string);
              return -1;
            }
	}
    }
  return 0;
}

jint
_Jv_CreateJavaVM (JvVMInitArgs* vm_args)
{
  using namespace gcj;

  if (runtimeInitialized)
    return -1;

  runtimeInitialized = true;
  startTime = _Jv_platform_gettimeofday();

  jint result = parse_init_args (vm_args);
  if (result < 0)
    return -1;

  PROCESS_GCJ_PROPERTIES;

  /* Threads must be initialized before the GC, so that it inherits the
     signal mask.  */
  _Jv_InitThreads ();
  _Jv_InitGC ();
  _Jv_InitializeSyncMutex ();
  
#ifdef INTERPRETER
  _Jv_InitInterpreter ();
#endif  

#ifdef HANDLE_SEGV
  INIT_SEGV;
#endif

#ifdef HANDLE_FPE
  INIT_FPE;
#endif

  /* Initialize Utf8 constants declared in jvm.h. */
  void_signature = _Jv_makeUtf8Const ("()V", 3);
  clinit_name = _Jv_makeUtf8Const ("<clinit>", 8);
  init_name = _Jv_makeUtf8Const ("<init>", 6);
  finit_name = _Jv_makeUtf8Const ("finit$", 6);

  /* Initialize built-in classes to represent primitive TYPEs. */
  _Jv_InitPrimClass (&_Jv_byteClass,    "byte",    'B', 1);
  _Jv_InitPrimClass (&_Jv_shortClass,   "short",   'S', 2);
  _Jv_InitPrimClass (&_Jv_intClass,     "int",     'I', 4);
  _Jv_InitPrimClass (&_Jv_longClass,    "long",    'J', 8);
  _Jv_InitPrimClass (&_Jv_booleanClass, "boolean", 'Z', 1);
  _Jv_InitPrimClass (&_Jv_charClass,    "char",    'C', 2);
  _Jv_InitPrimClass (&_Jv_floatClass,   "float",   'F', 4);
  _Jv_InitPrimClass (&_Jv_doubleClass,  "double",  'D', 8);
  _Jv_InitPrimClass (&_Jv_voidClass,    "void",    'V', 0);

  // We have to initialize this fairly early, to avoid circular class
  // initialization.  In particular we want to start the
  // initialization of ClassLoader before we start the initialization
  // of VMClassLoader.
  _Jv_InitClass (&java::lang::ClassLoader::class$);

  // Set up the system class loader and the bootstrap class loader.
  gnu::gcj::runtime::ExtensionClassLoader::initialize();
  java::lang::VMClassLoader::initialize(JvNewStringLatin1(TOOLEXECLIBDIR));

  _Jv_RegisterBootstrapPackages();

  no_memory = new java::lang::OutOfMemoryError;

#ifdef USE_LTDL
  LTDL_SET_PRELOADED_SYMBOLS ();
#endif

  _Jv_platform_initialize ();

  _Jv_JNI_Init ();

#ifdef INTERPRETER
  _Jv_JVMTI_Init ();
#endif

  _Jv_GCInitializeFinalizers (&::gnu::gcj::runtime::FinalizerThread::finalizerReady);

  // Start the GC finalizer thread.  A VirtualMachineError can be
  // thrown by the runtime if, say, threads aren't available.
  try
    {
      using namespace gnu::gcj::runtime;
      FinalizerThread *ft = new FinalizerThread ();
      ft->start ();
    }
  catch (java::lang::VirtualMachineError *ignore)
    {
    }

  runtimeInitialized = true;

  return 0;
}

void
_Jv_RunMain (JvVMInitArgs *vm_args, jclass klass, const char *name, int argc,
             const char **argv, bool is_jar)
{
#ifndef DISABLE_MAIN_ARGS
  _Jv_SetArgs (argc, argv);
#endif

  java::lang::Runtime *runtime = NULL;

  try
    {
      if (_Jv_CreateJavaVM (vm_args) < 0)
	{
	  fprintf (stderr, "libgcj: couldn't create virtual machine\n");
	  exit (1);
	}
      
      if (vm_args == NULL)
	gcj::vmArgs = JvConvertArgv(0, NULL);
      else
	{
	  const char* vmArgs[vm_args->nOptions];
	  const char** vmPtr = vmArgs;
	  struct _Jv_VMOption* optionPtr = vm_args->options;
	  for (int i = 0; i < vm_args->nOptions; ++i)
	    *vmPtr++ = (*optionPtr++).optionString;
	  gcj::vmArgs = JvConvertArgv(vm_args->nOptions, vmArgs);
	}

      // Get the Runtime here.  We want to initialize it before searching
      // for `main'; that way it will be set up if `main' is a JNI method.
      runtime = java::lang::Runtime::getRuntime ();

#ifdef DISABLE_MAIN_ARGS
      arg_vec = JvConvertArgv (0, 0);
#else      
      arg_vec = JvConvertArgv (argc - 1, argv + 1);
#endif

      using namespace gnu::java::lang;
      if (klass)
	main_thread = new MainThread (klass, arg_vec);
      else
	main_thread = new MainThread (JvNewStringUTF (name),
				      arg_vec, is_jar);
      _Jv_AttachCurrentThread (main_thread);

#ifdef INTERPRETER
      // Start JVMTI if an agent function has been found.
      if (jvmti_agentonload)
        (*jvmti_agentonload) (_Jv_GetJavaVM (), jvmti_agent_opts, NULL);

      // Start JDWP
      if (remoteDebug)
	{
	  using namespace gnu::classpath::jdwp;
	  VMVirtualMachine::initialize ();
	  Jdwp *jdwp = new Jdwp ();
	  jdwp->setDaemon (true);	  
	  jdwp->configure (JvNewStringLatin1 (jdwpOptions));
	  jdwp->start ();

	  // Wait for JDWP to initialize and start
	  jdwp->join ();
	}
      // Send VMInit
      if (JVMTI_REQUESTED_EVENT (VMInit))
	_Jv_JVMTI_PostEvent (JVMTI_EVENT_VM_INIT, main_thread);
#endif // INTERPRETER
    }
  catch (java::lang::Throwable *t)
    {
      java::lang::System::err->println (JvNewStringLatin1 
        ("Exception during runtime initialization"));
      t->printStackTrace();
      if (runtime)
	java::lang::Runtime::exitNoChecksAccessor (1);
      // In case the runtime creation failed.
      ::exit (1);
    }

  _Jv_ThreadRun (main_thread);

#ifdef INTERPRETER
  // Send VMDeath
  if (JVMTI_REQUESTED_EVENT (VMDeath))
    {
      java::lang::Thread *thread = java::lang::Thread::currentThread ();
      JNIEnv *jni_env = _Jv_GetCurrentJNIEnv ();
      _Jv_JVMTI_PostEvent (JVMTI_EVENT_VM_DEATH, thread, jni_env);
    }

  // Run JVMTI AgentOnUnload if it exists and an agent is loaded.
  if (jvmti_agentonunload)
    (*jvmti_agentonunload) (_Jv_GetJavaVM ());
#endif // INTERPRETER

  // If we got here then something went wrong, as MainThread is not
  // supposed to terminate.
  ::exit (1);
}

void
_Jv_RunMain (jclass klass, const char *name, int argc, const char **argv, 
	     bool is_jar)
{
  _Jv_RunMain (NULL, klass, name, argc, argv, is_jar);
}

void
JvRunMain (jclass klass, int argc, const char **argv)
{
  _Jv_RunMain (klass, NULL, argc, argv, false);
}

void
JvRunMainName (const char *name, int argc, const char **argv)
{
  _Jv_RunMain (NULL, name, argc, argv, false);
}



// Parse a string and return a heap size.
static size_t
parse_memory_size (const char *spec)
{
  char *end;
  unsigned long val = strtoul (spec, &end, 10);
  if (*end == 'k' || *end == 'K')
    val *= 1024;
  else if (*end == 'm' || *end == 'M')
    val *= 1048576;
  return (size_t) val;
}

// Set the initial heap size.  This might be ignored by the GC layer.
// This must be called before _Jv_RunMain.
void
_Jv_SetInitialHeapSize (const char *arg)
{
  size_t size = parse_memory_size (arg);
  _Jv_GCSetInitialHeapSize (size);
}

// Set the maximum heap size.  This might be ignored by the GC layer.
// This must be called before _Jv_RunMain.
void
_Jv_SetMaximumHeapSize (const char *arg)
{
  size_t size = parse_memory_size (arg);
  _Jv_GCSetMaximumHeapSize (size);
}

void
_Jv_SetStackSize (const char *arg)
{
  size_t size = parse_memory_size (arg);
  gcj::stack_size = size;
}

void *
_Jv_Malloc (jsize size)
{
  if (__builtin_expect (size == 0, false))
    size = 1;
  void *ptr = malloc ((size_t) size);
  if (__builtin_expect (ptr == NULL, false))
    throw no_memory;
  return ptr;
}

void *
_Jv_Realloc (void *ptr, jsize size)
{
  if (__builtin_expect (size == 0, false))
    size = 1;
  ptr = realloc (ptr, (size_t) size);
  if (__builtin_expect (ptr == NULL, false))
    throw no_memory;
  return ptr;
}

void *
_Jv_MallocUnchecked (jsize size)
{
  if (__builtin_expect (size == 0, false))
    size = 1;
  return malloc ((size_t) size);
}

void
_Jv_Free (void* ptr)
{
  return free (ptr);
}



// In theory, these routines can be #ifdef'd away on machines which
// support divide overflow signals.  However, we never know if some
// code might have been compiled with "-fuse-divide-subroutine", so we
// always include them in libgcj.

jint
_Jv_divI (jint dividend, jint divisor)
{
  if (__builtin_expect (divisor == 0, false))
    {
      java::lang::ArithmeticException *arithexception 
	= new java::lang::ArithmeticException (JvNewStringLatin1 ("/ by zero"));      
      throw arithexception;
    }
  
  if (dividend == (jint) 0x80000000L && divisor == -1)
    return dividend;

  return dividend / divisor;
}

jint
_Jv_remI (jint dividend, jint divisor)
{
  if (__builtin_expect (divisor == 0, false))
    {
      java::lang::ArithmeticException *arithexception 
	= new java::lang::ArithmeticException (JvNewStringLatin1 ("/ by zero"));      
      throw arithexception;
    }
  
  if (dividend == (jint) 0x80000000L && divisor == -1)
    return 0;
  
  return dividend % divisor;
}

jlong
_Jv_divJ (jlong dividend, jlong divisor)
{
  if (__builtin_expect (divisor == 0, false))
    {
      java::lang::ArithmeticException *arithexception 
	= new java::lang::ArithmeticException (JvNewStringLatin1 ("/ by zero"));      
      throw arithexception;
    }

  if (dividend == (jlong) 0x8000000000000000LL && divisor == -1)
    return dividend;

  return dividend / divisor;
}

jlong
_Jv_remJ (jlong dividend, jlong divisor)
{
  if (__builtin_expect (divisor == 0, false))
    {
      java::lang::ArithmeticException *arithexception 
	= new java::lang::ArithmeticException (JvNewStringLatin1 ("/ by zero"));      
      throw arithexception;
    }

  if (dividend == (jlong) 0x8000000000000000LL && divisor == -1)
    return 0;

  return dividend % divisor;
}



// Return true if SELF_KLASS can access a field or method in
// OTHER_KLASS.  The field or method's access flags are specified in
// FLAGS.
jboolean
_Jv_CheckAccess (jclass self_klass, jclass other_klass, jint flags)
{
  using namespace java::lang::reflect;
  return ((self_klass == other_klass)
	  || ((flags & Modifier::PUBLIC) != 0)
	  || (((flags & Modifier::PROTECTED) != 0)
	      && _Jv_IsAssignableFromSlow (self_klass, other_klass))
	  || (((flags & Modifier::PRIVATE) == 0)
	      && _Jv_ClassNameSamePackage (self_klass->name,
					   other_klass->name)));
}

// Prepend GCJ_VERSIONED_LIBDIR to a module search path stored in a C
// char array, if the path is not already prefixed by
// GCJ_VERSIONED_LIBDIR.  Return a newly JvMalloc'd char buffer.  The
// result should be freed using JvFree.
char*
_Jv_PrependVersionedLibdir (char* libpath)
{
  char* retval = 0;

  if (libpath && libpath[0] != '\0')
    {
      if (! strncmp (libpath,
                     GCJ_VERSIONED_LIBDIR,
                     sizeof (GCJ_VERSIONED_LIBDIR) - 1))
        {
          // LD_LIBRARY_PATH is already prefixed with
          // GCJ_VERSIONED_LIBDIR.
          retval = (char*) _Jv_Malloc (strlen (libpath) + 1);
          strcpy (retval, libpath);
        }
      else
        {
          // LD_LIBRARY_PATH is not prefixed with
          // GCJ_VERSIONED_LIBDIR.
	  char path_sep[2];
	  path_sep[0] = (char) _Jv_platform_path_separator;
	  path_sep[1] = '\0';
          jsize total = ((sizeof (GCJ_VERSIONED_LIBDIR) - 1)
			 + 1 /* path separator */ + strlen (libpath) + 1);
          retval = (char*) _Jv_Malloc (total);
          strcpy (retval, GCJ_VERSIONED_LIBDIR);
          strcat (retval, path_sep);
          strcat (retval, libpath);
        }
    }
  else
    {
      // LD_LIBRARY_PATH was not specified or is empty.
      retval = (char*) _Jv_Malloc (sizeof (GCJ_VERSIONED_LIBDIR));
      strcpy (retval, GCJ_VERSIONED_LIBDIR);
    }

  return retval;
}
