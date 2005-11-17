// jvm.h - Header file for private implementation information. -*- c++ -*-

/* Copyright (C) 1998, 1999, 2000, 2001, 2002, 2003, 2004, 2005  Free Software Foundation

   This file is part of libgcj.

This software is copyrighted work licensed under the terms of the
Libgcj License.  Please consult the file "LIBGCJ_LICENSE" for
details.  */

#ifndef __JAVA_JVM_H__
#define __JAVA_JVM_H__

// Define this before including jni.h.
// jni.h is included by jvmpi.h, which might be included.  We define
// this unconditionally because it is convenient and it lets other
// files include jni.h without difficulty.
#define __GCJ_JNI_IMPL__

#include <gcj/javaprims.h>

#include <java-assert.h>
#include <java-threads.h>
// Must include java-gc.h before Object.h for the implementation.
#include <java-gc.h>

#include <java/lang/Object.h>

// Include cni.h before field.h to enable all definitions.  FIXME.
#include <gcj/cni.h>
#include <gcj/field.h>

/* Macro for possible unused arguments.  */
#define MAYBE_UNUSED __attribute__((__unused__))

/* Structure of the virtual table.  */
struct _Jv_VTable
{
#ifdef __ia64__
  typedef struct { void *pc, *gp; } vtable_elt;
#else
  typedef void *vtable_elt;
#endif
  jclass clas;
  void *gc_descr;

  // This must be last, as derived classes "extend" this by
  // adding new data members.
  vtable_elt method[1];

#ifdef __ia64__
  void *get_method(int i) { return &method[i]; }
  void set_method(int i, void *fptr) { method[i] = *(vtable_elt *)fptr; }
  void *get_finalizer()
  {
    // We know that get_finalizer is only used for checking whether
    // this object needs to have a finalizer registered.  So it is
    // safe to simply return just the PC component of the vtable
    // slot.
    return ((vtable_elt *)(get_method(0)))->pc;
  }
#else
  void *get_method(int i) { return method[i]; }
  void set_method(int i, void *fptr) { method[i] = fptr; }
  void *get_finalizer() { return get_method(0); }
#endif

  static size_t vtable_elt_size() { return sizeof(vtable_elt); }

  // Given a method index, return byte offset from the vtable pointer.
  static jint idx_to_offset (int index)
  {
    return (2 * sizeof (void *)) + (index * vtable_elt_size ());
  }
  static _Jv_VTable *new_vtable (int count);
};

// Number of virtual methods on object.  FIXME: it sucks that we have
// to keep this up to date by hand.
#define NUM_OBJECT_METHODS 5

union _Jv_word
{
  jobject o;
  jint i;			// Also stores smaller integral types.
  jfloat f;
  jint ia[1];			// Half of _Jv_word2.
  void* p;

#if SIZEOF_VOID_P == 8
  // We can safely put a long or a double in here without increasing
  // the size of _Jv_Word; we take advantage of this in the interpreter.
  jlong l;
  jdouble d;
#endif

  jclass                     clazz;
  jstring                    string;
  struct _Jv_Field          *field;
  struct _Jv_Utf8Const      *utf8;
  struct _Jv_ResolvedMethod *rmethod;
};

union _Jv_word2
{
  jint ia[2];
  jlong l;
  jdouble d;
};                              

union _Jv_value
{
  jbyte byte_value;
  jshort short_value;
  jchar char_value;
  jint int_value;
  jlong long_value;
  jfloat float_value;
  jdouble double_value;
  jobject object_value;
};

// An instance of this type is used to represent a single frame in a
// backtrace.  If the interpreter has been built, we also include
// information about the interpreted method.
struct _Jv_frame_info
{
  // PC value.
  void *addr;
#ifdef INTERPRETER
  // Actually a _Jv_InterpMethod, but we don't want to include
  // java-interp.h everywhere.
  void *interp;
#endif // INTERPRETER
};

/* Extract a character from a Java-style Utf8 string.
 * PTR points to the current character.
 * LIMIT points to the end of the Utf8 string.
 * PTR is incremented to point after the character thta gets returns.
 * On an error, -1 is returned. */
#define UTF8_GET(PTR, LIMIT) \
  ((PTR) >= (LIMIT) ? -1 \
   : *(PTR) < 128 ? *(PTR)++ \
   : (*(PTR)&0xE0) == 0xC0 && ((PTR)+=2)<=(LIMIT) && ((PTR)[-1]&0xC0) == 0x80 \
   ? (((PTR)[-2] & 0x1F) << 6) + ((PTR)[-1] & 0x3F) \
   : (*(PTR) & 0xF0) == 0xE0 && ((PTR) += 3) <= (LIMIT) \
   && ((PTR)[-2] & 0xC0) == 0x80 && ((PTR)[-1] & 0xC0) == 0x80 \
   ? (((PTR)[-3]&0x0F) << 12) + (((PTR)[-2]&0x3F) << 6) + ((PTR)[-1]&0x3F) \
   : ((PTR)++, -1))

extern int _Jv_strLengthUtf8(char* str, int len);

typedef struct _Jv_Utf8Const Utf8Const;
_Jv_Utf8Const *_Jv_makeUtf8Const (char *s, int len);
_Jv_Utf8Const *_Jv_makeUtf8Const (jstring string);
extern jboolean _Jv_equalUtf8Consts (const _Jv_Utf8Const *, const _Jv_Utf8Const *);
extern jboolean _Jv_equal (_Jv_Utf8Const *, jstring, jint);
extern jboolean _Jv_equaln (_Jv_Utf8Const *, jstring, jint);

/* Helper class which converts a jstring to a temporary char*.
   Uses the supplied buffer, if non-null. Otherwise, allocates
   the buffer on the heap. Use the JV_TEMP_UTF_STRING macro,
   which follows, to automatically allocate a stack buffer if
   the string is small enough. */
class _Jv_TempUTFString
{
public:
  _Jv_TempUTFString(jstring jstr, char* buf=0);
  ~_Jv_TempUTFString();

// Accessors
  operator const char*() const
  {
    return buf_;
  }
  const char* buf() const
  {
    return buf_;
  }
  char* buf()
  {
    return buf_;
  }

private:
  char* buf_;
  bool heapAllocated_;
};

inline _Jv_TempUTFString::_Jv_TempUTFString (jstring jstr, char* buf)
  : buf_(0), heapAllocated_(false)
{
  if (!jstr) return;
  jsize len = JvGetStringUTFLength (jstr);
  if (buf)
    buf_ = buf;
  else
    {
      buf_ = (char*) _Jv_Malloc (len+1);
      heapAllocated_ = true;
    }

  JvGetStringUTFRegion (jstr, 0, jstr->length(), buf_);
  buf_[len] = '\0';
}

inline _Jv_TempUTFString::~_Jv_TempUTFString ()
{
  if (heapAllocated_)
    _Jv_Free (buf_);
}

/* Macro which uses _Jv_TempUTFString. Allocates a stack-based
   buffer if the string and its null terminator are <= 256
   characters in length. Otherwise, a heap-based buffer is
   used. The parameters to this macro are the variable name
   which is an instance of _Jv_TempUTFString (above) and a
   jstring.
   
   Sample Usage:
   
   jstring jstr = getAJString();
   JV_TEMP_UTF_STRING(utfstr, jstr);
   printf("The string is: %s\n", utfstr.buf());
   
 */
#define JV_TEMP_UTF_STRING(utfstr, jstr) \
  jstring utfstr##thejstr = (jstr); \
  jsize utfstr##_len = utfstr##thejstr ? JvGetStringUTFLength (utfstr##thejstr) + 1 : 0; \
  char utfstr##_buf[utfstr##_len <= 256 ? utfstr##_len : 0]; \
  _Jv_TempUTFString utfstr(utfstr##thejstr, sizeof(utfstr##_buf)==0 ? 0 : utfstr##_buf)

namespace gcj
{
  /* Some constants used during lookup of special class methods.  */
  extern _Jv_Utf8Const *void_signature; /* "()V" */
  extern _Jv_Utf8Const *clinit_name;    /* "<clinit>" */
  extern _Jv_Utf8Const *init_name;      /* "<init>" */
  extern _Jv_Utf8Const *finit_name;     /* "finit$", */
  
  /* Set to true by _Jv_CreateJavaVM. */
  extern bool runtimeInitialized;

  /* Print out class names as they are initialized. */
  extern bool verbose_class_flag;
  
  /* When true, enable the bytecode verifier and BC-ABI verification. */
  extern bool verifyClasses;

  /* Thread stack size specified by the -Xss runtime argument. */
  extern size_t stack_size;
}

// This class handles all aspects of class preparation and linking.
class _Jv_Linker
{
private:
  static _Jv_Field *find_field_helper(jclass, _Jv_Utf8Const *, _Jv_Utf8Const *,
				      jclass *);
  static _Jv_Field *find_field(jclass, jclass, jclass *, _Jv_Utf8Const *,
			       _Jv_Utf8Const *);
  static void prepare_constant_time_tables(jclass);
  static jshort get_interfaces(jclass, _Jv_ifaces *);
  static void link_symbol_table(jclass);
  static void link_exception_table(jclass);
  static void layout_interface_methods(jclass);
  static void layout_vtable_methods(jclass);
  static void set_vtable_entries(jclass, _Jv_VTable *);
  static void make_vtable(jclass);
  static void ensure_fields_laid_out(jclass);
  static void ensure_class_linked(jclass);
  static void ensure_supers_installed(jclass);
  static void add_miranda_methods(jclass, jclass);
  static void ensure_method_table_complete(jclass);
  static void verify_class(jclass);
  static jshort find_iindex(jclass *, jshort *, jshort);
  static jshort indexof(void *, void **, jshort);
  static int get_alignment_from_class(jclass);
  static void generate_itable(jclass, _Jv_ifaces *, jshort *);
  static jshort append_partial_itable(jclass, jclass, void **, jshort);
  static _Jv_Method *search_method_in_class (jclass, jclass,
					     _Jv_Utf8Const *,
					     _Jv_Utf8Const *);

public:

  static bool has_field_p (jclass, _Jv_Utf8Const *);
  static void print_class_loaded (jclass);
  static void resolve_class_ref (jclass, jclass *);
  static void wait_for_state(jclass, int);
  static _Jv_word resolve_pool_entry (jclass, int);
  static void resolve_field (_Jv_Field *, java::lang::ClassLoader *);
  static void verify_type_assertions (jclass);
};

/* Type of pointer used as finalizer.  */
typedef void _Jv_FinalizerFunc (jobject);

/* Allocate space for a new Java object.  */
void *_Jv_AllocObj (jsize size, jclass cl) __attribute__((__malloc__));
/* Allocate space for a potentially uninitialized pointer-free object.
   Interesting only with JV_HASH_SYNCHRONIZATION.  */
void *_Jv_AllocPtrFreeObj (jsize size, jclass cl) __attribute__((__malloc__));
/* Allocate space for an array of Java objects.  */
void *_Jv_AllocArray (jsize size, jclass cl) __attribute__((__malloc__));
/* Allocate space that is known to be pointer-free.  */
void *_Jv_AllocBytes (jsize size) __attribute__((__malloc__));
/* Allocate space for a new non-Java object, which does not have the usual 
   Java object header but may contain pointers to other GC'ed objects.  */
void *_Jv_AllocRawObj (jsize size) __attribute__((__malloc__));
/* Explicitly throw an out-of-memory exception.	*/
void _Jv_ThrowNoMemory() __attribute__((__noreturn__));
/* Allocate an object with a single pointer.  The first word is reserved
   for the GC, and the second word is the traced pointer.  */
void *_Jv_AllocTraceOne (jsize size /* incl. reserved slot */);
/* Ditto, but for two traced pointers.			   */
void *_Jv_AllocTraceTwo (jsize size /* incl. reserved slot */);
/* Initialize the GC.  */
void _Jv_InitGC (void);
/* Register a finalizer.  */
void _Jv_RegisterFinalizer (void *object, _Jv_FinalizerFunc *method);
/* Compute the GC descriptor for a class */
void * _Jv_BuildGCDescr(jclass);

/* Allocate some unscanned, unmoveable memory.  Return NULL if out of
   memory.  */
void *_Jv_MallocUnchecked (jsize size) __attribute__((__malloc__));

/* Initialize finalizers.  The argument is a function to be called
   when a finalizer is ready to be run.  */
void _Jv_GCInitializeFinalizers (void (*notifier) (void));
/* Run finalizers for objects ready to be finalized..  */
void _Jv_RunFinalizers (void);
/* Run all finalizers.  Should be called only before exit.  */
void _Jv_RunAllFinalizers (void);
/* Perform a GC.  */
void _Jv_RunGC (void);
/* Disable and enable GC.  */
void _Jv_DisableGC (void);
void _Jv_EnableGC (void);
/* Register a disappearing link.  This is a field F which should be
   cleared when *F is found to be inaccessible.  This is used in the
   implementation of java.lang.ref.Reference.  */
void _Jv_GCRegisterDisappearingLink (jobject *objp);
/* Return true if OBJECT should be reclaimed.  This is used to
   implement soft references.  */
jboolean _Jv_GCCanReclaimSoftReference (jobject obj);

/* Register a finalizer for a String object.  This is only used by
   the intern() implementation.  */
void _Jv_RegisterStringFinalizer (jobject str);
/* This is called to actually finalize a possibly-intern()d String.  */
void _Jv_FinalizeString (jobject str);

/* Return approximation of total size of heap.  */
long _Jv_GCTotalMemory (void);
/* Return approximation of total free memory.  */
long _Jv_GCFreeMemory (void);

/* Set initial heap size.  If SIZE==0, ignore.  Should be run before
   _Jv_InitGC.  Not required to have any actual effect.  */
void _Jv_GCSetInitialHeapSize (size_t size);

/* Set maximum heap size.  If SIZE==0, unbounded.  Should be run
   before _Jv_InitGC.  Not required to have any actual effect.  */
void _Jv_GCSetMaximumHeapSize (size_t size);

/* External interface to setting the heap size.  Parses ARG (a number
   which can optionally have "k" or "m" appended and calls
   _Jv_GCSetInitialHeapSize.  */
void _Jv_SetInitialHeapSize (const char *arg);

/* External interface to setting the maximum heap size.  Parses ARG (a
   number which can optionally have "k" or "m" appended and calls
   _Jv_GCSetMaximumHeapSize.  */
void _Jv_SetMaximumHeapSize (const char *arg);

/* Set the stack size for threads.  Parses ARG, a number which can 
   optionally have "k" or "m" appended.  */
void _Jv_SetStackSize (const char *arg);

extern "C" void JvRunMain (jclass klass, int argc, const char **argv);
void _Jv_RunMain (jclass klass, const char *name, int argc, const char **argv, 
		  bool is_jar);

void _Jv_RunMain (struct _Jv_VMInitArgs *vm_args, jclass klass,
                  const char *name, int argc, const char **argv, bool is_jar);

// Delayed until after _Jv_AllocBytes is declared.
//
// Note that we allocate this as unscanned memory -- the vtables
// are handled specially by the GC.

inline _Jv_VTable *
_Jv_VTable::new_vtable (int count)
{
  size_t size = sizeof(_Jv_VTable) + (count - 1) * vtable_elt_size ();
  return (_Jv_VTable *) _Jv_AllocBytes (size);
}

// Determine if METH gets an entry in a VTable.
static inline jboolean _Jv_isVirtualMethod (_Jv_Method *meth)
{
  using namespace java::lang::reflect;
  return (((meth->accflags & (Modifier::STATIC | Modifier::PRIVATE)) == 0)
          && meth->name->first() != '<');
}

// This function is used to determine the hash code of an object.
inline jint
_Jv_HashCode (jobject obj)
{
  // This was chosen to yield relatively well distributed results on
  // both 32- and 64-bit architectures.  Note 0x7fffffff is prime.
  // FIXME: we assume sizeof(long) == sizeof(void *).
  return (jint) ((unsigned long) obj % 0x7fffffff);
}

// Return a raw pointer to the elements of an array given the array
// and its element type.  You might think we could just pick a single
// array type and use elements() on it, but we can't because we must
// account for alignment of the element type.  When ARRAY is null, we
// obtain the number of bytes taken by the base part of the array.
inline char *
_Jv_GetArrayElementFromElementType (jobject array,
				    jclass element_type)
{
  char *elts;
  if (element_type == JvPrimClass (byte))
    elts = (char *) elements ((jbyteArray) array);
  else if (element_type == JvPrimClass (short))
    elts = (char *) elements ((jshortArray) array);
  else if (element_type == JvPrimClass (int))
    elts = (char *) elements ((jintArray) array);
  else if (element_type == JvPrimClass (long))
    elts = (char *) elements ((jlongArray) array);
  else if (element_type == JvPrimClass (boolean))
    elts = (char *) elements ((jbooleanArray) array);
  else if (element_type == JvPrimClass (char))
    elts = (char *) elements ((jcharArray) array);
  else if (element_type == JvPrimClass (float))
    elts = (char *) elements ((jfloatArray) array);
  else if (element_type == JvPrimClass (double))
    elts = (char *) elements ((jdoubleArray) array);
  else
    elts = (char *) elements ((jobjectArray) array);
  return elts;
}

extern "C" void _Jv_ThrowBadArrayIndex (jint bad_index)
  __attribute__((noreturn));
extern "C" void _Jv_ThrowNullPointerException (void)
  __attribute__((noreturn));
extern "C" jobject _Jv_NewArray (jint type, jint size)
  __attribute__((__malloc__));
extern "C" jobject _Jv_NewMultiArray (jclass klass, jint dims, ...)
  __attribute__((__malloc__));
extern "C" void *_Jv_CheckCast (jclass klass, jobject obj);
extern "C" void *_Jv_LookupInterfaceMethod (jclass klass, Utf8Const *name,
                                           Utf8Const *signature);
extern "C" void *_Jv_LookupInterfaceMethodIdx (jclass klass, jclass iface, 
                                               int meth_idx);
extern "C" void _Jv_CheckArrayStore (jobject array, jobject obj);
extern "C" void _Jv_RegisterClass (jclass klass);
extern "C" void _Jv_RegisterClasses (const jclass *classes);
extern "C" void _Jv_RegisterClasses_Counted (const jclass *classes,
					     size_t count);
extern "C" void _Jv_RegisterResource (void *vptr);
extern void _Jv_UnregisterClass (_Jv_Utf8Const*, java::lang::ClassLoader*);

extern jclass _Jv_FindClass (_Jv_Utf8Const *name,
			     java::lang::ClassLoader *loader);
extern jclass _Jv_FindClassFromSignature (char *,
					  java::lang::ClassLoader *loader);
extern void _Jv_GetTypesFromSignature (jmethodID method,
				       jclass declaringClass,
				       JArray<jclass> **arg_types_out,
				       jclass *return_type_out);

extern jboolean _Jv_CheckAccess (jclass self_klass, jclass other_klass,
				 jint flags);

extern jobject _Jv_CallAnyMethodA (jobject obj, jclass return_type,
				   jmethodID meth, jboolean is_constructor,
				   JArray<jclass> *parameter_types,
				   jobjectArray args,
				   jclass iface = NULL);

union jvalue;
extern void _Jv_CallAnyMethodA (jobject obj,
				jclass return_type,
				jmethodID meth,
				jboolean is_constructor,
				jboolean is_virtual_call,
				JArray<jclass> *parameter_types,
				jvalue *args,
				jvalue *result,
				jboolean is_jni_call = true,
				jclass iface = NULL);

extern jobject _Jv_NewMultiArray (jclass, jint ndims, jint* dims)
  __attribute__((__malloc__));

/* Checked divide subroutines. */
extern "C"
{
  jint _Jv_divI (jint, jint);
  jint _Jv_remI (jint, jint);
  jlong _Jv_divJ (jlong, jlong);
  jlong _Jv_remJ (jlong, jlong);
}

/* Get the number of arguments (cf. argc) or 0 if our argument
   list was never initialized.  */
extern int _Jv_GetNbArgs (void);

/* Get the specified argument (cf. argv[index]) or "" if either
   our argument list was never initialized or the specified index
   is out of bounds.  */
extern const char * _Jv_GetSafeArg (int index);

/* Sets our argument list. Can be used by programs with non-standard
   entry points.  */
extern void _Jv_SetArgs (int argc, const char **argv);

/* Get the name of the running executable.  */
extern const char *_Jv_ThisExecutable (void);

/* Return a pointer to a symbol in executable or loaded library.  */
void *_Jv_FindSymbolInExecutable (const char *);

/* Initialize JNI.  */
extern void _Jv_JNI_Init (void);

/* Get or set the per-thread JNIEnv used by the invocation API.  */
_Jv_JNIEnv *_Jv_GetCurrentJNIEnv ();
void _Jv_SetCurrentJNIEnv (_Jv_JNIEnv *);

/* Free a JNIEnv. */
void _Jv_FreeJNIEnv (_Jv_JNIEnv *);

/* Free a JNIEnv. */
void _Jv_FreeJNIEnv (_Jv_JNIEnv *);

struct _Jv_JavaVM;
_Jv_JavaVM *_Jv_GetJavaVM (); 

// Some verification functions from defineclass.cc.
bool _Jv_VerifyFieldSignature (_Jv_Utf8Const*sig);
bool _Jv_VerifyMethodSignature (_Jv_Utf8Const*sig);
bool _Jv_VerifyClassName (unsigned char* ptr, _Jv_ushort length);
bool _Jv_VerifyClassName (_Jv_Utf8Const *name);
bool _Jv_VerifyIdentifier (_Jv_Utf8Const *);
bool _Jv_ClassNameSamePackage (_Jv_Utf8Const *name1, _Jv_Utf8Const *name2);

struct _Jv_core_chain
{
  int name_length;
  const char *name;
  int data_length;
  const void *data;

  struct _Jv_core_chain *next;
};

// This is called when new core data is loaded.
extern void (*_Jv_RegisterCoreHook) (_Jv_core_chain *);

_Jv_core_chain *_Jv_FindCore (_Jv_core_chain *node, jstring name);
void _Jv_FreeCoreChain (_Jv_core_chain *chain);

#ifdef ENABLE_JVMPI

#include "jvmpi.h"

extern void (*_Jv_JVMPI_Notify_OBJECT_ALLOC) (JVMPI_Event *event);
extern void (*_Jv_JVMPI_Notify_THREAD_START) (JVMPI_Event *event);
extern void (*_Jv_JVMPI_Notify_THREAD_END) (JVMPI_Event *event);
#endif

/* FIXME: this should really be defined in some more generic place */
#define ROUND(V, A) (((((unsigned) (V))-1) | ((A)-1))+1)

extern void _Jv_RegisterBootstrapPackages ();

#define FLAG_BINARYCOMPAT_ABI (1<<31)  /* Class is built with the BC-ABI. */

#define FLAG_BOOTSTRAP_LOADER (1<<30)  /* Used when defining a class that 
					should be loaded by the bootstrap
					loader.  */

// These are used to find ABI versions we recognize.
#define GCJ_CXX_ABI_VERSION (__GNUC__ * 10000 + __GNUC_MINOR__ * 10)

// This is the old-style BC version ID used by GCJ 4.0.0.
#define OLD_GCJ_40_BC_ABI_VERSION (4 * 10000 + 0 * 10 + 5)

// New style version IDs used by GCJ 4.0.1 and later.
#define GCJ_40_BC_ABI_VERSION (4 * 100000 + 0 * 1000)

inline bool
_Jv_CheckABIVersion (unsigned long value)
{
  // We are compatible with GCJ 4.0.0 BC-ABI classes. This release used a
  // different format for the version ID string.
   if (value == OLD_GCJ_40_BC_ABI_VERSION)
     return true;
     
  // The 20 low-end bits are used for the version number.
  unsigned long version = value & 0xfffff;

  if (value & FLAG_BINARYCOMPAT_ABI)
    {
      int abi_rev = version % 100;
      int abi_ver = version - abi_rev;
      if (abi_ver == GCJ_40_BC_ABI_VERSION && abi_rev <= 0)
	return true;
    }
  else
    // C++ ABI
    return version == GCJ_CXX_ABI_VERSION;
  
  return false;
}

// It makes the source cleaner if we simply always define this
// function.  If the interpreter is not built, it will never return
// 'true'.
extern inline jboolean
_Jv_IsInterpretedClass (jclass c)
{
  return (c->accflags & java::lang::reflect::Modifier::INTERPRETED) != 0;
}

// Return true if the class was compiled with the BC ABI.
extern inline jboolean
_Jv_IsBinaryCompatibilityABI (jclass c)
{
  // There isn't really a better test for the ABI type at this point,
  // that will work once the class has been registered.
  return c->otable_syms || c->atable_syms || c->itable_syms;
}

#endif /* __JAVA_JVM_H__ */
