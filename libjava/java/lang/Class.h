// Class.h - Header file for java.lang.Class.  -*- c++ -*-

/* Copyright (C) 1998, 1999, 2000, 2001, 2002, 2003, 2004, 2005, 2006  Free Software Foundation

   This file is part of libgcj.

This software is copyrighted work licensed under the terms of the
Libgcj License.  Please consult the file "LIBGCJ_LICENSE" for
details.  */

// Written primary using compiler source and Class.java as guides.
#ifndef __JAVA_LANG_CLASS_H__
#define __JAVA_LANG_CLASS_H__

#pragma interface

#include <stddef.h>
#include <java/lang/Object.h>
#include <java/lang/String.h>
#include <java/net/URL.h>
#include <java/lang/reflect/Modifier.h>
#include <java/security/ProtectionDomain.h>
#include <java/lang/Package.h>

// Avoid including SystemClassLoader.h.
extern "Java"
{
  namespace gnu
  {
    namespace gcj
    {
      namespace runtime
      {
        class SystemClassLoader;
      }
    }
  }
}

// We declare these here to avoid including gcj/cni.h.
extern "C" void _Jv_InitClass (jclass klass);
extern "C" void _Jv_RegisterClasses (const jclass *classes);
extern "C" void _Jv_RegisterClasses_Counted (const jclass *classes,
					     size_t count);

// This must be predefined with "C" linkage.
extern "C" void *_Jv_LookupInterfaceMethodIdx (jclass klass, jclass iface, 
                                               int meth_idx);
extern "C" void *_Jv_ResolvePoolEntry (jclass this_class, jint index);

// These are the possible values for the `state' field of the class
// structure.  Note that ordering is important here.  Whenever the
// state changes, one should notify all waiters of this class.
enum
{
  JV_STATE_NOTHING = 0,		// Set by compiler.

  JV_STATE_PRELOADING = 1,	// Can do _Jv_FindClass.
  JV_STATE_LOADING = 3,		// Has super installed.
  JV_STATE_READ = 4,		// Has been completely defined.
  JV_STATE_LOADED = 5,		// Has Miranda methods defined.

  JV_STATE_COMPILED = 6,	// This was a compiled class.

  JV_STATE_PREPARED = 7,	// Layout & static init done.
  JV_STATE_LINKED = 9,		// Strings interned.

  JV_STATE_IN_PROGRESS = 10,	// <clinit> running.

  JV_STATE_ERROR = 12,

  JV_STATE_PHANTOM = 13,	// Bytecode is missing. In many cases we can
                                // work around that. If not, throw a
                                // NoClassDefFoundError.

  JV_STATE_DONE = 14,		// Must be last.


};

struct _Jv_Field;
struct _Jv_VTable;
union _Jv_word;
struct _Jv_ArrayVTable;
class _Jv_Linker;
class _Jv_ExecutionEngine;
class _Jv_CompiledEngine;
class _Jv_InterpreterEngine;

#ifdef INTERPRETER
class _Jv_ClassReader;
class _Jv_InterpClass;
class _Jv_InterpMethod;
#endif

struct _Jv_Constants
{
  jint size;
  jbyte *tags;
  _Jv_word *data;
};

struct _Jv_Method
{
  // Method name.
  _Jv_Utf8Const *name;
  // Method signature.
  _Jv_Utf8Const *signature;
  // Access flags.
  _Jv_ushort accflags;
  // Method's index in the vtable.
  _Jv_ushort index;
  // Pointer to underlying function.
  void *ncode;
  // NULL-terminated list of exception class names; can be NULL if
  // there are none such.
  _Jv_Utf8Const **throws;

  _Jv_Method *getNextMethod ()
  { return this + 1; }
};

// Interface Dispatch Tables 
union _Jv_IDispatchTable
{
  struct
  {
    // Index into interface's ioffsets.
    jshort iindex;
    jshort itable_length;
    // Class Interface dispatch table.
    void **itable;
  } cls;

  struct
  {
    // Offsets into implementation class itables.
    jshort *ioffsets;
  } iface;
};

// Used by _Jv_Linker::get_interfaces ()
struct _Jv_ifaces
{
  jclass *list;
  jshort len;
  jshort count;
};

struct _Jv_MethodSymbol
{
  _Jv_Utf8Const *class_name;
  _Jv_Utf8Const *name;
  _Jv_Utf8Const *signature;
};

struct _Jv_OffsetTable
{
  jint state;
  jint offsets[];
};

struct _Jv_AddressTable
{
  jint state;
  void *addresses[];
};

struct _Jv_CatchClass
{
  java::lang::Class **address;
  _Jv_Utf8Const *classname;
};

// Possible values for the assertion_code field in the type assertion table.
enum
{
  JV_ASSERT_END_OF_TABLE = 0,
  JV_ASSERT_TYPES_COMPATIBLE = 1,
  JV_ASSERT_IS_INSTANTIABLE = 2
};

// Entry in the type assertion table, used to validate type constraints
// for binary compatibility.
struct _Jv_TypeAssertion
{
  jint assertion_code;
  _Jv_Utf8Const *op1;
  _Jv_Utf8Const *op2;
};

#define JV_PRIMITIVE_VTABLE ((_Jv_VTable *) -1)

#define JV_CLASS(Obj) ((jclass) (*(_Jv_VTable **) Obj)->clas)

// Forward declarations for friends of java::lang::Class

// Friend functions implemented in natClass.cc.
_Jv_Method *_Jv_GetMethodLocal (jclass klass, _Jv_Utf8Const *name,
				_Jv_Utf8Const *signature);
jboolean _Jv_IsAssignableFrom (jclass, jclass);
jboolean _Jv_IsAssignableFromSlow (jclass, jclass);
jboolean _Jv_InterfaceAssignableFrom (jclass, jclass);

_Jv_Method* _Jv_LookupDeclaredMethod (jclass, _Jv_Utf8Const *, 
				      _Jv_Utf8Const*, jclass * = NULL);
jfieldID JvGetFirstInstanceField (jclass);
jint JvNumInstanceFields (jclass);
jfieldID JvGetFirstStaticField (jclass);
jint JvNumStaticFields (jclass);

jobject _Jv_AllocObject (jclass);
void *_Jv_AllocObj (jint, jclass);
void *_Jv_AllocPtrFreeObj (jint, jclass);
void *_Jv_AllocArray (jint, jclass);

bool _Jv_getInterfaceMethod(jclass, jclass&, int&, 
			    const _Jv_Utf8Const*,
			    const _Jv_Utf8Const*);

jobject _Jv_JNI_ToReflectedField (_Jv_JNIEnv *, jclass, jfieldID,
				  jboolean);
jobject _Jv_JNI_ToReflectedMethod (_Jv_JNIEnv *, jclass, jmethodID,
				   jboolean);
jfieldID _Jv_FromReflectedField (java::lang::reflect::Field *);

jmethodID _Jv_FromReflectedMethod (java::lang::reflect::Method *);
jmethodID _Jv_FromReflectedConstructor (java::lang::reflect::Constructor *);
jint JvNumMethods (jclass);
jmethodID JvGetFirstMethod (jclass);

#ifdef INTERPRETER
// Finds a desired interpreter method in the given class or NULL if not found
_Jv_InterpMethod* _Jv_FindInterpreterMethod (jclass, jmethodID);
#endif

// Friend classes and functions to implement the ClassLoader
class java::lang::ClassLoader;
class java::lang::VMClassLoader;

class java::io::ObjectOutputStream;
class java::io::ObjectInputStream;
class java::io::ObjectStreamClass;

void _Jv_RegisterClassHookDefault (jclass klass);
void _Jv_RegisterInitiatingLoader (jclass,java::lang::ClassLoader*);
void _Jv_UnregisterInitiatingLoader (jclass,java::lang::ClassLoader*);
void _Jv_UnregisterClass (jclass);
jclass _Jv_FindClassNoException (_Jv_Utf8Const *name,
		      java::lang::ClassLoader *loader);
jclass _Jv_FindClass (_Jv_Utf8Const *name,
		      java::lang::ClassLoader *loader);
jclass _Jv_FindClassInCache (_Jv_Utf8Const *name);
jclass _Jv_PopClass (void);
void _Jv_PushClass (jclass k);
void _Jv_NewArrayClass (jclass element,
			java::lang::ClassLoader *loader,
			_Jv_VTable *array_vtable = 0);
jclass _Jv_NewClass (_Jv_Utf8Const *name, jclass superclass,
		     java::lang::ClassLoader *loader);
void _Jv_InitNewClassFields (jclass klass);

// Friend functions and classes in prims.cc
void _Jv_InitPrimClass (jclass, char *, char, int);
jstring _Jv_GetMethodString (jclass, _Jv_Method *, jclass = NULL);

jboolean _Jv_CheckAccess (jclass self_klass, jclass other_klass,
			  jint flags);
jclass _Jv_GetArrayClass (jclass klass, java::lang::ClassLoader *loader);

jboolean _Jv_IsInterpretedClass (jclass);
jboolean _Jv_IsBinaryCompatibilityABI (jclass);

jboolean _Jv_IsPhantomClass (jclass);

void _Jv_CopyClassesToSystemLoader (gnu::gcj::runtime::SystemClassLoader *);

#ifdef INTERPRETER
void _Jv_InitField (jobject, jclass, int);
#endif

class _Jv_StackTrace;
class _Jv_BytecodeVerifier;
class java::io::VMObjectStreamClass;

void _Jv_sharedlib_register_hook (jclass klass);


class java::lang::Class : public java::lang::Object
{
public:
  static jclass forName (jstring className, jboolean initialize, 
			 java::lang::ClassLoader *loader);
  static jclass forName (jstring className);
  JArray<jclass> *getClasses (void);

  java::lang::ClassLoader *getClassLoader (void);

  // This is an internal method that circumvents the usual security
  // checks when getting the class loader.
  java::lang::ClassLoader *getClassLoaderInternal (void)
  {
    return loader;
  }

  java::lang::reflect::Constructor *getConstructor (JArray<jclass> *);
  JArray<java::lang::reflect::Constructor *> *getConstructors (void);
  java::lang::reflect::Constructor *getDeclaredConstructor (JArray<jclass> *);
  JArray<java::lang::reflect::Constructor *> *getDeclaredConstructors (jboolean);
  java::lang::reflect::Field *getDeclaredField (jstring);
  JArray<java::lang::reflect::Field *> *getDeclaredFields ();
  JArray<java::lang::reflect::Field *> *getDeclaredFields (jboolean);
  java::lang::reflect::Method *getDeclaredMethod (jstring, JArray<jclass> *);
  JArray<java::lang::reflect::Method *> *getDeclaredMethods (void);

  JArray<jclass> *getDeclaredClasses (void);
  JArray<jclass> *getDeclaredClasses (jboolean);
  jclass getDeclaringClass (void);

  java::lang::reflect::Field *getField (jstring);
private:
  JArray<java::lang::reflect::Field *> internalGetFields ();
  java::lang::reflect::Field *getField (jstring, jint);
  jint _getMethods (JArray<java::lang::reflect::Method *> *result,
		    jint offset);
  java::lang::reflect::Field *getPrivateField (jstring);
  java::lang::reflect::Method *getPrivateMethod (jstring, JArray<jclass> *);
  java::security::ProtectionDomain *getProtectionDomain0 ();

  java::lang::reflect::Method *_getMethod (jstring, JArray<jclass> *);
  java::lang::reflect::Method *_getDeclaredMethod (jstring, JArray<jclass> *);

public:
  JArray<java::lang::reflect::Field *> *getFields (void);

  JArray<jclass> *getInterfaces (void);

  void getSignature (java::lang::StringBuffer *buffer);
  static jstring getSignature (JArray<jclass> *, jboolean is_constructor);
  JArray<java::lang::reflect::Method *> *getMethods (void);

  inline jint getModifiers (void)
  {
    return accflags & java::lang::reflect::Modifier::ALL_FLAGS;
  }

  jstring getName (void);

  java::net::URL        *getResource (jstring resourceName);
  java::io::InputStream *getResourceAsStream (jstring resourceName);
  JArray<jobject> *getSigners (void);
  void setSigners(JArray<jobject> *);

  inline jclass getSuperclass (void)
  {
    return superclass;
  }

  inline jclass getInterface (jint n)
  {
    return interfaces[n];
  }

  inline jboolean isArray (void)
    {
      return name->first() == '[';
    }

  inline jclass getComponentType (void)
    {
      return isArray () ? (* (jclass *) &methods) : 0;
    }

  jboolean isAssignableFrom (jclass cls);
  jboolean isInstance (jobject obj);

  inline jboolean isInterface (void)
  {
    return (accflags & java::lang::reflect::Modifier::INTERFACE) != 0;
  }
  
  inline jboolean isPrimitive (void)
    {
      return vtable == JV_PRIMITIVE_VTABLE;
    }

  jobject newInstance (void);
  java::security::ProtectionDomain *getProtectionDomain (void);
  java::lang::Package *getPackage (void);
  jstring toString (void);
  jboolean desiredAssertionStatus (void);

  // FIXME: this probably shouldn't be public.
  jint size (void)
  {
    return size_in_bytes;
  }

  // The index of the first method we declare ourself (as opposed to
  // inheriting).
  inline jint firstMethodIndex (void)
  {
    return vtable_method_count - method_count;
  }
    
  // finalization
  void finalize ();

  // This constructor is used to create Class object for the primitive
  // types. See prims.cc.
  Class ();

  static java::lang::Class class$;

private:   

  void memberAccessCheck (jint flags);

  void initializeClass (void);

  static jstring getPackagePortion (jstring);

  void set_state (jint nstate)
  {
    state = nstate;
    notifyAll ();
  }

  // Friend functions implemented in natClass.cc.
  friend _Jv_Method *::_Jv_GetMethodLocal (jclass klass, _Jv_Utf8Const *name,
					   _Jv_Utf8Const *signature);
  friend jboolean (::_Jv_IsAssignableFrom) (jclass, jclass);
  friend jboolean (::_Jv_IsAssignableFromSlow) (jclass, jclass);
  friend jboolean (::_Jv_InterfaceAssignableFrom) (jclass, jclass);
  friend void *::_Jv_LookupInterfaceMethodIdx (jclass klass, jclass iface, 
					       int method_idx);

  friend void ::_Jv_InitClass (jclass klass);

  friend _Jv_Method* ::_Jv_LookupDeclaredMethod (jclass, _Jv_Utf8Const *, 
						 _Jv_Utf8Const*, jclass *);
  friend jfieldID (::JvGetFirstInstanceField) (jclass);
  friend jint (::JvNumInstanceFields) (jclass);
  friend jfieldID (::JvGetFirstStaticField) (jclass);
  friend jint (::JvNumStaticFields) (jclass);

  friend jobject (::_Jv_AllocObject) (jclass);
  friend void *::_Jv_AllocObj (jint, jclass);
  friend void *::_Jv_AllocPtrFreeObj (jint, jclass);
  friend void *::_Jv_AllocArray (jint, jclass);

  friend jobject (::_Jv_JNI_ToReflectedField) (_Jv_JNIEnv *, jclass, jfieldID,
					       jboolean);
  friend jobject (::_Jv_JNI_ToReflectedMethod) (_Jv_JNIEnv *, jclass, jmethodID,
						jboolean);
  friend jfieldID (::_Jv_FromReflectedField) (java::lang::reflect::Field *);

  friend jmethodID (::_Jv_FromReflectedMethod) (java::lang::reflect::Method *);
  friend jmethodID (::_Jv_FromReflectedConstructor) (java::lang::reflect::Constructor *);
  friend jint (::JvNumMethods) (jclass);
  friend jmethodID (::JvGetFirstMethod) (jclass);
#ifdef INTERPRETER
  friend _Jv_InterpMethod* (::_Jv_FindInterpreterMethod) (jclass klass,
							  jmethodID desired_method);
#endif

  // Friends classes and functions to implement the ClassLoader
  friend class java::lang::ClassLoader;
  friend class java::lang::VMClassLoader;

  friend class java::io::ObjectOutputStream;
  friend class java::io::ObjectInputStream;
  friend class java::io::ObjectStreamClass;

  friend void ::_Jv_RegisterClasses (const jclass *classes);
  friend void ::_Jv_RegisterClasses_Counted (const jclass *classes, 
					     size_t count);
  friend void ::_Jv_RegisterClassHookDefault (jclass klass);
  friend void ::_Jv_RegisterInitiatingLoader (jclass,java::lang::ClassLoader*);
  friend void ::_Jv_UnregisterInitiatingLoader (jclass,java::lang::ClassLoader*);
  friend void ::_Jv_UnregisterClass (jclass);
  friend jclass (::_Jv_FindClassNoException) (_Jv_Utf8Const *name,
				   java::lang::ClassLoader *loader);
  friend jclass (::_Jv_FindClass) (_Jv_Utf8Const *name,
				   java::lang::ClassLoader *loader);
  friend jclass (::_Jv_FindClassInCache) (_Jv_Utf8Const *name);
  friend jclass (::_Jv_PopClass) (void);
  friend void ::_Jv_PushClass (jclass k);
  friend void ::_Jv_NewArrayClass (jclass element,
				   java::lang::ClassLoader *loader,
				   _Jv_VTable *array_vtable);
  friend jclass (::_Jv_NewClass) (_Jv_Utf8Const *name, jclass superclass,
				  java::lang::ClassLoader *loader);
  friend void ::_Jv_InitNewClassFields (jclass klass);

  // in prims.cc
  friend void ::_Jv_InitPrimClass (jclass, char *, char, int);

  friend jstring (::_Jv_GetMethodString) (jclass, _Jv_Method *, jclass);

  friend jboolean (::_Jv_CheckAccess) (jclass self_klass, jclass other_klass,
				   jint flags);
  
  friend bool (::_Jv_getInterfaceMethod) (jclass, jclass&, int&, 
					  const _Jv_Utf8Const*,
					  const _Jv_Utf8Const*);

  friend jclass (::_Jv_GetArrayClass) (jclass klass,
				       java::lang::ClassLoader *loader);

  friend jboolean (::_Jv_IsInterpretedClass) (jclass);
  friend jboolean (::_Jv_IsBinaryCompatibilityABI) (jclass);

  friend jboolean (::_Jv_IsPhantomClass) (jclass);

#ifdef INTERPRETER
  friend void ::_Jv_InitField (jobject, jclass, int);

  friend class ::_Jv_ClassReader;	
  friend class ::_Jv_InterpClass;
  friend class ::_Jv_InterpMethod;
#endif
  friend class ::_Jv_StackTrace;

#ifdef JV_MARKOBJ_DECL
  friend JV_MARKOBJ_DECL;
#endif

  friend class ::_Jv_BytecodeVerifier;
  friend class java::io::VMObjectStreamClass;

  friend class ::_Jv_Linker;
  friend class ::_Jv_ExecutionEngine;
  friend class ::_Jv_CompiledEngine;
  friend class ::_Jv_InterpreterEngine;

  friend void ::_Jv_sharedlib_register_hook (jclass klass);

  friend void *::_Jv_ResolvePoolEntry (jclass this_class, jint index);

  friend void ::_Jv_CopyClassesToSystemLoader (gnu::gcj::runtime::SystemClassLoader *);

  // Chain for class pool.  This also doubles as the ABI version
  // number.  It is only used for this purpose at class registration
  // time, and only for precompiled classes.
  jclass next_or_version;
  // Name of class.
  _Jv_Utf8Const *name;
  // Access flags for class.
  _Jv_ushort accflags;
  // The superclass, or null for Object.
  jclass superclass;
  // Class constants.
  _Jv_Constants constants;
  // Methods.  If this is an array class, then this field holds a
  // pointer to the element type.
  _Jv_Method *methods;
  // Number of methods.  If this class is primitive, this holds the
  // character used to represent this type in a signature.
  jshort method_count;
  // Number of methods in the vtable.
  jshort vtable_method_count;
  // The fields.
  _Jv_Field *fields;
  // Size of instance fields, in bytes.
  jint size_in_bytes;
  // Total number of fields (instance and static).
  jshort field_count;
  // Number of static fields.
  jshort static_field_count;
  // The vtbl for all objects of this class.
  _Jv_VTable *vtable;
  // Virtual method offset table.
  _Jv_OffsetTable *otable;
  // Offset table symbols.
  _Jv_MethodSymbol *otable_syms;
  // Address table
  _Jv_AddressTable *atable;
  _Jv_MethodSymbol *atable_syms;
  // Interface table
  _Jv_AddressTable *itable;
  _Jv_MethodSymbol *itable_syms;
  _Jv_CatchClass *catch_classes;
  // Interfaces implemented by this class.
  jclass *interfaces;
  // The class loader for this class.
  java::lang::ClassLoader *loader;
  // Number of interfaces.
  jshort interface_count;
  // State of this class.
  jbyte state;
  // The thread which has locked this class.  Used during class
  // initialization.
  java::lang::Thread *thread;
  // How many levels of "extends" this class is removed from Object.
  jshort depth;
  // Vector of this class's superclasses, ordered by decreasing depth.
  jclass *ancestors;
  // Interface Dispatch Table.
  _Jv_IDispatchTable *idt;
  // Pointer to the class that represents an array of this class.
  jclass arrayclass;
  // Security Domain to which this class belongs (or null).
  java::security::ProtectionDomain *protectionDomain;
  // Pointer to the type assertion table for this class.
  _Jv_TypeAssertion *assertion_table;
  // Signers of this class (or null).
  JArray<jobject> *hack_signers;
  // Used by Jv_PopClass and _Jv_PushClass to communicate with StackTrace.
  jclass chain;
  // Additional data, specific to the generator (JIT, native,
  // interpreter) of this class.
  void *aux_info;
  // Execution engine.
  _Jv_ExecutionEngine *engine;
};

// Inline functions that are friends of java::lang::Class

inline void _Jv_InitClass (jclass klass)
{
  if (__builtin_expect (klass->state == JV_STATE_DONE, true))
    return;
  klass->initializeClass ();  
}

// Return array class corresponding to element type KLASS, creating it if
// necessary.
inline jclass
_Jv_GetArrayClass (jclass klass, java::lang::ClassLoader *loader)
{
  extern void _Jv_NewArrayClass (jclass element,
				 java::lang::ClassLoader *loader,
				 _Jv_VTable *array_vtable = 0);
  if (__builtin_expect (!klass->arrayclass, false))
    _Jv_NewArrayClass (klass, loader);
  return klass->arrayclass;
}

#endif /* __JAVA_LANG_CLASS_H__ */
