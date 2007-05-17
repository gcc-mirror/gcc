// natVMMethod.cc -- native support for VMMethod

/* Copyright (C) 2006, 2007 Free Software Foundation

   This file is part of libgcj.

This software is copyrighted work licensed under the terms of the
Libgcj License.  Please consult the file "LIBGCJ_LICENSE" for
details.  */

#include <config.h>
#include <gcj/cni.h>
#include <java-interp.h>
#include <jvmti.h>
#include "jvmti-int.h"

#include <java/lang/reflect/Modifier.h>
#include <gnu/classpath/jdwp/VMMethod.h>
#include <gnu/classpath/jdwp/exception/AbsentInformationException.h>
#include <gnu/classpath/jdwp/exception/InvalidMethodException.h>
#include <gnu/classpath/jdwp/exception/JdwpInternalErrorException.h>
#include <gnu/classpath/jdwp/util/LineTable.h>
#include <gnu/classpath/jdwp/util/VariableTable.h>

using namespace java::lang;

#define CHECK_INTERP_CLASS()	\
do								\
  {								\
    if (!_Jv_IsInterpretedClass (getDeclaringClass ()))	\
      {													\
        ::java::lang::String *msg = JvNewStringLatin1 ("native class"); \
        throw new exception::JdwpInternalErrorException (msg); 			\
      }													\
  }								\
while (0)

jstring
gnu::classpath::jdwp::VMMethod::getName ()
{
  jvmtiEnv *env = _Jv_GetJDWP_JVMTIEnv ();
  jmethodID method = reinterpret_cast<jmethodID> (_methodId);
  char *name;
  env->GetMethodName (method, &name, NULL, NULL);
  jstring string = JvNewStringUTF (name);
  env->Deallocate (reinterpret_cast<unsigned char *> (name));
  return string;
}

jstring
gnu::classpath::jdwp::VMMethod::getSignature ()
{
  jvmtiEnv *env = _Jv_GetJDWP_JVMTIEnv ();
  jmethodID method = reinterpret_cast<jmethodID> (_methodId);
  char *signature;
  env->GetMethodName (method, NULL, &signature, NULL);
  jstring string = JvNewStringUTF (signature);
  env->Deallocate (reinterpret_cast<unsigned char *> (signature));
  return string;
}

jint
gnu::classpath::jdwp::VMMethod::getModifiers ()
{
  jvmtiEnv *env = _Jv_GetJDWP_JVMTIEnv ();
  jmethodID method = reinterpret_cast<jmethodID> (_methodId);
  jint flags;
  env->GetMethodModifiers (method, &flags);

  // If this class is compiled, as far as JDWP is concerned, its methods are 
  // native.  This will set the native flag for these methods.
  if (!_Jv_IsInterpretedClass (getDeclaringClass ()))
    flags |= ::java::lang::reflect::Modifier::NATIVE;

  return flags;
}

gnu::classpath::jdwp::util::LineTable *
gnu::classpath::jdwp::VMMethod::getLineTable ()
{
  CHECK_INTERP_CLASS ();

  jmethodID desired_method = reinterpret_cast<jmethodID> (_methodId);

  _Jv_MethodBase *theMethod
    = _Jv_FindInterpreterMethod (getDeclaringClass (), desired_method);

  if (theMethod == NULL)
    {
      // this should not happen
      ::java::lang::String *msg
	= JvNewStringLatin1 ("could not find method in class");
      throw new exception::JdwpInternalErrorException (msg);
    }

  if (::java::lang::reflect::Modifier::isNative (desired_method->accflags))
    {
      jintArray lines = JvNewIntArray (0);
      jlongArray indices = JvNewLongArray (0);
      return new util::LineTable (-1, -1, lines, indices);
    }

  // get the linetable
  _Jv_InterpMethod *imeth = reinterpret_cast<_Jv_InterpMethod *> (theMethod);
  jlong start;
  jlong end;
  jintArray lines;
  jlongArray indices;
  imeth->get_line_table (start, end, lines, indices);
  return new util::LineTable (start, end, lines, indices);
}


gnu::classpath::jdwp::util::VariableTable*
gnu::classpath::jdwp::VMMethod::getVariableTable ()
{
  using namespace gnu::classpath::jdwp::util;
  
  jvmtiEnv *env = _Jv_GetJDWP_JVMTIEnv ();
	
  CHECK_INTERP_CLASS ();
  
  jmethodID meth = reinterpret_cast<jmethodID> (_methodId);
  jvmtiLocalVariableEntry *var_table;
  jint num_slots, args_len;
  
  jvmtiError jerr = env->GetLocalVariableTable (meth, &num_slots, &var_table);
  
  if (jerr != JVMTI_ERROR_NONE)
    goto error;
  
  jerr = env->GetArgumentsSize (meth, &args_len);
  
  if (jerr != JVMTI_ERROR_NONE)
    {
    error:
      using namespace gnu::classpath::jdwp::exception;
      char *error;
      env->GetErrorName (jerr, &error);
      String *msg = JvNewStringUTF (error);
      env->Deallocate (reinterpret_cast<unsigned char *> (error));
      
      if (jerr == JVMTI_ERROR_NATIVE_METHOD)
        throw new AbsentInformationException (msg);
      else if (jerr == JVMTI_ERROR_INVALID_METHODID)
        throw new InvalidMethodException (_methodId);
      else
        throw new JdwpInternalErrorException (msg);
    }
  
  jlongArray start_pcs = JvNewLongArray (num_slots);
  jlong *start_pcs_ptr = elements (start_pcs);
  jintArray lengths = JvNewIntArray (num_slots);
  jint *lengths_ptr = elements (lengths);
  jintArray slots = JvNewIntArray (num_slots);
  jint *slots_ptr = elements (slots);
  JArray<String *> *names = reinterpret_cast<JArray<String *> *> 
                              (JvNewObjectArray (num_slots, 
                                                 &String::class$, NULL));
  jstring *names_ptr = elements (names);
  JArray<String *> *signatures = reinterpret_cast<JArray<String *> *>
                                   (JvNewObjectArray (num_slots, 
                                                      &String::class$, NULL));
  jstring *signatures_ptr = elements (signatures);
  
  // Get the information out of the JVMTI strucutre and Deallocate the strings.
  for (int i = 0; i < num_slots; i++)
    {
      start_pcs_ptr[i] = var_table[i].start_location;
      lengths_ptr[i] = var_table[i].length;
      slots_ptr[i] = var_table[i].slot;
      names_ptr[i] = JvNewStringUTF (var_table[i].name);
      env->Deallocate (reinterpret_cast<unsigned char *> 
                         (var_table[i].name));
      signatures_ptr[i] = JvNewStringUTF (var_table[i].signature);
      env->Deallocate (reinterpret_cast<unsigned char *> 
                         (var_table[i].signature));
      env->Deallocate (reinterpret_cast<unsigned char *>
                         (var_table[i].generic_signature));
    }
  
  // Now Deallocate the table since it's strings have already been freed.
  env->Deallocate (reinterpret_cast<unsigned char *> (var_table));
  
  // Create the new JDWP VariableTable to return with the now filled arrays.
  VariableTable* jdwp_vtable = new VariableTable (args_len, num_slots,
                                                  start_pcs, names, signatures,
                                                  lengths, slots);
  
  return jdwp_vtable;
}
