// jni-libjvm.cc - an implementation of the JNI invocation API.

/* Copyright (C) 2006  Free Software Foundation

   This file is part of libgcj.

This software is copyrighted work licensed under the terms of the
Libgcj License.  Please consult the file "LIBGCJ_LICENSE" for
details.  */

#include <gcj/cni.h>
#include <gcj/javaprims.h>
#include <java-assert.h>
#include <jvm.h>
#include <jni.h>

using namespace gcj;

// Forward declarations.
extern struct JNIInvokeInterface_ _Jv_JNI_InvokeFunctions;
extern jint JNICALL _Jv_JNI_AttachCurrentThread (JavaVM *vm,
                                                 void **penv, void *args);
extern JavaVM *_Jv_the_vm;

jint JNICALL
JNI_GetDefaultJavaVMInitArgs (void *args)
{
  jint version = * (jint *) args;
  // Here we only support 1.2 and 1.4.
  if (version != JNI_VERSION_1_2 && version != JNI_VERSION_1_4)
    return JNI_EVERSION;

  JavaVMInitArgs *ia = reinterpret_cast<JavaVMInitArgs *> (args);
  ia->version = JNI_VERSION_1_4;
  ia->nOptions = 0;
  ia->options = NULL;
  ia->ignoreUnrecognized = true;

  return 0;
}

jint JNICALL
JNI_CreateJavaVM (JavaVM **vm, void **penv, void *args)
{
  JvAssert (! _Jv_the_vm);

  jint version = * (jint *) args;
  // We only support 1.2 and 1.4.
  if (version != JNI_VERSION_1_2 && version != JNI_VERSION_1_4)
    return JNI_EVERSION;

  JvVMInitArgs* vm_args = reinterpret_cast<JvVMInitArgs *> (args);

  jint result = _Jv_CreateJavaVM (vm_args);
  if (result)
    return result;

  // FIXME: synchronize
  JavaVM *nvm = (JavaVM *) _Jv_MallocUnchecked (sizeof (JavaVM));
  if (nvm == NULL)
    return JNI_ERR;
  nvm->functions = &_Jv_JNI_InvokeFunctions;

  jint r =_Jv_JNI_AttachCurrentThread (nvm, penv, NULL);
  if (r < 0)
    return r;

  _Jv_the_vm = nvm;
  *vm = _Jv_the_vm;

  return 0;
}

jint JNICALL
JNI_GetCreatedJavaVMs (JavaVM **vm_buffer, jsize buf_len, jsize *n_vms)
{
  if (buf_len <= 0)
    return JNI_ERR;

  // We only support a single VM.
  if (_Jv_the_vm != NULL)
    {
      vm_buffer[0] = _Jv_the_vm;
      *n_vms = 1;
    }
  else
    *n_vms = 0;
  return 0;
}
