#include <gcj/cni.h>

#include <jvm.h>
#include <jvmti.h>
#include <stdio.h>

#include "jvmti-int.h"
#include "geterrorname.h"

static void
get_error (jvmtiEnv *env, jvmtiError err)
{
  char *s;
  env->GetErrorName (err, &s);
  printf ("%s\n", s);
  env->Deallocate (reinterpret_cast<unsigned char *> (s));
}

void
geterrorname::do_errorname_tests ()
{
  jvmtiEnv *env;
  JavaVM *vm = _Jv_GetJavaVM ();
  vm->GetEnv (reinterpret_cast<void **> (&env), JVMTI_VERSION_1_0);

  get_error (env, JVMTI_ERROR_NONE);
  get_error (env, JVMTI_ERROR_NULL_POINTER);
  get_error (env, JVMTI_ERROR_OUT_OF_MEMORY);
  get_error (env, JVMTI_ERROR_ACCESS_DENIED);
  get_error (env, JVMTI_ERROR_WRONG_PHASE);
  get_error (env, JVMTI_ERROR_INTERNAL);
  get_error (env, JVMTI_ERROR_UNATTACHED_THREAD);
  get_error (env, JVMTI_ERROR_INVALID_ENVIRONMENT);
  get_error (env, JVMTI_ERROR_INVALID_PRIORITY);
  get_error (env, JVMTI_ERROR_THREAD_NOT_SUSPENDED);
  get_error (env, JVMTI_ERROR_THREAD_SUSPENDED);
  get_error (env, JVMTI_ERROR_THREAD_NOT_ALIVE);
  get_error (env, JVMTI_ERROR_CLASS_NOT_PREPARED);
  get_error (env, JVMTI_ERROR_NO_MORE_FRAMES);
  get_error (env, JVMTI_ERROR_OPAQUE_FRAME);
  get_error (env, JVMTI_ERROR_DUPLICATE);
  get_error (env, JVMTI_ERROR_NOT_FOUND);
  get_error (env, JVMTI_ERROR_NOT_MONITOR_OWNER);
  get_error (env, JVMTI_ERROR_INTERRUPT);
  get_error (env, JVMTI_ERROR_UNMODIFIABLE_CLASS);
  get_error (env, JVMTI_ERROR_NOT_AVAILABLE);
  get_error (env, JVMTI_ERROR_ABSENT_INFORMATION);
  get_error (env, JVMTI_ERROR_INVALID_EVENT_TYPE);
  get_error (env, JVMTI_ERROR_NATIVE_METHOD);
  get_error (env, JVMTI_ERROR_INVALID_THREAD);
  get_error (env, JVMTI_ERROR_INVALID_THREAD_GROUP);
  get_error (env, JVMTI_ERROR_INVALID_OBJECT);
  get_error (env, JVMTI_ERROR_INVALID_CLASS);
  get_error (env, JVMTI_ERROR_INVALID_METHODID);
  get_error (env, JVMTI_ERROR_INVALID_LOCATION);
  get_error (env, JVMTI_ERROR_INVALID_FIELDID);
  get_error (env, JVMTI_ERROR_TYPE_MISMATCH);
  get_error (env, JVMTI_ERROR_INVALID_SLOT);
  get_error (env, JVMTI_ERROR_INVALID_MONITOR);
  get_error (env, JVMTI_ERROR_INVALID_CLASS_FORMAT);
  get_error (env, JVMTI_ERROR_CIRCULAR_CLASS_DEFINITION);
  get_error (env, JVMTI_ERROR_UNSUPPORTED_REDEFINITION_METHOD_ADDED);
  get_error (env, JVMTI_ERROR_UNSUPPORTED_REDEFINITION_SCHEMA_CHANGED);
  get_error (env, JVMTI_ERROR_INVALID_TYPESTATE);
  get_error (env, JVMTI_ERROR_FAILS_VERIFICATION);
  get_error (env, JVMTI_ERROR_UNSUPPORTED_REDEFINITION_HIERARCHY_CHANGED);
  get_error (env, JVMTI_ERROR_UNSUPPORTED_REDEFINITION_METHOD_DELETED);
  get_error (env, JVMTI_ERROR_UNSUPPORTED_VERSION);
  get_error (env, JVMTI_ERROR_NAMES_DONT_MATCH);
  get_error (env,
	     JVMTI_ERROR_UNSUPPORTED_REDEFINITION_CLASS_MODIFIERS_CHANGED);
  get_error (env,
	     JVMTI_ERROR_UNSUPPORTED_REDEFINITION_METHOD_MODIFIERS_CHANGED);
  get_error (env, JVMTI_ERROR_MUST_POSSESS_CAPABILITY);
  get_error (env, JVMTI_ERROR_ILLEGAL_ARGUMENT);
}
