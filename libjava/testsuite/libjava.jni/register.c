#include <stdlib.h>
#include <assert.h>
#include <register.h>

static jint
some_random_name (JNIEnv *env, jclass k, jint v)
{
  return v - 1;
}

jint
JNI_OnLoad (JavaVM *vm, void *nothing)
{
  JNIEnv *env;
  JNINativeMethod meth;
  jclass k;
  jint r;

  r = (*vm)->GetEnv (vm, (void **) &env, JNI_VERSION_1_2);
  assert (r == JNI_OK);
  k = (*env)->FindClass (env, "register");
  assert (k != NULL);

  meth.name = "doit";
  meth.signature = "(I)I";
  meth.fnPtr = some_random_name;

  r = (*env)->RegisterNatives (env, k, &meth, 1);
  assert (r == JNI_OK);

  return JNI_VERSION_1_2;
}
