#include <stdlib.h>
#include <assert.h>
#include <PR18116.h>

// The purpose of this test is to ensure that signatures with non-top
// level class arguments work.

static jint
some_random_name (JNIEnv *env, jclass k, jobject v)
{
  return 555;
}

JNIEXPORT jint JNICALL
JNI_OnLoad (JavaVM *vm, void *nothing)
{
  JNIEnv *env;
  JNINativeMethod meth;
  jclass k;
  jint r;

  r = (*vm)->GetEnv (vm, (void **) &env, JNI_VERSION_1_2);
  assert (r == JNI_OK);
  k = (*env)->FindClass (env, "PR18116");
  assert (k != NULL);

  meth.name = "doit";
  meth.signature = "(Ljava/lang/String;)I";
  meth.fnPtr = some_random_name;

  r = (*env)->RegisterNatives (env, k, &meth, 1);
  assert (r == JNI_OK);

  return JNI_VERSION_1_2;
}
