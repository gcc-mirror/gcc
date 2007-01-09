#include <stdlib.h>
#include <assert.h>
#include <jni.h>

static int
twentythree (JNIEnv *env, jclass k)
{
  return 23;
}

static int
oneninetyseven (JNIEnv *env, jclass k)
{
  return 197;
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
  k = (*env)->FindClass (env, "register2$I1");
  assert (k != NULL);

  meth.name = "doit";
  meth.signature = "()I";
  meth.fnPtr = twentythree;

  r = (*env)->RegisterNatives (env, k, &meth, 1);
  assert (r == JNI_OK);

  k = (*env)->FindClass (env, "register2$I2");
  assert (k != NULL);

  meth.name = "doit";
  meth.signature = "()I";
  meth.fnPtr = oneninetyseven;

  r = (*env)->RegisterNatives (env, k, &meth, 1);
  assert (r == JNI_OK);

  return JNI_VERSION_1_2;
}
