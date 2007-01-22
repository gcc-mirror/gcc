
#include <stdlib.h>
#include <assert.h>
#include <pr29812_injar.h>

JNIEXPORT jint JNICALL
JNI_OnLoad (JavaVM *vm, void *nothing)
{
  JNIEnv *env;
  jint r;
  jclass k;

  r = (*vm)->GetEnv (vm, (void **) &env, JNI_VERSION_1_2);
  assert (r == JNI_OK);
  k = (*env)->FindClass (env, "pr29812_injar$inner");
  assert (k != NULL);

  return JNI_VERSION_1_2;
}

void
Java_pr29812_1injar_doit (JNIEnv *env, jclass b)
{
  jclass k = (*env)->FindClass(env, "pr29812_injar$inner");
  assert (k != NULL);
}
