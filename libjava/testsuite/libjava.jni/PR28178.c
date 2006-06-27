#include <PR28178.h>

void
Java_PR28178_m (JNIEnv *env, jclass ignore)
{
  (*env)->DeleteLocalRef(env, NULL);
  (*env)->DeleteGlobalRef(env, NULL);
}


