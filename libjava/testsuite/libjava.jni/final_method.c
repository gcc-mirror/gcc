#include <final_method.h>

JNIEXPORT jstring JNICALL
Java_final_1method_meth (JNIEnv *env, jobject thisv)
{
  return (*env)->NewStringUTF (env, "zardoz has spoken");
}
