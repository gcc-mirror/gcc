#include <final_method.h>

jstring
Java_final_1method_meth (JNIEnv *env, jobject thisv)
{
  return (*env)->NewStringUTF (env, "zardoz has spoken");
}
