#include <jniutf.h>

JNIEXPORT void JNICALL
Java_jniutf_printString (JNIEnv *env, jobject obj, jstring str)
{
  const char *cstr;

  cstr = (*env)->GetStringUTFChars (env, str, NULL);
  (*env)->ReleaseStringUTFChars (env, str, cstr);
}
