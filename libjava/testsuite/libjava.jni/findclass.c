#include <stdlib.h>

#include <findclass.h>

jclass
Java_findclass_doit (JNIEnv *env, jclass klass, jstring name)
{
  const char *buf = (*env)->GetStringUTFChars (env, name, NULL);
  jclass k = (*env)->FindClass (env, buf);
  (*env)->ReleaseStringUTFChars (env, name, buf);
  return k;
}
