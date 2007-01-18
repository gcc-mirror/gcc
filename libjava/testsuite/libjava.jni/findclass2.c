#include <stdlib.h>

#include <findclass2.h>

JNIEXPORT void JNICALL
Java_findclass2_searchClass (JNIEnv *env, jclass klass)
{
  (*env)->FindClass (env, "findclass2$inner");
}
