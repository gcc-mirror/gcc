#include <invoke.h>

JNIEXPORT jint JNICALL
Java_invoke_val (JNIEnv *env, jclass klass)
{
  return 23;
}
