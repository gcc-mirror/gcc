#include <simple_int.h>

JNIEXPORT jint JNICALL
Java_simple_1int_nat (JNIEnv *env, jclass klass, jint val)
{
  return 2 * val;
}
