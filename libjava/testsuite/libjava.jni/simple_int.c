#include <simple_int.h>

jint
Java_simple_1int_nat (JNIEnv *env, jclass klass, jint val)
{
  return 2 * val;
}
