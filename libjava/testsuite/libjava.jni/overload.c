#include <overload.h>

jint
Java_overload_over__I (JNIEnv *env, jclass klass, jint val)
{
  return val;
}


jint
Java_overload_over__II (JNIEnv *env, jclass klass, jint one, jint two)
{
  return one + two;
}


