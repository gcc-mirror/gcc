#include <overload.h>

JNIEXPORT jint JNICALL
Java_overload_over__I (JNIEnv *env, jclass klass, jint val)
{
  return val;
}


JNIEXPORT jint JNICALL
Java_overload_over__II (JNIEnv *env, jclass klass, jint one, jint two)
{
  return one + two;
}


