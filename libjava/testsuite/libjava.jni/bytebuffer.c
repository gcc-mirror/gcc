#include "bytebuffer.h"

static void
test_buffer (JNIEnv *env, jobject buffer, const char *name)
{
  void *tmp = (*env)->GetDirectBufferAddress (env, buffer);

  if (tmp == NULL)
    printf ("PASS: address of %s\n", name);
  else
    printf ("FAIL: address of %s\n", name);

  int tmplen = (*env)->GetDirectBufferCapacity (env, buffer);

  if (tmplen == -1)
    printf ("PASS: length of %s\n", name);
  else
    printf ("FAIL: length of %s\n", name);
}

JNIEXPORT void JNICALL
Java_bytebuffer_testByteBuffer (JNIEnv *env, jclass k, jobject buffer)
{
  test_buffer (env, buffer, "java.nio.ByteBuffer");
}

JNIEXPORT void JNICALL
Java_bytebuffer_testCharBuffer (JNIEnv *env, jclass k, jobject buffer)
{
  test_buffer (env, buffer, "java.nio.CharBuffer");
}

JNIEXPORT void JNICALL
Java_bytebuffer_testDoubleBuffer (JNIEnv *env, jclass k, jobject buffer)
{
  test_buffer (env, buffer, "java.nio.DoubleBuffer");
}

JNIEXPORT void JNICALL
Java_bytebuffer_testFloatBuffer (JNIEnv *env, jclass k, jobject buffer)
{
  test_buffer (env, buffer, "java.nio.FloatBuffer");
}

JNIEXPORT void JNICALL
Java_bytebuffer_testIntBuffer (JNIEnv *env, jclass k, jobject buffer)
{
  test_buffer (env, buffer, "java.nio.IntBuffer");
}

JNIEXPORT void JNICALL
Java_bytebuffer_testLongBuffer (JNIEnv *env, jclass k, jobject buffer)
{
  test_buffer (env, buffer, "java.nio.LongBuffer");
}

JNIEXPORT void JNICALL
Java_bytebuffer_testShortBuffer (JNIEnv *env, jclass k, jobject buffer)
{
  test_buffer (env, buffer, "java.nio.ShortBuffer");
}

