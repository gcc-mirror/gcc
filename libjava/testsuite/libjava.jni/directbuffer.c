#include <stdlib.h>

#include "directbuffer.h"

#define BUFFER_SIZE 1024

static void *address;

JNIEXPORT jobject JNICALL
Java_directbuffer_createDirectByteBuffer (JNIEnv *env, jclass k)
{
  address = malloc (BUFFER_SIZE);
  return (*env)->NewDirectByteBuffer (env, address, 1024);
}

static void
test_buffer (JNIEnv *env, jobject buffer, const char *name, int len)
{
  void *tmp = (*env)->GetDirectBufferAddress (env, buffer);

  if (address == tmp)
    printf ("PASS: address of %s\n", name);
  else
    printf ("FAIL: address of %s\n", name);

  int tmplen = (*env)->GetDirectBufferCapacity (env, buffer);

  if (len == tmplen)
    printf ("PASS: length of %s\n", name);
  else
    printf ("FAIL: length of %s\n", name);
}

JNIEXPORT void JNICALL
Java_directbuffer_testDirectByteBuffer (JNIEnv *env, jclass k, jobject buffer, jint len)
{
  test_buffer (env, buffer, "direct java.nio.ByteBuffer", len);
}

JNIEXPORT void JNICALL
Java_directbuffer_testCharBuffer (JNIEnv *env, jclass k, jobject buffer, jint len)
{
  test_buffer (env, buffer, "java.nio.CharBuffer view", len);
}

JNIEXPORT void JNICALL
Java_directbuffer_testDoubleBuffer (JNIEnv *env, jclass k, jobject buffer, jint len)
{
  test_buffer (env, buffer, "java.nio.DoubleBuffer view", len);
}

JNIEXPORT void JNICALL
Java_directbuffer_testFloatBuffer (JNIEnv *env, jclass k, jobject buffer, jint len)
{
  test_buffer (env, buffer, "java.nio.FloatBuffer view", len);
}

JNIEXPORT void JNICALL
Java_directbuffer_testIntBuffer (JNIEnv *env, jclass k, jobject buffer, jint len)
{
  test_buffer (env, buffer, "java.nio.IntBuffer view", len);
}

JNIEXPORT void JNICALL
Java_directbuffer_testLongBuffer (JNIEnv *env, jclass k, jobject buffer, jint len)
{
  test_buffer (env, buffer, "java.nio.LongBuffer view", len);
}

JNIEXPORT void JNICALL
Java_directbuffer_testShortBuffer (JNIEnv *env, jclass k, jobject buffer, jint len)
{
  test_buffer (env, buffer, "java.nio.ShortBuffer view", len);
}

