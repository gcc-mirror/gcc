#include <jni.h>
#include "martin.h"
#include <stdio.h>

JNIEXPORT void JNICALL
Java_martin_myNative(JNIEnv* env, jobject this, jstring s)
{
  jclass cls;
  jfieldID fid;
  jobject obj;
  jmethodID mid;

  printf("From C\n");
  fflush(stdout);

  cls = (*env)->FindClass(env, "java/lang/System");
  if (cls == 0) {
    printf("java/lang/System lookup failed\n");
    return;
  }
  fid = (*env)->GetStaticFieldID(env, cls, "out", "Ljava/io/PrintStream;");
  if (fid == 0) {
    printf("java/lang/System::out lookup failed\n");
    return;
  }
  obj = (*env)->GetStaticObjectField(env, cls, fid);
  if (obj == 0) {
    printf("GetStaticObjectField call failed\n");
    return;
  }
  cls = (*env)->GetObjectClass(env, obj);
  if (cls == 0) {
    printf("GetObjectClass(out) failed\n");
    return;
  }
  mid = (*env)->GetMethodID(env, cls, "println", "(Ljava/lang/String;)V");
  if (mid == 0) {
    printf("println method lookup failed\n");
    return;
  }
  (*env)->CallVoidMethod(env, obj, mid, s);
}

