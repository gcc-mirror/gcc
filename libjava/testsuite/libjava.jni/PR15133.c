#include <stdio.h>
#include <jni.h>
#include "PR15133.h"

JNIEXPORT void JNICALL
Java_PR15133_printIt (JNIEnv *env, jobject x, jint y)
{
  printf ("%d\n", y);
}
