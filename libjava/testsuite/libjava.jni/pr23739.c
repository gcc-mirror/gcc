#include <stdio.h>
#include "pr23739.h"

JNIEXPORT void JNICALL
Java_pr23739_checkOrder (JNIEnv *env, jclass cls, jclass clazz1, jclass clazz2)
{
  printf ("B extends A\n");
  printf ("isAssignableFrom (A, B): %d\n",
          (*env)->IsAssignableFrom (env, clazz1, clazz2));
  printf ("isAssignableFrom (B, A): %d\n",
          (*env)->IsAssignableFrom (env, clazz2, clazz1));
}
