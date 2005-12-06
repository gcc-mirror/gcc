#include <jni.h>
#include <stdio.h>

#include "pr18278.h"

jobject Java_pr18278_weakRef(JNIEnv *env, jclass cls, jobject data)
{
  jobject r =  (* env)->NewWeakGlobalRef(env, data);
  return r;
}
