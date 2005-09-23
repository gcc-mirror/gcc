#ifndef NATIVEWRAPPER_H
#define NATIVEWRAPPER_H

#include <jni.h>

void *getNativeObject( JNIEnv *env, jobject qtcomponent );

void setNativeObject( JNIEnv *env, jobject qtcomponent, void *ptr );

#endif
