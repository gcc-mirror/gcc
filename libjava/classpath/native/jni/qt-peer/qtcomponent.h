#ifndef QTCOMPONENT_H
#define QTCOMPONENT_H

#include <QSize>
#include <QPoint>
//#include <cassert.h>
#include <jni.h>
#include "nativewrapper.h"

void *getParentWidget( JNIEnv *env, jobject qtcomponentpeer );

jobject makeDimension(JNIEnv *env, QSize *size);

jobject makePoint(JNIEnv *env, QPoint &p);

#endif
