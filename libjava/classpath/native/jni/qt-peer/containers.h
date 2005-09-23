#ifndef QTFRAME_H
#define QTFRAME_H

#include <jni.h>
#include <QWidget>

QWidget *frameChildWidget( JNIEnv *env, jobject component );
QWidget *scrollPaneChildWidget( JNIEnv *env, jobject component);

#endif
