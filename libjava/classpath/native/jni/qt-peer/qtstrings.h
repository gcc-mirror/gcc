#ifndef QTSTRINGS_H
#define QTSTRINGS_H

#include <jni.h>
#include <QString>

QString *getQString(JNIEnv *env, jstring str);
jstring getJavaString(JNIEnv *env, QString *qstring);

#endif
