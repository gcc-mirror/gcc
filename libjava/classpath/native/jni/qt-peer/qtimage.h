#ifndef QTIMAGE_H
#define QTIMAGE_H

#include <QImage>
#include <QPixmap>

QImage *getQtImage( JNIEnv *env, jobject obj );
QPixmap *getQtVolatileImage( JNIEnv *env, jobject obj );

#endif
