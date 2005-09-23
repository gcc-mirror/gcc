#ifndef QTGRAPHICS_H
#define QTGRAPHICS_H

#include <jni.h>
#include <QPainter>
#include <QPaintDevice>
#include <QPen>
#include <QBrush>

class GraphicsPainter : public QPainter
{
public:
  QPen *currentPen;
  QBrush *currentBrush;
  QColor *currentColor;
  GraphicsPainter(QPaintDevice *dev) : QPainter( dev )
  {
    currentPen = new QPen();
    currentBrush = new QBrush();
    currentColor = new QColor();
  }
};

GraphicsPainter *getPainter( JNIEnv *env, jobject obj );

#endif
