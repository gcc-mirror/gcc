#ifndef KEYBINDINGS_H
#define KEYBINDINGS_H

#include <QApplication>
#include <QKeyEvent>
#include <QMouseEvent>
#include <QPainter>
#include <QPoint>
#include <qwidget.h>
#include <qstring.h>
#include "qtgraphics.h"

int mapKeyCode(QKeyEvent *key);
int getUnicode(QKeyEvent *key);
int getKeyModifiers(Qt::KeyboardModifiers state);
int getAEKeyModifiers(Qt::KeyboardModifiers state);
int getMouseModifiers(QMouseEvent *event);
int getReleaseModifiers(QMouseEvent *e);

#endif
