#ifndef BUTTONEVENT_H
#define BUTTONEVENT_H

#include <QAbstractButton>
#include "mainthreadinterface.h"

class AWTLabelEvent : public AWTEvent {
  
 private:
  QAbstractButton *widget;
  QString *string;
  
 public:
  AWTLabelEvent(QAbstractButton *w, QString *s) : AWTEvent()
  {
    widget = w;
    string = s;
  }

  void runEvent()
  {
    widget->setText( *string );
    delete string;
  }
};

#endif
