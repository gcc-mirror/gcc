// { dg-do run }

#include "event-private.h"

template<typename T> void derefIfNotNull(T* ptr)
{
  if (ptr != 0)
    ptr->deref();
}

int main()
{
  Event * ev = new Event;
  derefIfNotNull(ev);
}
