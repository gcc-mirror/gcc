#include <stdio.h>
#include "shortfield.h"

void shortfield::ouch ()
{
  printf ("list: %d %d 0x%x\n",
	  modCount,
	  size__,
	  data);
}
