/* KeyBoardLEDs.c provide access to the keyboard LEDs.

Copyright (C) 2005-2022 Free Software Foundation, Inc.
Contributed by Gaius Mulley <gaius.mulley@southwales.ac.uk>.

This file is part of GNU Modula-2.

GNU Modula-2 is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 3, or (at your option)
any later version.

GNU Modula-2 is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
General Public License for more details.

Under Section 7 of GPL version 3, you are granted additional
permissions described in the GCC Runtime Library Exception, version
3.1, as published by the Free Software Foundation.

You should have received a copy of the GNU General Public License and
a copy of the GCC Runtime Library Exception along with this program;
see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see
<http://www.gnu.org/licenses/>.  */

#include <config.h>
#include <m2rts.h>

#if defined(linux)

#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <linux/kd.h>
#include <sys/ioctl.h>
#include <stdio.h>

#if !defined(TRUE)
#   define TRUE (1==1)
#endif
#if !defined(FALSE)
#   define FALSE (1==0)
#endif

#include <stdlib.h>

static int fd;
static int initialized = FALSE;


extern "C" void
KeyBoardLEDs_SwitchScroll (int scrolllock)
{
  unsigned char leds;
  int r = ioctl (fd, KDGETLED, &leds);
  if (scrolllock)
    leds = leds | LED_SCR;
  else
    leds = leds & (~ LED_SCR);
  r = ioctl (fd, KDSETLED, leds);
}

extern "C" void
KeyBoardLEDs_SwitchNum (int numlock)
{
  unsigned char leds;
  int r = ioctl (fd, KDGETLED, &leds);
  if (numlock)
    leds = leds | LED_NUM;
  else
    leds = leds & (~ LED_NUM);
  r = ioctl (fd, KDSETLED, leds);
}

extern "C" void
KeyBoardLEDs_SwitchCaps (int capslock)
{
  unsigned char leds;
  int r = ioctl (fd, KDGETLED, &leds);
  if (capslock)
    leds = leds | LED_CAP;
  else
    leds = leds & (~ LED_CAP);
  r = ioctl (fd, KDSETLED, leds);
}

extern "C" void
KeyBoardLEDs_SwitchLeds (int numlock, int capslock, int scrolllock)
{
  KeyBoardLEDs_SwitchScroll (scrolllock);
  KeyBoardLEDs_SwitchNum (numlock);
  KeyBoardLEDs_SwitchCaps (capslock);
}

extern "C" void
_M2_KeyBoardLEDs_init (int, char **, char **)
{
  if (! initialized)
    {
      initialized = TRUE;
      fd = open ("/dev/tty", O_RDONLY);
      if (fd == -1)
	{
	  perror ("unable to open /dev/tty");
	  exit (1);
	}
    }
}

#else
extern "C" void
KeyBoardLEDs_SwitchLeds (int numlock, int capslock, int scrolllock)
{
}

extern "C" void
KeyBoardLEDs_SwitchScroll (int scrolllock)
{
}

extern "C" void
KeyBoardLEDs_SwitchNum (int numlock)
{
}

extern "C" void
KeyBoardLEDs_SwitchCaps (int capslock)
{
}

extern "C" void
_M2_KeyBoardLEDs_init (int, char **, char **)
{
}

#endif

/* GNU Modula-2 linking hooks.  */

extern "C" void
_M2_KeyBoardLEDs_finish (int, char **, char **)
{
}

extern "C" void
_M2_KeyBoardLEDs_dep (void)
{
}

struct _M2_KeyBoardLEDs_ctor { _M2_KeyBoardLEDs_ctor (); } _M2_KeyBoardLEDs_ctor;

_M2_KeyBoardLEDs_ctor::_M2_KeyBoardLEDs_ctor (void)
{
  M2RTS_RegisterModule ("KeyBoardLEDs", _M2_KeyBoardLEDs_init, _M2_KeyBoardLEDs_finish,
			_M2_KeyBoardLEDs_dep);
}
