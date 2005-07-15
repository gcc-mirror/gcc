/* Copyright (C) 2000  Free Software Foundation

   This file is part of libgcj.

This software is copyrighted work licensed under the terms of the
Libgcj License.  Please consult the file "LIBGCJ_LICENSE" for
details.  */

#include <config.h>
#include <platform.h>

#include <gcj/javaprims.h>
#include <jvm.h>

#include <X11/Xlib.h>

#include <gcj/cni.h>
#include <gnu/gcj/RawData.h>

#include <java/lang/RuntimeException.h>

#include <java/lang/System.h>
#include <java/io/PrintStream.h>

#include <gnu/gcj/xlib/Display.h>
#include <gnu/gcj/xlib/Window.h>
#include <gnu/gcj/xlib/XAnyEvent.h>
#include <gnu/gcj/xlib/XExposeEvent.h>
#include <gnu/gcj/xlib/XException.h>

#include <unistd.h>
#include <posix.h>

void gnu::gcj::xlib::XAnyEvent::init()
{
  ::XEvent* event = new ::XEvent;
  int *pipes = new int[2];
  pipe(pipes);
  structure = reinterpret_cast<gnu::gcj::RawData*>(event);
  pipefds = reinterpret_cast<gnu::gcj::RawData*>(pipes);
}

void gnu::gcj::xlib::XAnyEvent::finalize()
{
  delete structure;
  int *pipe = reinterpret_cast<int *>(pipefds);
  close(pipe[0]);
  close(pipe[1]);
  delete [] pipefds;
  pipefds = 0;
  structure = 0;
}

jboolean gnu::gcj::xlib::XAnyEvent::loadNext(jboolean block)
{
  ::Display* dpy = (::Display*) display->display;
  ::XEvent* evt = (::XEvent*) structure;

  if (XPending(dpy))
    {
      XNextEvent(dpy, evt);
      return true;  
    }

  if (!block)
    return false;

  int *pipe = reinterpret_cast<int *>(pipefds);
  int xfd = XConnectionNumber(dpy);
  int pipefd = pipe[0];
  int n = (xfd > pipefd ? xfd : pipefd) + 1;
  fd_set rfds;
  FD_ZERO(&rfds);
  FD_SET(xfd, &rfds);
  FD_SET(pipefd, &rfds);  
  int sel = _Jv_select (n, &rfds, NULL, NULL, NULL);
  if (sel > 0)
    {
      if (FD_ISSET(xfd, &rfds))
	{
	  XNextEvent(dpy, evt);
	  return true;  
	}
      if (FD_ISSET(pipefd, &rfds))
	{
	  char c;
	  read(pipefd, &c, 1);
	}
    }
  return false;
}

void gnu::gcj::xlib::XAnyEvent::interrupt()
{
  int *pipe = reinterpret_cast<int *>(pipefds);
  write(pipe[1], "W", 1);
}

jint gnu::gcj::xlib::XAnyEvent::getType()
{
  ::XEvent* event = (::XEvent*) structure;
  return event->type;
}

void gnu::gcj::xlib::XAnyEvent::setType(jint type)
{
  ::XEvent* event = (::XEvent*) structure;
  event->type = type;
}

gnu::gcj::xlib::Window* gnu::gcj::xlib::XAnyEvent::getWindow()
{
  ::XEvent* event = (::XEvent*) structure;
  return display->getWindow(event->xany.window);
}

void gnu::gcj::xlib::XAnyEvent::setWindow(gnu::gcj::xlib::Window* window)
{
  ::XEvent* event = (::XEvent*) structure;
  event->xany.window = window->getXID();
}

jlong gnu::gcj::xlib::XAnyEvent::getSerial()
{
  ::XEvent* event = (::XEvent*) structure;
  return event->xany.serial;
}

void gnu::gcj::xlib::XAnyEvent::send(gnu::gcj::xlib::Window* destination,
				     jboolean propagate, jlong mask)
{
  ::Display* dpy = (::Display*) display->display;
  ::XEvent* event = (::XEvent*) structure;

  Status status = 
    XSendEvent(dpy, destination->getXID(), propagate ? True : False,
	       mask, event);

  switch (status)
    {
    case 0:
      throw new XException(JvNewStringLatin1("conversion to wire "
					     "protocol failed"));
    case BadWindow:
    case BadValue:
      throw new XException(display, status);

    default:
      /* All other return values indicate success.  Ie. (status ==
	 1) indicates success, not BadRequest. */
      ; // NOP
    }
}
