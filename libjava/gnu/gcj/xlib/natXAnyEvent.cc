/* Copyright (C) 2000  Free Software Foundation

   This file is part of libgcj.

This software is copyrighted work licensed under the terms of the
Libgcj License.  Please consult the file "LIBGCJ_LICENSE" for
details.  */

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

void gnu::gcj::xlib::XAnyEvent::init()
{
  ::XEvent* event = new ::XEvent;
  structure = reinterpret_cast<gnu::gcj::RawData*>(event);
}

void gnu::gcj::xlib::XAnyEvent::finalize()
{
  delete structure;
  structure = 0;
}

void gnu::gcj::xlib::XAnyEvent::loadNext()
{
  ::Display* dpy = (::Display*) display->display;
  ::XEvent* evt = (::XEvent*) structure;
  XNextEvent(dpy, evt);
  // What does XNextEvent return?
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
