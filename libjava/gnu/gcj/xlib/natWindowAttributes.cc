/* Copyright (C) 2000  Free Software Foundation

   This file is part of libgcj.

This software is copyrighted work licensed under the terms of the
Libgcj License.  Please consult the file "LIBGCJ_LICENSE" for
details.  */

#include <X11/Xlib.h>

#include <gcj/cni.h>
#include <gnu/gcj/RawData.h>

#include <gnu/gcj/xlib/Display.h>
#include <gnu/gcj/xlib/Window.h>
#include <gnu/gcj/xlib/WindowAttributes.h>
#include <gnu/gcj/xlib/Pixmap.h>
#include <gnu/gcj/xlib/XException.h>
#include <gnu/gcj/xlib/Screen.h>
#include <gnu/gcj/xlib/Visual.h>


void gnu::gcj::xlib::WindowAttributes::initFromWindow(Window* from)
{
  display = from->getDisplay();
  ::Display* dpy = (::Display*) display->display;
  ::Window win = from->getXID();
  
  XWindowAttributes* attributesIn  = new XWindowAttributes;
  in = reinterpret_cast<gnu::gcj::RawData*>(attributesIn);

  Status status = XGetWindowAttributes(dpy, win, attributesIn);
  if ((status == BadDrawable) | (status == BadWindow))
    throw new XException(display, status);
}


void gnu::gcj::xlib::WindowAttributes::init(WindowAttributes* copyFrom)
{
  XSetWindowAttributes* attributes = new XSetWindowAttributes;

  if (copyFrom != 0)
    {
      XSetWindowAttributes* from = 
	(XSetWindowAttributes*) copyFrom->out;
      (*attributes) = (*from);
    } 
  
  out = reinterpret_cast<gnu::gcj::RawData*>(attributes);
}

void gnu::gcj::xlib::WindowAttributes::finalize()
{
  delete in; in = 0;
  delete out; out = 0;
}

void gnu::gcj::xlib::WindowAttributes::setBackground(jlong pixel)
{
  XSetWindowAttributes* attributes = (XSetWindowAttributes*) out;
  
  attributes->background_pixel = pixel;
  mask = mask | CWBackPixel;
}

void gnu::gcj::xlib::WindowAttributes::setBackground(Pixmap* pixmap)
{
  XSetWindowAttributes* attributes = (XSetWindowAttributes*) out;

  attributes->background_pixmap = pixmap->getXID();
  mask = mask | CWBackPixmap;
}

void gnu::gcj::xlib::WindowAttributes::setEventMask(jlong eventMask)
{
  XSetWindowAttributes* attributes = (XSetWindowAttributes*) out;

  attributes->event_mask = eventMask;
  mask = mask | CWEventMask;
}

gnu::gcj::xlib::Visual* gnu::gcj::xlib::WindowAttributes::getVisual()
{
  if (in == 0)
    return 0;

  XWindowAttributes* attributesIn = (XWindowAttributes*) in;

  gnu::gcj::RawData* screenRef =
    reinterpret_cast<gnu::gcj::RawData*>(attributesIn->screen);

  Screen* screen = new Screen(display, screenRef);

  gnu::gcj::RawData* visualRef =
    reinterpret_cast<gnu::gcj::RawData*>(attributesIn->visual);

  return new gnu::gcj::xlib::Visual(visualRef, screen, attributesIn->depth);
}


void gnu::gcj::xlib::WindowAttributes::apply(Window* window)
{
  ::Display* dpy = (::Display*) window->getDisplay()->display;
  ::Window win = window->getXID();
  XSetWindowAttributes* attributes = (XSetWindowAttributes*) out;
  
  XChangeWindowAttributes(dpy, win, mask, attributes);
}

