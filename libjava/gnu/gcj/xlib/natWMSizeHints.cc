/* Copyright (C) 2000  Free Software Foundation

   This file is part of libgcj.

This software is copyrighted work licensed under the terms of the
Libgcj License.  Please consult the file "LIBGCJ_LICENSE" for
details.  */

#include <X11/Xlib.h>
#include <X11/Xutil.h>

#include <gcj/cni.h>
#include <gnu/gcj/RawData.h>
#include <java/lang/OutOfMemoryError.h>

#include <gnu/gcj/xlib/Window.h>
#include <gnu/gcj/xlib/Display.h>
#include <gnu/gcj/xlib/WMSizeHints.h>

void gnu::gcj::xlib::WMSizeHints::init(WMSizeHints* copyFrom)
{
  XSizeHints* hints = XAllocSizeHints();
  if (hints == 0)
    {
      jstring errorMessage = JvNewStringLatin1("XAllocSizeHints failed");
      throw new java::lang::OutOfMemoryError(errorMessage);
    }

  if (copyFrom != 0)
    {
      XSizeHints* from = (XSizeHints*) copyFrom->structure;
      (*hints) = (*from);
    } 
  else
    {
      // Is this necessary?
      hints->flags = 0;
    }
  structure = reinterpret_cast<gnu::gcj::RawData*>(hints);
}

void gnu::gcj::xlib::WMSizeHints::finalize()
{
  delete structure;
}

void gnu::gcj::xlib::WMSizeHints::applyNormalHints(gnu::gcj::xlib::Window* window)
{
  Display* display = window->display;
  ::Display* dpy = (::Display*) display->display;
  ::Window win = window->getXID();
  XSizeHints* hints = (XSizeHints*) structure;
  
  XSetWMNormalHints(dpy, win, hints);
  /* FIXME, alternative?
     // X11 source reports XSetWMNormalHints() as an old routine. (?)
     XSetWMSizeHints(dpy, win, hints, display->getAtom("WM_NORMAL_HINTS"));
  */
}

void gnu::gcj::xlib::WMSizeHints::setMinSize(jint width, jint height)
{
  XSizeHints* hints = (XSizeHints*) structure;
  hints->min_width = width;
  hints->min_height = height;
  hints->flags = hints->flags | PMinSize;
}

void gnu::gcj::xlib::WMSizeHints::setMaxSize(jint width, jint height)
{
  XSizeHints* hints = (XSizeHints*) structure;
  hints->max_width = width;
  hints->max_height = height;
  hints->flags = hints->flags | PMaxSize;
}
