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
#include <gnu/gcj/xlib/XAnyEvent.h>
#include <gnu/gcj/xlib/XEvent.h>
#include <gnu/gcj/xlib/XUnmapEvent.h>

void gnu::gcj::xlib::XUnmapEvent::setUnmappedWindow(gnu::gcj::xlib::Window*
						    unmappedWindow)
{
  ::XUnmapEvent* evt = (::XUnmapEvent*) event->structure;
  evt->window = unmappedWindow->getXID();
}

void gnu::gcj::xlib::XUnmapEvent::setFromConfigure(jboolean fromConfigure)
{
  ::XUnmapEvent* evt = (::XUnmapEvent*) event->structure;
  evt->from_configure = fromConfigure ? True : False;
}

