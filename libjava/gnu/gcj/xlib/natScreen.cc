/* Copyright (C) 2000  Free Software Foundation

   This file is part of libgcj.

This software is copyrighted work licensed under the terms of the
Libgcj License.  Please consult the file "LIBGCJ_LICENSE" for
details.  */

#include <X11/Xlib.h>

#include <gcj/cni.h>
#include <gnu/gcj/xlib/Display.h>
#include <gnu/gcj/xlib/Screen.h>

void gnu::gcj::xlib::Screen::initStructure()
{
  ::Display* dpy = (::Display*) (display->display);
  ::Screen* screen = ScreenOfDisplay(dpy, screenNumber);
  
  structure = reinterpret_cast<gnu::gcj::RawData*>(screen);
}


jint gnu::gcj::xlib::Screen::getRootDepth()
{
  ::Screen* screen = (::Screen*) structure;
  return screen->root_depth;
}

jint gnu::gcj::xlib::Screen::getRootWindowXID()
{
  ::Screen* screen = (::Screen*) structure;
  return RootWindowOfScreen(screen);
}

jint gnu::gcj::xlib::Screen::getDefaultColormapXID()
{
  ::Screen* screen = (::Screen*) structure;
  return DefaultColormapOfScreen(screen);
}

jint gnu::gcj::xlib::Screen::findScreenNumber()
{
  ::Screen* screen = (::Screen*) structure;
  return XScreenNumberOfScreen(screen);
}

gnu::gcj::RawData* gnu::gcj::xlib::Screen::getRootVisualStructure()
{
  ::Screen* screen = (::Screen*) structure;
  ::Visual* visual = DefaultVisualOfScreen(screen);
  return reinterpret_cast<gnu::gcj::RawData*>(visual);
}
