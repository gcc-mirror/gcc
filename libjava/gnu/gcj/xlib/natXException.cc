/* Copyright (C) 2000  Free Software Foundation

   This file is part of libgcj.

This software is copyrighted work licensed under the terms of the
Libgcj License.  Please consult the file "LIBGCJ_LICENSE" for
details.  */

#include <X11/Xlib.h>

#include <gcj/cni.h>
#include <gnu/gcj/xlib/Display.h>
#include <gnu/gcj/xlib/XException.h>

jstring gnu::gcj::xlib::XException::toString(Display* display, jint status)
{
  ::Display* dpy = (::Display*) (display->display);
  
  const int ERROR_TEXT_LENGTH = 256;
  char errorText[ERROR_TEXT_LENGTH];
  
  XGetErrorText(dpy, status, errorText, ERROR_TEXT_LENGTH);
  return JvNewStringLatin1(errorText);
}
