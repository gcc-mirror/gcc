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

#include <gnu/gcj/xlib/Display.h>
#include <gnu/gcj/xlib/Screen.h>
#include <gnu/gcj/xlib/Visual.h>
#include <gnu/gcj/xlib/Drawable.h>
#include <gnu/gcj/xlib/XImage.h>
#include <gnu/gcj/xlib/Pixmap.h>

jint gnu::gcj::xlib::Pixmap::createXID(Drawable* drawable,
				       jint width, jint height,
				       jint depth)
{
  Display* display = drawable->getDisplay();
  ::Display* dpy = (::Display*) (display->display);
  jint xid = drawable->getXID();
  
  return XCreatePixmap(dpy, xid, width, height, depth);
}

void gnu::gcj::xlib::Pixmap::finalize()
{
  ::Display* dpy = (::Display*) (getDisplay()->display);
  XFreePixmap(dpy, getXID());
}
