/* Copyright (C) 2000  Free Software Foundation

   This file is part of libgcj.

This software is copyrighted work licensed under the terms of the
Libgcj License.  Please consult the file "LIBGCJ_LICENSE" for
details.  */

#include <X11/Xlib.h>

#include <gcj/cni.h>
#include <gnu/gcj/RawData.h>

#include <gnu/gcj/xlib/XException.h>
#include <gnu/gcj/xlib/Display.h>
#include <gnu/gcj/xlib/Drawable.h>
#include <gnu/gcj/xlib/XImage.h>

#include <java/awt/Rectangle.h>

jboolean gnu::gcj::xlib::Drawable::copyIntoXImageImpl(XImage* image,
						      jint x, jint y,
						      jint width, jint height,
						      jint destX, jint destY)
{
  ::Display* dpy = (::Display*) (getDisplay()->display);
  ::XImage* ximage = (::XImage*) image->structure;
  int format = image->getFormat();
  int xid = getXID();

  ::XImage* result = XGetSubImage(dpy, xid,
				  x, y, width, height,
				  ~0, // plane mask
				  format,
				  ximage,
				  destX, destY);
  if (result == 0)
    return false;
    
  if (result != ximage)
    throw new XException(MSG_XGETSUBIMAGE_FAILED);

  return true;
}

java::awt::Rectangle*
gnu::gcj::xlib::Drawable::getBounds(java::awt::Rectangle* rv)
{
  ::Display* dpy = (::Display*) (getDisplay()->display);

  ::Window root;
  int x, y;
  unsigned int w, h, bw, depth;

  Status status = XGetGeometry(dpy, getXID(), &root,
			       &x, &y, &w, &h,
			       &bw, &depth);
  
  switch (status)
    {
    case BadDrawable:
      throw new XException(display, status);
    default:
      ; // All OK, NOP.
    }
 
  if (rv == 0)
    {
      rv = new java::awt::Rectangle(x, y, w, h);
    }
  else
    {
      rv->x = x;
      rv->y = y;
      rv->width = w;
      rv->height = h;
    }
  return rv;
}
