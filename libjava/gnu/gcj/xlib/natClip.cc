/* Copyright (C) 2000  Free Software Foundation

   This file is part of libgcj.

This software is copyrighted work licensed under the terms of the
Libgcj License.  Please consult the file "LIBGCJ_LICENSE" for
details.  */

// Needed to avoid linking in libstdc++
#ifndef __STL_USE_EXCEPTIONS
#   include <java/lang/OutOfMemoryError.h>
#   define __THROW_BAD_ALLOC throw new java::lang::OutOfMemoryError()
#endif 

#include <vector>

#include <X11/Xlib.h>
#include <gcj/cni.h>
#include <gnu/gcj/RawData.h>
#include <java/awt/Rectangle.h>

#include "gnu/gcj/xlib/Clip.h"

typedef java::awt::Rectangle AWTRect;
typedef JArray<AWTRect*> AWTRectArray;
typedef std::vector<XRectangle> XRectVector;

void gnu::gcj::xlib::Clip::init(AWTRectArray* rectangles)
{
  // Prepare rectangles:
  
  int numRect = JvGetArrayLength(rectangles);
  XRectVector* xrectvector = new XRectVector(numRect);
  
  for (int i=0; i<numRect; i++)
    {
      AWTRect* awtrect = elements(rectangles)[i];
      XRectangle& xrect = (*xrectvector)[i];
      
      xrect.x      = awtrect->x;
      xrect.y      = awtrect->y;
      xrect.width  = awtrect->width;
      xrect.height = awtrect->height;
    }

  xrects = reinterpret_cast<gnu::gcj::RawData*>(xrectvector);
}

void gnu::gcj::xlib::Clip::dispose()
{
  if (xrects)
  {
    delete xrects; 
    xrects = 0;
  }
}
