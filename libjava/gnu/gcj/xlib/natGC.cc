/* Copyright (C) 2000, 2003  Free Software Foundation

   This file is part of libgcj.

This software is copyrighted work licensed under the terms of the
Libgcj License.  Please consult the file "LIBGCJ_LICENSE" for
details.  */

#include <vector>

#include <X11/Xlib.h>

#include <gcj/cni.h>
#include <gcj/array.h>
#include <gnu/gcj/RawData.h>
#include <java/lang/String.h>
#include <java/awt/Rectangle.h>

#include <gnu/gcj/xlib/Display.h>
#include <gnu/gcj/xlib/XID.h>
#include <gnu/gcj/xlib/Drawable.h>
#include <gnu/gcj/xlib/Font.h>
#include <gnu/gcj/xlib/XImage.h>
#include <gnu/gcj/xlib/XException.h>
#include <gnu/gcj/xlib/Clip.h>
#include <gnu/gcj/xlib/GC.h>
#include <gnu/gcj/xlib/XException.h>

typedef java::awt::Rectangle AWTRect;
typedef JArray<AWTRect*> AWTRectArray;
typedef std::vector<XRectangle> XRectVector;

void gnu::gcj::xlib::GC::initStructure(GC* copyFrom)
{
  Display* display = target->getDisplay();
  ::Display* dpy = (::Display*) (display->display);
  ::GC gc = (::GC) structure;
  if (gc == 0)
  {
    // If we haven't already created a GC, create one now
    ::Drawable drawableXID = target->getXID();
    gc = XCreateGC(dpy, drawableXID, 0, 0);
    structure = reinterpret_cast<gnu::gcj::RawData*>(gc);
    if (gc == 0) 
      throw new XException(JvNewStringLatin1("GC creation failed"));
  }

  if (copyFrom != 0)
    {
      ::GC fromGC = (::GC) copyFrom->structure;
      XCopyGC(dpy, fromGC, ~0, gc);
      // no fast fail
    }
}

void gnu::gcj::xlib::GC::disposeImpl()
{
  gnu::gcj::RawData* lStructure = structure;
  Drawable* lTargetType = target;

  if ((lStructure == 0) || (lTargetType == 0))
    return;
    
  structure = 0;
  target = 0;
	
  Display* display = lTargetType->getDisplay();
  ::Display* dpy = (::Display*) (display->display);
  ::GC gc = (::GC) lStructure;
	
  XFreeGC(dpy, gc);
  // no fast fail
}

void gnu::gcj::xlib::GC::setForeground(jlong pixel)
{
  Display* display = target->getDisplay();
  ::Display* dpy = (::Display*) (display->display);
  ::GC gc = (::GC) structure;
  XSetForeground(dpy, gc, pixel);
  // no fast fail
}

void gnu::gcj::xlib::GC::setFont(Font* font)
{
  Display* display = target->getDisplay();
  ::Display* dpy = (::Display*) (display->display);
  ::GC gc = (::GC) structure;
  XSetFont(dpy, gc, font->getXID());
  // no fast fail
}

void gnu::gcj::xlib::GC::drawString(jstring text, jint x, jint y)
{
  Display* display = target->getDisplay();
  ::Display* dpy = (::Display*) (display->display);
  ::Drawable drawableXID = target->getXID();
  ::GC gc = (::GC) structure;
  
  jint length = text->length();
  jchar* txt = JvGetStringChars(text);

  XChar2b xwchars[length];
    
  // FIXME: Convert to the character set used in the font, which may
  // or may not be unicode. For now, treat everything as 16-bit and
  // use character codes directly, which should be OK for unicode or
  // 8-bit ascii fonts.

  for (int i=0; i<length; i++)
    {
      XChar2b* xc = &(xwchars[i]);
      jchar jc = txt[i];
      xc->byte1 = (jc >> 8) & 0xff;
      xc->byte2 = jc & 0xff;
    }
  XDrawString16(dpy, drawableXID, gc, x, y, xwchars, length);
}

void gnu::gcj::xlib::GC::drawPoint(jint x, jint y)
{
  Display* display = target->getDisplay();
  ::Display* dpy = (::Display*) (display->display);
  ::Drawable drawableXID = target->getXID();
  ::GC gc = (::GC) structure;
  XDrawPoint (dpy, drawableXID, gc, x, y);
}

void gnu::gcj::xlib::GC::drawLine(jint x1, jint y1, jint x2, jint y2)
{
  Display* display = target->getDisplay();
  ::Display* dpy = (::Display*) (display->display);
  ::Drawable drawableXID = target->getXID();
  ::GC gc = (::GC) structure;
  XDrawLine(dpy, drawableXID, gc, x1, y1, x2, y2);
  // no fast fail
}

void gnu::gcj::xlib::GC::drawRectangle(jint x, jint y, jint w, jint h)
{
  Display* display = target->getDisplay();
  ::Display* dpy = (::Display*) (display->display);
  ::Drawable drawableXID = target->getXID();
  ::GC gc = (::GC) structure;
  XDrawRectangle(dpy, drawableXID, gc, x, y, w, h);
  // no fast fail
}

void gnu::gcj::xlib::GC::fillRectangle(jint x, jint y, jint w, jint h)
{
  Display* display = target->getDisplay();
  ::Display* dpy = (::Display*) (display->display);
  ::Drawable drawableXID = target->getXID();
  ::GC gc = (::GC) structure;
  XFillRectangle(dpy, drawableXID, gc, x, y, w, h);
  // no fast fail
}

void gnu::gcj::xlib::GC::drawArc(jint x, jint y, jint w, jint h,jint startAngle, jint arcAngle)
{
  Display* display = target->getDisplay();
  ::Display* dpy = (::Display*) (display->display);
  ::Drawable drawableXID = target->getXID();
  ::GC gc = (::GC) structure;
  XDrawArc(dpy, drawableXID, gc, x, y, w, h, startAngle * 64, arcAngle * 64);
}

void gnu::gcj::xlib::GC::fillArc(jint x, jint y, jint w, jint h,jint startAngle, jint arcAngle)
{
  Display* display = target->getDisplay();
  ::Display* dpy = (::Display*) (display->display);
  ::Drawable drawableXID = target->getXID();
  ::GC gc = (::GC) structure;
  XFillArc(dpy, drawableXID, gc, x, y, w, h, startAngle * 64, arcAngle * 64);
}

void gnu::gcj::xlib::GC::fillPolygon(jintArray xPoints, jintArray yPoints,
				     jint nPoints,
				     jint translateX, jint translateY)
{
  Display* display = target->getDisplay();
  ::Display* dpy = (::Display*) (display->display);
  ::Drawable drawableXID = target->getXID();
  ::GC gc = (::GC) structure;
  typedef ::XPoint xpoint;
  std::vector<xpoint> points(nPoints+1);
  for (int i=0; i<nPoints; i++)
    {
      points[i].x = elements(xPoints)[i] + translateX;
      points[i].y = elements(yPoints)[i] + translateY;
    }
  points[nPoints] = points[0];
  XFillPolygon(dpy, drawableXID, gc, &(points.front()), nPoints,
	       Complex, CoordModeOrigin);
  // no fast fail
}

void gnu::gcj::xlib::GC::clearArea(jint x, jint y, jint w, jint h,
				   jboolean exposures)
{
  Display* display = target->getDisplay();
  ::Display* dpy = (::Display*) (display->display);
  ::Drawable drawableXID = target->getXID();
  
  XClearArea(dpy, drawableXID, x, y, w, h,
	     exposures ? True : False);
  // no fast fail
}


void gnu::gcj::xlib::GC::putImage(XImage* image,
				  jint srcX, jint srcY,
				  jint destX, jint destY,
				  jint width, jint height)
{
  Display* display = target->getDisplay();
  ::Display* dpy = (::Display*) (display->display);
  ::Drawable drawableXID = target->getXID();
  ::GC gc = (::GC) structure;
  ::XImage* ximage = (::XImage*) (image->structure);
  
  XPutImage(dpy, drawableXID, gc, ximage,
	    srcX, srcY,
	    destX, destY,
	    width, height);
  // no fast fail
}

void gnu::gcj::xlib::GC::updateClip(AWTRectArray* rectangles)
{
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

  Display* display = target->getDisplay();
  ::Display* dpy = (::Display*) (display->display);
  ::GC gc = (::GC) structure;

  int originX = 0;
  int originY = 0;
  int ordering = Unsorted;
  XSetClipRectangles(dpy, gc, originX, originY,
		     &(xrectvector->front()), numRect,
		     ordering);
  delete xrectvector;
}

void gnu::gcj::xlib::GC::copyArea (gnu::gcj::xlib::Drawable * source, 
				  jint srcX, jint srcY,
				  jint destX, jint destY,
				  jint width, jint height)
{
  Display* display = target->getDisplay ();
  ::Display* dpy = (::Display*) (display->display);
  ::Drawable drawableXID = target->getXID ();
  ::GC gc = (::GC) structure;
  ::Drawable srcXID = source->getXID ();

  XCopyArea (dpy, srcXID, drawableXID, gc, srcX, srcY, width, height,
    destX, destY);
}
