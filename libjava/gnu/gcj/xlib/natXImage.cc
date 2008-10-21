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
#include <gnu/gcj/xlib/XImage.h>
#include <java/lang/System.h>
#include <java/io/PrintStream.h>

void gnu::gcj::xlib::XImage::init(Visual* visual, jint depth,
				  jint format, jint xoffset,
				  jint width, jint height,
				  jint bitmapPad, jint bytesPerLine,
				  jint bitsPerPixel)
{
  ::Display* dpy = (::Display*) visual->display->display;
  ::Visual* vis = (::Visual*) visual->getVisualStructure();

  char* data = 0; // no preallocated data
  ::XImage* ximage = XCreateImage(dpy, vis, depth, format, xoffset,
				  data,
				  width, height, 
				  bitmapPad,
				  bytesPerLine
				  );
  if (ximage == 0)
    {
      jstring errorMessage = JvNewStringLatin1("XCreateImage failed");
      throw new ::java::lang::OutOfMemoryError(errorMessage);
    }
    
  bool reinitialize = false;
    
  if ((bitsPerPixel != 0) && (ximage->bits_per_pixel != bitsPerPixel))
    {
      ximage->bits_per_pixel = bitsPerPixel;
      reinitialize = true;
    }
    
  // FIXME: make autoconf test?
  jshort endianTestShort[] = { 1 };
  jbyte* endianTestByte = reinterpret_cast<jbyte*>(endianTestShort);
    
  jint byteOrder;
  if (endianTestByte[0] == 1)
    {
      // little endian machine
      byteOrder = LEAST_SIGNIFICANT_B_FIRST_ORDER;
    }
  else
    {
      // big endian machine
      byteOrder = MOST_SIGNIFICANT_B_FIRST_ORDER;
    }
  /* NB: This doesn't consider those weird machines out there with
     middle-endian byte order. */
    
  if (byteOrder != ximage->byte_order)
    {
      ximage->byte_order = byteOrder;
      reinitialize = true;
    }
    
  if (reinitialize)
    XInitImage(ximage);
    
  structure = reinterpret_cast<gnu::gcj::RawData*>(ximage);
  // Notice that no image data has been allocated at this point
}

void gnu::gcj::xlib::XImage::init(Visual* visual, 
				  jint width,
				  jint height)
{
  int depth = visual->getDepth();
    
  int format = ZPixmap; // Chunky, not planar.
  int offset = 0;
  int bitmapPad = 32; // FIXME, don't hardcode this
  int bytesPerLine = 0; // Let the server figure it out

  init(visual, depth, format, offset, width, height, bitmapPad,
       bytesPerLine, 0);
}

void gnu::gcj::xlib::XImage::internalSetData(jbyteArray data, jint offset)
{
  ::XImage* ximage = (::XImage*) structure;
  ximage->data = reinterpret_cast<char*>(elements(data)+offset);
}

void gnu::gcj::xlib::XImage::internalSetData(jshortArray data, jint offset)
{
  ::XImage* ximage = (::XImage*) structure;
  ximage->data = reinterpret_cast<char*>(elements(data)+offset);
}

void gnu::gcj::xlib::XImage::internalSetData(jintArray data, jint offset)
{
  ::XImage* ximage = (::XImage*) structure;
  ximage->data = reinterpret_cast<char*>(elements(data)+offset);
}

void gnu::gcj::xlib::XImage::finalize()
{
  ::XImage* ximage = (::XImage*) structure;
  if (ownsData)
    delete ximage->data;
  
  ximage->data = 0; // Never allow XLib to free the data allocation.
  dataRef = 0;
  XDestroyImage(ximage);
}

jint gnu::gcj::xlib::XImage::getWidth()
{
  ::XImage* ximage = (::XImage*) structure;
  return ximage->width;
}

jint gnu::gcj::xlib::XImage::getHeight()
{
  ::XImage* ximage = (::XImage*) structure;
  return ximage->height;
}

jint gnu::gcj::xlib::XImage::getDepth()
{
  ::XImage* ximage = (::XImage*) structure;
  return ximage->depth;
}

jint gnu::gcj::xlib::XImage::getFormat()
{
  ::XImage* ximage = (::XImage*) structure;
  return ximage->format;
}

jint gnu::gcj::xlib::XImage::getXOffset()
{
  ::XImage* ximage = (::XImage*) structure;
  return ximage->xoffset;
}

jint gnu::gcj::xlib::XImage::getImageByteOrder()
{
  ::XImage* ximage = (::XImage*) structure;
  return ximage->byte_order;
}

jint gnu::gcj::xlib::XImage::getBitmapBitOrder()
{
  ::XImage* ximage = (::XImage*) structure;
  return ximage->bitmap_bit_order;
}

jint gnu::gcj::xlib::XImage::getBitmapUnit()
{
  ::XImage* ximage = (::XImage*) structure;
  return ximage->bitmap_unit;
}

jint gnu::gcj::xlib::XImage::getBitmapPad()
{
  ::XImage* ximage = (::XImage*) structure;
  return ximage->bitmap_pad;
}

jint gnu::gcj::xlib::XImage::getBytesPerLine()
{
  ::XImage* ximage = (::XImage*) structure;
  return ximage->bytes_per_line;
}

jint gnu::gcj::xlib::XImage::getBitsPerPixel()
{
  ::XImage* ximage = (::XImage*) structure;
  return ximage->bits_per_pixel;
}


// True/Direct Color specific:

jint gnu::gcj::xlib::XImage::getRedMask()
{
  ::XImage* ximage = (::XImage*) structure;
  return ximage->red_mask;
}

jint gnu::gcj::xlib::XImage::getGreenMask()
{
  ::XImage* ximage = (::XImage*) structure;
  return ximage->green_mask;
}

jint gnu::gcj::xlib::XImage::getBlueMask()
{
  ::XImage* ximage = (::XImage*) structure;
  return ximage->blue_mask;
}

void gnu::gcj::xlib::XImage::setPixel(jint x, jint y, jint pixel)
{
  ::XImage* ximage = (::XImage*) structure;
  XPutPixel(ximage, x, y, pixel);
}
