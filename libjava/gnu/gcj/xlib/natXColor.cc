/* Copyright (C) 2000  Free Software Foundation

   This file is part of libgcj.

This software is copyrighted work licensed under the terms of the
Libgcj License.  Please consult the file "LIBGCJ_LICENSE" for
details.  */

#include <X11/Xlib.h>

#include <gcj/cni.h>
#include <gnu/gcj/RawData.h>
#include <gnu/gcj/xlib/XColor.h>

void gnu::gcj::xlib::XColor::init()
{
  structure = reinterpret_cast<gnu::gcj::RawData*>(new ::XColor);
}

void gnu::gcj::xlib::XColor::finalize()
{
  delete structure;
}

void gnu::gcj::xlib::XColor::setRGB(jint r, jint g, jint b)
{
  ::XColor* color = (::XColor*) structure;
  color->red   = r;
  color->green = g;
  color->blue  = b;
}

jint gnu::gcj::xlib::XColor::getRed()
{
  ::XColor* color = (::XColor*) structure;
  return color->red;
}

jint gnu::gcj::xlib::XColor::getGreen()
{
  ::XColor* color = (::XColor*) structure;
  return color->green;
}

jint gnu::gcj::xlib::XColor::getBlue()
{
  ::XColor* color = (::XColor*) structure;
  return color->blue;
}

jbyte gnu::gcj::xlib::XColor::getFlags()
{
  ::XColor* color = (::XColor*) structure;
  return color->flags;
}

jlong gnu::gcj::xlib::XColor::getPixelValue()
{
  ::XColor* color = (::XColor*) structure;
  return color->pixel;
}
