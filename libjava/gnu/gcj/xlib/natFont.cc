/* Copyright (C) 2000  Free Software Foundation

   This file is part of libgcj.

This software is copyrighted work licensed under the terms of the
Libgcj License.  Please consult the file "LIBGCJ_LICENSE" for
details.  */

#include <X11/Xlib.h>

#include <gcj/cni.h>
#include <gnu/gcj/RawData.h>
#include <java/lang/String.h>

#include <gnu/gcj/xlib/Display.h>
#include <gnu/gcj/xlib/Font.h>
#include <gnu/gcj/xlib/XException.h>

gnu::gcj::RawData* gnu::gcj::xlib::Font::loadFont(Display* display,
						  jstring lfdNamePattern)
{
  ::Display* dpy = (::Display*) display->display;
  int len = JvGetStringUTFLength(lfdNamePattern);
  char cName[len+1];
  JvGetStringUTFRegion(lfdNamePattern, 0, lfdNamePattern->length(),
		       cName);
  cName[len] = '\0';

  XFontStruct* fontStruct = XLoadQueryFont(dpy, cName);
  if (fontStruct == 0)
    {
      throw new XException(JvNewStringLatin1("font not found"));
    }

  return reinterpret_cast<gnu::gcj::RawData*>(fontStruct);
}

jint gnu::gcj::xlib::Font::getXIDFromStruct(gnu::gcj::RawData* structure)
{
  XFontStruct* fontStruct = (XFontStruct*) structure;
  return fontStruct->fid;
}

jint gnu::gcj::xlib::Font::getMaxAscent()
{
  XFontStruct* fontStruct = (XFontStruct*) structure;
  return fontStruct->max_bounds.ascent;
}

jint gnu::gcj::xlib::Font::getMaxDescent()
{
  XFontStruct* fontStruct = (XFontStruct*) structure;
  return fontStruct->max_bounds.descent;
}

jint gnu::gcj::xlib::Font::getAscent()
{
  XFontStruct* fontStruct = (XFontStruct*) structure;
  return fontStruct->ascent;
}

jint gnu::gcj::xlib::Font::getDescent()
{
  XFontStruct* fontStruct = (XFontStruct*) structure;
  return fontStruct->ascent;
}

jint gnu::gcj::xlib::Font::getStringWidth(java::lang::String* text)
{
  XFontStruct* fontStruct = (XFontStruct*) structure;
  
  // FIXME: make proper unicode conversion
  int len = JvGetStringUTFLength(text);
  char ctxt[len+1];
  JvGetStringUTFRegion(text, 0, text->length(), ctxt);
  ctxt[len] = '\0';
  int width = XTextWidth(fontStruct, ctxt, len);
  return width;
}

void gnu::gcj::xlib::Font::finalize()
{
  if (structure != 0)
    {
      ::Display* dpy = (::Display*) display->display;
      XFontStruct* fontStruct = (XFontStruct*) structure;
      int result = XFreeFont(dpy, fontStruct);

      if (result == BadFont) 
	throw new XException(display, result);
      
      structure = 0; xid = 0;
    }
}

