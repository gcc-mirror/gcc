/* Copyright (C) 2000, 2003  Free Software Foundation

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
  return fontStruct->max_bounds.ascent+1;   // +1 to include the baseline
}

jint gnu::gcj::xlib::Font::getMaxDescent()
{
  XFontStruct* fontStruct = (XFontStruct*) structure;
  return fontStruct->max_bounds.descent-1;  // -1 to exclude the baseline
}

jint gnu::gcj::xlib::Font::getAscent()
{
  XFontStruct* fontStruct = (XFontStruct*) structure;
  jint returnValue = fontStruct->ascent;
  if (fontStruct->min_byte1==0 && fontStruct->min_char_or_byte2<=(unsigned)'O')
    returnValue = fontStruct
        ->per_char[(unsigned)'O'-fontStruct->min_char_or_byte2]
        .ascent;
  return returnValue+1;  // +1 to include the baseline
}

jint gnu::gcj::xlib::Font::getDescent()
{
  XFontStruct* fontStruct = (XFontStruct*) structure;
  jint returnValue = fontStruct->descent;
  if (fontStruct->min_byte1==0 && fontStruct->min_char_or_byte2<=(unsigned)'y')
    returnValue = fontStruct
        ->per_char[(unsigned)'y'-fontStruct->min_char_or_byte2]
        .descent;
  return returnValue-1;  // -1 to exclude the baseline
}

jint gnu::gcj::xlib::Font::getStringWidth(java::lang::String* text)
{
  XFontStruct* fontStruct = (XFontStruct*) structure;
  
  // FIXME: Convert to the character set used in the font, which may
  // or may not be unicode. For now, treat everything as 16-bit and
  // use character codes directly, which should be OK for unicode or
  // 8-bit ascii fonts.
  jint length = text->length();
  jchar* txt = JvGetStringChars(text);
  XChar2b xwchars[length];
  for (int i=0; i<length; i++)
    {
      XChar2b* xc = &(xwchars[i]);
      jchar jc = txt[i];
      xc->byte1 = (jc >> 8) & 0xff;
      xc->byte2 = jc & 0xff;
    }
  return XTextWidth16(fontStruct, xwchars, length);
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

