/* Copyright (C) 2000  Free Software Foundation

   This file is part of libgcj.

This software is copyrighted work licensed under the terms of the
Libgcj License.  Please consult the file "LIBGCJ_LICENSE" for
details.  */

#include <X11/Xlib.h>
#include <X11/Xutil.h>

#include <gcj/cni.h>
#include <gnu/gcj/xlib/Visual.h>
#include <gnu/gcj/xlib/Screen.h>
#include <gnu/gcj/xlib/Display.h>
#include <gnu/gcj/xlib/XException.h>
#include <gnu/gcj/RawData.h>

using namespace gnu::gcj;

void gnu::gcj::xlib::Visual::init(RawData* visual, jint depth)
{
  XVisualInfo* info = new XVisualInfo;
  xVisualInfo = reinterpret_cast<gnu::gcj::RawData*>(info);
  infoMask = 0;
    
  if (visual != 0)
    {
      ::Visual* visualStructure = (::Visual*) visual;
      info->visual = visualStructure;
      info->visualid = XVisualIDFromVisual(visualStructure);
      infoMask |= MASK_ID | MASK_VISUAL_STRUCTURE;
    }

  if (depth  != 0)
    {
      info->depth = depth;
      infoMask |= MASK_DEPTH;
    }
}

void gnu::gcj::xlib::Visual::finalize()
{
  if (xVisualInfo != 0)
    {
      delete xVisualInfo;
      xVisualInfo = 0;
    }
}

RawData* gnu::gcj::xlib::Visual::getVisualStructure()
{
    ensureXVisualInfo(MASK_ALL); // Make sure structure is set
    XVisualInfo* info = (XVisualInfo*) xVisualInfo;
    return reinterpret_cast<gnu::gcj::RawData*>(info->visual);
}

jint gnu::gcj::xlib::Visual::getRedMask()
{
  ensureXVisualInfo(MASK_RED);
  XVisualInfo* info = (XVisualInfo*) xVisualInfo;
  return info->red_mask;
}

jint gnu::gcj::xlib::Visual::getGreenMask()
{
  ensureXVisualInfo(MASK_GREEN);
  XVisualInfo* info = (XVisualInfo*) xVisualInfo;
  return info->green_mask;
}

jint gnu::gcj::xlib::Visual::getBlueMask()
{
  ensureXVisualInfo(MASK_BLUE);
  XVisualInfo* info = (XVisualInfo*) xVisualInfo;
  return info->blue_mask;
}

jint gnu::gcj::xlib::Visual::getScreenNumber()
{
  if (screen != 0)
    return screen->getScreenNumber();

  ensureXVisualInfo(MASK_SCREEN);
  XVisualInfo* info = (XVisualInfo*) xVisualInfo;
  return info->screen;
}

jint gnu::gcj::xlib::Visual::getDepth()
{
  ensureXVisualInfo(MASK_DEPTH);
  
  XVisualInfo* info = (XVisualInfo*) xVisualInfo;
  return info->depth;
}

jint gnu::gcj::xlib::Visual::getVisualClass()
{
  ensureXVisualInfo(MASK_CLASS);
  ::XVisualInfo* info = (::XVisualInfo*) xVisualInfo;
  return info->c_class;
}

void gnu::gcj::xlib::Visual::ensureXVisualInfo(jint requiredMask)
{
  int missingInformation = ~infoMask;
  if ((missingInformation & requiredMask) == 0)
    return;
  
  // We need more info...

  XVisualInfo* info = (XVisualInfo*) xVisualInfo;

  // Store everything we know into template
  if (screen != 0)
    {
      info->screen = screen->getScreenNumber();
      infoMask |= MASK_SCREEN;
    }
  
  // Aquire info using the current info as template for matching
  ::Display* dpy = (::Display*) display->display;
  int visualInfoCount;

  long mask = infoMask & MASK_ALL & (~MASK_VISUAL_STRUCTURE);
  XVisualInfo* matches = XGetVisualInfo(dpy, mask,
					  info, &visualInfoCount);
  if (matches != 0)
    {
      (*info) = matches[0];

      // redundant?
      xVisualInfo = reinterpret_cast<gnu::gcj::RawData*>(info);

      infoMask = ~0; // ALL
      XFree(matches);
    } 
  else 
    {
      char msg[] = 
	"XGetVisualInfo failed to find any matching visuals. The template "
	"describes a combination of properties that does not exist on "
	"this X server.";
      throw new XException(JvNewStringLatin1(msg));
    }
}
