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
#include <java/lang/RuntimeException.h>
#include <gnu/gcj/xlib/Display.h>
#include <gnu/gcj/xlib/Screen.h>
#include <gnu/gcj/xlib/Colormap.h>
#include <gnu/gcj/xlib/XColor.h>
#include <gnu/gcj/RawData.h>

jlong gnu::gcj::xlib::Colormap::allocateColorPixel(XColor* color)
{
  ::Display* dpy = (::Display*) (screen->getDisplay()->display);
  ::XColor* col = (::XColor*) (color->structure);
  Status result = XAllocColor(dpy, xid, col);
  if (result == 0)
    throw new ::java::lang::RuntimeException(
      JvNewStringLatin1("Unable to allocate color pixel."));

  return col->pixel;
}

typedef JArray<gnu::gcj::xlib::XColor*>* xcolorarray;

xcolorarray gnu::gcj::xlib::Colormap::getSharedColors()
{
  ::Display* dpy = (::Display*) (screen->getDisplay()->display);
  unsigned int nCells = CellsOfScreen(ScreenOfDisplay(dpy, screen->screenNumber));

  typedef ::XColor xcolor;
  std::vector<xcolor> colors(nCells);
  for (unsigned int i=0; i<nCells; i++)
    colors[i].pixel = i;
  ::XColor* cols = colors.get_allocator().address(colors.front());
  XQueryColors(dpy, xid, cols,
	       nCells);

  int nShared = 0;
  for (unsigned int i=0; i<nCells; i++)
    {
      ::XColor color = colors[i];

      if (!XAllocColor(dpy, xid, &color))
	continue;

      /* FIXME: In some cases this algorithm may identify a free
	 color cell as a shared one. */
      if (color.pixel != i)
	{
	  // Oops, the color wasn't shared. Free it.
	  XFreeColors(dpy, xid, &(color.pixel), 1, 0);
	  colors[i].flags = FLAG_NOT_SHARED;
	  continue;
	}
      
      // FIXME: Shared or free?
      
      nShared++;
      colors[i].flags = FLAG_SHARED;
    }
  
  JArray<XColor*>* shared = newXColorArray(nShared);
  int si=0;
  for (unsigned int i=0; i<nCells; i++)
    {
      if (colors[i].flags != FLAG_SHARED)
	continue;
      
      XColor* col = elements(shared)[si++];
      gnu::gcj::RawData* colorData = col->structure;
      ::XColor* colStruct = reinterpret_cast<xcolor*>(colorData);
      *colStruct = colors[i];
    }

  return shared;
}

xcolorarray gnu::gcj::xlib::Colormap::getXColors()
{
  ::Display* dpy = (::Display*) (screen->getDisplay()->display);
  unsigned int nCells =
    CellsOfScreen(ScreenOfDisplay(dpy, screen->screenNumber));
  
  typedef ::XColor xcolor;
  std::vector<xcolor> colors(nCells);
  
  JArray<XColor*>* colArray = newXColorArray(nCells);
  
  for (unsigned int i=0; i<nCells; i++)
    colors[i].pixel = i;
  
  XQueryColors(dpy, xid, &(colors.front()), nCells);

  /* TODO: The current problem with this code is that it relies on
     (color.pixel == i) as an indicator that the color is
     shared. However, (color.pixel == i), may also occur simply
     because color cell i simply was the next free in the list of
     unallocated color cells.  IDEA: run through the list both
     backwards and forwards, and only pick out the colorcells that
     have been identified as shared during both passes.  Reversing the
     traversal direction might prevent i from corresponding to the
     next free colorcell, atleast in one of the passes. */
  for (unsigned int i=0; i<nCells; i++)
    {
      ::XColor color = colors[i];
      
      char flag = FLAG_NOT_SHARED;
      if (XAllocColor(dpy, xid, &color))
	{
	  if (color.pixel == i)
	    {
	      flag = FLAG_SHARED;
	    }
	  else
	    {
	      // Oops, the color wasn't shared. Free it.
	      XFreeColors(dpy, xid, &(color.pixel), 1, 0);
	    }
	}
      
      // Copy color data into object in array
      XColor* col = elements(colArray)[i];
      gnu::gcj::RawData* colorData = col->structure;
      ::XColor* colStruct = reinterpret_cast<xcolor*>(colorData);
      *colStruct = colors[i];
      colStruct->flags = flag;
    }
  
  return colArray;
}

