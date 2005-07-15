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
#include <java/awt/Rectangle.h>
#include <gnu/gcj/xlib/Display.h>
#include <gnu/gcj/xlib/Window.h>
#include <gnu/gcj/xlib/WindowAttributes.h>
#include <gnu/gcj/xlib/Visual.h>
#include <gnu/gcj/xlib/XException.h>

jint gnu::gcj::xlib::Window::createChildXID(java::awt::Rectangle* bounds,
					jint borderWidth,
					WindowAttributes* attributes,
					jint windowIOClass, 
					Visual* visual)
{
  ::Window parentXID = xid;
  
  int x      = bounds->x;
  int y      = bounds->y;
  int width  = bounds->width;
  int height = bounds->height;
  
  long mask = attributes->mask;
  XSetWindowAttributes* attr = (XSetWindowAttributes*)
    attributes->getXSetWindowAttributesStructure();
  
  ::Visual* vis = CopyFromParent;
  int depth = CopyFromParent;
  if (visual != 0)
    {
      vis = (::Visual*) visual->getVisualStructure();
      depth = visual->getDepth();
    }

  ::Window childXID = XCreateWindow((::Display*) (display->display),
				    parentXID,
				    x, y, width, height,
				    borderWidth, depth, windowIOClass,
				    vis,
				    mask, attr);
  // no fast fail
  return childXID;
}

void gnu::gcj::xlib::Window::destroy()
{
  ::Display* dpy = (::Display*) (display->display);
  ::Window window = xid;
  XDestroyWindow(dpy, window);
  // no fast fail
}

void gnu::gcj::xlib::Window::setAttributes(WindowAttributes* attributes)
{
  ::Display* dpy = (::Display*) (display->display);
  ::Window window = xid;
  ::XSetWindowAttributes* attr = (::XSetWindowAttributes*)
      attributes->getXSetWindowAttributesStructure();

  XChangeWindowAttributes(dpy, window, attributes->mask, attr);
  // no fast fail
}

void gnu::gcj::xlib::Window::toBack()
{
  ::Display* dpy = (::Display*) (display->display);
  ::Window window = xid;
  XLowerWindow(dpy, window);
}

void gnu::gcj::xlib::Window::toFront()
{
  ::Display* dpy = (::Display*) (display->display);
  ::Window window = xid;
  XRaiseWindow(dpy, window);
}

void gnu::gcj::xlib::Window::map()
{
  ::Display* dpy = (::Display*) (display->display);
  ::Window window = xid;
  XMapWindow(dpy, window);
  // no fast fail
}

void gnu::gcj::xlib::Window::unmap()
{
  ::Display* dpy = (::Display*) (display->display);
  ::Window window = xid;
  XUnmapWindow(dpy, window);
  // no fast fail
}

void gnu::gcj::xlib::Window::setProperty(jint nameAtom, jint typeAtom, 
					 jbyteArray data)
{
  ::Display* dpy = (::Display*) (display->display);
  int format = 8;
  int mode = PropModeReplace;
  unsigned char* pData = (unsigned char*) elements(data);
  int len = data->length;
  
  XChangeProperty(dpy, xid, nameAtom, typeAtom, format, mode,
		  pData, len);
  // no fast fail
}

void gnu::gcj::xlib::Window::setWMProtocols(jintArray atoms)
{
  ::Display* dpy = (::Display*) (display->display);
  
  size_t length = atoms->length;
  jint* atomsBegin = elements(atoms);
  jint* atomsEnd   = atomsBegin + length;
  
  // Avoid confusion between Xlib.h and Atom.java "Atom" types.
  typedef ::Atom XLibAtom;
  
  std::vector<XLibAtom> atomVector(atomsBegin, atomsEnd);
  XLibAtom* atomsArray = &(atomVector.front());
  
  XSetWMProtocols(dpy, xid, atomsArray, length);
  // no fail fast
}

jintArray gnu::gcj::xlib::Window::getWMProtocols()
{
  ::Display* dpy = (::Display*) (display->display);
  
  ::Atom* protocolsReturn;
  int countReturn;
  
  Status success = XGetWMProtocols(dpy, xid, &protocolsReturn,
				   &countReturn);
  
  if (!success)
    throw new XException(JvNewStringLatin1("cannot get "
					   "WM protocols "));
  
  jintArray atoms;
  try
    {
      ::Atom* protocolsBegin = protocolsReturn;
      ::Atom* protocolsEnd = protocolsBegin + countReturn;
      
      atoms = JvNewIntArray(countReturn);
      jint* atomsBegin = elements(atoms);
      
      std::copy(protocolsBegin, protocolsEnd, atomsBegin);
      
    }
  catch (...)
    {
      XFree(protocolsReturn);
      throw;
    }
  XFree(protocolsReturn);
  
  return atoms;
}

void gnu::gcj::xlib::Window::setBounds(jint x, jint y,
				       jint width, jint height)
{
  ::Display* dpy = (::Display*) (display->display);
  
  XMoveResizeWindow(dpy, xid, x, y, width, height);
  // no fast fail
}
