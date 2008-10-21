/* Copyright (C) 1999, 2000  Free Software Foundation

   This file is part of libgcj.

This software is copyrighted work licensed under the terms of the
Libgcj License.  Please consult the file "LIBGCJ_LICENSE" for
details.  */

#include <X11/Xproto.h>
#include <X11/Xlib.h>
#include <stdio.h>

#include <java/lang/System.h>
#include <java/lang/RuntimeException.h>
#include <java/io/PrintStream.h>
#include <gcj/cni.h>

#include <gnu/gcj/xlib/Display.h>
#include <gnu/gcj/xlib/XConnectException.h>
#include <gnu/gcj/xlib/XException.h>

void gnu::gcj::xlib::Display::init()
{
  ::Display* openedDisplay = XOpenDisplay(0); // default display
  
  if (openedDisplay == 0) {
    jstring msg = JvNewStringLatin1("Unable to open display");
    throw new gnu::gcj::xlib::XConnectException(msg);
  }
  
  display = reinterpret_cast<gnu::gcj::RawData*>(openedDisplay); 
}

void gnu::gcj::xlib::Display::finalize()
{
  if (display == 0) return;
  ::Display* dpy = (::Display*) display;
  XCloseDisplay(dpy);
}

jint gnu::gcj::xlib::Display::getDefaultScreenNumber()
{
  ::Display* dpy = (::Display*) display;
  return DefaultScreen(dpy);
}

jint gnu::gcj::xlib::Display::getDefaultRootWindowXID()
{
  ::Display* dpy = (::Display*) display;
  return DefaultRootWindow(dpy);
}

jint gnu::gcj::xlib::Display::internAtom(jstring name)
{
  ::Display* dpy = (::Display*) display;
  int len = JvGetStringUTFLength(name);
  char cName[len+1];
  JvGetStringUTFRegion(name, 0, name->length(), cName);
  cName[len] = '\0';
  bool onlyIfExists = false;
  return XInternAtom(dpy, cName, onlyIfExists);
}

jstring gnu::gcj::xlib::Display::getAtomName(jint atom)
{
  ::Display* dpy = (::Display*) display;
  char* cName = XGetAtomName(dpy, atom);
  jstring name = JvNewStringLatin1(cName);
  XFree(cName);
  return name;
}

static int handleXError(Display* dpy, XErrorEvent* xee)
{
  const int ERROR_TEXT_LENGTH = 256;
  char errorText[ERROR_TEXT_LENGTH];
  XGetErrorText(dpy, xee->error_code, errorText, ERROR_TEXT_LENGTH);
  int requestCode = xee->request_code;
  
  if (requestCode == X_GetImage)
    {
      /* The current implementation of Drawable.copyIntoXImage()
	 will generate harmless X_GetImage errors if the initially
	 requested area is not completly within the drawable. Until
	 we find a better solution, simply ignore these errors. */
      return 0;
    }

  java::lang::System::err->print(JvNewStringLatin1("X error: "));
  java::lang::System::err->print(JvNewStringLatin1(errorText));
  java::lang::System::err->print(JvNewStringLatin1(", serial="));
  java::lang::System::err->print((jlong) xee->serial);
  java::lang::System::err->print(JvNewStringLatin1(", requestCode="));
  java::lang::System::err->print((jint) requestCode);
  
  if (requestCode < 128)
    {
      char number[8];
      snprintf(number, 8, "%d", requestCode);
      number[7] = '\0';

      XGetErrorDatabaseText(dpy, "XRequest", number,
			    "", errorText, ERROR_TEXT_LENGTH);
      java::lang::System::err->print(JvNewStringLatin1(" ("));
      java::lang::System::err->print(JvNewStringLatin1(errorText));
      java::lang::System::err->print(JvNewStringLatin1(")"));
    }

  java::lang::System::err->print(JvNewStringLatin1(", minorCode="));
  java::lang::System::err->print((jint) xee->minor_code);
  java::lang::System::err->print(JvNewStringLatin1(", XID="));
  java::lang::System::err->println((jlong) xee->resourceid);
  
  return 0;
}

void gnu::gcj::xlib::Display::staticInit()
{
  if (XInitThreads() == 0)
    {
      char msg[] = "threads are not supported on this platform";
      throw new ::java::lang::RuntimeException(JvNewStringLatin1(msg));
    }
  
  XSetErrorHandler(&handleXError);
}

void gnu::gcj::xlib::Display::flush() 
{
  ::Display* dpy = (::Display*) display;
  XFlush(dpy);
}
