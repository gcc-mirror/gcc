/* AppletStub.java -- Low level interface to the browser.
   Copyright (C) 1999 Free Software Foundation, Inc.
 
This file is part of GNU Classpath.
 
GNU Classpath is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2, or (at your option)
any later version.
 
GNU Classpath is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
General Public License for more details.
 
You should have received a copy of the GNU General Public License
along with GNU Classpath; see the file COPYING.  If not, write to the
Free Software Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA
02111-1307 USA.
 
As a special exception, if you link this library with other files to
produce an executable, this library does not by itself cause the
resulting executable to be covered by the GNU General Public License.
This exception does not however invalidate any other reasons why the
executable file might be covered by the GNU General Public License. */

package java.applet;

import java.net.URL;

/**
  * This interface is the low level interface between the applet and the
  * browser.
  *
  * @author Aaron M. Renn (arenn@urbanophile.com)
  */
public interface AppletStub
{
  /**
    * Returns the URL of the document this applet is embedded in.
    *
    * @return The URL of the document this applet is embedded in.
    */
  URL getDocumentBase();

  /**
    * Returns the URL of the code base for this applet.
    *
    * @return The URL of the code base for this applet.
    */
  URL getCodeBase();

  /**
    * Returns the value of the specified parameter that was specified in 
    * the &lt;APPLET&gt; tag for this applet.
    *
    * @param name The parameter name.
    *
    * @param value The parameter value, or <code>null</code> if the parameter
    * does not exist.
    */
  String getParameter(String name);

  /**
    * Returns the applet context for this applet.
    *
    * @return The applet context for this applet.
    */
  AppletContext getAppletContext();

  /**
    * Tests whether or not this applet is currently active.
    *
    * @return <code>true</code> if this applet is active, <code>false</code>
    * otherwise.
    */
  boolean isActive();

  /**
    * Requests that the applet window for this applet be resized.
    *
    * @param width The new width in pixels.
    * @param height The new height in pixels.
    */
  void appletResize(int width, int height);

} // interface AppletStub

