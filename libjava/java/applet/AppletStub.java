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
 
Linking this library statically or dynamically with other modules is
making a combined work based on this library.  Thus, the terms and
conditions of the GNU General Public License cover the whole
combination.

As a special exception, the copyright holders of this library give you
permission to link this library with independent modules to produce an
executable, regardless of the license terms of these independent
modules, and to copy and distribute the resulting executable under
terms of your choice, provided that you also meet, for each linked
independent module, the terms and conditions of the license of that
module.  An independent module is a module which is not derived from
or based on this library.  If you modify this library, you may extend
this exception to your version of the library, but you are not
obligated to do so.  If you do not wish to do so, delete this
exception statement from your version. */

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

