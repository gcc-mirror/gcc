/* CommonAppletStub.java -- an applet-browser interface class
   Copyright (C) 2003, 2006  Free Software Foundation, Inc.

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
Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
02110-1301 USA.

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

package gnu.classpath.tools.appletviewer;

import java.applet.AppletContext;
import java.applet.AppletStub;
import java.applet.Applet;
import java.net.MalformedURLException;
import java.net.URL;


class CommonAppletStub
  implements AppletStub
{
  private AppletTag tag;
  private AppletContext context;
  private Applet applet;

  CommonAppletStub(AppletTag tag, AppletContext context, Applet applet)
  {
    this.tag = tag;
    this.context = context;
    this.applet = applet;
  }

  ////////////////////////////////
  ////// AppletStub Methods //////
  ////////////////////////////////

  /**
   * Tests whether or not this applet is currently active. An applet
   * becomes active just before the browser invokes start (), and
   * becomes inactive just before the browser invokes stop ().
   *
   * @return true if applet is active, false otherwise
   */
  public boolean isActive()
  {
    return true;
  }

  /**
   * Returns the basename URL of the document in which this applet is
   * embedded.
   *
   * @return the document base url.
   */
  public URL getDocumentBase()
  {
    return tag.getDocumentBase();
  }

  /**
   * Returns the URL of the code base for this applet.
   *
   * @return the codebase url
   */
  public URL getCodeBase()
  {
    try
      {
        return tag.prependCodeBase("");
      }
    catch (MalformedURLException e)
      {
        throw new RuntimeException("Attempted to create"
                                   + " invalid codebase URL.", e);
      }
  }

  /**
   * Returns the value of the specified parameter that was specified
   * in the <code>APPLET</code> tag for this applet.
   *
   * @param name the key name
   *
   * @return the key value
   */
  public String getParameter(String name)
  {
    return tag.getParameter(name.toLowerCase());
  }

  /**
   * Returns the applet context for this applet.
   *
   * @return the context
   */
  public AppletContext getAppletContext()
  {
    return context;
  }

  /**
   * Requests that the applet window for this applet be resized.
   *
   * @param width the new witdh
   * @param height the new height
   */
  public void appletResize(int width, int height)
  {
    applet.setBounds (0, 0, width, height);
  }
}
