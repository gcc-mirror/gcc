/* CommonAppletContext.java -- a common applet's context
   Copyright (C) 2004, 2005, 2006  Free Software Foundation, Inc.

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

import java.applet.Applet;
import java.applet.AppletContext;
import java.applet.AudioClip;
import java.awt.Image;
import java.awt.Toolkit;
import java.io.InputStream;
import java.net.URL;
import java.util.ArrayList;
import java.util.Collections;
import java.util.Enumeration;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;


/*
 * CommonAppletContext represents the common context stuff for both
 * types, plugins and standalone.
 */
abstract class CommonAppletContext
  implements AppletContext
{
  // FIXME: this needs to be static, and we need one AppletContext per
  // Applet.
  List applets = new ArrayList();
  HashMap streams = new HashMap();

  void addApplet(Applet applet)
  {
    applets.add(applet);
  }

  ///////////////////////////////
  //// AppletContext methods ////
  ///////////////////////////////
  public AudioClip getAudioClip(URL url)
  {
    return Applet.newAudioClip(url);
  }

  public Image getImage(URL url)
  {
    return Toolkit.getDefaultToolkit().getImage(url);
  }

  public Applet getApplet(String name)
  {
    Applet a;
    String appletName;
    Iterator i = applets.iterator();

    while (i.hasNext())
      {
	a = (Applet) i.next();

	appletName = a.getParameter("name");
	if (a != null && appletName != null && appletName.equals(name))
	  return a;
      }
    return null;
  }

  public Enumeration getApplets()
  {
    return Collections.enumeration(applets);
  }

  public void showDocument(URL url)
  {
    showDocument(url, "_self");
  }

  /*
  // FIXME: implement.
  public abstract void showDocument (URL url, String target);

  // FIXME: implement.
  public abstract void showStatus (String status);
  */
  public void setStream(String key, InputStream stream)
  {
    streams.put(key, stream);
  }

  public InputStream getStream(String key)
  {
    return (InputStream) streams.get(key);
  }

  public Iterator getStreamKeys()
  {
    return streams.keySet().iterator();
  }
}
