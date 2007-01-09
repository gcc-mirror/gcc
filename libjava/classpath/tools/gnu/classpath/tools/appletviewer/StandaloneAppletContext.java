/* StandaloneAppletContext.java -- an applet's context within the
   standalone viewer
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

import java.net.URL;
import java.util.Iterator;
import java.util.List;


/**
 * StandaloneAppletContext represents the context within a webpage of a
 * group of applets that all share the same codebase.
 */
class StandaloneAppletContext extends CommonAppletContext
{
  private List appletWindows;

  StandaloneAppletContext(List appletWindows)
  {
    this.appletWindows = appletWindows;
  }

  public void showDocument(URL url, String target)
  {
    System.err.println
      (Messages.getString("StandaloneAppletContext.ShowDocumentError"));
  }

  // In standalone mode, there are potentially several windows, each
  // with its own status bar.  In plugin mode, all the applets in the
  // same context (on the same page) share the browser's status bar.
  // The best way to simulate the plugin mode behaviour in standalone
  // mode is to show the same status on each window's status bar.
  public void showStatus(String status)
  {
    Iterator window = appletWindows.iterator();
    while (window.hasNext())
      ((StandaloneAppletWindow) window.next()).showStatus(status);
  }
}
