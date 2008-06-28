/* EmbeddedWindow.java --
   Copyright (C) 2003, 2004 Free Software Foundation, Inc.

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


package gnu.java.awt;

import gnu.java.awt.peer.EmbeddedWindowPeer;
import gnu.java.security.action.SetAccessibleAction;

import java.awt.Component;
import java.awt.Frame;
import java.lang.reflect.Field;
import java.security.AccessController;

/**
 * Represents an AWT window that can be embedded into another
 * application.
 * 
 * @author Michael Koch (konqueror@gmx.de)
 */
public class EmbeddedWindow extends Frame
{
  private long handle;
  
  /**
   * Creates a window to be embedded into another application.  The
   * window will only be embedded after its setHandle method has been
   * called.
   */
  public EmbeddedWindow ()
  {
    super();
    this.handle = 0;
  }
  
  /**
   * Creates a window to be embedded into another application.
   *
   * @param handle the native handle to the screen area where the AWT
   * window should be embedded
   */
  public EmbeddedWindow (long handle)
  {
    super();
    this.handle = handle;
  }
  
  /**
   * Creates the native peer for this embedded window.
   */
  public void addNotify()
  {
    // Assume we're using ClasspathToolkit
    ClasspathToolkit tk = (ClasspathToolkit) getToolkit();

    // Circumvent the package-privateness of the AWT internal
    // java.awt.Component.peer member variable.
    try
      {
	Field peerField = Component.class.getDeclaredField("peer");
	AccessController.doPrivileged(new SetAccessibleAction(peerField));
	peerField.set(this, tk.createEmbeddedWindow (this));
      }
    catch (IllegalAccessException e)
      {
        throw new AssertionError (e);
      }
    catch (NoSuchFieldException e)
      {
        throw new AssertionError (e);
      }

    super.addNotify();
  }

  /**
   * If the native peer for this embedded window has been created,
   * then setHandle will embed the window.  If not, setHandle tells
   * us where to embed ourselves when our peer is created.
   *
   * @param handle the native handle to the screen area where the AWT
   * window should be embedded
   */
  public void setHandle(long handle)
  {
    if (this.handle != 0)
      throw new RuntimeException ("EmbeddedWindow is already embedded");

    this.handle = handle;
    if (getPeer() != null)
      ((EmbeddedWindowPeer) getPeer()).embed (this.handle);
  }

  /**
   * Gets the native handle of the screen area where the window will
   * be embedded.
   *
   * @return The native handle that was passed to the constructor.
   */
  public long getHandle()
  {
    return handle;
  }
}
