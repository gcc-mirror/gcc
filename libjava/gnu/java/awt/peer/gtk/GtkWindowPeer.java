/* GtkWindowPeer.java -- Implements WindowPeer with GTK
   Copyright (C) 1998, 1999, 2002, 2005  Free Software Foundation, Inc.

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


package gnu.java.awt.peer.gtk;

import java.awt.Component;
import java.awt.Frame;
import java.awt.Window;
import java.awt.event.WindowEvent;
import java.awt.peer.WindowPeer;

public class GtkWindowPeer extends GtkContainerPeer
  implements WindowPeer
{
  protected static final int GDK_WINDOW_TYPE_HINT_NORMAL = 0;
  protected static final int GDK_WINDOW_TYPE_HINT_DIALOG = 1;
  protected static final int GDK_WINDOW_TYPE_HINT_MENU = 2;
  protected static final int GDK_WINDOW_TYPE_HINT_TOOLBAR = 3;
  protected static final int GDK_WINDOW_TYPE_HINT_SPLASHSCREEN = 4;
  protected static final int GDK_WINDOW_TYPE_HINT_UTILITY = 5;
  protected static final int GDK_WINDOW_TYPE_HINT_DOCK = 6;
  protected static final int GDK_WINDOW_TYPE_HINT_DESKTOP = 7;

  private boolean hasBeenShown = false;
  private int oldState = Frame.NORMAL;

  native void gtkWindowSetTitle (String title);
  native void gtkWindowSetResizable (boolean resizable);
  native void gtkWindowSetModal (boolean modal);

  int getWidth ()
  {
    return awtComponent.getWidth();
  }

  int getHeight ()
  {
    return awtComponent.getHeight();
  }

  native void create (int type, boolean decorated, GtkWindowPeer parent);

  void create (int type, boolean decorated)
  {
    GtkWindowPeer parent_peer = null;
    Component parent = awtComponent.getParent();

    if (parent != null)
      parent_peer = (GtkWindowPeer) awtComponent.getParent().getPeer();

    create (type, decorated, parent_peer);
  }

  void create ()
  {
    // Create a normal undecorated window.
    create (GDK_WINDOW_TYPE_HINT_NORMAL, false);
  }

  void setParent ()
  {
    setVisible (awtComponent.isVisible ());
    setEnabled (awtComponent.isEnabled ());
  }

  void setVisibleAndEnabled ()
  {
  }

  native void connectSignals ();

  public GtkWindowPeer (Window window)
  {
    super (window);
  }

  public native void toBack();
  public native void toFront();

  native void nativeSetBounds (int x, int y, int width, int height);

  public void setBounds (int x, int y, int width, int height)
  {
    nativeSetBounds (x, y,
		     width - insets.left - insets.right,
		     height - insets.top - insets.bottom);
  }

  public void setTitle (String title)
  {
    gtkWindowSetTitle (title);
  }

  native void setSize (int width, int height);

  public void setResizable (boolean resizable)
  {
    // Call setSize; otherwise when resizable is changed from true to
    // false the window will shrink to the dimensions it had before it
    // was resizable.
    setSize (awtComponent.getWidth() - insets.left - insets.right,
	     awtComponent.getHeight() - insets.top - insets.bottom);
    gtkWindowSetResizable (resizable);
  }

  native void setBoundsCallback (Window window,
				 int x, int y,
				 int width, int height);

  protected void postInsetsChangedEvent (int top, int left,
					 int bottom, int right)
  {
    insets.top = top;
    insets.left = left;
    insets.bottom = bottom;
    insets.right = right;
  }

  protected void postConfigureEvent (int x, int y, int width, int height)
  {
    int frame_x = x - insets.left;
    int frame_y = y - insets.top;
    int frame_width = width + insets.left + insets.right;
    int frame_height = height + insets.top + insets.bottom;

    if (frame_x != awtComponent.getX()
	|| frame_y != awtComponent.getY()
	|| frame_width != awtComponent.getWidth()
	|| frame_height != awtComponent.getHeight())
      {
        setBoundsCallback ((Window) awtComponent,
                           frame_x, frame_y, frame_width, frame_height);

        awtComponent.validate();
      }
  }

  native void nativeSetVisible (boolean b);
  public void setVisible (boolean b)
  {
    // Prevent the window manager from automatically placing this
    // window when it is shown.
    if (b)
      setBounds (awtComponent.getX(),
		 awtComponent.getY(),
		 awtComponent.getWidth(),
		 awtComponent.getHeight());
    nativeSetVisible (b);
  }

  void postWindowEvent (int id, Window opposite, int newState)
  {
    if (id == WindowEvent.WINDOW_OPENED)
      {
	// Post a WINDOW_OPENED event the first time this window is shown.
	if (!hasBeenShown)
	  {
	    q().postEvent (new WindowEvent ((Window) awtComponent, id,
					  opposite));
	    hasBeenShown = true;
	  }
      }
    else if (id == WindowEvent.WINDOW_STATE_CHANGED)
      {
	if (oldState != newState)
	  {
	    q().postEvent (new WindowEvent ((Window) awtComponent, id, opposite,
					  oldState, newState));
	    oldState = newState;
	  }
      }
    else
      q().postEvent (new WindowEvent ((Window) awtComponent, id, opposite));
  }
}
