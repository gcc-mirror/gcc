/* GtkFramePeer.java -- Implements FramePeer with GTK
   Copyright (C) 1999, 2002 Free Software Foundation, Inc.

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
import java.awt.Graphics;
import java.awt.Graphics2D;
import java.awt.Image;
import java.awt.MenuBar;
import java.awt.Rectangle;
import java.awt.event.PaintEvent;
import java.awt.peer.FramePeer;
import java.awt.peer.MenuBarPeer;

public class GtkFramePeer extends GtkWindowPeer
    implements FramePeer
{
  int menuBarHeight = 0;
  native int getMenuBarHeight ();

  native public void setMenuBarPeer (MenuBarPeer bar);

  public void setMenuBar (MenuBar bar)
  {
    if (bar == null)
      setMenuBarPeer (null);
    else
      setMenuBarPeer ((MenuBarPeer) bar.getPeer ());
  }

  public GtkFramePeer (Frame frame)
  {
    super (frame);
  }

  void create ()
  {
    // Create a normal decorated window.
    create (GDK_WINDOW_TYPE_HINT_NORMAL, true);
  }

  public void getArgs (Component component, GtkArgList args)
  {
    super.getArgs (component, args);

    Frame frame = (Frame) component;

    args.add ("title", frame.getTitle ());
    args.add ("allow_shrink", frame.isResizable ());
    args.add ("allow_grow", frame.isResizable ());
  }

  public void setIconImage (Image image) 
  {
      /* TODO: Waiting on Toolkit Image routines */
  }

  public Graphics getGraphics ()
  {
    Graphics g;
    if (GtkToolkit.useGraphics2D ())
      g = new GdkGraphics2D (this);
    else
      g = new GdkGraphics (this);
    g.translate (-insets.left, -insets.top);
    return g;
  }

  // FIXME: When MenuBars work, override postConfigureEvent and
  // setBounds to account for MenuBar dimensions.

  protected void postMouseEvent(int id, long when, int mods, int x, int y, 
				int clickCount, boolean popupTrigger)
  {
    super.postMouseEvent (id, when, mods, 
			  x + insets.left, y + insets.top, 
			  clickCount, popupTrigger);
  }

  protected void postExposeEvent (int x, int y, int width, int height)
  {
    q.postEvent (new PaintEvent (awtComponent, PaintEvent.PAINT,
				 new Rectangle (x + insets.left, 
						y + insets.top, 
						width, height)));
  }

  public int getState ()
  {
    return 0;
  }

  public void setState (int state)
  {

  }

  public void setMaximizedBounds (Rectangle r)
  {

  }
}


