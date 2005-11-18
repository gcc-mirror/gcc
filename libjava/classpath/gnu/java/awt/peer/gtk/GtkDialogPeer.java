/* GtkDialogPeer.java -- Implements DialogPeer with GTK
   Copyright (C) 1998, 1999, 2002 Free Software Foundation, Inc.

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


package gnu.java.awt.peer.gtk;

import java.awt.Dialog;
import java.awt.Graphics;
import java.awt.Rectangle;
import java.awt.event.PaintEvent;
import java.awt.peer.DialogPeer;

public class GtkDialogPeer extends GtkWindowPeer
  implements DialogPeer
{
  public GtkDialogPeer (Dialog dialog)
  {
    super (dialog);
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
  
  protected void postMouseEvent(int id, long when, int mods, int x, int y, 
				int clickCount, boolean popupTrigger)
  {
    super.postMouseEvent (id, when, mods, 
			  x + insets.left, y + insets.top, 
			  clickCount, popupTrigger);
  }

  protected void postExposeEvent (int x, int y, int width, int height)
  {
    if (!isInRepaint)
      q().postEvent (new PaintEvent (awtComponent, PaintEvent.PAINT,
                                   new Rectangle (x + insets.left, 
                                                  y + insets.top, 
                                                  width, height)));
  }

  void create ()
  {
    Dialog dialog = (Dialog) awtComponent;
    
    // Create a decorated dialog window.
    create (GDK_WINDOW_TYPE_HINT_DIALOG, !((Dialog) awtComponent).isUndecorated ());

    gtkWindowSetModal (dialog.isModal ());
    setTitle (dialog.getTitle ());
    setResizable (dialog.isResizable ());
  }
}
