/* GtkWindowPeer.java -- Implements WindowPeer with GTK
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
import java.awt.peer.*;
import java.awt.*;

public class GtkWindowPeer extends GtkContainerPeer
  implements WindowPeer
{
  static protected final int GTK_WINDOW_TOPLEVEL = 0;
  static protected final int GTK_WINDOW_DIALOG = 1;
  static protected final int GTK_WINDOW_POPUP = 2;

  native void create (int type);

  void create ()
  {
    create (GTK_WINDOW_POPUP);
  }

  native void connectHooks ();

  public GtkWindowPeer (Window window)
  {
    super (window);

    Dimension d = window.getSize ();
    setBounds (0, 0, d.width, d.height);
  }

  public void getArgs (Component component, GtkArgList args)
  {
    args.add ("visible", component.isVisible ());
    args.add ("sensitive", component.isEnabled ());
  }
  
  native public void toBack ();
  native public void toFront ();

  native public void setBounds (int x, int y, int width, int height);

  public void setTitle (String title)
  {
    set ("title", title);
  }

  native public void setResizable (boolean r);

  protected void postConfigureEvent (int x, int y, int width, int height,
				     int top, int left, int bottom, int right)
  {
    /* 
       If our borders change (which often happens when we opaque resize),
       we need to make sure that a new layout will happen, since Sun
       forgets to handle this case.
    */
    if (insets.top != top
	|| insets.left != left
	|| insets.bottom != bottom
	|| insets.right != right)
      {
	awtComponent.invalidate ();
      }
    
    insets.top = top;
    insets.left = left;
    insets.bottom = bottom;
    insets.right = right;

    awtComponent.setBounds (x, y, width, height);
    awtComponent.validate ();
  }
  
  native public void setVisible (boolean b);
}
