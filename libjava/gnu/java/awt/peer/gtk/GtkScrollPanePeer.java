/* GtkScrollPanePeer.java -- Implements ScrollPanePeer with GTK
   Copyright (C) 1998, 1999, 2005  Free Software Foundation, Inc.

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

import java.awt.Adjustable;
import java.awt.Dimension;
import java.awt.ScrollPane;
import java.awt.peer.ScrollPanePeer;

public class GtkScrollPanePeer extends GtkContainerPeer
  implements ScrollPanePeer
{
  native void create (int width, int height);

  void create ()
  {
    create (awtComponent.getWidth (), awtComponent.getHeight ());
  }

  // native void gtkScrolledWindowSetScrollPosition(int x, int y);
  native void gtkScrolledWindowSetHScrollIncrement (int u);
  native void gtkScrolledWindowSetVScrollIncrement (int u);

  public GtkScrollPanePeer (ScrollPane sp)
  {
    super (sp);

    setPolicy (sp.getScrollbarDisplayPolicy ());
  }

  native void setPolicy (int policy);
  public void childResized (int width, int height)
  {
    int dim[] = new int[2];

    gtkWidgetGetDimensions (dim);

    // If the child is in this range, GTK adds both scrollbars, but
    // the AWT doesn't.  So set the peer's scroll policy to
    // GTK_POLICY_NEVER.
    if ((width > dim[0] - getVScrollbarWidth ()
	 && width <= dim[0])
	&& (height > dim[1] - getHScrollbarHeight ()
	    && height <= dim[1]))
      setPolicy (ScrollPane.SCROLLBARS_NEVER);
    else
      setPolicy (((ScrollPane) awtComponent).getScrollbarDisplayPolicy ());
  }

  public native int getHScrollbarHeight();
  public native int getVScrollbarWidth();
  public native void setScrollPosition(int x, int y);

  public Dimension getPreferredSize ()
  {
    return awtComponent.getSize();
  }

  public void setUnitIncrement (Adjustable adj, int u)
  {
    if (adj.getOrientation()==Adjustable.HORIZONTAL)
      gtkScrolledWindowSetHScrollIncrement (u);
    else
      gtkScrolledWindowSetVScrollIncrement (u);
  }

  public void setValue (Adjustable adj, int v)
  {
//      System.out.println("SPP: setVal: "+adj+":"+v);
//      Point p=myScrollPane.getScrollPosition ();
//      if (adj.getOrientation()==Adjustable.HORIZONTAL)
//        gtkScrolledWindowSetScrollPosition (v,p.y);
//      else
//        gtkScrolledWindowSetScrollPosition (p.x,v);
//      adj.setValue(v);
  }
}
