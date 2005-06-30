/* GtkCanvasPeer.java
   Copyright (C) 1998, 1999 Free Software Foundation, Inc.

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

import java.awt.AWTEvent;
import java.awt.Canvas;
import java.awt.Dimension;
import java.awt.Graphics;
import java.awt.event.PaintEvent;
import java.awt.peer.CanvasPeer;

public class GtkCanvasPeer extends GtkComponentPeer implements CanvasPeer
{
  native void create ();

  public GtkCanvasPeer (Canvas c)
  {
    super (c);
  }

  public Graphics getGraphics ()
  {
    if (GtkToolkit.useGraphics2D ())
      return new GdkGraphics2D (this);
    else
    return new GdkGraphics (this);
  }

  public void handleEvent (AWTEvent event)
  {
    int id = event.getID();
      
    switch (id)
      {
      case PaintEvent.PAINT:
      case PaintEvent.UPDATE:
	{
	  try 
	    {
	      Graphics g = getGraphics ();
	      g.setClip (((PaintEvent)event).getUpdateRect());
		
	      if (id == PaintEvent.PAINT)
		awtComponent.paint (g);
	      else
		awtComponent.update (g);
	      
	      g.dispose ();
	    } 
	  catch (InternalError e)
	    { 
	      System.err.println (e);
	    }
	}
	break;
      }
  }

  /* Preferred size for a drawing widget is always what the user requested */
  public Dimension getPreferredSize ()
  {
    return awtComponent.getSize ();
  }
}
