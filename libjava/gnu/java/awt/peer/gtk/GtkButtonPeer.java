/* GtkButtonPeer.java -- Implements ButtonPeer with GTK
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

import java.awt.AWTEvent;
import java.awt.Button;
import java.awt.Component;
import java.awt.Font;
import java.awt.Point;
import java.awt.event.MouseEvent;
import java.awt.event.KeyEvent;
import java.awt.peer.ButtonPeer;

public class GtkButtonPeer extends GtkComponentPeer
    implements ButtonPeer
{
  native void create (String label);

  public native void connectJObject ();
  public native void connectSignals ();

  native void gtkSetFont (String name, int style, int size);
  native void gtkSetLabel (String label);
  native void gtkWidgetSetForeground (int red, int green, int blue);
  native void gtkActivate ();

  public GtkButtonPeer (Button b)
  {
    super (b);
  }

  void create ()
  {
    create (((Button) awtComponent).getLabel ());
  }

  public void setLabel (String label) 
  {
    gtkSetLabel(label);
  }

  public void handleEvent (AWTEvent e)
  {
    if (e.getID () == MouseEvent.MOUSE_RELEASED && isEnabled ())
      {
	MouseEvent me = (MouseEvent) e;
	Point p = me.getPoint();
	p.translate(((Component) me.getSource()).getX(),
	            ((Component) me.getSource()).getY());
	if (!me.isConsumed ()
	    && (me.getModifiersEx () & MouseEvent.BUTTON1_DOWN_MASK) != 0
	    && awtComponent.getBounds().contains(p))
	  postActionEvent (((Button)awtComponent).getActionCommand (), 
			   me.getModifiersEx ());
      }

    if (e.getID () == KeyEvent.KEY_PRESSED)
      {
	KeyEvent ke = (KeyEvent) e;
	if (!ke.isConsumed () && ke.getKeyCode () == KeyEvent.VK_SPACE)
          {
            postActionEvent (((Button) awtComponent).getActionCommand (),
                             ke.getModifiersEx ());
            gtkActivate ();
          }
      }

    super.handleEvent (e);
  }
}
