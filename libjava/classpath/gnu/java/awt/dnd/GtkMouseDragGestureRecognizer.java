/* GtkMouseDragGestureRecognizer.java --
   Copyright (C) 2006 Free Software Foundation, Inc.

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


package gnu.java.awt.dnd;

import java.awt.Component;
import java.awt.Point;
import java.awt.dnd.DnDConstants;
import java.awt.dnd.DragGestureListener;
import java.awt.dnd.DragSource;
import java.awt.dnd.MouseDragGestureRecognizer;
import java.awt.event.MouseEvent;

public class GtkMouseDragGestureRecognizer
    extends MouseDragGestureRecognizer
{

  public GtkMouseDragGestureRecognizer (DragSource ds)
  {
    this(ds, null, 0, null);
  }

  public GtkMouseDragGestureRecognizer (DragSource ds, Component c)
  {
    this (ds, c, 0, null);
  }

  public GtkMouseDragGestureRecognizer (DragSource ds, Component c, int act)
  {
    this(ds, c, act, null);
  }

  public GtkMouseDragGestureRecognizer (DragSource ds, Component c, int act,
                                        DragGestureListener dgl)
  {
    super(ds, c, act, dgl);
  }

  public void registerListeners ()
  {
    super.registerListeners();
  }

  public void unregisterListeners ()
  {
    super.unregisterListeners();
  }

  public void mouseClicked (MouseEvent e)
  {
    // Nothing to do here.
  }

  public void mousePressed (MouseEvent e)
  {
    events.clear();
    if (getDropActionFromEvent(e) != DnDConstants.ACTION_NONE)
      appendEvent(e);
  }

  public void mouseReleased (MouseEvent e)
  {
    events.clear();
  }

  public void mouseEntered (MouseEvent e)
  {
    events.clear();
  }

  public void mouseExited(MouseEvent e)
  {
    if (!events.isEmpty())
      if (getDropActionFromEvent(e) == DnDConstants.ACTION_NONE)
        events.clear();
  }

  public void mouseDragged(MouseEvent e)
  {
    if (!events.isEmpty())
      {
        int act = getDropActionFromEvent(e);

        if (act == DnDConstants.ACTION_NONE)
          return;

        Point origin = ((MouseEvent) events.get(0)).getPoint();
        Point current = e.getPoint();
        int dx = Math.abs(origin.x - current.x);
        int dy = Math.abs(origin.y - current.y);
        int threshold = DragSource.getDragThreshold();

        if (dx > threshold || dy > threshold)
          fireDragGestureRecognized(act, origin);
        else
          appendEvent(e);
      }
  }

  public void mouseMoved (MouseEvent e)
  {
    // Nothing to do here.
  }

  private int getDropActionFromEvent(MouseEvent e)
  {
    int modEx = e.getModifiersEx();
    int buttons =  modEx & (MouseEvent.BUTTON1_DOWN_MASK
               | MouseEvent.BUTTON2_DOWN_MASK | MouseEvent.BUTTON3_DOWN_MASK);
    if (!(buttons == MouseEvent.BUTTON1_DOWN_MASK ||
        buttons == MouseEvent.BUTTON2_DOWN_MASK))
      return DnDConstants.ACTION_NONE;

    // Convert modifier to a drop action
    int sourceActions = getSourceActions();
    int mod = modEx
              & (MouseEvent.SHIFT_DOWN_MASK | MouseEvent.CTRL_DOWN_MASK);
    switch (mod)
      {
      case MouseEvent.SHIFT_DOWN_MASK | MouseEvent.CTRL_DOWN_MASK:
        return DnDConstants.ACTION_LINK & sourceActions;
      case MouseEvent.CTRL_DOWN_MASK:
        return DnDConstants.ACTION_COPY & sourceActions;
      case MouseEvent.SHIFT_DOWN_MASK:
        return DnDConstants.ACTION_MOVE & sourceActions;
      default:
        if ((sourceActions & DnDConstants.ACTION_MOVE) != 0)
          return DnDConstants.ACTION_MOVE & sourceActions;
        else if ((sourceActions & DnDConstants.ACTION_COPY) != 0)
          return DnDConstants.ACTION_COPY & sourceActions;
        else if ((sourceActions & DnDConstants.ACTION_LINK) != 0)
          return DnDConstants.ACTION_LINK & sourceActions;
      }

    return DnDConstants.ACTION_NONE & sourceActions;
  }
}
