/* DragSourceContext.java --
   Copyright (C) 2002 Free Software Foundation

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


package java.awt.dnd;

import java.awt.Component;
import java.awt.Cursor;
import java.awt.Image;
import java.awt.Point;
import java.awt.datatransfer.Transferable;
import java.awt.dnd.peer.DragSourceContextPeer;
import java.io.Serializable;
import java.util.TooManyListenersException;

public class DragSourceContext
  implements DragSourceListener, DragSourceMotionListener, Serializable
{
  static final long serialVersionUID = -115407898692194719L;

  protected static final int DEFAULT = 0;
  protected static final int ENTER = 1;
  protected static final int OVER = 2;
  protected static final int CHANGED = 3;

  public DragSourceContext(DragSourceContextPeer peer, DragGestureEvent dge,
                           Cursor cursor, Image image, Point offset,
                           Transferable trans, DragSourceListener dsl)
  {
  }

  public DragSource getDragSource()
  {
    return null;
  }

  public Component getComponent()
  {
    return null;
  }

  public DragGestureEvent getTrigger()
  {
    return null;
  }

  public int getSourceActions()
  {
    return 0;
  }

  public void setCursor(Cursor c)
  {
  }

  public Cursor getCursor()
  {
    return null;
  }

  public void addDragSourceListener(DragSourceListener l)
    throws TooManyListenersException
  {
  }

  public void removeDragSourceListener(DragSourceListener l)
  {
  }

  public void transferablesFlavorsChanged()
  {
  }

  public void dragEnter(DragSourceDragEvent e)
  {
  }

  public void dragOver(DragSourceDragEvent e)
  {
  }

  public void dragExit(DragSourceEvent e)
  {
  }

  public void dropActionChanged(DragSourceDragEvent e)
  {
  }

  public void dragDropEnd(DragSourceDropEvent e)
  {
  }

  public void dragMouseMoved(DragSourceDragEvent e)
  {
  }

  public Transferable getTransferable()
  {
    return null;
  }

  protected void updateCurrentCursor(int dropOp, int targetAct, int status)
  {
  }
} // class DragSourceContext
