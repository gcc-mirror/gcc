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


package java.awt.dnd;

import java.awt.Component;
import java.awt.Cursor;
import java.awt.Image;
import java.awt.Point;
import java.awt.datatransfer.Transferable;
import java.awt.dnd.peer.DragSourceContextPeer;
import java.io.Serializable;
import java.util.TooManyListenersException;

/**
 * @since 1.2
 */
public class DragSourceContext
  implements DragSourceListener, DragSourceMotionListener, Serializable
{
  /**
   * Compatible with JDK 1.2+
   */
  static final long serialVersionUID = -115407898692194719L;

  protected static final int DEFAULT = 0;
  protected static final int ENTER = 1;
  protected static final int OVER = 2;
  protected static final int CHANGED = 3;

  private DragSourceContextPeer peer;
  private Cursor cursor;
  private Transferable transferable;
  private DragGestureEvent trigger;
  private DragSourceListener dragSourceListener;
  private boolean useCustomCursor; // FIXME: currently unused but needed for serialization.
  private int sourceActions; // FIXME: currently unused but needed for serialization.
  private Image image;
  private Point offset;
  
  /**
   * Initializes a drag source context.
   *
   * @exception IllegalArgumentException If Component or DragSource of trigger
   * are null, the drag action for the trigger event is DnDConstants.ACTION_NONE
   * or if the source actions for the DragGestureRecognizer associated with the
   * trigger event are equal to DnDConstants.ACTION_NONE.
   * @exception NullPointerException If peer or trigger is null.
   */
  public DragSourceContext (DragSourceContextPeer peer,
                            DragGestureEvent trigger, Cursor cursor,
                            Image image, Point offset, Transferable trans,
                            DragSourceListener dsl)
  {
    if (peer == null
        || trigger == null)
      throw new NullPointerException ();

    if (trigger.getComponent () == null
        || trigger.getDragSource () == null
        || trigger.getDragAction () == DnDConstants.ACTION_NONE
        || trigger.getSourceAsDragGestureRecognizer ()
              .getSourceActions () == DnDConstants.ACTION_NONE)
      throw new IllegalArgumentException ();

    this.peer = peer;
    this.trigger = trigger;
    this.cursor = cursor;
    this.image = image;
    this.offset = offset;
    this.transferable = trans;
    this.dragSourceListener = dsl;
    
    throw new Error ("not implemented");
  }

  public DragSource getDragSource()
  {
    return trigger.getDragSource ();
  }

  public Component getComponent()
  {
    return trigger.getComponent ();
  }

  public DragGestureEvent getTrigger()
  {
    return trigger;
  }

  public int getSourceActions()
  {
    return trigger.getSourceAsDragGestureRecognizer ().getSourceActions ();
  }

  public void setCursor (Cursor cursor)
  {
    this.cursor = cursor;
    // FIXME: Check if we need to do more here
  }

  public Cursor getCursor()
  {
    return cursor;
  }

  /**
   * Adds a <code>DragSourceListener</code>.
   *
   * @exception TooManyListenersException If a <code>DragSourceListener</code>
   * has already been added.
   */
  public void addDragSourceListener (DragSourceListener dsl)
    throws TooManyListenersException
  {
    if (dragSourceListener != null)
      throw new TooManyListenersException ();

    dragSourceListener = dsl;
  }

  public void removeDragSourceListener (DragSourceListener dsl)
  {
    if (dragSourceListener == dsl)
      dragSourceListener = null;
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
    return transferable;
  }

  protected void updateCurrentCursor(int dropOp, int targetAct, int status)
  {
  }
} // class DragSourceContext
