/* DragGestureEvent.java --
   Copyright (C) 2002 Free Software Foundation, Inc.

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
import java.awt.event.InputEvent;
import java.util.EventObject;
import java.util.Iterator;
import java.util.List;

/**
 * STUBBED
 * @see DragGestureRecognizer
 * @see DragGestureListener
 * @see DragSource
 * @since 1.2
 */
public class DragGestureEvent extends EventObject
{
  /**
   * Compatible with JDK 1.2+.
   */
  private static final long serialVersionUID = 9080172649166731306L;

  private DragSource dragSource;
  private Component component;
  private final Point origin;
  private final int action;

  public DragGestureEvent(DragGestureRecognizer dgr, int action, Point origin,
                          List events)
  {
    super(dgr);
    if (origin == null || events == null)
      throw new IllegalArgumentException();
    this.origin = origin;
    this.action = action;
  }

  public DragGestureRecognizer getSourceAsDragGestureRecognizer()
  {
    return (DragGestureRecognizer) source;
  }
  public Component getComponent()
  {
    return null;
  }
  public DragSource getDragSource()
  {
    return null;
  }
  public Point getDragOrigin()
  {
    return origin;
  }
  public Iterator iterator()
  {
    return null;
  }
  public Object[] toArray()
  {
    return null;
  }
  public Object[] toArray(Object[] array)
  {
    return array;
  }
  public int getDragAction()
  {
    return 0;
  }
  public InputEvent getTriggerEvent()
  {
    return null;
  }

  /**
   * Starts the drag given the initial Cursor to display, the Transferable
   * object, and the DragSourceListener to use.
   *
   * @exception InvalidDnDOperationException If the Drag and Drop system is
   * unable to initiate a drag operation, or if the user attempts to start
   * a drag while an existing drag operation is still executing.
   */
  public void startDrag(Cursor dragCursor, Transferable trans)
  {
    startDrag(dragCursor, null, null, trans, null);
  }

  /**
   * Starts the drag given the initial Cursor to display, the Transferable
   * object, and the DragSourceListener to use.
   *
   * @exception InvalidDnDOperationException If the Drag and Drop system is
   * unable to initiate a drag operation, or if the user attempts to start
   * a drag while an existing drag operation is still executing.
   */
  public void startDrag(Cursor dragCursor, Transferable trans,
                        DragSourceListener l)
  {
    startDrag(dragCursor, null, null, trans, l);
  }

  /**
   * Starts the drag given the initial Cursor to display, the Transferable
   * object, and the DragSourceListener to use.
   *
   * @exception InvalidDnDOperationException If the Drag and Drop system is
   * unable to initiate a drag operation, or if the user attempts to start
   * a drag while an existing drag operation is still executing.
   */
  public void startDrag(Cursor dragCursor, Image dragImage, Point imageOffset,
                        Transferable trans, DragSourceListener l)
  {
  }
} // class DragGestureEvent
