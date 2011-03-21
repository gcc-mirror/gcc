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
  private List<InputEvent> events;
  private DragGestureRecognizer dgr;

  /**
   * Constructs a new DragGestureEvent.
   * @param dgr - DragGestureRecognizer firing this event
   * @param action - user's preferred action
   * @param origin - origin of the drag
   * @param events - List of events that make up the gesture
   * @throws IllegalArgumentException - if input parameters are null
   */
  public DragGestureEvent(DragGestureRecognizer dgr, int action, Point origin,
                          List<? extends InputEvent> events)
  {
    super(dgr);
    if (origin == null || events == null || dgr == null)
      throw new IllegalArgumentException();

    this.origin = origin;
    this.action = action;
    this.events = (List<InputEvent>) events;
    this.dgr = dgr;
    this.component = dgr.getComponent();
    this.dragSource = dgr.getDragSource();
  }

  /**
   * Returns the source casted as a DragGestureRecognizer.
   *
   * @return the source casted as a DragGestureRecognizer.
   */
  public DragGestureRecognizer getSourceAsDragGestureRecognizer()
  {
    return (DragGestureRecognizer) getSource();
  }

  /**
   * Returns the Component corresponding to this.
   *
   * @return the Component corresponding to this.
   */
  public Component getComponent()
  {
    return component;
  }

  /**
   * Gets the DragSource corresponding to this.
   *
   * @return the DragSource corresponding to this.
   */
  public DragSource getDragSource()
  {
    return dragSource;
  }

  /**
   * Returns the origin of the drag.
   *
   * @return the origin of the drag.
   */
  public Point getDragOrigin()
  {
    return origin;
  }

  /**
   * Gets an iterator representation of the List of events.
   *
   * @return an iterator representation of the List of events.
   */
  public Iterator<InputEvent> iterator()
  {
    return events.iterator();
  }

  /**
   * Gets an array representation of the List of events.
   *
   * @return an array representation of the List of events.
   */
  public Object[] toArray()
  {
    return events.toArray();
  }

  /**
   * Gets an array representation of the List of events.
   *
   * @param array - the array to store the events in.
   * @return an array representation of the List of events.
   */
  public Object[] toArray(Object[] array)
  {
    return events.toArray(array);
  }

  /**
   * Gets the user's preferred action.
   *
   * @return the user's preferred action.
   */
  public int getDragAction()
  {
    return action;
  }

  /**
   * Get the event that triggered this gesture.
   *
   * @return the event that triggered this gesture.
   */
  public InputEvent getTriggerEvent()
  {
    return dgr.getTriggerEvent();
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
    dragSource.startDrag(this, dragCursor, dragImage, imageOffset, trans, l);
  }
} // class DragGestureEvent
