/* DragGestureRecognizer.java --
   Copyright (C) 2002, 2005, 2006 Free Software Foundation, Inc.

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
import java.awt.Point;
import java.awt.event.InputEvent;
import java.io.IOException;
import java.io.ObjectInputStream;
import java.io.ObjectOutputStream;
import java.io.Serializable;
import java.util.ArrayList;
import java.util.TooManyListenersException;

/**
 * STUBBED
 * @author Michael Koch (konqueror@gmx.de)
 * @author Andrew John Hughes (gnu_andrew@member.fsf.org)
 * @since 1.2
 */
public abstract class DragGestureRecognizer implements Serializable
{
  /**
   * Compatible with JDK 1.2+.
   */
  private static final long serialVersionUID = 8996673345831063337L;

  protected DragSource dragSource;
  protected Component component;
  protected transient DragGestureListener dragGestureListener;
  protected int sourceActions;
  protected ArrayList<InputEvent> events = new ArrayList<InputEvent>();

  protected DragGestureRecognizer(DragSource ds, Component c, int sa,
                                  DragGestureListener dgl)
  {
    if (ds == null)
      throw new IllegalArgumentException();
    dragSource = ds;
    component = c;
    sourceActions = sa;
    dragGestureListener = dgl;
  }

  protected DragGestureRecognizer(DragSource ds, Component c, int sa)
  {
    this(ds, c, sa, null);
  }

  protected DragGestureRecognizer(DragSource ds, Component c)
  {
    this(ds, c, 0, null);
  }

  protected DragGestureRecognizer(DragSource ds)
  {
    this(ds, null, 0, null);
  }

  protected abstract void registerListeners();

  protected abstract void unregisterListeners();

  public DragSource getDragSource()
  {
    return dragSource;
  }

  public Component getComponent()
  {
    return component;
  }

  public void setComponent(Component c)
  {
    component = c;
  }

  public int getSourceActions()
  {
    return sourceActions;
  }

  public void setSourceActions(int sa)
  {
    sourceActions = sa;
  }

  public InputEvent getTriggerEvent()
  {
    return events.size() > 0 ? (InputEvent) events.get(0) : null;
  }

  /**
   * Resets the recognizer. If a gesture is currently recognize, discard it.
   */
  public void resetRecognizer()
  {
    events.clear();
  }

  /**
   * Register a new DragGestureListener.
   *
   * @exception TooManyListenersException If a DragGestureListener has already
   * been added.
   */
  public void addDragGestureListener(DragGestureListener dgl)
    throws TooManyListenersException
  {
    if (dragGestureListener != null)
      throw new TooManyListenersException();
    dragGestureListener = dgl;
  }

  public void removeDragGestureListener(DragGestureListener dgl)
  {
    if (dragGestureListener != dgl)
      throw new IllegalArgumentException();
    dragGestureListener = null;
  }

  /**
   * Fires a <code>DragGestureEvent</code> to the DragGestureListener
   * associated with this object, if there is one.
   */
  protected void fireDragGestureRecognized(int dragAction, Point p)
  {
    if(dragGestureListener != null)
      dragGestureListener.dragGestureRecognized
	(new DragGestureEvent(this, dragAction, p, events));
    resetRecognizer();
  }

  protected void appendEvent(InputEvent e)
  {
    if (e == null)
      return;
    events.add(e);
  }

  private void readObject(ObjectInputStream s)
    throws ClassNotFoundException, IOException
  {
    s.defaultReadObject();
    dragGestureListener = (DragGestureListener) s.readObject();
  }

  private void writeObject(ObjectOutputStream s) throws IOException
  {
    s.defaultWriteObject();
    s.writeObject(dragGestureListener instanceof Serializable
                  ? dragGestureListener : null);
  }
} // class DragGestureRecognizer
