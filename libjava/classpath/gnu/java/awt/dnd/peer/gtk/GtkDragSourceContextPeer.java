/* GtkDragSourceContextPeer.java --
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


package gnu.java.awt.dnd.peer.gtk;

import gnu.java.awt.peer.gtk.GtkGenericPeer;

import java.awt.Component;
import java.awt.Cursor;
import java.awt.Image;
import java.awt.Point;
import java.awt.dnd.DragGestureEvent;
import java.awt.dnd.DragSourceContext;
import java.awt.dnd.DragSourceDragEvent;
import java.awt.dnd.DragSourceDropEvent;
import java.awt.dnd.DragSourceEvent;
import java.awt.dnd.InvalidDnDOperationException;
import java.awt.dnd.peer.DragSourceContextPeer;
import java.awt.peer.ComponentPeer;
import java.awt.peer.LightweightPeer;

public class GtkDragSourceContextPeer
    extends GtkGenericPeer
    implements DragSourceContextPeer
{
  private ComponentPeer peer;
  private Cursor cursor;
  private DragSourceContext context;
  public static Component target;
  
  native void nativeStartDrag(Image i, int x, int y, int action, String target);
  native void connectSignals(ComponentPeer comp);
  native void create(ComponentPeer comp);
  native void nativeSetCursor(int cursor);
  native void setTarget(GtkDropTargetContextPeer target);
  
  public GtkDragSourceContextPeer(DragGestureEvent e)
  {
    super(e.getComponent());
    Component comp = e.getComponent();
    peer = getComponentPeer(comp);
    
    create(peer);
    connectSignals(peer);
    cursor = comp.getCursor();
    
    // FIXME: Where do we set the target?
    
    if ((target != null))
      setTarget(new GtkDropTargetContextPeer(target));
  }
  
  ComponentPeer getComponentPeer(Component c)
  {
    if (c == null)
      return null;
    
    Component curr = c;
    while (curr.getPeer() instanceof LightweightPeer)
      curr = curr.getParent();
    
    if (curr != null)
      return curr.getPeer();
    return null;
  }
  
  public void startDrag(DragSourceContext context, Cursor c, Image i, Point p)
      throws InvalidDnDOperationException
  {   
    this.context = context;

    if (p == null)
      p = new Point();
    
    // FIXME: use proper DataFlavor, not "text/plain".
    // Also, add check to determine if dragging.
    
    setCursor(c);
    nativeStartDrag(i, p.x, p.y, context.getTrigger().getDragAction(),
                    "text/plain");
  }

  public Cursor getCursor()
  {
    return cursor;
  }

  public void setCursor(Cursor c) throws InvalidDnDOperationException
  {
    if (c != null)
      {
        nativeSetCursor(c.getType());
        cursor = c;
      }
  }

  public void transferablesFlavorsChanged()
  {
    // Nothing to do here.
  }
  
  /**
   * Called from native code.
   */

  public void dragEnter(int action, int modifiers)
  {
    context.dragEnter(new DragSourceDragEvent(context, action,
                                              action
                                                  & context.getSourceActions(),
                                              modifiers));
  }

  public void dragExit(int action, int x, int y)
  {
    context.dragExit(new DragSourceEvent(context, x, y));
  }

  public void dragDropEnd(int action, boolean success, int x, int y)
  {
    context.dragDropEnd(new DragSourceDropEvent(context, action, success, x, y));
  }

  public void dragMouseMoved(int action, int modifiers)
  {
    context.dragMouseMoved(new DragSourceDragEvent(context,
                                                   action,
                                                   action
                                                       & context.getSourceActions(),
                                                   modifiers));
  }

  public void dragOver(int action, int modifiers)
  {
    context.dragOver(new DragSourceDragEvent(context, action,
                                             action
                                                 & context.getSourceActions(),
                                             modifiers));
  }

  public void dragActionChanged(int newAction, int modifiers)
  {
    context.dropActionChanged(new DragSourceDragEvent(context,
                                                      newAction,
                                                      newAction
                                                          & context.getSourceActions(),
                                                      modifiers));
  }
}
