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
  private boolean useCustomCursor;
  private int sourceActions;
  private Image image;
  private Point offset;
  
  /**
   * Initializes a drag source context.
   *
   * @exception IllegalArgumentException If Component or DragSource of trigger
   * are null, the drag action for the trigger event is DnDConstants.ACTION_NONE
   * or if the source actions for the DragGestureRecognizer associated with the
   * trigger event are equal to DnDConstants.ACTION_NONE.
   * @exception NullPointerException If peer, trans or trigger is null or if the
   * image is not null but the offset is. 
   */
  public DragSourceContext (DragSourceContextPeer peer,
                            DragGestureEvent trigger, Cursor cursor,
                            Image image, Point offset, Transferable trans,
                            DragSourceListener dsl)
  {    
    if (peer == null
        || trigger == null || trans == null
        || (image != null && offset == null))
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
    this.sourceActions = trigger.getSourceAsDragGestureRecognizer().getSourceActions();
    
    setCursor(cursor);
    updateCurrentCursor(trigger.getDragAction(), sourceActions, DEFAULT);
  }

  /**
   * Returns the DragSource object associated with the
   * DragGestureEvent.
   * 
   * @return the DragSource associated with the trigger.
   */
  public DragSource getDragSource()
  {
    return trigger.getDragSource ();
  }

  /**
   * Returns the component associated with this.
   * 
   * @return the component associated with the trigger.
   */
  public Component getComponent()
  {
    return trigger.getComponent ();
  }

  /**
   * Gets the trigger associated with this.
   * 
   * @return the trigger.
   */
  public DragGestureEvent getTrigger()
  {
    return trigger;
  }

  /**
   * Returns the source actions for the DragGestureRecognizer.
   * 
   * @return the source actions for DragGestureRecognizer.
   */
  public int getSourceActions()
  {
    if (sourceActions == 0)
      sourceActions = trigger.getSourceAsDragGestureRecognizer().getSourceActions();
    return sourceActions;
  }

  /**
   * Sets the cursor for this drag operation to the specified cursor.
   * 
   * @param cursor c - the Cursor to use, or null to use the default drag
   *          cursor.
   */
  public void setCursor(Cursor cursor)
  {
    if (cursor == null)
      useCustomCursor = false;
    else
      useCustomCursor = true;
    this.cursor = cursor;
    peer.setCursor(cursor);
  }

  /**
   * Returns the current cursor or null if the default
   * drag cursor is used.
   * 
   * @return the current cursor or null.
   */
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

  /**
   * This function tells the peer that the DataFlavors have been modified.
   */
  public void transferablesFlavorsChanged()
  {
    peer.transferablesFlavorsChanged();
  }

  /**
   * Calls dragEnter on the listeners registered with this
   * and with the DragSource.
   * 
   * @param e - the DragSourceDragEvent
   */
  public void dragEnter(DragSourceDragEvent e)
  {
    if (dragSourceListener != null)
      dragSourceListener.dragEnter(e);
    
    DragSource ds = getDragSource();
    DragSourceListener[] dsl = ds.getDragSourceListeners();
    for (int i = 0; i < dsl.length; i++)
      dsl[i].dragEnter(e);
    
    updateCurrentCursor(e.getDropAction(), e.getTargetActions(), ENTER);
  }

  /**
   * Calls dragOver on the listeners registered with this
   * and with the DragSource.
   * 
   * @param e - the DragSourceDragEvent
   */
  public void dragOver(DragSourceDragEvent e)
  {
    if (dragSourceListener != null)
      dragSourceListener.dragOver(e);
    
    DragSource ds = getDragSource();
    DragSourceListener[] dsl = ds.getDragSourceListeners();
    for (int i = 0; i < dsl.length; i++)
      dsl[i].dragOver(e);
    
    updateCurrentCursor(e.getDropAction(), e.getTargetActions(), OVER);
  }
  
  /**
   * Calls dragExit on the listeners registered with this
   * and with the DragSource.
   * 
   * @param e - the DragSourceEvent
   */
  public void dragExit(DragSourceEvent e)
  {
    if (dragSourceListener != null)
      dragSourceListener.dragExit(e);
    
    DragSource ds = getDragSource();
    DragSourceListener[] dsl = ds.getDragSourceListeners();
    for (int i = 0; i < dsl.length; i++)
      dsl[i].dragExit(e);
    
    updateCurrentCursor(DnDConstants.ACTION_NONE, DnDConstants.ACTION_NONE,
                        DEFAULT);
  }

  /**
   * Calls dropActionChanged on the listeners registered with this
   * and with the DragSource.
   * 
   * @param e - the DragSourceDragEvent
   */
  public void dropActionChanged(DragSourceDragEvent e)
  {
    if (dragSourceListener != null)
      dragSourceListener.dropActionChanged(e);
    
    DragSource ds = getDragSource();
    DragSourceListener[] dsl = ds.getDragSourceListeners();
    for (int i = 0; i < dsl.length; i++)
      dsl[i].dropActionChanged(e);
    
    updateCurrentCursor(e.getDropAction(), e.getTargetActions(), CHANGED);
  }

  /**
   * Calls dragDropEnd on the listeners registered with this
   * and with the DragSource.
   * 
   * @param e - the DragSourceDropEvent
   */
  public void dragDropEnd(DragSourceDropEvent e)
  {
    if (dragSourceListener != null)
      dragSourceListener.dragDropEnd(e);
    
    DragSource ds = getDragSource();
    DragSourceListener[] dsl = ds.getDragSourceListeners();
    for (int i = 0; i < dsl.length; i++)
      dsl[i].dragDropEnd(e);
  }

  /**
   * Calls dragMouseMoved on the listeners registered with the DragSource.
   * 
   * @param e - the DragSourceDragEvent
   */
  public void dragMouseMoved(DragSourceDragEvent e)
  {
    DragSource ds = getDragSource();
    DragSourceMotionListener[] dsml = ds.getDragSourceMotionListeners();
    for (int i = 0; i < dsml.length; i++)
      dsml[i].dragMouseMoved(e);
  }

  /**
   * Returns the Transferable set with this object.
   * 
   * @return the transferable.
   */
  public Transferable getTransferable()
  {
    return transferable;
  }

  /**
   * This function sets the drag cursor for the specified operation, actions and
   * status if the default drag cursor is active. Otherwise, the cursor is not
   * updated in any way.
   * 
   * @param dropOp - the current operation.
   * @param targetAct - the supported actions.
   * @param status - the status of the cursor (constant).
   */
  protected void updateCurrentCursor(int dropOp, int targetAct, int status)
  {
    if (! useCustomCursor)
      {
        Cursor newCursor = null;
        switch (status)
          {
          default:
            targetAct = DnDConstants.ACTION_NONE;
          case ENTER:
          case CHANGED:
          case OVER:
            int action = dropOp & targetAct;
            if (action == DnDConstants.ACTION_NONE)
              {
                if ((dropOp & DnDConstants.ACTION_LINK) != 0)
                  newCursor = DragSource.DefaultLinkNoDrop;
                else if ((dropOp & DnDConstants.ACTION_MOVE) != 0)
                  newCursor = DragSource.DefaultMoveNoDrop;
                else
                  newCursor = DragSource.DefaultCopyNoDrop;
              }
            else
              {
                if ((dropOp & DnDConstants.ACTION_LINK) != 0)
                  newCursor = DragSource.DefaultLinkDrop;
                else if ((dropOp & DnDConstants.ACTION_MOVE) != 0)
                  newCursor = DragSource.DefaultMoveDrop;
                else
                  newCursor = DragSource.DefaultCopyDrop;
              }
          }
        
        if (cursor == null || ! cursor.equals(newCursor))
          {
            cursor = newCursor;
            DragSourceContextPeer p = peer;
            if (p != null)
              p.setCursor(cursor);
          }
      }
  }
} // class DragSourceContext
