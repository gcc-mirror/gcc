/* DropTargetDropEvent.java --
   Copyright (C) 2002, 2006 Free Software Foundation, Inc.

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

import java.awt.Point;
import java.awt.datatransfer.DataFlavor;
import java.awt.datatransfer.Transferable;
import java.util.List;

/**
 * @since 1.2
 */
public class DropTargetDropEvent extends DropTargetEvent
{
  /**
   * Compatible with JDK 1.2+
   */
  private static final long serialVersionUID = -1721911170440459322L;

  private final int dropAction;
  private final int actions;
  private final Point location;
  private final boolean isLocalTx;
  
  /**
   * Initializes a <code>DropTargetDropEvent</code>. By default this constructor
   * assumes that the target is not int same JVM.
   *
   * @exception IllegalArgumentException If dropAction is not one of DnDConstants,
   * actions is not a bitwise mask of DnDConstants, or dtc is null.
   * @exception NullPointerException If location is null.
   */
  public DropTargetDropEvent(DropTargetContext dtc, Point location,
                             int dropAction, int actions)
  {
    this(dtc, location, dropAction, actions, false);
  }

  /**
   * Initializes a <code>DropTargetDropEvent</code>.
   *
   * @exception IllegalArgumentException If dropAction is not one of DnDConstants,
   * actions is not a bitwise mask of DnDConstants, or dtc is null.
   * @exception NullPointerException If location is null.
   */
  public DropTargetDropEvent(DropTargetContext dtc, Point location,
                             int dropAction, int actions, boolean isLocalTx)
  {
    super(dtc);

    if (location == null)
      throw new NullPointerException();

    if (dtc == null)
      throw new IllegalArgumentException();

    if (dropAction != DnDConstants.ACTION_NONE
        && dropAction != DnDConstants.ACTION_COPY
        && dropAction != DnDConstants.ACTION_MOVE
        && dropAction != DnDConstants.ACTION_COPY_OR_MOVE
        && dropAction != DnDConstants.ACTION_LINK
        && dropAction != DnDConstants.ACTION_REFERENCE)
      throw new IllegalArgumentException();

    int actionsMask = DnDConstants.ACTION_NONE
                      | DnDConstants.ACTION_COPY
                      | DnDConstants.ACTION_MOVE
                      | DnDConstants.ACTION_COPY_OR_MOVE
                      | DnDConstants.ACTION_LINK
                      | DnDConstants.ACTION_REFERENCE;
    
    if (~(actions ^ actionsMask) != 0)
      throw new IllegalArgumentException();
    
    this.dropAction = dropAction;
    this.actions = actions;
    this.location = location;
    this.isLocalTx = isLocalTx;
  }
  
  public Point getLocation()
  {
    return location;
  }

  public DataFlavor[] getCurrentDataFlavors()
  {
    return context.getCurrentDataFlavors();
  }

  public List<DataFlavor> getCurrentDataFlavorsAsList()
  {
    return context.getCurrentDataFlavorsAsList();
  }

  public boolean isDataFlavorSupported(DataFlavor flavor)
  {
    return context.isDataFlavorSupported(flavor);
  }

  public int getSourceActions()
  {
    return actions;
  }

  public int getDropAction()
  {
    return dropAction;
  }

  public Transferable getTransferable()
  {
    return context.getTransferable ();
  }

  public void acceptDrop(int dropAction)
  {
    context.acceptDrop(dropAction);
  }

  public void rejectDrop()
  {
    context.rejectDrop();
  }

  public void dropComplete(boolean success)
  {
    context.dropComplete(success);
  }

  public boolean isLocalTransfer()
  {
    return isLocalTx;
  }
} // class DropTargetDropEvent
