/* DropTargetDropEvent.java --
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

import java.awt.Point;
import java.awt.datatransfer.DataFlavor;
import java.awt.datatransfer.Transferable;
import java.util.List;

public class DropTargetDropEvent extends DropTargetEvent
{
  private final int dropAction;
  private final int srcActions;
  private final Point location;
  private final boolean isLocal;
  
  public DropTargetDropEvent (DropTargetContext dtc, Point location,
                              int dropAction, int srcActions)
  {
    super (dtc);
    this.dropAction = dropAction;
    this.srcActions = srcActions;
    this.location = location;
    this.isLocal = false;
  }

  public DropTargetDropEvent (DropTargetContext dtc, Point location,
                              int dropAction, int srcActions, boolean isLocal)
  {
    super (dtc);
    this.dropAction = dropAction;
    this.srcActions = srcActions;
    this.location = location;
    this.isLocal = isLocal;
  }
  
  public Point getLocation ()
  {
    return location;
  }

  public DataFlavor[] getCurrentDataFlavors ()
  {
    // FIXME: implement this
    return null;
  }

  public List getCurrentDataFlavorsAsList ()
  {
    // FIXME: implement this
    return null;
  }

  public boolean isDataFlavorSupported (DataFlavor flavor)
  {
    // FIXME: implement this
    return false;
  }

  public int getSourceActions ()
  {
    // FIXME: implement this
    return 0;
  }

  public int getDropAction ()
  {
    // FIXME: implement this
    return 0;
  }

  public Transferable getTransferable ()
  {
    // FIXME: implement this
    return null;
  }

  public void acceptDrop (int dropAction)
  {
    // FIXME: implement this
  }

  public void rejectDrop ()
  {
    // FIXME: implement this
  }

  public void dropComplete (boolean success)
  {
    // FIXME: implement this
  }

  public boolean isLocalTransfer()
  {
    return isLocal;
  }
} // class DropTargetDropEvent
