/* GtkDropTargetContextPeer.java --
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

import java.awt.datatransfer.DataFlavor;
import java.awt.datatransfer.Transferable;
import java.awt.dnd.DropTarget;
import java.awt.dnd.InvalidDnDOperationException;
import java.awt.dnd.peer.DropTargetContextPeer;

public class GtkDropTargetContextPeer
    extends GtkGenericPeer
    implements DropTargetContextPeer
{
  
  public GtkDropTargetContextPeer(Object obj)
  {
    super(obj);
  }
  
  public void setTargetActions(int actions)
  {
    // FIXME: Not Implemented

  }

  public int getTargetActions()
  {
    // FIXME: Not Implemented
    return 0;
  }

  public DropTarget getDropTarget()
  {
    // FIXME: Not Implemented
    return null;
  }

  public DataFlavor[] getTransferDataFlavors()
  {
    // FIXME: Not Implemented
    return null;
  }

  public Transferable getTransferable() throws InvalidDnDOperationException
  {
    // FIXME: Not Implemented
    return null;
  }

  public boolean isTransferableJVMLocal()
  {
    // FIXME: Not Implemented
    return false;
  }

  public void acceptDrag(int dragAction)
  {
    // FIXME: Not Implemented

  }

  public void rejectDrag()
  {
    // FIXME: Not Implemented

  }

  public void acceptDrop(int dropAction)
  {
    // FIXME: Not Implemented

  }

  public void rejectDrop()
  {
    // FIXME: Not Implemented

  }

  public void dropComplete(boolean success)
  {
    // FIXME: Not Implemented

  }

}
