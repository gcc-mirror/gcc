/* DropTargetContext.java --
   Copyright (C) 2002, 2003, 2004, 2005, 2006 Free Software Foundation

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
import java.awt.datatransfer.DataFlavor;
import java.awt.datatransfer.Transferable;
import java.awt.datatransfer.UnsupportedFlavorException;
import java.awt.dnd.peer.DropTargetContextPeer;
import java.io.IOException;
import java.io.Serializable;
import java.util.Arrays;
import java.util.List;

/**
 * @author Michael Koch (konqueror@gmx.de)
 * @author Andrew John Hughes (gnu_andrew@member.fsf.org)
 * @since 1.2
 */
public class DropTargetContext implements Serializable
{
  static final long serialVersionUID = -634158968993743371L;

  protected class TransferableProxy implements Transferable
  {
    protected boolean isLocal;
    protected Transferable transferable;

    TransferableProxy(Transferable t, boolean local)
    {
      this.transferable = t;
      this.isLocal = local;
    }

    public DataFlavor[] getTransferDataFlavors()
    {
      return transferable.getTransferDataFlavors();
    }

    public boolean isDataFlavorSupported(DataFlavor flavor)
    {
      return transferable.isDataFlavorSupported(flavor);
    }

    public Object getTransferData(DataFlavor flavor)
      throws UnsupportedFlavorException, IOException
    {
      return transferable.getTransferData (flavor);
    }
  }

  private DropTarget dropTarget;
  private int targetActions;
  private DropTargetContextPeer dtcp;

  // package private
  DropTargetContext(DropTarget dropTarget)
  {
    this.dropTarget = dropTarget;
  }

  public DropTarget getDropTarget()
  {
    return dropTarget;
  }

  public Component getComponent()
  {
    return dropTarget.getComponent();
  }

  public void addNotify(DropTargetContextPeer dtcp)
  {
    this.dtcp = dtcp;
  }

  public void removeNotify()
  {
    this.dtcp = null;
  }

  protected void setTargetActions(int actions)
  {
    targetActions = actions;
  }

  protected int getTargetActions()
  {
    return targetActions;
  }

  /**
   * Signals that the drop is completed.
   *
   * @exception InvalidDnDOperationException If a drop is not outstanding.
   */
  public void dropComplete (boolean success)
  {
    if (dtcp != null)
      dtcp.dropComplete(success);
  }

  protected void acceptDrag (int dragOperation)
  {
    if (dtcp != null)
      dtcp.acceptDrag(dragOperation);
  }

  protected void rejectDrag ()
  {
    if (dtcp != null)
      dtcp.rejectDrag();
  }

  protected void acceptDrop (int dropOperation)
  {
    if (dtcp != null)
      dtcp.acceptDrop(dropOperation);
  }

  protected void rejectDrop ()
  {
    if (dtcp != null)
      dtcp.rejectDrop();
  }

  protected DataFlavor[] getCurrentDataFlavors ()
  {
    if (dtcp != null)
      dtcp.getTransferDataFlavors();
    return null;
  }

  protected List<DataFlavor> getCurrentDataFlavorsAsList ()
  {
    return Arrays.asList(getCurrentDataFlavors ());
  }

  protected boolean isDataFlavorSupported (DataFlavor flavor)
  {
    return getCurrentDataFlavorsAsList().contains (flavor);
  }

  /**
   * Return the <code>Transferable</code> operandof this operation.
   *
   * @exception InvalidDnDOperationException If a drag is not outstanding.
   */
  protected Transferable getTransferable()
    throws InvalidDnDOperationException
  {
    // FIXME: Implement this
    if (dtcp != null)
      return dtcp.getTransferable();
    return null;
  }

  protected Transferable createTransferableProxy(Transferable t, boolean local)
  {
    return new TransferableProxy(t, local);
  }
} // class DropTargetContext
