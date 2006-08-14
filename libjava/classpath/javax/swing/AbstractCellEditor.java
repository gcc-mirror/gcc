/* AbstractCellEditor.java --
   Copyright (C) 2002, 2004, 2005, 2006, Free Software Foundation, Inc.

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


package javax.swing;

import java.io.Serializable;
import java.util.EventObject;

import javax.swing.event.CellEditorListener;
import javax.swing.event.ChangeEvent;
import javax.swing.event.EventListenerList;

/**
 * An abstract superclass for table and tree cell editors. This provides some
 * common shared functionality.
 *
 * @author Andrew Selkirk
 */
public abstract class AbstractCellEditor
  implements CellEditor, Serializable
{
  private static final long serialVersionUID = -1048006551406220959L;

  /**
   * Our Swing event listeners.
   */
  protected EventListenerList listenerList;

  /**
   * The cached ChangeEvent.
   */
  protected transient ChangeEvent changeEvent;

  /**
   * Creates a new instance of AbstractCellEditor.
   */
  public AbstractCellEditor() 
  {
    listenerList = new EventListenerList();
    changeEvent = new ChangeEvent(this);
  }

  /**
   * Returns <code>true</code> if the cell is editable using
   * <code>event</code>, <code>false</code>
   * if it's not. The default behaviour is to return <code>true</code>.
   *
   * @param event an event
   *
   * @return <code>true</code> if the cell is editable using
   *     <code>event</code>, <code>false</code> if it's not
   */
  public boolean isCellEditable(EventObject event) 
  {
    return true;
  } 

  /**
   * Returns <code>true</code> if the editing cell should be selected,
   * <code>false</code> otherwise. This is usually returning <code>true</code>,
   * but in some special cases it might be useful not to switch cell selection
   * when editing one cell.
   *
   * @param event an event
   *
   * @return <code>true</code> if the editing cell should be selected,
   *     <code>false</code> otherwise
   */
  public boolean shouldSelectCell(EventObject event) 
  {
    return true;
  }

  /**
   * Stop editing the cell and accept any partial value that has been entered
   * into the cell.
   *
   * @return <code>true</code> if editing has been stopped successfully,
   *     <code>false</code>otherwise
   */
  public boolean stopCellEditing() 
  {
    fireEditingStopped();
    return true;
  }

  /**
   * Stop editing the cell and do not accept any partial value that has
   * been entered into the cell.
   */
  public void cancelCellEditing() 
  {
    fireEditingCanceled();
  } 

  /**
   * Adds a CellEditorListener to the list of CellEditorListeners of this
   * CellEditor.
   *
   * @param listener the CellEditorListener to add
   */
  public void addCellEditorListener(CellEditorListener listener)
  {
    listenerList.add(CellEditorListener.class, listener);
  }

  /**
   * Removes the specified CellEditorListener from the list of the
   * CellEditorListeners of this CellEditor.
   *
   * @param listener the CellEditorListener to remove
   */
  public void removeCellEditorListener(CellEditorListener listener)
  {
    listenerList.remove(CellEditorListener.class, listener);
  }
	
  /**
   * Returns the list of CellEditorListeners that have been registered
   * in this CellEditor.
   *
   * @return the list of CellEditorListeners that have been registered
   *     in this CellEditor
   *
   * @since 1.4
   */
  public CellEditorListener[] getCellEditorListeners()
  {
    return (CellEditorListener[]) listenerList.getListeners(
        CellEditorListener.class);
  }

  /**
   * Notifies all registered listeners that the editing of the cell has has been
   * stopped.
   */
  protected void fireEditingStopped()
  {
    CellEditorListener[] listeners = getCellEditorListeners();

    for (int index = 0; index < listeners.length; index++)
      {
        listeners[index].editingStopped(changeEvent);
      }
  }

  /**
   * Notifies all registered listeners that the editing of the cell has
   * has been canceled.
   */
  protected void fireEditingCanceled()
  {
    CellEditorListener[] listeners = getCellEditorListeners();

    for (int index = 0; index < listeners.length; index++)
      {
        listeners[index].editingCanceled(changeEvent);
      }
  }
}
