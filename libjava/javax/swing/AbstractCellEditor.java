/* AbstractCellEditor.java --
   Copyright (C) 2002, 2004 Free Software Foundation, Inc.

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


package javax.swing;

import java.io.Serializable;
import java.util.EventObject;

import javax.swing.event.CellEditorListener;
import javax.swing.event.ChangeEvent;
import javax.swing.event.EventListenerList;

/**
 * AbstractCellEditor
 * @author	Andrew Selkirk
 * @version	1.0
 */
public abstract class AbstractCellEditor
  implements CellEditor, Serializable
{
  private static final long serialVersionUID = -1048006551406220959L;

  /**
   * listenerList
   */
  protected EventListenerList listenerList;

  /**
   * changeEvent
   */
  protected transient ChangeEvent changeEvent;

	/**
	 * Constructor AbstractCellEditor
	 */
	public AbstractCellEditor() {
		// TODO
	} // AbstractCellEditor()

	/**
	 * isCellEditable
	 * @param event TODO
	 * @returns boolean
	 */
	public boolean isCellEditable(EventObject event) {
		return false; // TODO
	} // isCellEditable()

	/**
	 * shouldSelectCell
	 * @param event TODO
	 * @returns boolean
	 */
	public boolean shouldSelectCell(EventObject event) {
		return false; // TODO
	} // shouldSelectCell()

	/**
	 * stopCellEditing
	 * @returns boolean
	 */
	public boolean stopCellEditing() {
		return false; // TODO
	} // stopCellEditing()

	/**
	 * cancelCellEditing
	 */
	public void cancelCellEditing() {
		// TODO
	} // cancelCellEditing()

  /**
   * addCellEditorListener
   *
   * @param listener The listener to add
   */
  public void addCellEditorListener (CellEditorListener listener)
  {
    listenerList.add (CellEditorListener.class, listener);
  }

  /**
   * removeCellEditorListener
   *
   * @param listener The listener to remove
   */
  public void removeCellEditorListener (CellEditorListener listener)
  {
    listenerList.remove (CellEditorListener.class, listener);
  }
	
  /**
   * getCellEditorListeners
   *
   * @since 1.4
   */
  public CellEditorListener[] getCellEditorListeners()
  {
    return (CellEditorListener[]) listenerList.getListeners (CellEditorListener.class);
  }

  /**
   * fireEditingStopped
   */
  protected void fireEditingStopped()
  {
    CellEditorListener[] listeners = getCellEditorListeners();

    for (int index = 0; index < listeners.length; index++)
      {
	listeners [index].editingStopped (changeEvent);
      }
  }

  /**
   * fireEditingCanceled
   */
  protected void fireEditingCanceled()
  {
    CellEditorListener[] listeners = getCellEditorListeners();

    for (int index = 0; index < listeners.length; index++)
      {
	listeners [index].editingCanceled (changeEvent);
      }
  }
}
