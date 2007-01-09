/* DefaultSingleSelectionModel.java --
   Copyright (C) 2002, 2004, 2006,  Free Software Foundation, Inc.

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
import java.util.EventListener;

import javax.swing.event.ChangeEvent;
import javax.swing.event.ChangeListener;
import javax.swing.event.EventListenerList;

/**
 * The default implementation of {@link SingleSelectionModel}, used in
 * {@link JTabbedPane}, {@link JMenuBar} and {@link JPopupMenu}.
 *
 * @author Andrew Selkirk
 */
public class DefaultSingleSelectionModel
  implements SingleSelectionModel, Serializable
{
  private static final long serialVersionUID = 3676229404753786004L;

  /**
   * changeEvent
   */
  protected transient ChangeEvent changeEvent;

  /**
   * listenerList
   */
  protected EventListenerList listenerList = new EventListenerList();

  /**
   * The selected index (or -1 for no selection).
   */
  private int index = -1;

  /**
   * Creates a new <code>DefaultSingleSelectionModel</code> with no current
   * selection.
   */
  public DefaultSingleSelectionModel()
  {
    // Do nothing.
  }

  /**
   * Returns the selected index or <code>-1</code> if there is no selection.
   * 
   * @return The selected index.
   * 
   * @see #setSelectedIndex(int)
   */
  public int getSelectedIndex()
  {
    return index;
  }

  /**
   * Sets the selected index and, if this is different to the previous 
   * selection, sends a {@link ChangeEvent} to all registered listeners.
   * 
   * @param index  the index (use <code>-1</code> to represent no selection).
   * 
   * @see #getSelectedIndex()
   * @see #clearSelection
   */
  public void setSelectedIndex(int index)
  {
    if (this.index != index)
      {
        this.index = index;
        fireStateChanged();
      }
  }

  /**
   * Clears the selection by setting the selected index to <code>-1</code> and
   * sends a {@link ChangeEvent} to all registered listeners.  If the selected
   * index is already <code>-1</code>, this method does nothing.  
   */
  public void clearSelection()
  {
    setSelectedIndex(-1);
  }

  /**
   * Returns <code>true</code> if there is a selection, and <code>false</code>
   * otherwise.  
   * 
   * @return A boolean.
   */
  public boolean isSelected()
  {
    return index != -1;
  }

  /**
   * Registers a listener to receive {@link ChangeEvent} notifications from
   * this model whenever the selected index changes.
   *
   * @param listener the listener to add.
   */
  public void addChangeListener(ChangeListener listener)
  {
    listenerList.add(ChangeListener.class, listener);
  }

  /**
   * Deregisters a listener so that it no longer receives {@link ChangeEvent}
   * notifications from this model.
   *
   * @param listener the listener to remove.
   */
  public void removeChangeListener(ChangeListener listener)
  {
    listenerList.remove(ChangeListener.class, listener);
  }

  /**
   * fireStateChanged
   */
  protected void fireStateChanged()
  {
    if (changeEvent == null)
      changeEvent = new ChangeEvent(this);
    ChangeListener[] listeners = getChangeListeners();
    for (int i = 0; i < listeners.length; i++)
      listeners[i].stateChanged(changeEvent);
  }

  /**
   * getListeners
   *
   * @param listenerClass the type fo listener
   *
   * @return an array of listeners
   *
   * @since 1.3
   */
  public <T extends EventListener> T[] getListeners(Class<T> listenerClass)
  {
    return listenerList.getListeners(listenerClass);
  }

  /**
   * getChangeListeners
   *
   * @since 1.4
   */
  public ChangeListener[] getChangeListeners()
  {
    return (ChangeListener[]) getListeners(ChangeListener.class);
  }
}
