/* DefaultSingleSelectionModel.java --
   Copyright (C) 2002, 2004  Free Software Foundation, Inc.

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
import java.util.EventListener;

import javax.swing.event.ChangeEvent;
import javax.swing.event.ChangeListener;
import javax.swing.event.EventListenerList;

/**
 * DefaultSingleSelectionModel
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
  protected transient ChangeEvent changeEvent = new ChangeEvent(this);

  /**
   * listenerList
   */
  protected EventListenerList listenerList = new EventListenerList();

  /**
   * index
   */
  private int index = -1;

  /**
   * Constructor DefaultSingleSelectionModel
   */
  public DefaultSingleSelectionModel()
  {
    // Do nothing.
  }

  /**
   * getSelectedIndex
   * @return int
   */
  public int getSelectedIndex()
  {
    return index;
  }

  /**
   * setSelectedIndex
   * @param index TODO
   */
  public void setSelectedIndex(int index)
  {
    this.index = index;
    fireStateChanged();
  }

  /**
   * clearSelection
   */
  public void clearSelection()
  {
    index = -1;
    fireStateChanged();
  }

  /**
   * isSelected
   * @return boolean
   */
  public boolean isSelected()
  {
    return index != -1;
  }

  /**
   * addChangeListener
   *
   * @param listener the listener to add
   */
  public void addChangeListener(ChangeListener listener)
  {
    listenerList.add(ChangeListener.class, listener);
  }

  /**
   * removeChangeListener
   *
   * @param listener the listener to remove
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
  public EventListener[] getListeners(Class listenerClass)
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
