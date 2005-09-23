/* AbstractListModel.java --
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

import javax.swing.event.EventListenerList;
import javax.swing.event.ListDataEvent;
import javax.swing.event.ListDataListener;

/**
 * Provides standard implementations of some methods in {@link ListModel}.
 *
 * @author Ronald Veldema
 * @author Andrew Selkirk
 */
public abstract class AbstractListModel implements ListModel, Serializable
{
  private static final long serialVersionUID = -3285184064379168730L;

  /** List of ListDataListeners called for each change to the list. */
  protected EventListenerList listenerList;

  public AbstractListModel()
  {
    listenerList = new EventListenerList();
  }

  /**
   * Add a listener object to this model. The listener will be called
   * any time the set of elements in the model is changed.
   *
   * @param listener The listener to add
   */
  public void addListDataListener(ListDataListener listener)
  {
    listenerList.add(ListDataListener.class, listener);
  }

  /**
   * Add a listener object to this model. The listener will no longer be
   * called when the set of elements in the model is changed.
   *
   * @param listener The listener to remove
   */
  public void removeListDataListener(ListDataListener listener)
  {
    listenerList.remove(ListDataListener.class, listener);
  }

  /**
   * Call {@link ListDataListener#contentsChanged} on each element of the
   * {@link #listenerList} which is a {@link ListDataListener}. The event
   * fired has type {@ListDataEvent.CONTENTS_CHANGED} and represents a
   * change to the data elements in the range [startIndex, endIndex]
   * inclusive.
   *
   * @param source The source of the change, typically <code>this</code>
   * @param startIndex The index of the first element which changed
   * @param endIndex The index of the last element which changed
   */
  protected void fireContentsChanged(Object source, int startIndex,
                                     int endIndex)
  {
    ListDataEvent event = new ListDataEvent(source, ListDataEvent.CONTENTS_CHANGED,
                                            startIndex, endIndex);
    ListDataListener[] listeners = getListDataListeners();

    for (int index = 0; index < listeners.length; index++)
      listeners[index].contentsChanged(event);
  }

  /**
   * Call {@link ListDataListener#intervalAdded} on each element of the
   * {@link #listenerList} which is a {@link ListDataListener}. The event
   * fired has type {@ListDataEvent.INTERVAL_ADDED} and represents an
   * addition of the data elements in the range [startIndex, endIndex]
   * inclusive.
   *
   * @param source The source of the change, typically <code>this</code>
   * @param startIndex The index of the first new element
   * @param endIndex The index of the last new element
   */
  protected void fireIntervalAdded(Object source, int startIndex, int endIndex)
  {
    ListDataEvent event =
      new ListDataEvent(source, ListDataEvent.INTERVAL_ADDED,
			startIndex, endIndex);
    ListDataListener[] listeners = getListDataListeners();

    for (int index = 0; index < listeners.length; index++)
      listeners[index].intervalAdded(event);
  }

  /**
   * Call {@link ListDataListener#intervalRemoved} on each element of the
   * {@link #listenerList} which is a {@link ListDataListener}. The event
   * fired has type {@ListDataEvent.INTERVAL_REMOVED} and represents a
   * removal of the data elements in the range [startIndex, endIndex]
   * inclusive.
   *
   * @param source The source of the change, typically <code>this</code>
   * @param startIndex The index of the first element removed
   * @param endIndex The index of the last element removed
   */
  protected void fireIntervalRemoved(Object source, int startIndex,
                                     int endIndex)
  {
    ListDataEvent event =
      new ListDataEvent(source, ListDataEvent.INTERVAL_REMOVED,
			startIndex, endIndex);
    ListDataListener[] listeners = getListDataListeners();

    for (int index = 0; index < listeners.length; index++)
      listeners[index].intervalRemoved(event);
  }

  /**
   * Return the subset of {@link EventListener} objects found in this
   * object's {@link #listenerList} which are elements of the specified
   * type.
   *
   * @param listenerType The type of listeners to select
   *
   * @return The set of listeners of the specified type
   */
  public EventListener[] getListeners(Class listenerType)
  {
    return listenerList.getListeners(listenerType);
  }

  /**
   * A synonym for <code>getListeners(ListDataListener.class)</code>.
   *
   * @return The set of ListDataListeners found in the {@link #listenerList}
   */
  public ListDataListener[] getListDataListeners()
  {
    return (ListDataListener[]) getListeners(ListDataListener.class);
  }
}
