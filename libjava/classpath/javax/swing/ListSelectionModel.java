/* ListSelectionModel.java -- 
   Copyright (C) 2002, 2006, Free Software Foundation, Inc.

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

import javax.swing.event.ListSelectionEvent;
import javax.swing.event.ListSelectionListener;

/**
 * A model that tracks the selection status of a list of items.  Each item in 
 * the list is identified by a zero-based index only, so the model can be used
 * to track the selection status of any type of list.  The model 
 * supports three modes:
 * <ul>
 * <li><code>SINGLE_SELECTION</code> - only one item in the list may be 
 *     selected;</li>
 * <li><code>SINGLE_INTERVAL_SELECTION</code> - only one interval in the list 
 *     may be selected;</li>
 * <li><code>MULTIPLE_INTERVAL_SELECTION</code> - any combination of items in 
 *     the list may be selected.</li>
 * </ul>
 * The model uses an event notification mechanism to notify listeners (see 
 * {@link ListSelectionListener}) about updates to the selection model.
 * <p>
 * This model is used to track row selections in the {@link JList} component,
 * and row and column selections in the {@link JTable} component.
 */
public interface ListSelectionModel
{
  
  /**
   * A selection mode in which only one item can be selected.
   * 
   * @see #setSelectionMode(int)
   */
  int SINGLE_SELECTION = 0;

  /**
   * A selection mode in which a single interval can be selected (an interval
   * is a range containing one or more contiguous items).
   * 
   * @see #setSelectionMode(int)
   */
  int SINGLE_INTERVAL_SELECTION = 1;

  /**
   * A selection mode in which any combination of items can be selected.
   * 
   * @see #setSelectionMode(int)
   */
  int MULTIPLE_INTERVAL_SELECTION = 2;

  /**
   * Sets the selection mode.
   * <p>
   * FIXME: The spec is silent about what happens to existing selections, for
   * example when changing from an interval selection to single selection.
   * 
   * @param mode  one of {@link #SINGLE_SELECTION}, 
   *     {@link #SINGLE_INTERVAL_SELECTION} and 
   *     {@link #MULTIPLE_INTERVAL_SELECTION}.
   *     
   * @see #getSelectionMode()
   * 
   * @throws IllegalArgumentException if <code>mode</code> is not one of the
   *     specified values.
   */
  void setSelectionMode(int mode);

  /**
   * Returns the selection mode, which is one of {@link #SINGLE_SELECTION}, 
   * {@link #SINGLE_INTERVAL_SELECTION} and 
   * {@link #MULTIPLE_INTERVAL_SELECTION}.
   * 
   * @return The selection mode.
   * 
   * @see #setSelectionMode(int)
   */
  int getSelectionMode();

  /**
   * Clears the current selection from the model.  If the selection state 
   * changes (that is, the existing selection is non-empty) a 
   * {@link ListSelectionEvent} should be sent to all registered listeners.
   * <p>
   * FIXME: what happens to the anchor and lead selection indices (the spec
   * is silent about this)?  See:
   * <p>
   * http://bugs.sun.com/bugdatabase/view_bug.do?bug_id=4334792
   */
  void clearSelection();

  /**
   * Returns the lowest selected index, or <code>-1</code> if there is no 
   * selection.
   * 
   * @return The lowest selected index.
   * 
   * @see #getMaxSelectionIndex()
   */
  int getMinSelectionIndex();

  /**
   * Returns the highest selected index, or <code>-1</code> if there is no
   * selection.
   * 
   * @return The highest selected index.
   * 
   * @see #getMinSelectionIndex()
   */
  int getMaxSelectionIndex();

  /**
   * Returns <code>true</code> if the specified item is selected, and 
   * <code>false</code> otherwise.  Special note: if <code>index</code> is 
   * negative, this method should return <code>false</code> (no exception 
   * should be thrown).
   * 
   * @param index  the item index (zero-based).
   * 
   * @return <code>true</code> if the specified item is selected, and 
   *     <code>false</code> otherwise.
   */
  boolean isSelectedIndex(int index);

  /**
   * Returns <code>true</code> if there is no selection, and <code>false</code>
   * otherwise.
   * 
   * @return  <code>true</code> if there is no selection, and 
   *     <code>false</code> otherwise.
   */
  boolean isSelectionEmpty();

  /**
   * Sets the selection interval to the specified range (note that 
   * <code>anchor</code> can be less than, equal to, or greater than 
   * <code>lead</code>).  If this results in the selection being changed, 
   * a {@link ListSelectionEvent} is sent to all registered listeners.
   * <p>
   * If the selection mode is {@link #SINGLE_SELECTION}, only the 
   * <code>lead</code> item is selected.
   * 
   * @param anchor  the anchor index.
   * @param lead  the lead index.
   */
  void setSelectionInterval(int anchor, int lead);

  /**
   * Marks the items in the specified interval as selected.  The behaviour of 
   * this method depends on the selection mode:
   * <ul>
   * <li><code>SINGLE_SELECTION</code> - only the <code>lead</code> item is 
   *     selected;</li>
   * <li><code>SINGLE_INTERVAL_SELECTION</code> - the existing selection 
   *     interval is replaced by the specified interval;</li>
   * <li><code>MULTIPLE_INTERVAL_SELECTION</code> - the specified interval is 
   *     merged into the currently selected intervals.</li>
   * </ul>
   * Note that <code>anchor</code> can be less than, equal to, or greater than 
   * <code>lead</code>.
   * 
   * @param anchor  the index of the anchor item
   * @param lead  the index of the lead item.
   */
  void addSelectionInterval(int anchor, int lead);

  /**
   * Marks the items in the specified interval as not selected.  The behaviour 
   * of this method depends on the selection mode:
   * <ul>
   * <li><code>SINGLE_SELECTION</code> - XXX;</li>
   * <li><code>SINGLE_INTERVAL_SELECTION</code> - XXX;</li>
   * <li><code>MULTIPLE_INTERVAL_SELECTION</code> - XXX.</li>
   * </ul>
   * Note that <code>anchor</code> can be less than, equal to, or greater than 
   * <code>lead</code>.
   * 
   * @param anchor  the index of the anchor item
   * @param lead  the index of the lead item.
   */
  void removeSelectionInterval(int anchor, int lead);

  /**
   * Inserts a new interval containing <code>length</code> items at the
   * specified <code>index</code> (the <code>before</code> flag indicates
   * whether the range is inserted before or after the existing item at
   * <code>index</code>).
   * 
   * FIXME: What is the selection status of the new items? Bug 4870694.
   * FIXME: What event is generated?
   * 
   * @param index  the index of the item. 
   * @param length  the number of items in the interval to be inserted.
   * @param before  if <code>true</code>, the interval should be inserted 
   *     before <code>index</code>, otherwise it is inserted after.
   *     
   * @see #removeIndexInterval(int, int)
   */
  void insertIndexInterval(int index, int length, boolean before);

  /**
   * Removes the items in the specified range (inclusive) from the selection
   * model.  This method should be called when an interval is deleted from 
   * the underlying list.
   * 
   * FIXME: what happens to the lead and anchor indices if they are part of 
   * the range that is removed? 
   * FIXME: what event is generated
   * 
   * @param index0  XXX
   * @param index1  XXX
   * 
   * @see #insertIndexInterval(int, int, boolean)
   */
  void removeIndexInterval(int index0, int index1);

  /**
   * Returns the index of the anchor item. 
   * 
   * @return The index of the anchor item.
   * 
   * @see #setAnchorSelectionIndex(int)
   */
  int getAnchorSelectionIndex();

  /**
   * Sets the index of the anchor item.
   * 
   * @param index  the item index.
   * 
   * @see #getAnchorSelectionIndex()
   */
  void setAnchorSelectionIndex(int index);

  /**
   * Returns the index of the lead item.
   * 
   * @return The index of the lead item.
   * 
   * @see #setLeadSelectionIndex(int)
   */
  int getLeadSelectionIndex();

  /**
   * Sets the index of the lead item.
   * 
   * @param index  the item index.
   * 
   * @see #getLeadSelectionIndex()
   */
  void setLeadSelectionIndex(int index);

  /**
   * Sets the flag that is passed to listeners for each change notification.
   * If a sequence of changes is made to the selection model, this flag should
   * be set to <code>true</code> at the start of the sequence, and 
   * <code>false</code> for the last change - this gives listeners the option
   * to ignore interim changes if that is more efficient.
   * 
   * @param valueIsAdjusting  the flag value.
   * 
   * @see #getValueIsAdjusting()
   */
  void setValueIsAdjusting(boolean valueIsAdjusting);

  /**
   * Returns a flag that is passed to registered listeners when changes are
   * made to the model.  See the description for 
   * {@link #setValueIsAdjusting(boolean)} for more information.
   *  
   * @return The flag.
   */
  boolean getValueIsAdjusting();

  /**
   * Registers a listener with the model so that it receives notification
   * of changes to the model.
   * 
   * @param listener  the listener (<code>null</code> ignored).
   * 
   * @see #removeListSelectionListener(ListSelectionListener)
   */
  void addListSelectionListener(ListSelectionListener listener);

  /**
   * Deregisters a listener so that it no longer receives notification of
   * changes to the model.  If the specified listener is not registered with
   * the model, or is <code>null</code>, this method does nothing.
   * 
   * @param listener  the listener (<code>null</code> ignored).
   * 
   * @see #addListSelectionListener(ListSelectionListener)
   */
  void removeListSelectionListener(ListSelectionListener listener);

}
