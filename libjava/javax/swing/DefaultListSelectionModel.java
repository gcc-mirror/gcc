/* DefaultListSelectionModel.java --
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
import java.util.BitSet;
import java.util.EventListener;

import javax.swing.event.EventListenerList;
import javax.swing.event.ListSelectionEvent;
import javax.swing.event.ListSelectionListener;

/**
 * <p>This class provides a default implementation of {@link
 * ListSelectioModel}, which is used by {@link javax.swing.JList} and
 * similar classes to manage the selection status of a number of data
 * elements. </p>
 *
 * <p>The class is organized <em>abstractly</em> as a set of intervals of
 * integers. Each interval indicates an inclusive range of indices in a
 * list -- held by some other object and unknown to this class -- which is
 * considered "selected". There are various accessors for querying and
 * modifying the set of intervals, with simplified forms accepting a single
 * index, representing an interval with only one element. </p>
 */
public class DefaultListSelectionModel implements Cloneable,
                                                  ListSelectionModel,
                                                  Serializable
{
  private static final long serialVersionUID = -5718799865110415860L;

  /** The list of ListSelectionListeners subscribed to this selection model. */
  protected EventListenerList listenerList = new EventListenerList();


  /** 
   * The current list selection mode. Must be one of the numeric constants
   * <code>SINGLE_SELECTION</code>, <code>SINGLE_INTERVAL_SELECTION</code>
   * or <code>MULTIPLE_INTERVAL_SELECTION</code> from {@link
   * ListSelectionModel}. The default value is
   * <code>MULTIPLE_INTERVAL_SELECTION</code>.
   */
  int selectionMode = MULTIPLE_INTERVAL_SELECTION;


  /**
   * The index of the "lead" of the most recent selection. The lead is the
   * second argument in any call to {@link #setSelectionInterval}, {@link
   * #addSelectionInterval} or {@link #removeSelectionInterval}. Generally
   * the lead refers to the most recent position a user dragged their mouse
   * over.
   */
  int leadSelectionIndex = -1;


  /**
   * The index of the "anchor" of the most recent selection. The anchor is
   * the first argument in any call to {@link #setSelectionInterval},
   * {@link #addSelectionInterval} or {@link
   * #removeSelectionInterval}. Generally the anchor refers to the first
   * recent position a user clicks when they begin to drag their mouse over
   * a list.
   *
   * @see #getAnchorSelectionIndex
   * @see #setAnchorSelectionIndex
   */
  int anchorSelectionIndex = -1;


  /**
   * controls the range of indices provided in any {@link
   * ListSelectionEvent} fired by the selectionModel. Let
   * <code>[A,L]</code> be the range of indices between {@link
   * anchorSelectionIndex} and {@link leadSelectionIndex} inclusive, and
   * let <code>[i0,i1]</code> be the range of indices changed in a given
   * call which generates a {@link ListSelectionEvent}. Then when this
   * property is <code>true</code>, the {@link ListSelectionEvent} contains
   * the range <code>[A,L] union [i0,i1]</code>; when <code>false</code> it
   * will contain only <code>[i0,i1]</code>. The default is
   * <code>true</code>.
   *
   * @see #isLeadAnchorNotificationEnabled
   * @see #setLeadAnchorNotificationEnabled
   */
  protected boolean leadAnchorNotificationEnabled = true;


  /**
   * Whether the selection is currently "adjusting". Any {@link
   * ListSelectionEvent} events constructed in response to changes in this
   * list selection model will have their {@link
   * ListSelectionEvent#isAdjusting} field set to this value.
   *
   * @see #getValueIsAdjusting
   * @see #setValueIsAdjusting
   */
  boolean valueIsAdjusting = false;


  /** 
   * The current set of "intervals", represented simply by a {@link
   * java.util.BitSet}. A set bit indicates a selected index, whereas a
   * cleared bit indicates a non-selected index.
   */
  BitSet sel = new BitSet();


  /**
   * Gets the value of the {@link #selectionMode} property.
   *
   * @return The current value of the property
   */
  public int getSelectionMode()
  {
    return selectionMode;
  }

  /**
   * Sets the value of the {@link #selectionMode} property.
   *
   * @param a The new value of the property
   */
  public void setSelectionMode(int a)
  {
    selectionMode = a;
  }

  /**
   * Gets the value of the {@link #anchorSelectionIndex} property.
   * 
   * @return The current property value
   *
   * @see #setAnchorSelectionIndex
   */
  public int getAnchorSelectionIndex()
  {
    return anchorSelectionIndex;
  }

  /**
   * Sets the value of the {@link #anchorSelectionIndex} property.
   * 
   * @param anchorIndex The new property value
   *
   * @see #getAnchorSelectionIndex
   */
  public void setAnchorSelectionIndex(int anchorIndex)
  {
    anchorSelectionIndex = anchorIndex;
  }
  
  /**
   * Gets the value of the {@link #leadSelectionIndex} property.
   * 
   * @return The current property value
   *
   * @see #setLeadSelectionIndex
   */
  public int getLeadSelectionIndex()
  {
    return leadSelectionIndex;
  }

  /**
   * <p>Sets the value of the {@link #anchorSelectionIndex} property. As a
   * side effect, alters the selection status of two ranges of indices. Let
   * <code>OL</code> be the old lead selection index, <code>NL</code> be
   * the new lead selection index, and <code>A</code> be the anchor
   * selection index. Then if <code>A</code> is a valid selection index,
   * one of two things happens depending on the seleciton status of
   * <code>A</code>:</p>
   *
   * <ul>
   *
   * <li><code>isSelectedIndex(A) == true</code>: set <code>[A,OL]</code>
   * to <em>deselected</em>, then set <code>[A,NL]</code> to
   * <em>selected</em>.</li>
   *
   * <li><code>isSelectedIndex(A) == false</code>: set <code>[A,OL]</code>
   * to <em>selected</em>, then set <code>[A,NL]</code> to
   * <em>deselected</em>.</li>
   *
   * </ul>
   *
   * <p>This method generates at most a single {@link ListSelectionEvent}
   * despite changing multiple ranges. The range of values provided to the
   * {@link ListSelectionEvent} includes only the minimum range of values
   * which changed selection status between the beginning and end of the
   * method.</p>
   * 
   * @param anchorIndex The new property value
   *
   * @see #getAnchorSelectionIndex
   */
  public void setLeadSelectionIndex(int leadIndex)
  {
    int oldLeadIndex = leadSelectionIndex;
    leadSelectionIndex = leadIndex;

    if (anchorSelectionIndex == -1)
      return;

    int R1 = Math.min(anchorSelectionIndex, oldLeadIndex);
    int R2 = Math.max(anchorSelectionIndex, oldLeadIndex);
    int S1 = Math.min(anchorSelectionIndex, leadIndex);
    int S2 = Math.max(anchorSelectionIndex, leadIndex);
    
    int lo = Math.min(R1, S1);
    int hi = Math.max(R2, S2);

    BitSet oldRange = sel.get(lo, hi+1);

    if (isSelectedIndex(anchorSelectionIndex))
      {
        sel.clear(R1, R2+1);
        sel.set(S1, S2+1);
      }
    else
      {
        sel.set(R1, R2+1);
        sel.clear(S1, S2+1);
      }
    
    BitSet newRange = sel.get(lo, hi+1);
    newRange.xor(oldRange);

    int beg = sel.nextSetBit(0), end = -1;
    for(int i=beg; i >= 0; i=sel.nextSetBit(i+1)) 
      { 
        end = i;
      }
    fireValueChanged(beg, end, valueIsAdjusting);    
  }

  /**
   * Gets the value of the {@link #leadAnchorNotificationEnabled} property.
   * 
   * @return The current property value
   *
   * @see #setLeadAnchorNotificationEnabled
   */
  public boolean isLeadAnchorNotificationEnabled()
  {
    return leadAnchorNotificationEnabled;
  }

  /**
   * Sets the value of the {@link #leadAnchorNotificationEnabled} property.
   * 
   * @param flag The new property value
   *
   * @see #getLeadAnchorNotificationEnabled
   */
  public void setLeadAnchorNotificationEnabled(boolean l)
  {
    leadAnchorNotificationEnabled = l;
  }


  /**
   * Gets the value of the {@link #valueIsAdjusting} property.
   *
   * @return The current property value
   *
   * @see #setValueIsAdjusting
   */
  public boolean getValueIsAdjusting()
  {
    return valueIsAdjusting;
  }

  /**
   * Sets the value of the {@link #valueIsAdjusting} property.
   *
   * @param v The new property value
   *
   * @see #getValueIsAdjusting
   */
  public void setValueIsAdjusting(boolean v)
  {
    valueIsAdjusting = v;
  }

  /**
   * Determines whether the selection is empty.
   *
   * @return <code>true</code> if the selection is empty, otherwise
   * <code>false</code>
   */
  public boolean isSelectionEmpty()
  {
    return sel.isEmpty();
  }


  /**
   * Gets the smallest index which is currently a member of a selection
   * interval.
   *
   * @return The least integer <code>i</code> such that <code>i >=
   * 0</code> and <code>i</code> is a member of a selected interval, or
   * <code>-1</code> if there are no selected intervals
   *
   * @see #getMaxSelectionIndex
   */
  public int getMinSelectionIndex()
  {
    if (isSelectionEmpty())
      return -1;
    
    return sel.nextSetBit(0);
  }

  /**
   * Gets the largest index which is currently a member of a selection
   * interval.
   *
   * @return The greatest integer <code>i</code> such that <code>i >=
   * 0</code> and <code>i</code> is a member of a selected interval, or
   * <code>-1</code> if there are no selected intervals
   *
   * @see #getMinSelectionIndex
   */
  public int getMaxSelectionIndex()
  {
    if (isSelectionEmpty())
      return -1;

    int mx = -1;
    for(int i=sel.nextSetBit(0); i >= 0; i=sel.nextSetBit(i+1)) 
      { 
        mx = i;
      }
    return mx;
  }

  /**
   * Determines whether a particular index is a member of a selection
   * interval.
   *
   * @param a The index to search for
   *
   * @return <code>true</code> if the index is a member of a selection interval,
   * otherwise <code>false</code>
   */
  public boolean isSelectedIndex(int a)
  {
    return sel.get(a);
  }

  /**
   * If the {@link #selectionMode} property is equal to
   * <code>SINGLE_SELECTION</code> or
   * <code>SINGLE_INTERVAL_SELECTION</code>, equivalent to calling
   * <code>setSelectionInterval(index1, index2)</code>; otherwise adds the
   * range <code>[index0, index1]</code> to the selection interval set.
   *
   * @param index0 The beginning of the range of indices to select
   * @param index1 The end of the range of indices to select
   *
   * @see #setSelectionInterval
   * @see #removeSelectionInterval
   */
  public void addSelectionInterval(int index0, int index1) 
  {
    if (selectionMode == SINGLE_SELECTION
        || selectionMode == SINGLE_INTERVAL_SELECTION)
      sel.clear();
    
    if (selectionMode == SINGLE_SELECTION)
      index0 = index1;

    int lo = Math.min(index0, index1);
    int hi = Math.max(index0, index1);

    sel.set(lo, hi+1);
    fireValueChanged(lo, hi, valueIsAdjusting);
  }


  /**
   * Deselects all indices in the inclusive range
   * <code>[index0,index1]</code>.
   *
   * @param index0 The beginning of the range of indices to deselect
   * @param index1 The end of the range of indices to deselect
   *
   * @see #addSelectionInterval
   * @see #setSelectionInterval
   */
  public void removeSelectionInterval(int index0,
                                      int index1)
  {
    int lo = Math.min(index0, index1);
    int hi = Math.max(index0, index1);
    sel.clear(lo, hi+1); 
    fireValueChanged(lo, hi, valueIsAdjusting);
  }

  /**
   * Removes all intervals in the selection set.
   */
  public void clearSelection()
  {
    int sz = sel.size();
    sel.clear();
    fireValueChanged(0, sz, valueIsAdjusting);
  }
  
  /**
   * Clears the current selection and marks a given interval as
   * "selected". If the current selection mode is
   * <code>SINGLE_SELECTION</code> only the index <code>index2</code> is
   * selected.
   *
   * @param index0 The low end of the new selection 
   * @param index1 The high end of the new selection
   */
  public void setSelectionInterval(int index0, int index1)
  {
    sel.clear();
    if (selectionMode == SINGLE_SELECTION)
      index0 = index1;

    int lo = Math.min(index0, index1);
    int hi = Math.max(index0, index1);
    sel.set(lo, hi+1);
    fireValueChanged(lo, hi, valueIsAdjusting);
  }

  /**
   * Inserts a number of indices either before or after a particular
   * position in the set of indices. Renumbers all indices after the
   * inserted range. The new indices in the inserted range are not
   * selected. This method is typically called to synchronize the selection
   * model with an inserted range of elements in a {@link ListModel}.
   *
   * @param index The position to insert indices at
   * @param length The number of indices to insert
   * @param before Indicates whether to insert the indices before the index
   * or after it
   */
  public void insertIndexInterval(int index,
                                  int length,
                                  boolean before)
  {
    if (!before)
      {        
        index++;
        length--;
      }
    BitSet tmp = sel.get(index, sel.size());
    sel.clear(index, sel.size());
    int n = tmp.size();
    for (int i = 0; i < n; ++i)
      sel.set(index + length + i, tmp.get(i));
  }

  /**
   * Removes a range from the set of indices. Renumbers all indices after
   * the removed range. This method is typically called to synchronize the
   * selection model with a deleted range of elements in a {@link
   * ListModel}.
   *
   * @param index0 The first index to remove (inclusive)
   * @param index1 The last index to remove (inclusive)
   */
  public void removeIndexInterval(int index0,
                                  int index1)
  {
    int lo = Math.min(index0, index1);
    int hi = Math.max(index0, index1);

    BitSet tmp = sel.get(hi, sel.size());
    sel.clear(lo, sel.size());
    int n = tmp.size();
    for (int i = 0; i < n; ++i)
      sel.set(lo + i, tmp.get(i));
  }

  /**
   * Fires a {@link ListSelectionEvent} to all the listeners of type {@link
   * ListSelectionListener} registered with this selection model.
   *
   * @param firstIndex The low index of the changed range
   * @param lastIndex The high index of the changed range
   */
  protected void fireValueChanged(int firstIndex, int lastIndex)
  {
    fireValueChanged(firstIndex, lastIndex, getValueIsAdjusting());
  }
  
  /**
   * Fires a {@link ListSelectionEvent} to all the listeners of type {@link
   * ListSelectionListener} registered with this selection model.
   *
   * @param firstIndex The low index of the changed range
   * @param lastIndex The high index of the changed range
   * @param isAdjusting Whether this change is part of a seqence of adjustments
   * made to the selection, such as during interactive scrolling
   */
  protected void fireValueChanged(int firstIndex, int lastIndex,
				  boolean isAdjusting)
  {
    ListSelectionEvent evt = new ListSelectionEvent(this, firstIndex,
                                                    lastIndex, isAdjusting);
    ListSelectionListener[] listeners = getListSelectionListeners();
    for (int i = 0; i < listeners.length; ++i)
      listeners[i].valueChanged(evt);
  }

  /**
   * Adds a listener.
   *
   * @param listener The listener to add
   *
   * @see removeListSelectionListener
   * @see getListSelectionListeners
   */
  public void addListSelectionListener(ListSelectionListener listener)
  {
    listenerList.add(ListSelectionListener.class, listener);
  }

  /**
   * Removes a registered listener.
   *
   * @param listener The listener to remove
   *
   * @see addListSelectionListener
   * @see getListSelectionListeners
   */
  public void removeListSelectionListener(ListSelectionListener listener)
  {
    listenerList.remove(ListSelectionListener.class, listener);
  }

  /**
   * Returns an array of all registerers listeners.
   *
   * @param listenerType The type of listener to retrieve
   *
   * @return The array
   *
   * @see getListSelectionListener
   * @since 1.3
   */
  public EventListener[] getListeners(Class listenerType)
  {
    return listenerList.getListeners(listenerType);
  }

  /**
   * Returns an array of all registerd list selection listeners.
   *
   * @return the array
   *
   * @see addListSelectionListener
   * @see removeListSelectionListener
   * @see getListeners
   * @since 1.4
   */
  public ListSelectionListener[] getListSelectionListeners()
  {
    return (ListSelectionListener[]) getListeners(ListSelectionListener.class);
  }

  /**
   * Returns a clone of this object.
   * <code>listenerList</code> don't gets duplicated.
   *
   * @return the cloned object
   *
   * @throws CloneNotSupportedException if an error occurs
   */
  public Object clone()
    throws CloneNotSupportedException
  {
    DefaultListSelectionModel model =
      (DefaultListSelectionModel) super.clone();
    model.sel = (BitSet) sel.clone();
    return model;
  }
}
