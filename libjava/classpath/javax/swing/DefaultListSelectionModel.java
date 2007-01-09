/* DefaultListSelectionModel.java --
   Copyright (C) 2002, 2004, 2005 Free Software Foundation, Inc.

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
import java.util.BitSet;
import java.util.EventListener;

import javax.swing.event.EventListenerList;
import javax.swing.event.ListSelectionEvent;
import javax.swing.event.ListSelectionListener;

/**
 * The default implementation of {@link ListSelectionModel},
 * which is used by {@link javax.swing.JList} and
 * similar classes to manage the selection status of a number of data
 * elements.
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
   * #anchorSelectionIndex} and {@link #leadSelectionIndex} inclusive, and
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
   * A variable to store the previous value of sel.
   * Used to make sure we only fireValueChanged when the BitSet
   * actually does change.
   */
  Object oldSel;

  /**
   * Whether this call of setLeadSelectionInterval was called locally
   * from addSelectionInterval
   */
  boolean setLeadCalledFromAdd = false;

  /**
   * Returns the selection mode, which is one of {@link #SINGLE_SELECTION}, 
   * {@link #SINGLE_INTERVAL_SELECTION} and 
   * {@link #MULTIPLE_INTERVAL_SELECTION}.  The default value is
   * {@link #MULTIPLE_INTERVAL_SELECTION}.
   * 
   * @return The selection mode.
   * 
   * @see #setSelectionMode(int)
   */
  public int getSelectionMode()
  {
    return selectionMode;
  }

  /**
   * Sets the value of the {@link #selectionMode} property.
   *
   * @param mode The new value of the property
   */
  public void setSelectionMode(int mode)
  {
    if (mode < ListSelectionModel.SINGLE_SELECTION 
        || mode > ListSelectionModel.MULTIPLE_INTERVAL_SELECTION)
      throw new IllegalArgumentException("Unrecognised mode: " + mode);
    selectionMode = mode;
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
   * @param index The new property value
   *
   * @see #getAnchorSelectionIndex
   */
  public void setAnchorSelectionIndex(int index)
  {
    if (anchorSelectionIndex != index)
      {
        int old = anchorSelectionIndex;
        anchorSelectionIndex = index;
        if (leadAnchorNotificationEnabled)
          fireValueChanged(index, old);
      }
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
   * @param leadIndex The new property value
   *
   * @see #getAnchorSelectionIndex
   */
  public void setLeadSelectionIndex(int leadIndex)
  {
    // Only set the lead selection index to < 0 if anchorSelectionIndex < 0.
    if (leadIndex < 0)
      {
        if (anchorSelectionIndex < 0)
          leadSelectionIndex = -1;
        else
          return;
      }

    // Only touch the lead selection index if the anchor is >= 0.
    if (anchorSelectionIndex < 0)
      return;

    if (selectionMode == SINGLE_SELECTION)
      setSelectionInterval (leadIndex, leadIndex);
    
    int oldLeadIndex = leadSelectionIndex;
    if (oldLeadIndex == -1)
      oldLeadIndex = leadIndex;
    if (setLeadCalledFromAdd == false)
      oldSel = sel.clone();
    leadSelectionIndex = leadIndex;

    if (anchorSelectionIndex == -1)
      return;    
    
    int R1 = Math.min(anchorSelectionIndex, oldLeadIndex);
    int R2 = Math.max(anchorSelectionIndex, oldLeadIndex);
    int S1 = Math.min(anchorSelectionIndex, leadIndex);
    int S2 = Math.max(anchorSelectionIndex, leadIndex);

    int lo = Math.min(R1, S1);
    int hi = Math.max(R2, S2);

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

    int beg = sel.nextSetBit(0), end = -1;
    for(int i=beg; i >= 0; i=sel.nextSetBit(i+1)) 
      end = i;
    
    BitSet old = (BitSet) oldSel;
    
    // The new and previous lead location requires repainting.
    old.set(oldLeadIndex, !sel.get(oldLeadIndex));
    old.set(leadSelectionIndex, !sel.get(leadSelectionIndex));
    
    fireDifference(sel, old);
  }

  /**
   * Moves the lead selection index to <code>leadIndex</code> without 
   * changing the selection values.
   * 
   * If leadAnchorNotificationEnabled is true, send a notification covering the
   * old and new lead cells.
   * 
   * @param leadIndex the new lead selection index
   * @since 1.5
   */
  public void moveLeadSelectionIndex (int leadIndex)
  {
    if (leadSelectionIndex == leadIndex)
      return;
    
    leadSelectionIndex = leadIndex;
    if (isLeadAnchorNotificationEnabled())
      fireValueChanged(Math.min(leadSelectionIndex, leadIndex),
                       Math.max(leadSelectionIndex, leadIndex));
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
   * @param l The new property value
   *
   * @see #isLeadAnchorNotificationEnabled
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
   *     0</code> and <code>i</code> is a member of a selected interval, or
   *     <code>-1</code> if there are no selected intervals
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
   *     0</code> and <code>i</code> is a member of a selected interval, or
   *     <code>-1</code> if there are no selected intervals
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
   *     otherwise <code>false</code>
   */
  public boolean isSelectedIndex(int a)
  {
    // TODO: Probably throw an exception here?
    if (a >= sel.length() || a < 0)
      return false;
    return sel.get(a);
  }

  /**
   * If the {@link #selectionMode} property is equal to
   * <code>SINGLE_SELECTION</code> equivalent to calling
   * <code>setSelectionInterval(index1, index2)</code>; 
   * If the {@link #selectionMode} property is equal to 
   * <code>SINGLE_INTERVAL_SELECTION</code> and the interval being
   * added is not adjacent to an already selected interval,
   * equivalent to <code>setSelectionInterval(index1, index2)</code>.
   * Otherwise adds the range <code>[index0, index1]</code> 
   * to the selection interval set.
   *
   * @param index0 The beginning of the range of indices to select
   * @param index1 The end of the range of indices to select
   *
   * @see #setSelectionInterval
   * @see #removeSelectionInterval
   */
  public void addSelectionInterval(int index0, int index1) 
  {
    if (index0 == -1 || index1 == -1)
      return;
    
    if (selectionMode == SINGLE_SELECTION)
      setSelectionInterval(index0, index1);
    else
    {
    int lo = Math.min(index0, index1);
    int hi = Math.max(index0, index1);
    oldSel = sel.clone();


    // COMPAT: Like Sun (but not like IBM), we allow calls to 
    // addSelectionInterval when selectionMode is
    // SINGLE_SELECTION_INTERVAL iff the interval being added
    // is adjacent to an already selected interval
    if (selectionMode == SINGLE_INTERVAL_SELECTION)
      if (!(isSelectedIndex(index0) || 
            isSelectedIndex(index1) || 
            isSelectedIndex(Math.max(lo-1,0)) || 
            isSelectedIndex(Math.min(hi+1,sel.size()))))
        sel.clear();    

    // We have to update the anchorSelectionIndex and leadSelectionIndex
    // variables
    
    // The next if statements breaks down to "if this selection is adjacent
    // to the previous selection and going in the same direction"
    if ((isSelectedIndex(leadSelectionIndex)) 
        && ((index0 - 1 == leadSelectionIndex 
             && (index1 >= index0) 
             && (leadSelectionIndex >= anchorSelectionIndex))
            || (index0 + 1 == leadSelectionIndex && (index1 <= index0) 
                && (leadSelectionIndex <= anchorSelectionIndex)))
        && (anchorSelectionIndex != -1 || leadSelectionIndex != -1))
      {
        // setting setLeadCalledFromAdd to true tells setLeadSelectionIndex
        //   not to update oldSel
        setLeadCalledFromAdd = true;
        setLeadSelectionIndex(index1);
        setLeadCalledFromAdd = false;
      }
    else
      {
        leadSelectionIndex = index1;
        anchorSelectionIndex = index0;
        sel.set(lo, hi+1);
        fireDifference(sel, (BitSet) oldSel);
      }
    }
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
    if (index0 == -1 || index1 == -1)
      return;
    
    oldSel = sel.clone();
    int lo = Math.min(index0, index1);
    int hi = Math.max(index0, index1);
    
    // if selectionMode is SINGLE_INTERVAL_SELECTION and removing the interval
    //   (index0,index1) would leave two disjoint selection intervals, remove all
    //   selected indices from lo to the last selected index
    if (getMinSelectionIndex() > 0 && getMinSelectionIndex() < lo && 
        selectionMode == SINGLE_INTERVAL_SELECTION)
      hi = sel.size() - 1;

    sel.clear(lo, hi+1); 
    //update anchorSelectionIndex and leadSelectionIndex variables
    //TODO: will probably need MouseDragged to test properly and know if this works
    setAnchorSelectionIndex(index0);
    leadSelectionIndex = index1;
    
    fireDifference(sel, (BitSet) oldSel);
  }

  /**
   * Removes all intervals in the selection set.
   */
  public void clearSelection()
  {
    // Find the selected interval.
    int from = sel.nextSetBit(0);
    if (from < 0)
      return; // Empty selection - nothing to do.
    int to = from;
    
    int i;

    for (i = from; i>=0; i=sel.nextSetBit(i+1))
      to = i;
    
    sel.clear();
    fireValueChanged(from, to, valueIsAdjusting);
  }
  
  /**
   * Fire the change event, covering the difference between the two sets.
   * 
   * @param current the current set
   * @param x the previous set, the object will be reused.
   */
  private void fireDifference(BitSet current, BitSet x)
  {
    x.xor(current);
    int from = x.nextSetBit(0);
    if (from < 0)
      return; // No difference.
    int to = from;
    int i;

    for (i = from; i >= 0; i = x.nextSetBit(i+1))
      to = i;

    fireValueChanged(from, to, valueIsAdjusting);
  }
  
  /**
   * Clears the current selection and marks a given interval as "selected". If
   * the current selection mode is <code>SINGLE_SELECTION</code> only the
   * index <code>index2</code> is selected.
   * 
   * @param anchor  the anchor selection index.
   * @param lead  the lead selection index.
   */
  public void setSelectionInterval(int anchor, int lead)
  {
    if (anchor == -1 || lead == -1)
      return;
    if (selectionMode == SINGLE_SELECTION)
      {
        int lo = lead;
        int hi = lead;
        int selected = sel.nextSetBit(0);
        if (selected == lead)
          return;  // the selection is not changing
        if (selected >= 0)
          {
            lo = Math.min(lo, selected);
            hi = Math.max(hi, selected);
          }
        if (anchorSelectionIndex >= 0)
          {
            lo = Math.min(lo, anchorSelectionIndex);
            hi = Math.max(hi, anchorSelectionIndex);
          }
        sel.clear();
        sel.set(lead);
        leadSelectionIndex = lead;
        anchorSelectionIndex = lead;
        fireValueChanged(lo, hi);
      }
    else if (selectionMode == SINGLE_INTERVAL_SELECTION)
      {
        // determine the current interval
        int first = sel.nextSetBit(0);
        int last = first;
        if (first >= 0)
          last += (sel.cardinality() - 1);
        
        // update the selection
        int lo = Math.min(anchor, lead);
        int hi = Math.max(anchor, lead);
        if (lo == first && hi == last)
          return;  // selected interval is not being changed
        sel.clear();
        sel.set(lo, hi + 1);
        
        // include the old selection in the event range
        if (first >= 0)
          lo = Math.min(lo, first);
        if (last >= 0)
          hi = Math.max(hi, last);
        if (anchorSelectionIndex >= 0)
          {
            lo = Math.min(lo, anchorSelectionIndex);
            hi = Math.max(hi, anchorSelectionIndex);
          }
        anchorSelectionIndex = anchor;
        leadSelectionIndex = lead;
        fireValueChanged(lo, hi);
      }    
    else
    {
      BitSet oldSel = (BitSet) sel.clone();
      sel.clear();
      if (selectionMode == SINGLE_SELECTION)
        anchor = lead;

      int lo = Math.min(anchor, lead);
      int hi = Math.max(anchor, lead);
      sel.set(lo, hi+1);
      // update the anchorSelectionIndex and leadSelectionIndex variables
      setAnchorSelectionIndex(anchor);
      leadSelectionIndex = lead;
    
      fireDifference(sel, oldSel);
    }
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
   *     or after it
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
   * ListSelectionListener} registered with this selection model to
   * indicate that a series of adjustment has just ended.
   *
   * The values of {@link #getMinSelectionIndex} and
   * {@link #getMaxSelectionIndex} are used in the {@link ListSelectionEvent}
   * that gets fired.
   *
   * @param isAdjusting <code>true</code> if this is the final change
   *     in a series of adjustments, <code>false/code> otherwise
   */
  protected void fireValueChanged(boolean isAdjusting)
  {
    fireValueChanged(getMinSelectionIndex(), getMaxSelectionIndex(),
                     isAdjusting);
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
   *     made to the selection, such as during interactive scrolling
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
   * @see #removeListSelectionListener
   * @see #getListSelectionListeners
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
   * @see #addListSelectionListener
   * @see #getListSelectionListeners
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
   * @see #getListSelectionListeners
   * @since 1.3
   */
  public <T extends EventListener> T[] getListeners(Class<T> listenerType)
  {
    return listenerList.getListeners(listenerType);
  }

  /**
   * Returns an array of all registerd list selection listeners.
   *
   * @return the array
   *
   * @see #addListSelectionListener
   * @see #removeListSelectionListener
   * @see #getListeners
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
    model.listenerList = new EventListenerList();
    return model;
  }
}
