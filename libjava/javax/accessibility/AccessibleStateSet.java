/* AccessibleStateSet.java -- the combined state of an accessible object
   Copyright (C) 2002 Free Software Foundation

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

package javax.accessibility;

import java.util.Vector;

/**
 * Describes all elements of an accessible object's state. For example, an
 * object may be enabled and have focus.
 *
 * @author Eric Blake <ebb9@email.byu.edu>
 * @see AccessibleState
 * @since 1.2
 * @status updated to 1.4
 */
public class AccessibleStateSet
{
  /**
   * The list of states, should be instances of AccessibleState. Don't set
   * this to null.
   *
   * @see #add(AccessibleState)
   * @see #addAll(AccessibleState[])
   * @see #remove(AccessibleState)
   * @see #contains(AccessibleState)
   * @see #toArray()
   * @see #clear()
   */
  protected Vector states = new Vector();

  /**
   * Create an empty state set.
   */
  public AccessibleStateSet()
  {
  }

  /**
   * Create a state set initialized with the given states, duplicates are
   * ignored.
   *
   * @param states the states to insert
   * @throws NullPointerException if states is null
   */
  public AccessibleStateSet(AccessibleState[] states)
  {
    addAll(states);
  }

  /**
   * Add a new state to the current set. Return true if the state was added,
   * as duplicates are ignored. Entering a null state will cause problems
   * later, so don't do it.
   *
   * @param state the state to add
   * @return true if the state was added
   */
  public boolean add(AccessibleState state)
  {
    return states.contains(state) ? false : states.add(state);
  }

  /**
   * Add all of the states to the current set. Duplicates are ignored.
   * Entering a null state will cause problems later, so don't do it.
   *
   * @param array the array of states to add
   * @throws NullPointerException if array is null
   */
  public void addAll(AccessibleState[] array)
  {
    int i = array.length;
    while (--i >= 0)
      add(array[i]);
  }

  /**
   * Remove a state from the set. If a state was removed, return true.
   *
   * @param state the state to remove
   * @return true if the set changed
   */
  public boolean remove(AccessibleState state)
  {
    return states.remove(state);
  }

  /**
   * Clear all states in the set.
   */
  public void clear()
  {
    states.clear();
  }

  /**
   * Check if the current state is in the set.
   *
   * @param state the state to locate
   * @return true if it is in the set
   */
  public boolean contains(AccessibleState state)
  {
    return states.contains(state);
  }

  /**
   * Return the state set as an array.
   *
   * @return an array of the current states
   */
  public AccessibleState[] toArray()
  {
    AccessibleState[] result = new AccessibleState[states.size()];
    states.toArray(result);
    return result;
  }

  /**
   * Return a localized, comma-separated string representing all states
   * in the set. This is in arbitrary order.
   *
   * @return the string representation
   * @see AccessibleBundle#toDisplayString(String, Locale)
   */
  public String toString()
  {
    int i = states.size();
    if (i == 0)
      return "";
    // Pre-allocate an average of 10 chars per state.
    StringBuffer b = new StringBuffer(i * 10);
    while (--i >= 0)
      b.append(states.get(i)).append(',');
    return b.substring(0, b.length() - 1);
  }
} // class AccessibleStateSet
