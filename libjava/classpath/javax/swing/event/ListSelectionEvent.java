/* ListSelectionEvent.java --
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

package javax.swing.event;

import java.util.EventObject;

import javax.swing.ListSelectionModel;

/**
 * An event that indicates a change to a list selection, including the source
 * of the change (a {@link ListSelectionModel}) and the range of items in the
 * list that have potentially changed their selection status.
 * 
 * @author Andrew Selkirk
 * @author Ronald Veldema
 */
public class ListSelectionEvent extends EventObject 
{

  /** 
   * The index of the first list item in the range of items that has 
   * potentially had its selection status modified. 
   */
  private int firstIndex = 0;

  /** 
   * The index of the last list item in the range of items that has 
   * potentially had its selection status modified. 
   */
  private int lastIndex = 0;

  /** A flag that indicates that this event is one in a series of events. */
  private boolean isAdjusting = false;

  /**
   * Creates a new <code>ListSelectionEvent</code>.
   * 
   * @param source  the event source (<code>null</code> not permitted).
   * @param firstIndex  the first index.
   * @param lastIndex  the last index.
   * @param isAdjusting  a flag indicating that this event is one in a series 
   *                     of events updating a selection.
   * 
   * @throws IllegalArgumentException if <code>source</code> is 
   *         <code>null</code>.
   */
  public ListSelectionEvent(Object source, int firstIndex,
      int lastIndex, boolean isAdjusting) 
  {
    super(source);
    this.firstIndex = firstIndex;
    this.lastIndex = lastIndex;
    this.isAdjusting = isAdjusting;
  }
 
  /**
   * Returns the first index.
   * 
   * @return The first index.
   */
  public int getFirstIndex() 
  {
    return firstIndex;
  }

  /**
   * Returns the last index.
   * 
   * @return The last index.
   */
  public int getLastIndex() 
  {
    return lastIndex;
  }

  /**
   * Returns the flag that indicates that this event is one in a series of 
   * events updating a selection.
   * 
   * @return A boolean.
   */
  public boolean getValueIsAdjusting() 
  {
    return isAdjusting;
  }

  /**
   * Returns a string representation of the event, typically used for debugging
   * purposes.
   * 
   * @return A string representation of the event.
   */
  public String toString() 
  {
    return this.getClass().toString() + "[ source=" + source.toString() 
        + " firstIndex= " + firstIndex + " lastIndex= " + lastIndex 
        + " isAdjusting= " + isAdjusting + " ]";
  }

}
