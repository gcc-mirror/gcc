/* AccessibleSelection.java -- aids in accessibly selecting components
   Copyright (C) 2000, 2002, 2005  Free Software Foundation, Inc.

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

/**
 * If an object implements this interface then it must be able to control
 * the selection of its children. Accessibility software can use the
 * implementations of this interface to change the selection set of children.
 *
 * <p>The <code>AccessibleContext.getAccessibleSelection()</code> method should
 * return <code>null</code> if an object does not implement this interface.
 *
 * @author Eric Blake (ebb9@email.byu.edu)
 * @see Accessible
 * @see AccessibleContext
 * @see AccessibleContext#getAccessibleSelection()
 * @since 1.2
 * @status updated to 1.4
 */
public interface AccessibleSelection
{
  /**
   * Returns the number of currently selected Accessible children, which may
   * be 0 if nothing is selected.
   *
   * @return the number of selected children
   */
  int getAccessibleSelectionCount();

  /**
   * Returns the i-th selected child (not necessarily the overall i-th child)
   * of this Accessible object. If i is out of bounds, null is returned.
   *
   * @param i zero-based index of selected child objects
   * @return the Accessible child, or null
   * @see #getAccessibleSelectionCount()
   */
  Accessible getAccessibleSelection(int i);

  /**
   * Determine if i-th overall child of this accessible object is selected.
   * If i is out of bounds, false is returned.
   *
   * @param i zero-based index of child objects
   * @return true if specified child exists and is selected
   */
  boolean isAccessibleChildSelected(int i);

  /**
   * Select the specified child if it is not already selected, placing it in
   * the object's current selection. If the object does not support multiple
   * selections then the new selection replaces the old. If the specified
   * child is already selected, or is out of bounds, this method does nothing.
   *
   * @param i zero-based index of child objects
   */
  void addAccessibleSelection(int i);

  /**
   * Unselect the specified child of this Accessible object. If the specified
   * child is not selected, or is out of bounds, this method does nothing.
   *
   * @param i the zero-based index of the child objects
   */
  void removeAccessibleSelection(int i);

  /**
   * Unselect all children of this Accessible object.
   */
  void clearAccessibleSelection();

  /**
   * Select all children of this Accessible object if the object supports
   * multiple selections or has a single child. Otherwise this does nothing.
   */
  void selectAllAccessibleSelection();
} // interface AccessibleSelection
