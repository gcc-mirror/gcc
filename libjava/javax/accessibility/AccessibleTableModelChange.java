/* AccessibleTableModelChange.java -- describes change to an accessible table
   Copyright (C) 2002, 2005  Free Software Foundation, Inc.

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
 * Describes a change to an accessible table. Accessibility software can use
 * the implementations of this interface to update their state after a
 * change to a table.
 *
 * @author Eric Blake (ebb9@email.byu.edu)
 * @see Accessible
 * @see AccessibleContext
 * @see AccessibleContext#getAccessibleTable()
 * @see AccessibleTable
 * @since 1.2
 * @status updated to 1.4
 */
public interface AccessibleTableModelChange
{
  /** Identifies insertion of rows or columns. */
  int INSERT = 1;

  /** Identifies change to existing data. */
  int UPDATE = 0;

  /** Identifies deletion of rows or columns. */
  int DELETE = -1;

  /**
   * Returns the change type.
   *
   * @return the type
   * @see #INSERT
   * @see #UPDATE
   * @see #DELETE
   */
  int getType();

  /**
   * Returns the first row that changed.
   *
   * @return the 0-based index of the first row to change
   */
  int getFirstRow();

  /**
   * Returns the last row that changed.
   *
   * @return the 0-based index of the last row to change
   */
  int getLastRow();

  /**
   * Returns the first column that changed.
   *
   * @return the 0-based index of the first column to change
   */
  int getFirstColumn();

  /**
   * Returns the last column that changed.
   *
   * @return the 0-based index of the last column to change
   */
  int getLastColumn();
} // interface AccessibleTableModelChange
