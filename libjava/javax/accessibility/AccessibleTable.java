/* AccessibleTable.java -- aids in accessibly manipulating tables
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
 * Objects which present information in a 2-dimensional table should implement
 * this interface. Accessibility software can use the implementations of
 * this interface to navigate and change the attributes of the table.
 *
 * <p>The <code>AccessibleContext.getAccessibleTable()</code> method
 * should return <code>null</code> if an object does not implement this
 * interface.
 *
 * @author Eric Blake (ebb9@email.byu.edu)
 * @see Accessible
 * @see AccessibleContext
 * @see AccessibleContext#getAccessibleTable()
 * @since 1.2
 * @status updated to 1.4
 */
public interface AccessibleTable
{
  /**
   * Return the caption for the table, or null if unknown.
   *
   * @return the table caption
   */
  Accessible getAccessibleCaption();

  /**
   * Set the table caption.
   *
   * @param caption the new caption
   */
  void setAccessibleCaption(Accessible caption);

  /**
   * Return the summary description of the table, or null if unknown.
   *
   * @return the summary description
   */
  Accessible getAccessibleSummary();

  /**
   * Set the table summary description.
   *
   * @param summary the new summary
   */
  void setAccessibleSummary(Accessible summary);

  /**
   * Return the number of rows in the table.
   *
   * @return the row count
   */
  int getAccessibleRowCount();

  /**
   * Return the number of columns in the table.
   *
   * @return the column count
   */
  int getAccessibleColumnCount();

  /**
   * Return the cell at the specified row and column, or null if out of bounds.
   *
   * @param r the 0-based row index
   * @param c the 0-based column index
   * @return the cell at (r,c)
   */
  Accessible getAccessibleAt(int r, int c);

  /**
   * Returns the number of merged rows occupied at the specified row and
   * column, or 0 if out of bounds.
   *
   * @param r the 0-based row index
   * @param c the 0-based column index
   * @return the row extent at (r,c)
   */
  int getAccessibleRowExtentAt(int r, int c);

  /**
   * Returns the number of merged columns occupied at the specified row and
   * column, or 0 if out of bounds.
   *
   * @param r the 0-based row index
   * @param c the 0-based column index
   * @return the column extent at (r,c)
   */
  int getAccessibleColumnExtentAt(int r, int c);

  /**
   * Return the row headers as a table.
   *
   * @return the row headers, or null if there are none
   */
  AccessibleTable getAccessibleRowHeader();

  /**
   * Set the row headers.
   *
   * @param header the new row header
   */
  // XXX What happens if header is incompatible size?
  void setAccessibleRowHeader(AccessibleTable header);

  /**
   * Return the column headers as a table.
   *
   * @return the column headers, or null if there are none
   */
  AccessibleTable getAccessibleColumnHeader();

  /**
   * Set the column headers.
   *
   * @param header the new column header
   */
  // XXX What happens if header is incompatible size?
  void setAccessibleColumnHeader(AccessibleTable header);

  /**
   * Return the description of a row, or null if there is none or the index
   * is out of bounds.
   *
   * @param r the 0-based row index
   * @return the description
   */
  Accessible getAccessibleRowDescription(int r);

  /**
   * Set the description of a row. Does nothing if the index is invalid.
   *
   * @param r the 0-based row index
   * @param description the new description
   */
  void setAccessibleRowDescription(int r, Accessible description);

  /**
   * Return the description of a column, or null if there is none or the index
   * is out of bounds.
   *
   * @param c the 0-based column index
   * @return the description
   */
  Accessible getAccessibleColumnDescription(int c);

  /**
   * Set the description of a column. Does nothing if the index is invalid.
   *
   * @param c the 0-based column index
   * @param description the new description
   */
  void setAccessibleColumnDescription(int c, Accessible description);

  /**
   * Return whether the cell at the specified location is selected. Returns
   * false if the index is out of bounds.
   *
   * @param r the 0-based row index
   * @param c the 0-based column index
   * @return true if that cell is selected
   */
  boolean isAccessibleSelected(int r, int c);

  /**
   * Return whether the specified row is selected. Returns false if the
   * index is out of bounds.
   *
   * @param r the 0-based row index
   * @return true if that row is selected
   */
  boolean isAccessibleRowSelected(int r);

  /**
   * Return whether the specified column is selected. Returns false if the
   * index is out of bounds.
   *
   * @param c the 0-based column index
   * @return true if that column is selected
   */
  boolean isAccessibleColumnSelected(int c);

  /**
   * Return the selected rows. May be null or empty if there is no selection.
   *
   * @return the indices of selected rows
   */
  int[] getSelectedAccessibleRows();

  /**
   * Return the selected columns. May be null or empty if there is no
   * selection.
   *
   * @return the indices of selected columns
   */
  int[] getSelectedAccessibleColumns();
} // interface AccessibleTable
