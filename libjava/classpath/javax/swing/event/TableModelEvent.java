/* TableModelEvent.java --
   Copyright (C) 2002, 2004, 2005  Free Software Foundation, Inc.

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

import javax.swing.table.TableModel;

/**
 * An event that describes changes to a {@link TableModel}.
 *
 * @see javax.swing.event.TableModelListener
 *
 * @author Andrew Selkirk
 */
public class TableModelEvent extends EventObject
{
  private static final long serialVersionUID = -7849342674552212824L;

  /** A column index representing all columns. */
  public static final int ALL_COLUMNS = -1;

  /**
   * An event type indicating that one or more rows have been deleted from the
   * model.
   */
  public static final int DELETE = -1;

  /** A row index representing the header row. */
  public static final int HEADER_ROW = -1;

  /**
   * An event type indicating that one or more rows have been inserted into the
   * model.
   */
  public static final int INSERT = 1;

  /** An event type indicating that data has been updated in the model. */
  public static final int UPDATE = 0;

  /** The column in the table model that the event relates to. */
  protected int column = 0;

  /** The first row in the table model that the event relates to. */
  protected int firstRow = 0;

  /** The last row in the table model that the event relates to. */
  protected int lastRow = 0;

  /**
   * The event type (one of {@link #UPDATE}, {@link #INSERT}, {@link #DELETE}).
   */
  protected int type = 0;

  /**
   * Creates a new <code>TableModelEvent</code> indicating an {@link #UPDATE}
   * to the data in all columns and rows.
   *
   * @param source  the source object (<code>null</code> not permitted).
   *
   * @throws IllegalArgumentException if <code>source</code> is
   *         <code>null</code>.
   */
  public TableModelEvent(TableModel source)
  {
    this(source, 0, Integer.MAX_VALUE, ALL_COLUMNS, UPDATE);
  }

  /**
   * Creates a new <code>TableModelEvent</code> indicating an {@link #UPDATE}
   * to the data in a single row across all columns.
   *
   * @param source  the source object (<code>null</code> not permitted).
   * @param row  the updated row.
   *
   * @throws IllegalArgumentException if <code>source</code> is
   *         <code>null</code>.
   */
  public TableModelEvent(TableModel source, int row)
  {
    this(source, row, row, ALL_COLUMNS, UPDATE);
  }

  /**
   * Creates a new <code>TableModelEvent</code> indicating an {@link #UPDATE}
   * to the data in the specified rows across all columns.
   *
   * @param source  the source object (<code>null</code> not permitted).
   * @param firstRow  the first row of update.
   * @param lastRow  the last row of update.
   *
   * @throws IllegalArgumentException if <code>source</code> is
   *         <code>null</code>.
   */
  public TableModelEvent(TableModel source, int firstRow, int lastRow)
  {
    this(source, firstRow, lastRow, ALL_COLUMNS, UPDATE);
  }

  /**
   * Creates a new <code>TableModelEvent</code> indicating an {@link #UPDATE}
   * to the data in the specified rows and column.  Use {@link #ALL_COLUMNS}
   * for the <code>column</code> argument to indicate all columns.
   *
   * @param source  the source object (<code>null</code> not permitted).
   * @param firstRow  the first row of update.
   * @param lastRow  the last row of update.
   * @param column  the affected column.
   *
   * @throws IllegalArgumentException if <code>source</code> is
   *         <code>null</code>.
   */
  public TableModelEvent(TableModel source, int firstRow, int lastRow,
                         int column)
  {
    this(source, firstRow, lastRow, column, UPDATE);
  }

  /**
   * Creates a new <code>TableModelEvent</code> indicating an operation of
   * the specified <code>type</code> on the data in the specified rows and
   * column.  The event type is usually one of {@link #UPDATE}, {@link #INSERT},
   * and {@link #DELETE}.
   *
   * @param source  the source object (<code>null</code> not permitted).
   * @param firstRow  the first row of update.
   * @param lastRow  the last row of update.
   * @param column  the affected column.
   * @param type  the type of change.
   *
   * @throws IllegalArgumentException if <code>source</code> is
   *         <code>null</code>.
   */
  public TableModelEvent(TableModel source, int firstRow, int lastRow,
                         int column, int type)
  {
    super(source);
    this.firstRow = firstRow;
    this.lastRow = lastRow;
    this.column = column;
    this.type = type;
  }

  /**
   * Returns the affected column of this event.
   *
   * @return The column index.
   */
  public int getColumn()
  {
    return column;
  }

  /**
   * Returns the first affected row of this event.
   *
   * @return The row index.
   */
  public int getFirstRow()
  {
    return firstRow;
  }

  /**
   * Returns the last affected row of this event.
   *
   * @return The row index.
   */
  public int getLastRow()
  {
    return lastRow;
  }

  /**
   * Returns the type of change indicated by this event (usually one of
   * {@link #UPDATE}, {@link #INSERT}, {@link #DELETE}).
   *
   * @return The type.
   */
  public int getType()
  {
    return type;
  }
}
