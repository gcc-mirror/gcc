/* TableModelEvent.java --
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


package javax.swing.event;

import java.util.EventObject;

import javax.swing.table.TableModel;

/**
 * @author Andrew Selkirk
 */
public class TableModelEvent extends EventObject
{
  private static final long serialVersionUID = -7849342674552212824L;
  
  public static final int ALL_COLUMNS = -1;
  public static final int DELETE = -1;
  public static final int HEADER_ROW = -1;
  public static final int INSERT = 1;
  public static final int UPDATE = 0;

  protected int column = 0;
  protected int firstRow = 0;
  protected int lastRow = 0;
  protected int type = 0;

  /**
   * Creates a <code>TableModelEvent</code> event.
   * 
   * @param source The source object
   */
  public TableModelEvent(TableModel source)
  {
    this(source, 0, source.getRowCount(), ALL_COLUMNS, UPDATE);
  }

  /**
   * Creates a <code>TableModelEvent</code> event.
   * 
   * @param source The source object
   * @param row The updated row
   */
  public TableModelEvent(TableModel source, int row)
  {
    this(source, row, row, ALL_COLUMNS, UPDATE);
  }

  /**
   * Creates a <code>TableModelEvent</code> event.
   * 
   * @param source The source object
   * @param firstRow The first row of update
   * @param lastRow The last row of update
   */
  public TableModelEvent(TableModel source, int firstRow, int lastRow)
  {
    this(source, firstRow, lastRow, ALL_COLUMNS, UPDATE);
  }

  /**
   * Creates a <code>TableModelEvent</code> event.
   * 
   * @param source The source object
   * @param firstRow The first row of update
   * @param lastRow The last row of update
   * @param column The affected column
   */
  public TableModelEvent(TableModel source, int firstRow, int lastRow, int column)
  {
    this(source, firstRow, lastRow, column, UPDATE);
  }

  /**
   * Creates a <code>TableModelEvent</code> event.
   * 
   * @param source The source object
   * @param firstRow The first row of update
   * @param lastRow The last row of update
   * @param column The affected column
   * @param type The type of change
   */
  public TableModelEvent(TableModel source, int firstRow, int lastRow, int column, int type)
  {
    super(source);
    this.firstRow = firstRow;
    this.lastRow = lastRow;
    this.column = column;
    this.type = type;
  }

  /**
   * Returns the affected column of this event.
   */
  public int getColumn()
  {
    return column;
  }

  /**
   * Returns the first affected row of this event.
   */
  public int getFirstRow()
  {
    return firstRow;
  }

  /**
   * Returns the last affected row of this event.
   */
  public int getLastRow()
  {
    return lastRow;
  }

  /**
   * Returns the type of change of this event.
   */
  public int getType()
  {
    return type;
  }
}
