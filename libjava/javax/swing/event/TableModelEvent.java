/* TableModelEvent.java --
   Copyright (C) 2002 Free Software Foundation, Inc.

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

// Imports
import java.util.EventObject;
import javax.swing.table.TableModel;

/**
 * TableModelEvent
 * @author Andrew Selkirk
 */
public class TableModelEvent extends EventObject {

	//-------------------------------------------------------------
	// Constants --------------------------------------------------
	//-------------------------------------------------------------

	/**
	 * ALL_COLUMNS
	 */
	public static	int	ALL_COLUMNS		= -1;

	/**
	 * DELETE
	 */
	public static	int	DELETE			= -1;

	/**
	 * HEADER_ROW
	 */
	public static	int	HEADER_ROW		= -1;

	/**
	 * INSERT
	 */
	public static	int	INSERT			= 1;

	/**
	 * UPDATE
	 */
	public static	int	UPDATE			= 0;


	//-------------------------------------------------------------
	// Variables --------------------------------------------------
	//-------------------------------------------------------------

	/**
	 * column
	 */
	protected		int	column			= 0;
	
	/**
	 * firstRow
	 */
	protected		int firstRow		= 0;
	
	/**
	 * lastRow
	 */
	protected		int	lastRow			= 0;

	/**
	 * type
	 */
	protected		int	type			= 0;


	//-------------------------------------------------------------
	// Initialization ---------------------------------------------
	//-------------------------------------------------------------

	/**
	 * Constructor TableModelEvent
	 * @param source Source object
	 */
	public TableModelEvent(TableModel source) {
		this(source, 0, source.getRowCount(), ALL_COLUMNS, UPDATE);
	} // TableModelEvent()

	/**
	 * Constructor TableModelEvent
	 * @param source Source table model
	 * @param row Updated row
	 */
	public TableModelEvent(TableModel source, int row) {
		this(source, row, row, ALL_COLUMNS, UPDATE);
	} // TableModelEvent()

	/**
	 * Constructor TableModelEvent
	 * @param source Source table model
	 * @param firstRow First row of update
	 * @param lastRow Last row of update
	 */
	public TableModelEvent(TableModel source, int firstRow,
							int lastRow) {
		this(source, firstRow, lastRow, ALL_COLUMNS, UPDATE);
	} // TableModelEvent()

	/**
	 * Constructor TableModelEvent
	 * @param source Source table model
	 * @param firstRow First row of update
	 * @param lastRow Last row of update
	 * @param column Affected column
	 */
	public TableModelEvent(TableModel source, int firstRow,
							int lastRow, int column) {
		this(source, firstRow, lastRow, column, UPDATE);
	} // TableModelEvent()

	/**
	 * Constructor TableModelEvent
	 * @param source Source table model
	 * @param firstRow First row of update
	 * @param lastRow Last row of update
	 * @param column Affected column
	 * @param type Type of change
	 */
	public TableModelEvent(TableModel source, int firstRow,
							int lastRow, int column, int type) {
		super(source);
		this.firstRow	= firstRow;
		this.lastRow	= lastRow;
		this.column		= column;
		this.type		= type;
	} // TableModelEvent()


	//-------------------------------------------------------------
	// Methods ----------------------------------------------------
	//-------------------------------------------------------------

	/**
	 * getColumn
	 * @returns column
	 */
	public int getColumn() {
		return column;
	} // getColumn()

	/**
	 * getFirstRow
	 * @returns row
	 */
	public int getFirstRow() {
		return firstRow;
	} // getFirstRow()

	/**
	 * getLastRow
	 * @returns row
	 */
	public int getLastRow() {
		return lastRow;
	} // getLastRow()

	/**
	 * Get type
	 * @returns Type of event
	 */
	public int getType() {
		return type;
	} // getType()


} // TableModelEvent
