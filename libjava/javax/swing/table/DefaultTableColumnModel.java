/* DefaultTableColumnModel.java --
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

package javax.swing.table;

// Imports
import java.beans.*;
import java.io.*;
import java.util.*;
import javax.swing.*;
import javax.swing.event.*;

/**
 * DefaultTableColumnModel
 * @author	Andrew Selkirk
 * @version	1.0
 */
public class DefaultTableColumnModel implements TableColumnModel, PropertyChangeListener, ListSelectionListener, Serializable {

	//-------------------------------------------------------------
	// Variables --------------------------------------------------
	//-------------------------------------------------------------

	/**
	 * tableColumns
	 */
	protected Vector tableColumns;

	/**
	 * selectionModel
	 */
	protected ListSelectionModel selectionModel;

	/**
	 * columnMargin
	 */
	protected int columnMargin;

	/**
	 * listenerList
	 */
	protected EventListenerList listenerList;

	/**
	 * changeEvent
	 */
	protected transient ChangeEvent changeEvent;

	/**
	 * columnSelectionAllowed
	 */
	protected boolean columnSelectionAllowed;

	/**
	 * totalColumnWidth
	 */
	protected int totalColumnWidth;


	//-------------------------------------------------------------
	// Initialization ---------------------------------------------
	//-------------------------------------------------------------

	/**
	 * Constructor DefaultTableColumnModel
	 */
	public DefaultTableColumnModel() {
		// TODO
	} // DefaultTableColumnModel()


	//-------------------------------------------------------------
	// Methods ----------------------------------------------------
	//-------------------------------------------------------------

	/**
	 * addColumn
	 * @param value0 TODO
	 */
	public void addColumn(TableColumn value0) {
		// TODO
	} // addColumn()

	/**
	 * removeColumn
	 * @param value0 TODO
	 */
	public void removeColumn(TableColumn value0) {
		// TODO
	} // removeColumn()

	/**
	 * moveColumn
	 * @param value0 TODO
	 * @param value1 TODO
	 */
	public void moveColumn(int value0, int value1) {
		// TODO
	} // moveColumn()

	/**
	 * setColumnMargin
	 * @param value0 TODO
	 */
	public void setColumnMargin(int value0) {
		// TODO
	} // setColumnMargin()

	/**
	 * getColumnCount
	 * @returns int
	 */
	public int getColumnCount() {
		return 0; // TODO
	} // getColumnCount()

	/**
	 * getColumns
	 * @returns Enumeration
	 */
	public Enumeration getColumns() {
		return null; // TODO
	} // getColumns()

	/**
	 * getColumnIndex
	 * @param value0 TODO
	 * @returns int
	 */
	public int getColumnIndex(Object value0) {
		return 0; // TODO
	} // getColumnIndex()

	/**
	 * getColumn
	 * @param value0 TODO
	 * @returns TableColumn
	 */
	public TableColumn getColumn(int value0) {
		return null; // TODO
	} // getColumn()

	/**
	 * getColumnMargin
	 * @returns int
	 */
	public int getColumnMargin() {
		return 0; // TODO
	} // getColumnMargin()

	/**
	 * getColumnIndexAtX
	 * @param value0 TODO
	 * @returns int
	 */
	public int getColumnIndexAtX(int value0) {
		return 0; // TODO
	} // getColumnIndexAtX()

	/**
	 * getTotalColumnWidth
	 * @returns int
	 */
	public int getTotalColumnWidth() {
		return 0; // TODO
	} // getTotalColumnWidth()

	/**
	 * setSelectionModel
	 * @param value0 TODO
	 */
	public void setSelectionModel(ListSelectionModel value0) {
		// TODO
	} // setSelectionModel()

	/**
	 * getSelectionModel
	 * @returns ListSelectionModel
	 */
	public ListSelectionModel getSelectionModel() {
		return null; // TODO
	} // getSelectionModel()

	/**
	 * setColumnSelectionAllowed
	 * @param value0 TODO
	 */
	public void setColumnSelectionAllowed(boolean value0) {
		// TODO
	} // setColumnSelectionAllowed()

	/**
	 * getColumnSelectionAllowed
	 * @returns boolean
	 */
	public boolean getColumnSelectionAllowed() {
		return false; // TODO
	} // getColumnSelectionAllowed()

	/**
	 * getSelectedColumns
	 * @returns int[]
	 */
	public int[] getSelectedColumns() {
		return null; // TODO
	} // getSelectedColumns()

	/**
	 * getSelectedColumnCount
	 * @returns int
	 */
	public int getSelectedColumnCount() {
		return 0; // TODO
	} // getSelectedColumnCount()

	/**
	 * addColumnModelListener
	 * @param value0 TODO
	 */
	public void addColumnModelListener(TableColumnModelListener value0) {
		// TODO
	} // addColumnModelListener()

	/**
	 * removeColumnModelListener
	 * @param value0 TODO
	 */
	public void removeColumnModelListener(TableColumnModelListener value0) {
		// TODO
	} // removeColumnModelListener()

	/**
	 * fireColumnAdded
	 * @param value0 TODO
	 */
	protected void fireColumnAdded(TableColumnModelEvent value0) {
		// TODO
	} // fireColumnAdded()

	/**
	 * fireColumnRemoved
	 * @param value0 TODO
	 */
	protected void fireColumnRemoved(TableColumnModelEvent value0) {
		// TODO
	} // fireColumnRemoved()

	/**
	 * fireColumnMoved
	 * @param value0 TODO
	 */
	protected void fireColumnMoved(TableColumnModelEvent value0) {
		// TODO
	} // fireColumnMoved()

	/**
	 * fireColumnSelectionChanged
	 * @param value0 TODO
	 */
	protected void fireColumnSelectionChanged(ListSelectionEvent value0) {
		// TODO
	} // fireColumnSelectionChanged()

	/**
	 * fireColumnMarginChanged
	 */
	protected void fireColumnMarginChanged() {
		// TODO
	} // fireColumnMarginChanged()

	/**
	 * getListeners
	 * @param value0 TODO
	 * @returns EventListener[]
	 */
	public EventListener[] getListeners(Class value0) {
		return null; // TODO
	} // getListeners()

	/**
	 * propertyChange
	 * @param value0 TODO
	 */
	public void propertyChange(PropertyChangeEvent value0) {
		// TODO
	} // propertyChange()

	/**
	 * valueChanged
	 * @param value0 TODO
	 */
	public void valueChanged(ListSelectionEvent value0) {
		// TODO
	} // valueChanged()

	/**
	 * createSelectionModel
	 * @returns ListSelectionModel
	 */
	protected ListSelectionModel createSelectionModel() {
		return null; // TODO
	} // createSelectionModel()

	/**
	 * recalcWidthCache
	 */
	protected void recalcWidthCache() {
		// TODO
	} // recalcWidthCache()

	/**
	 * invalidateWidthCache
	 */
	private void invalidateWidthCache() {
		// TODO
	} // invalidateWidthCache()


} // DefaultTableColumnModel

