/* TableColumn.java --
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
import javax.swing.event.*;

/**
 * TableColumn
 * @author	Andrew Selkirk
 * @version	1.0
 */
public class TableColumn implements Serializable {

	//-------------------------------------------------------------
	// Variables --------------------------------------------------
	//-------------------------------------------------------------

	/**
	 * COLUMN_WIDTH_PROPERTY
	 */
	public static final String COLUMN_WIDTH_PROPERTY = "columWidth";

	/**
	 * HEADER_VALUE_PROPERTY
	 */
	public static final String HEADER_VALUE_PROPERTY = "headerValue";

	/**
	 * HEADER_RENDERER_PROPERTY
	 */
	public static final String HEADER_RENDERER_PROPERTY = "headerRenderer";

	/**
	 * CELL_RENDERER_PROPERTY
	 */
	public static final String CELL_RENDERER_PROPERTY = "cellRenderer";

	/**
	 * modelIndex
	 */
	protected int modelIndex;

	/**
	 * identifier
	 */
	protected Object identifier;

	/**
	 * width
	 */
	protected int width;

	/**
	 * minWidth
	 */
	protected int minWidth	= 15;

	/**
	 * preferredWidth
	 */
	private int preferredWidth;

	/**
	 * maxWidth
	 */
	protected int maxWidth	= Integer.MAX_VALUE;

	/**
	 * headerRenderer
	 */
	protected TableCellRenderer headerRenderer;

	/**
	 * headerValue
	 */
	protected Object headerValue;

	/**
	 * cellRenderer
	 */
	protected TableCellRenderer cellRenderer;

	/**
	 * cellEditor
	 */
	protected TableCellEditor cellEditor;

	/**
	 * isResizable
	 */
	protected boolean isResizable	= true;

	/**
	 * resizedPostingDisableCount
	 */
	protected transient int resizedPostingDisableCount;

	/**
	 * changeSupport
	 */
	private SwingPropertyChangeSupport changeSupport = new SwingPropertyChangeSupport(this);


	//-------------------------------------------------------------
	// Initialization ---------------------------------------------
	//-------------------------------------------------------------

	/**
	 * Constructor TableColumn
	 */
	public TableColumn() {
		this(0, 75, null, null);
	} // TableColumn()

	/**
	 * Constructor TableColumn
	 * @param modelIndex TODO
	 */
	public TableColumn(int modelIndex) {
		this(modelIndex, 75, null, null);
	} // TableColumn()

	/**
	 * Constructor TableColumn
	 * @param modelIndex TODO
	 * @param width TODO
	 */
	public TableColumn(int modelIndex, int width) {
		this(modelIndex, width, null, null);
	} // TableColumn()

	/**
	 * Constructor TableColumn
	 * @param modelIndex TODO
	 * @param width TODO
	 * @param cellRenderer TODO
	 * @param cellEditor TODO
	 */
	public TableColumn(int modelIndex, int width,
			TableCellRenderer cellRenderer, TableCellEditor cellEditor) {
		this.modelIndex = modelIndex;
		this.width = width;
		this.preferredWidth = width;
		this.cellRenderer = cellRenderer;
		this.cellEditor = cellEditor;
		this.headerValue = null;
		this.identifier = null;
	} // TableColumn()


	//-------------------------------------------------------------
	// Methods ----------------------------------------------------
	//-------------------------------------------------------------

	/**
	 * firePropertyChange
	 * @param property TODO
	 * @param oldValue TODO
	 * @param newValue TODO
	 */
	private void firePropertyChange(String property, Object oldValue, Object newValue) {
		changeSupport.firePropertyChange(property, oldValue, newValue);
	} // firePropertyChange()

	/**
	 * firePropertyChange
	 * @param property TODO
	 * @param oldValue TODO
	 * @param newValue TODO
	 */
	private void firePropertyChange(String property, int oldValue, int newValue) {
		firePropertyChange(property, new Integer(oldValue), new Integer(newValue));
	} // firePropertyChange()

	/**
	 * firePropertyChange
	 * @param property TODO
	 * @param oldValue TODO
	 * @param newValue TODO
	 */
	private void firePropertyChange(String property, boolean oldValue, boolean newValue) {
		firePropertyChange(property, new Boolean(oldValue), new Boolean(newValue));
	} // firePropertyChange()

	/**
	 * setModelIndex
	 * @param modelIndex TODO
	 */
	public void setModelIndex(int modelIndex) {
		this.modelIndex = modelIndex;
	} // setModelIndex()

	/**
	 * getModelIndex
	 * @returns int
	 */
	public int getModelIndex() {
		return modelIndex;
	} // getModelIndex()

	/**
	 * setIdentifier
	 * @param identifier TODO
	 */
	public void setIdentifier(Object identifier) {
		this.identifier = identifier;
	} // setIdentifier()

	/**
	 * getIdentifier
	 * @returns Object
	 */
	public Object getIdentifier() {
		if (identifier == null) {
			return getHeaderValue();
		} // if
		return identifier;
	} // getIdentifier()

	/**
	 * setHeaderValue
	 * @param headerValue TODO
	 */
	public void setHeaderValue(Object headerValue) {

		// Variables
		Object	oldValue;

		// Get Old Value
		oldValue = this.headerValue;

		// Set Propeprty
		this.headerValue = headerValue;

		// Notify Listeners of change
		firePropertyChange(HEADER_VALUE_PROPERTY, 
				oldValue, headerValue);

	} // setHeaderValue()

	/**
	 * getHeaderValue
	 * @returns Object
	 */
	public Object getHeaderValue() {
		return headerValue;
	} // getHeaderValue()

	/**
	 * setHeaderRenderer
	 * @param headerRenderer TODO
	 */
	public void setHeaderRenderer(TableCellRenderer headerRenderer) {

		// Variables
		TableCellRenderer	oldRenderer;

		// Get Old Renderer
		oldRenderer = this.headerRenderer;

		// Set Property
		this.headerRenderer = headerRenderer;

		// Notify Listeners of change
		firePropertyChange(HEADER_RENDERER_PROPERTY, 
				oldRenderer, headerRenderer);

	} // setHeaderRenderer()

	/**
	 * getHeaderRenderer
	 * @returns TableCellRenderer
	 */
	public TableCellRenderer getHeaderRenderer() {
		return headerRenderer;
	} // getHeaderRenderer()

	/**
	 * setCellRenderer
	 * @param cellRenderer TODO
	 */
	public void setCellRenderer(TableCellRenderer cellRenderer) {

		// Variables
		TableCellRenderer	oldRenderer;

		// Get Old Renderer
		oldRenderer = this.cellRenderer;

		// Set Property
		this.cellRenderer = cellRenderer;

		// Notify Listeners of change
		firePropertyChange(CELL_RENDERER_PROPERTY, 
				oldRenderer, cellRenderer);

	} // setCellRenderer()

	/**
	 * getCellRenderer
	 * @returns TableCellRenderer
	 */
	public TableCellRenderer getCellRenderer() {
		return cellRenderer;
	} // getCellRenderer()

	/**
	 * setCellEditor
	 * @param cellEditor TODO
	 */
	public void setCellEditor(TableCellEditor cellEditor) {
		this.cellEditor = cellEditor;
	} // setCellEditor()

	/**
	 * getCellEditor
	 * @returns TableCellEditor
	 */
	public TableCellEditor getCellEditor() {
		return cellEditor;
	} // getCellEditor()

	/**
	 * setWidth
	 * @param width TODO
	 */
	public void setWidth(int width) {

		// Variables
		int	oldWidth;

		// Get Old Width
		oldWidth = this.width;

		// Adjust Width within Limits
		if (width < minWidth) {
			this.width = minWidth;
		} else if (width > maxWidth) {
			this.width = maxWidth;
		} else {
			this.width = width;
		} // if

		// Fire Property Change
		firePropertyChange(COLUMN_WIDTH_PROPERTY, oldWidth, this.width);

	} // setWidth()

	/**
	 * getWidth
	 * @returns int
	 */
	public int getWidth() {
		return width;
	} // getWidth()

	/**
	 * setPreferredWidth
	 * @param preferredWidth TODO
	 */
	public void setPreferredWidth(int preferredWidth) {
		if (preferredWidth < minWidth) {
			this.preferredWidth = minWidth;
		} else if (preferredWidth > maxWidth) {
			this.preferredWidth = maxWidth;
		} else {
			this.preferredWidth = preferredWidth;
		} // if
	} // setPreferredWidth()

	/**
	 * getPreferredWidth
	 * @returns int
	 */
	public int getPreferredWidth() {
		return preferredWidth;
	} // getPreferredWidth()

	/**
	 * setMinWidth
	 * @param minWidth TODO
	 */
	public void setMinWidth(int minWidth) {
		this.minWidth = minWidth;
		setWidth(getWidth());
		setPreferredWidth(getPreferredWidth());
	} // setMinWidth()

	/**
	 * getMinWidth
	 * @returns int
	 */
	public int getMinWidth() {
		return minWidth;
	} // getMinWidth()

	/**
	 * setMaxWidth
	 * @param maxWidth TODO
	 */
	public void setMaxWidth(int maxWidth) {
		this.maxWidth = maxWidth;
		setWidth(getWidth());
		setPreferredWidth(getPreferredWidth());
	} // setMaxWidth()

	/**
	 * getMaxWidth
	 * @returns int
	 */
	public int getMaxWidth() {
		return maxWidth;
	} // getMaxWidth()

	/**
	 * setResizable
	 * @param isResizable TODO
	 */
	public void setResizable(boolean isResizable) {
		this.isResizable = isResizable;
	} // setResizable()

	/**
	 * getResizable
	 * @returns boolean
	 */
	public boolean getResizable() {
		return isResizable;
	} // getResizable()

	/**
	 * sizeWidthToFit
	 */
	public void sizeWidthToFit() {
		// TODO
	} // sizeWidthToFit()

	/**
	 * disableResizedPosting
	 */
	public void disableResizedPosting() {
		// Does nothing
	} // disableResizedPosting()

	/**
	 * enableResizedPosting
	 */
	public void enableResizedPosting() {
		// Does nothing
	} // enableResizedPosting()

	/**
	 * addPropertyChangeListener
	 * @param listener TODO
	 */
	public synchronized void addPropertyChangeListener(PropertyChangeListener listener) {
		changeSupport.addPropertyChangeListener(listener);
	} // addPropertyChangeListener()

	/**
	 * removePropertyChangeListener
	 * @param listener TODO
	 */
	public synchronized void removePropertyChangeListener(PropertyChangeListener listener) {
		changeSupport.removePropertyChangeListener(listener);
	} // removePropertyChangeListener()

	/**
	 * createDefaultHeaderRenderer
	 * @returns TableCellRenderer
	 */
	protected TableCellRenderer createDefaultHeaderRenderer() {
		return new DefaultTableCellRenderer();
	} // createDefaultHeaderRenderer()


} // TableColumn
