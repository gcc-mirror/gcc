/* DefaultTreeCellEditor.java --
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


package javax.swing.tree;

import java.awt.Color;
import java.awt.Component;
import java.awt.Container;
import java.awt.Dimension;
import java.awt.Font;
import java.awt.Graphics;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.io.IOException;
import java.io.ObjectInputStream;
import java.io.ObjectOutputStream;
import java.util.EventObject;
import javax.swing.Icon;
import javax.swing.JTextField;
import javax.swing.JTree;
import javax.swing.border.Border;
import javax.swing.event.CellEditorListener;
import javax.swing.event.EventListenerList;
import javax.swing.event.TreeSelectionEvent;
import javax.swing.event.TreeSelectionListener;

/**
 * DefaultTreeCellEditor
 * @author Andrew Selkirk
 */
public class DefaultTreeCellEditor implements ActionListener, TreeCellEditor, TreeSelectionListener {

	//-------------------------------------------------------------
	// Classes ----------------------------------------------------
	//-------------------------------------------------------------

	/**
	 * EditorContainer
	 */
	public class EditorContainer extends Container {

		//-------------------------------------------------------------
		// Variables --------------------------------------------------
		//-------------------------------------------------------------


		//-------------------------------------------------------------
		// Initialization ---------------------------------------------
		//-------------------------------------------------------------

		/**
		 * Constructor EditorContainer
		 * @param value0 TODO
		 */
		public EditorContainer(DefaultTreeCellEditor value0) {
			// TODO
		} // EditorContainer()


		//-------------------------------------------------------------
		// Methods ----------------------------------------------------
		//-------------------------------------------------------------

		/**
		 * getPreferredSize
		 * @returns Dimension
		 */
		public Dimension getPreferredSize() {
			return null; // TODO
		} // getPreferredSize()

		/**
		 * paint
		 * @param value0 TODO
		 */
		public void paint(Graphics value0) {
			// TODO
		} // paint()

		/**
		 * doLayout
		 */
		public void doLayout() {
			// TODO
		} // doLayout()


	} // EditorContainer

	/**
	 * DefaultTextField
	 */
	public class DefaultTextField extends JTextField {

		//-------------------------------------------------------------
		// Variables --------------------------------------------------
		//-------------------------------------------------------------

		/**
		 * border
		 */
		protected Border border;


		//-------------------------------------------------------------
		// Initialization ---------------------------------------------
		//-------------------------------------------------------------

		/**
		 * Constructor DefaultTextField
		 * @param value0 TODO
		 * @param value1 TODO
		 */
		public DefaultTextField(DefaultTreeCellEditor value0, Border value1) {
			// TODO
		} // DefaultTextField()


		//-------------------------------------------------------------
		// Methods ----------------------------------------------------
		//-------------------------------------------------------------

		/**
		 * getFont
		 * @returns Font
		 */
		public Font getFont() {
			return null; // TODO
		} // getFont()

		/**
		 * getBorder
		 * @returns Border
		 */
		public Border getBorder() {
			return null; // TODO
		} // getBorder()

		/**
		 * getPreferredSize
		 * @returns Dimension
		 */
		public Dimension getPreferredSize() {
			return null; // TODO
		} // getPreferredSize()


	} // DefaultTextField


	//-------------------------------------------------------------
	// Variables --------------------------------------------------
	//-------------------------------------------------------------

	/**
	 * realEditor
	 */
	protected TreeCellEditor realEditor;

	/**
	 * renderer
	 */
	protected DefaultTreeCellRenderer renderer;

	/**
	 * editingContainer
	 */
	protected Container editingContainer;

	/**
	 * editingComponent
	 */
	protected transient Component editingComponent;

	/**
	 * canEdit
	 */
	protected boolean canEdit;

	/**
	 * offset
	 */
	protected transient int offset;

	/**
	 * tree
	 */
	protected transient JTree tree;

	/**
	 * lastPath
	 */
	protected transient TreePath lastPath;

	/**
	 * timer
	 */
	protected transient javax.swing.Timer timer; // TODO

	/**
	 * lastRow
	 */
	protected transient int lastRow;

	/**
	 * borderSelectionColor
	 */
	protected Color borderSelectionColor;

	/**
	 * editingIcon
	 */
	protected transient Icon editingIcon;

	/**
	 * font
	 */
	protected Font font;


	//-------------------------------------------------------------
	// Initialization ---------------------------------------------
	//-------------------------------------------------------------

	/**
	 * Constructor DefaultTreeCellEditor
	 * @param value0 TODO
	 * @param value1 TODO
	 */
	public DefaultTreeCellEditor(JTree value0, DefaultTreeCellRenderer value1) {
		// TODO
	} // DefaultTreeCellEditor()

	/**
	 * Constructor DefaultTreeCellEditor
	 * @param value0 TODO
	 * @param value1 TODO
	 * @param value2 TODO
	 */
	public DefaultTreeCellEditor(JTree value0, DefaultTreeCellRenderer value1, TreeCellEditor value2) {
		// TODO
	} // DefaultTreeCellEditor()


	//-------------------------------------------------------------
	// Methods ----------------------------------------------------
	//-------------------------------------------------------------

	/**
	 * writeObject
	 * @param value0 TODO
	 * @exception IOException TODO
	 */
	private void writeObject(ObjectOutputStream value0) throws IOException {
		// TODO
	} // writeObject()

	/**
	 * readObject
	 * @param value0 TODO
	 * @exception IOException TODO
	 * @exception ClassNotFoundException TODO
	 */
	private void readObject(ObjectInputStream value0) throws IOException, ClassNotFoundException {
		// TODO
	} // readObject()

	/**
	 * setBorderSelectionColor
	 * @param value0 TODO
	 */
	public void setBorderSelectionColor(Color value0) {
		// TODO
	} // setBorderSelectionColor()

	/**
	 * getBorderSelectionColor
	 * @returns Color
	 */
	public Color getBorderSelectionColor() {
		return null; // TODO
	} // getBorderSelectionColor()

	/**
	 * setFont
	 * @param value0 TODO
	 */
	public void setFont(Font value0) {
		// TODO
	} // setFont()

	/**
	 * getFont
	 * @returns Font
	 */
	public Font getFont() {
		return null; // TODO
	} // getFont()

	/**
	 * getTreeCellEditorComponent
	 * @param value0 TODO
	 * @param value1 TODO
	 * @param value2 TODO
	 * @param value3 TODO
	 * @param value4 TODO
	 * @param value5 TODO
	 * @returns Component
	 */
	public Component getTreeCellEditorComponent(JTree value0, Object value1, boolean value2, boolean value3, boolean value4, int value5) {
		return null; // TODO
	} // getTreeCellEditorComponent()

	/**
	 * getCellEditorValue
	 * @returns Object
	 */
	public Object getCellEditorValue() {
		return null; // TODO
	} // getCellEditorValue()

	/**
	 * isCellEditable
	 * @param value0 TODO
	 * @returns boolean
	 */
	public boolean isCellEditable(EventObject value0) {
		return false; // TODO
	} // isCellEditable()

	/**
	 * shouldSelectCell
	 * @param value0 TODO
	 * @returns boolean
	 */
	public boolean shouldSelectCell(EventObject value0) {
		return false; // TODO
	} // shouldSelectCell()

	/**
	 * stopCellEditing
	 * @returns boolean
	 */
	public boolean stopCellEditing() {
		return false; // TODO
	} // stopCellEditing()

	/**
	 * cancelCellEditing
	 */
	public void cancelCellEditing() {
		// TODO
	} // cancelCellEditing()

	/**
	 * addCellEditorListener
	 * @param value0 TODO
	 */
	public void addCellEditorListener(CellEditorListener value0) {
		// TODO
	} // addCellEditorListener()

	/**
	 * removeCellEditorListener
	 * @param value0 TODO
	 */
	public void removeCellEditorListener(CellEditorListener value0) {
		// TODO
	} // removeCellEditorListener()

	/**
	 * valueChanged
	 * @param value0 TODO
	 */
	public void valueChanged(TreeSelectionEvent value0) {
		// TODO
	} // valueChanged()

	/**
	 * actionPerformed
	 * @param value0 TODO
	 */
	public void actionPerformed(ActionEvent value0) {
		// TODO
	} // actionPerformed()

	/**
	 * setTree
	 * @param value0 TODO
	 */
	protected void setTree(JTree value0) {
		// TODO
	} // setTree()

	/**
	 * shouldStartEditingTimer
	 * @param value0 TODO
	 * @returns boolean
	 */
	protected boolean shouldStartEditingTimer(EventObject value0) {
		return false; // TODO
	} // shouldStartEditingTimer()

	/**
	 * startEditingTimer
	 */
	protected void startEditingTimer() {
		// TODO
	} // startEditingTimer()

	/**
	 * canEditImmediately
	 * @param value0 TODO
	 * @returns boolean
	 */
	protected boolean canEditImmediately(EventObject value0) {
		return false; // TODO
	} // canEditImmediately()

	/**
	 * inHitRegion
	 * @param value0 TODO
	 * @param value1 TODO
	 * @returns boolean
	 */
	protected boolean inHitRegion(int value0, int value1) {
		return false; // TODO
	} // inHitRegion()

	/**
	 * determineOffset
	 * @param value0 TODO
	 * @param value1 TODO
	 * @param value2 TODO
	 * @param value3 TODO
	 * @param value4 TODO
	 * @param value5 TODO
	 */
	protected void determineOffset(JTree value0, Object value1, boolean value2, boolean value3, boolean value4, int value5) {
		// TODO
	} // determineOffset()

	/**
	 * prepareForEditing
	 */
	protected void prepareForEditing() {
		// TODO
	} // prepareForEditing()

	/**
	 * createContainer
	 * @returns Container
	 */
	protected Container createContainer() {
		return null; // TODO
	} // createContainer()

	/**
	 * createTreeCellEditor
	 * @returns TreeCellEditor
	 */
	protected TreeCellEditor createTreeCellEditor() {
		return null; // TODO
	} // createTreeCellEditor()


} // DefaultTreeCellEditor
