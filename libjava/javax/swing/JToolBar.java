/* JToolBar.java --
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

package javax.swing;

// Imports
import java.awt.*;
import java.beans.*;
import java.io.*;
import javax.accessibility.*;
import javax.swing.plaf.*;

/**
 * JToolBar
 * @author	Andrew Selkirk
 * @version	1.0
 */
public class JToolBar extends JComponent 
		implements SwingConstants, Accessible {

	//-------------------------------------------------------------
	// Classes ----------------------------------------------------
	//-------------------------------------------------------------

	/**
	 * AccessibleJToolBar
	 */
	protected class AccessibleJToolBar extends AccessibleJComponent {

		//-------------------------------------------------------------
		// Initialization ---------------------------------------------
		//-------------------------------------------------------------

		/**
		 * Constructor AccessibleJToolBar
		 * @param component TODO
		 */
		protected AccessibleJToolBar(JToolBar component) {
			super(component);
			// TODO
		} // AccessibleJToolBar()


		//-------------------------------------------------------------
		// Methods ----------------------------------------------------
		//-------------------------------------------------------------

		/**
		 * getAccessibleStateSet
		 * @returns AccessibleStateSet
		 */
		public AccessibleStateSet getAccessibleStateSet() {
			return null; // TODO
		} // getAccessibleStateSet()

		/**
		 * getAccessibleRole
		 * @returns AccessibleRole
		 */
		public AccessibleRole getAccessibleRole() {
			return AccessibleRole.TOOL_BAR;
		} // getAccessibleRole()


	} // AccessibleJToolBar

	/**
	 * Separator
	 */
	public static class Separator extends JSeparator {

		//-------------------------------------------------------------
		// Variables --------------------------------------------------
		//-------------------------------------------------------------

		/**
		 * separatorSize
		 */
		private Dimension size;


		//-------------------------------------------------------------
		// Initialization ---------------------------------------------
		//-------------------------------------------------------------

		/**
		 * Constructor Separator
		 */
		public Separator() {
			// TODO
		} // Separator()

		/**
		 * Constructor Separator
		 * @param size TODO
		 */
		public Separator(Dimension size) {
			// TODO
		} // Separator()


		//-------------------------------------------------------------
		// Methods ----------------------------------------------------
		//-------------------------------------------------------------

		/**
		 * getUIClassID
		 * @returns String
		 */
		public String getUIClassID() {
			return null; // TODO
		} // getUIClassID()

		/**
		 * getPreferredSize
		 * @returns Dimension
		 */
		public Dimension getPreferredSize() {
			return null; // TODO
		} // getPreferredSize()

		/**
		 * getMaximumSize
		 * @returns Dimension
		 */
		public Dimension getMaximumSize() {
			return null; // TODO
		} // getMaximumSize()

		/**
		 * getMinimumSize
		 * @returns Dimension
		 */
		public Dimension getMinimumSize() {
			return null; // TODO
		} // getMinimumSize()

		/**
		 * getSeparatorSize
		 * @returns Dimension
		 */
		public Dimension getSeparatorSize() {
			return null; // TODO
		} // getSeparatorSize()

		/**
		 * setSeparatorSize
		 * @param size TODO
		 */
		public void setSeparatorSize(Dimension size) {
			// TODO
		} // setSeparatorSize()


	} // Separator


	//-------------------------------------------------------------
	// Variables --------------------------------------------------
	//-------------------------------------------------------------

	/**
	 * uiClassID
	 */
	private static final String uiClassID = "ToolBarUI";

	/**
	 * paintBorder
	 */
	private boolean paintBorder;

	/**
	 * margin
	 */
	private Insets margin;

	/**
	 * floatable
	 */
	private boolean floatable;

	/**
	 * orientation
	 */
	private int orientation;


	//-------------------------------------------------------------
	// Initialization ---------------------------------------------
	//-------------------------------------------------------------

	/**
	 * Constructor JToolBar
	 */
	public JToolBar() {
		// TODO
	} // JToolBar()

	/**
	 * Constructor JToolBar
	 * @param orientation TODO
	 */
	public JToolBar(int orientation) {
		// TODO
	} // JToolBar()

	/**
	 * Constructor JToolBar
	 * @param name TODO
	 */
	public JToolBar(String name) {
		// TODO
	} // JToolBar()

	/**
	 * Constructor JToolBar
	 * @param name TODO
	 * @param orientation TODO
	 */
	public JToolBar(String name, int orientation) {
		// TODO
	} // JToolBar()


	//-------------------------------------------------------------
	// Methods ----------------------------------------------------
	//-------------------------------------------------------------

	/**
	 * writeObject
	 * @param stream TODO
	 * @exception IOException TODO
	 */
	private void writeObject(ObjectOutputStream stream) throws IOException {
		// TODO
	} // writeObject()

	/**
	 * add
	 * @param action TODO
	 * @returns JButton
	 */
	public JButton add(Action action) {
		return null; // TODO
	} // add()

	/**
	 * paintBorder
	 * @param graphics TODO
	 */
	protected void paintBorder(Graphics graphics) {
		// TODO
	} // paintBorder()

	/**
	 * getUI
	 * @returns ToolBarUI
	 */
	public ToolBarUI getUI() {
		return (ToolBarUI) ui;
	} // getUI()

	/**
	 * setUI
	 * @param ui TODO
	 */
	public void setUI(ToolBarUI ui) {
		super.setUI(ui);
	} // setUI()

	/**
	 * updateUI
	 */
	public void updateUI() {
		setUI((ToolBarUI) UIManager.get(this));
		invalidate();
	} // updateUI()

	/**
	 * getUIClassID
	 * @returns String
	 */
	public String getUIClassID() {
		return uiClassID;
	} // getUIClassID()

	/**
	 * getComponentIndex
	 * @param component TODO
	 * @returns int
	 */
	public int getComponentIndex(Component component) {
		return 0; // TODO
	} // getComponentIndex()

	/**
	 * getComponentAtIndex
	 * @param index TODO
	 * @returns Component
	 */
	public Component getComponentAtIndex(int index) {
		return null; // TODO
	} // getComponentAtIndex()

	/**
	 * getMargin
	 * @returns Insets
	 */
	public Insets getMargin() {
		return null; // TODO
	} // getMargin()

	/**
	 * setMargin
	 * @param margin TODO
	 */
	public void setMargin(Insets margin) {
		// TODO
	} // setMargin()

	/**
	 * isBorderPainted
	 * @returns boolean
	 */
	public boolean isBorderPainted() {
		return false; // TODO
	} // isBorderPainted()

	/**
	 * setBorderPainted
	 * @param painted TODO
	 */
	public void setBorderPainted(boolean painted) {
		// TODO
	} // setBorderPainted()

	/**
	 * isFloatable
	 * @returns boolean
	 */
	public boolean isFloatable() {
		return false; // TODO
	} // isFloatable()

	/**
	 * setFloatable
	 * @param floatable TODO
	 */
	public void setFloatable(boolean floatable) {
		// TODO
	} // setFloatable()

	/**
	 * getOrientation
	 * @returns int
	 */
	public int getOrientation() {
		return 0; // TODO
	} // getOrientation()

	/**
	 * setOrientation
	 * @param orientation TODO
	 */
	public void setOrientation(int orientation) {
		// TODO
	} // setOrientation()

	/**
	 * addSeparator
	 */
	public void addSeparator() {
		// TODO
	} // addSeparator()

	/**
	 * addSeparator
	 * @param size TODO
	 */
	public void addSeparator(Dimension size) {
		// TODO
	} // addSeparator()

	/**
	 * createActionComponent
	 * @param action TODO
	 * @returns JButton
	 */
	protected JButton createActionComponent(Action action) {
		return null; // TODO
	} // createActionComponent()

	/**
	 * createActionChangeListener
	 * @param button TODO
	 * @returns PropertyChangeListener
	 */
	protected PropertyChangeListener createActionChangeListener(JButton button) {
		return null; // TODO
	} // createActionChangeListener()

	/**
	 * addImpl
	 * @param component TODO
	 * @param constraints TODO
	 * @param index TODO
	 */
	protected void addImpl(Component component, Object constraints, int index) {
		// TODO
	} // addImpl()

	/**
	 * paramString
	 * @returns String
	 */
	protected String paramString() {
		return null; // TODO
	} // paramString()

	/**
	 * getAccessibleContext
	 * @returns AccessibleContext
	 */
	public AccessibleContext getAccessibleContext() {
		if (accessibleContext == null) {
			accessibleContext = new AccessibleJToolBar(this);
		} // if
		return accessibleContext;
	} // getAccessibleContext()


} // JToolBar
