/* JPopupMenu.java --
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
import java.awt.event.*;
import java.beans.*;
import java.io.*;
import javax.accessibility.*;
import javax.swing.event.*;
import javax.swing.plaf.*;

/**
 * JPopupMenu
 * @author	Andrew Selkirk
 * @version	1.0
 */
public class JPopupMenu extends JComponent implements Accessible, MenuElement {

	//-------------------------------------------------------------
	// Classes ----------------------------------------------------
	//-------------------------------------------------------------

	/**
	 * Separator
	 */
	public static class Separator extends JSeparator {

		//-------------------------------------------------------------
		// Initialization ---------------------------------------------
		//-------------------------------------------------------------

		/**
		 * Constructor Separator
		 */
		public Separator() {
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


	} // Separator

	/**
	 * AccessibleJPopupMenu
	 */
	protected class AccessibleJPopupMenu extends AccessibleJComponent {

		//-------------------------------------------------------------
		// Variables --------------------------------------------------
		//-------------------------------------------------------------


		//-------------------------------------------------------------
		// Initialization ---------------------------------------------
		//-------------------------------------------------------------

		/**
		 * Constructor AccessibleJPopupMenu
		 * @param component TODO
		 */
		protected AccessibleJPopupMenu(JPopupMenu component) {
			super(component);
			// TODO
		} // AccessibleJPopupMenu()


		//-------------------------------------------------------------
		// Methods ----------------------------------------------------
		//-------------------------------------------------------------

		/**
		 * getAccessibleRole
		 * @returns AccessibleRole
		 */
		public AccessibleRole getAccessibleRole() {
			return AccessibleRole.POPUP_MENU;
		} // getAccessibleRole()


	} // AccessibleJPopupMenu


	//-------------------------------------------------------------
	// Variables --------------------------------------------------
	//-------------------------------------------------------------

	/**
	 * uiClassID
	 */
	private static final String uiClassID = "PopupMenuUI";

	/**
	 * invoker
	 */
	transient Component invoker;

	/**
	 * desiredLocationX
	 */
	private int desiredLocationX;

	/**
	 * desiredLocationY
	 */
	private int desiredLocationY;

	/**
	 * label
	 */
	private String label;

	/**
	 * paintBorder
	 */
	private boolean paintBorder;

	/**
	 * margin
	 */
	private Insets margin;

	/**
	 * defaultLWPopupEnabledKey
	 */
	private static final Object defaultLWPopupEnabledKey = null; // TODO

	/**
	 * lightWeightPopupEnabled
	 */
	private boolean lightWeightPopupEnabled;

	/**
	 * selectionModel
	 */
	private SingleSelectionModel selectionModel;


	//-------------------------------------------------------------
	// Initialization ---------------------------------------------
	//-------------------------------------------------------------

	/**
	 * Constructor JPopupMenu
	 */
	public JPopupMenu() {
		// TODO
	} // JPopupMenu()

	/**
	 * Constructor JPopupMenu
	 * @param label TODO
	 */
	public JPopupMenu(String label) {
		// TODO
	} // JPopupMenu()


	//-------------------------------------------------------------
	// Methods ----------------------------------------------------
	//-------------------------------------------------------------

	/**
	 * readObject
	 * @param stream TODO
	 * @exception IOException TODO
	 * @exception ClassNotFoundException TODO
	 */
	private void readObject(ObjectInputStream stream) 
			throws IOException, ClassNotFoundException {
		// TODO
	} // readObject()

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
	 * @param item TODO
	 * @returns JMenuItem
	 */
	public JMenuItem add(JMenuItem item) {
		return null; // TODO
	} // add()

	/**
	 * add
	 * @param text TODO
	 * @returns JMenuItem
	 */
	public JMenuItem add(String text) {
		return null; // TODO
	} // add()

	/**
	 * add
	 * @param action TODO
	 * @returns JMenuItem
	 */
	public JMenuItem add(Action action) {
		return null; // TODO
	} // add()

	/**
	 * remove
	 * @param index TODO
	 */
	public void remove(int index) {
		// TODO
	} // remove()

	/**
	 * insert
	 * @param action TODO
	 * @param index TODO
	 */
	public void insert(Action action, int index) {
		// TODO
	} // insert()

	/**
	 * insert
	 * @param component TODO
	 * @param index TODO
	 */
	public void insert(Component component, int index) {
		// TODO
	} // insert()

	/**
	 * paintBorder
	 * @param graphics TODO
	 */
	protected void paintBorder(Graphics graphics) {
		// TODO
	} // paintBorder()

	/**
	 * getDefaultLightWeightPopupEnabled
	 * @returns boolean
	 */
	public static boolean getDefaultLightWeightPopupEnabled() {
		return false; // TODO
	} // getDefaultLightWeightPopupEnabled()

	/**
	 * setDefaultLightWeightPopupEnabled
	 * @param enabled TODO
	 */
	public static void setDefaultLightWeightPopupEnabled(boolean enabled) {
		// TODO
	} // setDefaultLightWeightPopupEnabled()

	/**
	 * getUI
	 * @returns PopupMenuUI
	 */
	public PopupMenuUI getUI() {
		return (PopupMenuUI) ui;
	} // getUI()

	/**
	 * setUI
	 * @param ui TODO
	 */
	public void setUI(PopupMenuUI ui) {
		super.setUI(ui);
		// TODO
	} // setUI()

	/**
	 * updateUI
	 */
	public void updateUI() {
		setUI((PopupMenuUI) UIManager.get(this));
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
	 * getSelectionModel
	 * @returns SingleSelectionModel
	 */
	public SingleSelectionModel getSelectionModel() {
		return null; // TODO
	} // getSelectionModel()

	/**
	 * setSelectionModel
	 * @param model TODO
	 */
	public void setSelectionModel(SingleSelectionModel model) {
		// TODO
	} // setSelectionModel()

	/**
	 * createActionComponent
	 * @param action TODO
	 * @returns JMenuItem
	 */
	protected JMenuItem createActionComponent(Action action) {
		return null; // TODO
	} // createActionComponent()

	/**
	 * createActionChangeListener
	 * @param item TODO
	 * @returns PropertyChangeListener
	 */
	protected PropertyChangeListener createActionChangeListener(JMenuItem item) {
		return null; // TODO
	} // createActionChangeListener()

	/**
	 * isLightWeightPopupEnabled
	 * @returns boolean
	 */
	public boolean isLightWeightPopupEnabled() {
		return false; // TODO
	} // isLightWeightPopupEnabled()

	/**
	 * setLightWeightPopupEnabled
	 * @param enabled TODO
	 */
	public void setLightWeightPopupEnabled(boolean enabled) {
		// TODO
	} // setLightWeightPopupEnabled()

	/**
	 * getLabel
	 * @returns String
	 */
	public String getLabel() {
		return null; // TODO
	} // getLabel()

	/**
	 * setLabel
	 * @param label TODO
	 */
	public void setLabel(String label) {
		// TODO
	} // setLabel()

	/**
	 * addSeparator
	 */
	public void addSeparator() {
		// TODO
	} // addSeparator()

	/**
	 * addPopupMenuListener
	 * @param listener TODO
	 */
	public void addPopupMenuListener(PopupMenuListener listener) {
		// TODO
	} // addPopupMenuListener()

	/**
	 * removePopupMenuListener
	 * @param listener TODO
	 */
	public void removePopupMenuListener(PopupMenuListener listener) {
		// TODO
	} // removePopupMenuListener()

	/**
	 * firePopupMenuWillBecomeVisible
	 */
	protected void firePopupMenuWillBecomeVisible() {
		// TODO
	} // firePopupMenuWillBecomeVisible()

	/**
	 * firePopupMenuWillBecomeInvisible
	 */
	protected void firePopupMenuWillBecomeInvisible() {
		// TODO
	} // firePopupMenuWillBecomeInvisible()

	/**
	 * firePopupMenuCanceled
	 */
	protected void firePopupMenuCanceled() {
		// TODO
	} // firePopupMenuCanceled()

	/**
	 * pack
	 */
	public void pack() {
		// TODO
	} // pack()

	/**
	 * isVisible
	 * @returns boolean
	 */
	public boolean isVisible() {
		return false; // TODO
	} // isVisible()

	/**
	 * setVisible
	 * @param visible TODO
	 */
	public void setVisible(boolean visible) {
		// TODO
	} // setVisible()

	/**
	 * setLocation
	 * @param x TODO
	 * @param y TODO
	 */
	public void setLocation(int x, int y) {
		// TODO
	} // setLocation()

	/**
	 * isPopupMenu
	 * @returns boolean
	 */
	private boolean isPopupMenu() {
		return false; // TODO
	} // isPopupMenu()

	/**
	 * getInvoker
	 * @returns Component
	 */
	public Component getInvoker() {
		return null; // TODO
	} // getInvoker()

	/**
	 * setInvoker
	 * @param component TODO
	 */
	public void setInvoker(Component component) {
		// TODO
	} // setInvoker()

	/**
	 * show
	 * @param component TODO
	 * @param x TODO
	 * @param y TODO
	 */
	public void show(Component component, int x, int y) {
		// TODO
	} // show()

	/**
	 * getRootPopupMenu
	 * @returns JPopupMenu
	 */
	JPopupMenu getRootPopupMenu() {
		return null; // TODO
	} // getRootPopupMenu()

	/**
	 * getComponentAtIndex
	 * @param index TODO
	 * @returns Component
	 */
	public Component getComponentAtIndex(int index) {
		return null; // TODO
	} // getComponentAtIndex()

	/**
	 * getComponentIndex
	 * @param component TODO
	 * @returns int
	 */
	public int getComponentIndex(Component component) {
		return 0; // TODO
	} // getComponentIndex()

	/**
	 * setPopupSize
	 * @param size TODO
	 */
	public void setPopupSize(Dimension size) {
		// TODO
	} // setPopupSize()

	/**
	 * setPopupSize
	 * @param x TODO
	 * @param y TODO
	 */
	public void setPopupSize(int x, int y) {
		// TODO
	} // setPopupSize()

	/**
	 * setSelected
	 * @param selected TODO
	 */
	public void setSelected(Component selected) {
		// TODO
	} // setSelected()

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
	 * getMargin
	 * @returns Insets
	 */
	public Insets getMargin() {
		return null; // TODO
	} // getMargin()

	/**
	 * paramString
	 * @returns String
	 */
	protected String paramString() {
		return null; // TODO
	} // paramString()

	/**
	 * processMouseEvent
	 * @param event TODO
	 * @param path TODO
	 * @param manager TODO
	 */
	public void processMouseEvent(MouseEvent event, MenuElement[] path,
			MenuSelectionManager manager) {
		// TODO
	} // processMouseEvent()

	/**
	 * processKeyEvent
	 * @param event TODO
	 * @param path TODO
	 * @param manager TODO
	 */
	public void processKeyEvent(KeyEvent event, MenuElement[] path,
			MenuSelectionManager manager) {
		// TODO
	} // processKeyEvent()

	/**
	 * menuSelectionChanged
	 * @param changed TODO
	 */
	public void menuSelectionChanged(boolean changed) {
		// TODO
	} // menuSelectionChanged()

	/**
	 * getSubElements
	 * @returns MenuElement[]
	 */
	public MenuElement[] getSubElements() {
		return null; // TODO
	} // getSubElements()

	/**
	 * getComponent
	 * @returns Component
	 */
	public Component getComponent() {
		return null; // TODO
	} // getComponent()

	/**
	 * isPopupTrigger
	 * @param event TODO
	 * @returns boolean
	 */
	public boolean isPopupTrigger(MouseEvent event) {
		return false; // TODO
	} // isPopupTrigger()

	/**
	 * getAccessibleContext
	 * @returns AccessibleContext
	 */
	public AccessibleContext getAccessibleContext() {
		if (accessibleContext == null) {
			accessibleContext = new AccessibleJPopupMenu(this);
		} // if
		return accessibleContext;
	} // getAccessibleContext()


} // JPopupMenu
