/* JMenuItem.java --
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

import java.awt.Component;
import java.awt.event.KeyEvent;
import java.awt.event.MouseEvent;
import java.beans.PropertyChangeListener;
import java.io.IOException;
import java.io.ObjectInputStream;
import java.io.ObjectOutputStream;
import javax.accessibility.Accessible;
import javax.accessibility.AccessibleContext;
import javax.accessibility.AccessibleRole;
import javax.swing.event.ChangeEvent;
import javax.swing.event.ChangeListener;
import javax.swing.event.MenuDragMouseEvent;
import javax.swing.event.MenuDragMouseListener;
import javax.swing.event.MenuKeyEvent;
import javax.swing.event.MenuKeyListener;
import javax.swing.plaf.MenuItemUI;

/**
 * JMenuItem
 * @author	Andrew Selkirk
 * @version	1.0
 */
public class JMenuItem extends AbstractButton implements Accessible, MenuElement {

	//-------------------------------------------------------------
	// Classes ----------------------------------------------------
	//-------------------------------------------------------------

	/**
	 * AccessibleJMenuItem
	 */
	protected class AccessibleJMenuItem extends AccessibleAbstractButton 
			implements ChangeListener {

		//-------------------------------------------------------------
		// Variables --------------------------------------------------
		//-------------------------------------------------------------


		//-------------------------------------------------------------
		// Initialization ---------------------------------------------
		//-------------------------------------------------------------

		/**
		 * Constructor AccessibleJMenuItem
		 * @param component TODO
		 */
		AccessibleJMenuItem(JMenuItem component) {
			super(component);
			// TODO
		} // AccessibleJMenuItem()


		//-------------------------------------------------------------
		// Methods ----------------------------------------------------
		//-------------------------------------------------------------

		/**
		 * stateChanged
		 * @param event TODO
		 */
		public void stateChanged(ChangeEvent event) {
			// TODO
		} // stateChanged()

		/**
		 * getAccessibleRole
		 * @returns AccessibleRole
		 */
		public AccessibleRole getAccessibleRole() {
			return AccessibleRole.MENU_ITEM;
		} // getAccessibleRole()


	} // AccessibleJMenuItem


	//-------------------------------------------------------------
	// Variables --------------------------------------------------
	//-------------------------------------------------------------

	/**
	 * uiClassID
	 */
	private static final String uiClassID = "MenuItemUI";

	/**
	 * accelerator
	 */
	private KeyStroke accelerator;


	//-------------------------------------------------------------
	// Initialization ---------------------------------------------
	//-------------------------------------------------------------

	/**
	 * Constructor JMenuItem
	 */
	public JMenuItem() {
		// TODO
	} // JMenuItem()

	/**
	 * Constructor JMenuItem
	 * @param icon TODO
	 */
	public JMenuItem(Icon icon) {
		// TODO
	} // JMenuItem()

	/**
	 * Constructor JMenuItem
	 * @param text TODO
	 */
	public JMenuItem(String text) {
		// TODO
	} // JMenuItem()

	/**
	 * Constructor JMenuItem
	 * @param action TODO
	 */
	public JMenuItem(Action action) {
		// TODO
	} // JMenuItem()

	/**
	 * Constructor JMenuItem
	 * @param text TODO
	 * @param icon TODO
	 */
	public JMenuItem(String text, Icon icon) {
		// TODO
	} // JMenuItem()

	/**
	 * Constructor JMenuItem
	 * @param text TODO
	 * @param mnemonic TODO
	 */
	public JMenuItem(String text, int mnemonic) {
		// TODO
	} // JMenuItem()


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
	 * init
	 * @param text TODO
	 * @param icon TODO
	 */
	protected void init(String text, Icon icon) {
		// TODO
	} // init()

	/**
	 * setUI
	 * @param ui TODO
	 */
	public void setUI(MenuItemUI ui) {
		super.setUI(ui);
		// TODO
	} // setUI()

	/**
	 * updateUI
	 */
	public void updateUI() {
		setUI((MenuItemUI) UIManager.get(this));
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
	 * isArmed
	 * @returns boolean
	 */
	public boolean isArmed() {
		return false; // TODO
	} // isArmed()

	/**
	 * setArmed
	 * @param armed TODO
	 */
	public void setArmed(boolean armed) {
		// TODO
	} // setArmed()

	/**
	 * setEnabled
	 * @param enabled TODO
	 */
	public void setEnabled(boolean enabled) {
		// TODO
	} // setEnabled()

	/**
	 * getAccelerator
	 * @returns KeyStroke
	 */
	public KeyStroke getAccelerator() {
		return null; // TODO
	} // getAccelerator()

	/**
	 * setAccelerator
	 * @param keystroke TODO
	 */
	public void setAccelerator(KeyStroke keystroke) {
		// TODO
	} // setAccelerator()

	/**
	 * configurePropertiesFromAction
	 * @param action TODO
	 */
	protected void configurePropertiesFromAction(Action action) {
		// TODO
	} // configurePropertiesFromAction()

	/**
	 * createActionPropertyChangeListener
	 * @param action TODO
	 * @returns PropertyChangeListener
	 */
	protected PropertyChangeListener createActionPropertyChangeListener(Action action) {
		return null; // TODO
	} // createActionPropertyChangeListener()

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
	 * processMenuDragMouseEvent
	 * @param event TODO
	 */
	public void processMenuDragMouseEvent(MenuDragMouseEvent event) {
		// TODO
	} // processMenuDragMouseEvent()

	/**
	 * processMenuKeyEvent
	 * @param event TODO
	 */
	public void processMenuKeyEvent(MenuKeyEvent event) {
		// TODO
	} // processMenuKeyEvent()

	/**
	 * fireMenuDragMouseEntered
	 * @param event TODO
	 */
	protected void fireMenuDragMouseEntered(MenuDragMouseEvent event) {
		// TODO
	} // fireMenuDragMouseEntered()

	/**
	 * fireMenuDragMouseExited
	 * @param event TODO
	 */
	protected void fireMenuDragMouseExited(MenuDragMouseEvent event) {
		// TODO
	} // fireMenuDragMouseExited()

	/**
	 * fireMenuDragMouseDragged
	 * @param event TODO
	 */
	protected void fireMenuDragMouseDragged(MenuDragMouseEvent event) {
		// TODO
	} // fireMenuDragMouseDragged()

	/**
	 * fireMenuDragMouseReleased
	 * @param event TODO
	 */
	protected void fireMenuDragMouseReleased(MenuDragMouseEvent event) {
		// TODO
	} // fireMenuDragMouseReleased()

	/**
	 * fireMenuKeyPressed
	 * @param event TODO
	 */
	protected void fireMenuKeyPressed(MenuKeyEvent event) {
		// TODO
	} // fireMenuKeyPressed()

	/**
	 * fireMenuKeyReleased
	 * @param event TODO
	 */
	protected void fireMenuKeyReleased(MenuKeyEvent event) {
		// TODO
	} // fireMenuKeyReleased()

	/**
	 * fireMenuKeyTyped
	 * @param event TODO
	 */
	protected void fireMenuKeyTyped(MenuKeyEvent event) {
		// TODO
	} // fireMenuKeyTyped()

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
	 * addMenuDragMouseListener
	 * @param listener TODO
	 */
	public void addMenuDragMouseListener(MenuDragMouseListener listener) {
		// TODO
	} // addMenuDragMouseListener()

	/**
	 * removeMenuDragMouseListener
	 * @param listener TODO
	 */
	public void removeMenuDragMouseListener(MenuDragMouseListener listener) {
		// TODO
	} // removeMenuDragMouseListener()

	/**
	 * addMenuKeyListener
	 * @param listener TODO
	 */
	public void addMenuKeyListener(MenuKeyListener listener) {
		// TODO
	} // addMenuKeyListener()

	/**
	 * removeMenuKeyListener
	 * @param listener TODO
	 */
	public void removeMenuKeyListener(MenuKeyListener listener) {
		// TODO
	} // removeMenuKeyListener()

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
			accessibleContext = new AccessibleJMenuItem(this);
		} // if
		return accessibleContext;
	} // getAccessibleContext()


} // JMenuItem
