/* JMenu.java --
   Copyright (C) 2002, 2004 Free Software Foundation, Inc.

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
import java.awt.Point;
import java.awt.event.KeyEvent;
import java.awt.event.WindowAdapter;
import java.awt.event.WindowEvent;
import java.beans.PropertyChangeListener;
import java.io.IOException;
import java.io.ObjectOutputStream;
import java.io.Serializable;
import java.util.Hashtable;
import javax.accessibility.Accessible;
import javax.accessibility.AccessibleContext;
import javax.accessibility.AccessibleRole;
import javax.accessibility.AccessibleSelection;
import javax.swing.event.MenuEvent;
import javax.swing.event.MenuListener;
import javax.swing.event.ChangeListener;

/**
 * JMenu
 * @author	Andrew Selkirk
 * @version	1.0
 */
public class JMenu
  extends JMenuItem
  implements Accessible, MenuElement
{
  static final long serialVersionUID = 4227225638931828014L;

	//-------------------------------------------------------------
	// Classes ----------------------------------------------------
	//-------------------------------------------------------------

	/**
	 * AccessibleJMenu
	 */
	protected class AccessibleJMenu extends AccessibleJMenuItem 
			implements AccessibleSelection {

		//-------------------------------------------------------------
		// Variables --------------------------------------------------
		//-------------------------------------------------------------


		//-------------------------------------------------------------
		// Initialization ---------------------------------------------
		//-------------------------------------------------------------

		/**
		 * Constructor AccessibleJMenu
		 * @param component TODO
		 */
		protected AccessibleJMenu(JMenu component) {
			super(component);
			// TODO
		} // AccessibleJMenu()


		//-------------------------------------------------------------
		// Methods ----------------------------------------------------
		//-------------------------------------------------------------

		/**
		 * getAccessibleChildrenCount
		 * @returns int
		 */
		public int getAccessibleChildrenCount() {
			return 0; // TODO
		} // getAccessibleChildrenCount()

		/**
		 * getAccessibleChild
		 * @param value0 TODO
		 * @returns Accessible
		 */
		public Accessible getAccessibleChild(int value0) {
			return null; // TODO
		} // getAccessibleChild()

		/**
		 * getAccessibleSelection
		 * @returns AccessibleSelection
		 */
		public AccessibleSelection getAccessibleSelection() {
			return null; // TODO
		} // getAccessibleSelection()

		/**
		 * getAccessibleSelection
		 * @param value0 TODO
		 * @returns Accessible
		 */
		public Accessible getAccessibleSelection(int value0) {
			return null; // TODO
		} // getAccessibleSelection()

		/**
		 * isAccessibleChildSelected
		 * @param value0 TODO
		 * @returns boolean
		 */
		public boolean isAccessibleChildSelected(int value0) {
			return false; // TODO
		} // isAccessibleChildSelected()

		/**
		 * getAccessibleRole
		 * @returns AccessibleRole
		 */
		public AccessibleRole getAccessibleRole() {
			return AccessibleRole.MENU;
		} // getAccessibleRole()

		/**
		 * getAccessibleSelectionCount
		 * @returns int
		 */
		public int getAccessibleSelectionCount() {
			return 0; // TODO
		} // getAccessibleSelectionCount()

		/**
		 * addAccessibleSelection
		 * @param value0 TODO
		 */
		public void addAccessibleSelection(int value0) {
			// TODO
		} // addAccessibleSelection()

		/**
		 * removeAccessibleSelection
		 * @param value0 TODO
		 */
		public void removeAccessibleSelection(int value0) {
			// TODO
		} // removeAccessibleSelection()

		/**
		 * clearAccessibleSelection
		 */
		public void clearAccessibleSelection() {
			// TODO
		} // clearAccessibleSelection()

		/**
		 * selectAllAccessibleSelection
		 */
		public void selectAllAccessibleSelection() {
			// TODO
		} // selectAllAccessibleSelection()


	} // AccessibleJMenu

	/**
	 * WinListener
	 */
	protected class WinListener extends WindowAdapter implements Serializable {

		//-------------------------------------------------------------
		// Variables --------------------------------------------------
		//-------------------------------------------------------------

		/**
		 * popupMenu
		 */
		JPopupMenu popupMenu;


		//-------------------------------------------------------------
		// Initialization ---------------------------------------------
		//-------------------------------------------------------------

		/**
		 * Constructor WinListener
		 * @param value0 TODO
		 * @param value1 TODO
		 */
		public WinListener(JMenu value0, JPopupMenu value1) {
			// TODO
		} // WinListener()


		//-------------------------------------------------------------
		// Methods ----------------------------------------------------
		//-------------------------------------------------------------

		/**
		 * windowClosing
		 * @param value0 TODO
		 */
		public void windowClosing(WindowEvent value0) {
			// TODO
		} // windowClosing()


	} // WinListener


	//-------------------------------------------------------------
	// Variables --------------------------------------------------
	//-------------------------------------------------------------

	/**
	 * uiClassID
	 */
	private static final String uiClassID = "MenuUI";

	/**
	 * popupMenu
	 */
	private JPopupMenu popupMenu;

	/**
	 * menuChangeListener
	 */
	private ChangeListener menuChangeListener;

	/**
	 * menuEvent
	 */
	private MenuEvent menuEvent;

	/**
	 * listenerRegistry
	 */
	private static Hashtable listenerRegistry = null; // TODO

	/**
	 * delay
	 */
	private int delay;

	/**
	 * TRACE
	 */
	private static final boolean TRACE = false; // TODO

	/**
	 * VERBOSE
	 */
	private static final boolean VERBOSE = false; // TODO

	/**
	 * DEBUG
	 */
	private static final boolean DEBUG = false; // TODO

	/**
	 * popupListener
	 */
	protected JMenu.WinListener popupListener;


	//-------------------------------------------------------------
	// Initialization ---------------------------------------------
	//-------------------------------------------------------------

	/**
	 * Constructor JMenu
	 */
	public JMenu() {
		// TODO
	} // JMenu()

	/**
	 * Constructor JMenu
	 * @param text TODO
	 */
	public JMenu(String text) {
		// TODO
	} // JMenu()

	/**
	 * Constructor JMenu
	 * @param action TODO
	 */
	public JMenu(Action action) {
		// TODO
	} // JMenu()

	/**
	 * Constructor JMenu
	 * @param text TODO
	 * @param tearoff TODO
	 */
	public JMenu(String text, boolean tearoff) {
		// TODO
	} // JMenu()


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
	 * @param value0 TODO
	 * @returns JMenuItem
	 */
	public JMenuItem add(JMenuItem item) {
		return null; // TODO
	} // add()

	/**
	 * add
	 * @param component TODO
	 * @returns Component
	 */
	public Component add(Component component) {
		return null; // TODO
	} // add()

	/**
	 * add
	 * @param component TODO
	 * @param index TODO
	 * @returns Component
	 */
	public Component add(Component component, int index) {
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
	 * @param item TODO
	 */
	public void remove(JMenuItem item) {
		// TODO
	} // remove()

	/**
	 * remove
	 * @param index TODO
	 */
	public void remove(int index) {
		// TODO
	} // remove()

	/**
	 * remove
	 * @param component TODO
	 */
	public void remove(Component component) {
		// TODO
	} // remove()

	/**
	 * removeAll
	 */
	public void removeAll() {
		// TODO
	} // removeAll()

	/**
	 * insert
	 * @param text TODO
	 * @param index TODO
	 */
	public void insert(String text, int index) {
		// TODO
	} // insert()

	/**
	 * insert
	 * @param item TODO
	 * @param index TODO
	 * @returns JMenuItem
	 */
	public JMenuItem insert(JMenuItem item, int index) {
		return null; // TODO
	} // insert()

	/**
	 * insert
	 * @param action TODO
	 * @param index TODO
	 * @returns JMenuItem
	 */
	public JMenuItem insert(Action action, int index) {
		return null; // TODO
	} // insert()

	/**
	 * updateUI
	 */
	public void updateUI() {
		//setUI((MenuUI) UIManager.get(this));
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
	 * setModel
	 * @param model TODO
	 */
	public void setModel(ButtonModel model) {
		// TODO
	} // setModel()

	/**
	 * isSelected
	 * @returns boolean
	 */
	public boolean isSelected() {
		return false; // TODO
	} // isSelected()

	/**
	 * setSelected
	 * @param selected TODO
	 */
	public void setSelected(boolean selected) {
		// TODO
	} // setSelected()

	/**
	 * isPopupMenuVisible
	 * @returns boolean
	 */
	public boolean isPopupMenuVisible() {
		return false; // TODO
	} // isPopupMenuVisible()

	/**
	 * setPopupMenuVisible
	 * @param popup TODO
	 */
	public void setPopupMenuVisible(boolean popup) {
		// TODO
	} // setPopupMenuVisible()

	/**
	 * getPopupMenuOrigin
	 * @returns Point
	 */
	protected Point getPopupMenuOrigin() {
		return null; // TODO
	} // getPopupMenuOrigin()

	/**
	 * getDelay
	 * @returns int
	 */
	public int getDelay() {
		return 0; // TODO
	} // getDelay()

	/**
	 * setDelay
	 * @param value0 TODO
	 */
	public void setDelay(int delay) {
		// TODO
	} // setDelay()

	/**
	 * setMenuLocation
	 * @param x TODO
	 * @param y TODO
	 */
	public void setMenuLocation(int x, int y) {
		// TODO
	} // setMenuLocation()

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
	 * addSeparator
	 */
	public void addSeparator() {
		// TODO
	} // addSeparator()

	/**
	 * insertSeparator
	 * @param index TODO
	 */
	public void insertSeparator(int index) {
		// TODO
	} // insertSeparator()

	/**
	 * getItem
	 * @param index TODO
	 * @returns JMenuItem
	 */
	public JMenuItem getItem(int index) {
		return null; // TODO
	} // getItem()

	/**
	 * getItemCount
	 * @returns int
	 */
	public int getItemCount() {
		return 0; // TODO
	} // getItemCount()

	/**
	 * isTearOff
	 * @returns boolean
	 */
	public boolean isTearOff() {
		return false; // TODO
	} // isTearOff()

	/**
	 * getMenuComponentCount
	 * @returns int
	 */
	public int getMenuComponentCount() {
		return 0; // TODO
	} // getMenuComponentCount()

	/**
	 * getMenuComponent
	 * @param index TODO
	 * @returns Component
	 */
	public Component getMenuComponent(int index) {
		return null; // TODO
	} // getMenuComponent()

	/**
	 * getMenuComponents
	 * @returns Component[]
	 */
	public Component[] getMenuComponents() {
		return null; // TODO
	} // getMenuComponents()

	/**
	 * isTopLevelMenu
	 * @returns boolean
	 */
	public boolean isTopLevelMenu() {
		return false; // TODO
	} // isTopLevelMenu()

	/**
	 * isMenuComponent
	 * @param component TODO
	 * @returns boolean
	 */
	public boolean isMenuComponent(Component component) {
		return false; // TODO
	} // isMenuComponent()

	/**
	 * getPopupMenu
	 * @returns JPopupMenu
	 */
	public JPopupMenu getPopupMenu() {
		return null; // TODO
	} // getPopupMenu()

	/**
	 * addMenuListener
	 * @param listener TODO
	 */
	public void addMenuListener(MenuListener listener) {
		// TODO
	} // addMenuListener()

	/**
	 * removeMenuListener
	 * @param listener TODO
	 */
	public void removeMenuListener(MenuListener listener) {
		// TODO
	} // removeMenuListener()

	/**
	 * fireMenuSelected
	 */
	protected void fireMenuSelected() {
		// TODO
	} // fireMenuSelected()

	/**
	 * fireMenuDeselected
	 */
	protected void fireMenuDeselected() {
		// TODO
	} // fireMenuDeselected()

	/**
	 * fireMenuCanceled
	 */
	protected void fireMenuCanceled() {
		// TODO
	} // fireMenuCanceled()

	/**
	 * createMenuChangeListener
	 * @returns ChangeListener
	 */
	private ChangeListener createMenuChangeListener() {
		return null; // TODO
	} // createMenuChangeListener()

	/**
	 * createWinListener
	 * @param popup TODO
	 * @returns JMenu.WinListener
	 */
	protected JMenu.WinListener createWinListener(JPopupMenu popup) {
		return null; // TODO
	} // createWinListener()

	/**
	 * menuSelectionChanged
	 * @param value0 TODO
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
	 * setAccelerator
	 * @param keystroke TODO
	 */
	public void setAccelerator(KeyStroke keystroke) {
		// TODO
	} // setAccelerator()

	/**
	 * processKeyEvent
	 * @param event TODO
	 */
	protected void processKeyEvent(KeyEvent event) {
		// TODO
	} // processKeyEvent()

	/**
	 * doClick
	 * @param time TODO
	 */
	public void doClick(int time) {
		// TODO
	} // doClick()

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
			accessibleContext = new AccessibleJMenu(this);
		} // if
		return accessibleContext;
	} // getAccessibleContext()


} // JMenu
