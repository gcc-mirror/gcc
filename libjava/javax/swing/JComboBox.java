/* JComboBox.java --
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

import java.awt.ItemSelectable;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.ItemEvent;
import java.awt.event.ItemListener;
import java.awt.event.KeyEvent;
import java.beans.PropertyChangeListener;
import java.io.IOException;
import java.io.ObjectOutputStream;
import java.util.Vector;

import javax.accessibility.Accessible;
import javax.accessibility.AccessibleAction;
import javax.accessibility.AccessibleContext;
import javax.accessibility.AccessibleRole;
import javax.accessibility.AccessibleSelection;
import javax.swing.event.ListDataEvent;
import javax.swing.event.ListDataListener;
import javax.swing.event.PopupMenuListener;
import javax.swing.plaf.ComboBoxUI;

/**
 * JComboBox
 * @author	Andrew Selkirk
 * @version	1.0
 */
public class JComboBox extends JComponent
  implements ItemSelectable, ListDataListener, ActionListener, Accessible
{
  private static final long serialVersionUID = 5654585963292734470L;

  /**
   * AccessibleJComboBox
   */
  protected class AccessibleJComboBox extends AccessibleJComponent 
    implements AccessibleAction, AccessibleSelection
  {
    private static final long serialVersionUID = 8217828307256675666L;

    /**
     * Constructor AccessibleJComboBox
     * @param component TODO
     */
    protected AccessibleJComboBox()
    {
    }

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
			return AccessibleRole.COMBO_BOX;
		} // getAccessibleRole()

		/**
		 * getAccessibleAction
		 * @returns AccessibleAction
		 */
		public AccessibleAction getAccessibleAction() {
			return null; // TODO
		} // getAccessibleAction()

		/**
		 * getAccessibleActionDescription
		 * @param value0 TODO
		 * @returns String
		 */
		public String getAccessibleActionDescription(int value0) {
			return null; // TODO
		} // getAccessibleActionDescription()

		/**
		 * getAccessibleActionCount
		 * @returns int
		 */
		public int getAccessibleActionCount() {
			return 0; // TODO
		} // getAccessibleActionCount()

		/**
		 * doAccessibleAction
		 * @param value0 TODO
		 * @returns boolean
		 */
		public boolean doAccessibleAction(int value0) {
			return false; // TODO
		} // doAccessibleAction()

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


	} // AccessibleJComboBox

	/**
	 * KeySelectionManager
	 */
	public static interface KeySelectionManager {

		//-------------------------------------------------------------
		// Methods ----------------------------------------------------
		//-------------------------------------------------------------

		/**
		 * selectionForKey
		 * @param value0 TODO
		 * @param value1 TODO
		 * @returns int
		 */
		int selectionForKey(char value0, ComboBoxModel value1);


	} // KeySelectionManager


	//-------------------------------------------------------------
	// Variables --------------------------------------------------
	//-------------------------------------------------------------

	/**
	 * uiClassID
	 */
	private static final String uiClassID = "ComboBoxUI";

	/**
	 * dataModel
	 */
	protected ComboBoxModel dataModel;

	/**
	 * renderer
	 */
	protected ListCellRenderer renderer;

	/**
	 * editor
	 */
	protected ComboBoxEditor editor;

	/**
	 * maximumRowCount
	 */
	protected int maximumRowCount;

	/**
	 * isEditable
	 */
	protected boolean isEditable;

	/**
	 * selectedItemReminder
	 */
	protected Object selectedItemReminder;

	/**
	 * keySelectionManager
	 */
	protected JComboBox.KeySelectionManager keySelectionManager;

	/**
	 * actionCommand
	 */
	protected String actionCommand;

	/**
	 * lightWeightPopupEnabled
	 */
	protected boolean lightWeightPopupEnabled;


	//-------------------------------------------------------------
	// Initialization ---------------------------------------------
	//-------------------------------------------------------------

	/**
	 * Constructor JComboBox
	 * @param value0 TODO
	 */
	public JComboBox(ComboBoxModel value0) {
		// TODO
	} // JComboBox()

	/**
	 * Constructor JComboBox
	 * @param value0 TODO
	 */
	public JComboBox(Object[] value0) {
		// TODO
	} // JComboBox()

	/**
	 * Constructor JComboBox
	 * @param value0 TODO
	 */
	public JComboBox(Vector value0) {
		// TODO
	} // JComboBox()

	/**
	 * Constructor JComboBox
	 */
	public JComboBox() {
		// TODO
	} // JComboBox()


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
	 * isEditable
	 * @returns boolean
	 */
	public boolean isEditable() {
		return false; // TODO
	} // isEditable()

	/**
	 * installAncestorListener
	 */
	protected void installAncestorListener() {
		// TODO
	} // installAncestorListener()

	/**
	 * setUI
	 * @param ui TODO
	 */
	public void setUI(ComboBoxUI ui) {
		super.setUI(ui);
	} // setUI()

	/**
	 * updateUI
	 */
	public void updateUI() {
		setUI((ComboBoxUI) UIManager.get(this));
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
	 * getUI
	 * @returns ComboBoxUI
	 */
	public ComboBoxUI getUI() {
		return (ComboBoxUI) ui;
	} // getUI()

	/**
	 * setModel
	 * @param value0 TODO
	 */
	public void setModel(ComboBoxModel value0) {
		// TODO
	} // setModel()

	/**
	 * getModel
	 * @returns ComboBoxModel
	 */
	public ComboBoxModel getModel() {
		return null; // TODO
	} // getModel()

	/**
	 * setLightWeightPopupEnabled
	 * @param value0 TODO
	 */
	public void setLightWeightPopupEnabled(boolean value0) {
		// TODO
	} // setLightWeightPopupEnabled()

	/**
	 * isLightWeightPopupEnabled
	 * @returns boolean
	 */
	public boolean isLightWeightPopupEnabled() {
		return false; // TODO
	} // isLightWeightPopupEnabled()

	/**
	 * setEditable
	 * @param value0 TODO
	 */
	public void setEditable(boolean value0) {
		// TODO
	} // setEditable()

	/**
	 * setMaximumRowCount
	 * @param value0 TODO
	 */
	public void setMaximumRowCount(int value0) {
		// TODO
	} // setMaximumRowCount()

	/**
	 * getMaximumRowCount
	 * @returns int
	 */
	public int getMaximumRowCount() {
		return 0; // TODO
	} // getMaximumRowCount()

	/**
	 * setRenderer
	 * @param value0 TODO
	 */
	public void setRenderer(ListCellRenderer value0) {
		// TODO
	} // setRenderer()

	/**
	 * getRenderer
	 * @returns ListCellRenderer
	 */
	public ListCellRenderer getRenderer() {
		return null; // TODO
	} // getRenderer()

	/**
	 * setEditor
	 * @param value0 TODO
	 */
	public void setEditor(ComboBoxEditor value0) {
		// TODO
	} // setEditor()

	/**
	 * getEditor
	 * @returns ComboBoxEditor
	 */
	public ComboBoxEditor getEditor() {
		return null; // TODO
	} // getEditor()

	/**
	 * setSelectedItem
	 * @param value0 TODO
	 */
	public void setSelectedItem(Object value0) {
		// TODO
	} // setSelectedItem()

	/**
	 * getSelectedItem
	 * @returns Object
	 */
	public Object getSelectedItem() {
		return null; // TODO
	} // getSelectedItem()

	/**
	 * setSelectedIndex
	 * @param value0 TODO
	 */
	public void setSelectedIndex(int value0) {
		// TODO
	} // setSelectedIndex()

	/**
	 * getSelectedIndex
	 * @returns int
	 */
	public int getSelectedIndex() {
		return 0; // TODO
	} // getSelectedIndex()

	/**
	 * addItem
	 * @param value0 TODO
	 */
	public void addItem(Object value0) {
		// TODO
	} // addItem()

	/**
	 * insertItemAt
	 * @param value0 TODO
	 * @param value1 TODO
	 */
	public void insertItemAt(Object value0, int value1) {
		// TODO
	} // insertItemAt()

	/**
	 * removeItem
	 * @param value0 TODO
	 */
	public void removeItem(Object value0) {
		// TODO
	} // removeItem()

	/**
	 * removeItemAt
	 * @param value0 TODO
	 */
	public void removeItemAt(int value0) {
		// TODO
	} // removeItemAt()

	/**
	 * removeAllItems
	 */
	public void removeAllItems() {
		// TODO
	} // removeAllItems()
	
	/**
	 * showPopup
	 */
	public void showPopup() {
		// TODO
	} // showPopup()

	/**
	 * hidePopup
	 */
	public void hidePopup() {
		// TODO
	} // hidePopup()

	/**
	 * setPopupVisible
	 * @param value0 TODO
	 */
	public void setPopupVisible(boolean value0) {
		// TODO
	} // setPopupVisible()

	/**
	 * isPopupVisible
	 * @returns boolean
	 */
	public boolean isPopupVisible() {
		return false; // TODO
	} // isPopupVisible()

	/**
	 * setActionCommand
	 * @param value0 TODO
	 */
	public void setActionCommand(String value0) {
		// TODO
	} // setActionCommand()

	/**
	 * getActionCommand
	 * @returns String
	 */
	public String getActionCommand() {
		return null; // TODO
	} // getActionCommand()

	/**
	 * setAction
	 * @param value0 TODO
	 */
	public void setAction(Action value0) {
		// TODO
	} // setAction()

	/**
	 * isListener
	 * @param value0 TODO
	 * @param value1 TODO
	 * @returns boolean
	 */
	private boolean isListener(Class value0, ActionListener value1) {
		return false; // TODO
	} // isListener()

	/**
	 * getAction
	 * @returns Action
	 */
	public Action getAction() {
		return null; // TODO
	} // getAction()

	/**
	 * configurePropertiesFromAction
	 * @param value0 TODO
	 */
	protected void configurePropertiesFromAction(Action value0) {
		// TODO
	} // configurePropertiesFromAction()

	/**
	 * createActionPropertyChangeListener
	 * @param value0 TODO
	 * @returns PropertyChangeListener
	 */
	protected PropertyChangeListener createActionPropertyChangeListener(Action value0) {
		return null; // TODO
	} // createActionPropertyChangeListener()

	/**
	 * fireItemStateChanged
	 * @param value0 TODO
	 */
	protected void fireItemStateChanged(ItemEvent value0) {
		// TODO
	} // fireItemStateChanged()

	/**
	 * fireActionEvent
	 */
	protected void fireActionEvent() {
		// TODO
	} // fireActionEvent()

	/**
	 * selectedItemChanged
	 */
	protected void selectedItemChanged() {
		// TODO
	} // selectedItemChanged()

	/**
	 * getSelectedObjects
	 * @returns Object[]
	 */
	public Object[] getSelectedObjects() {
		return null; // TODO
	} // getSelectedObjects()

	/**
	 * actionPerformed
	 * @param value0 TODO
	 */
	public void actionPerformed(ActionEvent value0) {
		// TODO
	} // actionPerformed()

	/**
	 * contentsChanged
	 * @param value0 TODO
	 */
	public void contentsChanged(ListDataEvent value0) {
		// TODO
	} // contentsChanged()

	/**
	 * selectWithKeyChar
	 * @param value0 TODO
	 * @returns boolean
	 */
	public boolean selectWithKeyChar(char value0) {
		return false; // TODO
	} // selectWithKeyChar()

	/**
	 * intervalAdded
	 * @param value0 TODO
	 */
	public void intervalAdded(ListDataEvent value0) {
		// TODO
	} // intervalAdded()

	/**
	 * intervalRemoved
	 * @param value0 TODO
	 */
	public void intervalRemoved(ListDataEvent value0) {
		// TODO
	} // intervalRemoved()

	/**
	 * setEnabled
	 * @param value0 TODO
	 */
	public void setEnabled(boolean value0) {
		// TODO
	} // setEnabled()

	/**
	 * configureEditor
	 * @param value0 TODO
	 * @param value1 TODO
	 */
	public void configureEditor(ComboBoxEditor value0, Object value1) {
		// TODO
	} // configureEditor()

	/**
	 * processKeyEvent
	 * @param value0 TODO
	 */
	public void processKeyEvent(KeyEvent value0) {
		// TODO
	} // processKeyEvent()

	/**
	 * isFocusTraversable
	 * @returns boolean
         * @deprecated
	 */
	public boolean isFocusTraversable() {
		return false; // TODO
	} // isFocusTraversable()

	/**
	 * setKeySelectionManager
	 * @param value0 TODO
	 */
	public void setKeySelectionManager(KeySelectionManager value0) {
		// TODO
	} // setKeySelectionManager()

	/**
	 * getKeySelectionManager
	 * @returns JComboBox.KeySelectionManager
	 */
	public JComboBox.KeySelectionManager getKeySelectionManager() {
		return null; // TODO
	} // getKeySelectionManager()

	/**
	 * getItemCount
	 * @returns int
	 */
	public int getItemCount() {
		return 0; // TODO
	} // getItemCount()

	/**
	 * getItemAt
	 * @param value0 TODO
	 * @returns Object
	 */
	public Object getItemAt(int value0) {
		return null; // TODO
	} // getItemAt()

	/**
	 * createDefaultKeySelectionManager
	 * @returns KeySelectionManager
	 */
	protected KeySelectionManager createDefaultKeySelectionManager() {
		return null; // TODO
	} // createDefaultKeySelectionManager()

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
  public AccessibleContext getAccessibleContext()
  {
    if (accessibleContext == null)
      accessibleContext = new AccessibleJComboBox();

    return accessibleContext;
  }
  
  /**
   * addActionListener
   * @param listener TODO
   */
  public void addActionListener (ActionListener listener)
  {
    listenerList.add (ActionListener.class, listener);
  }

  /**
   * removeActionListener
   * @param listener TODO
   */
  public void removeActionListener (ActionListener listener)
  {
    listenerList.remove (ActionListener.class, listener);
  }

  /**
   * @since 1.4
   */
  public ActionListener[] getActionListeners()
  {
    return (ActionListener[]) getListeners (ActionListener.class);
  }

  /**
   * addItemListener
   * @param listener TODO
   */
  public void addItemListener(ItemListener listener)
  {
    listenerList.add (ItemListener.class, listener);
  }

  /**
   * removeItemListener
   * @param listener TODO
   */
  public void removeItemListener(ItemListener listener)
  {
    listenerList.remove (ItemListener.class, listener);
  }

  /**
   * @since 1.4
   */
  public ItemListener[] getItemListeners()
  {
    return (ItemListener[]) getListeners (ItemListener.class);
  }

  public void addPopupMenuListener (PopupMenuListener listener)
  {
    listenerList.add (PopupMenuListener.class, listener);
  }

  public void removePopupMenuListener (PopupMenuListener listener)
  {
    listenerList.remove (PopupMenuListener.class, listener);
  }

  /**
   * @since 1.4
   */
  public PopupMenuListener[] getPopupMenuListeners()
  {
    return (PopupMenuListener[]) getListeners (PopupMenuListener.class);
  }
}
