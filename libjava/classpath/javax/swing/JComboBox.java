/* JComboBox.java --
   Copyright (C) 2002, 2004, 2005  Free Software Foundation, Inc.

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
Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
02110-1301 USA.

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
import java.beans.PropertyChangeEvent;
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
 * A component that allows a user to select any item in its list and
 * displays the selected item to the user. JComboBox also can show/hide a
 * popup menu containing its list of item whenever the mouse is pressed
 * over it.
 *
 * @author Andrew Selkirk
 * @author Olga Rodimina
 * @author Robert Schuster
 */
public class JComboBox extends JComponent implements ItemSelectable,
                                                     ListDataListener,
                                                     ActionListener,
                                                     Accessible
{

  private static final long serialVersionUID = 5654585963292734470L;

  /**
   * Classes implementing this interface are
   * responsible for matching key characters typed by the user with combo
   * box's items.
   */
  public static interface KeySelectionManager
  {
    int selectionForKey(char aKey, ComboBoxModel aModel);
  }

  /**
   * Maximum number of rows that should be visible by default  in the
   * JComboBox's popup
   */
  private static final int DEFAULT_MAXIMUM_ROW_COUNT = 8;

  /**
   * Data model used by JComboBox to keep track of its list data and currently
   * selected element in the list.
   */
  protected ComboBoxModel dataModel;

  /**
   * Renderer renders(paints) every object in the combo box list in its
   * associated list cell. This ListCellRenderer is used only when  this
   * JComboBox is uneditable.
   */
  protected ListCellRenderer renderer;

  /**
   * Editor that is responsible for editing an object in a combo box list.
   */
  protected ComboBoxEditor editor;

  /**
   * Number of rows that will be visible in the JComboBox's popup.
   */
  protected int maximumRowCount;

  /**
   * This field indicates if textfield of this JComboBox is editable or not.
   */
  protected boolean isEditable;

  /**
   * This field is reference to the current selection of the combo box.
   */
  protected Object selectedItemReminder;

  /**
   * keySelectionManager
   */
  protected KeySelectionManager keySelectionManager;

  /**
   * This actionCommand is used in ActionEvent that is fired to JComboBox's
   * ActionListeneres.
   */
  protected String actionCommand;

  /**
   * This property indicates if heavyweight popup or lightweight popup will be
   * used to diplay JComboBox's elements.
   */
  protected boolean lightWeightPopupEnabled;

  /**
   * The action taken when new item is selected in the JComboBox
   */
  private Action action;

  /**
   * since 1.4  If this field is set then comboBox's display area for the
   * selected item  will be set by default to this value.
   */
  private Object prototypeDisplayValue;

  /**
   * Constructs JComboBox object with specified data model for it.
   * <p>Note that the JComboBox will not change the value that
   * is preselected by your ComboBoxModel implementation.</p>
   *
   * @param model Data model that will be used by this JComboBox to keep track
   *        of its list of items.
   */
  public JComboBox(ComboBoxModel model)
  {
    setEditable(false);
    setEnabled(true);
    setMaximumRowCount(DEFAULT_MAXIMUM_ROW_COUNT);
    setModel(model);
    setActionCommand("comboBoxChanged");

    lightWeightPopupEnabled = true;
    isEditable = false;

    updateUI();
  }

  /**
   * Constructs JComboBox with specified list of items.
   *
   * @param itemArray array containing list of items for this JComboBox
   */
  public JComboBox(Object[] itemArray)
  {
    this(new DefaultComboBoxModel(itemArray));
    
    if (itemArray.length > 0) 
      setSelectedIndex(0);
  }

  /**
   * Constructs JComboBox object with specified list of items.
   *
   * @param itemVector vector containing list of items for this JComboBox.
   */
  public JComboBox(Vector itemVector)
  {
    this(new DefaultComboBoxModel(itemVector));

    if (itemVector.size() > 0)
      setSelectedIndex(0);
  }

  /**
   * Constructor. Creates new empty JComboBox. ComboBox's data model is set to
   * DefaultComboBoxModel.
   */
  public JComboBox()
  {
    this(new DefaultComboBoxModel());
  }

  private void writeObject(ObjectOutputStream stream) throws IOException
  {
  }

  /**
   * This method returns true JComboBox is editable and false otherwise
   *
   * @return boolean true if JComboBox is editable and false otherwise
   */
  public boolean isEditable()
  {
    return isEditable;
  }

  /*
   * This method adds ancestor listener to this JComboBox.
   */
  protected void installAncestorListener()
  {
    /* FIXME: Need to implement.
     *
     * Need to add ancestor listener to this JComboBox. This listener
     * should close combo box's popup list of items whenever it
     * receives an AncestorEvent.
     */
  }

  /**
   * Set the "UI" property of the combo box, which is a look and feel class
   * responsible for handling comboBox's input events and painting it.
   *
   * @param ui The new "UI" property
   */
  public void setUI(ComboBoxUI ui)
  {
    super.setUI(ui);
  }

  /**
   * This method sets this comboBox's UI to the UIManager's default for the
   * current look and feel.
   */
  public void updateUI()
  {
    setUI((ComboBoxUI) UIManager.getUI(this));
    invalidate();
  }

  /**
   * This method returns the String identifier for the UI class to the used
   * with the JComboBox.
   *
   * @return The String identifier for the UI class.
   */
  public String getUIClassID()
  {
    return "ComboBoxUI";
  }

  /**
   * This method returns the UI used to display the JComboBox.
   *
   * @return The UI used to display the JComboBox.
   */
  public ComboBoxUI getUI()
  {
    return (ComboBoxUI) ui;
  }

  /**
   * Set the data model for this JComboBox. This un-registers all  listeners
   * associated with the current model, and re-registers them with the new
   * model.
   *
   * @param newDataModel The new data model for this JComboBox
   */
  public void setModel(ComboBoxModel newDataModel)
  {
    // dataModel is null if it this method is called from inside the constructors.
    if (dataModel != null)
      {
        // Prevents unneccessary updates.
        if (dataModel == newDataModel)
          return;

        // Removes itself (as DataListener) from the to-be-replaced model.
        dataModel.removeListDataListener(this);
      }
    
    /* Adds itself as a DataListener to the new model.
     * It is intentioned that this operation will fail with a NullPointerException if the
     * caller delivered a null argument.
     */
    newDataModel.addListDataListener(this);

    // Stores old data model for event notification.
    ComboBoxModel oldDataModel = dataModel;
    dataModel = newDataModel;

    // Notifies the listeners of the model change.
    firePropertyChange("model", oldDataModel, dataModel);
  }

  /**
   * This method returns data model for this comboBox.
   *
   * @return ComboBoxModel containing items for this combo box.
   */
  public ComboBoxModel getModel()
  {
    return dataModel;
  }

  /**
   * This method sets JComboBox's popup to be either lightweight or
   * heavyweight. If 'enabled' is true then lightweight popup is used and
   * heavyweight otherwise. By default lightweight popup is used to display
   * this JComboBox's elements.
   *
   * @param enabled indicates if lightweight popup or heavyweight popup should
   *        be used to display JComboBox's elements.
   */
  public void setLightWeightPopupEnabled(boolean enabled)
  {
    lightWeightPopupEnabled = enabled;
  }

  /**
   * This method returns whether popup menu that is used to display list of
   * combo box's item is lightWeight or not.
   *
   * @return boolean true if popup menu is lightweight and false otherwise.
   */
  public boolean isLightWeightPopupEnabled()
  {
    return lightWeightPopupEnabled;
  }

  /**
   * This method sets editability of the combo box. If combo box  is editable
   * the user can choose component from the combo box list by typing
   * component's name in the editor(JTextfield by default).  Otherwise if not
   * editable, the user should use the list to choose   the component. This
   * method fires PropertyChangeEvents to JComboBox's registered
   * PropertyChangeListeners to indicate that 'editable' property of the
   * JComboBox has changed.
   *
   * @param editable indicates if the JComboBox's textfield should be editable
   *        or not.
   */
  public void setEditable(boolean editable)
  {
    if (isEditable != editable)
      {
        isEditable = editable;
        firePropertyChange("editable", !isEditable, isEditable);
      }
  }

  /**
   * Sets number of rows that should be visible in this JComboBox's popup. If
   * this JComboBox's popup has more elements that maximum number or rows
   * then popup will have a scroll pane to allow users to view other
   * elements.
   *
   * @param rowCount number of rows that will be visible in JComboBox's popup.
   */
  public void setMaximumRowCount(int rowCount)
  {
    if (maximumRowCount != rowCount)
      {
        int oldMaximumRowCount = maximumRowCount;
        maximumRowCount = rowCount;
        firePropertyChange("maximumRowCount", oldMaximumRowCount,
                           maximumRowCount);
      }
  }

  /**
   * This method returns number of rows visible in the JComboBox's list of
   * items.
   *
   * @return int maximun number of visible rows in the JComboBox's list.
   */
  public int getMaximumRowCount()
  {
    return maximumRowCount;
  }

  /**
   * This method sets cell renderer for this JComboBox that will be used to
   * paint combo box's items. The Renderer should only be used only when
   * JComboBox is not editable.  In the case when JComboBox is editable  the
   * editor must be used.  This method also fires PropertyChangeEvent when
   * cellRendered for this JComboBox has changed.
   *
   * @param aRenderer cell renderer that will be used by this JComboBox to
   *        paint its elements.
   */
  public void setRenderer(ListCellRenderer aRenderer)
  {
    if (renderer != aRenderer)
      {
        ListCellRenderer oldRenderer = renderer;
        renderer = aRenderer;
        firePropertyChange("renderer", oldRenderer, renderer);
      }
  }

  /**
   * This method returns renderer responsible for rendering selected item in
   * the combo box
   *
   * @return ListCellRenderer
   */
  public ListCellRenderer getRenderer()
  {
    return renderer;
  }

  /**
   * Sets editor for this JComboBox
   *
   * @param newEditor ComboBoxEditor for this JComboBox. This method fires
   *        PropertyChangeEvent when 'editor' property is changed.
   */
  public void setEditor(ComboBoxEditor newEditor)
  {
    if (editor == newEditor)
      return;

    if (editor != null)
      editor.removeActionListener(this);

    ComboBoxEditor oldEditor = editor;
    editor = newEditor;

    if (editor != null)
      editor.addActionListener(this);

    firePropertyChange("editor", oldEditor, editor);
  }

  /**
   * Returns editor component that is responsible for displaying/editing
   * selected item in the combo box.
   *
   * @return ComboBoxEditor
   */
  public ComboBoxEditor getEditor()
  {
    return editor;
  }

  /**
   * Forces combo box to select given item
   *
   * @param item element in the combo box to select.
   */
  public void setSelectedItem(Object item)
  {
    dataModel.setSelectedItem(item);
  }

  /**
   * Returns currently selected item in the combo box.
   * The result may be <code>null</code> to indicate that nothing is
   * currently selected.
   *
   * @return element that is currently selected in this combo box.
   */
  public Object getSelectedItem()
  {
    return dataModel.getSelectedItem();
  }

  /**
   * Forces JComboBox to select component located in the given index in the
   * combo box.
   * <p>If the index is below -1 or exceeds the upper bound an
   * <code>IllegalArgumentException</code> is thrown.<p/>
   * <p>If the index is -1 then no item gets selected.</p>
   *
   * @param index index specifying location of the component that  should be
   *        selected.
   */
  public void setSelectedIndex(int index)
  {
  	if (index < -1 || index >= dataModel.getSize())
      // Fails because index is out of bounds.
      throw new IllegalArgumentException("illegal index: " + index);
    else
       // Selects the item at the given index or clears the selection if the
       // index value is -1.
      setSelectedItem((index == -1) ? null : dataModel.getElementAt(index));
  }

  /**
   * Returns index of the item that is currently selected in the combo box. If
   * no item is currently selected, then -1 is returned.
   * <p>
   * Note: For performance reasons you should minimize invocation of this
   * method. If the data model is not an instance of
   * <code>DefaultComboBoxModel</code> the complexity is O(n) where n is the
   * number of elements in the combo box.
   * </p>
   * 
   * @return int Index specifying location of the currently selected item in the
   *         combo box or -1 if nothing is selected in the combo box.
   */
  public int getSelectedIndex()
  {
    Object selectedItem = getSelectedItem();

    if (selectedItem != null)
      {
        if (dataModel instanceof DefaultComboBoxModel)
          // Uses special method of DefaultComboBoxModel to retrieve the index.
          return ((DefaultComboBoxModel) dataModel).getIndexOf(selectedItem);
        else
          {
            // Iterates over all items to retrieve the index.
            int size = dataModel.getSize();

            for (int i = 0; i < size; i++)
              {
                Object o = dataModel.getElementAt(i);

                // XXX: Is special handling of ComparableS neccessary?
                if ((selectedItem != null) ? selectedItem.equals(o) : o == null)
                  return i;
              }
          }
      }

    // returns that no item is currently selected
    return -1;
  }

  public Object getPrototypeDisplayValue()
  {
    return prototypeDisplayValue;
  }

  public void setPrototypeDisplayValue(Object newPrototypeDisplayValue)
  {
    prototypeDisplayValue = newPrototypeDisplayValue;
  }

  /**
   * This method adds given element to this JComboBox.
   * <p>A <code>RuntimeException</code> is thrown if the data model is not
   * an instance of {@link MutableComboBoxModel}.</p>
   *
   * @param element element to add
   */
  public void addItem(Object element)
  {
  	if (dataModel instanceof MutableComboBoxModel)
      ((MutableComboBoxModel) dataModel).addElement(element);
    else
      throw new RuntimeException("Unable to add the item because the data "
                                 + "model it is not an instance of "
                                 + "MutableComboBoxModel.");
  }

  /**
   * Inserts given element at the specified index to this JComboBox.
   * <p>A <code>RuntimeException</code> is thrown if the data model is not
   * an instance of {@link MutableComboBoxModel}.</p>
   *
   * @param element element to insert
   * @param index position where to insert the element
   */
  public void insertItemAt(Object element, int index)
  {
	if (dataModel instanceof MutableComboBoxModel)
      ((MutableComboBoxModel) dataModel).insertElementAt(element, index);
    else
      throw new RuntimeException("Unable to insert the item because the data "
                                 + "model it is not an instance of "
                                 + "MutableComboBoxModel.");
  }

  /**
   * This method removes given element from this JComboBox.
   * <p>A <code>RuntimeException</code> is thrown if the data model is not
   * an instance of {@link MutableComboBoxModel}.</p>
   *
   * @param element element to remove
   */
  public void removeItem(Object element)
  {
	if (dataModel instanceof MutableComboBoxModel)
      ((MutableComboBoxModel) dataModel).removeElement(element);
    else
      throw new RuntimeException("Unable to remove the item because the data "
                                 + "model it is not an instance of "
                                 + "MutableComboBoxModel.");
  }

  /**
   * This method remove element location in the specified index in the
   * JComboBox.
   * <p>A <code>RuntimeException</code> is thrown if the data model is not
   * an instance of {@link MutableComboBoxModel}.</p>
   *
   * @param index index specifying position of the element to remove
   */
  public void removeItemAt(int index)
  {
    if (dataModel instanceof MutableComboBoxModel)
      ((MutableComboBoxModel) dataModel).removeElementAt(index);
    else
      throw new RuntimeException("Unable to remove the item because the data "
                                 + "model it is not an instance of "
                                 + "MutableComboBoxModel.");
  }

  /**
   * This method removes all elements from this JComboBox.
   * <p>
   * A <code>RuntimeException</code> is thrown if the data model is not an
   * instance of {@link MutableComboBoxModel}.
   * </p>
   */
  public void removeAllItems()
  {
    if (dataModel instanceof DefaultComboBoxModel)
      // Uses special method if we have a DefaultComboBoxModel.
      ((DefaultComboBoxModel) dataModel).removeAllElements();
    else if (dataModel instanceof MutableComboBoxModel)
      {
        // Iterates over all items and removes each.
        MutableComboBoxModel mcbm = (MutableComboBoxModel) dataModel;

         // We intentionally remove the items backwards to support models which
         // shift their content to the beginning (e.g. linked lists)
        for (int i = mcbm.getSize() - 1; i >= 0; i--)
          mcbm.removeElementAt(i);
      }
    else
      throw new RuntimeException("Unable to remove the items because the data "
                                 +"model it is not an instance of "
                                 + "MutableComboBoxModel.");
  }

  /**
   * This method displays popup with list of combo box's items on the screen
   */
  public void showPopup()
  {
    setPopupVisible(true);
  }

  /**
   * This method hides popup containing list of combo box's items
   */
  public void hidePopup()
  {
    setPopupVisible(false);
  }

  /**
   * This method either displayes or hides the popup containing  list of combo
   * box's items.
   *
   * @param visible show popup if 'visible' is true and hide it otherwise
   */
  public void setPopupVisible(boolean visible)
  {
    getUI().setPopupVisible(this, visible);
  }

  /**
   * Checks if popup is currently visible on the screen.
   *
   * @return boolean true if popup is visible and false otherwise
   */
  public boolean isPopupVisible()
  {
    return getUI().isPopupVisible(this);
  }

  /**
   * This method sets actionCommand to the specified string. ActionEvent fired
   * to this JComboBox  registered ActionListeners will contain this
   * actionCommand.
   *
   * @param aCommand new action command for the JComboBox's ActionEvent
   */
  public void setActionCommand(String aCommand)
  {
    actionCommand = aCommand;
  }

  /**
   * Returns actionCommand associated with the ActionEvent fired by the
   * JComboBox to its registered ActionListeners.
   *
   * @return String actionCommand for the ActionEvent
   */
  public String getActionCommand()
  {
    return actionCommand;
  }

  /**
   * setAction
   *
   * @param a action to set
   */
  public void setAction(Action a)
  {
    Action old = action;
    action = a;
    configurePropertiesFromAction(action);
    if (action != null)
      // FIXME: remove from old action and add to new action 
      // PropertyChangeListener to listen to changes in the action
      addActionListener(action);
  }

  /**
   * This method returns Action that is invoked when selected item is changed
   * in the JComboBox.
   *
   * @return Action
   */
  public Action getAction()
  {
    return action;
  }

  /**
   * Configure properties of the JComboBox by reading properties of specified
   * action. This method always sets the comboBox's "enabled" property to the
   * value of the Action's "enabled" property.
   *
   * @param a An Action to configure the combo box from
   */
  protected void configurePropertiesFromAction(Action a)
  {
    if (a == null)
      {
        setEnabled(true);
        setToolTipText(null);
      }
    else
      {
        setEnabled(a.isEnabled());
        setToolTipText((String) (a.getValue(Action.SHORT_DESCRIPTION)));
      }
  }

  /**
   * Creates PropertyChangeListener to listen for the changes in comboBox's
   * action properties.
   *
   * @param action action to listen to for property changes
   *
   * @return a PropertyChangeListener that listens to changes in
   *         action properties.
   */
  protected PropertyChangeListener createActionPropertyChangeListener(Action action)
  {
    return new PropertyChangeListener()
      {
        public void propertyChange(PropertyChangeEvent e)
        {
          Action act = (Action) (e.getSource());
          configurePropertiesFromAction(act);
        }
      };
  }

  /**
   * This method fires ItemEvent to this JComboBox's registered ItemListeners.
   * This method is invoked when currently selected item in this combo box
   * has changed.
   *
   * @param e the ItemEvent describing the change in the combo box's
   *        selection.
   */
  protected void fireItemStateChanged(ItemEvent e)
  {
    ItemListener[] ll = getItemListeners();

    for (int i = 0; i < ll.length; i++)
      ll[i].itemStateChanged(e);
  }

  /**
   * This method fires ActionEvent to this JComboBox's registered
   * ActionListeners. This method is invoked when user explicitly changes
   * currently selected item.
   */
  protected void fireActionEvent()
  {
    ActionListener[] ll = getActionListeners();

    for (int i = 0; i < ll.length; i++)
      ll[i].actionPerformed(new ActionEvent(this,
                                            ActionEvent.ACTION_PERFORMED,
                                            actionCommand));
  }

  /**
   * This method is invoked whenever selected item changes in the combo box's
   * data model. It fires ItemEvent and ActionEvent to all registered
   * ComboBox's ItemListeners and ActionListeners respectively, indicating
   * the change.
   */
  protected void selectedItemChanged()
  {
    // Fire ItemEvent to indicated that previously selected item is now
    // deselected        
    if (selectedItemReminder != null)
      fireItemStateChanged(new ItemEvent(this, ItemEvent.ITEM_STATE_CHANGED,
                                         selectedItemReminder,
                                         ItemEvent.DESELECTED));

    // Fire ItemEvent to indicate that new item is selected    
    Object newSelection = getSelectedItem();
    fireItemStateChanged(new ItemEvent(this, ItemEvent.ITEM_STATE_CHANGED,
                                       newSelection, ItemEvent.SELECTED));

    // Fire Action Event to JComboBox's registered listeners					 				 
    fireActionEvent();

    selectedItemReminder = newSelection;
  }

  /**
   * Returns Object array of size 1 containing currently selected element in
   * the JComboBox.
   *
   * @return Object[] Object array of size 1 containing currently selected
   *         element in the JComboBox.
   */
  public Object[] getSelectedObjects()
  {
    return new Object[] { getSelectedItem() };
  }

  /**
   * This method handles actionEvents fired by the ComboBoxEditor. It changes
   * this JComboBox's selection to the new value currently in the editor and
   * hides list of combo box items.
   *
   * @param e the ActionEvent
   */
  public void actionPerformed(ActionEvent e)
  {
    setSelectedItem(((ComboBoxEditor) e.getSource()).getItem());
    setPopupVisible(false);
  }

  /**
   * This method selects item in this combo box that matches specified
   * specified keyChar and returns true if such item is found. Otherwise
   * false is returned.
   *
   * @param keyChar character indicating which item in the combo box should be
   *        selected.
   *
   * @return boolean true if item corresponding to the specified keyChar
   *         exists in the combo box. Otherwise false is returned.
   */
  public boolean selectWithKeyChar(char keyChar)
  {
    // FIXME: Need to implement
    return false;
  }

  /**
   * The part of implementation of ListDataListener interface. This method is
   * invoked when some items where added to the JComboBox's data model.
   *
   * @param event ListDataEvent describing the change
   */
  public void intervalAdded(ListDataEvent event)
  {
    // FIXME: Need to implement
    repaint();
  }

  /**
   * The part of implementation of ListDataListener interface. This method is
   * invoked when some items where removed from the JComboBox's data model.
   *
   * @param event ListDataEvent describing the change.
   */
  public void intervalRemoved(ListDataEvent event)
  {
    // FIXME: Need to implement
    repaint();
  }

  /**
   * The part of implementation of ListDataListener interface. This method is
   * invoked when contents of the JComboBox's  data model changed.
   *
   * @param event ListDataEvent describing the change
   */
  public void contentsChanged(ListDataEvent event)
  {
    // if first and last index of the given ListDataEvent are both -1,
    // then it indicates that selected item in the combo box data model
    // have changed. 
    if (event.getIndex0() == -1 && event.getIndex1() == -1)
      selectedItemChanged();
  }

  /**
   * This method disables or enables JComboBox. If the JComboBox is enabled,
   * then user is able to make item choice, otherwise if JComboBox is
   * disabled then user is not able to make a selection.
   *
   * @param enabled if 'enabled' is true then enable JComboBox and disable it
   */
  public void setEnabled(boolean enabled)
  {
    boolean oldEnabled = super.isEnabled();
    if (enabled != oldEnabled)
      {
        super.setEnabled(enabled);
        firePropertyChange("enabled", oldEnabled, enabled);
      }
  }

  /**
   * This method initializes specified ComboBoxEditor to display given item.
   *
   * @param anEditor ComboBoxEditor to initialize
   * @param anItem Item that should displayed in the specified editor
   */
  public void configureEditor(ComboBoxEditor anEditor, Object anItem)
  {
    anEditor.setItem(anItem);
  }

  /**
   * This method hides  combo box's popup whenever TAB key is pressed.
   *
   * @param e The KeyEvent indicating which key was pressed.
   */
  public void processKeyEvent(KeyEvent e)
  {
  }

  /**
   * This method always returns false to indicate that JComboBox  itself is
   * not focus traversable.
   *
   * @return false to indicate that JComboBox itself is not focus traversable.
   *
   * @deprecated
   */
  public boolean isFocusTraversable()
  {
    return false;
  }

  /**
   * setKeySelectionManager
   *
   * @param aManager
   */
  public void setKeySelectionManager(KeySelectionManager aManager)
  {
  }

  /**
   * getKeySelectionManager
   *
   * @return JComboBox.KeySelectionManager
   */
  public KeySelectionManager getKeySelectionManager()
  {
    return null;
  }

  /**
   * This method returns number of elements in this JComboBox
   *
   * @return int number of elements in this JComboBox
   */
  public int getItemCount()
  {
    return dataModel.getSize();
  }

  /**
   * Returns elements located in the combo box at the given index.
   *
   * @param index index specifying location of the component to  return.
   *
   * @return component in the combo box that is located in  the given index.
   */
  public Object getItemAt(int index)
  {
    return dataModel.getElementAt(index);
  }

  /**
   * createDefaultKeySelectionManager
   *
   * @return KeySelectionManager
   */
  protected KeySelectionManager createDefaultKeySelectionManager()
  {
    return null;
  }

  /**
   * A string that describes this JComboBox. Normally only used for debugging.
   *
   * @return A string describing this JComboBox
   */
  protected String paramString()
  {
    return "JComboBox";
  }

  public AccessibleContext getAccessibleContext()
  {
    if (accessibleContext == null)
      accessibleContext = new AccessibleJComboBox();

    return accessibleContext;
  }

  /**
   * This methods adds specified ActionListener to this JComboBox.
   *
   * @param listener to add
   */
  public void addActionListener(ActionListener listener)
  {
    listenerList.add(ActionListener.class, listener);
  }

  /**
   * This method removes specified ActionListener from this JComboBox.
   *
   * @param listener ActionListener
   */
  public void removeActionListener(ActionListener listener)
  {
    listenerList.remove(ActionListener.class, listener);
  }

  /**
   * This method returns array of ActionListeners that are registered with
   * this JComboBox.
   *
   * @since 1.4
   */
  public ActionListener[] getActionListeners()
  {
    return (ActionListener[]) getListeners(ActionListener.class);
  }

  /**
   * This method registers given ItemListener with this JComboBox
   *
   * @param listener to remove
   */
  public void addItemListener(ItemListener listener)
  {
    listenerList.add(ItemListener.class, listener);
  }

  /**
   * This method unregisters given ItemListener from this JComboBox
   *
   * @param listener to remove
   */
  public void removeItemListener(ItemListener listener)
  {
    listenerList.remove(ItemListener.class, listener);
  }

  /**
   * This method returns array of ItemListeners that are registered with this
   * JComboBox.
   *
   * @since 1.4
   */
  public ItemListener[] getItemListeners()
  {
    return (ItemListener[]) getListeners(ItemListener.class);
  }

  /**
   * Adds PopupMenuListener to combo box to listen to the events fired by the
   * combo box's popup menu containing its list of items
   *
   * @param listener to add
   */
  public void addPopupMenuListener(PopupMenuListener listener)
  {
    listenerList.add(PopupMenuListener.class, listener);
  }

  /**
   * Removes PopupMenuListener to combo box to listen to the events fired by
   * the combo box's popup menu containing its list of items
   *
   * @param listener to add
   */
  public void removePopupMenuListener(PopupMenuListener listener)
  {
    listenerList.remove(PopupMenuListener.class, listener);
  }

  /**
   * Returns array of PopupMenuListeners that are registered with  combo box.
   */
  public PopupMenuListener[] getPopupMenuListeners()
  {
    return (PopupMenuListener[]) getListeners(PopupMenuListener.class);
  }

  /**
   * Accessibility support for <code>JComboBox</code>.
   */
  protected class AccessibleJComboBox extends AccessibleJComponent
    implements AccessibleAction, AccessibleSelection
  {
    private static final long serialVersionUID = 8217828307256675666L;

    protected AccessibleJComboBox()
    {
    }

    public int getAccessibleChildrenCount()
    {
      return 0;
    }

    public Accessible getAccessibleChild(int value0)
    {
      return null;
    }

    public AccessibleSelection getAccessibleSelection()
    {
      return null;
    }

    public Accessible getAccessibleSelection(int value0)
    {
      return null;
    }

    public boolean isAccessibleChildSelected(int value0)
    {
      return false;
    }

    public AccessibleRole getAccessibleRole()
    {
      return AccessibleRole.COMBO_BOX;
    }

    public AccessibleAction getAccessibleAction()
    {
      return null;
    }

    public String getAccessibleActionDescription(int value0)
    {
      return null;
    }

    public int getAccessibleActionCount()
    {
      return 0;
    }

    public boolean doAccessibleAction(int value0)
    {
      return false;
    }

    public int getAccessibleSelectionCount()
    {
      return 0;
    }

    public void addAccessibleSelection(int value0)
    {
    }

    public void removeAccessibleSelection(int value0)
    {
    }

    public void clearAccessibleSelection()
    {
    }

    public void selectAllAccessibleSelection()
    {
    }
  }
}
