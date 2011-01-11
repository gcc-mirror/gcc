/* DefaultCellEditor.java --
   Copyright (C) 2002, 2004, 2006, Free Software Foundation, Inc.

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

import java.awt.Component;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.ItemEvent;
import java.awt.event.ItemListener;
import java.awt.event.MouseEvent;
import java.io.Serializable;
import java.util.EventObject;

import javax.swing.JTable;
import javax.swing.JTextField;
import javax.swing.event.CellEditorListener;
import javax.swing.table.TableCellEditor;
import javax.swing.tree.TreeCellEditor;

/**
 * The default implementation of {@link TableCellEditor} and
 * {@link TreeCellEditor}. It provides editor components for
 * some standard object types.
 *
 * @author Andrew Selkirk
 * @author Audrius Meskauskas
 */
public class DefaultCellEditor
  extends AbstractCellEditor
  implements TableCellEditor, TreeCellEditor
{
  private static final long serialVersionUID = 3564035141373880027L;

  /**
   * This changeable module access the editor component in the component
   * specific way. For instance, to set the value for JTextField, we need to
   * call setText(String), and for JCheckBox we need to call
   * setSelected(boolean). Each default editor has the component specific
   * derivative of this class. These derivatives are private inner classes of
   * the DefaultCellEditor.
   *
   * The editor delegate is also set for the editor component as the action
   * listener. It listens for the events that indicate that editing has stopped.
   */
  protected class EditorDelegate
    implements ActionListener, ItemListener, Serializable
  {
    /**
     * Use the serial version UID for interoperability.
     */
    private static final long serialVersionUID = -1420007406015481933L;

    /**
     * The object value (updated when getting and setting the value).
     */
    protected Object value;

    /**
     * Constructor EditorDelegate
     */
    protected EditorDelegate()
    {
      // Nothing to do here.
    }

    /**
     * Set the value for the editor component. This method is normally
     * overridden to set the value in the way, specific for the text
     * component, check box or combo box.
     *
     * @param aValue the value to set (String, Boolean or Number).
     */
    public void setValue(Object aValue)
    {
      value = aValue;
    }

    /**
     * Get the value for the editor component. This method is normally
     * overridden to obtain the value in the way, specific for the text
     * component, check box or combo box.
     *
     * @return value the value of the component (String, Boolean or Number).
     */
    public Object getCellEditorValue()
    {
      return value;
    }

    /**
     * The default method returns true for the {@link MouseEvent} and false
     * for any other events.
     *
     * @param event the event to check
     *
     * @return true if the passed event is the mouse event and false otherwise.
     */
    public boolean isCellEditable(EventObject event)
    {
      if (event == null || !(event instanceof MouseEvent) ||
          (((MouseEvent) event).getClickCount() >= getClickCountToStart()))
        return true;
      return false;
    } // isCellEditable()

    /**
     * Returns true to indicate that the editing cell can be selected.
     *
     * The default method returns true without action but may be overridden
     * in derived classes for more specific behavior.
     *
     * @param event unused in default method
     *
     * @return true always
     */
    public boolean shouldSelectCell(EventObject event)
    {
      // return true to indicate that the editing cell may be selected
      return true;
    }

    /**
     * Finish the cell editing session. This method notifies the registered
     * cell editor listeners (including the table) that the editing has been
     * stopped.
     *
     * @return boolean
     */
    public boolean stopCellEditing()
    {
      fireEditingStopped();
      return true;
    } // stopCellEditing()

    /**
     * Cancel the cell editing session. This method notifies the registered
     * cell editor listeners (including the table) that the editing has been
     * canceled.
     */
    public void cancelCellEditing()
    {
      fireEditingCanceled();
    } // cancelCellEditing()

    /**
     * Start editing session and returns true to indicate the editing has begun.
     * The default method returns true without action but may be overridden
     * in derived classes for more specific behavior.
     *
     * @param event  the event.
     *
     * @return true, always
     */
    public boolean startCellEditing(EventObject event)
    {
      // return true to indicate that editing has begun
      return true;
    } // startCellEditing()

    /**
     * This event is fired by the editor component (for instance, by pressing
     * ENTER in the {@link JTextField}. The default method delegates call to
     * the {@link #stopCellEditing}, finishing the editing session.
     *
     * @param event unused in default method
     */
    public void actionPerformed(ActionEvent event)
    {
      stopCellEditing();
    } // actionPerformed()

    /**
     * This event is fired by the editor component.The default method delegates
     * call to the {@link #stopCellEditing}, finishing the editing session.
     *
     * @param event unused in default method
     */
    public void itemStateChanged(ItemEvent event)
    {
      stopCellEditing();
    } // itemStateChanged()

    /**
     * Notify the registered listeners (including the table) that the editing
     * has been completed.
     */
    void fireEditingStopped()
    {
      CellEditorListener[] listeners = getCellEditorListeners();
      for (int index = 0; index < listeners.length; index++)
        listeners[index].editingStopped(changeEvent);

    }

    /**
     * Notify the registered listeners (including the table) that the editing
     * has been canceled.
     */
    void fireEditingCanceled()
    {
      CellEditorListener[] listeners = getCellEditorListeners();
      for (int index = 0; index < listeners.length; index++)
        listeners[index].editingCanceled(changeEvent);
    }
  } // EditorDelegate

  /**
   * Provides getter and setter methods to work with the text component.
   *
   * @author Audrius Meskauskas (audriusa@Bioinformatics.org)
   */
  private class JTextFieldDelegate extends EditorDelegate
  {
    /**
     * Use the serial version UID for interoperability.
     */
    private static final long serialVersionUID = 1;

    /**
     * Set the value for the editor component.
     *
     * @param aValue the value to set (toString() will be called).
     */
    public void setValue(Object aValue)
    {
      value = aValue;
      JTextField f = (JTextField) editorComponent;
      if (value == null)
        f.setText("");
      else
        f.setText(value.toString());
    }

    /**
     * Get the value for the editor component.
     *
     * @return value the value of the component (String)
     */
    public Object getCellEditorValue()
    {
      JTextField f = (JTextField) editorComponent;
      return value = f.getText();
    }
  }

  /**
   * Provides getter and setter methods to work with the combo box.
   *
   * @author Audrius Meskauskas (audriusa@Bioinformatics.org)
   */
  private class JComboBoxDelegate extends EditorDelegate
  {
    /**
     * Use the serial version UID for interoperability.
     */
    private static final long serialVersionUID = 1;

    /**
     * Set the value for the editor component.
     *
     * @param aValue the value to set.
     */
    public void setValue(Object aValue)
    {
      value = aValue;
      JComboBox c = (JComboBox) editorComponent;
      if (value != null)
        c.setSelectedItem(value);
    }

    /**
     * Get the value for the editor component.
     *
     * @return value the value of the component (as String)
     */
    public Object getCellEditorValue()
    {
      JComboBox c = (JComboBox) editorComponent;
      return value = c.getSelectedItem();
    }

    /**
     * Returns true to indicate that the editing cell can be selected. If the
     * check box is not editable, expands it. If it is editable, brings
     * focus to the editor field.
     *
     * @param event unused in default method
     *
     * @return true always
     */
    public boolean shouldSelectCell(EventObject event)
    {
      JComboBox c = (JComboBox) editorComponent;
      if (!c.isEditable)
        c.showPopup();
      return true;
    }
  }

  /**
   * Provides getter and setter methods to work with the check box.
   *
   * @author Audrius Meskauskas (audriusa@Bioinformatics.org)
   */
  private class JCheckBoxDelegate extends EditorDelegate
  {
    /**
     * Use the serial version UID for interoperability.
     */
    private static final long serialVersionUID = 1;

    /**
     * Set the value for the editor component.
     *
     * @param value the value to set (must be Boolean).
     */
    public void setValue(Object value)
    {
      JCheckBox c = (JCheckBox) editorComponent;

      if (value == null)
        c.setSelected(false);
      else
        c.setSelected( ((Boolean) value).booleanValue());
    }

    /**
     * Get the value for the editor component.
     *
     * @return value the value of the component (must be CharSequence)
     */
    public Object getCellEditorValue()
    {
      JCheckBox c = (JCheckBox) editorComponent;
      value = c.isSelected() ? Boolean.TRUE : Boolean.FALSE;
      return value;
    }
  }

  /**
   * The Swing JComponent, performing the editing session.
   */
  protected JComponent editorComponent;

  /**
   * The editor delegate, responsible for listening the {@link #editorComponent}
   * events and getting/setting its value.
   */
  protected EditorDelegate delegate;

  /**
   * The number of the mouse clicks, required to start the editing session.
   */
  protected int clickCountToStart;

  /**
   * Create the DefaultCellEditor that uses the text field as its editor
   * component (appropriate for the text content)
   *
   * @param textfield the text field as will be used as the editor component
   */
  public DefaultCellEditor(JTextField textfield)
  {
    editorComponent = textfield;
    clickCountToStart = 2;
    delegate = new JTextFieldDelegate();
    textfield.addActionListener(delegate);
  } // DefaultCellEditor()

  /**
   * Constructor DefaultCellEditor that uses the checkbox (appropriate
   * for boolean values)
   *
   * @param checkbox the checkbox that will be used with this editor.
   */
  public DefaultCellEditor(JCheckBox checkbox)
  {
    editorComponent = checkbox;
    clickCountToStart = 1;
    delegate = new JCheckBoxDelegate();
    checkbox.addActionListener(delegate);
  } // DefaultCellEditor()

  /**
   * Constructor DefaultCellEditor that uses the combo box.
   *
   * @param combobox the combo box that will be used with this editor.
   */
  public DefaultCellEditor(JComboBox combobox)
  {
    editorComponent = combobox;
    clickCountToStart = 1;
    delegate = new JComboBoxDelegate();
    combobox.addActionListener(delegate);
  } // DefaultCellEditor()

  /**
   * Get the component that performs the editing sessions. It is the same
   * component that was passed in constructor.
   *
   * @return the component, performing the editing sessions.
   */
  public Component getComponent()
  {
    return editorComponent;
  } // getComponent()

  /**
   * Get the number of mouse clicks, required to start the editing session.
   *
   * @return int the number of mouse clicks, required to start the session
   */
  public int getClickCountToStart()
  {
    return clickCountToStart;
  } // getClickCountToStart()

  /**
   * Set the number of mouse clicks, required to start the editing session.
   *
   * @param count the number of clicks, required to start the session
   */
  public void setClickCountToStart(int count)
  {
    clickCountToStart = count;
  } // setClickCountToStart()

  /**
   * Get the value, currently being displayed by the editor component. The
   * call is forwarded to the {@link #delegate}.
   *
   * @return Object the value (class depends on the editor component)
   */
  public Object getCellEditorValue()
  {
    return delegate.getCellEditorValue();
  } // getCellEditorValue()

  /**
   * Forwards call to the {@link #delegate}.
   *
   * @param event forwarded to the delegate.
   *
   * @return boolean returned by delegate
   */
  public boolean isCellEditable(EventObject event)
  {
    return delegate.isCellEditable(event);
  } // isCellEditable()

  /**
   * Forwards call to the {@link #delegate}.
   *
   * @param event forwarded to the delegate.
   *
   * @return boolean returned by delegate
   */
  public boolean shouldSelectCell(EventObject event)
  {
    return delegate.shouldSelectCell(event);
  } // shouldSelectCell()

  /**
   * Forwards call to the {@link #delegate}.
   *
   * @return boolean returned by delegate
   */
  public boolean stopCellEditing()
  {
    return delegate.stopCellEditing();
  } // stopCellEditing()

  /**
   * Forwards call to the {@link #delegate}.
   */
  public void cancelCellEditing()
  {
    delegate.cancelCellEditing();
  } // cancelCellEditing()

  /**
   * Sets an initial value for the editor.
   * This will cause the editor to stopEditing and lose any partially
   * edited value if the editor is editing when this method is called.
   * Returns the component that should be added to the client's Component
   * hierarchy. Once installed in the client's hierarchy this component will
   * then be able to draw and receive user input.
   *
   * @param tree - the JTree that is asking the editor to edit; this
   * parameter can be null
   * @param value - the value of the cell to be edited
   * @param isSelected - true is the cell is to be renderer with selection
   * highlighting
   * @param expanded - true if the node is expanded
   * @param leaf - true if the node is a leaf node
   * @param row - the row index of the node being edited
   *
   * @return Component the component for editing
   */
  public Component getTreeCellEditorComponent(JTree tree, Object value,
                                              boolean isSelected,
                                              boolean expanded, boolean leaf,
                                              int row)
  {
    delegate.setValue(value);
    return editorComponent;
  } // getTreeCellEditorComponent()

  /**
   * Get the cell editor component that will perform the editing session. If
   * returned once, the same component is also returned on the repetetive calls
   * again (reused).
   *
   * @param table the table where the editing is performed
   * @param value the current value of the table. It is set as the initial
   *        component value.
   * @param isSelected if true, the cell is currently selected
   * @param row the row of the cell being edited
   * @param column the column of the cell being edited
   *
   * @return Component the component that will perform the editing session
   */
  public Component getTableCellEditorComponent(JTable table, Object value,
                                               boolean isSelected, int row,
                                               int column)
  {
    // NOTE: as specified by Sun, we don't call new() everytime, we return
    // editorComponent on each call to getTableCellEditorComponent or
    // getTreeCellEditorComponent.
    delegate.setValue(value);
    return editorComponent;
  } // getTableCellEditorComponent()

}
