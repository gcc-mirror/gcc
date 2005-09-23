/* DefaultCellEditor.java --
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
 *
 * @status mostly unimplemented
 */
public class DefaultCellEditor
  extends AbstractCellEditor
  implements TableCellEditor, TreeCellEditor
{
  private static final long serialVersionUID = 3564035141373880027L;

  /**
   * Delegates a couple of method calls (such as {@link #isCellEditable)
   * to the component it contains and listens for events that indicate
   * that editing has stopped.
   */
  protected class EditorDelegate
    implements ActionListener, ItemListener, Serializable
  {
    private static final long serialVersionUID = -1420007406015481933L;

    /**
     * value
     */
    protected Object value;

    /**
     * Constructor EditorDelegate
     */
    protected EditorDelegate()
    {
    }

    /**
     * setValue
     *
     * @param event TODO
     */
    public void setValue(Object value)
    {
      // TODO: should be setting the value in the editorComp
      this.value = value;
    }

   /**
     * getCellEditorValue
     * 
     * @returns Object
     */
    public Object getCellEditorValue()
    {
      // TODO: should be getting the updated value from the editorComp
      return value;
    } // getCellEditorValue()

    /**
     * isCellEditable
     * 
     * @param event TODO
     *
     * @returns boolean
     */
    public boolean isCellEditable(EventObject event)
    {
      if (event == null || !(event instanceof MouseEvent) ||
          (((MouseEvent) event).getClickCount() >= getClickCountToStart()))
        return true;
      return false;
    } // isCellEditable()

    /**
     * shouldSelectCell
     * 
     * @param event TODO
     *
     * @returns boolean
     */
    public boolean shouldSelectCell(EventObject event)
    {
      // return true to indicate that the editing cell may be selected
      return true;
    } // shouldSelectCell()

    /**
     * stopCellEditing
     * 
     * @returns boolean
     */
    public boolean stopCellEditing()
    {
      fireEditingStopped();
      return true;
    } // stopCellEditing()

    /**
     * cancelCellEditing
     */
    public void cancelCellEditing()
    {
      fireEditingCanceled();
    } // cancelCellEditing()

    /**
     * startCellEditing
     * 
     * @param event TODO
     *
     * @returns boolean
     */
    public boolean startCellEditing(EventObject event)
    {
      // return true to indicate that editing has begun
      return true;
    } // startCellEditing()

    /**
     * actionPerformed
     * 
     * @param event TODO
     */
    public void actionPerformed(ActionEvent event)
    {
      stopCellEditing();
    } // actionPerformed()

    /**
     * itemStateChanged
     * 
     * @param event TODO
     */
    public void itemStateChanged(ItemEvent event)
    {
      stopCellEditing();
    } // itemStateChanged()

    void fireEditingStopped()
    {
      CellEditorListener[] listeners = getCellEditorListeners();
      for (int index = 0; index < listeners.length; index++)
        listeners[index].editingStopped(changeEvent);
      
    }
    
    void fireEditingCanceled()
    {
      CellEditorListener[] listeners = getCellEditorListeners();
      for (int index = 0; index < listeners.length; index++)
        listeners[index].editingCanceled(changeEvent);
    }
  } // EditorDelegate

	/**
   * editorComponent
   */
  protected JComponent editorComponent;

  /**
   * delegate
   */
  protected EditorDelegate delegate;

  /**
   * clickCountToStart
   */
  protected int clickCountToStart;

  /**
   * Constructor DefaultCellEditor
   * 
   * @param textfield TODO
   */
  public DefaultCellEditor(JTextField textfield)
  {
    editorComponent = textfield;
    clickCountToStart = 3;
  } // DefaultCellEditor()

  /**
   * Constructor DefaultCellEditor
   * 
   * @param checkbox TODO
   */
  public DefaultCellEditor(JCheckBox checkbox)
  {
    editorComponent = checkbox;
    clickCountToStart = 1;
  } // DefaultCellEditor()

  /**
   * Constructor DefaultCellEditor
   * 
   * @param combobox TODO
   */
  public DefaultCellEditor(JComboBox combobox)
  {
    editorComponent = combobox;
    clickCountToStart = 1;
  } // DefaultCellEditor()

  /**
   * getComponent
   * 
   * @returns Component
   */
  public Component getComponent()
  {
    return editorComponent; 
  } // getComponent()

  /**
   * getClickCountToStart
   * 
   * @returns int
   */
  public int getClickCountToStart()
  {
    return clickCountToStart;
  } // getClickCountToStart()

  /**
   * setClickCountToStart
   * 
   * @param count TODO
   */
  public void setClickCountToStart(int count)
  {
    clickCountToStart = count;
  } // setClickCountToStart()

  /**
   * getCellEditorValue
   * 
   * @returns Object
   */
  public Object getCellEditorValue()
  {
    return delegate.getCellEditorValue();
  } // getCellEditorValue()

  /**
   * isCellEditable
   * 
   * @param event TODO
   *
   * @returns boolean
   */
  public boolean isCellEditable(EventObject event)
  {
    return delegate.isCellEditable(event);
  } // isCellEditable()

  /**
   * shouldSelectCell
   * 
   * @param event TODO
   *
   * @returns boolean
   */
  public boolean shouldSelectCell(EventObject event)
  {
    return delegate.shouldSelectCell(event);
  } // shouldSelectCell()

  /**
   * stopCellEditing
   * 
   * @returns boolean
   */
  public boolean stopCellEditing()
  {
    return delegate.stopCellEditing();
  } // stopCellEditing()

  /**
   * cancelCellEditing
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
   * @returns Component the component for editing
   */
  public Component getTreeCellEditorComponent(JTree tree, Object value,
                                              boolean isSelected,
                                              boolean expanded, boolean leaf,
                                              int row)
  {
    if (editorComponent instanceof JTextField)
      {
        ((JTextField)editorComponent).setText(value.toString());
        delegate = new EditorDelegate();
        ((JTextField)editorComponent).addActionListener(delegate);
      }
    else if (editorComponent instanceof JCheckBox)
      {
        ((JCheckBox)editorComponent).setText(value.toString());
        delegate = new EditorDelegate();
        ((JCheckBox)editorComponent).addActionListener(delegate);
      }
    else if (editorComponent instanceof JComboBox)
      {
        ((JComboBox)editorComponent).setSelectedItem(value.toString());
        delegate = new EditorDelegate();
        ((JComboBox)editorComponent).addActionListener(delegate);
      }

    return editorComponent;
  } // getTreeCellEditorComponent()

  /**
   * getTableCellEditorComponent
   * 
   * @param tree TODO
   * @param value TODO
   * @param isSelected TODO
   * @param row TODO
   * @param column TODO
   *
   * @returns Component
   */
  public Component getTableCellEditorComponent(JTable table, Object value,
                                               boolean isSelected, int row,
                                               int column)
  {
    // NOTE: as specified by Sun, we don't call new() everytime, we return 
    // editorComponent on each call to getTableCellEditorComponent or
    // getTreeCellEditorComponent.  However, currently JTextFields have a
    // problem with getting rid of old text, so without calling new() there
    // are some strange results.  If you edit more than one cell in the table
    // text from previously edited cells may unexpectedly show up in the 
    // cell you are currently editing.  This will be fixed automatically
    // when JTextField is fixed.
    if (editorComponent instanceof JTextField)
      {
        ((JTextField)editorComponent).setText(value.toString());
        delegate = new EditorDelegate();
        ((JTextField)editorComponent).addActionListener(delegate);
      }
    else
      {
        // TODO
      }
    return editorComponent;
  } // getTableCellEditorComponent()


}
