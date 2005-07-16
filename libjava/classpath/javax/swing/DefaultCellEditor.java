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
import java.io.Serializable;
import java.util.EventObject;

import javax.swing.table.TableCellEditor;
import javax.swing.tree.TreeCellEditor;

/**
 * DefaultCellEditor
 * @author	Andrew Selkirk
 * @version	1.0
 */
public class DefaultCellEditor
  extends AbstractCellEditor
  implements TableCellEditor, TreeCellEditor
{
  private static final long serialVersionUID = 3564035141373880027L;

  /**
   * EditorDelegate
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
     *
     * @param value0 TODO
     */
    protected EditorDelegate()
    {
    }

    /**
     * setValue
     *
     * @param event TODO
     */
    public void setValue(Object event)
    {
    }

   /**
     * getCellEditorValue
     * 
     * @returns Object
     */
    public Object getCellEditorValue()
    {
      return null; // TODO
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
      return false; // TODO
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
      return false; // TODO
    } // shouldSelectCell()

    /**
     * stopCellEditing
     * 
     * @returns boolean
     */
    public boolean stopCellEditing()
    {
      return false; // TODO
    } // stopCellEditing()

    /**
     * cancelCellEditing
     */
    public void cancelCellEditing()
    {
      // TODO
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
      return false; // TODO
    } // startCellEditing()

    /**
     * actionPerformed
     * 
     * @param event TODO
     */
    public void actionPerformed(ActionEvent event)
    {
      // TODO
    } // actionPerformed()

    /**
     * itemStateChanged
     * 
     * @param event TODO
     */
    public void itemStateChanged(ItemEvent event)
    {
      // TODO
    } // itemStateChanged()

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
    // TODO
  } // DefaultCellEditor()

  /**
   * Constructor DefaultCellEditor
   * 
   * @param checkbox TODO
   */
  public DefaultCellEditor(JCheckBox checkbox)
  {
    // TODO
  } // DefaultCellEditor()

  /**
   * Constructor DefaultCellEditor
   * 
   * @param combobox TODO
   */
  public DefaultCellEditor(JComboBox combobox)
  {
    // TODO
  } // DefaultCellEditor()

  /**
   * getComponent
   * 
   * @returns Component
   */
  public Component getComponent()
  {
    return null; // TODO
  } // getComponent()

  /**
   * getClickCountToStart
   * 
   * @returns int
   */
  public int getClickCountToStart()
  {
    return 0; // TODO
  } // getClickCountToStart()

  /**
   * setClickCountToStart
   * 
   * @param count TODO
   */
  public void setClickCountToStart(int count)
  {
    // TODO
  } // setClickCountToStart()

  /**
   * getCellEditorValue
   * 
   * @returns Object
   */
  public Object getCellEditorValue()
  {
    return null; // TODO
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
    return false; // TODO
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
    return false; // TODO
  } // shouldSelectCell()

  /**
   * stopCellEditing
   * 
   * @returns boolean
   */
  public boolean stopCellEditing()
  {
    return false; // TODO
  } // stopCellEditing()

  /**
   * cancelCellEditing
   */
  public void cancelCellEditing()
  {
    // TODO
  } // cancelCellEditing()

  /**
   * getTreeCellEditorComponent
   * 
   * @param tree TODO
   * @param value TODO
   * @param isSelected TODO
   * @param expanded TODO
   * @param leaf TODO
   * @param row TODO
   *
   * @returns Component
   */
  public Component getTreeCellEditorComponent(JTree tree, Object value,
                                              boolean isSelected,
                                              boolean expanded, boolean leaf,
                                              int row)
  {
    return null; // TODO
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
  public Component getTableCellEditorComponent(JTable tree, Object value,
                                               boolean isSelected, int row,
                                               int column)
  {
    return null; // TODO
  } // getTableCellEditorComponent()
}
