/* BasicTableUI.java --
   Copyright (C) 2004 Free Software Foundation, Inc.

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


package javax.swing.plaf.basic;

import java.awt.Color;
import java.awt.Component;
import java.awt.ComponentOrientation;
import java.awt.Dimension;
import java.awt.Graphics;
import java.awt.Point;
import java.awt.Rectangle;
import java.awt.event.ActionEvent;
import java.awt.event.FocusEvent;
import java.awt.event.FocusListener;
import java.awt.event.KeyEvent;
import java.awt.event.KeyListener;
import java.awt.event.MouseEvent;
import java.beans.PropertyChangeEvent;
import java.beans.PropertyChangeListener;

import javax.swing.AbstractAction;
import javax.swing.Action;
import javax.swing.ActionMap;
import javax.swing.CellRendererPane;
import javax.swing.DefaultCellEditor;
import javax.swing.DefaultListSelectionModel;
import javax.swing.InputMap;
import javax.swing.JComponent;
import javax.swing.JTable;
import javax.swing.ListSelectionModel;
import javax.swing.LookAndFeel;
import javax.swing.SwingUtilities;
import javax.swing.TransferHandler;
import javax.swing.UIManager;
import javax.swing.border.Border;
import javax.swing.event.ChangeEvent;
import javax.swing.event.MouseInputListener;
import javax.swing.plaf.ActionMapUIResource;
import javax.swing.plaf.ComponentUI;
import javax.swing.plaf.TableUI;
import javax.swing.table.TableCellEditor;
import javax.swing.table.TableCellRenderer;
import javax.swing.table.TableColumn;
import javax.swing.table.TableColumnModel;
import javax.swing.table.TableModel;

public class BasicTableUI extends TableUI
{
  public static ComponentUI createUI(JComponent comp) 
  {
    return new BasicTableUI();
  }

  protected FocusListener focusListener;  
  protected KeyListener keyListener;   
  protected MouseInputListener  mouseInputListener;   
  protected CellRendererPane rendererPane;   
  protected JTable table;

  /** The normal cell border. */
  Border cellBorder;

  /** The action bound to KeyStrokes. */
  TableAction action;

  /**
   * Listens for changes to the tables properties.
   */
  private PropertyChangeListener propertyChangeListener;

  /**
   * Handles key events for the JTable. Key events should be handled through
   * the InputMap/ActionMap mechanism since JDK1.3. This class is only there
   * for backwards compatibility.
   * 
   * @author Roman Kennke (kennke@aicas.com)
   */
  public class KeyHandler implements KeyListener
  {

    /**
     * Receives notification that a key has been pressed and released.
     * Activates the editing session for the focused cell by pressing the
     * character keys.
     *
     * @param event the key event
     */
    public void keyTyped(KeyEvent event)
    {
      // Key events should be handled through the InputMap/ActionMap mechanism
      // since JDK1.3. This class is only there for backwards compatibility.
      
      // Editor activation is a specific kind of response to ''any''
      // character key. Hence it is handled here.
      if (!table.isEditing() && table.isEnabled())
        {
          int r = table.getSelectedRow();
          int c = table.getSelectedColumn();
          if (table.isCellEditable(r, c))
            table.editCellAt(r, c);
        }
    }

    /**
     * Receives notification that a key has been pressed.
     *
     * @param event the key event
     */
    public void keyPressed(KeyEvent event)
    {
      // Key events should be handled through the InputMap/ActionMap mechanism
      // since JDK1.3. This class is only there for backwards compatibility.
    }

    /**
     * Receives notification that a key has been released.
     *
     * @param event the key event
     */
    public void keyReleased(KeyEvent event)
    {
      // Key events should be handled through the InputMap/ActionMap mechanism
      // since JDK1.3. This class is only there for backwards compatibility.
    }
  }

  public class FocusHandler implements FocusListener
  {
    public void focusGained(FocusEvent e)
    {
      // The only thing that is affected by a focus change seems to be
      // how the lead cell is painted. So we repaint this cell.
      repaintLeadCell();
    }

    public void focusLost(FocusEvent e)
    {
      // The only thing that is affected by a focus change seems to be
      // how the lead cell is painted. So we repaint this cell.
      repaintLeadCell();
    }

    /**
     * Repaints the lead cell in response to a focus change, to refresh
     * the display of the focus indicator.
     */
    private void repaintLeadCell()
    {
      int rowCount = table.getRowCount();
      int columnCount = table.getColumnCount();
      int rowLead = table.getSelectionModel().getLeadSelectionIndex();
      int columnLead = table.getColumnModel().getSelectionModel().
                                                       getLeadSelectionIndex();
      if (rowLead >= 0 && rowLead < rowCount && columnLead >= 0
          && columnLead < columnCount)
        {
          Rectangle dirtyRect = table.getCellRect(rowLead, columnLead, false);
          table.repaint(dirtyRect);
        }
    }
  }

  public class MouseInputHandler implements MouseInputListener
  {
    Point begin, curr;

    private void updateSelection(boolean controlPressed)
    {
      // Update the rows
      int lo_row = table.rowAtPoint(begin);
      int hi_row  = table.rowAtPoint(curr);
      ListSelectionModel rowModel = table.getSelectionModel();
      if (lo_row != -1 && hi_row != -1)
        {
          if (controlPressed && rowModel.getSelectionMode() 
              != ListSelectionModel.SINGLE_SELECTION)
            rowModel.addSelectionInterval(lo_row, hi_row);
          else
            rowModel.setSelectionInterval(lo_row, hi_row);
        }
      
      // Update the columns
      int lo_col = table.columnAtPoint(begin);
      int hi_col = table.columnAtPoint(curr);
      ListSelectionModel colModel = table.getColumnModel().
        getSelectionModel();
      if (lo_col != -1 && hi_col != -1)
        {
          if (controlPressed && colModel.getSelectionMode() != 
              ListSelectionModel.SINGLE_SELECTION)
            colModel.addSelectionInterval(lo_col, hi_col);
          else
            colModel.setSelectionInterval(lo_col, hi_col);
        }
    }
    
    /**
     * For the double click, start the cell editor.
     */
    public void mouseClicked(MouseEvent e)
    {
      Point p = e.getPoint();
      int row = table.rowAtPoint(p);
      int col = table.columnAtPoint(p);
      if (table.isCellEditable(row, col))
        {
          // If the cell editor is the default editor, we request the
          // number of the required clicks from it. Otherwise,
          // require two clicks (double click).
          TableCellEditor editor = table.getCellEditor(row, col);
          if (editor instanceof DefaultCellEditor)
            {
              DefaultCellEditor ce = (DefaultCellEditor) editor;
              if (e.getClickCount() < ce.getClickCountToStart())
                return;
            }
          table.editCellAt(row, col);
        }
    }

    public void mouseDragged(MouseEvent e) 
    {
      if (table.isEnabled())
        {
          curr = new Point(e.getX(), e.getY());
          updateSelection(e.isControlDown());
        }
    }

    public void mouseEntered(MouseEvent e)
    {
      // Nothing to do here.
    }

    public void mouseExited(MouseEvent e)
    {
      // Nothing to do here.
    }

    public void mouseMoved(MouseEvent e)
    {
      // Nothing to do here.
    }

    public void mousePressed(MouseEvent e) 
    {
      if (table.isEnabled())
        {
          ListSelectionModel rowModel = table.getSelectionModel();
          ListSelectionModel colModel = table.getColumnModel().getSelectionModel();
          int rowLead = rowModel.getLeadSelectionIndex();
          int colLead = colModel.getLeadSelectionIndex();

          begin = new Point(e.getX(), e.getY());
          curr = new Point(e.getX(), e.getY());
          //if control is pressed and the cell is already selected, deselect it
          if (e.isControlDown() && table.isCellSelected(
              table.rowAtPoint(begin), table.columnAtPoint(begin)))
            {                                       
              table.getSelectionModel().
              removeSelectionInterval(table.rowAtPoint(begin), 
                                      table.rowAtPoint(begin));
              table.getColumnModel().getSelectionModel().
              removeSelectionInterval(table.columnAtPoint(begin), 
                                      table.columnAtPoint(begin));
            }
          else
            updateSelection(e.isControlDown());

          // If we were editing, but the moved to another cell, stop editing
          if (rowLead != rowModel.getLeadSelectionIndex() ||
              colLead != colModel.getLeadSelectionIndex())
            if (table.isEditing())
              table.editingStopped(new ChangeEvent(e));

          // Must request focus explicitly.
          table.requestFocusInWindow();
        }
    }

    public void mouseReleased(MouseEvent e) 
    {
      if (table.isEnabled())
        {
          begin = null;
          curr = null;
        }
    }
  }

  /**
   * Listens for changes to the model property of the JTable and adjusts some
   * settings.
   *
   * @author Roman Kennke (kennke@aicas.com)
   */
  private class PropertyChangeHandler implements PropertyChangeListener
  {
    /**
     * Receives notification if one of the JTable's properties changes.
     *
     * @param ev the property change event
     */
    public void propertyChange(PropertyChangeEvent ev)
    {
      String propName = ev.getPropertyName();
      if (propName.equals("model"))
        {
          ListSelectionModel rowSel = table.getSelectionModel();
          rowSel.clearSelection();
          ListSelectionModel colSel = table.getColumnModel().getSelectionModel();
          colSel.clearSelection();
          TableModel model = table.getModel();

          // Adjust lead and anchor selection indices of the row and column
          // selection models.
          if (model.getRowCount() > 0)
            {
              rowSel.setAnchorSelectionIndex(0);
              rowSel.setLeadSelectionIndex(0);
            }
          else
            {
              rowSel.setAnchorSelectionIndex(-1);
              rowSel.setLeadSelectionIndex(-1);
            }
          if (model.getColumnCount() > 0)
            {
              colSel.setAnchorSelectionIndex(0);
              colSel.setLeadSelectionIndex(0);
            }
          else
            {
              colSel.setAnchorSelectionIndex(-1);
              colSel.setLeadSelectionIndex(-1);
            }
        }
    }
  }

  protected FocusListener createFocusListener() 
  {
    return new FocusHandler();
  }

  protected MouseInputListener createMouseInputListener() 
  {
    return new MouseInputHandler();
  }


  /**
   * Creates and returns a key listener for the JTable.
   *
   * @return a key listener for the JTable
   */
  protected KeyListener createKeyListener()
  {
    return new KeyHandler();
  }

  /**
   * Return the maximum size of the table. The maximum height is the row 
    * height times the number of rows. The maximum width is the sum of 
    * the maximum widths of each column.
    * 
    *  @param comp the component whose maximum size is being queried,
    *  this is ignored.
    *  @return a Dimension object representing the maximum size of the table,
    *  or null if the table has no elements.
   */
  public Dimension getMaximumSize(JComponent comp) 
  {
    int maxTotalColumnWidth = 0;
    for (int i = 0; i < table.getColumnCount(); i++)
      maxTotalColumnWidth += table.getColumnModel().getColumn(i).getMaxWidth();

    return new Dimension(maxTotalColumnWidth, getHeight());
  }

  /**
   * Return the minimum size of the table. The minimum height is the row 
    * height times the number of rows. The minimum width is the sum of 
    * the minimum widths of each column.
    * 
    *  @param comp the component whose minimum size is being queried,
    *  this is ignored.
    *  @return a Dimension object representing the minimum size of the table,
    *  or null if the table has no elements.
   */
  public Dimension getMinimumSize(JComponent comp) 
  {
    int minTotalColumnWidth = 0;
    for (int i = 0; i < table.getColumnCount(); i++)
      minTotalColumnWidth += table.getColumnModel().getColumn(i).getMinWidth();

    return new Dimension(minTotalColumnWidth, getHeight());
  }

  /**
   * Returns the preferred size for the table of that UI.
   *
   * @param comp ignored, the <code>table</code> field is used instead
   *
   * @return the preferred size for the table of that UI
   */
  public Dimension getPreferredSize(JComponent comp) 
  {
    int prefTotalColumnWidth = 0;
    TableColumnModel tcm = table.getColumnModel();

    for (int i = 0; i < tcm.getColumnCount(); i++)
      {
        TableColumn col = tcm.getColumn(i);
        prefTotalColumnWidth += col.getPreferredWidth();
      }

    return new Dimension(prefTotalColumnWidth, getHeight());
  }

  /**
   * Returns the table height. This helper method is used by
   * {@link #getMinimumSize(JComponent)}, {@link #getPreferredSize(JComponent)}
   * and {@link #getMaximumSize(JComponent)} to determine the table height.
   * 
   * @return the table height
   */
  private int getHeight()
  {
    int height = 0;
    int rowCount = table.getRowCount(); 
    if (rowCount > 0 && table.getColumnCount() > 0)
      {
        Rectangle r = table.getCellRect(rowCount - 1, 0, true);
        height = r.y + r.height;
      }
    return height;
  }

  protected void installDefaults() 
  {
    LookAndFeel.installColorsAndFont(table, "Table.background",
                                     "Table.foreground", "Table.font");
    table.setGridColor(UIManager.getColor("Table.gridColor"));
    table.setSelectionForeground(UIManager.getColor("Table.selectionForeground"));
    table.setSelectionBackground(UIManager.getColor("Table.selectionBackground"));
    table.setOpaque(true);
  }

  /**
   * Installs keyboard actions on the table.
   */
  protected void installKeyboardActions() 
  {
    // Install the input map.
    InputMap inputMap =
      (InputMap) SharedUIDefaults.get("Table.ancestorInputMap");
    SwingUtilities.replaceUIInputMap(table,
                                 JComponent.WHEN_ANCESTOR_OF_FOCUSED_COMPONENT,
                                 inputMap);

    // FIXME: The JDK uses a LazyActionMap for parentActionMap
    SwingUtilities.replaceUIActionMap(table, getActionMap());

  }

  /**
   * Fetches the action map from  the UI defaults, or create a new one
   * if the action map hasn't been initialized.
   *
   * @return the action map
   */
  private ActionMap getActionMap()
  {
    ActionMap am = (ActionMap) UIManager.get("Table.actionMap");
    if (am == null)
      {
        am = createDefaultActions();
        UIManager.getLookAndFeelDefaults().put("Table.actionMap", am);
      }
    return am;
  }

  private ActionMap createDefaultActions()
  {
    ActionMapUIResource am = new ActionMapUIResource();
    Action action = new TableAction();

    am.put("cut", TransferHandler.getCutAction());
    am.put("copy", TransferHandler.getCopyAction());
    am.put("paste", TransferHandler.getPasteAction());

    am.put("cancel", action);
    am.put("selectAll", action);
    am.put("clearSelection", action);
    am.put("startEditing", action);

    am.put("selectNextRow", action);
    am.put("selectNextRowCell", action);
    am.put("selectNextRowExtendSelection", action);
    am.put("selectNextRowChangeLead", action);

    am.put("selectPreviousRow", action);
    am.put("selectPreviousRowCell", action);
    am.put("selectPreviousRowExtendSelection", action);
    am.put("selectPreviousRowChangeLead", action);

    am.put("selectNextColumn", action);
    am.put("selectNextColumnCell", action);
    am.put("selectNextColumnExtendSelection", action);
    am.put("selectNextColumnChangeLead", action);

    am.put("selectPreviousColumn", action);
    am.put("selectPreviousColumnCell", action);
    am.put("selectPreviousColumnExtendSelection", action);
    am.put("selectPreviousColumnChangeLead", action);

    am.put("scrollLeftChangeSelection", action);
    am.put("scrollLeftExtendSelection", action);
    am.put("scrollRightChangeSelection", action);
    am.put("scrollRightExtendSelection", action);

    am.put("scrollUpChangeSelection", action);
    am.put("scrollUpExtendSelection", action);
    am.put("scrollDownChangeSelection", action);
    am.put("scrolldownExtendSelection", action);

    am.put("selectFirstColumn", action);
    am.put("selectFirstColumnExtendSelection", action);
    am.put("selectLastColumn", action);
    am.put("selectLastColumnExtendSelection", action);

    am.put("selectFirstRow", action);
    am.put("selectFirstRowExtendSelection", action);
    am.put("selectLastRow", action);
    am.put("selectLastRowExtendSelection", action);

    am.put("addToSelection", action);
    am.put("toggleAndAnchor", action);
    am.put("extendTo", action);
    am.put("moveSelectionTo", action);

    return am;
  }

  /**
   * This class implements the actions that we want to happen
   * when specific keys are pressed for the JTable.  The actionPerformed
   * method is called when a key that has been registered for the JTable
   * is received.
   */
  private static class TableAction
    extends AbstractAction
  {
    /**
     * What to do when this action is called.
     *
     * @param e the ActionEvent that caused this action.
     */
    public void actionPerformed(ActionEvent e)
    {
      JTable table = (JTable) e.getSource();

      DefaultListSelectionModel rowModel 
          = (DefaultListSelectionModel) table.getSelectionModel();
      DefaultListSelectionModel colModel 
          = (DefaultListSelectionModel) table.getColumnModel().getSelectionModel();

      int rowLead = rowModel.getLeadSelectionIndex();
      int rowMax = table.getModel().getRowCount() - 1;
      
      int colLead = colModel.getLeadSelectionIndex();
      int colMax = table.getModel().getColumnCount() - 1;

      // The command with which the action has been called is stored
      // in this undocumented action value. This allows us to have only
      // one Action instance to serve all keyboard input for JTable.
      String command = (String) getValue("__command__");
      if (command.equals("selectPreviousRowExtendSelection"))
        {
          rowModel.setLeadSelectionIndex(Math.max(rowLead - 1, 0));
        }
      else if (command.equals("selectLastColumn"))
        {
          colModel.setSelectionInterval(colMax, colMax);
        }
      else if (command.equals("startEditing"))
        {
          if (table.isCellEditable(rowLead, colLead))
            table.editCellAt(rowLead, colLead);
        }
      else if (command.equals("selectFirstRowExtendSelection"))
        {              
          rowModel.setLeadSelectionIndex(0);
        }
      else if (command.equals("selectFirstColumn"))
        {
          colModel.setSelectionInterval(0, 0);
        }
      else if (command.equals("selectFirstColumnExtendSelection"))
        {
          colModel.setLeadSelectionIndex(0);
        }      
      else if (command.equals("selectLastRow"))
        {
          rowModel.setSelectionInterval(rowMax, rowMax);
        }
      else if (command.equals("selectNextRowExtendSelection"))
        {
          rowModel.setLeadSelectionIndex(Math.min(rowLead + 1, rowMax));
        }
      else if (command.equals("selectFirstRow"))
        {
          rowModel.setSelectionInterval(0, 0);
        }
      else if (command.equals("selectNextColumnExtendSelection"))
        {
          colModel.setLeadSelectionIndex(Math.min(colLead + 1, colMax));
        }
      else if (command.equals("selectLastColumnExtendSelection"))
        {
          colModel.setLeadSelectionIndex(colMax);
        }
      else if (command.equals("selectPreviousColumnExtendSelection"))
        {
          colModel.setLeadSelectionIndex(Math.max(colLead - 1, 0));
        }
      else if (command.equals("selectNextRow"))
        {
          rowModel.setSelectionInterval(Math.min(rowLead + 1, rowMax),
                                        Math.min(rowLead + 1, rowMax));
        }
      else if (command.equals("scrollUpExtendSelection"))
        {
          int target;
          if (rowLead == getFirstVisibleRowIndex(table))
            target = Math.max(0, rowLead - (getLastVisibleRowIndex(table) 
                - getFirstVisibleRowIndex(table) + 1));
          else
            target = getFirstVisibleRowIndex(table);
          
          rowModel.setLeadSelectionIndex(target);
          colModel.setLeadSelectionIndex(colLead);
        }
      else if (command.equals("selectPreviousRow"))
        {
          rowModel.setSelectionInterval(Math.max(rowLead - 1, 0),
                                        Math.max(rowLead - 1, 0));
        }
      else if (command.equals("scrollRightChangeSelection"))
        {
          int target;
          if (colLead == getLastVisibleColumnIndex(table))
            target = Math.min(colMax, colLead
                              + (getLastVisibleColumnIndex(table)
                              - getFirstVisibleColumnIndex(table) + 1));
          else
            target = getLastVisibleColumnIndex(table);
          
          colModel.setSelectionInterval(target, target);
          rowModel.setSelectionInterval(rowLead, rowLead);
        }
      else if (command.equals("selectPreviousColumn"))
        {
          colModel.setSelectionInterval(Math.max(colLead - 1, 0),
                                        Math.max(colLead - 1, 0));
        }
      else if (command.equals("scrollLeftChangeSelection"))
        {
          int target;
          if (colLead == getFirstVisibleColumnIndex(table))
            target = Math.max(0, colLead - (getLastVisibleColumnIndex(table) 
                                 - getFirstVisibleColumnIndex(table) + 1));
          else
            target = getFirstVisibleColumnIndex(table);
          
          colModel.setSelectionInterval(target, target);
          rowModel.setSelectionInterval(rowLead, rowLead);
        }
      else if (command.equals("clearSelection"))
        {
          table.clearSelection();
        }
      else if (command.equals("cancel"))
        {
          // FIXME: implement other parts of "cancel" like undo-ing last
          // selection.  Right now it just calls editingCancelled if
          // we're currently editing.
          if (table.isEditing())
            table.editingCanceled(new ChangeEvent("cancel"));
        }
      else if (command.equals("selectNextRowCell")
               || command.equals("selectPreviousRowCell")
               || command.equals("selectNextColumnCell")
               || command.equals("selectPreviousColumnCell"))
        {
          // If nothing is selected, select the first cell in the table
          if (table.getSelectedRowCount() == 0 && 
              table.getSelectedColumnCount() == 0)
            {
              rowModel.setSelectionInterval(0, 0);
              colModel.setSelectionInterval(0, 0);
              return;
            }
          
          // If the lead selection index isn't selected (ie a remove operation
          // happened, then set the lead to the first selected cell in the
          // table
          if (!table.isCellSelected(rowLead, colLead))
            {
              rowModel.addSelectionInterval(rowModel.getMinSelectionIndex(), 
                                            rowModel.getMinSelectionIndex());
              colModel.addSelectionInterval(colModel.getMinSelectionIndex(), 
                                            colModel.getMinSelectionIndex());
              return;
            }
          
          // multRowsSelected and multColsSelected tell us if multiple rows or
          // columns are selected, respectively
          boolean multRowsSelected, multColsSelected;
          multRowsSelected = table.getSelectedRowCount() > 1 &&
            table.getRowSelectionAllowed();
          
          multColsSelected = table.getSelectedColumnCount() > 1 &&
            table.getColumnSelectionAllowed();
          
          // If there is just one selection, select the next cell, and wrap
          // when you get to the edges of the table.
          if (!multColsSelected && !multRowsSelected)
            {
              if (command.indexOf("Column") != -1) 
                advanceSingleSelection(colModel, colMax, rowModel, rowMax, 
                    command.equals("selectPreviousColumnCell"));
              else
                advanceSingleSelection(rowModel, rowMax, colModel, colMax, 
                    command.equals("selectPreviousRowCell"));
              return;
            }
          
          
          // rowMinSelected and rowMaxSelected are the minimum and maximum
          // values respectively of selected cells in the row selection model
          // Similarly for colMinSelected and colMaxSelected.
          int rowMaxSelected = table.getRowSelectionAllowed() ? 
            rowModel.getMaxSelectionIndex() : table.getModel().getRowCount() - 1;
          int rowMinSelected = table.getRowSelectionAllowed() ? 
            rowModel.getMinSelectionIndex() : 0; 
          int colMaxSelected = table.getColumnSelectionAllowed() ? 
            colModel.getMaxSelectionIndex() : 
            table.getModel().getColumnCount() - 1;
          int colMinSelected = table.getColumnSelectionAllowed() ? 
            colModel.getMinSelectionIndex() : 0;
          
          // If there are multiple rows and columns selected, select the next
          // cell and wrap at the edges of the selection.  
          if (command.indexOf("Column") != -1) 
            advanceMultipleSelection(table, colModel, colMinSelected,
                                     colMaxSelected, rowModel, rowMinSelected,
                                     rowMaxSelected,
                                    command.equals("selectPreviousColumnCell"),
                                    true);
          
          else
            advanceMultipleSelection(table, rowModel, rowMinSelected,
                                     rowMaxSelected, colModel, colMinSelected,
                                     colMaxSelected,
                                     command.equals("selectPreviousRowCell"),
                                     false);
        }
      else if (command.equals("selectNextColumn"))
        {
          colModel.setSelectionInterval(Math.min(colLead + 1, colMax),
                                        Math.min(colLead + 1, colMax));
        }
      else if (command.equals("scrollLeftExtendSelection"))
        {
          int target;
          if (colLead == getFirstVisibleColumnIndex(table))
            target = Math.max(0, colLead - (getLastVisibleColumnIndex(table) 
                                 - getFirstVisibleColumnIndex(table) + 1));
          else
            target = getFirstVisibleColumnIndex(table);
          
          colModel.setLeadSelectionIndex(target);
          rowModel.setLeadSelectionIndex(rowLead);
        }
      else if (command.equals("scrollDownChangeSelection"))
        {
          int target;
          if (rowLead == getLastVisibleRowIndex(table))
            target = Math.min(rowMax, rowLead + (getLastVisibleRowIndex(table)
                                      - getFirstVisibleRowIndex(table) + 1));
          else
            target = getLastVisibleRowIndex(table);
          
          rowModel.setSelectionInterval(target, target);
          colModel.setSelectionInterval(colLead, colLead);
        }
      else if (command.equals("scrollRightExtendSelection"))
        {
          int target;
          if (colLead == getLastVisibleColumnIndex(table))
            target = Math.min(colMax, colLead + (getLastVisibleColumnIndex(table) 
                - getFirstVisibleColumnIndex(table) + 1));
          else
            target = getLastVisibleColumnIndex(table);
          
          colModel.setLeadSelectionIndex(target);
          rowModel.setLeadSelectionIndex(rowLead);
        }
      else if (command.equals("selectAll"))
        {
          table.selectAll();
        }
      else if (command.equals("selectLastRowExtendSelection"))
        {
          rowModel.setLeadSelectionIndex(rowMax);
          colModel.setLeadSelectionIndex(colLead);
        }
      else if (command.equals("scrollDownExtendSelection"))
        {
          int target;
          if (rowLead == getLastVisibleRowIndex(table))
            target = Math.min(rowMax, rowLead + (getLastVisibleRowIndex(table) 
                - getFirstVisibleRowIndex(table) + 1));
          else
            target = getLastVisibleRowIndex(table);
          
          rowModel.setLeadSelectionIndex(target);
          colModel.setLeadSelectionIndex(colLead);
        }      
      else if (command.equals("scrollUpChangeSelection"))
        {
          int target;
          if (rowLead == getFirstVisibleRowIndex(table))
            target = Math.max(0, rowLead - (getLastVisibleRowIndex(table) 
                - getFirstVisibleRowIndex(table) + 1));
          else
            target = getFirstVisibleRowIndex(table);
          
          rowModel.setSelectionInterval(target, target);
          colModel.setSelectionInterval(colLead, colLead);
        }
      else if (command.equals("selectNextRowChangeLead"))
          {
            if (rowModel.getSelectionMode() 
                != ListSelectionModel.MULTIPLE_INTERVAL_SELECTION)
              {
                // just "selectNextRow"
                rowModel.setSelectionInterval(Math.min(rowLead + 1, rowMax),
                                              Math.min(rowLead + 1, rowMax));
                colModel.setSelectionInterval(colLead, colLead);
              }
            else
              rowModel.moveLeadSelectionIndex(Math.min(rowLead + 1, rowMax));
          }
      else if (command.equals("selectPreviousRowChangeLead"))
        {
          if (rowModel.getSelectionMode() 
              != ListSelectionModel.MULTIPLE_INTERVAL_SELECTION)
            {
              // just selectPreviousRow
              rowModel.setSelectionInterval(Math.max(rowLead - 1, 0),
                                            Math.min(rowLead - 1, 0));
              colModel.setSelectionInterval(colLead, colLead);
            }
          else
            rowModel.moveLeadSelectionIndex(Math.max(rowLead - 1, 0));
        }
      else if (command.equals("selectNextColumnChangeLead"))
        {
          if (colModel.getSelectionMode() 
              != ListSelectionModel.MULTIPLE_INTERVAL_SELECTION)            
            {
              // just selectNextColumn
              rowModel.setSelectionInterval(rowLead, rowLead);
              colModel.setSelectionInterval(Math.min(colLead + 1, colMax),
                                            Math.min(colLead + 1, colMax));
            }
          else
            colModel.moveLeadSelectionIndex(Math.min(colLead + 1, colMax));
        }
      else if (command.equals("selectPreviousColumnChangeLead"))
        {
          if (colModel.getSelectionMode() 
              != ListSelectionModel.MULTIPLE_INTERVAL_SELECTION)            
            {
              // just selectPreviousColumn
              rowModel.setSelectionInterval(rowLead, rowLead);
              colModel.setSelectionInterval(Math.max(colLead - 1, 0),
                                            Math.max(colLead - 1, 0));
              
            }
          else
            colModel.moveLeadSelectionIndex(Math.max(colLead - 1, 0));
        }
      else if (command.equals("addToSelection"))
          {
            if (!table.isEditing())
              {
                int oldRowAnchor = rowModel.getAnchorSelectionIndex();
                int oldColAnchor = colModel.getAnchorSelectionIndex();
                rowModel.addSelectionInterval(rowLead, rowLead);
                colModel.addSelectionInterval(colLead, colLead);
                rowModel.setAnchorSelectionIndex(oldRowAnchor);
                colModel.setAnchorSelectionIndex(oldColAnchor);
              }
          }
      else if (command.equals("extendTo"))
        {
          rowModel.setSelectionInterval(rowModel.getAnchorSelectionIndex(),
                                        rowLead);
          colModel.setSelectionInterval(colModel.getAnchorSelectionIndex(),
                                        colLead);
        }
      else if (command.equals("toggleAndAnchor"))
        {
          if (rowModel.isSelectedIndex(rowLead))
            rowModel.removeSelectionInterval(rowLead, rowLead);
          else
            rowModel.addSelectionInterval(rowLead, rowLead);
          
          if (colModel.isSelectedIndex(colLead))
            colModel.removeSelectionInterval(colLead, colLead);
          else
            colModel.addSelectionInterval(colLead, colLead);
          
          rowModel.setAnchorSelectionIndex(rowLead);
          colModel.setAnchorSelectionIndex(colLead);
        }
      else if (command.equals("stopEditing"))
        {
          table.editingStopped(new ChangeEvent(command));
        }
      else 
        {
          // If we're here that means we bound this TableAction class
          // to a keyboard input but we either want to ignore that input
          // or we just haven't implemented its action yet.
          
          // Uncomment the following line to print the names of unused bindings
          // when their keys are pressed
          
          // System.out.println ("not implemented: "+e.getActionCommand());
        }

      // Any commands whose keyStrokes should be used by the Editor should not
      // cause editing to be stopped: ie, the SPACE sends "addToSelection" but 
      // if the table is in editing mode, the space should not cause us to stop
      // editing because it should be used by the Editor.
      if (table.isEditing() && command != "startEditing"
          && command != "addToSelection")
        table.editingStopped(new ChangeEvent("update"));
            
      table.scrollRectToVisible(table.getCellRect(
          rowModel.getLeadSelectionIndex(), colModel.getLeadSelectionIndex(), 
          false));
    }
    
    /**
     * Returns the column index of the first visible column.
     * @return the column index of the first visible column.
     */
    int getFirstVisibleColumnIndex(JTable table)
    {
      ComponentOrientation or = table.getComponentOrientation();
      Rectangle r = table.getVisibleRect();
      if (!or.isLeftToRight())
        r.translate((int) r.getWidth() - 1, 0);
      return table.columnAtPoint(r.getLocation());
    }
    
    /**
     * Returns the column index of the last visible column.
     *
     */
    int getLastVisibleColumnIndex(JTable table)
    {
      ComponentOrientation or = table.getComponentOrientation();
      Rectangle r = table.getVisibleRect();
      if (or.isLeftToRight())
        r.translate((int) r.getWidth() - 1, 0);
      return table.columnAtPoint(r.getLocation());      
    }
    
    /**
     * Returns the row index of the first visible row.
     *
     */
    int getFirstVisibleRowIndex(JTable table)
    {
      ComponentOrientation or = table.getComponentOrientation();
      Rectangle r = table.getVisibleRect();
      if (!or.isLeftToRight())
        r.translate((int) r.getWidth() - 1, 0);
      return table.rowAtPoint(r.getLocation());
    }
    
    /**
     * Returns the row index of the last visible row.
     *
     */
    int getLastVisibleRowIndex(JTable table)
    {
      ComponentOrientation or = table.getComponentOrientation();
      Rectangle r = table.getVisibleRect();
      r.translate(0, (int) r.getHeight() - 1);
      if (or.isLeftToRight())
        r.translate((int) r.getWidth() - 1, 0);
      // The next if makes sure that we don't return -1 simply because
      // there is white space at the bottom of the table (ie, the display
      // area is larger than the table)
      if (table.rowAtPoint(r.getLocation()) == -1)
        {
          if (getFirstVisibleRowIndex(table) == -1)
            return -1;
          else
            return table.getModel().getRowCount() - 1;
        }
      return table.rowAtPoint(r.getLocation());
    }

    /**
     * A helper method for the key bindings.  Used because the actions
     * for TAB, SHIFT-TAB, ENTER, and SHIFT-ENTER are very similar.
     *
     * Selects the next (previous if SHIFT pressed) column for TAB, or row for
     * ENTER from within the currently selected cells.
     *
     * @param firstModel the ListSelectionModel for columns (TAB) or
     * rows (ENTER)
     * @param firstMin the first selected index in firstModel
     * @param firstMax the last selected index in firstModel
     * @param secondModel the ListSelectionModel for rows (TAB) or 
     * columns (ENTER)
     * @param secondMin the first selected index in secondModel
     * @param secondMax the last selected index in secondModel
     * @param reverse true if shift was held for the event
     * @param eventIsTab true if TAB was pressed, false if ENTER pressed
     */
    void advanceMultipleSelection(JTable table, ListSelectionModel firstModel,
                                  int firstMin,
                                  int firstMax, ListSelectionModel secondModel, 
                                  int secondMin, int secondMax, boolean reverse,
                                  boolean eventIsTab)
    {
      // If eventIsTab, all the "firsts" correspond to columns, otherwise, to 
      // rows "seconds" correspond to the opposite
      int firstLead = firstModel.getLeadSelectionIndex();
      int secondLead = secondModel.getLeadSelectionIndex();
      int numFirsts = eventIsTab ? 
        table.getModel().getColumnCount() : table.getModel().getRowCount();
      int numSeconds = eventIsTab ? 
        table.getModel().getRowCount() : table.getModel().getColumnCount();

      // check if we have to wrap the "firsts" around, going to the other side
      if ((firstLead == firstMax && !reverse) || 
          (reverse && firstLead == firstMin))
        {
          firstModel.addSelectionInterval(reverse ? firstMax : firstMin, 
                                          reverse ? firstMax : firstMin);
          
          // check if we have to wrap the "seconds"
          if ((secondLead == secondMax && !reverse) || 
              (reverse && secondLead == secondMin))
            secondModel.addSelectionInterval(reverse ? secondMax : secondMin, 
                                             reverse ? secondMax : secondMin);

          // if we're not wrapping the seconds, we have to find out where we
          // are within the secondModel and advance to the next cell (or 
          // go back to the previous cell if reverse == true)
          else
            {
              int[] secondsSelected;
              if (eventIsTab && table.getRowSelectionAllowed() || 
                  !eventIsTab && table.getColumnSelectionAllowed())
                secondsSelected = eventIsTab ? 
                  table.getSelectedRows() : table.getSelectedColumns();
              else
                {
                  // if row selection is not allowed, then the entire column gets
                  // selected when you click on it, so consider ALL rows selected
                  secondsSelected = new int[numSeconds];
                  for (int i = 0; i < numSeconds; i++)
                  secondsSelected[i] = i;
                }

              // and now find the "next" index within the model
              int secondIndex = reverse ? secondsSelected.length - 1 : 0;
              if (!reverse)
                while (secondsSelected[secondIndex] <= secondLead)
                  secondIndex++;
              else
                while (secondsSelected[secondIndex] >= secondLead)
                  secondIndex--;
              
              // and select it - updating the lead selection index
              secondModel.addSelectionInterval(secondsSelected[secondIndex], 
                                               secondsSelected[secondIndex]);
            }
        }
      // We didn't have to wrap the firsts, so just find the "next" first
      // and select it, we don't have to change "seconds"
      else
        {
          int[] firstsSelected;
          if (eventIsTab && table.getColumnSelectionAllowed() || 
              !eventIsTab && table.getRowSelectionAllowed())
            firstsSelected = eventIsTab ? 
              table.getSelectedColumns() : table.getSelectedRows();
          else
            {
              // if selection not allowed, consider ALL firsts to be selected
              firstsSelected = new int[numFirsts];
              for (int i = 0; i < numFirsts; i++)
                firstsSelected[i] = i;
            }
          int firstIndex = reverse ? firstsSelected.length - 1 : 0;
          if (!reverse)
            while (firstsSelected[firstIndex] <= firstLead)
              firstIndex++;
          else 
            while (firstsSelected[firstIndex] >= firstLead)
              firstIndex--;
          firstModel.addSelectionInterval(firstsSelected[firstIndex], 
                                          firstsSelected[firstIndex]);
          secondModel.addSelectionInterval(secondLead, secondLead);
        }
    }
    
    /** 
     * A helper method for the key  bindings. Used because the actions
     * for TAB, SHIFT-TAB, ENTER, and SHIFT-ENTER are very similar.
     *
     * Selects the next (previous if SHIFT pressed) column (TAB) or row (ENTER)
     * in the table, changing the current selection.  All cells in the table
     * are eligible, not just the ones that are currently selected.
     * @param firstModel the ListSelectionModel for columns (TAB) or rows
     * (ENTER)
     * @param firstMax the last index in firstModel
     * @param secondModel the ListSelectionModel for rows (TAB) or columns
     * (ENTER)
     * @param secondMax the last index in secondModel
     * @param reverse true if SHIFT was pressed for the event
     */

    void advanceSingleSelection(ListSelectionModel firstModel, int firstMax, 
                                ListSelectionModel secondModel, int secondMax, 
                                boolean reverse)
    {
      // for TABs, "first" corresponds to columns and "seconds" to rows.
      // the opposite is true for ENTERs
      int firstLead = firstModel.getLeadSelectionIndex();
      int secondLead = secondModel.getLeadSelectionIndex();
      
      // if we are going backwards subtract 2 because we later add 1
      // for a net change of -1
      if (reverse && (firstLead == 0))
        {
          // check if we have to wrap around
          if (secondLead == 0)
            secondLead += secondMax + 1;
          secondLead -= 2;
        }
      
      // do we have to wrap the "seconds"?
      if (reverse && (firstLead == 0) || !reverse && (firstLead == firstMax))
        secondModel.setSelectionInterval((secondLead + 1) % (secondMax + 1), 
                                         (secondLead + 1) % (secondMax + 1));
      // if not, just reselect the current lead
      else
        secondModel.setSelectionInterval(secondLead, secondLead);
      
      // if we are going backwards, subtract 2  because we add 1 later
      // for net change of -1
      if (reverse)
        {
          // check for wraparound
          if (firstLead == 0)
            firstLead += firstMax + 1;
          firstLead -= 2;
        }
      // select the next "first"
      firstModel.setSelectionInterval((firstLead + 1) % (firstMax + 1), 
                                      (firstLead + 1) % (firstMax + 1));
    }
  }

  protected void installListeners() 
  {
    if (focusListener == null)
      focusListener = createFocusListener();
    table.addFocusListener(focusListener);
    if (keyListener == null)
      keyListener = createKeyListener();
    table.addKeyListener(keyListener);
    if (mouseInputListener == null)
      mouseInputListener = createMouseInputListener();
    table.addMouseListener(mouseInputListener);    
    table.addMouseMotionListener(mouseInputListener);
    if (propertyChangeListener == null)
      propertyChangeListener = new PropertyChangeHandler();
    table.addPropertyChangeListener(propertyChangeListener);
  }

  /**
   * Uninstalls UI defaults that have been installed by
   * {@link #installDefaults()}.
   */
  protected void uninstallDefaults()
  {
    // Nothing to do here for now.
  }

  /**
   * Uninstalls the keyboard actions that have been installed by
   * {@link #installKeyboardActions()}.
   */
  protected void uninstallKeyboardActions()
  {
    SwingUtilities.replaceUIInputMap(table, JComponent.
                                     WHEN_ANCESTOR_OF_FOCUSED_COMPONENT, null);
    SwingUtilities.replaceUIActionMap(table, null);
  }

  protected void uninstallListeners() 
  {
    table.removeFocusListener(focusListener);  
    table.removeKeyListener(keyListener);
    table.removeMouseListener(mouseInputListener);    
    table.removeMouseMotionListener(mouseInputListener);
    table.removePropertyChangeListener(propertyChangeListener);
    propertyChangeListener = null;
  }

  public void installUI(JComponent comp) 
  {
    table = (JTable) comp;
    rendererPane = new CellRendererPane();
    table.add(rendererPane);

    installDefaults();
    installKeyboardActions();
    installListeners();
  }

  public void uninstallUI(JComponent c) 
  {
    uninstallListeners();
    uninstallKeyboardActions();
    uninstallDefaults(); 

    table.remove(rendererPane);
    rendererPane = null;
    table = null;
  }

  /**
   * Paints a single cell in the table.
   *
   * @param g The graphics context to paint in
   * @param row The row number to paint
   * @param col The column number to paint
   * @param bounds The bounds of the cell to paint, assuming a coordinate
   * system beginning at <code>(0,0)</code> in the upper left corner of the
   * table
   * @param rend A cell renderer to paint with
   */
  void paintCell(Graphics g, int row, int col, Rectangle bounds,
                 TableCellRenderer rend)
  {
    Component comp = table.prepareRenderer(rend, row, col);
    rendererPane.paintComponent(g, comp, table, bounds);
  }
  
  /**
   * Paint the associated table.
   */
  public void paint(Graphics gfx, JComponent ignored) 
  {
    int ncols = table.getColumnCount();
    int nrows = table.getRowCount();
    if (nrows == 0 || ncols == 0)
      return;

    Rectangle clip = gfx.getClipBounds();

    // Determine the range of cells that are within the clip bounds.
    Point p1 = new Point(clip.x, clip.y);
    int c0 = table.columnAtPoint(p1);
    if (c0 == -1)
      c0 = 0;
    int r0 = table.rowAtPoint(p1);
    if (r0 == -1)
      r0 = 0;
    Point p2 = new Point(clip.x + clip.width, clip.y + clip.height);
    int cn = table.columnAtPoint(p2);
    if (cn == -1)
      cn = table.getColumnCount() - 1;
    int rn = table.rowAtPoint(p2);
    if (rn == -1)
      rn = table.getRowCount() - 1;

    int columnMargin = table.getColumnModel().getColumnMargin();
    int rowMargin = table.getRowMargin();

    TableColumnModel cmodel = table.getColumnModel();
    int[] widths = new int[cn + 1];
    for (int i = c0; i <= cn; i++)
      {
        widths[i] = cmodel.getColumn(i).getWidth() - columnMargin;
      }
    
    Rectangle bounds = table.getCellRect(r0, c0, false);
    // The left boundary of the area being repainted.
    int left = bounds.x;
    
    // The top boundary of the area being repainted.
    int top = bounds.y;
    
    // The bottom boundary of the area being repainted.
    int bottom;
    
    // paint the cell contents
    Color grid = table.getGridColor();    
    for (int r = r0; r <= rn; ++r)
      {
        for (int c = c0; c <= cn; ++c)
          {
            bounds.width = widths[c];
            paintCell(gfx, r, c, bounds, table.getCellRenderer(r, c));
            bounds.x += widths[c] + columnMargin;
          }
        bounds.x = left;
        bounds.y += table.getRowHeight(r);
        // Update row height for tables with custom heights.
        bounds.height = table.getRowHeight(r + 1) - rowMargin;
      }
    
    bottom = bounds.y - rowMargin;

    // paint vertical grid lines
    if (grid != null && table.getShowVerticalLines())
      {    
        Color save = gfx.getColor();
        gfx.setColor(grid);
        int x = left - columnMargin;
        for (int c = c0; c <= cn; ++c)
          {
            // The vertical grid is draw right from the cells, so we 
            // add before drawing.
            x += widths[c] + columnMargin;
            gfx.drawLine(x, top, x, bottom);
          }
        gfx.setColor(save);
      }

    // paint horizontal grid lines    
    if (grid != null && table.getShowHorizontalLines())
      {    
        Color save = gfx.getColor();
        gfx.setColor(grid);
        int y = top - rowMargin;
        for (int r = r0; r <= rn; ++r)
          {
            // The horizontal grid is draw below the cells, so we 
            // add before drawing.
            y += table.getRowHeight(r);
            gfx.drawLine(left, y, p2.x, y);
          }
        gfx.setColor(save);
      }
  }
}
