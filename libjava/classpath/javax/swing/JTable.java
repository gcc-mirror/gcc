/* JTable.java --
   Copyright (C) 2002, 2004, 2005, 2006  Free Software Foundation, Inc.

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

import java.awt.Color;
import java.awt.Component;
import java.awt.Cursor;
import java.awt.Dimension;
import java.awt.Font;
import java.awt.FontMetrics;
import java.awt.Point;
import java.awt.Rectangle;
import java.awt.event.FocusListener;
import java.beans.PropertyChangeEvent;
import java.beans.PropertyChangeListener;
import java.text.DateFormat;
import java.text.NumberFormat;
import java.util.Date;
import java.util.EventObject;
import java.util.Hashtable;
import java.util.Locale;
import java.util.Vector;

import javax.accessibility.Accessible;
import javax.accessibility.AccessibleComponent;
import javax.accessibility.AccessibleContext;
import javax.accessibility.AccessibleExtendedTable;
import javax.accessibility.AccessibleRole;
import javax.accessibility.AccessibleSelection;
import javax.accessibility.AccessibleState;
import javax.accessibility.AccessibleStateSet;
import javax.accessibility.AccessibleTable;
import javax.accessibility.AccessibleTableModelChange;
import javax.swing.event.CellEditorListener;
import javax.swing.event.ChangeEvent;
import javax.swing.event.ListSelectionEvent;
import javax.swing.event.ListSelectionListener;
import javax.swing.event.TableColumnModelEvent;
import javax.swing.event.TableColumnModelListener;
import javax.swing.event.TableModelEvent;
import javax.swing.event.TableModelListener;
import javax.swing.plaf.TableUI;
import javax.swing.table.DefaultTableCellRenderer;
import javax.swing.table.DefaultTableColumnModel;
import javax.swing.table.DefaultTableModel;
import javax.swing.table.JTableHeader;
import javax.swing.table.TableCellEditor;
import javax.swing.table.TableCellRenderer;
import javax.swing.table.TableColumn;
import javax.swing.table.TableColumnModel;
import javax.swing.table.TableModel;

/**
 * The table component, displaying information, organized in rows and columns.
 * The table can be placed in the scroll bar and have the optional header
 * that is always visible. Cell values may be editable after double clicking
 * on the cell. Cell columns may have various data types, that are
 * displayed and edited by the different renderers and editors. It is possible
 * to set different column width. The columns are also resizeable by
 * dragging the column boundary in the header.
 */
public class JTable
  extends JComponent
  implements TableModelListener, Scrollable, TableColumnModelListener,
             ListSelectionListener, CellEditorListener, Accessible
{
  /**
   * Provides accessibility support for <code>JTable</code>.
   *
   * @author Roman Kennke (kennke@aicas.com)
   */
  protected class AccessibleJTable
    extends AccessibleJComponent
    implements AccessibleSelection, ListSelectionListener, TableModelListener,
    TableColumnModelListener, CellEditorListener, PropertyChangeListener,
    AccessibleExtendedTable
  {

    /**
     * Provides accessibility support for table cells.
     *
     * @author Roman Kennke (kennke@aicas.com)
     */
    protected class AccessibleJTableCell
      extends AccessibleContext
      implements Accessible, AccessibleComponent
    {

      /**
       * The table of this cell.
       */
      private JTable table;

      /**
       * The row index of this cell.
       */
      private int row;

      /**
       * The column index of this cell.
       */
      private int column;

      /**
       * The index of this cell inside the AccessibleJTable parent.
       */
      private int index;

      /**
       * Creates a new <code>AccessibleJTableCell</code>.
       *
       * @param t the table
       * @param r the row
       * @param c the column
       * @param i the index of this cell inside the accessible table parent
       */
      public AccessibleJTableCell(JTable t, int r, int c, int i)
      {
        table = t;
        row = r;
        column = c;
        index = i;
      }

      /**
       * Returns the accessible row for the table cell.
       *
       * @return the accessible row for the table cell
       */
      public AccessibleRole getAccessibleRole()
      {
        // TODO: What is the role of the table cell?
        // Seems like the RI returns UNKNOWN here for 'normal' cells, might
        // be different for special renderers though (not tested yet).
        return AccessibleRole.UNKNOWN;
      }

      /**
       * Returns the accessible state set of this accessible table cell.
       *
       * @return the accessible state set of this accessible table cell
       */
      public AccessibleStateSet getAccessibleStateSet()
      {
        AccessibleStateSet state = new AccessibleStateSet();

        // Figure out the SHOWING state.
        Rectangle visibleRect = getVisibleRect();
        Rectangle cellRect = getCellRect(row, column, false);
        if (visibleRect.intersects(cellRect))
          state.add(AccessibleState.SHOWING);

        // Figure out SELECTED state.
        if (isCellSelected(row, column))
          state.add(AccessibleState.SELECTED);

        // Figure out ACTIVE state.
        if (row == getSelectedRow() && column == getSelectedColumn())
          state.add(AccessibleState.ACTIVE);

        // TRANSIENT seems to be always set in the RI.
        state.add(AccessibleState.TRANSIENT);

        // TODO: Any other state to handle here?
        return state;
      }

      /**
       * Returns the index of this cell in the parent object.
       *
       * @return the index of this cell in the parent object
       */
      public int getAccessibleIndexInParent()
      {
        return index;
      }

      /**
       * Returns the number of children of this object. Table cells cannot have
       * children, so we return <code>0</code> here.
       *
       * @return <code>0</code>
       */
      public int getAccessibleChildrenCount()
      {
        return 0;
      }

      /**
       * Returns the accessible child at index <code>i</code>. Table cells
       * don't have children, so we return <code>null</code> here.
       *
       * @return <code>null</code>
       */
      public Accessible getAccessibleChild(int i)
      {
        return null;
      }

      /**
       * Returns the locale setting for this accessible table cell.
       *
       * @return the locale setting for this accessible table cell
       */
      public Locale getLocale()
      {
        // TODO: For now, we return english here. This must be fixed as soon
        // as we have a localized Swing.
        return Locale.ENGLISH;
      }

      /**
       * Returns the accessible context of this table cell. Since accessible
       * table cells are their own accessible context, we return
       * <code>this</code>.
       *
       * @return the accessible context of this table cell
       */
      public AccessibleContext getAccessibleContext()
      {
        return this;
      }

      /**
       * Returns the background color of this cell.
       *
       * @return the background color of this cell
       */
      public Color getBackground()
      {
        return table.getBackground();
      }

      /**
       * Sets the background of the cell. Since table cells cannot have
       * individual background colors, this method does nothing. Set the
       * background directly on the table instead.
       *
       * @param color not used
       */
      public void setBackground(Color color)
      {
        // This method does nothing. See API comments.
      }

      /**
       * Returns the foreground color of the table cell.
       *
       * @return the foreground color of the table cell
       */
      public Color getForeground()
      {
        return table.getForeground();
      }

      /**
       * Sets the foreground of the cell. Since table cells cannot have
       * individual foreground colors, this method does nothing. Set the
       * foreground directly on the table instead.
       *
       * @param color not used
       */
      public void setForeground(Color color)
      {
        // This method does nothing. See API comments.
      }

      /**
       * Returns the cursor for this table cell.
       *
       * @return the cursor for this table cell
       */
      public Cursor getCursor()
      {
        return table.getCursor();
      }

      /**
       * Sets the cursor of the cell. Since table cells cannot have
       * individual cursors, this method does nothing. Set the
       * cursor directly on the table instead.
       *
       * @param cursor not used
       */
      public void setCursor(Cursor cursor)
      {
        // This method does nothing. See API comments.
      }

      /**
       * Returns the font of the table cell.
       *
       * @return the font of the table cell
       */
      public Font getFont()
      {
        return table.getFont();
      }

      /**
       * Sets the font of the cell. Since table cells cannot have
       * individual fonts, this method does nothing. Set the
       * font directly on the table instead.
       *
       * @param font not used
       */
      public void setFont(Font font)
      {
        // This method does nothing. See API comments.
      }

      /**
       * Returns the font metrics for a specified font.
       *
       * @param font the font for which we return the metrics
       *
       * @return the font metrics for a specified font
       */
      public FontMetrics getFontMetrics(Font font)
      {
        return table.getFontMetrics(font);
      }

      /**
       * Returns <code>true</code> if this table cell is enabled,
       * <code>false</code> otherwise.
       *
       * @return <code>true</code> if this table cell is enabled,
       *         <code>false</code> otherwise
       */
      public boolean isEnabled()
      {
        return table.isEnabled();
      }

      /**
       * Table cells cannot be disabled or enabled individually, so this method
       * does nothing. Set the enabled flag on the table itself.
       *
       * @param b not used here
       */
      public void setEnabled(boolean b)
      {
        // This method does nothing. See API comments.
      }

      /**
       * Returns <code>true</code> if this cell is visible, <code>false</code>
       * otherwise.
       *
       * @return <code>true</code> if this cell is visible, <code>false</code>
       *         otherwise
       */
      public boolean isVisible()
      {
        return table.isVisible();
      }

      /**
       * The visibility cannot be set on individual table cells, so this method
       * does nothing. Set the visibility on the table itself.
       *
       * @param b not used
       */
      public void setVisible(boolean b)
      {
        // This method does nothing. See API comments.
      }

      /**
       * Returns <code>true</code> if this table cell is currently showing on
       * screen.
       *
       * @return <code>true</code> if this table cell is currently showing on
       *         screen
       */
      public boolean isShowing()
      {
        return table.isShowing();
      }

      /**
       * Returns <code>true</code> if this table cell contains the location
       * at <code>point</code>, <code>false</code> otherwise.
       * <code>point</code> is interpreted as relative to the coordinate system
       * of the table cell.
       *
       * @return <code>true</code> if this table cell contains the location
       *         at <code>point</code>, <code>false</code> otherwise
       */
      public boolean contains(Point point)
      {
        Rectangle cellRect = table.getCellRect(row, column, true);
        cellRect.x = 0;
        cellRect.y = 0;
        return cellRect.contains(point);
      }

      /**
       * Returns the screen location of the table cell.
       *
       * @return the screen location of the table cell
       */
      public Point getLocationOnScreen()
      {
        Point tableLoc = table.getLocationOnScreen();
        Rectangle cellRect = table.getCellRect(row, column, true);
        tableLoc.x += cellRect.x;
        tableLoc.y += cellRect.y;
        return tableLoc;
      }

      /**
       * Returns the location of this cell relative to the table's bounds.
       *
       * @return the location of this cell relative to the table's bounds
       */
      public Point getLocation()
      {
        Rectangle cellRect = table.getCellRect(row, column, true);
        return new Point(cellRect.x, cellRect.y);
      }

      /**
       * The location of the table cells cannot be manipulated directly, so
       * this method does nothing.
       *
       * @param point not used
       */
      public void setLocation(Point point)
      {
        // This method does nothing. See API comments.
      }

      /**
       * Returns the bounds of the cell relative to its table.
       *
       * @return the bounds of the cell relative to its table
       */
      public Rectangle getBounds()
      {
        return table.getCellRect(row, column, true);
      }

      /**
       * The bounds of the table cells cannot be manipulated directly, so
       * this method does nothing.
       *
       * @param rectangle not used
       */
      public void setBounds(Rectangle rectangle)
      {
        // This method does nothing. See API comments.
      }

      /**
       * Returns the size of the table cell.
       *
       * @return the size of the table cell
       */
      public Dimension getSize()
      {
        Rectangle cellRect = table.getCellRect(row, column, true);
        return new Dimension(cellRect.width, cellRect.height);
      }

      /**
       * The size cannot be set on table cells directly, so this method does
       * nothing.
       *
       * @param dimension not used
       */
      public void setSize(Dimension dimension)
      {
        // This method does nothing. See API comments.
      }

      /**
       * Table cells have no children, so we return <code>null</code> here.
       *
       * @return <code>null</code>
       */
      public Accessible getAccessibleAt(Point point)
      {
        return null;
      }

      /**
       * Returns <code>true</code> if this table cell is focus traversable,
       * <code>false</code> otherwise.
       *
       * @return <code>true</code> if this table cell is focus traversable,
       *         <code>false</code> otherwise
       */
      public boolean isFocusTraversable()
      {
        return table.isFocusable();
      }

      /**
       * Requests that this table cell gets the keyboard focus.
       */
      public void requestFocus()
      {
        // We first set the selection models' lead selection to this cell.
        table.getColumnModel().getSelectionModel()
        .setLeadSelectionIndex(column);
        table.getSelectionModel().setLeadSelectionIndex(row);
        // Now we request that the table receives focus.
        table.requestFocus();
      }

      /**
       * Adds a focus listener to this cell. The focus listener is really
       * added to the table, so there is no way to find out when an individual
       * cell changes the focus.
       *
       * @param listener the focus listener to add
       */
      public void addFocusListener(FocusListener listener)
      {
        table.addFocusListener(listener);
      }

      /**
       * Removes a focus listener from the cell. The focus listener is really
       * removed from the table.
       *
       * @param listener the listener to remove
       */
      public void removeFocusListener(FocusListener listener)
      {
        table.removeFocusListener(listener);
      }

    }

    protected class AccessibleJTableModelChange
      implements AccessibleTableModelChange
    {
      protected int type;
      protected int firstRow;
      protected int lastRow;
      protected int firstColumn;
      protected int lastColumn;

      protected AccessibleJTableModelChange(int type, int firstRow,
                                            int lastRow, int firstColumn,
                                            int lastColumn)
      {
        this.type = type;
        this.firstRow = firstRow;
        this.lastRow = lastRow;
        this.firstColumn = firstColumn;
        this.lastColumn = lastColumn;
      }

      public int getType()
      {
        return type;
      }

      public int getFirstRow()
      {
        return firstRow;
      }

      public int getLastRow()
      {
        return lastRow;
      }

      public int getFirstColumn()
      {
        return firstColumn;
      }

      public int getLastColumn()
      {
        return lastColumn;
      }
    }

    /**
     * The RI returns an instance with this name in
     * {@link #getAccessibleColumnHeader()}, this makes sense, so we do the
     * same.
     */
    private class AccessibleTableHeader
      implements AccessibleTable
    {

      /**
       * The JTableHeader wrapped by this class.
       */
      private JTableHeader header;

      /**
       * Creates a new instance.
       *
       * @param h the JTableHeader to wrap
       */
      private AccessibleTableHeader(JTableHeader h)
      {
        header = h;
      }

      /**
       * Returns the caption for the table header.
       *
       * @return the caption for the table header
       */
      public Accessible getAccessibleCaption()
      {
        // The RI seems to always return null here, so do we.
        return null;
      }

      /**
       * Sets the caption for the table header.
       *
       * @param caption the caption to set
       */
      public void setAccessibleCaption(Accessible caption)
      {
        // This seems to be a no-op in the RI, so we do the same.
      }

      /**
       * Returns the caption for the table header.
       *
       * @return the caption for the table header
       */
      public Accessible getAccessibleSummary()
      {
        // The RI seems to always return null here, so do we.
        return null;
      }

      /**
       * Sets the summary for the table header.
       *
       * @param summary the caption to set
       */
      public void setAccessibleSummary(Accessible summary)
      {
        // This seems to be a no-op in the RI, so we do the same.
      }

      /**
       * Returns the number of rows, which is always 1 for the table header.
       *
       * @return the number of rows
       */
      public int getAccessibleRowCount()
      {
        return 1;
      }

      /**
       * Returns the number of columns in the table header.
       *
       * @return the number of columns in the table header
       */
      public int getAccessibleColumnCount()
      {
        return header.getColumnModel().getColumnCount();
      }

      /**
       * Returns the accessible child at the specified row and column.
       * The row number is ignored here, and we return an
       * AccessibleJTableHeaderCell here with the renderer component as
       * component.
       *
       * @param r the row number
       * @param c the column number
       *
       * @return the accessible child at the specified row and column
       */
      public Accessible getAccessibleAt(int r, int c)
      {
        TableColumn column = header.getColumnModel().getColumn(c);
        TableCellRenderer rend = column.getHeaderRenderer();
        if (rend == null)
          rend = header.getDefaultRenderer();
        Component comp =
          rend.getTableCellRendererComponent(header.getTable(),
                                             column.getHeaderValue(), false,
                                             false, -1, c);
        return new AccessibleJTableHeaderCell(header, comp, r, c);
      }

      public int getAccessibleRowExtentAt(int r, int c)
      {
        // TODO Auto-generated method stub
        return 0;
      }

      public int getAccessibleColumnExtentAt(int r, int c)
      {
        // TODO Auto-generated method stub
        return 0;
      }

      public AccessibleTable getAccessibleRowHeader()
      {
        // TODO Auto-generated method stub
        return null;
      }

      public void setAccessibleRowHeader(AccessibleTable header)
      {
        // TODO Auto-generated method stub

      }

      public AccessibleTable getAccessibleColumnHeader()
      {
        // TODO Auto-generated method stub
        return null;
      }

      public void setAccessibleColumnHeader(AccessibleTable header)
      {
        // TODO Auto-generated method stub

      }

      public Accessible getAccessibleRowDescription(int r)
      {
        // TODO Auto-generated method stub
        return null;
      }

      public void setAccessibleRowDescription(int r, Accessible description)
      {
        // TODO Auto-generated method stub

      }

      public Accessible getAccessibleColumnDescription(int c)
      {
        // TODO Auto-generated method stub
        return null;
      }

      public void setAccessibleColumnDescription(int c, Accessible description)
      {
        // TODO Auto-generated method stub

      }

      public boolean isAccessibleSelected(int r, int c)
      {
        // TODO Auto-generated method stub
        return false;
      }

      public boolean isAccessibleRowSelected(int r)
      {
        // TODO Auto-generated method stub
        return false;
      }

      public boolean isAccessibleColumnSelected(int c)
      {
        // TODO Auto-generated method stub
        return false;
      }

      public int[] getSelectedAccessibleRows()
      {
        // TODO Auto-generated method stub
        return null;
      }

      public int[] getSelectedAccessibleColumns()
      {
        // TODO Auto-generated method stub
        return null;
      }

    }

    /**
     * The RI returns an instance of such class for table header cells. This
     * makes sense so I added this class. This still needs to be fully
     * implemented, I just don't feel motivated enough to do so just now.
     */
    private class AccessibleJTableHeaderCell
      extends AccessibleContext
      implements Accessible, AccessibleComponent
    {

      JTableHeader header;

      int columnIndex;

      /**
       *
       * @param h  the table header.
       * @param comp
       * @param r
       * @param c  the column index.
       */
      private AccessibleJTableHeaderCell(JTableHeader h, Component comp, int r,
                                         int c)
      {
        header = h;
        columnIndex = c;
      }

      /**
       * Returns the header renderer.
       *
       * @return The header renderer.
       */
      Component getColumnHeaderRenderer()
      {
        TableColumn tc = header.getColumnModel().getColumn(columnIndex);
        TableCellRenderer r = tc.getHeaderRenderer();
        if (r == null)
          r = header.getDefaultRenderer();
        return r.getTableCellRendererComponent(header.getTable(),
            tc.getHeaderValue(), false, false, -1, columnIndex);
      }

      /**
       * Returns the accessible role for the table header cell.
       *
       * @return The accessible role.
       */
      public AccessibleRole getAccessibleRole()
      {
        Component renderer = getColumnHeaderRenderer();
        if (renderer instanceof Accessible)
          {
            Accessible ac = (Accessible) renderer;
            return ac.getAccessibleContext().getAccessibleRole();
          }
        return null;
      }

      public AccessibleStateSet getAccessibleStateSet()
      {
        // TODO Auto-generated method stub
        return null;
      }

      public int getAccessibleIndexInParent()
      {
        // TODO Auto-generated method stub
        return 0;
      }

      public int getAccessibleChildrenCount()
      {
        // TODO Auto-generated method stub
        return 0;
      }

      public Accessible getAccessibleChild(int i)
      {
        // TODO Auto-generated method stub
        return null;
      }

      public Locale getLocale()
      {
        // TODO Auto-generated method stub
        return null;
      }

      /**
       * Returns the accessible context.
       *
       * @return <code>this</code>.
       */
      public AccessibleContext getAccessibleContext()
      {
        return this;
      }

      public Color getBackground()
      {
        // TODO Auto-generated method stub
        return null;
      }

      public void setBackground(Color color)
      {
        // TODO Auto-generated method stub

      }

      public Color getForeground()
      {
        // TODO Auto-generated method stub
        return null;
      }

      public void setForeground(Color color)
      {
        // TODO Auto-generated method stub

      }

      public Cursor getCursor()
      {
        // TODO Auto-generated method stub
        return null;
      }

      public void setCursor(Cursor cursor)
      {
        // TODO Auto-generated method stub

      }

      public Font getFont()
      {
        // TODO Auto-generated method stub
        return null;
      }

      public void setFont(Font font)
      {
        // TODO Auto-generated method stub

      }

      public FontMetrics getFontMetrics(Font font)
      {
        // TODO Auto-generated method stub
        return null;
      }

      public boolean isEnabled()
      {
        // TODO Auto-generated method stub
        return false;
      }

      public void setEnabled(boolean b)
      {
        // TODO Auto-generated method stub

      }

      public boolean isVisible()
      {
        // TODO Auto-generated method stub
        return false;
      }

      public void setVisible(boolean b)
      {
        // TODO Auto-generated method stub

      }

      public boolean isShowing()
      {
        // TODO Auto-generated method stub
        return false;
      }

      public boolean contains(Point point)
      {
        // TODO Auto-generated method stub
        return false;
      }

      public Point getLocationOnScreen()
      {
        // TODO Auto-generated method stub
        return null;
      }

      public Point getLocation()
      {
        // TODO Auto-generated method stub
        return null;
      }

      public void setLocation(Point point)
      {
        // TODO Auto-generated method stub

      }

      public Rectangle getBounds()
      {
        // TODO Auto-generated method stub
        return null;
      }

      public void setBounds(Rectangle rectangle)
      {
        // TODO Auto-generated method stub

      }

      public Dimension getSize()
      {
        // TODO Auto-generated method stub
        return null;
      }

      public void setSize(Dimension dimension)
      {
        // TODO Auto-generated method stub

      }

      public Accessible getAccessibleAt(Point point)
      {
        // TODO Auto-generated method stub
        return null;
      }

      public boolean isFocusTraversable()
      {
        // TODO Auto-generated method stub
        return false;
      }

      public void requestFocus()
      {
        // TODO Auto-generated method stub

      }

      public void addFocusListener(FocusListener listener)
      {
        // TODO Auto-generated method stub

      }

      public void removeFocusListener(FocusListener listener)
      {
        // TODO Auto-generated method stub

      }

    }

    /**
     * The last selected row. This is needed to track the selection in
     * {@link #valueChanged(ListSelectionEvent)}.
     */
    private int lastSelectedRow;

    /**
     * The last selected column. This is needed to track the selection in
     * {@link #valueChanged(ListSelectionEvent)}.
     */
    private int lastSelectedColumn;

    /**
     * The caption of the table.
     */
    private Accessible caption;

    /**
     * The summary of the table.
     */
    private Accessible summary;

    /**
     * Accessible descriptions for rows.
     */
    private Accessible[] rowDescriptions;

    /**
     * Accessible descriptions for columns.
     */
    private Accessible[] columnDescriptions;

    /**
     * Creates a new <code>AccessibleJTable</code>.
     *
     * @since JDK1.5
     */
    protected AccessibleJTable()
    {
      getModel().addTableModelListener(this);
      getSelectionModel().addListSelectionListener(this);
      getColumnModel().addColumnModelListener(this);
      lastSelectedRow = getSelectedRow();
      lastSelectedColumn = getSelectedColumn();
      TableCellEditor editor = getCellEditor();
      if (editor != null)
        editor.addCellEditorListener(this);
    }

    /**
     * Returns the accessible role for the <code>JTable</code> component.
     *
     * @return {@link AccessibleRole#TABLE}.
     */
    public AccessibleRole getAccessibleRole()
    {
      return AccessibleRole.TABLE;
    }

    /**
     * Returns the accessible table.
     *
     * @return <code>this</code>.
     */
    public AccessibleTable getAccessibleTable()
    {
      return this;
    }

    /**
     * Returns the number of selected items in this table.
     */
    public int getAccessibleSelectionCount()
    {
      return getSelectedColumnCount();
    }

    /**
     * Returns the selected accessible object with the specified index
     * <code>i</code>. This basically returns the i-th selected cell in the
     * table when going though it row-wise, and inside the rows, column-wise.
     *
     * @param i the index of the selected object to find
     *
     * @return the selected accessible object with the specified index
     *         <code>i</code>
     */
    public Accessible getAccessibleSelection(int i)
    {
      Accessible found = null;

      int[] selectedRows = getSelectedRows();
      int[] selectedColumns = getSelectedColumns();
      int numCols = getColumnCount();
      int numRows = getRowCount();

      // We have to go through every selected row and column and count until we
      // find the specified index. This is potentially inefficient, but I can't
      // think of anything better atm.
      if (getRowSelectionAllowed() && getColumnSelectionAllowed())
        {
          int current = -1;
          int newIndex = current;
          int lastSelectedRow = -1;
          // Go through the selected rows array, don't forget the selected
          // cells inside the not-selected rows' columns.
          for (int j = 0; i < selectedRows.length; i++)
            {
              // Handle unselected rows between this selected and the last
              // selected row, if any.
              int selectedRow = selectedRows[j];
              int r = -1;
              int ci = -1;
              for (r = lastSelectedRow + 1;
                   r < selectedRow && current < i; r++)
                {
                  for (ci = 0; ci < selectedColumns.length && current < i;
                       ci++)
                    {
                      current++;
                    }
                }
              if (current == i)
                {
                  // We found the cell in the above loops, now get out of here.
                  found = getAccessibleChild(r * numCols
                                             + selectedColumns[ci]);
                  break;
                }

              // If we're still here, handle the current selected row.
              if (current < i && current + numCols >= i)
                {
                  // The cell must be in that row, which one is it?
                  found = getAccessibleChild(r * numCols + (i - current));
                  break;
                }
              current += numCols;
            }
          if (found == null)
            {
              // The cell can still be in the last couple of unselected rows.
              int r = 0;
              int ci = 0;
              for (r = lastSelectedRow + 1;
                   r < numRows && current < i; r++)
                {
                  for (ci = 0; ci < selectedColumns.length && current < i;
                       ci++)
                    {
                      current++;
                    }
                }
              if (current == i)
                {
                  // We found the cell in the above loops, now get out of here.
                  found = getAccessibleChild(r * numCols
                                             + selectedColumns[ci]);
                }
            }
        }
      // One or more rows can be completely selected.
      else if (getRowSelectionAllowed())
        {
          int c = i % numCols;
          int r = selectedRows[i / numCols];
          found = getAccessibleChild(r * numCols + c);
        }
      // One or more columns can be completely selected.
      else if (getRowSelectionAllowed())
        {
          int numSelectedColumns = selectedColumns.length;
          int c = selectedColumns[i % numSelectedColumns];
          int r = i / numSelectedColumns;
          found = getAccessibleChild(r * numCols + c);
        }

      return found;
    }

    /**
     * Returns <code>true</code> if the accessible child with the index
     * <code>i</code> is selected, <code>false</code> otherwise.
     *
     * @param i the index of the accessible to check
     *
     * @return <code>true</code> if the accessible child with the index
     *         <code>i</code> is selected, <code>false</code> otherwise
     */
    public boolean isAccessibleChildSelected(int i)
    {
      int r = getAccessibleRowAtIndex(i);
      int c = getAccessibleColumnAtIndex(i);
      return isCellSelected(r, c);
    }

    /**
     * Adds the accessible child with the specified index <code>i</code> to the
     * selection.
     *
     * @param i the index of the accessible child to add to the selection
     */
    public void addAccessibleSelection(int i)
    {
      int r = getAccessibleRowAtIndex(i);
      int c = getAccessibleColumnAtIndex(i);
      changeSelection(r, c, true, false);
    }

    /**
     * Removes the accessible child with the specified index <code>i</code>
     * from the current selection. This will only work on tables that have
     * cell selection enabled (<code>rowSelectionAllowed == false &&
     * columnSelectionAllowed == false</code>).
     *
     * @param i the index of the accessible to be removed from the selection
     */
    public void removeAccessibleSelection(int i)
    {
      if (! getRowSelectionAllowed() && ! getColumnSelectionAllowed())
        {
          int r = getAccessibleRowAtIndex(i);
          int c = getAccessibleColumnAtIndex(i);
          removeRowSelectionInterval(r, r);
          removeColumnSelectionInterval(c, c);
        }
    }

    /**
     * Deselects all selected accessible children.
     */
    public void clearAccessibleSelection()
    {
      clearSelection();
    }

    /**
     * Selects all accessible children that can be selected. This will only
     * work on tables that support multiple selections and that have individual
     * cell selection enabled.
     */
    public void selectAllAccessibleSelection()
    {
      selectAll();
    }

    /**
     * Receives notification when the row selection changes and fires
     * appropriate property change events.
     *
     * @param event the list selection event
     */
    public void valueChanged(ListSelectionEvent event)
    {
      firePropertyChange(AccessibleContext.ACCESSIBLE_SELECTION_PROPERTY,
                         Boolean.FALSE, Boolean.TRUE);
      int r = getSelectedRow();
      int c = getSelectedColumn();
      if (r != lastSelectedRow || c != lastSelectedColumn)
        {
          Accessible o = getAccessibleAt(lastSelectedRow,
                                         lastSelectedColumn);
          Accessible n = getAccessibleAt(r, c);
          firePropertyChange(AccessibleContext
                             .ACCESSIBLE_ACTIVE_DESCENDANT_PROPERTY, o, n);
          lastSelectedRow = r;
          lastSelectedColumn = c;
        }
    }

    /**
     * Receives notification when the table model changes. Depending on the
     * type of change, this method calls {@link #tableRowsInserted} or
     * {@link #tableRowsDeleted}.
     *
     * @param event the table model event
     */
    public void tableChanged(TableModelEvent event)
    {
      switch (event.getType())
        {
        case TableModelEvent.INSERT:
          tableRowsInserted(event);
          break;
        case TableModelEvent.DELETE:
          tableRowsDeleted(event);
          break;
        }
    }

    /**
     * Receives notification when one or more rows have been inserted into the
     * table and fires appropriate property change events.
     *
     * @param event the table model event
     */
    public void tableRowsInserted(TableModelEvent event)
    {
      handleRowChange(event);
    }

    /**
     * Receives notification when one or more rows have been deleted from the
     * table.
     *
     * @param event the table model event
     */
    public void tableRowsDeleted(TableModelEvent event)
    {
      handleRowChange(event);
    }

    /**
     * Fires a PropertyChangeEvent for inserted or deleted rows.
     *
     * @param event the table model event
     */
    private void handleRowChange(TableModelEvent event)
    {
      firePropertyChange(AccessibleContext.ACCESSIBLE_VISIBLE_DATA_PROPERTY,
                         null, null);
      int firstColumn = event.getColumn();
      int lastColumn = event.getColumn();
      if (firstColumn == TableModelEvent.ALL_COLUMNS)
        {
          firstColumn = 0;
          lastColumn = getColumnCount() - 1;
        }
      AccessibleJTableModelChange change = new AccessibleJTableModelChange
         (event.getType(), event.getFirstRow(), event.getLastRow(),
          firstColumn, lastColumn);
      firePropertyChange(AccessibleContext.ACCESSIBLE_TABLE_MODEL_CHANGED,
                         null, change);
    }

    public void columnAdded(TableColumnModelEvent event)
    {
      firePropertyChange(AccessibleContext.ACCESSIBLE_VISIBLE_DATA_PROPERTY,
                         null, null);
      handleColumnChange(AccessibleTableModelChange.INSERT,
                         event.getFromIndex(), event.getToIndex());
    }

    public void columnRemoved(TableColumnModelEvent event)
    {
      firePropertyChange(AccessibleContext.ACCESSIBLE_VISIBLE_DATA_PROPERTY,
                         null, null);
      handleColumnChange(AccessibleTableModelChange.DELETE,
                         event.getFromIndex(), event.getToIndex());
    }

    public void columnMoved(TableColumnModelEvent event)
    {
      firePropertyChange(AccessibleContext.ACCESSIBLE_VISIBLE_DATA_PROPERTY,
                         null, null);
      handleColumnChange(AccessibleTableModelChange.DELETE,
                         event.getFromIndex(), event.getFromIndex());
      handleColumnChange(AccessibleTableModelChange.INSERT,
                         event.getFromIndex(), event.getToIndex());
    }

    /**
     * Fires a PropertyChangeEvent for inserted or deleted columns.
     *
     * @param type the type of change
     * @param from the start of the change
     * @param to the target of the change
     */
    private void handleColumnChange(int type, int from, int to)
    {
      AccessibleJTableModelChange change =
        new AccessibleJTableModelChange(type, 0, 0, from, to);
      firePropertyChange(AccessibleContext.ACCESSIBLE_TABLE_MODEL_CHANGED,
                         null, change);
    }

    public void columnMarginChanged(ChangeEvent event)
    {
      firePropertyChange(AccessibleContext.ACCESSIBLE_VISIBLE_DATA_PROPERTY,
                         null, null);
    }

    public void columnSelectionChanged(ListSelectionEvent event)
    {
      // AFAICS, nothing is done here.
    }

    public void editingCanceled(ChangeEvent event)
    {
      // AFAICS, nothing is done here.
    }

    public void editingStopped(ChangeEvent event)
    {
      firePropertyChange(AccessibleContext.ACCESSIBLE_VISIBLE_DATA_PROPERTY,
                         null, null);
    }

    /**
     * Receives notification when any of the JTable's properties changes. This
     * is used to replace the listeners on the table's model, selection model,
     * column model and cell editor.
     *
     * @param e the property change event
     */
    public void propertyChange(PropertyChangeEvent e)
    {
      String propName = e.getPropertyName();
      if (propName.equals("tableModel"))
        {
          TableModel oldModel = (TableModel) e.getOldValue();
          oldModel.removeTableModelListener(this);
          TableModel newModel = (TableModel) e.getNewValue();
          newModel.addTableModelListener(this);
        }
      else if (propName.equals("columnModel"))
        {
          TableColumnModel oldModel = (TableColumnModel) e.getOldValue();
          oldModel.removeColumnModelListener(this);
          TableColumnModel newModel = (TableColumnModel) e.getNewValue();
          newModel.addColumnModelListener(this);
        }
      else if (propName.equals("selectionModel"))
        {
          ListSelectionModel oldModel = (ListSelectionModel) e.getOldValue();
          oldModel.removeListSelectionListener(this);
          ListSelectionModel newModel = (ListSelectionModel) e.getNewValue();
          newModel.addListSelectionListener(this);
        }
      else if (propName.equals("cellEditor"))
        {
          CellEditor oldEd = (CellEditor) e.getOldValue();
          oldEd.removeCellEditorListener(this);
          CellEditor newEd = (CellEditor) e.getNewValue();
          newEd.addCellEditorListener(this);
        }
    }

    /**
     * Returns the row number of an accessible child (cell) with the specified
     * index.
     *
     * @param index the index of the cell of which the row number is queried
     *
     * @return the row number of an accessible child (cell) with the specified
     *         index
     */
    public int getAccessibleRow(int index)
    {
      return getAccessibleRowAtIndex(index);
    }

    /**
     * Returns the column number of an accessible child (cell) with the
     * specified index.
     *
     * @param index the index of the cell of which the column number is queried
     *
     * @return the column number of an accessible child (cell) with the
     *         specified index
     */
    public int getAccessibleColumn(int index)
    {
      return getAccessibleColumnAtIndex(index);
    }

    /**
     * Returns the index of the accessible child at the specified row and
     * column.
     *
     * @param r the row number
     * @param c the column number
     *
     * @return the index of the accessible child at the specified row and
     *         column
     */
    public int getAccessibleIndex(int r, int c)
    {
      return getAccessibleIndexAt(r, c);
    }

    /**
     * Returns the caption of the table.
     *
     * @return the caption of the table
     *
     * @see #setAccessibleCaption(Accessible)
     */
    public Accessible getAccessibleCaption()
    {
      return caption;
    }

    /**
     * Sets the caption for the table.
     *
     * @param c the caption to set
     */
    public void setAccessibleCaption(Accessible c)
    {
      caption = c;
    }

    /**
     * Returns the summary for the table.
     *
     * @return the summary for the table
     */
    public Accessible getAccessibleSummary()
    {
      return summary;
    }

    /**
     * Sets the summary for the table.
     *
     * @param s the summary to set
     */
    public void setAccessibleSummary(Accessible s)
    {
      summary = s;
    }

    /**
     * Returns the number of rows in the table.
     *
     * @return the number of rows in the table
     */
    public int getAccessibleRowCount()
    {
      return getRowCount();
    }

    /**
     * Returns the number of columns in the table.
     *
     * @return the number of columns in the table
     */
    public int getAccessibleColumnCount()
    {
      return getColumnCount();
    }

    /**
     * Returns the accessible child at the given index.
     *
     * @param index  the child index.
     *
     * @return The accessible child.
     */
    public Accessible getAccessibleChild(int index)
    {
      int r = getAccessibleRow(index);
      int c = getAccessibleColumn(index);
      return getAccessibleAt(r, c);
    }

    /**
     * Returns the accessible child (table cell) at the specified row and
     * column.
     *
     * @param r the row number
     * @param c the column number
     *
     * @return the accessible child (table cell) at the specified row and
     *         column
     */
    public Accessible getAccessibleAt(int r, int c)
    {
      TableCellRenderer cellRenderer = getCellRenderer(r, c);
      Component renderer = cellRenderer.getTableCellRendererComponent(
          JTable.this, getValueAt(r, c), isCellSelected(r, c), false, r, c);
      if (renderer instanceof Accessible)
        return (Accessible) renderer;
      return null;
    }

    /**
     * Returns the number of rows that the specified cell occupies. The
     * standard table cells only occupy one row, so we return <code>1</code>
     * here.
     *
     * @param r the row number
     * @param c the column number
     *
     * @return the number of rows that the specified cell occupies
     */
    public int getAccessibleRowExtentAt(int r, int c)
    {
      return 1;
    }

    /**
     * Returns the number of columns that the specified cell occupies. The
     * standard table cells only occupy one column, so we return <code>1</code>
     * here.
     *
     * @param r the row number
     * @param c the column number
     *
     * @return the number of rows that the specified cell occupies
     */
    public int getAccessibleColumnExtentAt(int r, int c)
    {
      return 1;
    }

    /**
     * Returns the accessible row header.
     *
     * @return the accessible row header
     */
    public AccessibleTable getAccessibleRowHeader()
    {
      // The RI seems to always return null here, so do we.
      return null;
    }

    /**
     * Sets the accessible row header.
     *
     * @param header the header to set
     */
    public void setAccessibleRowHeader(AccessibleTable header)
    {
      // In the RI this seems to be a no-op.
    }

    /**
     * Returns the column header.
     *
     * @return the column header, or <code>null</code> if there is no column
     *         header
     */
    public AccessibleTable getAccessibleColumnHeader()
    {
      JTableHeader h = getTableHeader();
      AccessibleTable header = null;
      if (h != null)
        header = new AccessibleTableHeader(h);
      return header;
    }

    /**
     * Sets the accessible column header. The default implementation doesn't
     * allow changing the header this way, so this is a no-op.
     *
     * @param header the accessible column header to set
     */
    public void setAccessibleColumnHeader(AccessibleTable header)
    {
      // The RI doesn't seem to do anything, so we also do nothing.
    }

    /**
     * Returns the accessible description for the row with the specified index,
     * or <code>null</code> if no description has been set.
     *
     * @param r the row for which the description is queried
     *
     * @return the accessible description for the row with the specified index,
     *         or <code>null</code> if no description has been set
     */
    public Accessible getAccessibleRowDescription(int r)
    {
      Accessible descr = null;
      if (rowDescriptions != null)
        descr = rowDescriptions[r];
      return descr;
    }

    /**
     * Sets the accessible description for the row with the specified index.
     *
     * @param r the row number for which to set the description
     * @param description the description to set
     */
    public void setAccessibleRowDescription(int r, Accessible description)
    {
      if (rowDescriptions == null)
        rowDescriptions = new Accessible[getAccessibleRowCount()];
      rowDescriptions[r] = description;
    }

    /**
     * Returns the accessible description for the column with the specified
     * index, or <code>null</code> if no description has been set.
     *
     * @param c the column for which the description is queried
     *
     * @return the accessible description for the column with the specified
     *         index, or <code>null</code> if no description has been set
     */
    public Accessible getAccessibleColumnDescription(int c)
    {
      Accessible descr = null;
      if (columnDescriptions != null)
        descr = columnDescriptions[c];
      return descr;
    }

    /**
     * Sets the accessible description for the column with the specified index.
     *
     * @param c the column number for which to set the description
     * @param description the description to set
     */
    public void setAccessibleColumnDescription(int c, Accessible description)
    {
      if (columnDescriptions == null)
        columnDescriptions = new Accessible[getAccessibleRowCount()];
      columnDescriptions[c] = description;
    }

    /**
     * Returns <code>true</code> if the accessible child at the specified
     * row and column is selected, <code>false</code> otherwise.
     *
     * @param r the row number of the child
     * @param c the column number of the child
     *
     * @return <code>true</code> if the accessible child at the specified
     *         row and column is selected, <code>false</code> otherwise
     */
    public boolean isAccessibleSelected(int r, int c)
    {
      return isCellSelected(r, c);
    }

    /**
     * Returns <code>true</code> if the row with the specified index is
     * selected, <code>false</code> otherwise.
     *
     * @param r the row number
     *
     * @return <code>true</code> if the row with the specified index is
     *        selected, <code>false</code> otherwise
     */
    public boolean isAccessibleRowSelected(int r)
    {
      return isRowSelected(r);
    }

    /**
     * Returns <code>true</code> if the column with the specified index is
     * selected, <code>false</code> otherwise.
     *
     * @param c the column number
     *
     * @return <code>true</code> if the column with the specified index is
     *        selected, <code>false</code> otherwise
     */
    public boolean isAccessibleColumnSelected(int c)
    {
      return isColumnSelected(c);
    }

    /**
     * Returns the indices of all selected rows.
     *
     * @return the indices of all selected rows
     */
    public int[] getSelectedAccessibleRows()
    {
      return getSelectedRows();
    }

    /**
     * Returns the indices of all selected columns.
     *
     * @return the indices of all selected columns
     */
    public int[] getSelectedAccessibleColumns()
    {
      return getSelectedColumns();
    }

    /**
     * Returns the accessible row at the specified index.
     *
     * @param index the index for which to query the row
     *
     * @return the row number at the specified table index
     */
    public int getAccessibleRowAtIndex(int index)
    {
      // TODO: Back this up by a Mauve test and update API docs accordingly.
      return index / getColumnCount();
    }

    /**
     * Returns the accessible column at the specified index.
     *
     * @param index the index for which to query the column
     *
     * @return the column number at the specified table index
     */
    public int getAccessibleColumnAtIndex(int index)
    {
      // TODO: Back this up by a Mauve test and update API docs accordingly.
      return index % getColumnCount();
    }

    /**
     * Returns the accessible child index at the specified column and row.
     *
     * @param row the row
     * @param column the column
     *
     * @return the index of the accessible child at the specified row and
     *         column
     */
    public int getAccessibleIndexAt(int row, int column)
    {
      // TODO: Back this up by a Mauve test and update API docs accordingly.
      return row * getColumnCount() + column;
    }
  }
  /**
   * Handles property changes from the <code>TableColumn</code>s of this
   * <code>JTable</code>.
   *
   * More specifically, this triggers a {@link #revalidate()} call if the
   * preferredWidth of one of the observed columns changes.
   */
  class TableColumnPropertyChangeHandler implements PropertyChangeListener
  {
    /**
     * Receives notification that a property of the observed TableColumns has
     * changed.
     *
     * @param ev the property change event
     */
    public void propertyChange(PropertyChangeEvent ev)
    {
      if (ev.getPropertyName().equals("preferredWidth"))
        {
          JTableHeader header = getTableHeader();
          if (header != null)
            // Do nothing if the table is in the resizing mode.
            if (header.getResizingColumn() == null)
              {
                TableColumn col = (TableColumn) ev.getSource();
                header.setResizingColumn(col);
                doLayout();
                header.setResizingColumn(null);
              }
        }
    }
  }

  /**
   * A cell renderer for boolean values.
   */
  private class BooleanCellRenderer
    extends DefaultTableCellRenderer
  {
    /**
     * The CheckBox that is used for rendering.
     */
    private final JCheckBox checkBox;

    /**
     * Creates a new checkbox based boolean cell renderer. The checkbox is
     * centered by default.
     */
    BooleanCellRenderer()
    {
       checkBox = new JCheckBox();
       checkBox.setHorizontalAlignment(SwingConstants.CENTER);
    }

    /**
     * Get the check box.
     */
    JCheckBox getCheckBox()
    {
      return checkBox;
    }

    /**
     * Returns the component that is used for rendering the value.
     *
     * @param table the JTable
     * @param value the value of the object
     * @param isSelected is the cell selected?
     * @param hasFocus has the cell the focus?
     * @param row the row to render
     * @param column the cell to render
     * @return this component (the default table cell renderer)
     */
    public Component getTableCellRendererComponent(JTable table, Object value,
                                                   boolean isSelected,
                                                   boolean hasFocus, int row,
                                                   int column)
    {
      if (isSelected)
        {
          checkBox.setBackground(table.getSelectionBackground());
          checkBox.setForeground(table.getSelectionForeground());
        }
      else
        {
          checkBox.setBackground(table.getBackground());
          checkBox.setForeground(table.getForeground());
        }

      if (hasFocus)
        {
          checkBox.setBorder(
            UIManager.getBorder("Table.focusCellHighlightBorder"));
          if (table.isCellEditable(row, column))
            {
              checkBox.setBackground(
                UIManager.getColor("Table.focusCellBackground"));
              checkBox.setForeground(
                UIManager.getColor("Table.focusCellForeground"));
            }
        }
      else
        checkBox.setBorder(BorderFactory.createEmptyBorder(1, 1, 1, 1));

      // Null is rendered as false.
      if (value == null)
        checkBox.setSelected(false);
      else
        {
          Boolean boolValue = (Boolean) value;
          checkBox.setSelected(boolValue.booleanValue());
        }
      return checkBox;
    }
  }

  /**
   * A cell renderer for Date values.
   */
  private class DateCellRenderer
    extends DefaultTableCellRenderer
  {
    /**
     * Returns the component that is used for rendering the value.
     *
     * @param table the JTable
     * @param value the value of the object
     * @param isSelected is the cell selected?
     * @param hasFocus has the cell the focus?
     * @param row the row to render
     * @param column the cell to render
     *
     * @return this component (the default table cell renderer)
     */
    public Component getTableCellRendererComponent(JTable table, Object value,
                                                   boolean isSelected,
                                                   boolean hasFocus, int row,
                                                   int column)
    {
      super.getTableCellRendererComponent(table, value, isSelected, hasFocus,
                                          row, column);
      if (value instanceof Date)
        {
          Date dateValue = (Date) value;
          DateFormat df = DateFormat.getDateInstance(DateFormat.SHORT);
          setText(df.format(dateValue));
        }
      return this;
    }
  }

  /**
   * A cell renderer for Double values.
   */
  private class DoubleCellRenderer
    extends DefaultTableCellRenderer
  {
    /**
     * Creates a new instance of NumberCellRenderer.
     */
    public DoubleCellRenderer()
    {
      setHorizontalAlignment(JLabel.RIGHT);
    }

    /**
     * Returns the component that is used for rendering the value.
     *
     * @param table the JTable
     * @param value the value of the object
     * @param isSelected is the cell selected?
     * @param hasFocus has the cell the focus?
     * @param row the row to render
     * @param column the cell to render
     *
     * @return this component (the default table cell renderer)
     */
    public Component getTableCellRendererComponent(JTable table, Object value,
                                                   boolean isSelected,
                                                   boolean hasFocus, int row,
                                                   int column)
    {
      super.getTableCellRendererComponent(table, value, isSelected, hasFocus,
                                          row, column);
      if (value instanceof Double)
        {
          Double doubleValue = (Double) value;
          NumberFormat nf = NumberFormat.getInstance();
          setText(nf.format(doubleValue.doubleValue()));
        }
      return this;
    }
  }

  /**
   * A cell renderer for Float values.
   */
  private class FloatCellRenderer
    extends DefaultTableCellRenderer
  {
    /**
     * Creates a new instance of NumberCellRenderer.
     */
    public FloatCellRenderer()
    {
      setHorizontalAlignment(JLabel.RIGHT);
    }

    /**
     * Returns the component that is used for rendering the value.
     *
     * @param table the JTable
     * @param value the value of the object
     * @param isSelected is the cell selected?
     * @param hasFocus has the cell the focus?
     * @param row the row to render
     * @param column the cell to render
     *
     * @return this component (the default table cell renderer)
     */
    public Component getTableCellRendererComponent(JTable table, Object value,
                                                   boolean isSelected,
                                                   boolean hasFocus, int row,
                                                   int column)
    {
      super.getTableCellRendererComponent(table, value, isSelected, hasFocus,
                                          row, column);
      if (value instanceof Float)
        {
          Float floatValue = (Float) value;
          NumberFormat nf = NumberFormat.getInstance();
          setText(nf.format(floatValue.floatValue()));
        }
      return this;
    }
  }

  /**
   * A cell renderer for Number values.
   */
  private class NumberCellRenderer
    extends DefaultTableCellRenderer
  {
    /**
     * Creates a new instance of NumberCellRenderer.
     */
    public NumberCellRenderer()
    {
      setHorizontalAlignment(JLabel.RIGHT);
    }
  }

  /**
   * A cell renderer for Icon values.
   */
  private class IconCellRenderer
    extends DefaultTableCellRenderer
  {
    IconCellRenderer()
    {
      setHorizontalAlignment(SwingConstants.CENTER);
    }


    /**
     * Returns the component that is used for rendering the value.
     *
     * @param table the JTable
     * @param value the value of the object
     * @param isSelected is the cell selected?
     * @param hasFocus has the cell the focus?
     * @param row the row to render
     * @param column the cell to render
     *
     * @return this component (the default table cell renderer)
     */
    public Component getTableCellRendererComponent(JTable table, Object value,
                                                   boolean isSelected,
                                                   boolean hasFocus, int row,
                                                   int column)
    {
      super.getTableCellRendererComponent(table, value, isSelected, hasFocus,
                                          row, column);
      if (value instanceof Icon)
        {
          Icon iconValue = (Icon) value;
          setIcon(iconValue);
        }
      else
        {
          setIcon(null);
        }
      setText("");
      return this;
    }
  }

    /**
     * The JTable text component (used in editing) always has the table
     * as its parent. The scrollRectToVisible must be adjusted taking the
     * relative component position.
     *
     * @author Audrius Meskauskas (AudriusA@Bioinformatics.org)
     */
    private class TableTextField extends JTextField
    {
      /**
       * Create the text field without the border.
       */
      TableTextField()
      {
        setBorder(BorderFactory.createLineBorder(getGridColor(), 2));
      }
    }


  private static final long serialVersionUID = 3876025080382781659L;

  /**
   * This table, for referring identically name methods from inner classes.
   */
  final JTable this_table = this;


  /**
   * When resizing columns, do not automatically change any columns. In this
   * case the table should be enclosed in a {@link JScrollPane} in order to
   * accomodate cases in which the table size exceeds its visible area.
   */
  public static final int AUTO_RESIZE_OFF = 0;

  /**
   * When resizing column <code>i</code>, automatically change only the
   * single column <code>i+1</code> to provide or absorb excess space
   * requirements.
   */
  public static final int AUTO_RESIZE_NEXT_COLUMN = 1;

  /**
   * When resizing column <code>i</code> in a table of <code>n</code>
   * columns, automatically change all columns in the range <code>[i+1,
   * n)</code>, uniformly, to provide or absorb excess space requirements.
   */
  public static final int AUTO_RESIZE_SUBSEQUENT_COLUMNS = 2;

  /**
   * When resizing column <code>i</code> in a table of <code>n</code>
   * columns, automatically change all columns in the range <code>[0,
   * n)</code> (with the exception of column i) uniformly, to provide or
   * absorb excess space requirements.
   */
  public static final int AUTO_RESIZE_ALL_COLUMNS = 4;

  /**
   * When resizing column <code>i</code> in a table of <code>n</code>
   * columns, automatically change column <code>n-1</code> (the last column
   * in the table) to provide or absorb excess space requirements.
   */
  public static final int AUTO_RESIZE_LAST_COLUMN = 3;

  /**
   * A table mapping {@link java.lang.Class} objects to
   * {@link TableCellEditor} objects. This table is consulted by the
   * FIXME
   */
  protected Hashtable defaultEditorsByColumnClass = new Hashtable();

  /**
   * A table mapping {@link java.lang.Class} objects to
   * {@link TableCellEditor} objects. This table is consulted by the
   * FIXME
   */
  protected Hashtable defaultRenderersByColumnClass = new Hashtable();

  /**
   * The column that is edited, -1 if the table is not edited currently.
   */
  protected int editingColumn;

  /**
   * The row that is edited, -1 if the table is not edited currently.
   */
  protected int editingRow;

  /**
   * The component that is used for editing.
   * <code>null</code> if the table is not editing currently.
   *
   */
  protected transient Component editorComp;


  /**
   * Whether or not the table should automatically compute a matching
   * {@link TableColumnModel} and assign it to the {@link #columnModel}
   * property when the {@link #dataModel} property is changed.
   *
   * @see #setModel(TableModel)
   * @see #createDefaultColumnsFromModel()
   * @see #setColumnModel(TableColumnModel)
   * @see #setAutoCreateColumnsFromModel(boolean)
   * @see #getAutoCreateColumnsFromModel()
   */
  protected boolean autoCreateColumnsFromModel;

  /**
   * A numeric code specifying the resizing behavior of the table. Must be
   * one of {@link #AUTO_RESIZE_ALL_COLUMNS} (the default), {@link
   * #AUTO_RESIZE_LAST_COLUMN}, {@link #AUTO_RESIZE_NEXT_COLUMN}, {@link
   * #AUTO_RESIZE_SUBSEQUENT_COLUMNS}, or {@link #AUTO_RESIZE_OFF}.
   *
   * @see #doLayout()
   * @see #setAutoResizeMode(int)
   * @see #getAutoResizeMode()
   */
  protected int autoResizeMode;

  /**
   * The height in pixels of any row of the table. All rows in a table are
   * of uniform height. This differs from column width, which varies on a
   * per-column basis, and is stored in the individual columns of the
   * {@link #columnModel}.
   *
   * @see #getRowHeight()
   * @see #setRowHeight(int)
   * @see TableColumn#getWidth()
   * @see TableColumn#setWidth(int)
   */
  protected int rowHeight;

  /**
   * The height in pixels of the gap left between any two rows of the table.
   *
   * @see #setRowMargin(int)
   * @see #getRowHeight()
   * @see #getIntercellSpacing()
   * @see #setIntercellSpacing(Dimension)
   * @see TableColumnModel#getColumnMargin()
   * @see TableColumnModel#setColumnMargin(int)
   */
  protected int rowMargin;

  /**
   * Whether or not the table should allow row selection. If the table
   * allows both row <em>and</em> column selection, it is said to allow
   * "cell selection". Previous versions of the JDK supported cell
   * selection as an independent concept, but it is now represented solely
   * in terms of simultaneous row and column selection.
   *
   * @see TableColumnModel#getColumnSelectionAllowed()
   * @see #setRowSelectionAllowed(boolean)
   * @see #getRowSelectionAllowed()
   * @see #getCellSelectionEnabled()
   * @see #setCellSelectionEnabled(boolean)
   */
  protected boolean rowSelectionAllowed;

  /**
   * Obsolete. Use {@link #rowSelectionAllowed}, {@link
   * #getColumnSelectionAllowed}, or the combined methods {@link
   * #getCellSelectionEnabled} and {@link #setCellSelectionEnabled(boolean)}.
   */
  protected boolean cellSelectionEnabled;

  /**
   * The model for data stored in the table. Confusingly, the published API
   * requires that this field be called <code>dataModel</code>, despite its
   * property name. The table listens to its model as a {@link
   * TableModelListener}.
   *
   * @see #tableChanged(TableModelEvent)
   * @see TableModel#addTableModelListener(TableModelListener)
   */
  protected TableModel dataModel;

  /**
   * <p>A model of various aspects of the columns of the table, <em>not
   * including</em> the data stored in them. The {@link TableColumnModel}
   * is principally concerned with holding a set of {@link TableColumn}
   * objects, each of which describes the display parameters of a column
   * and the numeric index of the column from the data model which the
   * column is presenting.</p>
   *
   * <p>The TableColumnModel also contains a {@link ListSelectionModel} which
   * indicates which columns are currently selected. This selection model
   * works in combination with the {@link #selectionModel} of the table
   * itself to specify a <em>table selection</em>: a combination of row and
   * column selections.</p>
   *
   * <p>Most application programmers do not need to work with this property
   * at all: setting {@link #autoCreateColumnsFromModel} will construct the
   * columnModel automatically, and the table acts as a facade for most of
   * the interesting properties of the columnModel anyways.</p>
   *
   * @see #setColumnModel(TableColumnModel)
   * @see #getColumnModel()
   */
  protected TableColumnModel columnModel;

  /**
   * A model of the rows of this table which are currently selected. This
   * model is used in combination with the column selection model held as a
   * member of the {@link #columnModel} property, to represent the rows and
   * columns (or both: cells) of the table which are currently selected.
   *
   * @see #rowSelectionAllowed
   * @see #setSelectionModel(ListSelectionModel)
   * @see #getSelectionModel()
   * @see TableColumnModel#getSelectionModel()
   * @see ListSelectionModel#addListSelectionListener(ListSelectionListener)
   */
  protected ListSelectionModel selectionModel;

  /**
   * The current cell editor.
   */
  protected TableCellEditor cellEditor;

  /**
   * Whether or not drag-and-drop is enabled on this table.
   *
   * @see #setDragEnabled(boolean)
   * @see #getDragEnabled()
   */
  private boolean dragEnabled;

  /**
   * The color to paint the grid lines of the table, when either {@link
   * #showHorizontalLines} or {@link #showVerticalLines} is set.
   *
   * @see #setGridColor(Color)
   * @see #getGridColor()
   */
  protected Color gridColor;

  /**
   * The size this table would prefer its viewport assume, if it is
   * contained in a {@link JScrollPane}.
   *
   * @see #setPreferredScrollableViewportSize(Dimension)
   * @see #getPreferredScrollableViewportSize()
   */
  protected Dimension preferredViewportSize;

  /**
   * The color to paint the background of selected cells. Fires a property
   * change event with name {@link #SELECTION_BACKGROUND_CHANGED_PROPERTY}
   * when its value changes.
   *
   * @see #setSelectionBackground(Color)
   * @see #getSelectionBackground()
   */
  protected Color selectionBackground;

  /**
   * The name carried in property change events when the {@link
   * #selectionBackground} property changes.
   */
  private static final String SELECTION_BACKGROUND_CHANGED_PROPERTY = "selectionBackground";

  /**
   * The color to paint the foreground of selected cells. Fires a property
   * change event with name {@link #SELECTION_FOREGROUND_CHANGED_PROPERTY}
   * when its value changes.
   *
   * @see #setSelectionForeground(Color)
   * @see #getSelectionForeground()
   */
  protected Color selectionForeground;

  /**
   * The name carried in property change events when the
   * {@link #selectionForeground} property changes.
   */
  private static final String SELECTION_FOREGROUND_CHANGED_PROPERTY = "selectionForeground";

  /**
   * The showHorizontalLines property.
   */
  protected boolean showHorizontalLines;

  /**
   * The showVerticalLines property.
   */
  protected boolean showVerticalLines;

  /**
   * The tableHeader property.
   */
  protected JTableHeader tableHeader;

  /**
   * The property handler for this table's columns.
   */
  TableColumnPropertyChangeHandler tableColumnPropertyChangeHandler =
    new TableColumnPropertyChangeHandler();

  /**
   * Whether cell editors should receive keyboard focus when the table is
   * activated.
   */
  private boolean surrendersFocusOnKeystroke = false;

  /**
   * A Rectangle object to be reused in {@link #getCellRect}.
   */
  private Rectangle rectCache = new Rectangle();

  /**
   * Indicates if the rowHeight property has been set by a client program or by
   * the UI.
   *
   * @see #setUIProperty(String, Object)
   * @see LookAndFeel#installProperty(JComponent, String, Object)
   */
  private boolean clientRowHeightSet = false;

  /**
   * Stores the sizes and positions of each row, when using non-uniform row
   * heights. Initially the height of all rows is equal and stored in
   * {link #rowHeight}. However, when an application calls
   * {@link #setRowHeight(int,int)}, the table switches to non-uniform
   * row height mode which stores the row heights in the SizeSequence
   * object instead.
   *
   * @see #setRowHeight(int)
   * @see #getRowHeight()
   * @see #getRowHeight(int)
   * @see #setRowHeight(int, int)
   */
  private SizeSequence rowHeights;

  /**
   * This editor serves just a marker that the value must be simply changed to
   * the opposite one instead of starting the editing session.
   */
  private transient TableCellEditor booleanInvertingEditor;

  /**
   * Creates a new <code>JTable</code> instance.
   */
  public JTable ()
  {
    this(null, null, null);
  }

  /**
   * Creates a new <code>JTable</code> instance with the given number
   * of rows and columns.
   *
   * @param numRows an <code>int</code> value
   * @param numColumns an <code>int</code> value
   */
  public JTable (int numRows, int numColumns)
  {
    this(new DefaultTableModel(numRows, numColumns));
  }

  /**
   * Creates a new <code>JTable</code> instance, storing the given data
   * array and heaving the given column names. To see the column names,
   * you must place the JTable into the {@link JScrollPane}.
   *
   * @param data an <code>Object[][]</code> the table data
   * @param columnNames an <code>Object[]</code> the column headers
   */
  public JTable(Object[][] data, Object[] columnNames)
  {
    this(new DefaultTableModel(data, columnNames));
  }

  /**
   * Creates a new <code>JTable</code> instance, using the given data model
   * object that provides information about the table content. The table model
   * object is asked for the table size, other features and also receives
   * notifications in the case when the table has been edited by the user.
   *
   * @param model
   *          the table model.
   */
  public JTable (TableModel model)
  {
    this(model, null, null);
  }

  /**
   * Creates a new <code>JTable</code> instance, using the given model object
   * that provides information about the table content. The table data model
   * object is asked for the table size, other features and also receives
   * notifications in the case when the table has been edited by the user. The
   * table column model provides more detailed control on the table column
   * related features.
   *
   * @param dm
   *          the table data mode
   * @param cm
   *          the table column model
   */
  public JTable (TableModel dm, TableColumnModel cm)
  {
    this(dm, cm, null);
  }

  /**
   * Creates a new <code>JTable</code> instance, providing data model,
   * column model and list selection model. The list selection model
   * manages the selections.
   *
   * @param dm data model (manages table data)
   * @param cm column model (manages table columns)
   * @param sm list selection model (manages table selections)
   */
  public JTable (TableModel dm, TableColumnModel cm, ListSelectionModel sm)
  {
    boolean autoCreate = false;
    TableColumnModel columnModel;
    if (cm != null)
        columnModel = cm;
    else
      {
        columnModel = createDefaultColumnModel();
        autoCreate = true;
      }

    // Initialise the intercelar spacing before setting the column model to
    // avoid firing unnecessary events.
    // The initial incellar spacing is new Dimenstion(1,1).
    rowMargin = 1;
    columnModel.setColumnMargin(1);
    setColumnModel(columnModel);

    setSelectionModel(sm == null ? createDefaultSelectionModel() : sm);
    setModel(dm == null ? createDefaultDataModel() : dm);
    setAutoCreateColumnsFromModel(autoCreate);
    initializeLocalVars();

    // The following four lines properly set the lead selection indices.
    // After this, the UI will handle the lead selection indices.
    // FIXME: this should probably not be necessary, if the UI is installed
    // before the TableModel is set then the UI will handle things on its
    // own, but certain variables need to be set before the UI can be installed
    // so we must get the correct order for all the method calls in this
    // constructor.
    // These four lines are not needed.  A Mauve test that shows this is
    // gnu.testlet.javax.swing.JTable.constructors(linesNotNeeded).
    // selectionModel.setAnchorSelectionIndex(-1);
    // selectionModel.setLeadSelectionIndex(-1);
    // columnModel.getSelectionModel().setAnchorSelectionIndex(-1);
    // columnModel.getSelectionModel().setLeadSelectionIndex(-1);
    updateUI();
  }

  /**
   * Creates a new <code>JTable</code> instance that uses data and column
   * names, stored in {@link Vector}s.
   *
   * @param data the table data
   * @param columnNames the table column names.
   */
  public JTable(Vector data, Vector columnNames)
  {
    this(new DefaultTableModel(data, columnNames));
  }

  /**
   * Initialize local variables to default values.
   */
  protected void initializeLocalVars()
  {
    setTableHeader(createDefaultTableHeader());
    if (autoCreateColumnsFromModel)
      createDefaultColumnsFromModel();
    this.columnModel.addColumnModelListener(this);

    this.autoResizeMode = AUTO_RESIZE_SUBSEQUENT_COLUMNS;
    setRowHeight(16);
    this.rowMargin = 1;
    this.rowSelectionAllowed = true;

    // this.accessibleContext = new AccessibleJTable();
    this.cellEditor = null;

    // COMPAT: Both Sun and IBM have drag enabled
    this.dragEnabled = false;
    this.preferredViewportSize = new Dimension(450,400);
    this.showHorizontalLines = true;
    this.showVerticalLines = true;
    this.editingColumn = -1;
    this.editingRow = -1;
  }

  /**
   * Add the new table column. The table column class allows to specify column
   * features more precisely, setting the preferred width, column data type
   * (column class) and table headers.
   *
   * There is no need the add columns to the table if the default column
   * handling is sufficient.
   *
   * @param column
   *          the new column to add.
   */
  public void addColumn(TableColumn column)
  {
    if (column.getHeaderValue() == null)
      {
        String name = dataModel.getColumnName(column.getModelIndex());
        column.setHeaderValue(name);
      }

    columnModel.addColumn(column);
    column.addPropertyChangeListener(tableColumnPropertyChangeHandler);
  }

  /**
   * Create the default editors for this table. The default method creates
   * the editor for Booleans.
   *
   * Other fields are edited as strings at the moment.
   */
  protected void createDefaultEditors()
  {
    JCheckBox box = new BooleanCellRenderer().getCheckBox();
    box.setBorder(BorderFactory.createLineBorder(getGridColor(), 2));
    box.setBorderPainted(true);
    booleanInvertingEditor = new DefaultCellEditor(box);
    setDefaultEditor(Boolean.class, booleanInvertingEditor);
  }

  /**
   * Create the default renderers for this table. The default method creates
   * renderers for Boolean, Number, Double, Date, Icon and ImageIcon.
   *
   */
  protected void createDefaultRenderers()
  {
    setDefaultRenderer(Boolean.class, new BooleanCellRenderer());
    setDefaultRenderer(Number.class, new NumberCellRenderer());
    setDefaultRenderer(Double.class, new DoubleCellRenderer());
    setDefaultRenderer(Double.class, new FloatCellRenderer());
    setDefaultRenderer(Date.class, new DateCellRenderer());
    setDefaultRenderer(Icon.class, new IconCellRenderer());
    setDefaultRenderer(ImageIcon.class, new IconCellRenderer());
  }

  /**
   * @deprecated 1.0.2, replaced by <code>new JScrollPane(JTable)</code>
   */
  public static JScrollPane createScrollPaneForTable(JTable table)
  {
    return new JScrollPane(table);
  }

  /**
   * Create the default table column model that is used if the user-defined
   * column model is not provided. The default method creates
   * {@link DefaultTableColumnModel}.
   *
   * @return the created table column model.
   */
  protected TableColumnModel createDefaultColumnModel()
  {
    return new DefaultTableColumnModel();
  }

  /**
   * Create the default table data model that is used if the user-defined
   * data model is not provided. The default method creates
   * {@link DefaultTableModel}.
   *
   * @return the created table data model.
   */
  protected TableModel createDefaultDataModel()
  {
    return new DefaultTableModel();
  }

  /**
   * Create the default table selection model that is used if the user-defined
   * selection model is not provided. The default method creates
   * {@link DefaultListSelectionModel}.
   *
   * @return the created table data model.
   */
  protected ListSelectionModel createDefaultSelectionModel()
  {
    return new DefaultListSelectionModel();
  }

  /**
   * Create the default table header, if the user - defined table header is not
   * provided.
   *
   * @return the default table header.
   */
  protected JTableHeader createDefaultTableHeader()
  {
    return new JTableHeader(columnModel);
  }

  /**
   * Invoked when the column is added. Revalidates and repains the table.
   */
  public void columnAdded (TableColumnModelEvent event)
  {
    revalidate();
    repaint();
  }

  /**
   * Invoked when the column margin is changed.
   * Revalidates and repains the table.
   */
  public void columnMarginChanged (ChangeEvent event)
  {
    revalidate();
    repaint();
  }

  /**
   * Invoked when the column is moved. Revalidates and repains the table.
   */
  public void columnMoved (TableColumnModelEvent event)
  {
    if (isEditing())
      editingCanceled(null);
    revalidate();
    repaint();
  }

  /**
   * Invoked when the column is removed. Revalidates and repains the table.
   */
  public void columnRemoved (TableColumnModelEvent event)
  {
    revalidate();
    repaint();
  }

  /**
   * Invoked when the the column selection changes, repaints the changed
   * columns. It is not recommended to override this method, register the
   * listener instead.
   */
  public void columnSelectionChanged (ListSelectionEvent event)
  {
    // We must limit the indices to the bounds of the JTable's model, because
    // we might get values of -1 or greater then columnCount in the case
    // when columns get removed.
    int idx0 = Math.max(0, Math.min(getColumnCount() - 1,
                                    event.getFirstIndex()));
    int idxn = Math.max(0, Math.min(getColumnCount() - 1,
                                    event.getLastIndex()));

    int minRow = 0;
    int maxRow = getRowCount() - 1;
    if (getRowSelectionAllowed())
      {
        minRow = selectionModel.getMinSelectionIndex();
        maxRow = selectionModel.getMaxSelectionIndex();
        int leadRow = selectionModel.getLeadSelectionIndex();
        if (minRow == -1 && maxRow == -1)
          {
            minRow = leadRow;
            maxRow = leadRow;
          }
        else
          {
            // In this case we need to repaint also the range to leadRow, not
            // only between min and max.
            if (leadRow != -1)
              {
                minRow = Math.min(minRow, leadRow);
                maxRow = Math.max(maxRow, leadRow);
              }
          }
      }
    if (minRow != -1 && maxRow != -1)
      {
        Rectangle first = getCellRect(minRow, idx0, false);
        Rectangle last = getCellRect(maxRow, idxn, false);
        Rectangle dirty = SwingUtilities.computeUnion(first.x, first.y,
                                                      first.width,
                                                      first.height, last);
        repaint(dirty);
      }
  }

  /**
   * Invoked when the editing is cancelled.
   */
  public void editingCanceled (ChangeEvent event)
  {
    if (editorComp!=null)
      {
        remove(editorComp);
        repaint(editorComp.getBounds());
        editorComp = null;
      }
  }

  /**
   * Finish the current editing session and update the table with the
   * new value by calling {@link #setValueAt}.
   *
   * @param event the change event
   */
  public void editingStopped (ChangeEvent event)
  {
    if (editorComp!=null)
      {
        remove(editorComp);
        setValueAt(cellEditor.getCellEditorValue(), editingRow, editingColumn);
        repaint(editorComp.getBounds());
        editorComp = null;
      }
    requestFocusInWindow();
  }

  /**
   * Invoked when the table changes.
   * <code>null</code> means everything changed.
   */
  public void tableChanged (TableModelEvent event)
  {
    // update the column model from the table model if the structure has
    // changed and the flag autoCreateColumnsFromModel is set
    if (event == null || (event.getFirstRow() == TableModelEvent.HEADER_ROW))
      handleCompleteChange(event);
    else if (event.getType() == TableModelEvent.INSERT)
      handleInsert(event);
    else if (event.getType() == TableModelEvent.DELETE)
      handleDelete(event);
    else
      handleUpdate(event);
  }

  /**
   * Handles a request for complete relayout. This is the case when
   * event.getFirstRow() == TableModelEvent.HEADER_ROW.
   *
   * @param ev the table model event
   */
  private void handleCompleteChange(TableModelEvent ev)
  {
    clearSelection();
    checkSelection();
    rowHeights = null;
    if (getAutoCreateColumnsFromModel())
      createDefaultColumnsFromModel();
    else
      resizeAndRepaint();
  }

  /**
   * Handles table model insertions.
   *
   * @param ev the table model event
   */
  private void handleInsert(TableModelEvent ev)
  {
    // Sync selection model with data model.
    int first = ev.getFirstRow();
    if (first < 0)
      first = 0;
    int last = ev.getLastRow();
    if (last < 0)
      last = getRowCount() - 1;
    selectionModel.insertIndexInterval(first, last - first + 1, true);
    checkSelection();

    // For variable height rows we must update the SizeSequence thing.
    if (rowHeights != null)
      {
        rowHeights.insertEntries(first, last - first + 1, rowHeight);
        // TODO: We repaint the whole thing when the rows have variable
        // heights. We might want to handle this better though.
        repaint();
      }
    else
      {
        // Repaint the dirty region and revalidate.
        int rowHeight = getRowHeight();
        Rectangle dirty = new Rectangle(0, first * rowHeight,
                                        getColumnModel().getTotalColumnWidth(),
                                        (getRowCount() - first) * rowHeight);
        repaint(dirty);
      }
    revalidate();
  }

  /**
   * Handles table model deletions.
   *
   * @param ev the table model event
   */
  private void handleDelete(TableModelEvent ev)
  {
    // Sync selection model with data model.
    int first = ev.getFirstRow();
    if (first < 0)
      first = 0;
    int last = ev.getLastRow();
    if (last < 0)
      last = getRowCount() - 1;

    selectionModel.removeIndexInterval(first, last);

    checkSelection();

    if (dataModel.getRowCount() == 0)
      clearSelection();

    // For variable height rows we must update the SizeSequence thing.
    if (rowHeights != null)
      {
        rowHeights.removeEntries(first, last - first + 1);
        // TODO: We repaint the whole thing when the rows have variable
        // heights. We might want to handle this better though.
        repaint();
      }
    else
      {
        // Repaint the dirty region and revalidate.
        int rowHeight = getRowHeight();
        int oldRowCount = getRowCount() + last - first + 1;
        Rectangle dirty = new Rectangle(0, first * rowHeight,
                                        getColumnModel().getTotalColumnWidth(),
                                        (oldRowCount - first) * rowHeight);
        repaint(dirty);
      }
    revalidate();
  }

  /**
   * Handles table model updates without structural changes.
   *
   * @param ev the table model event
   */
  private void handleUpdate(TableModelEvent ev)
  {
    if (rowHeights == null)
      {
        // Some cells have been changed without changing the structure.
        // Figure out the dirty rectangle and repaint.
        int firstRow = ev.getFirstRow();
        int lastRow = ev.getLastRow();
        int col = ev.getColumn();
        Rectangle dirty;
        if (col == TableModelEvent.ALL_COLUMNS)
          {
            // All columns changed.
            dirty = new Rectangle(0, firstRow * getRowHeight(),
                                  getColumnModel().getTotalColumnWidth(), 0);
          }
        else
          {
            // Only one cell or column of cells changed.
            // We need to convert to view column first.
            int column = convertColumnIndexToModel(col);
            dirty = getCellRect(firstRow, column, false);
          }

        // Now adjust the height of the dirty region.
        dirty.height = (lastRow + 1) * getRowHeight();
        // .. and repaint.
        repaint(dirty);
      }
    else
      {
        // TODO: We repaint the whole thing when the rows have variable
        // heights. We might want to handle this better though.
        repaint();
      }
  }

  /**
   * Helper method for adjusting the lead and anchor indices when the
   * table structure changed. This sets the lead and anchor to -1 if there's
   * no more rows, or set them to 0 when they were at -1 and there are actually
   * some rows now.
   */
  private void checkSelection()
  {
    TableModel m = getModel();
    ListSelectionModel sm = selectionModel;
    if (m != null)
      {
        int lead = sm.getLeadSelectionIndex();
        int c = m.getRowCount();
        if (c == 0 && lead != -1)
          {
            // No rows in the model, reset lead and anchor to -1.
            sm.setValueIsAdjusting(true);
            sm.setAnchorSelectionIndex(-1);
            sm.setLeadSelectionIndex(-1);
            sm.setValueIsAdjusting(false);
          }
        else if (c != 0 && lead == -1)
          {
            // We have rows, but no lead/anchor. Set them to 0. We
            // do a little trick here so that the actual selection is not
            // touched.
            if (sm.isSelectedIndex(0))
              sm.addSelectionInterval(0, 0);
            else
              sm.removeSelectionInterval(0, 0);
          }
        // Nothing to do in the other cases.
      }
  }

  /**
   * Invoked when another table row is selected. It is not recommended
   * to override thid method, register the listener instead.
   */
  public void valueChanged (ListSelectionEvent event)
  {
    // If we are in the editing process, end the editing session.
    if (isEditing())
      editingStopped(null);

    // Repaint the changed region.
    int first = Math.max(0, Math.min(getRowCount() - 1, event.getFirstIndex()));
    int last = Math.max(0, Math.min(getRowCount() - 1, event.getLastIndex()));
    Rectangle rect1 = getCellRect(first, 0, false);
    Rectangle rect2 = getCellRect(last, getColumnCount() - 1, false);
    Rectangle dirty = SwingUtilities.computeUnion(rect2.x, rect2.y,
                                                  rect2.width, rect2.height,
                                                  rect1);
    repaint(dirty);
  }

 /**
   * Returns index of the column that contains specified point
   * or -1 if this table doesn't contain this point.
   *
   * @param point point to identify the column
   * @return index of the column that contains specified point or
   * -1 if this table doesn't contain this point.
   */
  public int columnAtPoint(Point point)
  {
    int ncols = getColumnCount();
    Dimension gap = getIntercellSpacing();
    TableColumnModel cols = getColumnModel();
    int x = point.x;

    for (int i = 0; i < ncols; ++i)
      {
        int width = cols.getColumn(i).getWidth()
                    + (gap == null ? 0 : gap.width);
        if (0 <= x && x < width)
          return i;
        x -= width;
      }
    return -1;
  }

  /**
   * Returns index of the row that contains specified point or -1 if this table
   * doesn't contain this point.
   *
   * @param point point to identify the row
   * @return index of the row that contains specified point or -1 if this table
   *         doesn't contain this point.
   */
  public int rowAtPoint(Point point)
  {
    if (point != null)
      {
        int nrows = getRowCount();
        int r;
        int y = point.y;
        if (rowHeights == null)
          {
            int height = getRowHeight();
            r = y / height;
          }
        else
          r = rowHeights.getIndex(y);

        if (r < 0 || r >= nrows)
          return -1;
        else
          return r;
      }
    else
      return -1;
  }

  /**
   * Calculate the visible rectangle for a particular row and column. The
   * row and column are specified in visual terms; the column may not match
   * the {@link #dataModel} column.
   *
   * @param row the visible row to get the cell rectangle of
   *
   * @param column the visible column to get the cell rectangle of, which may
   * differ from the {@link #dataModel} column
   *
   * @param includeSpacing whether or not to include the cell margins in the
   * resulting cell. If <code>false</code>, the result will only contain the
   * inner area of the target cell, not including its margins.
   *
   * @return a rectangle enclosing the specified cell
   */
  public Rectangle getCellRect(int row,
                               int column,
                               boolean includeSpacing)
  {
    Rectangle cellRect = new Rectangle(0, 0, 0, 0);

    // Check for valid range vertically.
    if (row >= getRowCount())
      {
        cellRect.height = getHeight();
      }
    else if (row >= 0)
      {
        cellRect.height = getRowHeight(row);
        if (rowHeights == null)
          cellRect.y = row * cellRect.height;
        else
          cellRect.y = rowHeights.getPosition(row);

        if (! includeSpacing)
          {
            // The rounding here is important.
            int rMargin = getRowMargin();
            cellRect.y += rMargin / 2;
            cellRect.height -= rMargin;
          }
      }
    // else row < 0, y = height = 0

    // Check for valid range horizontally.
    if (column < 0)
      {
        if (! getComponentOrientation().isLeftToRight())
          {
            cellRect.x = getWidth();
          }
      }
    else if (column >= getColumnCount())
      {
        if (getComponentOrientation().isLeftToRight())
          {
            cellRect.x = getWidth();
          }
      }
    else
      {
        TableColumnModel tcm = getColumnModel();
        if (getComponentOrientation().isLeftToRight())
          {
            for (int i = 0; i < column; i++)
              cellRect.x += tcm.getColumn(i).getWidth();
          }
        else
          {
            for (int i = tcm.getColumnCount() - 1; i > column; i--)
              cellRect.x += tcm.getColumn(i).getWidth();
          }
        cellRect.width = tcm.getColumn(column).getWidth();
        if (! includeSpacing)
          {
            // The rounding here is important.
            int cMargin = tcm.getColumnMargin();
            cellRect.x += cMargin / 2;
            cellRect.width -= cMargin;
          }
      }

    return cellRect;
  }

  public void clearSelection()
  {
    selectionModel.clearSelection();
    getColumnModel().getSelectionModel().clearSelection();
  }

  /**
   * Get the value of the selectedRow property by delegation to
   * the {@link ListSelectionModel#getMinSelectionIndex} method of the
   * {@link #selectionModel} field.
   *
   * @return The current value of the selectedRow property
   */
  public int getSelectedRow ()
  {
    return selectionModel.getMinSelectionIndex();
  }

  /**
   * Get the value of the {@link #selectionModel} property.
   *
   * @return The current value of the property
   */
  public ListSelectionModel getSelectionModel()
  {
    //Neither Sun nor IBM returns null if rowSelection not allowed
    return selectionModel;
  }

  public int getScrollableBlockIncrement(Rectangle visibleRect, int orientation, int direction)
  {
    int block;
    if (orientation == SwingConstants.HORIZONTAL)
      {
        block = visibleRect.width;
      }
    else
      {
        int rowHeight = getRowHeight();
        if (rowHeight > 0)
          block = Math.max(rowHeight, // Little hack for useful rounding.
                           (visibleRect.height / rowHeight) * rowHeight);
        else
          block = visibleRect.height;
      }
    return block;
  }

  /**
   * Get the value of the <code>scrollableTracksViewportHeight</code> property.
   *
   * @return The constant value <code>false</code>
   */
  public boolean getScrollableTracksViewportHeight()
  {
    return false;
  }

  /**
   * Get the value of the <code>scrollableTracksViewportWidth</code> property.
   *
   * @return <code>true</code> unless the {@link #autoResizeMode} property is
   * <code>AUTO_RESIZE_OFF</code>
   */
  public boolean getScrollableTracksViewportWidth()
  {
    if (autoResizeMode == AUTO_RESIZE_OFF)
      return false;
    else
      return true;
  }

  /**
   * Return the preferred scrolling amount (in pixels) for the given scrolling
   * direction and orientation. This method handles a partially exposed row by
   * returning the distance required to completely expose the item. When
   * scrolling the top item is completely exposed.
   *
   * @param visibleRect the currently visible part of the component.
   * @param orientation the scrolling orientation
   * @param direction the scrolling direction (negative - up, positive -down).
   *          The values greater than one means that more mouse wheel or similar
   *          events were generated, and hence it is better to scroll the longer
   *          distance.
   *
   * @author Roman Kennke (kennke@aicas.com)
   */
  public int getScrollableUnitIncrement(Rectangle visibleRect, int orientation,
                                        int direction)
  {
    int unit;
    if (orientation == SwingConstants.HORIZONTAL)
      unit = 100;
    else
      {
        unit = getRowHeight();
        // The following adjustment doesn't work for variable height rows.
        // It fully exposes partially visible rows in the scrolling direction.
        if (rowHeights == null)
          {
            if (direction > 0)
              {
                // Scroll down.
                // How much pixles are exposed from the last item?
                int exposed = (visibleRect.y + visibleRect.height) % unit;
                if (exposed > 0 && exposed < unit - 1)
                  unit = unit - exposed - 1;
              }
            else
              {
                // Scroll up.
                int exposed = visibleRect.y % unit;
                if (exposed > 0 && exposed < unit)
                  unit = exposed;
              }
          }
      }
    return unit;
  }

  /**
   * Get the cell editor, suitable for editing the given cell. The default
   * method requests the editor from the column model. If the column model does
   * not provide the editor, the call is forwarded to the
   * {@link #getDefaultEditor(Class)} with the parameter, obtained from
   * {@link TableModel#getColumnClass(int)}.
   *
   * @param row the cell row
   * @param column the cell column
   * @return the editor to edit that cell
   */
  public TableCellEditor getCellEditor(int row, int column)
  {
    TableCellEditor editor = columnModel.getColumn(column).getCellEditor();

    if (editor == null)
      {
        int mcolumn = convertColumnIndexToModel(column);
        editor = getDefaultEditor(dataModel.getColumnClass(mcolumn));
      }

    return editor;
  }

  /**
   * Get the default editor for editing values of the given type
   * (String, Boolean and so on).
   *
   * @param columnClass the class of the value that will be edited.
   *
   * @return the editor, suitable for editing this data type
   */
  public TableCellEditor getDefaultEditor(Class<?> columnClass)
  {
    if (defaultEditorsByColumnClass.containsKey(columnClass))
      return (TableCellEditor) defaultEditorsByColumnClass.get(columnClass);
    else
      {
        JTextField t = new TableTextField();
        TableCellEditor r = new DefaultCellEditor(t);
        defaultEditorsByColumnClass.put(columnClass, r);
        return r;
      }
  }

  /**
   * Get the cell renderer for rendering the given cell.
   *
   * @param row the cell row
   * @param column the cell column
   * @return the cell renderer to render that cell.
   */
  public TableCellRenderer getCellRenderer(int row, int column)
  {
    TableCellRenderer renderer = null;
    if (columnModel.getColumnCount() > 0)
      renderer = columnModel.getColumn(column).getCellRenderer();
    if (renderer == null)
      {
        int mcolumn = convertColumnIndexToModel(column);
        renderer = getDefaultRenderer(dataModel.getColumnClass(mcolumn));
      }
    return renderer;
  }

  /**
   * Set default renderer for rendering the given data type.
   *
   * @param columnClass the data type (String, Boolean and so on) that must be
   *          rendered.
   * @param rend the renderer that will rend this data type
   */
  public void setDefaultRenderer(Class<?> columnClass, TableCellRenderer rend)
  {
    defaultRenderersByColumnClass.put(columnClass, rend);
  }

  /**
   * Get the default renderer for rendering the given data type.
   *
   * @param columnClass the data that must be rendered
   *
   * @return the appropriate defauld renderer for rendering that data type.
   */
  public TableCellRenderer getDefaultRenderer(Class<?> columnClass)
  {
    if (defaultRenderersByColumnClass.containsKey(columnClass))
      return (TableCellRenderer) defaultRenderersByColumnClass.get(columnClass);
    else
      {
        TableCellRenderer r = new DefaultTableCellRenderer();
        defaultRenderersByColumnClass.put(columnClass, r);
        return r;
      }
  }

  /**
   * Convert the table model index into the table column number.
   * The model number need not match the real column position. The columns
   * may be rearranged by the user with mouse at any time by dragging the
   * column headers.
   *
   * @param vc the column number (0=first).
   *
   * @return the table column model index of this column.
   *
   * @see TableColumn#getModelIndex()
   */
  public int convertColumnIndexToModel(int vc)
  {
    if (vc < 0)
      return vc;
    else
      return columnModel.getColumn(vc).getModelIndex();
  }

  /**
   * Convert the table column number to the table column model index.
   * The model number need not match the real column position. The columns
   * may be rearranged by the user with mouse at any time by dragging the
   * column headers.
   *
   * @param mc the table column index (0=first).
   *
   * @return the table column number in the model
   *
   * @see TableColumn#getModelIndex()
   */
  public int convertColumnIndexToView(int mc)
  {
    if (mc < 0)
      return mc;
    int ncols = getColumnCount();
    for (int vc = 0; vc < ncols; ++vc)
      {
        if (columnModel.getColumn(vc).getModelIndex() == mc)
          return vc;
      }
    return -1;
  }

  /**
   * Prepare the renderer for rendering the given cell.
   *
   * @param renderer the renderer being prepared
   * @param row the row of the cell being rendered
   * @param column the column of the cell being rendered
   *
   * @return the component which .paint() method will paint the cell.
   */
  public Component prepareRenderer(TableCellRenderer renderer,
                                   int row,
                                   int column)
  {
    boolean rowSelAllowed = getRowSelectionAllowed();
    boolean colSelAllowed = getColumnSelectionAllowed();
    boolean isSel = false;
    if (rowSelAllowed && colSelAllowed || !rowSelAllowed && !colSelAllowed)
      isSel = isCellSelected(row, column);
    else
      isSel = isRowSelected(row) && getRowSelectionAllowed()
           || isColumnSelected(column) && getColumnSelectionAllowed();

    // Determine the focused cell. The focused cell is the cell at the
    // leadSelectionIndices of the row and column selection model.
    ListSelectionModel rowSel = getSelectionModel();
    ListSelectionModel colSel = getColumnModel().getSelectionModel();
    boolean hasFocus = hasFocus() && isEnabled()
                       && rowSel.getLeadSelectionIndex() == row
                       && colSel.getLeadSelectionIndex() == column;

    return renderer.getTableCellRendererComponent(this,
                                                  dataModel.getValueAt(row,
                                                                       convertColumnIndexToModel(column)),
                                                  isSel,
                                                  hasFocus,
                                                  row, column);
  }


  /**
   * Get the value of the {@link #autoCreateColumnsFromModel} property.
   *
   * @return The current value of the property
   */
  public boolean getAutoCreateColumnsFromModel()
  {
    return autoCreateColumnsFromModel;
  }

  /**
   * Get the value of the {@link #autoResizeMode} property.
   *
   * @return The current value of the property
   */
  public int getAutoResizeMode()
  {
    return autoResizeMode;
  }

  /**
   * Get the value of the {@link #rowHeight} property.
   *
   * @return The current value of the property
   */
  public int getRowHeight()
  {
    return rowHeight;
  }

  /**
   * Get the height of the specified row.
   *
   * @param row the row whose height to return
   */
  public int getRowHeight(int row)
  {
    int rh = rowHeight;
    if (rowHeights != null)
      rh = rowHeights.getSize(row);
    return rh;
  }


  /**
   * Get the value of the {@link #rowMargin} property.
   *
   * @return The current value of the property
   */
  public int getRowMargin()
  {
    return rowMargin;
  }

  /**
   * Get the value of the {@link #rowSelectionAllowed} property.
   *
   * @return The current value of the property
   *
   * @see #setRowSelectionAllowed(boolean)
   */
  public boolean getRowSelectionAllowed()
  {
    return rowSelectionAllowed;
  }

  /**
   * Get the value of the {@link #cellSelectionEnabled} property.
   *
   * @return The current value of the property
   */
  public boolean getCellSelectionEnabled()
  {
    return getColumnSelectionAllowed() && getRowSelectionAllowed();
  }

  /**
   * Get the value of the {@link #dataModel} property.
   *
   * @return The current value of the property
   */
  public TableModel getModel()
  {
    return dataModel;
  }

  /**
   * Get the value of the <code>columnCount</code> property by
   * delegation to the {@link #columnModel} field.
   *
   * @return The current value of the columnCount property
   */
  public int getColumnCount()
  {
    return columnModel.getColumnCount();
  }

  /**
   * Get the value of the <code>rowCount</code> property by
   * delegation to the {@link #dataModel} field.
   *
   * @return The current value of the rowCount property
   */
  public int getRowCount()
  {
    return dataModel.getRowCount();
  }

  /**
   * Get the value of the {@link #columnModel} property.
   *
   * @return The current value of the property
   */
  public TableColumnModel getColumnModel()
  {
    return columnModel;
  }

  /**
   * Get the value of the <code>selectedColumn</code> property by
   * delegation to the {@link #columnModel} field.
   *
   * @return The current value of the selectedColumn property
   */
  public int getSelectedColumn()
  {
    return columnModel.getSelectionModel().getMinSelectionIndex();
  }

  private static int countSelections(ListSelectionModel lsm)
  {
    int lo = lsm.getMinSelectionIndex();
    int hi = lsm.getMaxSelectionIndex();
    int sum = 0;
    if (lo != -1 && hi != -1)
      {
        switch (lsm.getSelectionMode())
          {
          case ListSelectionModel.SINGLE_SELECTION:
            sum = 1;
            break;

          case ListSelectionModel.SINGLE_INTERVAL_SELECTION:
            sum = hi - lo + 1;
            break;

          case ListSelectionModel.MULTIPLE_INTERVAL_SELECTION:
            for (int i = lo; i <= hi; ++i)
              if (lsm.isSelectedIndex(i))
                ++sum;
            break;
          }
      }
    return sum;
  }

  private static int[] getSelections(ListSelectionModel lsm)
  {
    int sz = countSelections(lsm);
    int [] ret = new int[sz];

    int lo = lsm.getMinSelectionIndex();
    int hi = lsm.getMaxSelectionIndex();
    int j = 0;
    if (lo != -1 && hi != -1)
      {
        switch (lsm.getSelectionMode())
          {
          case ListSelectionModel.SINGLE_SELECTION:
            ret[0] = lo;
            break;

          case ListSelectionModel.SINGLE_INTERVAL_SELECTION:
            for (int i = lo; i <= hi; ++i)
              ret[j++] = i;
            break;

          case ListSelectionModel.MULTIPLE_INTERVAL_SELECTION:
            for (int i = lo; i <= hi; ++i)
              if (lsm.isSelectedIndex(i))
                ret[j++] = i;
            break;
          }
      }
    return ret;
  }

  /**
   * Get the value of the <code>selectedColumnCount</code> property by
   * delegation to the {@link #columnModel} field.
   *
   * @return The current value of the selectedColumnCount property
   */
  public int getSelectedColumnCount()
  {
    return countSelections(columnModel.getSelectionModel());
  }

  /**
   * Get the value of the <code>selectedColumns</code> property by
   * delegation to the {@link #columnModel} field.
   *
   * @return The current value of the selectedColumns property
   */
  public int[] getSelectedColumns()
  {
    return getSelections(columnModel.getSelectionModel());
  }

  /**
   * Get the value of the <code>columnSelectionAllowed</code> property.
   *
   * @return The current value of the columnSelectionAllowed property
   *
   * @see #setColumnSelectionAllowed(boolean)
   */
  public boolean getColumnSelectionAllowed()
  {
    return getColumnModel().getColumnSelectionAllowed();
  }

  /**
   * Get the value of the <code>selectedRowCount</code> property by
   * delegation to the {@link #selectionModel} field.
   *
   * @return The current value of the selectedRowCount property
   */
  public int getSelectedRowCount()
  {
    return countSelections(selectionModel);
  }

  /**
   * Get the value of the <code>selectedRows</code> property by
   * delegation to the {@link #selectionModel} field.
   *
   * @return The current value of the selectedRows property
   */
  public int[] getSelectedRows()
  {
    return getSelections(selectionModel);
  }

  /**
   * Get the value of the {@link #accessibleContext} property.
   *
   * @return The current value of the property
   */
  public AccessibleContext getAccessibleContext()
  {
    if (accessibleContext == null)
      {
        AccessibleJTable ctx = new AccessibleJTable();
        addPropertyChangeListener(ctx);
        TableColumnModel tcm = getColumnModel();
        tcm.addColumnModelListener(ctx);
        tcm.getSelectionModel().addListSelectionListener(ctx);
        getSelectionModel().addListSelectionListener(ctx);

        accessibleContext = ctx;
      }
    return accessibleContext;
  }

  /**
   * Get the value of the {@link #cellEditor} property.
   *
   * @return The current value of the property
   */
  public TableCellEditor getCellEditor()
  {
    return cellEditor;
  }

  /**
   * Get the value of the {@link #dragEnabled} property.
   *
   * @return The current value of the property
   */
  public boolean getDragEnabled()
  {
    return dragEnabled;
  }

  /**
   * Get the value of the {@link #gridColor} property.
   *
   * @return The current value of the property
   */
  public Color getGridColor()
  {
    return gridColor;
  }

  /**
   * Get the value of the <code>intercellSpacing</code> property.
   *
   * @return The current value of the property
   */
  public Dimension getIntercellSpacing()
  {
    return new Dimension(columnModel.getColumnMargin(), rowMargin);
  }

  /**
   * Get the value of the {@link #preferredViewportSize} property.
   *
   * @return The current value of the property
   */
  public Dimension getPreferredScrollableViewportSize()
  {
    return preferredViewportSize;
  }

  /**
   * Get the value of the {@link #selectionBackground} property.
   *
   * @return The current value of the property
   */
  public Color getSelectionBackground()
  {
    return selectionBackground;
  }

  /**
   * Get the value of the {@link #selectionForeground} property.
   *
   * @return The current value of the property
   */
  public Color getSelectionForeground()
  {
    return selectionForeground;
  }

  /**
   * Get the value of the {@link #showHorizontalLines} property.
   *
   * @return The current value of the property
   */
  public boolean getShowHorizontalLines()
  {
    return showHorizontalLines;
  }

  /**
   * Get the value of the {@link #showVerticalLines} property.
   *
   * @return The current value of the property
   */
  public boolean getShowVerticalLines()
  {
    return showVerticalLines;
  }

  /**
   * Get the value of the {@link #tableHeader} property.
   *
   * @return The current value of the property
   */
  public JTableHeader getTableHeader()
  {
    return tableHeader;
  }

  /**
   * Removes specified column from displayable columns of this table.
   *
   * @param column column to removed
   */
  public void removeColumn(TableColumn column)
  {
    columnModel.removeColumn(column);
  }

  /**
   * Moves column at the specified index to new given location.
   *
   * @param column index of the column to move
   * @param targetColumn index specifying new location of the column
   */
  public void moveColumn(int column,int targetColumn)
  {
    columnModel.moveColumn(column, targetColumn);
  }

  /**
   * Set the value of the {@link #autoCreateColumnsFromModel} flag.  If the
   * flag changes from <code>false</code> to <code>true</code>, the
   * {@link #createDefaultColumnsFromModel()} method is called.
   *
   * @param autoCreate  the new value of the flag.
   */
  public void setAutoCreateColumnsFromModel(boolean autoCreate)
  {
    if (autoCreateColumnsFromModel != autoCreate)
    {
      autoCreateColumnsFromModel = autoCreate;
      if (autoCreate)
        createDefaultColumnsFromModel();
    }
  }

  /**
   * Set the value of the {@link #autoResizeMode} property.
   *
   * @param a The new value of the autoResizeMode property
   */
  public void setAutoResizeMode(int a)
  {
    autoResizeMode = a;
    revalidate();
    repaint();
  }

  /**
   * Sets the height for all rows in the table. If you want to change the
   * height of a single row instead, use {@link #setRowHeight(int, int)}.
   *
   * @param r the height to set for all rows
   *
   * @see #getRowHeight()
   * @see #setRowHeight(int, int)
   * @see #getRowHeight(int)
   */
  public void setRowHeight(int r)
  {
    if (r < 1)
      throw new IllegalArgumentException();

    clientRowHeightSet = true;

    rowHeight = r;
    rowHeights = null;
    revalidate();
    repaint();
  }

  /**
   * Sets the height of a single row in the table.
   *
   * @param rh the new row height
   * @param row the row to change the height of
   */
  public void setRowHeight(int row, int rh)
  {
    if (rowHeights == null)
      {
        rowHeights = new SizeSequence(getRowCount(), rowHeight);
      }
    rowHeights.setSize(row, rh);
  }

  /**
   * Set the value of the {@link #rowMargin} property.
   *
   * @param r The new value of the rowMargin property
   */
  public void setRowMargin(int r)
  {
    rowMargin = r;
    revalidate();
    repaint();
  }

  /**
   * Set the value of the {@link #rowSelectionAllowed} property.
   *
   * @param r The new value of the rowSelectionAllowed property
   *
   * @see #getRowSelectionAllowed()
   */
  public void setRowSelectionAllowed(boolean r)
  {
    if (rowSelectionAllowed != r)
      {
        rowSelectionAllowed = r;
        firePropertyChange("rowSelectionAllowed", !r, r);
        repaint();
      }
  }

  /**
   * Set the value of the {@link #cellSelectionEnabled} property.
   *
   * @param c The new value of the cellSelectionEnabled property
   */
  public void setCellSelectionEnabled(boolean c)
  {
    setColumnSelectionAllowed(c);
    setRowSelectionAllowed(c);
    // for backward-compatibility sake:
    cellSelectionEnabled = true;
  }

  /**
   * <p>Set the value of the {@link #dataModel} property.</p>
   *
   * <p>Unregister <code>this</code> as a {@link TableModelListener} from
   * previous {@link #dataModel} and register it with new parameter
   * <code>m</code>.</p>
   *
   * @param m The new value of the model property
   */
  public void setModel(TableModel m)
  {
    // Throw exception is m is null.
    if (m == null)
      throw new IllegalArgumentException();

    // Don't do anything if setting the current model again.
    if (dataModel == m)
      return;

    TableModel oldModel = dataModel;

    // Remove table as TableModelListener from old model.
    if (dataModel != null)
      dataModel.removeTableModelListener(this);

    if (m != null)
      {
        // Set property.
        dataModel = m;

        // Add table as TableModelListener to new model.
        dataModel.addTableModelListener(this);

        // Notify the tableChanged method.
        tableChanged(new TableModelEvent(dataModel,
                                         TableModelEvent.HEADER_ROW));

        // Automatically create columns.
        if (autoCreateColumnsFromModel)
          createDefaultColumnsFromModel();
      }

    // This property is bound, so we fire a property change event.
    firePropertyChange("model", oldModel, dataModel);

    // Repaint table.
    revalidate();
    repaint();
  }

  /**
   * <p>Set the value of the {@link #columnModel} property.</p>
   *
   * <p>Unregister <code>this</code> as a {@link TableColumnModelListener}
   * from previous {@link #columnModel} and register it with new parameter
   * <code>c</code>.</p>
   *
   * @param c The new value of the columnModel property
   */
  public void setColumnModel(TableColumnModel c)
  {
    if (c == null)
      throw new IllegalArgumentException();
    TableColumnModel tmp = columnModel;
    if (tmp != null)
      tmp.removeColumnModelListener(this);
    if (c != null)
      c.addColumnModelListener(this);
    columnModel = c;
    if (dataModel != null && columnModel != null)
      {
        int ncols = getColumnCount();
        TableColumn column;
        for (int i = 0; i < ncols; ++i)
          {
            column = columnModel.getColumn(i);
            if (column.getHeaderValue()==null)
              column.setHeaderValue(dataModel.getColumnName(i));
          }
      }

    // according to Sun's spec we also have to set the tableHeader's
    // column model here
    if (tableHeader != null)
      tableHeader.setColumnModel(c);

    revalidate();
    repaint();
  }

  /**
   * Set the value of the <code>columnSelectionAllowed</code> property.
   *
   * @param c The new value of the property
   *
   * @see #getColumnSelectionAllowed()
   */
  public void setColumnSelectionAllowed(boolean c)
  {
    if (columnModel.getColumnSelectionAllowed() != c)
      {
        columnModel.setColumnSelectionAllowed(c);
        firePropertyChange("columnSelectionAllowed", !c, c);
        repaint();
      }
  }

  /**
   * <p>Set the value of the {@link #selectionModel} property.</p>
   *
   * <p>Unregister <code>this</code> as a {@link ListSelectionListener}
   * from previous {@link #selectionModel} and register it with new
   * parameter <code>s</code>.</p>
   *
   * @param s The new value of the selectionModel property
   */
  public void setSelectionModel(ListSelectionModel s)
  {
    if (s == null)
      throw new IllegalArgumentException();
    ListSelectionModel tmp = selectionModel;
    if (tmp != null)
      tmp.removeListSelectionListener(this);
    if (s != null)
      s.addListSelectionListener(this);
    selectionModel = s;
    checkSelection();
  }

  /**
   * Set the value of the <code>selectionMode</code> property by
   * delegation to the {@link #selectionModel} field. The same selection
   * mode is set for row and column selection models.
   *
   * @param s The new value of the property
   */
  public void setSelectionMode(int s)
  {
    selectionModel.setSelectionMode(s);
    columnModel.getSelectionModel().setSelectionMode(s);

    repaint();
  }

  /**
   * <p>Set the value of the {@link #cellEditor} property.</p>
   *
   * <p>Unregister <code>this</code> as a {@link CellEditorListener} from
   * previous {@link #cellEditor} and register it with new parameter
   * <code>c</code>.</p>
   *
   * @param c The new value of the cellEditor property
   */
  public void setCellEditor(TableCellEditor c)
  {
    TableCellEditor tmp = cellEditor;
    if (tmp != null)
      tmp.removeCellEditorListener(this);
    if (c != null)
      c.addCellEditorListener(this);
    cellEditor = c;
  }

  /**
   * Set the value of the {@link #dragEnabled} property.
   *
   * @param d The new value of the dragEnabled property
   */
  public void setDragEnabled(boolean d)
  {
    dragEnabled = d;
  }

  /**
   * Set the value of the {@link #gridColor} property.
   *
   * @param g The new value of the gridColor property
   */
  public void setGridColor(Color g)
  {
    gridColor = g;
    repaint();
  }

  /**
   * Set the value of the <code>intercellSpacing</code> property.
   *
   * @param i The new value of the intercellSpacing property
   */
  public void setIntercellSpacing(Dimension i)
  {
    rowMargin = i.height;
    columnModel.setColumnMargin(i.width);
    repaint();
  }

  /**
   * Set the value of the {@link #preferredViewportSize} property.
   *
   * @param p The new value of the preferredViewportSize property
   */
  public void setPreferredScrollableViewportSize(Dimension p)
  {
    preferredViewportSize = p;
    revalidate();
    repaint();
  }

  /**
   * <p>Set the value of the {@link #selectionBackground} property.</p>
   *
   * <p>Fire a PropertyChangeEvent with name {@link
   * #SELECTION_BACKGROUND_CHANGED_PROPERTY} to registered listeners, if
   * selectionBackground changed.</p>
   *
   * @param s The new value of the selectionBackground property
   */
  public void setSelectionBackground(Color s)
  {
    Color tmp = selectionBackground;
    selectionBackground = s;
    if (((tmp == null && s != null)
         || (s == null && tmp != null)
         || (tmp != null && s != null && !tmp.equals(s))))
      firePropertyChange(SELECTION_BACKGROUND_CHANGED_PROPERTY, tmp, s);
    repaint();
  }

  /**
   * <p>Set the value of the {@link #selectionForeground} property.</p>
   *
   * <p>Fire a PropertyChangeEvent with name {@link
   * #SELECTION_FOREGROUND_CHANGED_PROPERTY} to registered listeners, if
   * selectionForeground changed.</p>
   *
   * @param s The new value of the selectionForeground property
   */
  public void setSelectionForeground(Color s)
  {
    Color tmp = selectionForeground;
    selectionForeground = s;
    if (((tmp == null && s != null)
         || (s == null && tmp != null)
         || (tmp != null && s != null && !tmp.equals(s))))
      firePropertyChange(SELECTION_FOREGROUND_CHANGED_PROPERTY, tmp, s);
    repaint();
  }

  /**
   * Set the value of the <code>showGrid</code> property.
   *
   * @param s The new value of the showGrid property
   */
  public void setShowGrid(boolean s)
  {
    setShowVerticalLines(s);
    setShowHorizontalLines(s);
  }

  /**
   * Set the value of the {@link #showHorizontalLines} property.
   *
   * @param s The new value of the showHorizontalLines property
   */
  public void setShowHorizontalLines(boolean s)
  {
    showHorizontalLines = s;
    repaint();
  }

  /**
   * Set the value of the {@link #showVerticalLines} property.
   *
   * @param s The new value of the showVerticalLines property
   */
  public void setShowVerticalLines(boolean s)
  {
    showVerticalLines = s;
    repaint();
  }

  /**
   * Set the value of the {@link #tableHeader} property.
   *
   * @param t The new value of the tableHeader property
   */
  public void setTableHeader(JTableHeader t)
  {
    if (tableHeader != null)
      tableHeader.setTable(null);
    tableHeader = t;
    if (tableHeader != null)
      tableHeader.setTable(this);
    revalidate();
    repaint();
  }

  protected void configureEnclosingScrollPane()
  {
    JScrollPane jsp = (JScrollPane) SwingUtilities.getAncestorOfClass(JScrollPane.class, this);
    if (jsp != null && tableHeader != null)
      {
        jsp.setColumnHeaderView(tableHeader);
      }
  }

  protected void unconfigureEnclosingScrollPane()
  {
    JScrollPane jsp = (JScrollPane) SwingUtilities.getAncestorOfClass(JScrollPane.class, this);
    if (jsp != null)
      {
        jsp.setColumnHeaderView(null);
      }
  }


  public void addNotify()
  {
    super.addNotify();
    configureEnclosingScrollPane();
  }

  public void removeNotify()
  {
    super.addNotify();
    unconfigureEnclosingScrollPane();
  }


  /**
   * This distributes the superfluous width in a table evenly on its columns.
   *
   * The implementation used here is different to that one described in
   * the JavaDocs. It is much simpler, and seems to work very well.
   *
   * TODO: correctly implement the algorithm described in the JavaDoc
   */
  private void distributeSpill(TableColumn[] cols, int spill)
  {
    int average = spill / cols.length;
    for (int i = 0; i < cols.length; i++)
      {
        if (cols[i] != null)
          cols[i].setWidth(cols[i].getPreferredWidth() + average);
      }
  }

  /**
   * This distributes the superfluous width in a table, setting the width of the
   * column being resized strictly to its preferred width.
   */
  private void distributeSpillResizing(TableColumn[] cols, int spill,
                                       TableColumn resizeIt)
  {
    int average = 0;
    if (cols.length != 1)
      average = spill / (cols.length-1);
    for (int i = 0; i < cols.length; i++)
      {
        if (cols[i] != null && !cols[i].equals(resizeIt))
          cols[i].setWidth(cols[i].getPreferredWidth() + average);
      }
    resizeIt.setWidth(resizeIt.getPreferredWidth());
  }

  /**
   * Set the widths of all columns, taking they preferred widths into
   * consideration. The excess space, if any, will be distrubuted between
   * all columns. This method also handles special cases when one of the
   * collumns is currently being resized.
   *
   * @see TableColumn#setPreferredWidth(int)
   */
  public void doLayout()
  {
    TableColumn resizingColumn = null;

    int ncols = columnModel.getColumnCount();
    if (ncols < 1)
      return;

    int prefSum = 0;
    int rCol = -1;

    if (tableHeader != null)
      resizingColumn = tableHeader.getResizingColumn();

    for (int i = 0; i < ncols; ++i)
      {
        TableColumn col = columnModel.getColumn(i);
        int p = col.getPreferredWidth();
        prefSum += p;
        if (resizingColumn == col)
          rCol = i;
      }

    int spill = getWidth() - prefSum;

    if (resizingColumn != null)
      {
        TableColumn col;
        TableColumn [] cols;

        switch (getAutoResizeMode())
          {
          case AUTO_RESIZE_LAST_COLUMN:
            col = columnModel.getColumn(ncols-1);
            col.setWidth(col.getPreferredWidth() + spill);
            break;

          case AUTO_RESIZE_NEXT_COLUMN:
            col = columnModel.getColumn(ncols-1);
            col.setWidth(col.getPreferredWidth() + spill);
            break;

          case AUTO_RESIZE_ALL_COLUMNS:
            cols = new TableColumn[ncols];
            for (int i = 0; i < ncols; ++i)
              cols[i] = columnModel.getColumn(i);
            distributeSpillResizing(cols, spill, resizingColumn);
            break;

          case AUTO_RESIZE_SUBSEQUENT_COLUMNS:

            // Subtract the width of the non-resized columns from the spill.
            int w = 0;
            int wp = 0;
            TableColumn column;
            for (int i = 0; i < rCol; i++)
              {
                column = columnModel.getColumn(i);
                w += column.getWidth();
                wp+= column.getPreferredWidth();
              }

            // The number of columns right from the column being resized.
            int n = ncols-rCol-1;
            if (n>0)
              {
                // If there are any columns on the right sied to resize.
                spill = (getWidth()-w) - (prefSum-wp);
                int average = spill / n;

                 // For all columns right from the column being resized:
                for (int i = rCol+1; i < ncols; i++)
                  {
                    column = columnModel.getColumn(i);
                    column.setWidth(column.getPreferredWidth() + average);
                  }
              }
            resizingColumn.setWidth(resizingColumn.getPreferredWidth());
            break;

          case AUTO_RESIZE_OFF:
          default:
            int prefWidth = resizingColumn.getPreferredWidth();
            resizingColumn.setWidth(prefWidth);
          }
      }
    else
      {
        TableColumn[] cols = new TableColumn[ncols];

        for (int i = 0; i < ncols; ++i)
          cols[i] = columnModel.getColumn(i);

        distributeSpill(cols, spill);
      }

    if (editorComp!=null)
      moveToCellBeingEdited(editorComp);

    int leftBoundary = getLeftResizingBoundary();
    int width = getWidth() - leftBoundary;
    repaint(leftBoundary, 0, width, getHeight());
    if (tableHeader != null)
      tableHeader.repaint(leftBoundary, 0, width, tableHeader.getHeight());
  }

  /**
   * Get the left boundary of the rectangle which changes during the column
   * resizing.
   */
  int getLeftResizingBoundary()
  {
    if (tableHeader == null || getAutoResizeMode() == AUTO_RESIZE_ALL_COLUMNS)
      return 0;
    else
      {
        TableColumn resizingColumn = tableHeader.getResizingColumn();
        if (resizingColumn == null)
          return 0;

        int rc = convertColumnIndexToView(resizingColumn.getModelIndex());
        int p = 0;

        for (int i = 0; i < rc; i++)
          p += columnModel.getColumn(i).getWidth();

        return p;
      }
  }


  /**
   * @deprecated Replaced by <code>doLayout()</code>
   */
  public void sizeColumnsToFit(boolean lastColumnOnly)
  {
    doLayout();
  }

  /**
   * Obsolete since JDK 1.4. Please use <code>doLayout()</code>.
   */
  public void sizeColumnsToFit(int resizingColumn)
  {
    doLayout();
  }

  public String getUIClassID()
  {
    return "TableUI";
  }

  /**
   * This method returns the table's UI delegate.
   *
   * @return The table's UI delegate.
   */
  public TableUI getUI()
  {
    return (TableUI) ui;
  }

  /**
   * This method sets the table's UI delegate.
   *
   * @param ui The table's UI delegate.
   */
  public void setUI(TableUI ui)
  {
    super.setUI(ui);
    // The editors and renderers must be recreated because they constructors
    // may use the look and feel properties.
    createDefaultEditors();
    createDefaultRenderers();
  }

  public void updateUI()
  {
    setUI((TableUI) UIManager.getUI(this));
  }

  /**
   * Get the class (datatype) of the column. The cells are rendered and edited
   * differently, depending from they data type.
   *
   * @param column the column (not the model index).
   *
   * @return the class, defining data type of that column (String.class for
   * String, Boolean.class for boolean and so on).
   */
  public Class<?> getColumnClass(int column)
  {
    return getModel().getColumnClass(convertColumnIndexToModel(column));
  }

  /**
   * Get the name of the column. If the column has the column identifier set,
   * the return value is the result of the .toString() method call on that
   * identifier. If the identifier is not explicitly set, the returned value
   * is calculated by
   * {@link javax.swing.table.AbstractTableModel#getColumnName(int)}.
   *
   * @param column the column
   *
   * @return the name of that column.
   */
  public String getColumnName(int column)
  {
    int modelColumn = columnModel.getColumn(column).getModelIndex();
    return dataModel.getColumnName(modelColumn);
  }

  /**
   * Get the column, currently being edited
   *
   * @return the column, currently being edited.
   */
  public int getEditingColumn()
  {
    return editingColumn;
  }

  /**
   * Set the column, currently being edited
   *
   * @param column the column, currently being edited.
   */
  public void setEditingColumn(int column)
  {
    editingColumn = column;
  }

  /**
   * Get the row currently being edited.
   *
   * @return the row, currently being edited.
   */
  public int getEditingRow()
  {
    return editingRow;
  }

  /**
   * Set the row currently being edited.
   *
   * @param row the row, that will be edited
   */
  public void setEditingRow(int row)
  {
    editingRow = row;
  }

  /**
   * Get the editor component that is currently editing one of the cells
   *
   * @return the editor component or null, if none of the cells is being
   * edited.
   */
  public Component getEditorComponent()
  {
    return editorComp;
  }

  /**
   * Check if one of the table cells is currently being edited.
   *
   * @return true if there is a cell being edited.
   */
  public boolean isEditing()
  {
    return editorComp != null;
  }

  /**
   * Set the default editor for the given column class (column data type).
   * By default, String is handled by text field and Boolean is handled by
   * the check box.
   *
   * @param columnClass the column data type
   * @param editor the editor that will edit this data type
   *
   * @see TableModel#getColumnClass(int)
   */
  public void setDefaultEditor(Class<?> columnClass, TableCellEditor editor)
  {
    if (editor != null)
      defaultEditorsByColumnClass.put(columnClass, editor);
    else
      defaultEditorsByColumnClass.remove(columnClass);
  }

  public void addColumnSelectionInterval(int index0, int index1)
  {
    if ((index0 < 0 || index0 > (getColumnCount()-1)
         || index1 < 0 || index1 > (getColumnCount()-1)))
      throw new IllegalArgumentException("Column index out of range.");

    getColumnModel().getSelectionModel().addSelectionInterval(index0, index1);
  }

  public void addRowSelectionInterval(int index0, int index1)
  {
    if ((index0 < 0 || index0 > (getRowCount()-1)
         || index1 < 0 || index1 > (getRowCount()-1)))
      throw new IllegalArgumentException("Row index out of range.");

    getSelectionModel().addSelectionInterval(index0, index1);
  }

  public void setColumnSelectionInterval(int index0, int index1)
  {
    if ((index0 < 0 || index0 > (getColumnCount()-1)
         || index1 < 0 || index1 > (getColumnCount()-1)))
      throw new IllegalArgumentException("Column index out of range.");

    getColumnModel().getSelectionModel().setSelectionInterval(index0, index1);
  }

  public void setRowSelectionInterval(int index0, int index1)
  {
    if ((index0 < 0 || index0 > (getRowCount()-1)
         || index1 < 0 || index1 > (getRowCount()-1)))
      throw new IllegalArgumentException("Row index out of range.");

    getSelectionModel().setSelectionInterval(index0, index1);
  }

  public void removeColumnSelectionInterval(int index0, int index1)
  {
    if ((index0 < 0 || index0 > (getColumnCount()-1)
         || index1 < 0 || index1 > (getColumnCount()-1)))
      throw new IllegalArgumentException("Column index out of range.");

    getColumnModel().getSelectionModel().removeSelectionInterval(index0, index1);
  }

  public void removeRowSelectionInterval(int index0, int index1)
  {
    if ((index0 < 0 || index0 > (getRowCount()-1)
         || index1 < 0 || index1 > (getRowCount()-1)))
      throw new IllegalArgumentException("Row index out of range.");

    getSelectionModel().removeSelectionInterval(index0, index1);
  }

  /**
   * Checks if the given column is selected.
   *
   * @param column the column
   *
   * @return true if the column is selected (as reported by the selection
   * model, associated with the column model), false otherwise.
   */
  public boolean isColumnSelected(int column)
  {
    return getColumnModel().getSelectionModel().isSelectedIndex(column);
  }

  /**
   * Checks if the given row is selected.
   *
   * @param row the row
   *
   * @return true if the row is selected (as reported by the selection model),
   * false otherwise.
   */
  public boolean isRowSelected(int row)
  {
    return getSelectionModel().isSelectedIndex(row);
  }

  /**
   * Checks if the given cell is selected. The cell is selected if both
   * the cell row and the cell column are selected.
   *
   * @param row the cell row
   * @param column the cell column
   *
   * @return true if the cell is selected, false otherwise
   */
  public boolean isCellSelected(int row, int column)
  {
    return isRowSelected(row) && isColumnSelected(column);
  }

  /**
   * Select all table.
   */
  public void selectAll()
  {
    // The table is empty - nothing to do!
    if (getRowCount() == 0 || getColumnCount() == 0)
      return;

    // rowLead and colLead store the current lead selection indices
    int rowLead = selectionModel.getLeadSelectionIndex();
    int colLead = getColumnModel().getSelectionModel().getLeadSelectionIndex();
    // the following calls to setSelectionInterval change the lead selection
    // indices
    setColumnSelectionInterval(0, getColumnCount() - 1);
    setRowSelectionInterval(0, getRowCount() - 1);
    // the following addSelectionInterval calls restore the lead selection
    // indices to their previous values
    addColumnSelectionInterval(colLead,colLead);
    addRowSelectionInterval(rowLead, rowLead);
  }

  /**
   * Get the cell value at the given position.
   *
   * @param row the row to get the value
   * @param column the actual column number (not the model index)
   * to get the value.
   *
   * @return the cell value, as returned by model.
   */
  public Object getValueAt(int row, int column)
  {
    return dataModel.getValueAt(row, convertColumnIndexToModel(column));
  }

  /**
   * Set value for the cell at the given position. The modified cell is
   * repainted.
   *
   * @param value the value to set
   * @param row the row of the cell being modified
   * @param column the column of the cell being modified
   */
  public void setValueAt(Object value, int row, int column)
  {
    dataModel.setValueAt(value, row, convertColumnIndexToModel(column));

    repaint(getCellRect(row, column, true));
  }

  /**
   * Get table column with the given identified.
   *
   * @param identifier the column identifier
   *
   * @return the table column with this identifier
   *
   * @throws IllegalArgumentException if <code>identifier</code> is
   *         <code>null</code> or there is no column with that identifier.
   *
   * @see TableColumn#setIdentifier(Object)
   */
  public TableColumn getColumn(Object identifier)
  {
    return columnModel.getColumn(columnModel.getColumnIndex(identifier));
  }

  /**
   * Returns <code>true</code> if the specified cell is editable, and
   * <code>false</code> otherwise.
   *
   * @param row  the row index.
   * @param column  the column index.
   *
   * @return true if the cell is editable, false otherwise.
   */
  public boolean isCellEditable(int row, int column)
  {
    return dataModel.isCellEditable(row, convertColumnIndexToModel(column));
  }

  /**
   * Clears any existing columns from the <code>JTable</code>'s
   * {@link TableColumnModel} and creates new columns to match the values in
   * the data ({@link TableModel}) used by the table.
   *
   * @see #setAutoCreateColumnsFromModel(boolean)
   */
  public void createDefaultColumnsFromModel()
  {
    assert columnModel != null : "The columnModel must not be null.";

    // remove existing columns
    int columnIndex = columnModel.getColumnCount() - 1;
    while (columnIndex >= 0)
    {
      columnModel.removeColumn(columnModel.getColumn(columnIndex));
      columnIndex--;
    }

    // add new columns to match the TableModel
    int columnCount = dataModel.getColumnCount();
    for (int c = 0; c < columnCount; c++)
    {
      TableColumn column = new TableColumn(c);
      column.setIdentifier(dataModel.getColumnName(c));
      column.setHeaderValue(dataModel.getColumnName(c));
      columnModel.addColumn(column);
      column.addPropertyChangeListener(tableColumnPropertyChangeHandler);
    }
  }

  public void changeSelection (int rowIndex, int columnIndex, boolean toggle, boolean extend)
  {
    if (toggle && extend)
      {
        // Leave the selection state as is, but move the anchor
        //   index to the specified location
        selectionModel.setAnchorSelectionIndex(rowIndex);
        getColumnModel().getSelectionModel().setAnchorSelectionIndex(columnIndex);
      }
    else if (toggle)
      {
        // Toggle the state of the specified cell
        if (isCellSelected(rowIndex,columnIndex))
          {
            selectionModel.removeSelectionInterval(rowIndex,rowIndex);
            getColumnModel().getSelectionModel().removeSelectionInterval(columnIndex,columnIndex);
          }
        else
          {
            selectionModel.addSelectionInterval(rowIndex,rowIndex);
            getColumnModel().getSelectionModel().addSelectionInterval(columnIndex,columnIndex);
          }
      }
    else if (extend)
      {
        // Extend the previous selection from the anchor to the
        // specified cell, clearing all other selections
        selectionModel.setLeadSelectionIndex(rowIndex);
        getColumnModel().getSelectionModel().setLeadSelectionIndex(columnIndex);
      }
    else
      {
        // Clear the previous selection and ensure the new cell
        // is selected
         selectionModel.clearSelection();
        selectionModel.setSelectionInterval(rowIndex,rowIndex);
        getColumnModel().getSelectionModel().clearSelection();
        getColumnModel().getSelectionModel().setSelectionInterval(columnIndex, columnIndex);


      }
  }

  /**
   * Programmatically starts editing the specified cell.
   *
   * @param row the row of the cell to edit.
   * @param column the column of the cell to edit.
   */
  public boolean editCellAt(int row, int column)
  {
    // Complete the previous editing session, if still active.
    if (isEditing())
      editingStopped(new ChangeEvent("editingStopped"));

    TableCellEditor editor = getCellEditor(row, column);

    // The boolean values are inverted by the single click without the
    // real editing session.
    if (editor == booleanInvertingEditor && isCellEditable(row, column))
      {
        if (Boolean.TRUE.equals(getValueAt(row, column)))
          setValueAt(Boolean.FALSE, row, column);
        else
          setValueAt(Boolean.TRUE, row, column);
        return false;
      }
    else
      {
        editingRow = row;
        editingColumn = column;

        setCellEditor(editor);
        editorComp = prepareEditor(cellEditor, row, column);

        // Remove the previous editor components, if present. Only one
        // editor component at time is allowed in the table.
        removeAll();
        add(editorComp);
        moveToCellBeingEdited(editorComp);
        scrollRectToVisible(editorComp.getBounds());
        editorComp.requestFocusInWindow();

        // Deliver the should select event.
        return editor.shouldSelectCell(null);
      }
  }

  /**
   * Move the given component under the cell being edited.
   * The table must be in the editing mode.
   *
   * @param component the component to move.
   */
  private void moveToCellBeingEdited(Component component)
  {
     Rectangle r = getCellRect(editingRow, editingColumn, true);
     // Adjust bounding box of the editing component, so that it lies
     // 'above' the grid on all edges, not only right and bottom.
     // The table grid is painted only at the right and bottom edge of a cell.
     r.x -= 1;
     r.y -= 1;
     r.width += 1;
     r.height += 1;
     component.setBounds(r);
  }

  /**
   * Programmatically starts editing the specified cell.
   *
   * @param row the row of the cell to edit.
   * @param column the column of the cell to edit.
   */
  public boolean editCellAt (int row, int column, EventObject e)
  {
    return editCellAt(row, column);
  }

  /**
   * Discards the editor object.
   */
  public void removeEditor()
  {
    editingStopped(new ChangeEvent(this));
  }

  /**
   * Prepares the editor by querying for the value and selection state of the
   * cell at (row, column).
   *
   * @param editor the TableCellEditor to set up
   * @param row the row of the cell to edit
   * @param column the column of the cell to edit
   * @return the Component being edited
   */
  public Component prepareEditor (TableCellEditor editor, int row, int column)
  {
    return editor.getTableCellEditorComponent
      (this, getValueAt(row, column), isCellSelected(row, column), row, column);
  }

  /**
   * This revalidates the <code>JTable</code> and queues a repaint.
   */
  protected void resizeAndRepaint()
  {
    revalidate();
    repaint();
  }

  /**
   * Sets whether cell editors of this table should receive keyboard focus
   * when the editor is activated by a keystroke. The default setting is
   * <code>false</code> which means that the table should keep the keyboard
   * focus until the cell is selected by a mouse click.
   *
   * @param value the value to set
   *
   * @since 1.4
   */
  public void setSurrendersFocusOnKeystroke(boolean value)
  {
    // TODO: Implement functionality of this property (in UI impl).
    surrendersFocusOnKeystroke = value;
  }

  /**
   * Returns whether cell editors of this table should receive keyboard focus
   * when the editor is activated by a keystroke. The default setting is
   * <code>false</code> which means that the table should keep the keyboard
   * focus until the cell is selected by a mouse click.
   *
   * @return whether cell editors of this table should receive keyboard focus
   *         when the editor is activated by a keystroke
   *
   * @since 1.4
   */
  public boolean getSurrendersFocusOnKeystroke()
  {
    // TODO: Implement functionality of this property (in UI impl).
    return surrendersFocusOnKeystroke;
  }

  /**
   * Helper method for
   * {@link LookAndFeel#installProperty(JComponent, String, Object)}.
   *
   * @param propertyName the name of the property
   * @param value the value of the property
   *
   * @throws IllegalArgumentException if the specified property cannot be set
   *         by this method
   * @throws ClassCastException if the property value does not match the
   *         property type
   * @throws NullPointerException if <code>c</code> or
   *         <code>propertyValue</code> is <code>null</code>
   */
  void setUIProperty(String propertyName, Object value)
  {
    if (propertyName.equals("rowHeight"))
      {
        if (! clientRowHeightSet)
          {
            setRowHeight(((Integer) value).intValue());
            clientRowHeightSet = false;
          }
      }
    else
      {
        super.setUIProperty(propertyName, value);
      }
  }
}
