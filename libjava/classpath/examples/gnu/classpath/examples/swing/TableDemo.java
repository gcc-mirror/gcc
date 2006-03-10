/* TableDemo.java -- Demonstrates the use of JTable.
   Copyright (C) 2006 Free Software Foundation, Inc.

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


package gnu.classpath.examples.swing;

import java.awt.BorderLayout;
import java.awt.Dimension;

import javax.swing.JFrame;
import javax.swing.JPanel;
import javax.swing.JScrollPane;
import javax.swing.JTable;
import javax.swing.table.DefaultTableColumnModel;
import javax.swing.table.DefaultTableModel;
import javax.swing.table.TableColumn;

/**
 * Displays the editable table. The first column consists of check boxes.
 * 
 * @author Audrius Meskauskas (audriusa@bioinformatics.org)
 */
public class TableDemo extends JFrame
{ 
  /**
   * The initial row count for this table.
   */ 
  static int rows = 32;
  
  /**
   * The initial column count for this table.
   */ 
  static int cols = 7;
  
  
  /**
   * The table model.
   */
  class TModel extends DefaultTableModel
  {
    
    /**
     * Return true if the cell is editable. All cells are editable.
     */
    public boolean isCellEditable(int parm1, int parm2)
    {
      return true;
    }
    
    /**
     * Get the number of the table rows.
     */
    public int getRowCount()
    {
      return rows;
    }

    /**
     * Get the number of the table columns.
     */
    public int getColumnCount()
    {
      return cols;
    }
    
    /**
     * Set the value at the given position
     */
    public void setValueAt(Object aValue, int aRow, int aColumn)
    {
      values[aRow][aColumn] = aValue;
    }
    
    /**
     * Get the value at the given position.
     */
    public Object getValueAt(int aRow, int aColumn)
    {
      return values[aRow][aColumn];
    }
    
    /**
     * The column name, as suggested by model. This header should not be
     * visible, as it is overridden by setting the header name with
     * {@link TableColumn#setHeaderValue} in {@link TableDemo#createContent}.
     */
    public String getColumnName(int column)
    {
       return "Error "+column;
    }
    
    /**
     * The first column contains booleans, others - default class.
     */
    public Class getColumnClass(int column)
    {
      if (column == 0)
        return Boolean.class;
      else
        return super.getColumnClass(column);
    }    
  }

  private JPanel content;

  /**
   * The table being displayed.
   */
  JTable table = new JTable();
 
  /**
   * The table model.
   */
  TModel model = new TModel();

  /**
   * The table value array.
   */
  Object[][] values;
  
  /**
   * Create the table demo with the given titel.
   * 
   * @param title the frame title.
   */
  public TableDemo(String title)
  {
    super(title);
    getContentPane().add(createContent(), BorderLayout.CENTER);
  }
  
  /**
   * Returns a panel with the demo content. The panel uses a BorderLayout(), and
   * the BorderLayout.SOUTH area is empty, to allow callers to add controls to
   * the bottom of the panel if they want to (a close button is added if this
   * demo is being run as a standalone demo).
   */
  JPanel createContent()
  {
    if (content == null)
      {
        JPanel p = new JPanel();
        p.setLayout(new BorderLayout());
        values = new Object[rows][];
        for (int i = 0; i < values.length; i++)
          {
            values[i] = new Object[cols];
            for (int j = 1; j < cols; j++)
              {
                values[i][j] = "" + ((char) ('a' + j)) + i;
              }
            values [i][0] = i % 2 == 0? Boolean.TRUE : Boolean.FALSE;
          }
        
        table.setModel(model);        
        
        // Make the columns with gradually increasing width:
        DefaultTableColumnModel cm = new DefaultTableColumnModel();
        for (int i = 0; i < cols; i++)
          {
            TableColumn column = new TableColumn(i);
            
            // Showing the variable width columns.
            int width = 100+20*i;
            column.setPreferredWidth(width);
            
            // If we do not set the header value here, the value, returned
            // by model, is used.
            column.setHeaderValue("Width +"+(20*i));
            
            cm.addColumn(column);            
          }

        table.setColumnModel(cm);

        // Create the table, place it into scroll pane and place
        // the pane into this frame.
        JScrollPane scroll = new JScrollPane();
        
        // The horizontal scroll bar is never needed.
        scroll.setHorizontalScrollBarPolicy(JScrollPane.HORIZONTAL_SCROLLBAR_NEVER);
        scroll.getViewport().add(table);
        p.add(scroll, BorderLayout.CENTER);
        content = p;
      }
    return content;
  }
  
  /**
   * The executable method to display the editable table.
   * 
   * @param args
   *          unused.
   */
  public static void main(String[] args)
  {
    TableDemo frame = new TableDemo("Table double click on the cell to edit.");
    frame.setSize(new Dimension(640, 100));
    frame.validate();
    frame.setVisible(true);
  }
}
