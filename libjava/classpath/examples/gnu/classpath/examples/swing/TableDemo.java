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
import java.awt.Component;
import java.awt.Dimension;
import java.awt.Rectangle;
import java.text.DateFormat;
import java.util.Date;

import javax.swing.AbstractCellEditor;
import javax.swing.BorderFactory;
import javax.swing.DefaultCellEditor;
import javax.swing.DefaultListCellRenderer;
import javax.swing.Icon;
import javax.swing.JComboBox;
import javax.swing.JComponent;
import javax.swing.JFrame;
import javax.swing.JList;
import javax.swing.JPanel;
import javax.swing.JScrollBar;
import javax.swing.JScrollPane;
import javax.swing.JSlider;
import javax.swing.JTable;
import javax.swing.JTextField;
import javax.swing.SwingUtilities;
import javax.swing.border.Border;
import javax.swing.plaf.metal.MetalIconFactory;
import javax.swing.table.DefaultTableColumnModel;
import javax.swing.table.DefaultTableModel;
import javax.swing.table.TableCellEditor;
import javax.swing.table.TableCellRenderer;
import javax.swing.table.TableColumn;
import javax.swing.table.TableColumnModel;

/**
 * Displays the editable table. The first column consists of check boxes.
 * 
 * @author Audrius Meskauskas (audriusa@bioinformatics.org)
 */
public class TableDemo extends JPanel
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
     * All cells are editable in our table.
     */
    public boolean isCellEditable(int row, int column)
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
     * The first column contains booleans, the second - icons, 
     * others - default class.
     */
    public Class getColumnClass(int column)
    {
      if (column == 0)
        return Boolean.class;
      else if (column == 1)
        return Icon.class;
      else
        return super.getColumnClass(column);
    }    
  }

  /**
   * The scroll bar renderer.
   */
  class SliderCell
      extends AbstractCellEditor
      implements TableCellEditor, TableCellRenderer
  {
    /**
     * The editor bar.
     */
    JSlider bar;
    
    /**
     * The renderer bar.
     */
    JSlider rendererBar;
    
    /**
     * The border around the bar, if required.
     */
    Border border = BorderFactory.createLineBorder(table.getGridColor());

    SliderCell()
    {
      bar = new JSlider();
      bar.setOrientation(JScrollBar.HORIZONTAL);
      bar.setMinimum(0);
      bar.setMaximum(rows);      
      bar.setBorder(border);
      
      rendererBar = new JSlider();
      rendererBar.setMinimum(0);
      rendererBar.setMaximum(rows);
      rendererBar.setEnabled(false);
    }

    /**
     * Get the editor.
     */
    public Component getTableCellEditorComponent(JTable table, Object value,
                                                 boolean isSelected, int row,
                                                 int column)
    {
      if (value instanceof Integer)
        bar.setValue(((Integer) value).intValue());
      return bar;
    }
    
    /**
     * Get the renderer.
     */
    public Component getTableCellRendererComponent(JTable table, Object value,
                                                   boolean isSelected,
                                                   boolean hasFocus, int row,
                                                   int column)
    {
      rendererBar.setValue(((Integer) value).intValue());
      if (hasFocus)
        rendererBar.setBorder(border);
      else
        rendererBar.setBorder(null);
      return rendererBar;
    }

    public Object getCellEditorValue()
    {
      return new Integer(bar.getValue());
    }

  }  
  
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
   * The icons that appear in the icon column.
   */ 
  Icon[] icons = new Icon[]
    {                            
      MetalIconFactory.getTreeComputerIcon(),
      MetalIconFactory.getTreeHardDriveIcon(),
      MetalIconFactory.getTreeFolderIcon(),
    };
    
  /**
   * The choices in the combo boxes
   */
  String [] sides = new String[]
    {
      "north", "south", "east", "west"                           
    };
  
  
  /**
   * Create the table demo with the given titel.
   */
  public TableDemo()
  {
    super();
    createContent();
  }
  
  /**
   * Returns a panel with the demo content. The panel uses a BorderLayout(), and
   * the BorderLayout.SOUTH area is empty, to allow callers to add controls to
   * the bottom of the panel if they want to (a close button is added if this
   * demo is being run as a standalone demo).
   */
  private void createContent()
  {
    setLayout(new BorderLayout());
    values = new Object[rows][];
    
    for (int i = 0; i < values.length; i++)
      {
        values[i] = new Object[cols];
        for (int j = 3; j < cols; j++)
          {
            values[i][j] = "" + ((char) ('a' + j)) + i;
          }
        values [i][0] = i % 2 == 0? Boolean.TRUE : Boolean.FALSE;
        values [i][1] = icons [ i % icons.length ];
        values [i][2] = sides [ i % sides.length ];
        values [i][4] = new Integer(i);
      }
        
    table.setModel(model);        
        
    // Make the columns with gradually increasing width:
    DefaultTableColumnModel cm = new DefaultTableColumnModel();
    table.setColumnModel(cm);
    
    for (int i = 0; i < cols; i++)
      {
        TableColumn column = new TableColumn(i);
            
        // Showing the variable width columns.
        int width = 100+10*i;
        column.setPreferredWidth(width);
            
        // If we do not set the header value here, the value, returned
        // by model, is used.
        column.setHeaderValue("Width +"+(20*i));
            
        cm.addColumn(column);            
      }
    
    setCustomEditors();
    setInformativeHeaders();

    // Create the table, place it into scroll pane and place
    // the pane into this frame.
    JScrollPane scroll = new JScrollPane();
        
    // The horizontal scroll bar is never needed.
    scroll.setHorizontalScrollBarPolicy(JScrollPane.HORIZONTAL_SCROLLBAR_NEVER);
    scroll.getViewport().add(table);
    add(scroll, BorderLayout.CENTER);
    
    // Increase the row height to make the icons and sliders look better.
    table.setRowHeight(table.getRowHeight()+2);
  }
  
  /**
   * Set the more informative column headers for specific columns.
   */
  void setInformativeHeaders()
  {
    TableColumnModel cm = table.getColumnModel();

    cm.getColumn(0).setHeaderValue("check");
    cm.getColumn(1).setHeaderValue("icon");
    cm.getColumn(2).setHeaderValue("combo");
    cm.getColumn(3).setHeaderValue("edit combo");
    cm.getColumn(4).setHeaderValue("slider");
  }
  
  /**
   * Set the custom editors for combo boxes. This method also sets one
   * custom renderer.
   */
  void setCustomEditors()
  {
    TableColumnModel cm = table.getColumnModel();
    
    // Set combo-box based editor for icons (note that no custom
    // renderer is needed for JComboBox to work with icons.
    JComboBox combo0 = new JComboBox(icons);
    cm.getColumn(1).setCellEditor(new DefaultCellEditor(combo0));
    
    // Set the simple combo box editor for the third column:
    JComboBox combo1 = new JComboBox(sides);
    cm.getColumn(2).setCellEditor(new DefaultCellEditor(combo1));
    
    // Set the editable combo box for the forth column:
    JComboBox combo2 = new JComboBox(sides);
    combo2.setEditable(true);
    cm.getColumn(3).setCellEditor(new DefaultCellEditor(combo2));
    
    SliderCell scrollView = new SliderCell();
    cm.getColumn(4).setCellEditor(scrollView);
    cm.getColumn(4).setCellRenderer(scrollView);    

    table.setColumnModel(cm);
  }
  
  /**
   * The executable method to display the editable table.
   * 
   * @param args
   *          unused.
   */
  public static void main(String[] args)
  {
    SwingUtilities.invokeLater
    (new Runnable()
     {
       public void run()
       {
         TableDemo demo = new TableDemo();
         JFrame frame = new JFrame();
         frame.getContentPane().add(demo);
         frame.setSize(new Dimension(640, 100));
         frame.setVisible(true);
       }
     });
  }

  /**
   * Returns a DemoFactory that creates a TableDemo.
   *
   * @return a DemoFactory that creates a TableDemo
   */
  public static DemoFactory createDemoFactory()
  {
    return new DemoFactory()
    {
      public JComponent createDemo()
      {
        return new TableDemo();
      }
    };
  }
}

