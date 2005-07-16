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
import java.awt.Dimension;
import java.awt.Graphics;
import java.awt.Point;
import java.awt.Rectangle;
import java.awt.event.FocusEvent;
import java.awt.event.FocusListener;
import java.awt.event.KeyEvent;
import java.awt.event.KeyListener;
import java.awt.event.MouseEvent;

import javax.swing.BorderFactory;
import javax.swing.CellRendererPane;
import javax.swing.JComponent;
import javax.swing.JTable;
import javax.swing.ListSelectionModel;
import javax.swing.UIDefaults;
import javax.swing.UIManager;
import javax.swing.border.Border;
import javax.swing.event.MouseInputListener;
import javax.swing.plaf.ComponentUI;
import javax.swing.plaf.TableUI;
import javax.swing.table.TableCellRenderer;
import javax.swing.table.TableColumn;
import javax.swing.table.TableColumnModel;

public class BasicTableUI
  extends TableUI
{
  public static ComponentUI createUI(JComponent comp) 
  {
    return new BasicTableUI();
  }

  protected FocusListener focusListener;  
  protected KeyListener keyListener;   
  protected MouseInputListener	mouseInputListener;   
  protected CellRendererPane rendererPane;   
  protected JTable table;

  /** The normal cell border. */
  Border cellBorder;

  /** The cell border for selected/highlighted cells. */
  Border highlightCellBorder;

  class FocusHandler implements FocusListener
  {
    public void focusGained(FocusEvent e) 
    {
    }
    public void focusLost(FocusEvent e) 
    {
    }
  }

  class KeyHandler implements KeyListener
  {
    public void keyPressed(KeyEvent e) 
    {
    }
    public void keyReleased(KeyEvent e) 
    {
    }
    public void keyTyped(KeyEvent e) 
    {
    }
  }

  class MouseInputHandler implements MouseInputListener
  {
    Point begin, curr;

    private void updateSelection(boolean controlPressed)
    {
      if (table.getRowSelectionAllowed())
        {
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
        }

      if (table.getColumnSelectionAllowed())
        {
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
    }

    public void mouseClicked(MouseEvent e) 
    {
    }
    public void mouseDragged(MouseEvent e) 
    {
      curr = new Point(e.getX(), e.getY());
      updateSelection(e.isControlDown());      
    }
    public void mouseEntered(MouseEvent e) 
    {
    }
    public void mouseExited(MouseEvent e) 
    {
    }
    public void mouseMoved(MouseEvent e) 
    {
    }
    public void mousePressed(MouseEvent e) 
    {
      begin = new Point(e.getX(), e.getY());
      curr = new Point(e.getX(), e.getY());
      //if control is pressed and the cell is already selected, deselect it
      if (e.isControlDown() && table.
          isCellSelected(table.rowAtPoint(begin),table.columnAtPoint(begin)))
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
      
    }
    public void mouseReleased(MouseEvent e) 
    {
      begin = null;
      curr = null;
    }
  }

  protected FocusListener createFocusListener() 
  {
    return new FocusHandler();
  }
  protected KeyListener createKeyListener() 
  {
    return new KeyHandler();
  }
  protected MouseInputListener createMouseInputListener() 
  {
    return new MouseInputHandler();
  }

  public Dimension getMaximumSize(JComponent comp) 
  {
    return getPreferredSize(comp);
  }

  public Dimension getMinimumSize(JComponent comp) 
  {
    return getPreferredSize(comp);
  }

  public Dimension getPreferredSize(JComponent comp) 
  {
    int width = table.getColumnModel().getTotalColumnWidth();
    int height = table.getRowCount() * table.getRowHeight();
    return new Dimension(width, height);
  }

  protected void installDefaults() 
  {
    UIDefaults defaults = UIManager.getLookAndFeelDefaults();
    table.setFont(defaults.getFont("Table.font"));
    table.setGridColor(defaults.getColor("Table.gridColor"));
    table.setForeground(defaults.getColor("Table.foreground"));
    table.setBackground(defaults.getColor("Table.background"));
    table.setSelectionForeground(defaults.getColor("Table.selectionForeground"));
    table.setSelectionBackground(defaults.getColor("Table.selectionBackground"));
    table.setOpaque(true);

    highlightCellBorder = defaults.getBorder("Table.focusCellHighlightBorder");
    cellBorder = BorderFactory.createEmptyBorder(1, 1, 1, 1);
  }
  protected void installKeyboardActions() 
  {
  }

  protected void installListeners() 
  {
    table.addFocusListener(focusListener);  
    table.addKeyListener(keyListener);
    table.addMouseListener(mouseInputListener);    
    table.addMouseMotionListener(mouseInputListener);
  }

  protected void uninstallDefaults() 
  {
    // TODO: this method used to do the following which is not
    // quite right (at least it breaks apps that run fine with the
    // JDK):
    //
    // table.setFont(null);
    // table.setGridColor(null);
    // table.setForeground(null);
    // table.setBackground(null);
    // table.setSelectionForeground(null);
    // table.setSelectionBackground(null);
    //
    // This would leave the component in a corrupt state, which is
    // not acceptable. A possible solution would be to have component
    // level defaults installed, that get overridden by the UI defaults
    // and get restored in this method. I am not quite sure about this
    // though. / Roman Kennke
  }

  protected void uninstallKeyboardActions() 
  {
  }

  protected void uninstallListeners() 
  {
    table.removeFocusListener(focusListener);  
    table.removeKeyListener(keyListener);
    table.removeMouseListener(mouseInputListener);    
    table.removeMouseMotionListener(mouseInputListener);
  }

  public void installUI(JComponent comp) 
  {
    table = (JTable)comp;
    focusListener = createFocusListener();  
    keyListener = createKeyListener();
    mouseInputListener = createMouseInputListener();
    installDefaults();
    installKeyboardActions();
    installListeners();
  }

  public void uninstallUI(JComponent c) 
  {
    uninstallListeners();
    uninstallKeyboardActions();
    uninstallDefaults();    
  }

  public void paint(Graphics gfx, JComponent ignored) 
  {
    int ncols = table.getColumnCount();
    int nrows = table.getRowCount();
    if (nrows == 0 || ncols == 0)
      return;

    Rectangle clip = gfx.getClipBounds();
    TableColumnModel cols = table.getColumnModel();

    int height = table.getRowHeight();
    int x0 = 0, y0 = 0;
    int x = x0;
    int y = y0;

    Dimension gap = table.getIntercellSpacing();
    int ymax = clip.y + clip.height;
    int xmax = clip.x + clip.width;

    // paint the cell contents
    for (int c = 0; c < ncols && x < xmax; ++c)
      {
        y = y0;
        TableColumn col = cols.getColumn(c);
        int width = col.getWidth();
        int modelCol = col.getModelIndex();

        for (int r = 0; r < nrows && y < ymax; ++r)
          {
            Rectangle bounds = new Rectangle(x, y, width, height);
              if (bounds.intersects(clip))
              {
                TableCellRenderer rend = table.getCellRenderer(r, c);
                Component comp = table.prepareRenderer(rend, r, c);
                gfx.translate(x, y);
                comp.setBounds(new Rectangle(0, 0, width, height));
                // Set correct border on cell renderer.
                if (comp instanceof JComponent)
                  {
                    if (table.isCellSelected(r, c))
                      ((JComponent) comp).setBorder(highlightCellBorder);
                    else
                      ((JComponent) comp).setBorder(cellBorder);
                  }
                comp.paint(gfx);
                gfx.translate(-x, -y);
              }
              y += height;
              if (gap != null)
                y += gap.height;
          }
        x += width;
        if (gap != null)
          x += gap.width;
      }

    // tighten up the x and y max bounds
    ymax = y;
    xmax = x;

    Color grid = table.getGridColor();    

    // paint vertical grid lines    
    if (grid != null && table.getShowVerticalLines())
      {    
        x = x0;
        Color save = gfx.getColor();
        gfx.setColor(grid);
        boolean paintedLine = false;
        for (int c = 0; c < ncols && x < xmax; ++c)
          {
            x += cols.getColumn(c).getWidth();;
            if (gap != null)
              x += gap.width;
            gfx.drawLine(x, y0, x, ymax);
            paintedLine = true;
          }
        gfx.setColor(save);
      }

    // paint horizontal grid lines    
    if (grid != null && table.getShowHorizontalLines())
      {    
        y = y0;
        Color save = gfx.getColor();
        gfx.setColor(grid);
        boolean paintedLine = false;
        for (int r = 0; r < nrows && y < ymax; ++r)
          {
            y += height;
            if (gap != null)
              y += gap.height;
            gfx.drawLine(x0, y, xmax, y);
            paintedLine = true;
          }
        gfx.setColor(save);
      }

  }

}
