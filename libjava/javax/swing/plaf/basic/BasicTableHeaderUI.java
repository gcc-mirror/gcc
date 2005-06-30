/* BasicTableHeaderUI.java --
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

import java.awt.Component;
import java.awt.Dimension;
import java.awt.Graphics;
import java.awt.Rectangle;
import java.awt.event.MouseEvent;

import javax.swing.CellRendererPane;
import javax.swing.JComponent;
import javax.swing.UIDefaults;
import javax.swing.UIManager;
import javax.swing.border.Border;
import javax.swing.event.MouseInputListener;
import javax.swing.plaf.ComponentUI;
import javax.swing.plaf.TableHeaderUI;
import javax.swing.table.JTableHeader;
import javax.swing.table.TableCellRenderer;
import javax.swing.table.TableColumn;
import javax.swing.table.TableColumnModel;

public class BasicTableHeaderUI
  extends TableHeaderUI
{

  public static ComponentUI createUI(JComponent h)
  {
    return new BasicTableHeaderUI();
  }

  protected JTableHeader header;
  protected MouseInputListener mouseInputListener;
  protected CellRendererPane rendererPane;
  protected Border cellBorder;

  class MouseInputHandler
    implements MouseInputListener
  {
    public void mouseClicked(MouseEvent e) {}
    public void mouseDragged(MouseEvent e) {}
    public void mouseEntered(MouseEvent e) {}
    public void mouseExited(MouseEvent e) {}
    public void mouseMoved(MouseEvent e) {}
    public void mousePressed(MouseEvent e) {}
    public void mouseReleased(MouseEvent e) {}
  }

  protected MouseInputListener createMouseInputListener()
  {
    return new MouseInputHandler();
  }

  public BasicTableHeaderUI()
  {
    mouseInputListener = createMouseInputListener();
  }

  protected void installDefaults()
  {
    UIDefaults defaults = UIManager.getLookAndFeelDefaults();
    header.setBackground(defaults.getColor("TableHeader.background"));
    header.setForeground(defaults.getColor("TableHeader.foreground"));
    header.setFont(defaults.getFont("TableHeader.font"));
    cellBorder = defaults.getBorder("TableHeader.cellBorder");
  }

  protected void installKeyboardActions()
  {
  }

  protected void installListeners()
  {
    header.addMouseListener(mouseInputListener);
  }

  public void installUI(JComponent c)
  {
    header = (JTableHeader) c;
    installDefaults();
    installKeyboardActions();
    installListeners();
  }

  protected void uninstallDefaults()
  {
    header.setBackground(null);
    header.setForeground(null);
    header.setFont(null);
  }

  protected void uninstallKeyboardActions()
  {
  }

  protected void uninstallListeners()
  {
    header.removeMouseListener(mouseInputListener);
  }

  public void uninstallUI(JComponent c)
  {
    uninstallListeners();
    uninstallKeyboardActions();
    uninstallDefaults();
  }

  public void paint(Graphics gfx, JComponent c)
  {
    TableColumnModel cmod = header.getColumnModel();
    int ncols = cmod.getColumnCount();
    if (ncols == 0)
      return;
    
    Rectangle clip = gfx.getClipBounds();
    TableCellRenderer defaultRend = header.getDefaultRenderer();

    for (int i = 0; i < ncols; ++i)
      {
        Rectangle bounds = header.getHeaderRect(i);
        if (bounds.intersects(clip))
          {
            TableColumn col = cmod.getColumn(i);
            TableCellRenderer rend = col.getHeaderRenderer();
            if (rend == null)
              rend = defaultRend;
            Object val = col.getHeaderValue();
            Component comp = rend.getTableCellRendererComponent(header.getTable(),
                                                                val,
                                                                false, // isSelected
                                                                false, // isFocused
                                                                -1, i);
            comp.setFont(header.getFont());
            comp.setBackground(header.getBackground());
            comp.setForeground(header.getForeground());
            if (comp instanceof JComponent)
              ((JComponent)comp).setBorder(cellBorder);
            gfx.translate(bounds.x, bounds.y);
            comp.setSize(bounds.width, bounds.height);
            comp.setLocation(0,0);
            comp.paint(gfx);
            gfx.translate(-bounds.x, -bounds.y);
          }
      }

  }
  
  public Dimension getPreferredSize(JComponent c)
  {
    TableColumnModel cmod = header.getColumnModel();
    TableCellRenderer defaultRend = header.getDefaultRenderer();
    int ncols = cmod.getColumnCount();    
    Dimension ret = new Dimension(0,0);
    int spacing = 0;

    if (header.getTable() != null 
        && header.getTable().getIntercellSpacing() != null)
      spacing = header.getTable().getIntercellSpacing().width;
    
    for (int i = 0; i < ncols; ++i)      
      {
        TableColumn col = cmod.getColumn(i);
        TableCellRenderer rend = col.getHeaderRenderer();
        if (rend == null)
          rend = defaultRend;
        Object val = col.getHeaderValue();
        Component comp = rend.getTableCellRendererComponent(header.getTable(),
                                                            val,
                                                            false, // isSelected
                                                            false, // isFocused
                                                            -1, i);
        comp.setFont(header.getFont());
        comp.setBackground(header.getBackground());
        comp.setForeground(header.getForeground());
        if (comp instanceof JComponent)
          ((JComponent)comp).setBorder(cellBorder);

        Dimension d = comp.getPreferredSize();
        ret.width += spacing;
        ret.height = Math.max(d.height, ret.height);        
      }
    ret.width = cmod.getTotalColumnWidth();
    return ret;
  }
  
  
}
