/* ScrollPaneLayout.java --
   Copyright (C) 2002 Free Software Foundation, Inc.

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
Free Software Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA
02111-1307 USA.

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
import java.awt.Container;
import java.awt.Dimension;
import java.awt.Insets;
import java.awt.LayoutManager;
import java.awt.Rectangle;
import java.io.Serializable;
import javax.swing.border.Border;

/**
 * ScrollPaneLayout
 * @author	Andrew Selkirk
 * @version	1.0
 */
public class ScrollPaneLayout
  implements LayoutManager, ScrollPaneConstants, Serializable
{
  static final long serialVersionUID = -4480022884523193743L;

  public static class UIResource extends ScrollPaneLayout 
    implements javax.swing.plaf.UIResource {
    public UIResource() {
    }
  }

  JViewport viewport;
  JScrollBar verticalScrollBar;
  JScrollBar horizontalScrollBar;
  JViewport rowHeader;
  JViewport columnHeader;
  Component lowerLeft;
  Component lowerRight;
  Component upperLeft;
  Component upperRight;
  int verticalScrollBarPolicy;
  int horizontalScrollBarPolicy;

  public ScrollPaneLayout() {
		
  }

  public void syncWithScrollPane(JScrollPane scrollPane) {
    viewport = scrollPane.getViewport();
    verticalScrollBar = scrollPane.getVerticalScrollBar();
    horizontalScrollBar = scrollPane.getHorizontalScrollBar();
    verticalScrollBarPolicy = scrollPane.getVerticalScrollBarPolicy();
    horizontalScrollBarPolicy = scrollPane.getHorizontalScrollBarPolicy();
    lowerLeft = scrollPane.getCorner(LOWER_LEFT_CORNER);
    lowerRight = scrollPane.getCorner(LOWER_RIGHT_CORNER);
    upperLeft = scrollPane.getCorner(UPPER_LEFT_CORNER);
    upperRight = scrollPane.getCorner(UPPER_RIGHT_CORNER);    
  }

  protected Component addSingletonComponent(Component oldComponent,
                                            Component newComponent) {
    return null;
  }

  public void addLayoutComponent(String key, Component component) 
  {
    if (key == VIEWPORT)
      viewport = (JViewport) component;
    else if (key == VERTICAL_SCROLLBAR)
      verticalScrollBar = (JScrollBar) component;
    else if (key == HORIZONTAL_SCROLLBAR)
      horizontalScrollBar = (JScrollBar) component;
    else if (key == ROW_HEADER)
      rowHeader = (JViewport) component;
    else if (key == COLUMN_HEADER)
      columnHeader = (JViewport) component;
    else if (key == LOWER_RIGHT_CORNER)
      lowerRight = component;
    else if (key == UPPER_RIGHT_CORNER)
      upperRight = component;
    else if (key == LOWER_LEFT_CORNER)
      lowerLeft = component;
    else if (key == UPPER_LEFT_CORNER)
      upperLeft = component;
  }

  public void removeLayoutComponent(Component component) {
    if (component == viewport)
      viewport = null;
    else if (component == verticalScrollBar)
      verticalScrollBar = null;
    else if (component == horizontalScrollBar)
      horizontalScrollBar = null;
    else if (component == rowHeader)
      rowHeader = null;
    else if (component == columnHeader)
      columnHeader = null;
    else if (component == lowerRight)
      lowerRight = null;
    else if (component == upperRight)
      upperRight = null;
    else if (component == lowerLeft)
      lowerLeft = null;
    else if (component == upperLeft)
      upperLeft = null;
  }

  public int getVerticalScrollBarPolicy() {
    return verticalScrollBarPolicy;
  }

  public void setVerticalScrollBarPolicy(int policy) {
    verticalScrollBarPolicy = policy;
  }

  public int getHorizontalScrollBarPolicy() {
    return horizontalScrollBarPolicy;
  }

  public void setHorizontalScrollBarPolicy(int policy) {
    horizontalScrollBarPolicy = policy;
  }

  public JViewport getViewport() {
    return viewport;
  }

  public JScrollBar getHorizontalScrollBar() {
    return horizontalScrollBar;
  }

  public JScrollBar getVerticalScrollBar() {
    return verticalScrollBar;
  }

  public JViewport getRowHeader() {
    return rowHeader;
  }

  public JViewport getColumnHeader() {
    return columnHeader;
  }

  public Component getCorner(String key) {
    if (key == LOWER_RIGHT_CORNER)
      return lowerRight;
    else if (key == UPPER_RIGHT_CORNER)
      return upperRight;
    else if (key == LOWER_LEFT_CORNER)
      return lowerLeft;
    else if (key == UPPER_LEFT_CORNER)
      return upperLeft;
    return null;
  }

  public Dimension preferredLayoutSize(Container parent) 
  {
    if (parent instanceof JScrollPane)
      {
        JScrollPane sc = (JScrollPane) parent;
        synchronized (sc.getTreeLock ())
          {
            Dimension insetsSize = new Dimension(0,0); 
            Dimension viewportSize = new Dimension(0,0); 
            Dimension viewportInsetsSize = new Dimension(0,0); 
            Dimension columnHeaderSize = new Dimension(0,0); 
            Dimension rowHeaderSize = new Dimension(0,0); 
            Dimension verticalScrollBarSize = new Dimension(0,0); 
            Dimension horizontalScrollBarSize = new Dimension(0,0); 

            Insets insets = sc.getInsets();
            Border viewportBorder = sc.getViewportBorder();
            Insets viewportInsets = null;

            if (viewportBorder != null)
              viewportInsets = viewportBorder.getBorderInsets(parent);

            if (insets != null)
              insetsSize.setSize(insets.left + insets.right,
                                 insets.top + insets.bottom);

            if (viewport != null)
              viewportSize.setSize(viewport.getPreferredSize());

            if (columnHeader != null)
              columnHeaderSize.setSize(columnHeader.getPreferredSize());
            
            if (rowHeader != null)
              rowHeaderSize.setSize(rowHeader.getPreferredSize());

            if (verticalScrollBar != null)
              verticalScrollBarSize.setSize(verticalScrollBar.getPreferredSize());

            if (horizontalScrollBar != null)
              horizontalScrollBarSize.setSize(horizontalScrollBar.getPreferredSize());

            /*
            System.err.println("widths: [vp=" + viewportSize.width +
                               ", h=" + columnHeaderSize.width +
                               ", sc=" + horizontalScrollBarSize.width + "]");

            System.err.println("heights: [vp=" + viewportSize.height +
                               ", h=" + rowHeaderSize.height +
                               ", sc=" + verticalScrollBarSize.height + "]");                    
            */

            return new Dimension(insetsSize.width 
                                 + viewportSize.width
                                 + viewportInsetsSize.width
                                 + rowHeaderSize.width
                                 + verticalScrollBarSize.width,
                                 insetsSize.height
                                 + viewportSize.height
                                 + viewportInsetsSize.height
                                 + columnHeaderSize.height
                                 + horizontalScrollBarSize.height);
          }
      }
    else
      {
        return new Dimension(0,0);
      }
  }

  public Dimension minimumLayoutSize(Container parent)
  {
    if (parent instanceof JScrollPane)
      {
        JScrollPane sc = (JScrollPane) parent;
        synchronized (sc.getTreeLock ())
          {
            Dimension insetsSize = new Dimension(0,0); 
            Dimension viewportSize = new Dimension(0,0); 
            Dimension viewportInsetsSize = new Dimension(0,0); 
            Dimension columnHeaderSize = new Dimension(0,0); 
            Dimension rowHeaderSize = new Dimension(0,0); 
            Dimension verticalScrollBarSize = new Dimension(0,0); 
            Dimension horizontalScrollBarSize = new Dimension(0,0); 

            Insets insets = sc.getInsets();
            Border viewportBorder = sc.getViewportBorder();
            Insets viewportInsets = null;

            if (viewportBorder != null)
              viewportInsets = viewportBorder.getBorderInsets(parent);

            if (insets != null)
              insetsSize.setSize(insets.left + insets.right,
                                 insets.top + insets.bottom);

            if (viewport != null)
              viewportSize.setSize(viewport.getMinimumSize());

            if (columnHeader != null)
              columnHeaderSize.setSize(columnHeader.getMinimumSize());
            
            if (rowHeader != null)
              rowHeaderSize.setSize(rowHeader.getMinimumSize());

            if (verticalScrollBar != null
                && verticalScrollBarPolicy != VERTICAL_SCROLLBAR_NEVER)
              verticalScrollBarSize.setSize(verticalScrollBar.getMinimumSize());

            if (horizontalScrollBar != null 
                && horizontalScrollBarPolicy != HORIZONTAL_SCROLLBAR_NEVER)
              horizontalScrollBarSize.setSize(horizontalScrollBar.getMinimumSize());
            
            return new Dimension(insetsSize.width 
                                 + viewportSize.width
                                 + viewportInsetsSize.width
                                 + rowHeaderSize.width
                                 + verticalScrollBarSize.width,
                                 insetsSize.height
                                 + viewportSize.height
                                 + viewportInsetsSize.height
                                 + columnHeaderSize.height
                                 + horizontalScrollBarSize.height);
          }
      }
    else
      {
        return new Dimension(0,0);
      }
  }

  /**
   *
   *     +----+--------------------+----+ y1
   *     | c1 |   column header    | c2 |
   *     +----+--------------------+----+ y2
   *     | r  |                    | v  |
   *     | o  |                    |    |
   *     | w  |                    | s  |
   *     |    |                    | r  |
   *     | h  |                    | o  |
   *     | e  |      viewport      | l  |
   *     | a  |                    | l  |
   *     | d  |                    | b  |
   *     | e  |                    | a  |
   *     | r  |                    | r  |
   *     +----+--------------------+----+ y3
   *     | c3 |    h scrollbar     | c4 |
   *     +----+--------------------+----+ y4
   *    x1   x2                   x3   x4
   *   
   */

  public void layoutContainer(Container parent) {
    if (parent instanceof JScrollPane)
      {
        JScrollPane sc = (JScrollPane) parent;
        synchronized (sc.getTreeLock ())
          {
            Rectangle scrollPaneBounds = sc.getBounds();
            JViewport viewport = sc.getViewport();
            Dimension viewportSize = viewport.getSize();
            Dimension viewSize = viewport.getView().getSize(); 

            int x1 = 0, x2 = 0, x3 = 0, x4 = 0;
            int y1 = 0, y2 = 0, y3 = 0, y4 = 0;

            x1 = scrollPaneBounds.x;
            y1 = scrollPaneBounds.y;
            x4 = scrollPaneBounds.x + scrollPaneBounds.width;
            y4 = scrollPaneBounds.y + scrollPaneBounds.height;
            
            if (columnHeader != null)
              y2 = columnHeader.getPreferredSize().height;
            else
              y2 = y1;

            if (rowHeader != null)
              x2 = rowHeader.getPreferredSize().width;
            else
              x2 = x1;

            int vsbPolicy = sc.getVerticalScrollBarPolicy();
            int hsbPolicy = sc.getHorizontalScrollBarPolicy();

            boolean showVsb = 
              (verticalScrollBar != null)
              && ((vsbPolicy == VERTICAL_SCROLLBAR_ALWAYS)
                  || (vsbPolicy == VERTICAL_SCROLLBAR_AS_NEEDED 
                      && viewSize.height > viewportSize.height));

            boolean showHsb = 
              (horizontalScrollBar != null)
              && ((hsbPolicy == HORIZONTAL_SCROLLBAR_ALWAYS)
                  || (hsbPolicy == HORIZONTAL_SCROLLBAR_AS_NEEDED 
                      && viewSize.width > viewportSize.width));
            
            if (showVsb)
              x3 = x4 - verticalScrollBar.getPreferredSize().width;
            else
              x3 = x4;

            if (showHsb)
              y3 = y4 - horizontalScrollBar.getPreferredSize().height;
            else
              y3 = y4;

            // now set the layout

            if (viewport != null)
              viewport.setBounds(new Rectangle(x2, y2, x3-x2, y3-y2));

            if (columnHeader != null)
              columnHeader.setBounds(new Rectangle(x2, y1, x3-x2, y2-y1));

            if (rowHeader != null)
              rowHeader.setBounds(new Rectangle(x1, y2, x2-x1, y3-y2));

            if (showVsb)
                verticalScrollBar.setBounds(new Rectangle(x3, y2, x4-x3, y3-y2));

            if (showHsb)
              horizontalScrollBar.setBounds(new Rectangle(x2, y3, x3-x2, y4-y3));

            if (upperLeft != null)
              upperLeft.setBounds(new Rectangle(x1, y1, x2-x1, y2-y1));

            if (upperRight != null)
              upperRight.setBounds(new Rectangle(x3, y1, x4-x3, y2-y1));

            if (lowerLeft != null)
              lowerLeft.setBounds(new Rectangle(x1, y3, x2-x1, y4-y3));

            if (lowerRight != null)
              lowerRight.setBounds(new Rectangle(x3, y3, x4-x3, y4-y3));

          }
      }
  }

  public Rectangle getViewportBorderBounds(JScrollPane scrollPane) {
    return null;
  }


}
