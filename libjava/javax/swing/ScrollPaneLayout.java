/* ScrollPaneLayout.java --
   Copyright (C) 2002, 2004  Free Software Foundation, Inc.

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
import java.awt.Point;
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
  private static final long serialVersionUID = -4480022884523193743L;

  public static class UIResource extends ScrollPaneLayout 
    implements javax.swing.plaf.UIResource {
    public UIResource() {
    }
  }

  protected JViewport viewport;
  protected JScrollBar vsb;
  protected JScrollBar hsb;
  protected JViewport rowHead;
  protected JViewport colHead;
  protected Component lowerLeft;
  protected Component lowerRight;
  protected Component upperLeft;
  protected Component upperRight;
  protected int vsbPolicy;
  protected int hsbPolicy;

  public ScrollPaneLayout() {
		
  }

  public void syncWithScrollPane(JScrollPane scrollPane) {
    viewport = scrollPane.getViewport();
    rowHead = scrollPane.getRowHeader();
    colHead = scrollPane.getColumnHeader();
    vsb = scrollPane.getVerticalScrollBar();
    hsb = scrollPane.getHorizontalScrollBar();
    vsbPolicy = scrollPane.getVerticalScrollBarPolicy();
    hsbPolicy = scrollPane.getHorizontalScrollBarPolicy();
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
      vsb = (JScrollBar) component;
    else if (key == HORIZONTAL_SCROLLBAR)
      hsb = (JScrollBar) component;
    else if (key == ROW_HEADER)
      rowHead = (JViewport) component;
    else if (key == COLUMN_HEADER)
      colHead = (JViewport) component;
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
    else if (component == vsb)
      vsb = null;
    else if (component == hsb)
      hsb = null;
    else if (component == rowHead)
      rowHead = null;
    else if (component == colHead)
      colHead = null;
    else if (component == lowerRight)
      lowerRight = null;
    else if (component == upperRight)
      upperRight = null;
    else if (component == lowerLeft)
      lowerLeft = null;
    else if (component == upperLeft)
      upperLeft = null;
  }

  public int getVerticalScrollBarPolicy()
  {
    return vsbPolicy;
  }

  public void setVerticalScrollBarPolicy(int policy)
  {
    vsbPolicy = policy;
  }

  public int getHorizontalScrollBarPolicy()
  {
    return hsbPolicy;
  }

  public void setHorizontalScrollBarPolicy(int policy)
  {
    hsbPolicy = policy;
  }

  public JViewport getViewport()
  {
    return viewport;
  }

  public JScrollBar getHorizontalScrollBar()
  {
    return hsb;
  }

  public JScrollBar getVerticalScrollBar()
  {
    return vsb;
  }

  public JViewport getRowHeader()
  {
    return rowHead;
  }

  public JViewport getColumnHeader()
  {
    return colHead;
  }

  public Component getCorner(String key)
  {
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

  private static void maybeSetPreferredSize(JComponent src, Dimension dim)
  {
    Dimension tmp = null;
    if (src != null)
      tmp = src.getPreferredSize();
    if (tmp != null)
      dim.setSize(tmp);        
  }

  private static void maybeSetMinimumSize(JComponent src, Dimension dim)
  {
    Dimension tmp = null;
    if (src != null)
      tmp = src.getMinimumSize();
    if (tmp != null)
      dim.setSize(tmp);
  }

  public Dimension preferredLayoutSize(Container parent) 
  {
    if (parent != null && parent instanceof JScrollPane)
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
              {
                viewportInsets = viewportBorder.getBorderInsets(parent);
                if (viewportInsets != null)
                  viewportInsetsSize.setSize(viewportInsets.left + viewportInsets.right,
                                             viewportInsets.top + viewportInsets.bottom);
              }

            if (insets != null)
              insetsSize.setSize(insets.left + insets.right,
                                 insets.top + insets.bottom);

            if (viewport != null)
              {
                Component view = null;
                Scrollable scr = null;
                Dimension pref = null;
                
                view = viewport.getView();
                if (view != null && view instanceof Scrollable)
                  scr = (Scrollable) view;
                if (scr != null)
                  pref = scr.getPreferredScrollableViewportSize();
                if (pref == null)
                  pref = viewport.getPreferredSize();
                if (pref != null)
                  viewportSize.setSize(pref);
              }
                       
            maybeSetPreferredSize(colHead, columnHeaderSize);
            maybeSetPreferredSize(rowHead, rowHeaderSize);
            maybeSetPreferredSize(vsb, verticalScrollBarSize);
            maybeSetPreferredSize(hsb, horizontalScrollBarSize);

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
              {
                viewportInsets = viewportBorder.getBorderInsets(parent);
                if (viewportInsets != null)
                  viewportInsetsSize.setSize(viewportInsets.left + viewportInsets.right,
                                             viewportInsets.top + viewportInsets.bottom);
              }
            
            if (insets != null)
              insetsSize.setSize(insets.left + insets.right,
                                 insets.top + insets.bottom);

            maybeSetMinimumSize(colHead, columnHeaderSize);
            maybeSetMinimumSize(rowHead, rowHeaderSize);
            
            if (vsbPolicy != VERTICAL_SCROLLBAR_NEVER)
              maybeSetMinimumSize(vsb, verticalScrollBarSize);

            if (hsbPolicy != HORIZONTAL_SCROLLBAR_NEVER)
              maybeSetMinimumSize(hsb, horizontalScrollBarSize);
            
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
  public void layoutContainer(Container parent)
  {
    if (parent instanceof JScrollPane)
      {
        JScrollPane sc = (JScrollPane) parent;
        synchronized (sc.getTreeLock ())
          {
            JViewport viewport = sc.getViewport();
            Dimension viewSize = viewport.getViewSize(); 
            Point viewPos = viewport.getViewPosition(); 

            int x1 = 0, x2 = 0, x3 = 0, x4 = 0;
            int y1 = 0, y2 = 0, y3 = 0, y4 = 0;

            Rectangle scrollPaneBounds = SwingUtilities.calculateInnerArea(sc, null);

            x1 = scrollPaneBounds.x;
            y1 = scrollPaneBounds.y;
            x4 = scrollPaneBounds.x + scrollPaneBounds.width;
            y4 = scrollPaneBounds.y + scrollPaneBounds.height;
            
            if (colHead != null)
              y2 = y1 + colHead.getPreferredSize().height;
            else
              y2 = y1;

            if (rowHead != null)
              x2 = x1 + rowHead.getPreferredSize().width;
            else
              x2 = x1;

            int vsbPolicy = sc.getVerticalScrollBarPolicy();
            int hsbPolicy = sc.getHorizontalScrollBarPolicy();

            x3 = x4 - vsb.getPreferredSize().width;
            y3 = y4 - hsb.getPreferredSize().height;

            boolean showVsb = 
              (vsb != null)
              && ((vsbPolicy == VERTICAL_SCROLLBAR_ALWAYS)
                  || (vsbPolicy == VERTICAL_SCROLLBAR_AS_NEEDED 
                      && viewSize.height > (y3 - y2)));

            boolean showHsb = 
              (hsb != null)
              && ((hsbPolicy == HORIZONTAL_SCROLLBAR_ALWAYS)
                  || (hsbPolicy == HORIZONTAL_SCROLLBAR_AS_NEEDED 
                      && viewSize.width > (x3 - x2)));
            
            if (!showVsb)
              x3 = x4;
            
            if (!showHsb)
              y3 = y4;

            // now set the layout

            if (viewport != null)
              viewport.setBounds(new Rectangle(x2, y2, x3-x2, y3-y2));

            if (colHead != null)
              colHead.setBounds(new Rectangle(x2, y1, x3-x2, y2-y1));

            if (rowHead != null)
              rowHead.setBounds(new Rectangle(x1, y2, x2-x1, y3-y2));

            if (showVsb)
              {
                vsb.setVisible(true);
                vsb.setBounds(new Rectangle(x3, y2, x4-x3, y3-y2));
              }
            else if (vsb != null)
              vsb.setVisible(false);

            if (showHsb)
              {
                hsb.setVisible(true);
                hsb.setBounds(new Rectangle(x2, y3, x3-x2, y4-y3));
              }
            else if (hsb != null)
              hsb.setVisible(false);

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
