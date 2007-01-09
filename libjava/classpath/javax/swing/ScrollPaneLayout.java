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
  private static final long serialVersionUID = -4480022884523193743L;

  public static class UIResource extends ScrollPaneLayout 
    implements javax.swing.plaf.UIResource
  {
    public UIResource()
    {
      super();
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

  public ScrollPaneLayout()
  {
	// Nothing to do here.
  }

  public void syncWithScrollPane(JScrollPane scrollPane) 
  {
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

  /**
   * Removes an existing component.  If oldComponent is not null
   * and is not equal to newComponent, oldComponent must be removed
   * from its parent.
   * @param oldComponent the old Component that may need to be removed.
   * @param newComponent the Component to add.
   * @return the newComponent
   */
  protected Component addSingletonComponent(Component oldComponent,
                                            Component newComponent) 
  {
    if (oldComponent != null && oldComponent != newComponent)
      oldComponent.getParent().remove(oldComponent);
    return newComponent;
  }

  /**
   * Add the specified component to the layout. 
   * @param key must be one of VIEWPORT, VERTICAL_SCROLLBAR,
   * HORIZONTAL_SCROLLBAR, ROW_HEADER, COLUMN_HEADER,
   * LOWER_RIGHT_CORNER, LOWER_LEFT_CORNER, UPPER_RIGHT_CORNER,
   * UPPER_LEFT_CORNER.
   * @param component the Component to add
   * @throws IllegalArgumentException if key is not as above
   */
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
    else
      throw new IllegalArgumentException();
  }

  public void removeLayoutComponent(Component component) 
  {
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
  
  /**
   * Sets the vertical scrollbar policy.
   * @param policy must be one of VERTICAL_SCROLLBAR_AS_NEEDED,
   * VERTICAL_SCROLLBAR_NEVER, VERTICAL_SCROLLBAR_ALWAYS.
   * @throws IllegalArgumentException if policy is not one of the valid
   * JScrollBar policies.
   */
  public void setVerticalScrollBarPolicy(int policy)
  {
    if (policy != VERTICAL_SCROLLBAR_AS_NEEDED && 
        policy != VERTICAL_SCROLLBAR_NEVER &&
        policy != VERTICAL_SCROLLBAR_ALWAYS)
      throw new IllegalArgumentException("Illegal Scrollbar Policy");
    vsbPolicy = policy;
  }

  public int getHorizontalScrollBarPolicy()
  {
    return hsbPolicy;
  }

  /**
   * Sets the horizontal scrollbar policy.
   * @param policy must be one of HORIZONTAL_SCROLLBAR_AS_NEEDED,
   * HORIZONTAL_SCROLLBAR_NEVER, HORIZONTAL_SCROLLBAR_ALWAYS.
   * @throws IllegalArgumentException if policy is not one of the valid 
   * JScrollbar policies.
   */
  public void setHorizontalScrollBarPolicy(int policy)
  {
    if (policy != HORIZONTAL_SCROLLBAR_AS_NEEDED && 
        policy != HORIZONTAL_SCROLLBAR_NEVER &&
        policy != HORIZONTAL_SCROLLBAR_ALWAYS)
      throw new IllegalArgumentException("Illegal Scrollbar Policy");
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

  /**
   * Returns the Component at the specified corner.
   * @param key the corner.
   * @return the Component at the specified corner, or null if
   * key is not one of the four valid corners.
   */
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

  public Dimension preferredLayoutSize(Container parent) 
  {
    // Sun's implementation simply throws a ClassCastException if
    // parent is no JScrollPane, so do we.
    JScrollPane sc = (JScrollPane) parent;
    Dimension viewportSize = viewport.getPreferredSize();
    Dimension viewSize = viewport.getViewSize();
    int width = viewportSize.width;
    int height = viewportSize.height;

    // horizontal scrollbar needed if the view's preferred width
    // is larger than the viewport's preferred width
    if (hsb != null && viewSize.width > viewportSize.width)
      height += hsb.getPreferredSize().height;

    // vertical scrollbar needed if the view's preferred height
    // is larger than the viewport's preferred height
    if (vsb != null && viewSize.height > viewportSize.height)
      width += vsb.getPreferredSize().width;
    if (rowHead != null && rowHead.isVisible())
      width += rowHead.getPreferredSize().width;
    if (colHead != null && colHead.isVisible())
      height += colHead.getPreferredSize().height;

    // Add insets of viewportBorder if present.
    Border vpBorder = sc.getViewportBorder();
    if (vpBorder != null)
      {
        Insets i = vpBorder.getBorderInsets(sc);
        width += i.left + i.right;
        height += i.top + i.bottom;
      }

    Insets i = sc.getInsets();
    return new Dimension(width + i.left + i.right,
                         height + i.left + i.right);
  }

  public Dimension minimumLayoutSize(Container parent)
  {
    // Sun's implementation simply throws a ClassCastException if
    // parent is no JScrollPane, so do we.
    JScrollPane sc = (JScrollPane) parent;
    Insets i = sc.getInsets();
    Dimension viewportMinSize = sc.getViewport().getMinimumSize();

    int width = i.left + i.right + viewportMinSize.width;
    if (sc.getVerticalScrollBarPolicy()
        != JScrollPane.VERTICAL_SCROLLBAR_NEVER)
      width += sc.getVerticalScrollBar().getMinimumSize().width;

    int height = i.top + i.bottom + viewportMinSize.height;
    if (sc.getHorizontalScrollBarPolicy()
        != JScrollPane.HORIZONTAL_SCROLLBAR_NEVER)
      height += sc.getHorizontalScrollBar().getMinimumSize().height;

    // Add insets of viewportBorder if present.
    Border vpBorder = sc.getViewportBorder();
    if (vpBorder != null)
      {
        i = vpBorder.getBorderInsets(sc);
        width += i.left + i.right;
        height += i.top + i.bottom;
      }

    return new Dimension(width, height);
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
    // Sun's implementation simply throws a ClassCastException if
    // parent is no JScrollPane, so do we.
    JScrollPane sc = (JScrollPane) parent;
    JViewport viewport = sc.getViewport();
    Component view = viewport.getView();
    
    // If there is no view in the viewport, there is no work to be done.
    if (view == null)
      return;
    
    Dimension viewSize = viewport.getView().getPreferredSize();

    int x1 = 0, x2 = 0, x3 = 0, x4 = 0;
    int y1 = 0, y2 = 0, y3 = 0, y4 = 0;
    Rectangle scrollPaneBounds = SwingUtilities.calculateInnerArea(sc, null);

    // If there is a viewportBorder, remove its insets from the available
    // space.
    Border vpBorder = sc.getViewportBorder();
    Insets vpi;
    if (vpBorder != null)
      vpi = vpBorder.getBorderInsets(sc);
    else
      vpi = new Insets(0, 0, 0, 0);

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
    
    int vsWidth = 0;
    int hsHeight = 0;

    boolean showVsb = 
      (vsb != null)
      && ((vsbPolicy == VERTICAL_SCROLLBAR_ALWAYS)
          || (vsbPolicy == VERTICAL_SCROLLBAR_AS_NEEDED 
              && viewSize.height > (y4 - y2)));
    
    if (showVsb)
      vsWidth = vsb.getPreferredSize().width;
    
    // The horizontal scroll bar may become necessary if the vertical scroll
    // bar appears, reducing the space, left for the component.
    
    boolean showHsb = 
      (hsb != null)
      && ((hsbPolicy == HORIZONTAL_SCROLLBAR_ALWAYS)
          || (hsbPolicy == HORIZONTAL_SCROLLBAR_AS_NEEDED 
              && viewSize.width > (x4 - x2 - vsWidth)));
    
    if (showHsb)
      hsHeight = hsb.getPreferredSize().height;
    
    // If the horizontal scroll bar appears, and the vertical scroll bar
    // was not necessary assuming that there is no horizontal scroll bar,
    // the vertical scroll bar may become necessary because the horizontal
    // scroll bar reduces the vertical space for the component.
    if (!showVsb)
      {
        showVsb = 
          (vsb != null)
          && ((vsbPolicy == VERTICAL_SCROLLBAR_ALWAYS)
              || (vsbPolicy == VERTICAL_SCROLLBAR_AS_NEEDED 
                  && viewSize.height > (y4 - y2)));
    
        if (showVsb)
          vsWidth = vsb.getPreferredSize().width;
      }

    x3 = x4 - vsWidth;
    y3 = y4 - hsHeight;

    // now set the layout
    if (viewport != null)
      viewport.setBounds(new Rectangle(x2 + vpi.left, y2 + vpi.top,
                                       x3 - x2 - vpi.left - vpi.right,
                                       y3 - y2 - vpi.top - vpi.bottom));

    if (colHead != null)
      colHead.setBounds(new Rectangle(x2, y1, x3 - x2, y2 - y1));

    if (rowHead != null)
      rowHead.setBounds(new Rectangle(x1, y2, x2 - x1, y3 - y2));

    if (showVsb)
      {
        vsb.setVisible(true);
        vsb.setBounds(new Rectangle(x3, y2, x4 - x3, y3 - y2 ));
      }
    else if (vsb != null)
      vsb.setVisible(false);

    if (showHsb)
      {
        hsb.setVisible(true);
        hsb.setBounds(new Rectangle(x2 , y3, x3 - x2, y4 - y3));
      }
    else if (hsb != null)
      hsb.setVisible(false);

    if (upperLeft != null)
      upperLeft.setBounds(new Rectangle(x1, y1, x2 - x1, y2 - y1));

    if (upperRight != null)
      upperRight.setBounds(new Rectangle(x3, y1, x4 - x3, y2 - y1));

    if (lowerLeft != null)
      lowerLeft.setBounds(new Rectangle(x1, y3, x2 - x1, y4 - y3));

    if (lowerRight != null)
      lowerRight.setBounds(new Rectangle(x3, y3, x4 - x3, y4 - y3));
  }

  /**
   * Returns the bounds of the border around a ScrollPane's viewport.
   *
   * @param scrollPane the ScrollPane for which's viewport the border
   *     is requested
   *
   * @deprecated As of Swing 1.1 replaced by
   *     {@link javax.swing.JScrollPane#getViewportBorderBounds}.
   */
  public Rectangle getViewportBorderBounds(JScrollPane scrollPane) 
  {
    return null;
  }


}
