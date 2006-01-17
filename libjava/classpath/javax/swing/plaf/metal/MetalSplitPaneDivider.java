/* MetalSplitPaneDivider.java
Copyright (C) 2005 Free Software Foundation, Inc.

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

package javax.swing.plaf.metal;

import java.awt.Color;
import java.awt.Component;
import java.awt.Container;
import java.awt.Dimension;
import java.awt.Graphics;
import java.awt.LayoutManager;
import java.awt.Point;

import javax.swing.JSplitPane;
import javax.swing.SwingConstants;
import javax.swing.plaf.basic.BasicArrowButton;
import javax.swing.plaf.basic.BasicSplitPaneDivider;

/**
 * The divider that is used by the MetalSplitPaneUI.
 *
 * @author Roman Kennke (roman@kennke.org)
 *
 */
class MetalSplitPaneDivider extends BasicSplitPaneDivider
{
  /** The dark color in the pattern. */
  Color dark;

  /** The light color in the pattern. */
  Color light;
  
  /** The JSplitPane the divider is on. */
  JSplitPane splitPane;

  /** The split pane orientation. */
  int orientation;
  
  /**
   * Creates a new instance of MetalSplitPaneDivider.
   *
   * @param ui the <code>MetalSplitPaneUI</code> that uses this divider
   */
  public MetalSplitPaneDivider(MetalSplitPaneUI ui, Color light, Color dark)
  {
    super(ui);
    setLayout(new MetalDividerLayout());
    this.splitPane = super.splitPane;
    this.orientation = super.orientation;
    this.light = light;
    this.dark = dark;
  }

  /**
   * Paints the divider.
   *
   * @param g the <code>Graphics</code> context to use for painting
   */
  public void paint(Graphics g)
  {
    Dimension s = getSize();
    MetalUtils.fillMetalPattern(splitPane, g, 2, 2, s.width - 4, s.height - 4,
                                light, dark);
    if (splitPane.isOneTouchExpandable())
      {
        ((BasicArrowButton) rightButton).paint(g);
        ((BasicArrowButton) leftButton).paint(g);
      }
  }
  
  /**
   * This helper class acts as the Layout Manager for the divider.
   */
  public class MetalDividerLayout implements LayoutManager
  {
    /** The right button. */
    BasicArrowButton rb;
    
    /** The left button. */
    BasicArrowButton lb;
    
    /**
     * Creates a new DividerLayout object.
     */
    public MetalDividerLayout()
    {
      // Nothing to do here
    }

    /**
     * This method is called when a Component is added.
     *
     * @param string The constraints string.
     * @param c The Component to add.
     */
    public void addLayoutComponent(String string, Component c)
    {
      // Nothing to do here, constraints are set depending on
      // orientation in layoutContainer
    }
    
    /**
     * This method is called to lay out the container.
     *
     * @param c The container to lay out.
     */
    public void layoutContainer(Container c)
    {
      // The only components we care about setting up are the
      // one touch buttons.
      if (splitPane.isOneTouchExpandable())
        {
          if (c.getComponentCount() == 2)
            {
              Component c1 = c.getComponent(0);
              Component c2 = c.getComponent(1);
              if ((c1 instanceof BasicArrowButton)
                  && (c2 instanceof BasicArrowButton))
                {
                  lb = ((BasicArrowButton) c1);
                  rb = ((BasicArrowButton) c2);
                }
            }
          if (rb != null && lb != null)
            {
              Point p = getLocation();
              lb.setSize(lb.getPreferredSize());
              rb.setSize(rb.getPreferredSize());
              lb.setLocation(p.x, p.y);
              
              if (orientation == JSplitPane.HORIZONTAL_SPLIT)
                {
                  rb.setDirection(SwingConstants.EAST);
                  lb.setDirection(SwingConstants.WEST);
                  rb.setLocation(p.x, p.y + lb.getHeight());
                }
              else
                {
                  rb.setDirection(SwingConstants.SOUTH);
                  lb.setDirection(SwingConstants.NORTH);
                  rb.setLocation(p.x + lb.getWidth(), p.y);
                }
            }
        }
    }

    /**
     * This method returns the minimum layout size.
     *
     * @param c The container to calculate for.
     *
     * @return The minimum layout size.
     */
    public Dimension minimumLayoutSize(Container c)
    {
      return preferredLayoutSize(c);
    }

    /**
     * This method returns the preferred layout size.
     *
     * @param c The container to calculate for.
     *
     * @return The preferred layout size.
     */
    public Dimension preferredLayoutSize(Container c)
    {
      int dividerSize = getDividerSize();
      return new Dimension(dividerSize, dividerSize);
    }

    /**
     * This method is called when a component is removed.
     *
     * @param c The component to remove.
     */
    public void removeLayoutComponent(Component c)
    {
      // Nothing to do here. If buttons are removed
      // they will not be layed out when layoutContainer is 
      // called.
    }
  }
}
