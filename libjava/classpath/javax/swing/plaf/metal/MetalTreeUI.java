/* MetalTreeUI.java
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

import java.awt.Graphics;
import java.awt.Insets;
import java.awt.Rectangle;
import java.beans.PropertyChangeEvent;
import java.beans.PropertyChangeListener;

import javax.swing.JComponent;
import javax.swing.JTree;
import javax.swing.UIManager;
import javax.swing.tree.TreePath;
import javax.swing.plaf.ComponentUI;
import javax.swing.plaf.basic.BasicTreeUI;

/**
 * A UI delegate for the {@link JTree} component.
 */
public class MetalTreeUI extends BasicTreeUI
{
  /**
   * Listens for property changes of the line style and updates the
   * internal setting.
   */
  private class LineStyleListener
    implements PropertyChangeListener
  {

    public void propertyChange(PropertyChangeEvent e)
    {
      if (e.getPropertyName().equals(LINE_STYLE_PROPERTY))
        decodeLineStyle(e.getNewValue());
    }

  }

  /**
   * The key to the lineStyle client property.
   */
  private static final String LINE_STYLE_PROPERTY = "JTree.lineStyle";

  /**
   * The property value indicating no line style.
   */
  private static final String LINE_STYLE_VALUE_NONE = "None";

  /**
   * The property value indicating angled line style.
   */
  private static final String LINE_STYLE_VALUE_ANGLED = "Angled";

  /**
   * The property value indicating horizontal line style.
   */
  private static final String LINE_STYLE_VALUE_HORIZONTAL = "Horizontal";

  /**
   * The line style for None.
   */
  private static final int LINE_STYLE_NONE = 0;

  /**
   * The line style for Angled.
   */
  private static final int LINE_STYLE_ANGLED = 1;

  /**
   * The line style for Horizontal.
   */
  private static final int LINE_STYLE_HORIZONTAL = 2;

  /**
   * The current line style.
   */
  private int lineStyle;

  /**
   * Listens for changes on the line style property and updates the
   * internal settings.
   */
  private PropertyChangeListener lineStyleListener;

  /**
   * Constructs a new instance of <code>MetalTreeUI</code>.
   */
  public MetalTreeUI()
  {
    super();
  }

  /**
   * Returns a new instance of <code>MetalTreeUI</code>.
   *
   * @param component the component for which we return an UI instance
   *
   * @return A new instance of <code>MetalTreeUI</code>.
   */
  public static ComponentUI createUI(JComponent component)
  {
    return new MetalTreeUI();
  }

  /**
   * The horizontal element of legs between nodes starts at the right of the
   * left-hand side of the child node by default. This method makes the
   * leg end before that.
   */
  protected int getHorizontalLegBuffer()
  {
    return super.getHorizontalLegBuffer();
  }

  /**
   * Configures the specified component appropriate for the look and feel.
   * This method is invoked when the ComponentUI instance is being installed
   * as the UI delegate on the specified component. This method should completely
   * configure the component for the look and feel, including the following:
   * 1. Install any default property values for color, fonts, borders, icons,
   *    opacity, etc. on the component. Whenever possible, property values
   *    initialized by the client program should not be overridden.
   * 2. Install a LayoutManager on the component if necessary.
   * 3. Create/add any required sub-components to the component.
   * 4. Create/install event listeners on the component.
   * 5. Create/install a PropertyChangeListener on the component in order
   *    to detect and respond to component property changes appropriately.
   * 6. Install keyboard UI (mnemonics, traversal, etc.) on the component.
   * 7. Initialize any appropriate instance data.
   */
  public void installUI(JComponent c)
  {
    super.installUI(c);

    Object lineStyleProp = c.getClientProperty(LINE_STYLE_PROPERTY);
    decodeLineStyle(lineStyleProp);
    if (lineStyleListener == null)
      lineStyleListener = new LineStyleListener();
    c.addPropertyChangeListener(lineStyleListener);
  }

  /**
   * Reverses configuration which was done on the specified component during
   * installUI. This method is invoked when this UIComponent instance is being
   * removed as the UI delegate for the specified component. This method should
   * undo the configuration performed in installUI, being careful to leave the
   * JComponent instance in a clean state (no extraneous listeners,
   * look-and-feel-specific property objects, etc.). This should include
   * the following:
   * 1. Remove any UI-set borders from the component.
   * 2. Remove any UI-set layout managers on the component.
   * 3. Remove any UI-added sub-components from the component.
   * 4. Remove any UI-added event/property listeners from the component.
   * 5. Remove any UI-installed keyboard UI from the component.
   * 6. Nullify any allocated instance data objects to allow for GC.
   */
  public void uninstallUI(JComponent c)
  {
    super.uninstallUI(c);
    if (lineStyleListener != null)
      c.removePropertyChangeListener(lineStyleListener);
    lineStyleListener = null;
  }

  /**
   * This function converts between the string passed into the client
   * property and the internal representation (currently an int).
   *
   * @param lineStyleFlag - String representation
   */
  protected void decodeLineStyle(Object lineStyleFlag)
  {
    if (lineStyleFlag == null || lineStyleFlag.equals(LINE_STYLE_VALUE_ANGLED))
      lineStyle = LINE_STYLE_ANGLED;
    else if (lineStyleFlag.equals(LINE_STYLE_VALUE_HORIZONTAL))
      lineStyle = LINE_STYLE_HORIZONTAL;
    else if (lineStyleFlag.equals(LINE_STYLE_VALUE_NONE))
      lineStyle = LINE_STYLE_NONE;
    else
      lineStyle = LINE_STYLE_ANGLED;
  }

  /**
   * Checks if the location is in expand control.
   *
   * @param row - current row
   * @param rowLevel - current level
   * @param mouseX - current x location of the mouse click
   * @param mouseY - current y location of the mouse click
   */
  protected boolean isLocationInExpandControl(int row, int rowLevel,
                                          int mouseX, int mouseY)
  {
    return super.isLocationInExpandControl(tree.getPathForRow(row),
                                           mouseX, mouseY);
  }

  /**
   * Paints the specified component appropriate for the look and feel.
   * This method is invoked from the ComponentUI.update method when the
   * specified component is being painted. Subclasses should override this
   * method and use the specified Graphics object to render the content of
   * the component.
   *
   * @param g - the current graphics configuration.
   * @param c - the current component to draw
   */
  public void paint(Graphics g, JComponent c)
  {
    // Calls BasicTreeUI's paint since it takes care of painting all
    // types of icons.
    super.paint(g, c);

    if (lineStyle == LINE_STYLE_HORIZONTAL)
      paintHorizontalSeparators(g, c);
  }

  /**
   * Paints the horizontal separators.
   *
   * @param g - the current graphics configuration.
   * @param c - the current component to draw
   */
  protected void paintHorizontalSeparators(Graphics g, JComponent c)
  {
    g.setColor(UIManager.getColor("Tree.line"));
    Rectangle clip = g.getClipBounds();
    int row0 = getRowForPath(tree, getClosestPathForLocation(tree, 0, clip.y));
    int row1 =
      getRowForPath(tree, getClosestPathForLocation(tree, 0,
                                                    clip.y + clip.height - 1));
    if (row0 >= 0 && row1 >= 0)
      {
        for (int i = row0; i <= row1; i++)
          {
            TreePath p = getPathForRow(tree, i);
            if (p != null && p.getPathCount() == 2)
              {
                Rectangle r = getPathBounds(tree, getPathForRow(tree, i));
                if (r != null)
                  {
                    g.drawLine(clip.x, r.y, clip.x + clip.width, r.y);
                  }
              }
          }
      }
  }


  /**
   * Paints the vertical part of the leg. The receiver should NOT modify
   * clipBounds, insets.
   *
   * @param g - the current graphics configuration.
   * @param clipBounds -
   * @param insets -
   * @param path - the current path
   */
  protected void paintVerticalPartOfLeg(Graphics g, Rectangle clipBounds,
                                    Insets insets, TreePath path)
  {
    if (lineStyle == LINE_STYLE_ANGLED)
      super.paintVerticalPartOfLeg(g, clipBounds, insets, path);
  }

  /**
   * Paints the horizontal part of the leg. The receiver should NOT \
   * modify clipBounds, or insets.
   * NOTE: parentRow can be -1 if the root is not visible.
   */
  protected void paintHorizontalPartOfLeg(Graphics g, Rectangle clipBounds,
                                        Insets insets, Rectangle bounds,
                                        TreePath path, int row,
                                        boolean isExpanded, boolean hasBeenExpanded,
                                        boolean isLeaf)
  {
    if (lineStyle == LINE_STYLE_ANGLED)
      super.paintHorizontalPartOfLeg(g, clipBounds, insets, bounds, path, row,
                                     isExpanded, hasBeenExpanded, isLeaf);
  }
}
