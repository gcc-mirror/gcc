/* MetalBorders.java
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


package javax.swing.plaf.metal;

import java.awt.Color;
import java.awt.Component;
import java.awt.Graphics;
import java.awt.Insets;

import javax.swing.AbstractButton;
import javax.swing.ButtonModel;
import javax.swing.JButton;
import javax.swing.border.AbstractBorder;
import javax.swing.border.Border;
import javax.swing.plaf.BorderUIResource;
import javax.swing.plaf.basic.BasicGraphicsUtils;
import javax.swing.plaf.basic.BasicBorders;

/**
 * This factory class creates borders for the different Swing components
 * UI.
 *
 * @author Roman Kennke (roman@kennke.org)
 */
public class MetalBorders
{

  /**
   * A MarginBorder that gets shared by multiple components.
   * Created on demand by the private helper function {@link
   * #getMarginBorder()}.
   */
  private static BasicBorders.MarginBorder sharedMarginBorder;

  /**
   * The border that is drawn around Swing buttons.
   */
  public static class MetalButtonBorder
    extends AbstractBorder
  {
    /** The borders insets. */
    protected static Insets borderInsets = new Insets(2, 2, 2, 2);

    /**
     * Creates a new instance of ButtonBorder.
     */
    public MetalButtonBorder()
    {
    }

    /**
     * Paints the button border.
     *
     * @param c the component for which we paint the border
     * @param g the Graphics context to use
     * @param x the X coordinate of the upper left corner of c
     * @param y the Y coordinate of the upper left corner of c
     * @param w the width of c
     * @param h the height of c
     */
    public void paintBorder(Component c, Graphics g, int x, int y, int w,
                            int h)
    {
      ButtonModel bmodel = null;
      
      if (c instanceof AbstractButton)
        bmodel = ((AbstractButton) c).getModel();

      Color darkShadow = MetalLookAndFeel.getControlDarkShadow();
      Color shadow = MetalLookAndFeel.getControlShadow();
      Color light = MetalLookAndFeel.getWhite();
      Color middle = MetalLookAndFeel.getControl();

      // draw dark border
      g.setColor(darkShadow);
      g.drawRect(x, y, w - 2, h - 2);

      if (!bmodel.isPressed())
        {
          // draw light border
          g.setColor(light);
          g.drawRect(x + 1, y + 1, w - 2, h - 2);

          // draw crossing pixels of both borders
          g.setColor(middle);
          g.drawRect(x + 1, y + h - 2, 0, 0);
          g.drawRect(x + w - 2, y + 1, 0, 0);
        }
      else
        {
          // draw light border
          g.setColor(light);
          g.drawLine(x + w - 1, y + 1, x + w - 1, y + h - 1);
          g.drawLine(x + 1, y + h - 1, x + w - 1, y + h - 1);

          // draw shadow border
          g.setColor(middle);
          g.drawLine(x + 1, y + 1, x + w - 2, y + 1);
          g.drawLine(x + 1, y + 1, x + 1, y + h - 2);

          // draw crossing pixels of both borders
          g.setColor(shadow);
          g.drawRect(x + 1, y + h - 2, 0, 0);
          g.drawRect(x + w - 2, y + 1, 0, 0);
        }
    }

    /**
     * Returns the insets of the ButtonBorder.
     *
     * @param c the component for which the border is used
     *
     * @return the insets of the ButtonBorder
     */
    public Insets getBorderInsets(Component c)
    {
      return getBorderInsets(c, null);
    }

    /**
     * Returns the insets of the ButtonBorder in the specified Insets object.
     *
     * @param c the component for which the border is used
     * @param newInsets the insets object where to put the values
     *
     * @return the insets of the ButtonBorder
     */
    public Insets getBorderInsets(Component c, Insets newInsets)
    {
      if (newInsets == null)
        newInsets = new Insets(0, 0, 0, 0);

      newInsets.bottom = borderInsets.bottom;
      newInsets.left = borderInsets.left;
      newInsets.right = borderInsets.right;
      newInsets.top = borderInsets.top;
      return newInsets;
    }
  }


  /**
   * Returns a border for Swing buttons in the Metal Look &amp; Feel.
   *
   * @return a border for Swing buttons in the Metal Look &amp; Feel
   */
  public static Border getButtonBorder()
  {
    Border outer = new MetalButtonBorder();
    Border inner = getMarginBorder();

    return new BorderUIResource.CompoundBorderUIResource(outer, inner);
  }

  /**
   * Returns a shared MarginBorder.
   */
  static Border getMarginBorder()  // intentionally not public
  {
    /* Swing is not designed to be thread-safe, so there is no
     * need to synchronize the access to the global variable.
     */
    if (sharedMarginBorder == null)
      sharedMarginBorder = new BasicBorders.MarginBorder();

    return sharedMarginBorder;
  }

}
