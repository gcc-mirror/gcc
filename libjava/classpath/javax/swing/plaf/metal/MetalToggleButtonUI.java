/* MetalToggleButtonUI.java
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
import java.awt.Font;
import java.awt.FontMetrics;
import java.awt.Graphics;
import java.awt.Rectangle;

import javax.swing.AbstractButton;
import javax.swing.ButtonModel;
import javax.swing.JComponent;
import javax.swing.JToggleButton;
import javax.swing.SwingConstants;
import javax.swing.SwingUtilities;
import javax.swing.UIManager;
import javax.swing.plaf.ComponentUI;
import javax.swing.plaf.UIResource;
import javax.swing.plaf.basic.BasicButtonUI;
import javax.swing.plaf.basic.BasicToggleButtonUI;

/**
 * A UI delegate for the {@link JToggleButton} component.
 */
public class MetalToggleButtonUI
  extends BasicToggleButtonUI
{

  /** The color for the focus border. */
  protected Color focusColor;

  /** The color that indicates a selected button. */
  protected Color selectColor;

  /** The color for disabled button labels. */
  protected Color disabledTextColor;

  /**
   * Returns a new instance of <code>MetalToggleButtonUI</code>.
   *
   * @param component the component for which we return an UI instance
   *
   * @return A new instance of <code>MetalToggleButtonUI</code>.
   */
  public static ComponentUI createUI(JComponent component)
  {
    return new MetalToggleButtonUI();
  }

  /**
   * Constructs a new instance of <code>MetalToggleButtonUI</code>.
   */
  public MetalToggleButtonUI()
  {
    super();
  }

  /**
   * Returns the color for the focus border.
   *
   * @return the color for the focus border
   */
  protected Color getFocusColor()
  {
    return focusColor;
  }

  /**
   * Returns the color that indicates a selected button.
   *
   * @return the color that indicates a selected button
   */
  protected Color getSelectColor()
  {
    return selectColor;
  }

  /**
   * Returns the color for the text label of disabled buttons.  The value
   * is initialised in the {@link #installDefaults(AbstractButton)} method
   * by reading the <code>ToggleButton.disabledText</code> item from the UI
   * defaults.
   *
   * @return The color for the text label of disabled buttons.
   */
  protected Color getDisabledTextColor()
  {
    return disabledTextColor;
  }

  /**
   * Updates the button with the defaults for this look and feel.
   *
   * @param b  the button.
   */
  public void installDefaults(AbstractButton b)
  {
    super.installDefaults(b);
    focusColor = UIManager.getColor(getPropertyPrefix() + "focus");
    selectColor = UIManager.getColor(getPropertyPrefix() + "select");
    disabledTextColor = UIManager.getColor(getPropertyPrefix() + "disabledText");
  }

  /**
   * Paints the button background when it is pressed/selected.
   *
   * @param g  the graphics device.
   * @param b  the button.
   */
  protected void paintButtonPressed(Graphics g, AbstractButton b)
  {
    if (b.isContentAreaFilled() && b.isOpaque())
      {
        Color saved = g.getColor();
        Rectangle bounds = SwingUtilities.getLocalBounds(b);
        g.setColor(selectColor);
        g.fillRect(bounds.x, bounds.y, bounds.width, bounds.height);
        g.setColor(saved);
      }
  }

  /**
   * Paints the text for the button.
   *
   * As of JDK 1.4 this method is obsolete.
   * Use {@link BasicButtonUI#paintText(java.awt.Graphics,
   * javax.swing.AbstractButton, java.awt.Rectangle, java.lang.String)}.
   *
   * @param g  the graphics device.
   * @param c  the component.
   * @param textRect  the bounds for the text.
   * @param text  the text.
   *
   */
  protected void paintText(Graphics g, JComponent c, Rectangle textRect,
                           String text)
  {
    Font savedFont = g.getFont();
    Color savedColor = g.getColor();
    g.setFont(c.getFont());
    if (c.isEnabled())
      g.setColor(c.getForeground());
    else
      g.setColor(disabledTextColor);
    FontMetrics fm = g.getFontMetrics(c.getFont());
    int ascent = fm.getAscent();
    g.drawString(text, textRect.x, textRect.y + ascent);
    g.setFont(savedFont);
    g.setColor(savedColor);
  }

  /**
   * Draws the focus highlight around the text and icon.
   *
   * @param g  the graphics device.
   * @param b  the button.
   */
  protected void paintFocus(Graphics g, AbstractButton b, Rectangle viewRect,
      Rectangle textRect, Rectangle iconRect)
  {
    if (!b.hasFocus())
      return;
    Color saved = g.getColor();
    g.setColor(focusColor);
    Rectangle fr = iconRect.union(textRect);
    g.drawRect(fr.x - 1, fr.y - 1, fr.width + 1, fr.height + 1);
    g.setColor(saved);
  }

  /**
   * If the property <code>ToggleButton.gradient</code> is set, then a gradient
   * is painted as background, otherwise the normal superclass behaviour is
   * called.
   */
  public void update(Graphics g, JComponent c)
  {
    AbstractButton b = (AbstractButton) c;
    ButtonModel m = b.getModel();
    if (b.getBackground() instanceof UIResource
        && b.isContentAreaFilled()
        && b.isEnabled() && ! m.isArmed() && ! m.isPressed()
        && UIManager.get(getPropertyPrefix() + "gradient") != null)
      {
        MetalUtils.paintGradient(g, 0, 0, c.getWidth(), c.getHeight(),
                                 SwingConstants.VERTICAL,
                                 getPropertyPrefix() + "gradient");
        paint(g, c);
      }
    else
      super.update(g, c);
  }

}
