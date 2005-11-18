/* MetalButtonUI.java
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
import javax.swing.JButton;
import javax.swing.JComponent;
import javax.swing.SwingConstants;
import javax.swing.UIManager;
import javax.swing.plaf.ComponentUI;
import javax.swing.plaf.UIResource;
import javax.swing.plaf.basic.BasicButtonListener;
import javax.swing.plaf.basic.BasicButtonUI;

/**
 * A UI delegate for the {@link JButton} component.
 *
 * @author Roman Kennke (roman@kennke.org)
 */
public class MetalButtonUI
  extends BasicButtonUI
{

  /** The color used to draw the focus rectangle around the text and/or icon. */
  protected Color focusColor;
    
  /** The background color for the button when it is pressed. */
  protected Color selectColor;

  /** The color for disabled button labels. */
  protected Color disabledTextColor;

  /**
   * Creates a new instance.
   */
  public MetalButtonUI()
  {
    super();
    focusColor = UIManager.getColor(getPropertyPrefix() + "focus");
    selectColor = UIManager.getColor(getPropertyPrefix() + "select");
    disabledTextColor = UIManager.getColor(getPropertyPrefix() + "disabledText");
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
   * Returns the color for the text label of disabled buttons.
   *
   * @return the color for the text label of disabled buttons
   */
  protected Color getDisabledTextColor()
  {
    return disabledTextColor;
  }

  /**
   * Returns a UI delegate for the specified component.
   * 
   * @param c  the component (should be a subclass of {@link AbstractButton}).
   * 
   * @return A new instance of <code>MetalButtonUI</code>.
   */
  public static ComponentUI createUI(JComponent c) {
    return new MetalButtonUI();
  }

  /**
   * Installs the default settings for the specified button.
   * 
   * @param button  the button.
   * 
   * @see #uninstallDefaults(AbstractButton)
   */
  public void installDefaults(AbstractButton button)
  {
    super.installDefaults(button);
    button.setRolloverEnabled(UIManager.getBoolean(
                                            getPropertyPrefix() + "rollover"));
  }
    
  /**
   * Removes the defaults added by {@link #installDefaults(AbstractButton)}.
   */
  public void uninstallDefaults(AbstractButton button) 
  {
    super.uninstallDefaults(button);
    button.setRolloverEnabled(false);
  }

  /**
   * Returns a button listener for the specified button.
   * 
   * @param button  the button.
   * 
   * @return A button listener.
   */
  protected BasicButtonListener createButtonListener(AbstractButton button) 
  {
    return new MetalButtonListener(button);
  }    

  /**
   * Paints the background of the button to indicate that it is in the "pressed"
   * state.
   * 
   * @param g  the graphics context.
   * @param b  the button.
   */
  protected void paintButtonPressed(Graphics g, AbstractButton b) 
  { 
    if (b.isContentAreaFilled())
    {
      Rectangle area = b.getVisibleRect();
      g.setColor(selectColor);
      g.fillRect(area.x, area.y, area.width, area.height);
    }
  }
    
  /** 
   * Paints the focus rectangle around the button text and/or icon.
   * 
   * @param g  the graphics context.
   * @param b  the button.
   * @param viewRect  the button bounds.
   * @param textRect  the text bounds.
   * @param iconRect  the icon bounds.
   */
  protected void paintFocus(Graphics g, AbstractButton b, Rectangle viewRect,
          Rectangle textRect, Rectangle iconRect) {
    if (b.isEnabled() && b.hasFocus() && b.isFocusPainted())
    {
      Color savedColor = g.getColor();
      g.setColor(getFocusColor());
      Rectangle focusRect = iconRect.union(textRect);
      g.drawRect(focusRect.x - 1, focusRect.y,
                 focusRect.width + 1, focusRect.height);
      g.setColor(savedColor);
    }
  }
    
  /**
   * Paints the button text.
   * 
   * @param g  the graphics context.
   * @param c  the button.
   * @param textRect  the text bounds.
   * @param text  the text to display.
   */
  protected void paintText(Graphics g, JComponent c, Rectangle textRect,
          String text) 
  {
    AbstractButton b = (AbstractButton) c;
    Font f = b.getFont();
    g.setFont(f);
    FontMetrics fm = g.getFontMetrics(f);

    if (b.isEnabled())
      {
        g.setColor(b.getForeground());
        g.drawString(text, textRect.x, textRect.y + fm.getAscent());
      }
    else
      {
        g.setColor(getDisabledTextColor());
        g.drawString(text, textRect.x, textRect.y + fm.getAscent());
      }  
  }

  /**
   * If the property <code>Button.gradient</code> is set, then a gradient is
   * painted as background, otherwise the normal superclass behaviour is
   * called.
   */
  public void update(Graphics g, JComponent c)
  {
    AbstractButton b = (AbstractButton) c;
    if (b.isOpaque() && UIManager.get(getPropertyPrefix() + "gradient") != null
        && !b.getModel().isPressed() && b.isEnabled())
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
