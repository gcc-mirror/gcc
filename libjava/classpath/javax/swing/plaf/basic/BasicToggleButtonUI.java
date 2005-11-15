/* BasicToggleButtonUI.java
   Copyright (C) 2002, 2004, 2005 Free Software Foundation, Inc.

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

import java.awt.Font;
import java.awt.Graphics;
import java.awt.Rectangle;

import javax.swing.AbstractButton;
import javax.swing.JComponent;
import javax.swing.SwingUtilities;
import javax.swing.plaf.ComponentUI;

public class BasicToggleButtonUI extends BasicButtonUI
{
  public static ComponentUI createUI(final JComponent component)
  {
    return new BasicToggleButtonUI();
  }    

  /**
   * Returns the prefix for the UI defaults property for this UI class.
   * This is &apos;ToggleButton&apos; for this class.
   *
   * @return the prefix for the UI defaults property
   */
  protected String getPropertyPrefix()
  {
    return "ToggleButton.";
  }

  /**
   * Paint the component, which is an {@link AbstractButton}, according to 
   * its current state.
   *
   * @param g The graphics context to paint with
   * @param c The component to paint the state of
   */
  public void paint(Graphics g, JComponent c)
  {      
    AbstractButton b = (AbstractButton) c;

    Rectangle tr = new Rectangle();
    Rectangle ir = new Rectangle();
    Rectangle vr = new Rectangle();

    Font f = c.getFont();

    g.setFont(f);

    if (b.isBorderPainted())
      SwingUtilities.calculateInnerArea(b, vr);
    else
      vr = SwingUtilities.getLocalBounds(b);
    String text = SwingUtilities.layoutCompoundLabel(c, g.getFontMetrics(f), 
                                                     b.getText(),
                                                     currentIcon(b),
                                                     b.getVerticalAlignment(), 
                                                     b.getHorizontalAlignment(),
                                                     b.getVerticalTextPosition(), 
                                                     b.getHorizontalTextPosition(),
                                                     vr, ir, tr, 
                                                     b.getIconTextGap() 
                                                     + defaultTextShiftOffset);

    if ((b.getModel().isArmed() && b.getModel().isPressed()) 
        || b.isSelected())
      paintButtonPressed(g, b);

    paintIcon(g, b, ir);
    if (text != null)
      paintText(g, b, tr, text);
    if (b.isFocusOwner() && b.isFocusPainted())
      paintFocus(g, b, vr, tr, ir);
  }

  /**
   * Paints the icon for the toggle button. This delegates to
   * {@link BasicButtonUI#paintIcon(Graphics, JComponent, Rectangle)}.
   *
   * @param g the graphics context
   * @param b the button to paint the icon for
   * @param iconRect the area allocated for the icon
   */
  protected void paintIcon(Graphics g, AbstractButton b, Rectangle iconRect)
  {
    super.paintIcon(g, b, iconRect);
  }
}
