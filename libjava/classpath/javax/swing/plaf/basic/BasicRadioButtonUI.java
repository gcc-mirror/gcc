/* BasicRadioButtonUI.java
   Copyright (C) 2002, 2004 Free Software Foundation, Inc.

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
import javax.swing.Icon;
import javax.swing.JComponent;
import javax.swing.SwingUtilities;
import javax.swing.UIDefaults;
import javax.swing.UIManager;
import javax.swing.plaf.ComponentUI;

/**
 * The BasicLookAndFeel UI implementation for
 * {@link javax.swing.JRadioButton}s.
 */
public class BasicRadioButtonUI extends BasicToggleButtonUI
{
  /**
   * The default icon for JRadioButtons. The default icon displays the usual
   * RadioButton and is sensible to the selection state of the button,
   * and can be used both as normal icon as well as selectedIcon.
   */
  protected Icon icon;

  /**
   * Creates and returns a new instance of <code>BasicRadioButtonUI</code>.
   *
   * @return a new instance of <code>BasicRadioButtonUI</code>
   */
  public static ComponentUI createUI(final JComponent c)  {
    return new BasicRadioButtonUI();
  }

  /**
   * Creates a new instance of <code>BasicButtonUI</code>.
   */
  public BasicRadioButtonUI()
  {
    icon = getDefaultIcon();
  }

  /**
   * Installs defaults from the Look &amp; Feel table on the specified
   * button.
   *
   * @param b the button on which to install the defaults
   */
  protected void installDefaults(AbstractButton b)
  {
    super.installDefaults(b);
    if (b.getIcon() == null)
      b.setIcon(icon);
    if (b.getSelectedIcon() == null)
      b.setSelectedIcon(icon);
  }

  /**
   * Returns the prefix used for UIDefaults properties. This is
   * <code>RadioButton</code> in this case.
   *
   * @return the prefix used for UIDefaults properties
   */
  protected String getPropertyPrefix()
  {
    return "RadioButton.";
  }

  /**
   * Returns the default icon for JRadioButtons.
   * The default icon displays the usual
   * RadioButton and is sensible to the selection state of the button,
   * and can be used both as normal icon as well as selectedIcon.
   *
   * @return the default icon for JRadioButtons
   */
  public Icon getDefaultIcon()
  {
    UIDefaults defaults = UIManager.getLookAndFeelDefaults();
    return defaults.getIcon(getPropertyPrefix() + "icon");
  }

  /**
   * Paints the RadioButton.
   *
   * @param g the Graphics context to paint with
   * @param c the button to paint
   */
  public void paint(Graphics g, JComponent c)
  {
    AbstractButton b = (AbstractButton) c;

    Rectangle tr = new Rectangle();
    Rectangle ir = new Rectangle();
    Rectangle vr = new Rectangle();

    Font f = c.getFont();

    g.setFont(f);

    Icon currentIcon = null;
    if (b.isSelected())
      currentIcon = b.getSelectedIcon();
    else
      currentIcon = b.getIcon();

    SwingUtilities.calculateInnerArea(b, vr);
    String text = SwingUtilities.layoutCompoundLabel
      (c, g.getFontMetrics(f), b.getText(), currentIcon,
       b.getVerticalAlignment(), b.getHorizontalAlignment(),
       b.getVerticalTextPosition(), b.getHorizontalTextPosition(),
       vr, ir, tr, b.getIconTextGap() + defaultTextShiftOffset);
    
    if (currentIcon != null)
      {
        currentIcon.paintIcon(c, g, ir.x, ir.y);
      }
    if (text != null)
      paintText(g, b, tr, text);
    paintFocus(g, b, vr, tr, ir);
  }
}
