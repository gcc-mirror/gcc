/* BasicRadioButtonUI.java
   Copyright (C) 2002, 2004, 2006, Free Software Foundation, Inc.

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

import java.awt.Color;
import java.awt.Dimension;
import java.awt.Font;
import java.awt.Graphics;
import java.awt.Insets;
import java.awt.Rectangle;

import javax.swing.AbstractButton;
import javax.swing.ButtonModel;
import javax.swing.Icon;
import javax.swing.JComponent;
import javax.swing.SwingUtilities;
import javax.swing.UIManager;
import javax.swing.plaf.ComponentUI;
import javax.swing.text.View;

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
  public static ComponentUI createUI(final JComponent c)
  {
    return new BasicRadioButtonUI();
  }

  /**
   * Creates a new instance of <code>BasicButtonUI</code>.
   */
  public BasicRadioButtonUI()
  {
    // nothing to do
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
    icon = UIManager.getIcon(getPropertyPrefix() + "icon");
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
    return icon;
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
    Dimension size = c.getSize();
    Insets i = b.getInsets();
    textR.x = 0;
    textR.y = 0;
    textR.width = 0;
    textR.height = 0;
    iconR.x = 0;
    iconR.y = 0;
    iconR.width = 0;
    iconR.height = 0;
    viewR.x = i.left;
    viewR.y = i.right;
    viewR.width = size.width - i.left - i.right;
    viewR.height = size.height - i.top - i.bottom;

    Font f = c.getFont();

    g.setFont(f);

    // This is the icon that we use for layout.
    Icon icon = b.getIcon();
    if (icon == null)
      icon = getDefaultIcon();

    // Figure out the correct icon.
    Icon currentIcon = getCurrentIcon(b);

    // Do the layout.
    String text = SwingUtilities.layoutCompoundLabel(c, g.getFontMetrics(f),
       b.getText(), currentIcon == null ? getDefaultIcon() : currentIcon,
       b.getVerticalAlignment(), b.getHorizontalAlignment(),
       b.getVerticalTextPosition(), b.getHorizontalTextPosition(),
       viewR, iconR, textR, b.getIconTextGap());

    // .. and paint it.
    if (currentIcon != null)
      currentIcon.paintIcon(c, g, iconR.x, iconR.y);

    // Paint text and focus indicator.
    if (text != null)
      {
        // Maybe render HTML in the radio button.
        View v = (View) c.getClientProperty(BasicHTML.propertyKey);
        if (v != null)
          v.paint(g, textR);
        else
          paintText(g, b, textR, text);

        // Paint focus indicator if necessary.
        if (b.hasFocus() && b.isFocusPainted()
            && textR.width > 0 && textR.height > 0)
          paintFocus(g, textR, size);
      }
  }

  /**
   * Determines the icon to be displayed for the specified radio button.
   *
   * @param b the radio button
   *
   * @return the icon
   */
  private Icon getCurrentIcon(AbstractButton b)
  {
    ButtonModel m = b.getModel();
    Icon currentIcon = b.getIcon();

    if (currentIcon == null)
      {
        currentIcon = getDefaultIcon();
      }
    else
      {
        if (! m.isEnabled())
          {
            if (m.isSelected())
              currentIcon = b.getDisabledSelectedIcon();
            else
              currentIcon = b.getDisabledIcon();
          }
        else if (m.isPressed() && m.isArmed())
          {
            currentIcon = b.getPressedIcon();
            if (currentIcon == null)
              currentIcon = b.getSelectedIcon();
          }
        else if (m.isSelected())
          {
            if (b.isRolloverEnabled() && m.isRollover())
              {
                currentIcon = b.getRolloverSelectedIcon();
                if (currentIcon == null)
                  currentIcon = b.getSelectedIcon();
              }
            else
              currentIcon = b.getSelectedIcon();
          }
        else if (b.isRolloverEnabled() && m.isRollover())
          {
            currentIcon = b.getRolloverIcon();
          }
        if (currentIcon == null)
          currentIcon = b.getIcon();
      }
    return currentIcon;
  }

  public Dimension getPreferredSize(JComponent c)
  {
    // This is basically the same code as in
    // BasicGraphicsUtils.getPreferredButtonSize() but takes the default icon
    // property into account. JRadioButton and subclasses always have an icon:
    // the check box. If the user explicitly changes it with setIcon() that
    // one will be used for layout calculations and painting instead.
    // The other icon properties are ignored.
    AbstractButton b = (AbstractButton) c;

    Insets insets = b.getInsets();

    String text = b.getText();
    Icon i = b.getIcon();
    if (i == null)
      i = getDefaultIcon();

    textR.x = 0;
    textR.y = 0;
    textR.width = 0;
    textR.height = 0;
    iconR.x = 0;
    iconR.y = 0;
    iconR.width = 0;
    iconR.height = 0;
    viewR.x = 0;
    viewR.y = 0;
    viewR.width = Short.MAX_VALUE;
    viewR.height = Short.MAX_VALUE;

    SwingUtilities.layoutCompoundLabel(b, // for the component orientation
                                       b.getFontMetrics(b.getFont()),
                                       text, i, b.getVerticalAlignment(),
                                       b.getHorizontalAlignment(),
                                       b.getVerticalTextPosition(),
                                       b.getHorizontalTextPosition(),
                                       viewR, iconR, textR,
                                       text == null ? 0 : b.getIconTextGap());

    Rectangle r = SwingUtilities.computeUnion(textR.x, textR.y, textR.width,
                                              textR.height, iconR);

    return new Dimension(insets.left + r.width + insets.right,
                         insets.top + r.height + insets.bottom);
  }

  /**
   * Paints the focus indicator for JRadioButtons.
   *
   * @param g the graphics context
   * @param tr the rectangle for the text label
   * @param size the size of the <code>JRadioButton</code> component.
   */
  protected void paintFocus(Graphics g, Rectangle tr, Dimension size)
  {
    Color focusColor = UIManager.getColor(getPropertyPrefix() + ".focus");
    Color saved = g.getColor();
    g.setColor(focusColor);
    g.drawRect(tr.x, tr.y, tr.width, tr.height);
    g.setColor(saved);
  }
}
