/* BasicButtonUI.java --
   Copyright (C) 2002, 2004, 2005  Free Software Foundation, Inc.

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

import java.awt.Dimension;
import java.awt.Font;
import java.awt.FontMetrics;
import java.awt.Graphics;
import java.awt.Rectangle;

import javax.swing.AbstractButton;
import javax.swing.ButtonModel;
import javax.swing.Icon;
import javax.swing.InputMap;
import javax.swing.JButton;
import javax.swing.JComponent;
import javax.swing.LookAndFeel;
import javax.swing.SwingUtilities;
import javax.swing.UIDefaults;
import javax.swing.UIManager;
import javax.swing.plaf.ButtonUI;
import javax.swing.plaf.ComponentUI;
import javax.swing.plaf.UIResource;

/**
 * A UI delegate for the {@link JButton} component.
 */
public class BasicButtonUI extends ButtonUI
{
  /**
   * A constant used to pad out elements in the button's layout and
   * preferred size calculations.
   */
  protected int defaultTextIconGap = 4;

  /**
   * A constant added to the defaultTextIconGap to adjust the text
   * within this particular button.
   */
  protected int defaultTextShiftOffset = 0;

  private int textShiftOffset;

  /**
   * Factory method to create an instance of BasicButtonUI for a given
   * {@link JComponent}, which should be an {@link AbstractButton}.
   *
   * @param c The component.
   *
   * @return A new UI capable of drawing the component
   */
  public static ComponentUI createUI(final JComponent c) 
  {
    return new BasicButtonUI();
  }

  /**
   * Returns the default gap between the button's text and icon (in pixels).
   * 
   * @param b  the button (ignored).
   * 
   * @return The gap.
   */
  public int getDefaultTextIconGap(AbstractButton b)
  {
    return defaultTextIconGap;
  }

  /**
   * Sets the text shift offset to zero.
   * 
   * @see #setTextShiftOffset()
   */
  protected void clearTextShiftOffset()
  {
    textShiftOffset = 0;
  }
  
  /**
   * Returns the text shift offset.
   * 
   * @return The text shift offset.
   * 
   * @see #clearTextShiftOffset()
   * @see #setTextShiftOffset()
   */
  protected int getTextShiftOffset()
  {
    return textShiftOffset;
  }

  /**
   * Sets the text shift offset to the value in {@link #defaultTextShiftOffset}.
   * 
   * @see #clearTextShiftOffset()
   */
  protected void setTextShiftOffset()
  {
    textShiftOffset = defaultTextShiftOffset;
  }

  /**
   * Returns the prefix for the UI defaults property for this UI class.
   * This is &apos;Button&apos; for this class.
   *
   * @return the prefix for the UI defaults property
   */
  protected String getPropertyPrefix()
  {
    return "Button.";
  }

  /**
   * Installs the default settings.
   * 
   * @param b  the button (<code>null</code> not permitted).
   */
  protected void installDefaults(AbstractButton b)
  {
    String prefix = getPropertyPrefix();
    LookAndFeel.installColorsAndFont(b, prefix + "background",
                                     prefix + "foreground", prefix + "font");
    LookAndFeel.installBorder(b, prefix + "border");
    b.setMargin(UIManager.getInsets(prefix + "margin"));
    b.setIconTextGap(UIManager.getInt(prefix + "textIconGap"));
    b.setInputMap(JComponent.WHEN_FOCUSED, 
                  (InputMap) UIManager.get(prefix + "focusInputMap"));
    b.setRolloverEnabled(UIManager.getBoolean(prefix + "rollover"));
  }

  /**
   * Removes the defaults added by {@link #installDefaults(AbstractButton)}.
   * 
   * @param b  the button (<code>null</code> not permitted).
   */
  protected void uninstallDefaults(AbstractButton b)
  {
    if (b.getFont() instanceof UIResource)
      b.setFont(null);
    b.setForeground(null);
    b.setBackground(null);
    b.setBorder(null);
    b.setIconTextGap(defaultTextIconGap);
    b.setMargin(null);
  }

  protected BasicButtonListener listener;

  /**
   * Creates and returns a new instance of {@link BasicButtonListener}.  This
   * method provides a hook to make it easy for subclasses to install a 
   * different listener.
   * 
   * @param b  the button.
   * 
   * @return A new listener.
   */
  protected BasicButtonListener createButtonListener(AbstractButton b)
  {
    return new BasicButtonListener(b);
  }

  /**
   * Installs listeners for the button.
   * 
   * @param b  the button (<code>null</code> not permitted).
   */
  protected void installListeners(AbstractButton b)
  {
    listener = createButtonListener(b);
    b.addChangeListener(listener);
    b.addPropertyChangeListener(listener);
    b.addFocusListener(listener);    
    b.addMouseListener(listener);
    b.addMouseMotionListener(listener);
  }

  /**
   * Uninstalls listeners for the button.
   * 
   * @param b  the button (<code>null</code> not permitted).
   */
  protected void uninstallListeners(AbstractButton b)
  {
    b.removeChangeListener(listener);
    b.removePropertyChangeListener(listener);
    b.removeFocusListener(listener);    
    b.removeMouseListener(listener);
    b.removeMouseMotionListener(listener);
  }

  protected void installKeyboardActions(AbstractButton b)
  {
    listener.installKeyboardActions(b);
  }

  protected void uninstallKeyboardActions(AbstractButton b)
  {
    listener.uninstallKeyboardActions(b);
  }

  /**
   * Install the BasicButtonUI as the UI for a particular component.
   * This means registering all the UI's listeners with the component,
   * and setting any properties of the button which are particular to 
   * this look and feel.
   *
   * @param c The component to install the UI into
   */
  public void installUI(final JComponent c) 
  {
    super.installUI(c);
    if (c instanceof AbstractButton)
      {
        AbstractButton b = (AbstractButton) c;
        installDefaults(b);
        installListeners(b);
        installKeyboardActions(b);
      }
  }

  /**
   * Calculate the preferred size of this component, by delegating to
   * {@link BasicGraphicsUtils#getPreferredButtonSize}.
   *
   * @param c The component to measure
   *
   * @return The preferred dimensions of the component
   */
  public Dimension getPreferredSize(JComponent c) 
  {
    AbstractButton b = (AbstractButton)c;
    Dimension d = 
      BasicGraphicsUtils.getPreferredButtonSize
      (b, defaultTextIconGap + defaultTextShiftOffset);
    return d;
  }

  static Icon currentIcon(AbstractButton b)
  {
    Icon i = b.getIcon();
    ButtonModel model = b.getModel();

    if (model.isPressed() && b.getPressedIcon() != null && b.isEnabled())
      i = b.getPressedIcon();

    else if (model.isRollover())
      {
        if (b.isSelected() && b.getRolloverSelectedIcon() != null)
          i = b.getRolloverSelectedIcon();
        else if (b.getRolloverIcon() != null)
          i = b.getRolloverIcon();
      }    

    else if (b.isSelected() && b.isEnabled())
      {
        if (b.isEnabled() && b.getSelectedIcon() != null)
          i = b.getSelectedIcon();
        else if (b.getDisabledSelectedIcon() != null)
          i = b.getDisabledSelectedIcon();
      }

    else if (! b.isEnabled() && b.getDisabledIcon() != null)
      i = b.getDisabledIcon();

    return i;
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
	
    paintIcon(g, c, ir);
    if (text != null)
      paintText(g, b, tr, text);
    if (b.isFocusOwner() && b.isFocusPainted())
      paintFocus(g, b, vr, tr, ir);
  }

  /**
   * Paint any focus decoration this {@link JComponent} might have.  The
   * component, which in this case will be an {@link AbstractButton},
   * should only have focus decoration painted if it has the focus, and its
   * "focusPainted" property is <code>true</code>.
   *
   * @param g Graphics context to paint with
   * @param b Button to paint the focus of
   * @param vr Visible rectangle, the area in which to paint
   * @param tr Text rectangle, contained in visible rectangle
   * @param ir Icon rectangle, contained in visible rectangle
   *
   * @see AbstractButton#isFocusPainted()
   * @see JComponent#hasFocus()
   */
  protected void paintFocus(Graphics g, AbstractButton b, Rectangle vr,
                            Rectangle tr, Rectangle ir)
  {
    // In the BasicLookAndFeel no focus border is drawn. This can be
    // overridden in subclasses to implement such behaviour.
  }

  /**
   * Paint the icon for this component. Depending on the state of the
   * component and the availability of the button's various icon
   * properties, this might mean painting one of several different icons.
   *
   * @param g Graphics context to paint with
   * @param c Component to paint the icon of
   * @param iconRect Rectangle in which the icon should be painted
   */
  protected void paintIcon(Graphics g, JComponent c, Rectangle iconRect)
  {
    AbstractButton b = (AbstractButton) c;
    Icon i = currentIcon(b);

    if (i != null)
      i.paintIcon(c, g, iconRect.x, iconRect.y);
  }

  /**
   * Paints the background area of an {@link AbstractButton} in the pressed
   * state.  This means filling the supplied area with a darker than normal 
   * background.
   *
   * @param g The graphics context to paint with
   * @param b The button to paint the state of
   */
  protected void paintButtonPressed(Graphics g, AbstractButton b)
  {
    if (b.isContentAreaFilled() && b.isOpaque())
      {
        Rectangle area = new Rectangle();
        SwingUtilities.calculateInnerArea(b, area);
        g.setColor(UIManager.getColor(getPropertyPrefix() + "shadow"));
        g.fillRect(area.x, area.y, area.width, area.height);
      }
  }
    
  /**
   * Paints the "text" property of an {@link AbstractButton}.
   *
   * @param g The graphics context to paint with
   * @param c The component to paint the state of
   * @param textRect The area in which to paint the text
   * @param text The text to paint
   */
  protected void paintText(Graphics g, JComponent c, Rectangle textRect,
                           String text) 
  {	
    paintText(g, (AbstractButton) c, textRect, text);
  }

  /**
   * Paints the "text" property of an {@link AbstractButton}.
   *
   * @param g The graphics context to paint with
   * @param b The button to paint the state of
   * @param textRect The area in which to paint the text
   * @param text The text to paint
   *
   * @since 1.4
   */
  protected void paintText(Graphics g, AbstractButton b, Rectangle textRect,
			   String text)
  {
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
        UIDefaults defaults = UIManager.getLookAndFeelDefaults();
        String prefix = getPropertyPrefix();
        g.setColor(defaults.getColor(prefix + "disabledText"));
        g.drawString(text, textRect.x, textRect.y + fm.getAscent());
      }
  } 
}
