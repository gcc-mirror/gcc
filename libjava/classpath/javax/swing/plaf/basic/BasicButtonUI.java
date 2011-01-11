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
import java.awt.Insets;
import java.awt.Rectangle;
import java.beans.PropertyChangeEvent;
import java.beans.PropertyChangeListener;

import javax.swing.AbstractButton;
import javax.swing.ButtonModel;
import javax.swing.Icon;
import javax.swing.JButton;
import javax.swing.JComponent;
import javax.swing.LookAndFeel;
import javax.swing.SwingUtilities;
import javax.swing.UIManager;
import javax.swing.plaf.ButtonUI;
import javax.swing.plaf.ComponentUI;
import javax.swing.plaf.UIResource;
import javax.swing.text.View;

/**
 * A UI delegate for the {@link JButton} component.
 */
public class BasicButtonUI extends ButtonUI
{
  /**
   * Cached rectangle for layouting the label. Used in paint() and
   * BasicGraphicsUtils.getPreferredButtonSize().
   */
  static Rectangle viewR = new Rectangle();

  /**
   * Cached rectangle for layouting the label. Used in paint() and
   * BasicGraphicsUtils.getPreferredButtonSize().
   */
  static Rectangle iconR = new Rectangle();

  /**
   * Cached rectangle for layouting the label. Used in paint() and
   * BasicGraphicsUtils.getPreferredButtonSize().
   */
  static Rectangle textR = new Rectangle();

  /**
   * Cached Insets instance, used in paint().
   */
  static Insets cachedInsets;

  /**
   * The shared button UI.
   */
  private static BasicButtonUI sharedUI;

  /**
   * The shared BasicButtonListener.
   */
  private static BasicButtonListener sharedListener;

  /**
   * A constant used to pad out elements in the button's layout and
   * preferred size calculations.
   */
  protected int defaultTextIconGap = 4;

  /**
   * A constant added to the defaultTextIconGap to adjust the text
   * within this particular button.
   */
  protected int defaultTextShiftOffset;

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
    if (sharedUI == null)
      sharedUI = new BasicButtonUI();
    return sharedUI;
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
    // Install colors and font.
    LookAndFeel.installColorsAndFont(b, prefix + "background",
                                     prefix + "foreground", prefix + "font");
    // Install border.
    LookAndFeel.installBorder(b, prefix + "border");

    // Install margin property.
    if (b.getMargin() == null || b.getMargin() instanceof UIResource)
      b.setMargin(UIManager.getInsets(prefix + "margin"));

    // Install rollover property.
    Object rollover = UIManager.get(prefix + "rollover");
    if (rollover != null)
      LookAndFeel.installProperty(b, "rolloverEnabled", rollover);

    // Fetch default textShiftOffset.
    defaultTextShiftOffset = UIManager.getInt(prefix + "textShiftOffset");

    // Make button opaque if needed.
    if (b.isContentAreaFilled())
      LookAndFeel.installProperty(b, "opaque", Boolean.TRUE);
    else
      LookAndFeel.installProperty(b, "opaque", Boolean.FALSE);
  }

  /**
   * Removes the defaults added by {@link #installDefaults(AbstractButton)}.
   *
   * @param b  the button (<code>null</code> not permitted).
   */
  protected void uninstallDefaults(AbstractButton b)
  {
    // The other properties aren't uninstallable.
    LookAndFeel.uninstallBorder(b);
  }

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
    // Note: The RI always returns a new instance here. However,
    // the BasicButtonListener class is perfectly suitable to be shared
    // between multiple buttons, so we return a shared instance here
    // for efficiency.
    if (sharedListener == null)
      sharedListener = new BasicButtonListener(b);
    return sharedListener;
  }

  /**
   * Installs listeners for the button.
   *
   * @param b  the button (<code>null</code> not permitted).
   */
  protected void installListeners(AbstractButton b)
  {
    BasicButtonListener listener = createButtonListener(b);
    if (listener != null)
      {
        b.addChangeListener(listener);
        b.addPropertyChangeListener(listener);
        b.addFocusListener(listener);
        b.addMouseListener(listener);
        b.addMouseMotionListener(listener);
      }
    // Fire synthetic property change event to let the listener update
    // the TextLayout cache.
    listener.propertyChange(new PropertyChangeEvent(b, "font", null,
                                                    b.getFont()));
  }

  /**
   * Uninstalls listeners for the button.
   *
   * @param b  the button (<code>null</code> not permitted).
   */
  protected void uninstallListeners(AbstractButton b)
  {
    BasicButtonListener listener = getButtonListener(b);
    if (listener != null)
      {
        b.removeChangeListener(listener);
        b.removePropertyChangeListener(listener);
        b.removeFocusListener(listener);
        b.removeMouseListener(listener);
        b.removeMouseMotionListener(listener);
      }
  }

  protected void installKeyboardActions(AbstractButton b)
  {
    BasicButtonListener listener = getButtonListener(b);
    if (listener != null)
      listener.installKeyboardActions(b);
  }

  protected void uninstallKeyboardActions(AbstractButton b)
  {
    BasicButtonListener listener = getButtonListener(b);
    if (listener != null)
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
        // It is important to install the listeners before installing
        // the keyboard actions, because the keyboard actions
        // are actually installed on the listener instance.
        installListeners(b);
        installKeyboardActions(b);
        BasicHTML.updateRenderer(b, b.getText());
      }
  }

  /**
   * Uninstalls the UI from the component.
   *
   * @param c the component from which to uninstall the UI
   */
  public void uninstallUI(JComponent c)
  {
    if (c instanceof AbstractButton)
      {
        AbstractButton b = (AbstractButton) c;
        uninstallKeyboardActions(b);
        uninstallListeners(b);
        uninstallDefaults(b);
        BasicHTML.updateRenderer(b, "");
        b.putClientProperty(BasicGraphicsUtils.CACHED_TEXT_LAYOUT, null);
      }
  }

  /**
   * Calculates the minimum size for the specified component.
   *
   * @param c the component for which to compute the minimum size
   *
   * @return the minimum size for the specified component
   */
  public Dimension getMinimumSize(JComponent c)
  {
    Dimension size = getPreferredSize(c);
    // When the HTML view has a minimum width different from the preferred
    // width, then substract this here accordingly. The height is not
    // affected by that.
    View html = (View) c.getClientProperty(BasicHTML.propertyKey);
    if (html != null)
      {
        size.width -= html.getPreferredSpan(View.X_AXIS)
                      - html.getPreferredSpan(View.X_AXIS);
      }
    return size;
  }

  /**
   * Calculates the maximum size for the specified component.
   *
   * @param c the component for which to compute the maximum size
   *
   * @return the maximum size for the specified component
   */
  public Dimension getMaximumSize(JComponent c)
  {
    Dimension size = getPreferredSize(c);
    // When the HTML view has a maximum width different from the preferred
    // width, then add this here accordingly. The height is not
    // affected by that.
    View html = (View) c.getClientProperty(BasicHTML.propertyKey);
    if (html != null)
      {
        size.width += html.getMaximumSpan(View.X_AXIS)
                      - html.getPreferredSpan(View.X_AXIS);
      }
    return size;
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
    AbstractButton b = (AbstractButton) c;
    Dimension d = BasicGraphicsUtils.getPreferredButtonSize(b,
                                                           b.getIconTextGap());
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

    Insets i = c.getInsets(cachedInsets);
    viewR.x = i.left;
    viewR.y = i.top;
    viewR.width = c.getWidth() - i.left - i.right;
    viewR.height = c.getHeight() - i.top - i.bottom;
    textR.x = 0;
    textR.y = 0;
    textR.width = 0;
    textR.height = 0;
    iconR.x = 0;
    iconR.y = 0;
    iconR.width = 0;
    iconR.height = 0;

    Font f = c.getFont();
    g.setFont(f);
    Icon icon = b.getIcon();
    String text = b.getText();
    text = SwingUtilities.layoutCompoundLabel(c, g.getFontMetrics(f),
                                              text, icon,
                                              b.getVerticalAlignment(),
                                              b.getHorizontalAlignment(),
                                              b.getVerticalTextPosition(),
                                              b.getHorizontalTextPosition(),
                                              viewR, iconR, textR,
                                              text == null ? 0
                                                         : b.getIconTextGap());

    ButtonModel model = b.getModel();
    if (model.isArmed() && model.isPressed())
      paintButtonPressed(g, b);

    if (icon != null)
      paintIcon(g, c, iconR);
    if (text != null)
      {
        View html = (View) b.getClientProperty(BasicHTML.propertyKey);
        if (html != null)
          html.paint(g, textR);
        else
          paintText(g, b, textR, text);
      }
    if (b.isFocusOwner() && b.isFocusPainted())
      paintFocus(g, b, viewR, textR, iconR);
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
      {
        ButtonModel m = b.getModel();
        if (m.isPressed() && m.isArmed())
          {
            int offs = getTextShiftOffset();
            i.paintIcon(c, g, iconRect.x + offs, iconRect.y + offs);
          }
        else
          i.paintIcon(c, g, iconRect.x, iconRect.y);
      }
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
    AbstractButton b = (AbstractButton) c;
    Font f = b.getFont();
    g.setFont(f);
    FontMetrics fm = g.getFontMetrics(f);

    if (b.isEnabled())
      {
        g.setColor(b.getForeground());
        // FIXME: Underline mnemonic.
        BasicGraphicsUtils.drawString(b, g, text, -1, textRect.x,
                                      textRect.y + fm.getAscent());
      }
    else
      {
        String prefix = getPropertyPrefix();
        g.setColor(UIManager.getColor(prefix + "disabledText"));
        // FIXME: Underline mnemonic.
        BasicGraphicsUtils.drawString(b, g, text, -1, textRect.x,
                                      textRect.y + fm.getAscent());
      }
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
    paintText(g, (JComponent) b, textRect, text);
  }

  /**
   * A helper method that finds the BasicButtonListener for the specified
   * button. This is there because this UI class is stateless and
   * shared for all buttons, and thus can't store the listener
   * as instance field. (We store our shared instance in sharedListener,
   * however, subclasses may override createButtonListener() and we would
   * be lost in this case).
   *
   * @param b the button
   *
   * @return the UI event listener
   */
  private BasicButtonListener getButtonListener(AbstractButton b)
  {
    // The listener gets installed as PropertyChangeListener,
    // so look for it in the list of property change listeners.
    PropertyChangeListener[] listeners = b.getPropertyChangeListeners();
    BasicButtonListener l = null;
    for (int i = 0; listeners != null && l == null && i < listeners.length;
           i++)
      {
        if (listeners[i] instanceof BasicButtonListener)
          l = (BasicButtonListener) listeners[i];
      }
    return l;
  }
}
