/* BasicButtonUI.java
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

package javax.swing.plaf.basic;

import java.awt.BasicStroke;
import java.awt.Color;
import java.awt.Dimension;
import java.awt.Font;
import java.awt.FontMetrics;
import java.awt.Graphics;
import java.awt.Graphics2D;
import java.awt.Stroke;
import java.awt.Insets;
import java.awt.Rectangle;
import java.awt.event.FocusAdapter;
import java.awt.event.FocusEvent;
import java.awt.event.InputEvent;
import java.awt.event.MouseAdapter;
import java.awt.event.MouseEvent;
import javax.swing.AbstractButton;
import javax.swing.JComponent;
import javax.swing.SwingUtilities;
import javax.swing.plaf.ButtonUI;
import javax.swing.plaf.ComponentUI;

public class BasicButtonUI extends ButtonUI
{
  /** A constant used to pad out elements in the button's layout and
      preferred size calculations. */
    int gap = 3;

  /** The color that text will be painted when the button is enabled */
  Color textColor;

  /** The color that text will be painted when the button is disabled */
  Color disabledTextColor;

  /** The color that the button's background will be painted when the
      button is pressed. */
    Color pressedBackgroundColor;

  /** The color that the button's background will be painted when the
      button is not pressed. */
  Color normalBackgroundColor;

  /**
   * Factory method to create an instance of BasicButtonUI for a given
   * {@link JComponent}, which should be an {@link AbstractButton}.
   *
   * @param c The component to create a UI got
   *
   * @return A new UI capable of drawing the component
   */
    public static ComponentUI createUI(final JComponent c) 
    {
	return new BasicButtonUI();
    }

  /**
   * Helper class which listens to a button's focus events and disarms the
   * button's model when focus is lost.
   */
  private static class FocusUIListener extends FocusAdapter
  {
    /** Button to listen to focus events from */
    AbstractButton button;

    /**
     * Creates a new FocusUIListener object.
     *
     * @param b The button to listen to
     */
    FocusUIListener(AbstractButton b)
    {
      button = b;
    }
    
    /**
     * Called when the button loses focus.
     *
     * @param event The loss of focus event.
     */
    public void focusLost(FocusEvent event)
    {
      // System.err.println("ButtonUI :: lost focus -- disarming");
      button.getModel().setArmed(false);
    }
  }

  /**
   * A helper class which interprets mouse events as 
   * state changes to the button's underlying model.
   */
  private static class ButtonUIListener extends MouseAdapter
  {
    /** The button to change the model of */
    AbstractButton button;

    /**
     * Creates a new ButtonUIListener object.
     *
     * @param b The button to change the model of
     */
    public ButtonUIListener(AbstractButton b)
    {
      button = b;
    }

    /**
     * Accept a mouse press event and arm the button's model.
     *
     * @param e The mouse press event to accept
     */
    public void mousePressed(MouseEvent e)
    {
      // System.err.println("ButtonUI :: mouse pressed");
      if ((e.getModifiers() & InputEvent.BUTTON1_MASK) != 0)
        {
          // System.err.println("ButtonUI :: arming");
          button.getModel().setArmed(true);
        }
    }

    /**
     * Accept a mouse enter event and set the button's model's
     * "rollover" property to <code>true</code>. If the button's
     * model is currently armed and the mouse button is not held
     * down, this enter event will also disarm the model.
     *
     * @param e The mouse enter event to accept
     */
    public void mouseEntered(MouseEvent e)
    {
      // System.err.println("ButtonUI :: mouse entered");
      // System.err.println("ButtonUI :: rolling over");
      button.getModel().setRollover(true);
      if (button.getModel().isArmed() 
          && (e.getModifiers() & InputEvent.BUTTON1_MASK) == 0)
        {
          // System.err.println("ButtonUI :: no button pressed -- disarming");
          button.getModel().setArmed(false);
        }
    }

    /**
     * Accept a mouse exit event and set the button's model's
     * "rollover" property to <code>false</code>.
     *
     * @param e The mouse exit event to accept
     */
    public void mouseExited(MouseEvent e)
    {
      // System.err.println("ButtonUI :: mouse exited");
      button.getModel().setRollover(false);
    }

    /**
     * Accept a mouse release event and set the button's model's
     * "pressed" property to <code>true</code>, if the model
     * is armed. If the model is not armed, ignore the event.
     *
     * @param e The mouse release event to accept
     */
    public void mouseReleased(MouseEvent e)
    {
      // System.err.println("ButtonUI :: mouse released");
      if (button.getModel().isArmed()
          && (e.getModifiers() & InputEvent.BUTTON1_MASK) != 0)
        {
          button.getModel().setPressed(true);
        }
    }
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

	textColor                = new Color(0,0,0);
	disabledTextColor        = new Color(130, 130, 130);
	pressedBackgroundColor   = new Color(150,150,150);
	pressedBackgroundColor   = new Color(150,150,150);
	normalBackgroundColor    = new Color(192,192,192);

    // this tells the border (if we have one) how to paint.
    c.setBackground(normalBackgroundColor);
    ((AbstractButton)c).setMargin (new Insets(10,10,10,10));

    c.addMouseListener(new ButtonUIListener((AbstractButton) c));
    c.addFocusListener(new FocusUIListener((AbstractButton) c));
  }

  /**
   * Calculate the preferred size of this component, by delegating to
   * {@link BasicGraphicsUtils.getPreferredButtonSize}.
   *
   * @param c The component to measure
   *
   * @return The preferred dimensions of the component
   */
    public Dimension getPreferredSize(JComponent c) 
    {
	AbstractButton b = (AbstractButton)c;
	Dimension d = BasicGraphicsUtils.getPreferredButtonSize(b, gap);
	return d;
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
    Rectangle br = new Rectangle();

        Font f = c.getFont();

        g.setFont(f);

        FontMetrics fm = g.getFontMetrics(f);

    Insets border = b.getInsets();
    Insets margin = b.getMargin();

    br.x = border.left;
    br.y = border.top;
    br.width = b.getWidth() - (border.right + border.left);
    br.height = b.getHeight() - (border.top + border.bottom);

    vr.x = br.x + margin.left;
    vr.y = br.y + margin.top;
    vr.width = br.width - (margin.right + margin.left);
    vr.height = br.height - (margin.top + margin.bottom);

    String text = SwingUtilities.layoutCompoundLabel(c, fm, b.getText(),
							 b.getIcon(),
							 b.getVerticalAlignment(), 
							 b.getHorizontalAlignment(),
							 b.getVerticalTextPosition(), 
							 b.getHorizontalTextPosition(),
                                                     vr, ir, tr, gap);

    if ((b.getModel().isRollover() && b.getModel().isArmed()) 
        || b.getModel().isSelected())
      paintButtonPressed(g, br, c);
	else
      paintButtonNormal(g, br, c);
	
	paintIcon(g, c, ir);
	paintText(g, c, tr, b.getText());
	paintFocus(g, c, vr, tr, ir);
    }

  /**
   * Paint any focus decoration this {@link JComponent} might have.  The
   * component, which in this case will be an {@link AbstractButton},
   * should only have focus decoration painted if it has the focus, and its
   * "focusPainted" property is <code>true</code>.
   *
   * @param g Graphics context to paint with
   * @param c Component to paint the focus of
   * @param vr Visible rectangle, the area in which to paint
   * @param tr Text rectangle, contained in visible rectangle
   * @param ir Icon rectangle, contained in visible rectangle
   *
   * @see AbstractButton.isFocusPainted()
   * @see JComponent.hasFocus()
   */
  protected void paintFocus(Graphics g, JComponent c, Rectangle vr,
                            Rectangle tr, Rectangle ir)
  {
    AbstractButton b = (AbstractButton) c;
    if (b.hasFocus() && b.isFocusPainted())
    {
        Graphics2D g2 = (Graphics2D) g;
        Stroke saved_stroke = g2.getStroke();
        Color saved_color = g2.getColor();
        float dashes[] = new float[] {1.0f, 1.0f};        
        BasicStroke s = new BasicStroke(1.0f, 
                                        BasicStroke.CAP_SQUARE, 
                                        BasicStroke.JOIN_MITER,
                                        10, dashes, 0.0f);
        g2.setStroke(s);
        g2.setColor(Color.BLACK);
        g2.drawRect(vr.x + 2, 
                    vr.y + 2, 
                    vr.width - 4,
                    vr.height - 4);
        g2.setStroke(saved_stroke);
        g2.setColor(saved_color);
      }
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
	if (b.getIcon() != null)
	    {
		int x = iconRect.x;
		int y = iconRect.y;
		b.getIcon().paintIcon(c, g, x, y);
	    }
    }

  /**
   * Paints the background area of an {@link AbstractButton} in the pressed
   * state.  This means filling the supplied area with the {@link
   * pressedBackgroundColor}.
   *
   * @param g The graphics context to paint with
   * @param area The area in which to paint
   * @param b The component to paint the state of
   */
  protected void paintButtonPressed(Graphics g, Rectangle area, JComponent b)
    {
	Dimension size = b.getSize();
	
	g.setColor(pressedBackgroundColor);
    g.fillRect(area.x, area.y, area.width, area.height);
    }
    
  /**
   * Paints the background area of an {@link AbstractButton} in the normal,
   * non-pressed state.  This means filling the supplied area with the
   * {@link normalBackgroundColor}.
   *
   * @param g The graphics context to paint with
   * @param area The area in which to paint
   * @param b The component to paint the state of
   */
  protected void paintButtonNormal(Graphics g, Rectangle area, JComponent b)
    {
	Dimension size = b.getSize();
	g.setColor(normalBackgroundColor);
    g.fillRect(area.x, area.y, area.width, area.height);
    }
    
  /**
   * Paints the "text" property of an {@link AbstractButton}, using the
   * {@link textColor} color.
   *
   * @param g The graphics context to paint with
   * @param c The component to paint the state of
   * @param textRect The area in which to paint the text
   * @param text The text to paint
   */
  protected void paintText(Graphics g, JComponent c, Rectangle textRect,
			     String text) 
    {	
	Font f = c.getFont();
        g.setFont(f);
        FontMetrics fm = g.getFontMetrics(f);
	g.setColor(c.isEnabled() ? textColor : disabledTextColor);
    BasicGraphicsUtils.drawString(g, text, 
				      0,
				      textRect.x, 
                                  textRect.y + fm.getAscent());
    } 
}
