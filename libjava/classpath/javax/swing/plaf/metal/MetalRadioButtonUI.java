/* MetalRadioButtonUI.java
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
import java.awt.Dimension;
import java.awt.Graphics;
import java.awt.Rectangle;

import javax.swing.AbstractButton;
import javax.swing.JComponent;
import javax.swing.JRadioButton;
import javax.swing.UIManager;
import javax.swing.plaf.ComponentUI;
import javax.swing.plaf.basic.BasicRadioButtonUI;


/**
 * A UI delegate for the {@link JRadioButton} component.
 */
public class MetalRadioButtonUI
  extends BasicRadioButtonUI
{

  /** Used to draw the focus rectangle. */
  protected Color focusColor;
  
  /** Used to fill the icon when the button is pressed. */
  protected Color selectColor;
  
  /** Used to draw disabled text. */
  protected Color disabledTextColor;
  
  /**
   * Constructs a new instance of <code>MetalRadioButtonUI</code>.
   */
  public MetalRadioButtonUI()
  {
    super();
  }

  /**
   * Returns a new instance of <code>MetalRadioButtonUI</code>.
   *
   * @param component the component for which we return an UI instance
   *
   * @return A new instance of <code>MetalRadioButtonUI</code>.
   */
  public static ComponentUI createUI(JComponent component)
  {
    return new MetalRadioButtonUI();
  }
  
  /**
   * Sets the default values for the specified button.
   * 
   * @param b  the button.
   */
  public void installDefaults(AbstractButton b)
  {
    super.installDefaults(b);
    disabledTextColor = UIManager.getColor("RadioButton.disabledText");
    focusColor = UIManager.getColor("RadioButton.focus");
    selectColor = UIManager.getColor("RadioButton.select");
  }
  
  /**
   * Clears any defaults set in the installDefaults() method.
   * 
   * @param b  the {@link JRadioButton}.
   */
  protected void uninstallDefaults(AbstractButton b)
  {
    super.uninstallDefaults(b);
    disabledTextColor = null;
    focusColor = null;
    selectColor = null;
  }
  
  /**
   * Returns the color used to fill the {@link JRadioButton}'s icon when the
   * button is pressed.  The default color is obtained from the 
   * {@link UIManager} defaults via an entry with the key 
   * <code>RadioButton.select</code>.
   * 
   * @return The select color.
   */
  protected Color getSelectColor()
  {
    return selectColor;    
  }
  
  /**
   * Returns the color for the {@link JRadioButton}'s text when the button is
   * disabled.  The default color is obtained from the {@link UIManager}
   * defaults via an entry with the key <code>RadioButton.disabledText</code>.
   * 
   * @return The disabled text color.
   */
  protected Color getDisabledTextColor()
  {
    return disabledTextColor;
  }
  
  /**
   * Returns the color used to draw the focus rectangle when the 
   * {@link JRadioButton} has the focus.  The default color is obtained from 
   * the {@link UIManager} defaults via an entry with the key 
   * <code>RadioButton.focus</code>.
   * 
   * @return The color used to draw the focus rectangle.
   * 
   * @see #paintFocus(Graphics, Rectangle, Dimension)
   */
  protected Color getFocusColor()
  {
    return focusColor;
  }
  
  /**
   * Paints the {@link JRadioButton}.
   * 
   * @param g  the graphics device.
   * @param c  the component (an instance of {@link JRadioButton}).
   */
  public void paint(Graphics g, JComponent c)
  {
    super.paint(g, c);
    // FIXME:  disabled text isn't being drawn correctly, it's possible that
    // it could be done here...
  }
  
  /**
   * Paints the focus rectangle for the {@link JRadioButton}.
   * 
   * @param g  the graphics device.
   * @param t  the bounding rectangle for the text.
   * @param d  ???
   */
  protected void paintFocus(Graphics g, Rectangle t, Dimension d)
  {
    g.setColor(focusColor);
    g.drawRect(t.x - 1, t.y + 2, t.width + 2, t.height - 4);
  }
  
}
