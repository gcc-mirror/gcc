/* MetalToolTipUI.java
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
import java.awt.Font;
import java.awt.FontMetrics;
import java.awt.Graphics;
import java.awt.Insets;
import java.awt.Rectangle;
import java.awt.Toolkit;
import java.awt.event.InputEvent;
import java.awt.event.KeyEvent;

import javax.swing.AbstractButton;
import javax.swing.JComponent;
import javax.swing.JMenuItem;
import javax.swing.JToolTip;
import javax.swing.KeyStroke;
import javax.swing.SwingConstants;
import javax.swing.SwingUtilities;
import javax.swing.UIManager;
import javax.swing.border.Border;
import javax.swing.plaf.ComponentUI;
import javax.swing.plaf.UIResource;
import javax.swing.plaf.basic.BasicToolTipUI;

/**
 * A UI delegate for the {@link JToolTip} component.
 */
public class MetalToolTipUI
  extends BasicToolTipUI
{
  /** 
   * The amount of space between the tool tip text and the accelerator 
   * description (if visible). 
   */
  public static final int padSpaceBetweenStrings = 12;

  /** The shared UI instance. */
  private static MetalToolTipUI instance = null;
  
  /** A flag controlling the visibility of the accelerator (if there is one). */
  private boolean isAcceleratorHidden;
  
  /** A string representing the accelerator key for the component. */
  private String acceleratorString;
  
  /** 
   * The delimiter for the accelerator string.
   */
  private String acceleratorDelimiter;
  
  /** The font for the accelerator string. */
  private Font acceleratorFont;
  
  /** The color for the accelerator string. */
  private Color acceleratorForeground;
  
  /** The active border. */
  private Border activeBorder;
  
  /** The inactive border. */
  private Border inactiveBorder;
  
  /**
   * Constructs a new instance of <code>MetalToolTipUI</code>.
   */
  public MetalToolTipUI()
  {
    super();
    activeBorder = UIManager.getBorder("ToolTip.border");
    inactiveBorder = UIManager.getBorder("ToolTip.borderInactive");
    isAcceleratorHidden = UIManager.getBoolean("ToolTip.hideAccelerator");
    acceleratorFont = UIManager.getFont("MenuItem.acceleratorFont");
    acceleratorForeground = UIManager.getColor("MenuItem.acceleratorForeground");
    acceleratorDelimiter = UIManager.getString("MenuItem.acceleratorDelimiter");
  }

  /**
   * Returns a shared instance of the <code>MetalToolTipUI</code> class.
   * Although this UI delegate does maintain state information, there is never
   * more than one tool tip visible, so it is OK to use a shared instance.
   *
   * @param component  the component (a {@link JToolTip}).
   *
   * @return A shared instance of the <code>MetalToolTipUI</code> class.
   */
  public static ComponentUI createUI(JComponent component)
  {
    if (instance == null)
      instance = new MetalToolTipUI();
    return instance;
  }
  
  /**
   * Returns a string representing the accelerator key (if there is one) for 
   * the component that the tool tip belongs to.
   * 
   * @return A string representing the accelerator key.
   */
  public String getAcceleratorString()
  {
    return acceleratorString;   
  }
  
  /**
   * Installs the UI for the specified component (a {@link JToolTip}).
   * 
   * @param c  the {@link JToolTip} component.
   */
  public void installUI(JComponent c)
  {
    super.installUI(c);
    Border existingBorder = c.getBorder();
    if (existingBorder == null || existingBorder instanceof UIResource)
      {
        if (c.isEnabled())
          c.setBorder(activeBorder);
        else
          c.setBorder(inactiveBorder);
      }   
  }
  
  /**
   * Clears the defaults set in {@link #installUI(JComponent)}.
   * 
   * @param c  the component.
   */
  public void uninstallUI(JComponent c)
  {
    super.uninstallUI(c);
    if (c.getBorder() instanceof UIResource)
      c.setBorder(null);
  }
  
  /**
   * Returns <code>true</code> if the accelerator string is hidden, and
   * <code>false</code> otherwise.  This setting is controlled by the
   * <code>ToolTip.hideAccelerator</code> entry in the UI defaults table.
   *
   * @return A boolean.
   */
  protected boolean isAcceleratorHidden()
  {
    return isAcceleratorHidden;
  }
  
  /**
   * Returns the preferred size for the {@link JToolTip} component.
   * 
   * @param c  the component (a {@link JToolTip}).
   * 
   * @return The preferred size.
   */
  public Dimension getPreferredSize(JComponent c)
  {
    if (isAcceleratorHidden())
      return super.getPreferredSize(c);
    else
      {
        Insets insets = c.getInsets();
        JToolTip tt = (JToolTip) c;
        String tipText = tt.getTipText();
        if (tipText != null)
          {
            FontMetrics fm = c.getFontMetrics(c.getFont());
            int prefH = fm.getHeight() + insets.top + insets.bottom;
            int prefW = fm.stringWidth(tipText) + insets.left + insets.right;

            // this seems to be the first opportunity we have to get the 
            // accelerator string from the component (if it has one)
            acceleratorString = fetchAcceleratorString(c);
            if (acceleratorString != null)
              {
                prefW += padSpaceBetweenStrings;
                fm = c.getFontMetrics(acceleratorFont);
                prefW += fm.stringWidth(acceleratorString);                
              }
            return new Dimension(prefW, prefH);  
          }
        else return new Dimension(0, 0);
      }
  }
  
  /**
   * Paints the tool tip.
   * 
   * @param g  the graphics context.
   * @param c  the {@link JToolTip} component.
   */
  public void paint(Graphics g, JComponent c)
  {
    JToolTip tip = (JToolTip) c;

    String text = tip.getTipText();
    Toolkit t = tip.getToolkit();
    if (text == null)
      return;

    Rectangle vr = new Rectangle();
    vr = SwingUtilities.calculateInnerArea(tip, vr);
    Rectangle ir = new Rectangle();
    Rectangle tr = new Rectangle();
    FontMetrics fm = t.getFontMetrics(tip.getFont());
    int ascent = fm.getAscent();
    SwingUtilities.layoutCompoundLabel(tip, fm, text, null, 
            SwingConstants.CENTER, SwingConstants.LEFT,
            SwingConstants.CENTER, SwingConstants.CENTER, vr, ir, tr, 0);
    Color saved = g.getColor();
    g.setColor(Color.BLACK);

    g.drawString(text, vr.x, vr.y + ascent); 
    
    // paint accelerator
    if (acceleratorString != null)
      {
        g.setFont(acceleratorFont);
        g.setColor(acceleratorForeground);
        fm = t.getFontMetrics(acceleratorFont);
        int width = fm.stringWidth(acceleratorString);
        g.drawString(acceleratorString, vr.x + vr.width - width - padSpaceBetweenStrings/2, 
                vr.y + vr.height - fm.getDescent());
      }

    g.setColor(saved);   
  }
  
  /**
   * Returns a string representing the accelerator for the component, or 
   * <code>null</code> if the component has no accelerator.
   * 
   * @param c  the component.
   * 
   * @return A string representing the accelerator (possibly 
   *         <code>null</code>).
   */
  private String fetchAcceleratorString(JComponent c)
  {
    String result = null;
    if (c instanceof JToolTip)
      {
        JToolTip toolTip = (JToolTip) c;
        JComponent component = toolTip.getComponent();
        KeyStroke ks = null;
        int mne = 0;
        if (component instanceof JMenuItem)
          {
            JMenuItem item = (JMenuItem) component;
            ks = item.getAccelerator();
            if (ks == null)
                mne = item.getMnemonic();
          }
        else if (component instanceof AbstractButton)
          {
            AbstractButton button = (AbstractButton) component;
            mne = button.getMnemonic();
          }
        if (mne > 0)
          ks = KeyStroke.getKeyStroke(Character.toUpperCase((char) mne), 
                InputEvent.ALT_MASK, false);
        if (ks != null)
          result = acceleratorToString(ks);
      }
    return result;
  }
  
  /**
   * Returns a string representing an accelerator.
   * 
   * @param accelerator  the accelerator (<code>null</code> not permitted).
   * 
   * @return A string representing an accelerator.
   */
  private String acceleratorToString(KeyStroke accelerator)
  {
    // convert keystroke into string format
    String modifiersText = "";
    int modifiers = accelerator.getModifiers();
    char keyChar = accelerator.getKeyChar();
    int keyCode = accelerator.getKeyCode();
    
    if (modifiers != 0)
      modifiersText = KeyEvent.getKeyModifiersText(modifiers) 
          + acceleratorDelimiter;

    if (keyCode == KeyEvent.VK_UNDEFINED)
      return modifiersText + keyChar;
    else
      return modifiersText + KeyEvent.getKeyText(keyCode);
  }

}
