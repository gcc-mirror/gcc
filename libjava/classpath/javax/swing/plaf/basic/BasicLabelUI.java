/* BasicLabelUI.java
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

import java.awt.Component;
import java.awt.Dimension;
import java.awt.Font;
import java.awt.FontMetrics;
import java.awt.Graphics;
import java.awt.Insets;
import java.awt.Rectangle;
import java.awt.Toolkit;
import java.awt.event.ActionEvent;
import java.awt.event.KeyEvent;
import java.beans.PropertyChangeEvent;
import java.beans.PropertyChangeListener;

import javax.swing.AbstractAction;
import javax.swing.ActionMap;
import javax.swing.Icon;
import javax.swing.InputMap;
import javax.swing.JComponent;
import javax.swing.JLabel;
import javax.swing.KeyStroke;
import javax.swing.LookAndFeel;
import javax.swing.SwingUtilities;
import javax.swing.plaf.ComponentUI;
import javax.swing.plaf.LabelUI;
import javax.swing.text.View;

/**
 * This is the Basic Look and Feel class for the JLabel.  One BasicLabelUI
 * object is used to paint all JLabels that utilize the Basic Look and Feel.
 */
public class BasicLabelUI extends LabelUI implements PropertyChangeListener
{
  /** The labelUI that is shared by all labels. */
  protected static BasicLabelUI labelUI;

  /**
   * These fields hold the rectangles for the whole label,
   * the icon and the text.
   */
  private Rectangle vr;
  private Rectangle ir;
  private Rectangle tr;

  /**
   * A cached Insets object for reuse in the label layout methods.
   */
  private Insets cachedInsets;

  /**
   * Creates a new BasicLabelUI object.
   */
  public BasicLabelUI()
  {
    super();
    vr = new Rectangle();
    ir = new Rectangle();
    tr = new Rectangle();
  }

  /**
   * Creates and returns a UI for the label. Since one UI is shared by  all
   * labels, this means creating only if necessary and returning the  shared
   * UI.
   *
   * @param c The {@link JComponent} that a UI is being created for.
   *
   * @return A label UI for the Basic Look and Feel.
   */
  public static ComponentUI createUI(JComponent c)
  {
    if (labelUI == null)
      labelUI = new BasicLabelUI();
    return labelUI;
  }

  /**
   * Returns the preferred size of this component as calculated by the
   * {@link #layoutCL(JLabel, FontMetrics, String, Icon, Rectangle, Rectangle, 
   * Rectangle)} method.
   *
   * @param c This {@link JComponent} to get a preferred size for.
   *
   * @return The preferred size.
   */
  public Dimension getPreferredSize(JComponent c)
  {
    JLabel lab = (JLabel) c;
    Insets insets = lab.getInsets();
    int insetsX = insets.left + insets.right;
    int insetsY = insets.top + insets.bottom;
    Icon icon = lab.getIcon();
    String text = lab.getText();
    Dimension ret;
    if (icon == null && text == null)
      ret = new Dimension(insetsX, insetsY);
    else if (icon != null && text == null)
      ret = new Dimension(icon.getIconWidth() + insetsX,
                          icon.getIconHeight() + insetsY);
    else
      {
        FontMetrics fm = getFontMetrics(lab);
        ir.x = 0;
        ir.y = 0;
        ir.width = 0;
        ir.height = 0;
        tr.x = 0;
        tr.y = 0;
        tr.width = 0;
        tr.height = 0;
        vr.x = 0;
        vr.y = 0;
        vr.width = Short.MAX_VALUE;
        vr.height = Short.MAX_VALUE;
        layoutCL(lab, fm, text, icon, vr, ir, tr);
        Rectangle cr = SwingUtilities.computeUnion(tr.x, tr.y, tr.width,
                                                   tr.height, ir);
        ret = new Dimension(cr.width + insetsX, cr.height + insetsY);
      }
    return ret;
  }

  /**
   * This method returns the minimum size of the {@link JComponent} given. If
   * this method returns null, then it is up to the Layout Manager to give
   * this component a minimum size.
   *
   * @param c The {@link JComponent} to get a minimum size for.
   *
   * @return The minimum size.
   */
  public Dimension getMinimumSize(JComponent c)
  {
    return getPreferredSize(c);
  }

  /**
   * This method returns the maximum size of the {@link JComponent} given. If
   * this method returns null, then it is up to the Layout Manager to give
   * this component a maximum size.
   *
   * @param c The {@link JComponent} to get a maximum size for.
   *
   * @return The maximum size.
   */
  public Dimension getMaximumSize(JComponent c)
  {
    return getPreferredSize(c);
  }

  /**
   * The method that paints the label according to its current state.
   * 
   * @param g The {@link Graphics} object to paint with.
   * @param c The {@link JComponent} to paint.
   */
  public void paint(Graphics g, JComponent c)
  {
    JLabel b = (JLabel) c;
    Icon icon = (b.isEnabled()) ? b.getIcon() : b.getDisabledIcon();
    String text = b.getText();
    if (icon != null || (text != null && ! text.equals("")))
      {
        FontMetrics fm = getFontMetrics(b);
        Insets i = c.getInsets(cachedInsets);
        vr.x = i.left;
        vr.y = i.right;
        vr.width = c.getWidth() - i.left - i.right;
        vr.height = c.getHeight() - i.top - i.bottom;
        ir.x = 0;
        ir.y = 0;
        ir.width = 0;
        ir.height = 0;
        tr.x = 0;
        tr.y = 0;
        tr.width = 0;
        tr.height = 0;

        text = layoutCL(b, fm, text, icon, vr, ir, tr);

        if (icon != null)
          icon.paintIcon(b, g, ir.x, ir.y);       

        if (text != null && ! text.equals(""))
          {
            Object htmlRenderer = b.getClientProperty(BasicHTML.propertyKey);
            if (htmlRenderer == null)
              {
                if (b.isEnabled())
                  paintEnabledText(b, g, text, tr.x, tr.y + fm.getAscent());
                else
                  paintDisabledText(b, g, text, tr.x, tr.y + fm.getAscent());
              }
            else
              {
                ((View) htmlRenderer).paint(g, tr);
              }
          }
      }
  }

  /**
   * This method is simply calls SwingUtilities's layoutCompoundLabel.
   * 
   * @param label The label to lay out.
   * @param fontMetrics The FontMetrics for the font used.
   * @param text The text to paint.
   * @param icon The icon to draw.
   * @param viewR The entire viewable rectangle.
   * @param iconR The icon bounds rectangle.
   * @param textR The text bounds rectangle.
   * 
   * @return A possibly clipped version of the text.
   */
  protected String layoutCL(JLabel label, FontMetrics fontMetrics, String text,
      Icon icon, Rectangle viewR, Rectangle iconR, Rectangle textR)
  {
    return SwingUtilities.layoutCompoundLabel(label, fontMetrics, text, icon,
        label.getVerticalAlignment(), label.getHorizontalAlignment(), label
            .getVerticalTextPosition(), label.getHorizontalTextPosition(),
        viewR, iconR, textR, label.getIconTextGap());
  }

  /**
   * Paints the text if the label is disabled. By default, this paints the
   * clipped text returned by layoutCompoundLabel using the
   * background.brighter() color. It also paints the same text using the
   * background.darker() color one pixel to the right and one pixel down.
   *
   * @param l The {@link JLabel} being painted.
   * @param g The {@link Graphics} object to paint with.
   * @param s The String to paint.
   * @param textX The x coordinate of the start of the baseline.
   * @param textY The y coordinate of the start of the baseline.
   */
  protected void paintDisabledText(JLabel l, Graphics g, String s, int textX,
      int textY)
  {
    g.setColor(l.getBackground().brighter());

    int mnemIndex = l.getDisplayedMnemonicIndex();

    if (mnemIndex != -1)
      BasicGraphicsUtils.drawStringUnderlineCharAt(g, s, mnemIndex, textX,
          textY);
    else
      g.drawString(s, textX, textY);

    g.setColor(l.getBackground().darker());
    if (mnemIndex != -1)
      BasicGraphicsUtils.drawStringUnderlineCharAt(g, s, mnemIndex, textX + 1,
          textY + 1);
    else
      g.drawString(s, textX + 1, textY + 1);
  }

  /**
   * Paints the text if the label is enabled. The text is painted using the
   * foreground color.
   *
   * @param l The {@link JLabel} being painted.
   * @param g The {@link Graphics} object to paint with.
   * @param s The String to paint.
   * @param textX The x coordinate of the start of the baseline.
   * @param textY The y coordinate of the start of the baseline.
   */
  protected void paintEnabledText(JLabel l, Graphics g, String s, int textX,
                                  int textY)
  {
    g.setColor(l.getForeground());

    int mnemIndex = l.getDisplayedMnemonicIndex();

    if (mnemIndex != -1)
      BasicGraphicsUtils.drawStringUnderlineCharAt(g, s, mnemIndex, textX,
          textY);
    else
      g.drawString(s, textX, textY);
  }

  /**
   * This method installs the UI for the given {@link JComponent}.  This
   * method will install the component, defaults, listeners,  and keyboard
   * actions.
   *
   * @param c The {@link JComponent} that this UI is being installed on.
   */
  public void installUI(JComponent c)
  {
    super.installUI(c);
    if (c instanceof JLabel)
    {
      JLabel l = (JLabel) c;

      installComponents(l);
      installDefaults(l);
      installListeners(l);
      installKeyboardActions(l);
    }
  }

  /**
   * This method uninstalls the UI for the given {@link JComponent}. This
   * method will uninstall the component, defaults, listeners,  and keyboard
   * actions.
   *
   * @param c The {@link JComponent} that this UI is being installed on.
   */
  public void uninstallUI(JComponent c)
  {
    super.uninstallUI(c);
    if (c instanceof JLabel)
    {
      JLabel l = (JLabel) c;

      uninstallKeyboardActions(l);
      uninstallListeners(l);
      uninstallDefaults(l);
      uninstallComponents(l);
    }
  }

  /**
   * This method installs the components for this {@link JLabel}.
   *
   * @param c The {@link JLabel} to install components for.
   */
  protected void installComponents(JLabel c)
  {
    BasicHTML.updateRenderer(c, c.getText());
  }

  /**
   * This method uninstalls the components for this {@link JLabel}.
   *
   * @param c The {@link JLabel} to uninstall components for.
   */
  protected void uninstallComponents(JLabel c)
  {
    c.putClientProperty(BasicHTML.propertyKey, null);
    c.putClientProperty(BasicHTML.documentBaseKey, null);
  }

  /**
   * This method installs the defaults that are defined in  the Basic look and
   * feel for this {@link JLabel}.
   *
   * @param c The {@link JLabel} to install defaults for.
   */
  protected void installDefaults(JLabel c)
  {
    LookAndFeel.installColorsAndFont(c, "Label.background", "Label.foreground",
                                     "Label.font");
    //XXX: There are properties we don't use called disabledForeground
    //and disabledShadow.
  }

  /**
   * This method uninstalls the defaults that are defined in the Basic look
   * and feel for this {@link JLabel}.
   *
   * @param c The {@link JLabel} to uninstall defaults for.
   */
  protected void uninstallDefaults(JLabel c)
  {
    c.setForeground(null);
    c.setBackground(null);
    c.setFont(null);
  }

  /**
   * Installs the keyboard actions for the given {@link JLabel}.
   *
   * @param l The {@link JLabel} to install keyboard actions for.
   */
  protected void installKeyboardActions(JLabel l)
  {
    Component c = l.getLabelFor();
    if (c != null)
      {
        int mnemonic = l.getDisplayedMnemonic();
        if (mnemonic > 0)
          {
            // add a keystroke for the given mnemonic mapping to 'press';
            InputMap keyMap = new InputMap();
            keyMap.put(KeyStroke.getKeyStroke(mnemonic, KeyEvent.VK_ALT), 
                "press");
            SwingUtilities.replaceUIInputMap(l, 
                JComponent.WHEN_IN_FOCUSED_WINDOW, keyMap);
            
            // add an action to focus the component when 'press' happens
            ActionMap map = new ActionMap();
            map.put("press", new AbstractAction() {
              public void actionPerformed(ActionEvent event)
              {
                JLabel label = (JLabel) event.getSource();
                Component c = label.getLabelFor();
                if (c != null)
                  c.requestFocus();
              }
            });
            SwingUtilities.replaceUIActionMap(l, map);
          }
      }   
  }

  /**
   * This method uninstalls the keyboard actions for the given {@link JLabel}.
   *
   * @param l The {@link JLabel} to uninstall keyboard actions for.
   */
  protected void uninstallKeyboardActions(JLabel l)
  {
    SwingUtilities.replaceUIActionMap(l, null);
    SwingUtilities.replaceUIInputMap(l, JComponent.WHEN_IN_FOCUSED_WINDOW, 
                                     null);
  }

  /**
   * This method installs the listeners for the  given {@link JLabel}. The UI
   * delegate only listens to  the label.
   *
   * @param c The {@link JLabel} to install listeners for.
   */
  protected void installListeners(JLabel c)
  {
    c.addPropertyChangeListener(this);
  }

  /**
   * This method uninstalls the listeners for the given {@link JLabel}. The UI
   * delegate only listens to the label.
   *
   * @param c The {@link JLabel} to uninstall listeners for.
   */
  protected void uninstallListeners(JLabel c)
  {
    c.removePropertyChangeListener(this);
  }

  /**
   * This method is called whenever any JLabel's that use this UI has one of
   * their properties change.
   *
   * @param e The {@link PropertyChangeEvent} that describes the change.
   */
  public void propertyChange(PropertyChangeEvent e)
  {
    if (e.getPropertyName().equals("text"))
      {
        String text = (String) e.getNewValue();
        JLabel l = (JLabel) e.getSource();
        BasicHTML.updateRenderer(l, text);
      }
    else if (e.getPropertyName().equals("displayedMnemonic"))
      {
        // update the key to action mapping
        JLabel label = (JLabel) e.getSource();
        if (label.getLabelFor() != null)
          {
            int oldMnemonic = ((Integer) e.getOldValue()).intValue();
            int newMnemonic = ((Integer) e.getNewValue()).intValue();
            InputMap keyMap = label.getInputMap(
                JComponent.WHEN_IN_FOCUSED_WINDOW);
            keyMap.put(KeyStroke.getKeyStroke(oldMnemonic, 
                KeyEvent.ALT_DOWN_MASK), null);
            keyMap.put(KeyStroke.getKeyStroke(newMnemonic, 
                KeyEvent.ALT_DOWN_MASK), "press");
          }
      }
    else if (e.getPropertyName().equals("labelFor"))
      {
        JLabel label = (JLabel) e.getSource();
        InputMap keyMap = label.getInputMap(JComponent.WHEN_IN_FOCUSED_WINDOW);
        int mnemonic = label.getDisplayedMnemonic();
        if (mnemonic > 0)
          keyMap.put(KeyStroke.getKeyStroke(mnemonic, KeyEvent.ALT_DOWN_MASK), 
              "press");       
      }
  }

  /**
   * Fetches a font metrics object for the specified label. This first
   * tries to get it from the label object itself by calling
   * {@link Component#getFontMetrics(Font)}, and if that does not work
   * (for instance, when we are in the initialization and have no parent yet),
   * it asks the Toolkit for a font metrics object.
   *
   * @param l the label
   *
   * @return a suitable font metrics object
   */
  private FontMetrics getFontMetrics(JLabel l)
  {
    Font font = l.getFont();
    FontMetrics fm = l.getFontMetrics(font);
    if (fm == null)
      {
        Toolkit tk = Toolkit.getDefaultToolkit();
        fm = tk.getFontMetrics(font);
      }
    return fm;
  }
}
