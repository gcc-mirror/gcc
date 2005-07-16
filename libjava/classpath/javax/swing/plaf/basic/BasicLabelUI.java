/* BasicLabelUI.java
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

import java.awt.Color;
import java.awt.Dimension;
import java.awt.Font;
import java.awt.FontMetrics;
import java.awt.Graphics;
import java.awt.Insets;
import java.awt.Rectangle;
import java.beans.PropertyChangeEvent;
import java.beans.PropertyChangeListener;

import javax.swing.Icon;
import javax.swing.JComponent;
import javax.swing.JLabel;
import javax.swing.SwingUtilities;
import javax.swing.UIDefaults;
import javax.swing.UIManager;
import javax.swing.plaf.ComponentUI;
import javax.swing.plaf.LabelUI;


/**
 * This is the Basic Look and Feel class for the JLabel.  One BasicLabelUI
 * object is used to paint all JLabels that utilize the Basic Look and Feel.
 */
public class BasicLabelUI extends LabelUI implements PropertyChangeListener
{
  /** The labelUI that is shared by all labels. */
  protected static BasicLabelUI labelUI;

  /**
   * Creates a new BasicLabelUI object.
   */
  public BasicLabelUI()
  {
    super();
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
    JLabel lab = (JLabel)c;
    Rectangle vr = new Rectangle();
    Rectangle ir = new Rectangle();
    Rectangle tr = new Rectangle();
    Insets insets = lab.getInsets();      
    FontMetrics fm = lab.getToolkit().getFontMetrics(lab.getFont());
    layoutCL(lab, fm, lab.getText(), lab.getIcon(), vr, ir, tr);
    Rectangle cr = tr.union(ir);
    return new Dimension(insets.left + cr.width + insets.right,
                         insets.top + cr.height + insets.bottom);
    
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

    Font saved_font = g.getFont();

    Rectangle tr = new Rectangle();
    Rectangle ir = new Rectangle();
    Rectangle vr = new Rectangle();

    Font f = c.getFont();

    g.setFont(f);
    FontMetrics fm = g.getFontMetrics(f);

    vr = SwingUtilities.calculateInnerArea(c, vr);

    if (vr.width < 0)
      vr.width = 0;
    if (vr.height < 0)
      vr.height = 0;
      
    Icon icon = (b.isEnabled()) ? b.getIcon() : b.getDisabledIcon();

    String text = layoutCL(b, fm, b.getText(), icon, vr, ir, tr);
    
    if (icon != null)
      icon.paintIcon(b, g, ir.x, ir.y);
    if (text != null && ! text.equals(""))
      {
	if (b.isEnabled())
	  paintEnabledText(b, g, text, tr.x, tr.y + fm.getAscent());
	else
	  paintDisabledText(b, g, text, tr.x, tr.y + fm.getAscent());
      }
    g.setFont(saved_font);
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
  protected String layoutCL(JLabel label, FontMetrics fontMetrics,
                            String text, Icon icon, Rectangle viewR,
                            Rectangle iconR, Rectangle textR)
  {
    return SwingUtilities.layoutCompoundLabel(label, fontMetrics, text, icon,
                                              label.getVerticalAlignment(),
                                              label.getHorizontalAlignment(),
                                              label.getVerticalTextPosition(),
                                              label.getHorizontalTextPosition(),
                                              viewR, iconR, textR,
                                              label.getIconTextGap());
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
    Color saved_color = g.getColor();

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

    g.setColor(saved_color);
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
    Color saved_color = g.getColor();
    g.setColor(l.getForeground());

    int mnemIndex = l.getDisplayedMnemonicIndex();

    if (mnemIndex != -1)
      BasicGraphicsUtils.drawStringUnderlineCharAt(g, s, mnemIndex, textX,
                                                   textY);
    else
      g.drawString(s, textX, textY);

    g.setColor(saved_color);
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
    //FIXME: fix javadoc + implement.
  }

  /**
   * This method uninstalls the components for this {@link JLabel}.
   *
   * @param c The {@link JLabel} to uninstall components for.
   */
  protected void uninstallComponents(JLabel c)
  {
    //FIXME: fix javadoc + implement.
  }

  /**
   * This method installs the defaults that are defined in  the Basic look and
   * feel for this {@link JLabel}.
   *
   * @param c The {@link JLabel} to install defaults for.
   */
  protected void installDefaults(JLabel c)
  {
    UIDefaults defaults = UIManager.getLookAndFeelDefaults();

    c.setForeground(defaults.getColor("Label.foreground"));
    c.setBackground(defaults.getColor("Label.background"));
    c.setFont(defaults.getFont("Label.font"));
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
   * This method installs the keyboard actions for the given {@link JLabel}.
   *
   * @param l The {@link JLabel} to install keyboard actions for.
   */
  protected void installKeyboardActions(JLabel l)
  {
    //FIXME: implement.
  }

  /**
   * This method uninstalls the keyboard actions for the given {@link JLabel}.
   *
   * @param l The {@link JLabel} to uninstall keyboard actions for.
   */
  protected void uninstallKeyboardActions(JLabel l)
  {
    //FIXME: implement.
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
    JLabel c = (JLabel) e.getSource();
    c.revalidate();
    c.repaint();
  }
}
