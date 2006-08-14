/* MetalComboBoxButton.java
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
import java.awt.Component;
import java.awt.Graphics;
import java.awt.Insets;

import javax.swing.CellRendererPane;
import javax.swing.Icon;
import javax.swing.JButton;
import javax.swing.JComboBox;
import javax.swing.JList;
import javax.swing.ListCellRenderer;
import javax.swing.UIManager;

/**
 * A button used by the {@link MetalComboBoxUI} class.
 */
public class MetalComboBoxButton
  extends JButton
{

  /** A reference to the JComboBox that the button belongs to. */
  protected JComboBox comboBox;

  /** A reference to the JList. */
  protected JList listBox;
  
  /**
   * Used for rendering the selected item.
   */
  protected CellRendererPane rendererPane;
  
  /** The button icon. */
  protected Icon comboIcon;
  
  /** Display just the icon, or the icon plus the label. */
  protected boolean iconOnly;
  
  /**
   * Creates a new button.
   * 
   * @param cb  the combo that the button is used for (<code>null</code> not 
   *            permitted).
   * @param i  the icon displayed on the button.
   * @param pane  the rendering pane.
   * @param list  the list.
   */
  public MetalComboBoxButton(JComboBox cb, Icon i, CellRendererPane pane,
      JList list)
  {
    this(cb, i, cb.isEditable(), pane, list);  
  }
  
  /**
   * Creates a new button.
   * 
   * @param cb  the combo that the button is used for (<code>null</code> not 
   *            permitted).
   * @param i  the icon displayed on the button.
   * @param onlyIcon  a flag that specifies whether the button displays only an
   *                  icon, or text as well.
   * @param pane  the rendering pane.
   * @param list  the list.
   */
  public MetalComboBoxButton(JComboBox cb, Icon i, boolean onlyIcon,
      CellRendererPane pane, JList list)
  {
    super();
    if (cb == null)
      throw new NullPointerException("Null 'cb' argument");
    comboBox = cb;
    comboIcon = i;
    iconOnly = onlyIcon;
    listBox = list;
    rendererPane = pane;
    setRolloverEnabled(false);
    setEnabled(comboBox.isEnabled());
    setFocusable(comboBox.isEnabled());
  }
  
  /**
   * Returns the combo box that the button is used with.
   * 
   * @return The combo box.
   */
  public final JComboBox getComboBox()
  {
    return comboBox;
  }
  
  /**
   * Sets the combo box that the button is used with.
   * 
   * @param cb  the combo box.
   */
  public final void setComboBox(JComboBox cb)
  {
    comboBox = cb;
  }
  
  /**
   * Returns the icon displayed by the button.  By default, this will be an
   * instance of {@link MetalComboBoxIcon}.
   * 
   * @return The icon displayed by the button.
   */
  public final Icon getComboIcon()
  {
    return comboIcon;
  }
  
  /**
   * Sets the icon displayed by the button.
   * 
   * @param i  the icon.
   */
  public final void setComboIcon(Icon i)
  {
    comboIcon = i;
  }
  
  /**
   * Returns a flag that controls whether the button displays an icon only,
   * or text as well.
   * 
   * @return A boolean.
   */
  public final boolean isIconOnly()
  {
    return iconOnly;
  }
  
  /**
   * Sets the flag that controls whether the button displays an icon only,
   * or text as well.
   * 
   * @param isIconOnly  the flag.
   */
  public final void setIconOnly(boolean isIconOnly)
  {
    iconOnly = isIconOnly;
  }
  
  /**
   * Returns <code>false</code>, to indicate that this component is not part
   * of the focus traversal group.
   * 
   * @return <code>false</code>
   */
  public boolean isFocusTraversable()
  {
    return false;
  }
  
  /**
   * Enables or disables the button.
   * 
   * @param enabled  the new status.
   */
  public void setEnabled(boolean enabled)
  {
    super.setEnabled(enabled);
    if (enabled)
      {
        setBackground(comboBox.getBackground());
        setForeground(comboBox.getForeground());
      }
    else
      {
        setBackground(UIManager.getColor("ComboBox.disabledBackground"));
        setForeground(UIManager.getColor("ComboBox.disabledForeground"));
      }
  }
  
  /**
   * Paints the component.
   * 
   * @param g  the graphics device.
   */
  public void paintComponent(Graphics g)
  {
    super.paintComponent(g);
    Insets insets = this.getInsets();
    int w = getWidth() - (insets.left + insets.right);
    int h = getHeight() - (insets.top + insets.bottom);
    if (h > 0 && w > 0)
      {
        int x1 = insets.left;
        int y1 = insets.top;
        int x2 = x1 + (w - 1);
        int y2 = y1 + (h - 1);
        int iconWidth = 0;
        int iconX = x2;
        if (comboIcon != null)
          {
            iconWidth = comboIcon.getIconWidth();
            int iconHeight = comboIcon.getIconHeight();
            int iconY;
            if (iconOnly)
              {
                iconX = getWidth() / 2 - iconWidth / 2;
                iconY = getHeight() / 2 - iconHeight / 2;
              }
            else
              {
                iconX = x1 + (w - 1) - iconWidth;
                iconY = y1 + (y2 - y1) / 2 - iconHeight / 2;
              }
            comboIcon.paintIcon(this, g, iconX, iconY);
            if (this.hasFocus())
              {
                g.setColor(MetalLookAndFeel.getFocusColor());
                g.drawRect(x1 - 1, y1 - 1, w + 3, h + 1);
              }
          }
        if (! iconOnly && comboBox != null)
          {
            ListCellRenderer renderer = comboBox.getRenderer();
            boolean pressed = this.getModel().isPressed();
            Component comp = renderer.getListCellRendererComponent(listBox,
                comboBox.getSelectedItem(), -1, false, false);
            comp.setFont(rendererPane.getFont());
            
            if ((model.isArmed() && model.isPressed())
                || (comboBox.isFocusOwner() && !comboBox.isPopupVisible()))
              {
                if (isOpaque())
                  {
                    comp.setBackground(UIManager.getColor("Button.select"));
                    comp.setForeground(comboBox.getForeground());
                  }
              }
            else if (! comboBox.isEnabled())
              {
                if (this.isOpaque())
                  {
                    Color dbg =
                      UIManager.getColor("ComboBox.disabledBackground");
                    comp.setBackground(dbg);
                    Color dfg =
                      UIManager.getColor("ComboBox.disabledForeground");
                    comp.setForeground(dfg);
                  }
              }
            else
              {
                comp.setForeground(comboBox.getForeground());
                comp.setBackground(comboBox.getBackground());
              }
            int wr = w - (insets.right + iconWidth);
            rendererPane.paintComponent(g, comp, this, x1, y1, wr, h);
          }
      }
  }
}
