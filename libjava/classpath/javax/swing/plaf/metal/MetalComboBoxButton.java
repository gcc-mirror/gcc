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

import java.awt.Component;
import java.awt.Graphics;
import java.awt.Insets;
import java.awt.Rectangle;

import javax.swing.CellRendererPane;
import javax.swing.Icon;
import javax.swing.JButton;
import javax.swing.JComboBox;
import javax.swing.JList;
import javax.swing.SwingUtilities;

/**
 * A button used by the {@link MetalComboBoxUI} class.
 */
public class MetalComboBoxButton extends JButton {

  /** A reference to the JComboBox that the button belongs to. */
  protected JComboBox comboBox;

  /** A reference to the JList. */
  protected JList listBox;
  
  /** ??? */
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
   * @parma onlyIcon  a flag that specifies whether the button displays only an
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
    // TODO: figure out what this might need to be used for
    // perhaps it has something to do with the button's icon and/or border?
  }
  
  /**
   * Paints the component.
   * 
   * @param g  the graphics device.
   */
  public void paintComponent(Graphics g)
  {
    super.paintComponent(g);
    if (iconOnly)
      {
        Rectangle bounds = getBounds();
        int x = (bounds.width - comboIcon.getIconWidth()) / 2;
        int y = (bounds.height - comboIcon.getIconHeight()) / 2;
        comboIcon.paintIcon(comboBox, g, x, y);  
      }
    else
      {
        Object selected = comboBox.getModel().getSelectedItem();
        if (selected == null)
          selected = "";
        Rectangle bounds = comboBox.getBounds();
        Rectangle innerArea = SwingUtilities.calculateInnerArea(this, null);
        Insets insets = comboBox.getInsets();
        Rectangle renderArea = new Rectangle(innerArea.x, innerArea.y, 
            innerArea.width - comboIcon.getIconWidth() - 4, innerArea.height);
        Component cellRenderer 
            = comboBox.getRenderer().getListCellRendererComponent(this.listBox,
                    selected, comboBox.getSelectedIndex(), false, false);
        cellRenderer.setBackground(comboBox.getBackground());
        cellRenderer.setEnabled(comboBox.isEnabled());
        rendererPane.paintComponent(g, cellRenderer, this, renderArea);
        if (comboBox.hasFocus())
          {
            g.setColor(MetalLookAndFeel.getFocusColor());
            g.drawRect(innerArea.x, innerArea.y - 1, innerArea.width - 1, 
                    innerArea.height);
          }
        int iconX = bounds.width - insets.right - comboIcon.getIconWidth() - 7;
        int iconY = insets.top 
            + (bounds.height - comboIcon.getIconHeight()) / 2; 
        comboIcon.paintIcon(comboBox, g, iconX, iconY);
      }
  }
}
