/* BasicComboBoxRenderer.java --
   Copyright (C) 2004, 2005  Free Software Foundation, Inc.

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
import java.awt.FontMetrics;
import java.io.Serializable;

import javax.swing.JComboBox;
import javax.swing.JLabel;
import javax.swing.JList;
import javax.swing.ListCellRenderer;
import javax.swing.SwingConstants;
import javax.swing.SwingUtilities;
import javax.swing.border.Border;
import javax.swing.border.EmptyBorder;

/**
 * A renderer for a {@link JComboBox}. 
 *
 * @author Olga Rodimina
 */
public class BasicComboBoxRenderer 
  extends JLabel 
  implements ListCellRenderer, Serializable
{
  /**
   * A shared border instance for all renderers.
   */
  protected static Border noFocusBorder = new EmptyBorder(0, 0, 0, 0);

  /**
   * Creates a new <code>BasicComboBoxRenderer</code> object.
   */
  public BasicComboBoxRenderer()
  {
    setHorizontalAlignment(SwingConstants.LEFT);
    setBorder(noFocusBorder);
  }

  /**
   * Returns preferredSize of the renderer
   *
   * @return preferredSize of the renderer
   */
  public Dimension getPreferredSize()
  {
    return super.getPreferredSize();
  }

  /**
   * Returns a component that has been configured to display the given
   * <code>value</code>.
   *
   * @param list List of items for which to the background and foreground
   *        colors
   * @param value object that should be rendered in the cell
   * @param index index of the cell in the list of items.
   * @param isSelected draw cell highlighted if isSelected is true
   * @param cellHasFocus draw focus rectangle around cell if the cell has
   *        focus
   *
   * @return Component that will be used to draw the desired cell.
   */
  public Component getListCellRendererComponent(JList list, Object value,
                                                int index, boolean isSelected,
                                                boolean cellHasFocus)
  {
    String s = value.toString();
    
    // String maybe larger than comboBox.
    FontMetrics fm = getToolkit().getFontMetrics(list.getFont());
    int strWidth = SwingUtilities.computeStringWidth(fm, s);
    int cbWidth = getSize().width;
    if (cbWidth != 0 && strWidth > cbWidth)
      {
        char[] str = s.toCharArray();
        int currWidth = 0;
        int i = 0;
        String postStr = "... ";
        cbWidth -= SwingUtilities.computeStringWidth(fm, postStr);
        while (i < str.length && currWidth < cbWidth)
          {
            ++i;
            currWidth = SwingUtilities.computeStringWidth(fm, new String(str, 0, i));
          }
        setText(new String(str, 0, i)  + postStr);
      }
    else   
      setText(s);
    
    setOpaque(true);

    if (isSelected || cellHasFocus)
      {
        setBackground(list.getSelectionBackground());
        setForeground(list.getSelectionForeground());
      }
    else
      {
        setBackground(list.getBackground());
        setForeground(list.getForeground());
      }

    setEnabled(list.isEnabled());
    setFont(list.getFont());
    return this;
  }

  /**
   * A subclass of {@link BasicComboBoxRenderer} that implements the
   * {@link javax.swing.plaf.UIResource} interface.
   */
  public static class UIResource extends BasicComboBoxRenderer
    implements javax.swing.plaf.UIResource
  {
    /**
     * Creates a new <code>UIResource</code> object.
     */
    public UIResource()
    {
      // Nothing to do here.
    }
  }
}
