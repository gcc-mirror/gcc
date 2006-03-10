/* DefaultListCellRenderer.java --
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


package javax.swing;

import java.awt.Component;
import java.awt.Rectangle;
import java.io.Serializable;

import javax.swing.border.Border;
import javax.swing.border.EmptyBorder;

/**
 * The default implementation {@link ListCellRenderer}. It provides a standard
 * renderer for data objects of all types via {@link Object#toString()}.
 *
 * @author Andrew Selkirk
 */
public class DefaultListCellRenderer extends JLabel
  implements ListCellRenderer, Serializable
{
  private static final long serialVersionUID = 7708947179685189462L;

  /**
   * Subclasses <code>DefaultListCellRenderers</code> and implements
   * {@link javax.swing.plaf.UIResource}. This is used by
   * {@link javax.swing.plaf.ListUI} subclasses to provide a default for
   * the <code>List.cellRenderer</code> property. If you want to override
   * this property, use <code>DefaultListCellRenderer</code> or a subclass.
   */
  public static class UIResource extends DefaultListCellRenderer
    implements javax.swing.plaf.UIResource
  {
    public UIResource()
    {
      super();
    }
  }

  /**
   * This border is used whenever renderer doesn't have a focus.
   */
  protected static Border noFocusBorder = new EmptyBorder(1, 1, 1, 1);

  /**
   * getListCellRendererComponent
   *
   * @param list JList list for the 'value'
   * @param value object that should be rendered in the cell
   * @param index index of the cell
   * @param isSelected draw cell highlighted if isSelected is true
   * @param cellHasFocus draw focus rectangle around cell if the cell has
   *        focus
   *
   * @return Component that will be painted to the desired cell.
   */
  public Component getListCellRendererComponent(JList list, Object value,
                                                int index, boolean isSelected,
                                                boolean cellHasFocus)
  {
    String s = value != null ? value.toString() : "";
    setText(s);
    setOpaque(true);
    setHorizontalAlignment(LEFT);

    if (isSelected)
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

    // Use focusCellHighlightBorder when renderer has focus and
    // noFocusBorder otherwise

    if (cellHasFocus)
      setBorder(UIManager.getBorder("List.focusCellHighlightBorder"));
    else
      setBorder(noFocusBorder);

    return this;
  }

  public void validate()
  {
    // Overridden to do nothing.
  }

  public void revalidate()
  {
    // Overridden to do nothing.
  }

  public void repaint(long tm, int x, int y, int w, int h)
  {
    // Overridden to do nothing.
  }

  public void repaint(Rectangle rect)
  {
    // Overridden to do nothing.
  }

  protected void firePropertyChange(String propertyName, Object oldValue,
                                    Object newValue)
  {
    // Overridden to do nothing.
  }

  public void firePropertyChange(String propertyName, byte oldValue,
                                 byte newValue)
  {
    // Overridden to do nothing.
  }

  public void firePropertyChange(String propertyName, char oldValue,
                                 char newValue)
  {
    // Overridden to do nothing.
  }

  public void firePropertyChange(String propertyName, short oldValue,
                                 short newValue)
  {
    // Overridden to do nothing.
  }

  public void firePropertyChange(String propertyName, int oldValue,
                                 int newValue)
  {
    // Overridden to do nothing.
  }

  public void firePropertyChange(String propertyName, long oldValue,
                                 long newValue)
  {
    // Overridden to do nothing.
  }

  public void firePropertyChange(String propertyName, float oldValue,
                                 float newValue)
  {
    // Overridden to do nothing.
  }

  public void firePropertyChange(String propertyName, double oldValue,
                                 double newValue)
  {
    // Overridden to do nothing.
  }

  public void firePropertyChange(String propertyName, boolean oldValue,
                                 boolean newValue)
  {
    // Overridden to do nothing.
  }
}
