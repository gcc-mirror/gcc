/* DefaultTableCellRenderer.java
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

package javax.swing.table;

import java.awt.Color;
import java.awt.Component;
import java.awt.Rectangle;
import java.io.Serializable;
import javax.swing.JLabel;
import javax.swing.JTable;
import javax.swing.border.Border;
import javax.swing.border.EmptyBorder;

/**
 * Class to display every cells.
 */
public class DefaultTableCellRenderer extends JLabel
  implements TableCellRenderer, Serializable
{
  static final long serialVersionUID = 7878911414715528324L;

  protected static Border noFocusBorder = new EmptyBorder(0, 0, 0, 0);

  public static class UIResource extends DefaultTableCellRenderer
    implements javax.swing.plaf.UIResource
  {
    public UIResource()
    {
    }
  }

  /**
   * Creates a default table cell renderer with an empty border.
   */
  public DefaultTableCellRenderer()
  {
    super();
  }

  /**
   * Assign the unselected-foreground.
   *
   * @param c the color to assign
   */
  public void setForeground(Color c)
  {
    super.setForeground(c);
  }

  /**
   * Assign the unselected-background.
   *
   * @param c the color to assign
   */
  public void setBackground(Color c)
  {
    super.setBackground(c);
  }

  /**
   * Look and feel has changed.
   *
   * <p>Replaces the current UI object with the  latest version from
   * the UIManager.</p>
   */
  public void updateUI()
  {
    super.updateUI();
  }

  /**
   * Get the string value of the object and pass it to setText().
   *
   * @param table the JTable
   * @param value the value of the object
   * @param isSelected is the cell selected?
   * @param hasFocus has the cell the focus?
   * @param row the row to render
   * @param column the cell to render
   * 
   * @return this component (the default table cell renderer)
   */
  public Component getTableCellRendererComponent(JTable table, Object value,
                                                 boolean isSelected,
                                                 boolean hasFocus,
                                                 int row, int column)
  {
    if (value!=null)
      super.setText(value.toString());
    
    return this;
  }

  /**
   * Overriden for performance.
   *
   * <p>This method needs to be overridden in a subclass to actually
   * do something.</p>
   *
   * @return always true
   */
  public boolean isOpaque()
  {
    return true;
  }

  /**
   * Overriden for performance.
   *
   * <p>This method needs to be overridden in a subclass to actually
   * do something.</p>
   */
  public void validate()
  {
    // Does nothing.
  }

  /**
   * Overriden for performance.
   *
   * <p>This method needs to be overridden in a subclass to actually
   * do something.</p>
   */
  public void repaint(long tm, int x, int y, int width, int height)
  {
    // Does nothing.
  }

  /**
   * Overriden for performance.
   *
   * <p>This method needs to be overridden in a subclass to actually
   * do something.</p>
   */
  public void repaint(Rectangle r)
  {
    // Does nothing.
  }

  /**
   * Overriden for performance.
   *
   * <p>This method needs to be overridden in a subclass to actually
   * do something.</p>
   */
  public void firePropertyChange(String propertyName, Object oldValue,
                                 Object newValue)
  {
    // Does nothing.
  }

  /**
   * Overriden for performance.
   *
   * <p>This method needs to be overridden in a subclass to actually
   * do something.</p>
   */
  public void firePropertyChange(String propertyName, boolean oldValue,
		                 boolean newValue)
  {
    // Does nothing.
  }

  /**
   * Sets the String for this cell.
   * 
   * @param value the string value for this cell; if value is null it
   * sets the text value to an empty string
   */
  protected void setValue(Object value)
  {
    super.setText((value!=null) ? value.toString() : "");
  }
}
