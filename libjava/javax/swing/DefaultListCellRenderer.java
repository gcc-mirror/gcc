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

package javax.swing;

import java.awt.Component;
import java.awt.Rectangle;
import java.io.Serializable;
import javax.swing.border.Border;


/**
 * DefaultListCellRenderer
 *
 * @author Andrew Selkirk
 * @version 1.0
 */
public class DefaultListCellRenderer 
  extends JLabel implements ListCellRenderer, Serializable
{
  static final long serialVersionUID = 7708947179685189462L;

  public static class UIResource extends DefaultListCellRenderer
    implements javax.swing.plaf.UIResource
  {
    public UIResource()
    {
    }
  }

  /** noFocusBorder */
  protected static Border noFocusBorder = null; // TODO

  /**
   * getListCellRendererComponent
   *
   * @param list TODO
   * @param value TODO
   * @param index TODO
   * @param isSelected TODO
   * @param cellHasFocus TODO
   *
   * @return Component
   */
  public Component getListCellRendererComponent(JList list, Object value,
                                                int index, boolean isSelected,
                                                boolean cellHasFocus)
  {
    String s = value.toString();
    setText(s);
    setOpaque(true);

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
    return this;
  }

  public void validate() {}
  public void revalidate() {}
  public void repaint(long tm, int x, int y, int w, int h) {}
  public void repaint(Rectangle rect) {}
  protected void firePropertyChange(String propertyName, Object oldValue, Object newValue){}
  public void firePropertyChange(String propertyName, byte oldValue, byte newValue) {}
  public void firePropertyChange(String propertyName, char oldValue, char newValue) {} 
  public void firePropertyChange(String propertyName, short oldValue, short newValue) {}
  public void firePropertyChange(String propertyName, int oldValue, int newValue) {}
  public void firePropertyChange(String propertyName, long oldValue, long newValue) {}
  public void firePropertyChange(String propertyName, float oldValue, float newValue) {}
  public void firePropertyChange(String propertyName, double oldValue, double newValue) {}
  public void firePropertyChange(String propertyName, boolean oldValue, boolean newValue) {}
}
