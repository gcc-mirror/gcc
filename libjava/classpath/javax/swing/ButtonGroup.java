/* ButtonGroup.java --
   Copyright (C) 2002 Free Software Foundation, Inc.

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

import java.io.Serializable;
import java.util.Enumeration;
import java.util.Vector;


/**
 * DOCUMENT ME!
 */
public class ButtonGroup implements Serializable
{
  /** DOCUMENT ME! */
  private static final long serialVersionUID = 4259076101881721375L;

  /** The buttons added to this button group. */
  protected Vector buttons = new Vector();

  /** The currently selected button model. */
  ButtonModel sel;

  /**
   * Creates a new button group.
   */
  public ButtonGroup()
  {
  }

  /**
   * Adds a button to this group.
   *
   * @param b the button to add
   */
  public void add(AbstractButton b)
  {
    b.getModel().setGroup(this);
    buttons.addElement(b);
  }

  /**
   * Removed a given button from this group.
   *
   * @param b the button to remove
   */
  public void remove(AbstractButton b)
  {
    b.getModel().setGroup(null);
    buttons.removeElement(b);
  }

  /**
   * Returns the currently added buttons.
   *
   * @return <code>Enumeration</code> over all added buttons
   */
  public Enumeration getElements()
  {
    return buttons.elements();
  }

  /**
   * Returns the currently selected button model.
   *
   * @return the currently selected button model, null if none was selected
   *         yet
   */
  public ButtonModel getSelection()
  {
    return sel;
  }

  /**
   * DOCUMENT ME!
   *
   * @param m DOCUMENT ME!
   *
   * @return DOCUMENT ME!
   */
  AbstractButton FindButton(ButtonModel m)
  {
    for (int i = 0; i < buttons.size(); i++)
      {
	AbstractButton a = (AbstractButton) buttons.get(i);
	if (a.getModel() == m)
	  return a;
      }
    return null;
  }

  /**
   * Sets the currently selected button model. Only one button of a group can
   * be selected at a time.
   *
   * @param m the model to select
   * @param b true if this button is to be selected, false otherwise
   */
  public void setSelected(ButtonModel m, boolean b)
  {
    if ((sel != m || b) && (! b || sel == m))
      return;

    if (b && sel != m)
      {
        ButtonModel old = sel;
        sel = m;

        if (old != null)
          old.setSelected(false);
        AbstractButton button = FindButton(old);
        if (button != null)
          button.repaint();
      }
    else if (!b && sel == m)
      m.setSelected(true);
  }

  /**
   * Checks if the given <code>ButtonModel</code> is selected in this button
   * group.
   *
   * @param m DOCUMENT ME!
   *
   * @return true of given <code>ButtonModel</code> is selected, false
   *         otherwise
   */
  public boolean isSelected(ButtonModel m)
  {
    return m == sel;
  }

  /**
   * Return the number of buttons in this button group.
   *
   * @return the number of buttons
   *
   * @since 1.3
   */
  public int getButtonCount()
  {
    return buttons.size();
  }
}
