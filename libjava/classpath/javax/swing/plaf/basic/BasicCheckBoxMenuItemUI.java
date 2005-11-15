/* BasicCheckBoxMenuItemUI.java --
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

import java.awt.event.MouseEvent;

import javax.swing.JComponent;
import javax.swing.JMenuItem;
import javax.swing.MenuElement;
import javax.swing.MenuSelectionManager;
import javax.swing.UIDefaults;
import javax.swing.UIManager;
import javax.swing.plaf.ComponentUI;

/**
 * DOCUMENT ME!
 */
public class BasicCheckBoxMenuItemUI extends BasicMenuItemUI
{
  
  /**
   * Creates a new BasicCheckBoxMenuItemUI object.
   */
  public BasicCheckBoxMenuItemUI()
  {
    super();
  }
  
  /**
   * Factory method to create a BasicCheckBoxMenuItemUI for the given {@link
   * JComponent}, which should be a JCheckBoxMenuItem
   *
   * @param c The {@link JComponent} a UI is being created for.
   *
   * @return A BasicCheckBoxMenuItemUI for the {@link JComponent}.
   */
  public static ComponentUI createUI(final JComponent c)
  {
    return new BasicCheckBoxMenuItemUI();
  }

  /**
   * Returns the prefix for entries in the {@link UIDefaults} table.
   *
   * @return "CheckBoxMenuItem"
   */
  protected String getPropertyPrefix()
  {
    return "CheckBoxMenuItem";
  }

  /**
   * DOCUMENT ME!
   *
   * @param item DOCUMENT ME!
   * @param e DOCUMENT ME!
   * @param path DOCUMENT ME!
   * @param manager DOCUMENT ME!
   */
  public void processMouseEvent(JMenuItem item, MouseEvent e,
                                MenuElement[] path,
                                MenuSelectionManager manager)
  {
    // TODO: May not be implemented properly.
    item.processMouseEvent(e, path, manager);
  }
}
