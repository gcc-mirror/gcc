/* BasicMenuUI.java
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

package javax.swing.plaf.basic;

import java.awt.Component;
import java.awt.Dimension;
import java.awt.Insets;
import java.awt.event.FocusAdapter;
import java.awt.event.FocusEvent;
import java.awt.event.FocusListener;
import java.awt.event.InputEvent;
import java.awt.event.KeyEvent;
import java.awt.event.MouseAdapter;
import java.awt.event.MouseEvent;
import java.beans.PropertyChangeEvent;
import java.beans.PropertyChangeListener;
import javax.swing.JComponent;
import javax.swing.JMenu;
import javax.swing.JMenuBar;
import javax.swing.JMenuItem;
import javax.swing.MenuElement;
import javax.swing.MenuSelectionManager;
import javax.swing.UIDefaults;
import javax.swing.UIManager;
import javax.swing.event.ChangeEvent;
import javax.swing.event.ChangeListener;
import javax.swing.event.MenuDragMouseEvent;
import javax.swing.event.MenuDragMouseListener;
import javax.swing.event.MenuEvent;
import javax.swing.event.MenuKeyEvent;
import javax.swing.event.MenuKeyListener;
import javax.swing.event.MenuListener;
import javax.swing.event.MouseInputListener;
import javax.swing.plaf.ComponentUI;
import javax.swing.plaf.MenuItemUI;


/**
 * DOCUMENT ME!
 *
 * @author $author$
 * @version $Revision: 1.2 $
 */
public class BasicMenuUI extends BasicMenuItemUI
{
  protected ChangeListener changeListener;
  protected MenuListener menuListener;
  protected PropertyChangeListener propertyChangeListener;

  /**
   * Creates a new BasicMenuUI object.
   */
  public BasicMenuUI()
  {
    mouseInputListener = createMouseInputListener(menuItem);
    menuListener = createMenuListener(menuItem);
  }

  /**
   * DOCUMENT ME!
   *
   * @param c DOCUMENT ME!
   *
   * @return DOCUMENT ME!
   */
  protected ChangeListener createChangeListener(JComponent c)
  {
    return null;
  }

  /**
   * DOCUMENT ME!
   *
   * @param c DOCUMENT ME!
   *
   * @return DOCUMENT ME!
   */
  protected MenuDragMouseListener createMenuDragMouseListener(JComponent c)
  {
    return null;
  }

  /**
   * DOCUMENT ME!
   *
   * @param c DOCUMENT ME!
   *
   * @return DOCUMENT ME!
   */
  protected MenuKeyListener createMenuKeyListener(JComponent c)
  {
    return null;
  }

  /**
   * DOCUMENT ME!
   *
   * @param c DOCUMENT ME!
   *
   * @return DOCUMENT ME!
   */
  protected MenuListener createMenuListener(JComponent c)
  {
    return new MenuHandler();
  }

  /**
   * DOCUMENT ME!
   *
   * @param c DOCUMENT ME!
   *
   * @return DOCUMENT ME!
   */
  protected MouseInputListener createMouseInputListener(JComponent c)
  {
    return new MouseInputHandler();
  }

  /**
   * DOCUMENT ME!
   *
   * @param c DOCUMENT ME!
   *
   * @return DOCUMENT ME!
   */
  protected PropertyChangeListener createPropertyChangeListener(JComponent c)
  {
    return null;
  }

  /**
   * DOCUMENT ME!
   *
   * @param x DOCUMENT ME!
   *
   * @return DOCUMENT ME!
   */
  public static ComponentUI createUI(JComponent x)
  {
    return new BasicMenuUI();
  }

  /**
   * DOCUMENT ME!
   *
   * @param c DOCUMENT ME!
   *
   * @return DOCUMENT ME!
   */
  public Dimension getMaximumSize(JComponent c)
  {
    return null;
  }

  /**
   * DOCUMENT ME!
   *
   * @return DOCUMENT ME!
   */
  protected String getPropertyPrefix()
  {
    return null;
  }

  /**
   * DOCUMENT ME!
   */
  protected void installDefaults()
  {
    UIDefaults defaults = UIManager.getLookAndFeelDefaults();

    menuItem.setBackground(defaults.getColor("Menu.background"));
    menuItem.setBorder(defaults.getBorder("Menu.border"));
    menuItem.setFont(defaults.getFont("Menu.font"));
    menuItem.setForeground(defaults.getColor("Menu.foreground"));
    menuItem.setMargin(defaults.getInsets("Menu.margin"));
    acceleratorFont = defaults.getFont("Menu.acceleratorFont");
    acceleratorForeground = defaults.getColor("Menu.acceleratorForeground");
    acceleratorSelectionForeground = defaults.getColor("Menu.acceleratorSelectionForeground");
    arrowIcon = defaults.getIcon("Menu.arrowIcon");
    oldBorderPainted = defaults.getBoolean("Menu.borderPainted");
  }

  /**
   * DOCUMENT ME!
   */
  protected void installKeyboardActions()
  {
  }

  /**
   * DOCUMENT ME!
   */
  protected void installListeners()
  {
    ((JMenu) menuItem).addMouseListener(mouseInputListener);
    ((JMenu) menuItem).addMenuListener(menuListener);
  }

  /**
   * DOCUMENT ME!
   *
   * @param menu DOCUMENT ME!
   */
  protected void setupPostTimer(JMenu menu)
  {
  }

  /**
   * DOCUMENT ME!
   */
  protected void uninstallDefaults()
  {
    menuItem.setBackground(null);
    menuItem.setBorder(null);
    menuItem.setFont(null);
    menuItem.setForeground(null);
    menuItem.setMargin(null);
    acceleratorFont = null;
    acceleratorForeground = null;
    acceleratorSelectionForeground = null;
    arrowIcon = null;
  }

  /**
   * DOCUMENT ME!
   */
  protected void uninstallKeyboardActions()
  {
  }

  /**
   * DOCUMENT ME!
   */
  protected void uninstallListeners()
  {
  }

  /**
  * DOCUMENT ME!
  *
  * @author $author$
  * @version $Revision: 1.2 $
  */
  protected class MouseInputHandler implements MouseInputListener
  {
    protected MouseInputHandler()
    {
    }

    public void mouseClicked(MouseEvent e)
    {
    }

    public void mouseDragged(MouseEvent e)
    {
      MenuSelectionManager manager = MenuSelectionManager.defaultManager();
      manager.processMouseEvent(e);
    }

    public void mouseEntered(MouseEvent e)
    {
      MenuSelectionManager manager = MenuSelectionManager.defaultManager();
      manager.setSelectedPath(getPath());
      manager.processMouseEvent(e);

      JMenu subMenu = (JMenu) menuItem;

      int x = 0;
      int y = 0;

      // location of the popup menu is relative to the invoker
      if (subMenu.isTopLevelMenu())
        {
	  JMenuBar mb = (JMenuBar) subMenu.getParent();

	  // Subtract menuBar's insets.bottom and popupMenu's insets.top, 
	  // s.t. the space between menu bar and its popup menu is equal to 
	  // menuBar's margin. By default menuBar's margin is Insets(0,0,0,0).
	  y = subMenu.getHeight() - mb.getInsets().bottom
	      - subMenu.getPopupMenu().getInsets().top + mb.getMargin().bottom;
        }
      else
	x = subMenu.getWidth();

      subMenu.getPopupMenu().show(subMenu, x, y);
    }

    public void mouseExited(MouseEvent e)
    {
    }

    public void mouseMoved(MouseEvent e)
    {
    }

    public void mousePressed(MouseEvent e)
    {
    }

    public void mouseReleased(MouseEvent e)
    {
      MenuSelectionManager manager = MenuSelectionManager.defaultManager();
      manager.processMouseEvent(e);
    }
  }

  protected class MenuHandler implements MenuListener
  {
    public void menuCanceled(MenuEvent e)
    {
    }

    public void menuDeselected(MenuEvent e)
    {
    }

    public void menuSelected(MenuEvent e)
    {
    }
  }
}
