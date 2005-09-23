/* BasicMenuUI.java
   Copyright (C) 2002, 2004, 2005  Free Software Foundation, Inc.

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

import java.awt.Dimension;
import java.awt.event.MouseEvent;
import java.beans.PropertyChangeEvent;
import java.beans.PropertyChangeListener;

import javax.swing.JComponent;
import javax.swing.JMenu;
import javax.swing.JMenuBar;
import javax.swing.JMenuItem;
import javax.swing.JPopupMenu;
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

/**
 * UI Delegate for JMenu
 */
public class BasicMenuUI extends BasicMenuItemUI
{
  protected ChangeListener changeListener;

  /* MenuListener listens to MenuEvents fired by JMenu */
  protected MenuListener menuListener;

  /* PropertyChangeListner that listens to propertyChangeEvents occuring in JMenu*/
  protected PropertyChangeListener propertyChangeListener;

  /**
   * Creates a new BasicMenuUI object.
   */
  public BasicMenuUI()
  {
    mouseInputListener = createMouseInputListener((JMenu) menuItem);
    menuListener = createMenuListener((JMenu) menuItem);
    propertyChangeListener = createPropertyChangeListener((JMenu) menuItem);
  }

  /**
   * This method creates a new ChangeListener.
   *
   * @return A new ChangeListener.
   */
  protected ChangeListener createChangeListener(JComponent c)
  {
    return new ChangeHandler();
  }

  /**
   * This method creates new MenuDragMouseListener to listen to mouse dragged events
   * occuring in the Menu
   *
   * @param c the menu to listen to
   *
   * @return The MenuDrageMouseListener
   */
  protected MenuDragMouseListener createMenuDragMouseListener(JComponent c)
  {
    return new MenuDragMouseHandler();
  }

  /**
   * This method creates new MenuDragKeyListener to listen to key events
   *
   * @param c the menu to listen to
   *
   * @return The MenuKeyListener
   */
  protected MenuKeyListener createMenuKeyListener(JComponent c)
  {
    return new MenuKeyHandler();
  }

  /**
   * This method creates new MenuListener to listen to menu events
   * occuring in the Menu
   *
   * @param c the menu to listen to
   *
   * @return The MenuListener
   */
  protected MenuListener createMenuListener(JComponent c)
  {
    return new MenuHandler();
  }

  /**
   * This method creates new MouseInputListener to listen to mouse input events
   * occuring in the Menu
   *
   * @param c the menu to listen to
   *
   * @return The MouseInputListener
   */
  protected MouseInputListener createMouseInputListener(JComponent c)
  {
    return new MouseInputHandler();
  }

  /**
   * This method creates newPropertyChangeListener to listen to property changes
   * occuring in the Menu
   *
   * @param c the menu to listen to
   *
   * @return The PropertyChangeListener
   */
  protected PropertyChangeListener createPropertyChangeListener(JComponent c)
  {
    return new PropertyChangeHandler();
  }

  /**
   * This method creates a new BasicMenuUI.
   *
   * @param c The JComponent to create a UI for.
   *
   * @return A new BasicMenuUI.
   */
  public static ComponentUI createUI(JComponent c)
  {
    return new BasicMenuUI();
  }

  /**
   * Get the component's maximum size.
   *
   * @param c The JComponent for which to get maximum size
   *
   * @return The maximum size of the component
   */
  public Dimension getMaximumSize(JComponent c)
  {
    // If this menu is in a popup menu, treat it like a regular JMenuItem
    if (!((JMenu)c).isTopLevelMenu())
      {
        JMenuItem menuItem = new JMenuItem(((JMenu)c).getText(), ((JMenu)c).getIcon());
        return menuItem.getMaximumSize();
      }
    return c.getPreferredSize();
  }

  /**
   * Returns the prefix for entries in the {@link UIDefaults} table.
   *
   * @return "Menu"
   */
  protected String getPropertyPrefix()
  {
    return "Menu";
  }

  /**
   * Initializes any default properties that this UI has from the defaults for
   * the Basic look and feel.
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
    selectionBackground = defaults.getColor("Menu.selectionBackground");
    selectionForeground = defaults.getColor("Menu.selectionForeground");
    arrowIcon = defaults.getIcon("Menu.arrowIcon");
    oldBorderPainted = defaults.getBoolean("Menu.borderPainted");
    menuItem.setOpaque(true);
  }

  /**
   * Installs any keyboard actions. The list of keys that need to be bound are
   * listed in Basic look and feel's defaults.
   *
   */
  protected void installKeyboardActions()
  {
    // FIXME: Need to implement
  }

  /**
   * Creates and registers all the listeners for this UI delegate.
   */
  protected void installListeners()
  {
    ((JMenu) menuItem).addMouseListener(mouseInputListener);
    ((JMenu) menuItem).addMouseMotionListener(mouseInputListener);
    ((JMenu) menuItem).addMenuListener(menuListener);
    ((JMenu) menuItem).addMenuDragMouseListener(menuDragMouseListener);
  }

  protected void setupPostTimer(JMenu menu)
  {
  }

  /**
   * This method uninstalls the defaults and sets any objects created during
   * install to null
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
    selectionBackground = null;
    selectionForeground = null;
    arrowIcon = null;
  }

  /**
   * Uninstalls any keyboard actions. The list of keys used  are listed in
   * Basic look and feel's defaults.
   */
  protected void uninstallKeyboardActions()
  {
    // FIXME: Need to implement
  }

  /**
   * Unregisters all the listeners that this UI delegate was using. In
   * addition, it will also null any listeners that it was using.
   */
  protected void uninstallListeners()
  {
    ((JMenu) menuItem).removeMouseListener(mouseInputListener);
    ((JMenu) menuItem).removeMenuListener(menuListener);
    ((JMenu) menuItem).removePropertyChangeListener(propertyChangeListener);
  }

  /**
   * This class is used by menus to handle mouse events occuring in the
   * menu.
   */
  protected class MouseInputHandler implements MouseInputListener
  {
    public void mouseClicked(MouseEvent e)
    {
      MenuSelectionManager manager = MenuSelectionManager.defaultManager();
      manager.processMouseEvent(e);
    }

    public void mouseDragged(MouseEvent e)
    {
      MenuSelectionManager manager = MenuSelectionManager.defaultManager();
      manager.processMouseEvent(e);
    }

    private boolean popupVisible()
    {
      JMenuBar mb = (JMenuBar) ((JMenu) menuItem).getParent();
      // check if mb.isSelected because if no menus are selected
      // we don't have to look through the list for popup menus
      if (!mb.isSelected())
        return false;
      for (int i = 0; i < mb.getMenuCount(); i++)
      {
         JMenu m = mb.getMenu(i);
        if (m != null && m.isPopupMenuVisible())
          return true;
      }
      return false;
    }

    public void mouseEntered(MouseEvent e)
    {
      /* When mouse enters menu item, it should be considered selected

       if (i) if this menu is a submenu in some other menu
          (ii) or if this menu is in a menu bar and some other menu in a 
          menu bar was just selected and has its popup menu visible. 
               (If nothing was selected, menu should be pressed before
               it will be selected)
      */
      JMenu menu = (JMenu) menuItem;

      // NOTE: the following if used to require !menu.isArmed but I could find
      // no reason for this and it was preventing some JDK-compatible behaviour.
      // Specifically, if a menu is selected but its popup menu not visible,
      // and then another menu is selected whose popup menu IS visible, when
      // the mouse is moved over the first menu, its popup menu should become
      // visible.

      if (! menu.isTopLevelMenu() || popupVisible())
        {
	  // set new selection and forward this event to MenuSelectionManager
	  MenuSelectionManager manager = MenuSelectionManager.defaultManager();
	  manager.setSelectedPath(getPath());
	  manager.processMouseEvent(e);
        }
    }

    public void mouseExited(MouseEvent e)
    {
      MenuSelectionManager manager = MenuSelectionManager.defaultManager();
      manager.processMouseEvent(e);
    }

    public void mouseMoved(MouseEvent e)
    {
    }

    public void mousePressed(MouseEvent e)
    {
      MenuSelectionManager manager = MenuSelectionManager.defaultManager();
      JMenu menu = (JMenu) menuItem;
      manager.processMouseEvent(e);

      // Menu should be displayed when the menu is pressed only if 
      // it is top-level menu
      if (menu.isTopLevelMenu())
        {
	  if (menu.getPopupMenu().isVisible())
	    // If menu is visible and menu button was pressed.. 
	    // then need to cancel the menu
	    manager.clearSelectedPath();
	  else
	    {
	      // Display the menu
	      int x = 0;
	      int y = menu.getHeight();

	      manager.setSelectedPath(getPath());

	      JMenuBar mb = (JMenuBar) menu.getParent();

	      // set selectedIndex of the selectionModel of a menuBar
	      mb.getSelectionModel().setSelectedIndex(mb.getComponentIndex(menu));
	    }
        }
    }

    public void mouseReleased(MouseEvent e)
    {
      MenuSelectionManager manager = MenuSelectionManager.defaultManager();
      manager.processMouseEvent(e);
    }
  }

  /**
   * This class handles MenuEvents fired by the JMenu
   */
  protected class MenuHandler implements MenuListener
  {
    /**
     * This method is called when menu is cancelled. The menu is cancelled
     * when its popup menu is closed without selection. It clears selected index
     * in the selectionModel of the menu parent.
     *
     * @param e The MenuEvent.
     */
    public void menuCanceled(MenuEvent e)
    {
      menuDeselected(e);
    }

    /**
     * This method is called when menu is deselected. It clears selected index
     * in the selectionModel of the menu parent.
     *
     * @param e The MenuEvent.
     */
    public void menuDeselected(MenuEvent e)
    {
      JMenu menu = (JMenu) menuItem;
      if (menu.isTopLevelMenu())
	((JMenuBar) menu.getParent()).getSelectionModel().clearSelection();
      else
	((JPopupMenu) menu.getParent()).getSelectionModel().clearSelection();
    }

    /**
     * This method is called when menu is selected.  It sets selected index
     * in the selectionModel of the menu parent.
     *
     * @param e The MenuEvent.
     */
    public void menuSelected(MenuEvent e)
    {
      JMenu menu = (JMenu) menuItem;
      if (menu.isTopLevelMenu())
	((JMenuBar) menu.getParent()).setSelected(menu);
      else
	((JPopupMenu) menu.getParent()).setSelected(menu);
    }
  }

  /**
   * This class handles PropertyChangeEvents fired from the JMenu
   */
  protected class PropertyChangeHandler implements PropertyChangeListener
  {
    /**
      * This method is called whenever one of the properties of the menu item
      * changes.
      *
      * @param e The PropertyChangeEvent.
      */
    public void propertyChange(PropertyChangeEvent e)
    {
    }
  }

  /**
   * @deprecated
   */
  public class ChangeHandler implements ChangeListener
  {
    public void stateChanged(ChangeEvent e)
    {
      // FIXME: It seems that this class is not used anywhere
    }
  }

  /**
   * This class handles mouse dragged events occuring in the menu.
   */
  protected class MenuDragMouseHandler implements MenuDragMouseListener
  {
    /**
     * This method is invoked when mouse is dragged over the menu item.
     *
     * @param e The MenuDragMouseEvent
     */
    public void menuDragMouseDragged(MenuDragMouseEvent e)
    {
      MenuSelectionManager manager = MenuSelectionManager.defaultManager();
      manager.setSelectedPath(e.getPath());
    }

    /**
     * This method is invoked when mouse enters the menu item while it is
     * being dragged.
     *
     * @param e The MenuDragMouseEvent
     */
    public void menuDragMouseEntered(MenuDragMouseEvent e)
    {
      MenuSelectionManager manager = MenuSelectionManager.defaultManager();
      manager.setSelectedPath(e.getPath());
    }

    /**
     * This method is invoked when mouse exits the menu item while
     * it is being dragged
     *
     * @param e The MenuDragMouseEvent
     */
    public void menuDragMouseExited(MenuDragMouseEvent e)
    {
    }

    /**
     * This method is invoked when mouse was dragged and released
     * inside the menu item.
     *
     * @param e The MenuDragMouseEvent
     */
    public void menuDragMouseReleased(MenuDragMouseEvent e)
    {
    }
  }

  /**
   * This class handles key events occuring when menu item is visible on the
   * screen.
   */
  protected class MenuKeyHandler implements MenuKeyListener
  {
    /**
     * This method is invoked when key has been pressed
     *
     * @param e A {@link MenuKeyEvent}.
     */
    public void menuKeyPressed(MenuKeyEvent e)
    {
    }

    /**
     * This method is invoked when key has been pressed
     *
     * @param e A {@link MenuKeyEvent}.
     */
    public void menuKeyReleased(MenuKeyEvent e)
    {
    }

    /**
     * This method is invoked when key has been typed
     * It handles the mnemonic key for the menu item.
     *
     * @param e A {@link MenuKeyEvent}.
     */
    public void menuKeyTyped(MenuKeyEvent e)
    {
    }
  }
}
