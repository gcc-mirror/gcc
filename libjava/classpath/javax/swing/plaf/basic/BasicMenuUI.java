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

import gnu.classpath.NotImplementedException;

import java.awt.Component;
import java.awt.Container;
import java.awt.Dimension;
import java.awt.Point;
import java.awt.event.ActionEvent;
import java.awt.event.MouseEvent;
import java.beans.PropertyChangeListener;

import javax.swing.AbstractAction;
import javax.swing.JComponent;
import javax.swing.JMenu;
import javax.swing.JMenuBar;
import javax.swing.JPopupMenu;
import javax.swing.LookAndFeel;
import javax.swing.MenuElement;
import javax.swing.MenuSelectionManager;
import javax.swing.Timer;
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
  /**
   * Selects a menu. This is used to delay menu selection.
   */
  class SelectMenuAction
    extends AbstractAction
  {
    /**
     * Performs the action.
     */
    public void actionPerformed(ActionEvent event)
    {
      JMenu menu = (JMenu) menuItem;
      MenuSelectionManager defaultManager =
        MenuSelectionManager.defaultManager();
      MenuElement path[] = defaultManager.getSelectedPath();
      if(path.length > 0 && path[path.length - 1] == menu)
        {
          MenuElement newPath[] = new MenuElement[path.length + 1];
          System.arraycopy(path, 0, newPath, 0, path.length);
          newPath[path.length] = menu.getPopupMenu();
          defaultManager.setSelectedPath(newPath);
      }
    }

  }

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
    return new ChangeHandler((JMenu) c, this);
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

    LookAndFeel.installBorder(menuItem, "Menu.border");
    LookAndFeel.installColorsAndFont(menuItem, "Menu.background",
                                     "Menu.foreground", "Menu.font");
    menuItem.setMargin(UIManager.getInsets("Menu.margin"));
    acceleratorFont = UIManager.getFont("Menu.acceleratorFont");
    acceleratorForeground = UIManager.getColor("Menu.acceleratorForeground");
    acceleratorSelectionForeground = UIManager.getColor("Menu.acceleratorSelectionForeground");
    selectionBackground = UIManager.getColor("Menu.selectionBackground");
    selectionForeground = UIManager.getColor("Menu.selectionForeground");
    arrowIcon = UIManager.getIcon("Menu.arrowIcon");
    oldBorderPainted = UIManager.getBoolean("Menu.borderPainted");
    ((JMenu) menuItem).setDelay(200);
  }

  /**
   * Installs any keyboard actions. The list of keys that need to be bound are
   * listed in Basic look and feel's defaults.
   *
   */
  protected void installKeyboardActions()
  {
    super.installKeyboardActions();
  }

  /**
   * Creates and registers all the listeners for this UI delegate.
   */
  protected void installListeners()
  {
    super.installListeners();
    ((JMenu) menuItem).addMenuListener(menuListener);
  }

  protected void setupPostTimer(JMenu menu)
  {
    Timer timer = new Timer(menu.getDelay(), new SelectMenuAction());
    timer.setRepeats(false);
    timer.start();
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
    super.installKeyboardActions();
  }

  /**
   * Unregisters all the listeners that this UI delegate was using. In
   * addition, it will also null any listeners that it was using.
   */
  protected void uninstallListeners()
  {
    super.uninstallListeners();
    ((JMenu) menuItem).removeMenuListener(menuListener);
  }

  /**
   * This class is used by menus to handle mouse events occuring in the
   * menu.
   */
  protected class MouseInputHandler implements MouseInputListener
  {
    public void mouseClicked(MouseEvent e)
    {
      // Nothing to do here.
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
      JMenu menu = (JMenu) menuItem;
      if (menu.isEnabled())
        {
          MenuSelectionManager manager =
            MenuSelectionManager.defaultManager();
          MenuElement[] selectedPath = manager.getSelectedPath();
          if (! menu.isTopLevelMenu())
            {
              // Open the menu immediately or delayed, depending on the
              // delay value.
              if(! (selectedPath.length > 0
                  && selectedPath[selectedPath.length - 1] == menu.getPopupMenu()))
                {
                  if(menu.getDelay() == 0)
                    {
                      MenuElement[] path = getPath();
                      MenuElement[] newPath = new MenuElement[path.length + 1];
                      System.arraycopy(path, 0, newPath, 0, path.length);
                      newPath[path.length] = menu.getPopupMenu();
                      manager.setSelectedPath(newPath);
                    }
                  else
                    {
                      manager.setSelectedPath(getPath());
                      setupPostTimer(menu);
                    }
                }
            }
          else
            {
              if(selectedPath.length > 0
                  && selectedPath[0] == menu.getParent())
                {
                  MenuElement[] newPath = new MenuElement[3];
                  newPath[0] = (MenuElement) menu.getParent();
                  newPath[1] = menu;
                  newPath[2] = menu.getPopupMenu();
                  manager.setSelectedPath(newPath);
                }
            }
        }
    }

    public void mouseExited(MouseEvent e)
    {
      MenuSelectionManager manager = MenuSelectionManager.defaultManager();
      manager.processMouseEvent(e);
    }

    public void mouseMoved(MouseEvent e)
    {
      // Nothing to do here.
    }

    public void mousePressed(MouseEvent e)
    {
      MenuSelectionManager manager = MenuSelectionManager.defaultManager();
      JMenu menu = (JMenu) menuItem;
      if (menu.isEnabled())
        {
          // Open up the menu immediately if it's a toplevel menu.
          // But not yet the popup, which might be opened delayed, see below.
          if (menu.isTopLevelMenu())
            {
              if (menu.isSelected())
                manager.clearSelectedPath();
              else
                {
                  Container cnt = menu.getParent();
                  if (cnt != null && cnt instanceof JMenuBar)
                    {
                      MenuElement[] me = new MenuElement[2];
                      me[0] = (MenuElement) cnt;
                      me[1] = menu;
                      manager.setSelectedPath(me);
                   }
                }
            }

          // Open the menu's popup. Either do that immediately if delay == 0,
          // or delayed when delay > 0.
          MenuElement[] selectedPath = manager.getSelectedPath();
          if (selectedPath.length > 0
              && selectedPath[selectedPath.length - 1] != menu.getPopupMenu())
            {
              if(menu.isTopLevelMenu() || menu.getDelay() == 0)
                {
                  MenuElement[] newPath =
                    new MenuElement[selectedPath.length + 1];
                  System.arraycopy(selectedPath, 0, newPath, 0,
                                   selectedPath.length);
                  newPath[selectedPath.length] = menu.getPopupMenu();
                  manager.setSelectedPath(newPath);
                }
              else
                {
                  setupPostTimer(menu);
                }
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
  private class MenuHandler implements MenuListener
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
      if (menu.getParent() != null)
        {
          if (menu.isTopLevelMenu())
            ((JMenuBar) menu.getParent()).getSelectionModel().clearSelection();
          else
            ((JPopupMenu) menu.getParent()).getSelectionModel().clearSelection();
        }
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
   * Obsolete as of JDK1.4.
   */
  public class ChangeHandler implements ChangeListener
  {
    /**
     * Not used.
     */
    public boolean isSelected;

    /**
     * Not used.
     */
    public JMenu menu;

    /**
     * Not used.
     */
    public BasicMenuUI ui;

    /**
     * Not used.
     */
    public Component wasFocused;

    /**
     * Not used.
     */
    public ChangeHandler(JMenu m, BasicMenuUI ui)
    {
      menu = m;
      this.ui = ui;
    }

    /**
     * Not used.
     */
    public void stateChanged(ChangeEvent e)
    {
      // Not used.
    }
  }

  /**
   * This class handles mouse dragged events occuring in the menu.
   */
  private class MenuDragMouseHandler implements MenuDragMouseListener
  {
    /**
     * This method is invoked when mouse is dragged over the menu item.
     *
     * @param e The MenuDragMouseEvent
     */
    public void menuDragMouseDragged(MenuDragMouseEvent e)
    {
      if (menuItem.isEnabled())
        {
          MenuSelectionManager manager = e.getMenuSelectionManager();
          MenuElement path[] = e.getPath();

          Point p = e.getPoint();
          if(p.x >= 0 && p.x < menuItem.getWidth()
              && p.y >= 0 && p.y < menuItem.getHeight())
            {
              JMenu menu = (JMenu) menuItem;
              MenuElement[] selectedPath = manager.getSelectedPath();
              if(! (selectedPath.length > 0
                  && selectedPath[selectedPath.length-1]
                                  == menu.getPopupMenu()))
                {
                  if(menu.isTopLevelMenu() || menu.getDelay() == 0
                     || e.getID() == MouseEvent.MOUSE_DRAGGED)
                    {
                      MenuElement[] newPath = new MenuElement[path.length + 1];
                      System.arraycopy(path, 0, newPath, 0, path.length);
                      newPath[path.length] = menu.getPopupMenu();
                      manager.setSelectedPath(newPath);
                    }
                  else
                    {
                      manager.setSelectedPath(path);
                      setupPostTimer(menu);
                    }
                }
            }
          else if (e.getID() == MouseEvent.MOUSE_RELEASED)
            {
              Component comp = manager.componentForPoint(e.getComponent(),
                                                         e.getPoint());
              if (comp == null)
                manager.clearSelectedPath();
            }
        }
    }

    /**
     * This method is invoked when mouse enters the menu item while it is
     * being dragged.
     *
     * @param e The MenuDragMouseEvent
     */
    public void menuDragMouseEntered(MenuDragMouseEvent e)
    {
      // Nothing to do here.
    }

    /**
     * This method is invoked when mouse exits the menu item while
     * it is being dragged
     *
     * @param e The MenuDragMouseEvent
     */
    public void menuDragMouseExited(MenuDragMouseEvent e)
    {
      // Nothing to do here.
    }

    /**
     * This method is invoked when mouse was dragged and released
     * inside the menu item.
     *
     * @param e The MenuDragMouseEvent
     */
    public void menuDragMouseReleased(MenuDragMouseEvent e)
    {
      // Nothing to do here.
    }
  }

  /**
   * This class handles key events occuring when menu item is visible on the
   * screen.
   */
  private class MenuKeyHandler implements MenuKeyListener
  {
    /**
     * This method is invoked when key has been pressed
     *
     * @param e A {@link MenuKeyEvent}.
     */
    public void menuKeyPressed(MenuKeyEvent e)
    {
      // Nothing to do here.
    }

    /**
     * This method is invoked when key has been pressed
     *
     * @param e A {@link MenuKeyEvent}.
     */
    public void menuKeyReleased(MenuKeyEvent e)
    {
      // Nothing to do here.
    }

    /**
     * This method is invoked when key has been typed
     * It handles the mnemonic key for the menu item.
     *
     * @param e A {@link MenuKeyEvent}.
     */
    public void menuKeyTyped(MenuKeyEvent e)
    throws NotImplementedException
    {
      // TODO: What should be done here, if anything?
    }
  }
}
