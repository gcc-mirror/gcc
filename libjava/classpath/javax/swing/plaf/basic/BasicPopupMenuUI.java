/* BasicPopupMenuUI.java
   Copyright (C) 2002, 2004, 2005 Free Software Foundation, Inc.

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
import java.awt.KeyboardFocusManager;
import java.awt.event.ActionEvent;
import java.awt.event.ComponentEvent;
import java.awt.event.ComponentListener;
import java.awt.event.MouseEvent;
import java.util.EventListener;

import javax.swing.AbstractAction;
import javax.swing.Action;
import javax.swing.ActionMap;
import javax.swing.BoxLayout;
import javax.swing.InputMap;
import javax.swing.JApplet;
import javax.swing.JComponent;
import javax.swing.JFrame;
import javax.swing.JMenu;
import javax.swing.JMenuBar;
import javax.swing.JMenuItem;
import javax.swing.JPopupMenu;
import javax.swing.JRootPane;
import javax.swing.LookAndFeel;
import javax.swing.MenuElement;
import javax.swing.MenuSelectionManager;
import javax.swing.SwingUtilities;
import javax.swing.UIManager;
import javax.swing.event.ChangeEvent;
import javax.swing.event.ChangeListener;
import javax.swing.event.PopupMenuEvent;
import javax.swing.event.PopupMenuListener;
import javax.swing.plaf.ActionMapUIResource;
import javax.swing.plaf.ComponentUI;
import javax.swing.plaf.PopupMenuUI;

/**
 * UI Delegate for JPopupMenu
 */
public class BasicPopupMenuUI extends PopupMenuUI
{
  /**
   * Handles keyboard navigation through menus.
   */
  private static class NavigateAction
    extends AbstractAction
  {

    /**
     * Creates a new NavigateAction instance.
     *
     * @param name the name of the action
     */
    NavigateAction(String name)
    {
      super(name);
    }

    /**
     * Actually performs the action.
     */
    public void actionPerformed(ActionEvent event)
    {
      String name = (String) getValue(Action.NAME);
      if (name.equals("selectNext"))
        navigateNextPrevious(true);
      else if (name.equals("selectPrevious"))
        navigateNextPrevious(false);
      else if (name.equals("selectChild"))
        navigateParentChild(true);
      else if (name.equals("selectParent"))
        navigateParentChild(false);
      else if (name.equals("cancel"))
        cancel();
      else if (name.equals("return"))
        doReturn();
      else
        assert false : "Must not reach here";
    }

    /**
     * Navigates to the next or previous menu item.
     *
     * @param dir <code>true</code>: navigate to next, <code>false</code>:
     *        navigate to previous
     */
    private void navigateNextPrevious(boolean dir)
    {
      MenuSelectionManager msm = MenuSelectionManager.defaultManager();
      MenuElement path[] = msm.getSelectedPath();
      int len = path.length;
      if (len >= 2)
        {

          if (path[0] instanceof JMenuBar &&
              path[1] instanceof JMenu && len == 2)
            {

              // A toplevel menu is selected, but its popup not yet shown.
              // Show the popup and select the first item
              JPopupMenu popup = ((JMenu)path[1]).getPopupMenu();
              MenuElement next =
                findEnabledChild(popup.getSubElements(), -1, true);
              MenuElement[] newPath;

              if (next != null)
                {
                  newPath = new MenuElement[4];
                  newPath[3] = next;
                }
              else
                {
                  // Menu has no enabled items, show the popup anyway.
                  newPath = new MenuElement[3];
                }
              System.arraycopy(path, 0, newPath, 0, 2);
              newPath[2] = popup;
              msm.setSelectedPath(newPath);
            }
          else if (path[len - 1] instanceof JPopupMenu &&
                   path[len - 2] instanceof JMenu)
            {
              // Select next item in already shown popup menu.
              JMenu menu = (JMenu) path[len - 2];
              JPopupMenu popup = menu.getPopupMenu();
              MenuElement next =
                findEnabledChild(popup.getSubElements(), -1, dir);

              if (next != null)
                {
                  MenuElement[] newPath = new MenuElement[len + 1];
                  System.arraycopy(path, 0, newPath, 0, len);
                  newPath[len] = next;
                  msm.setSelectedPath(newPath);
                }
              else
                {
                  // All items in the popup are disabled.
                  // Find the parent popup menu and select
                  // its next item. If there's no parent popup menu , do nothing.
                  if (len > 2 && path[len - 3] instanceof JPopupMenu)
                    {
                      popup = ((JPopupMenu) path[len - 3]);
                      next = findEnabledChild(popup.getSubElements(),
                                              menu, dir);
                      if (next != null && next != menu)
                        {
                          MenuElement[] newPath = new MenuElement[len - 1];
                          System.arraycopy(path, 0, newPath, 0, len - 2);
                          newPath[len - 2] = next;
                          msm.setSelectedPath(newPath);
                        }
                    }
                }
            }
          else
            {
              // Only select the next item.
              MenuElement subs[] = path[len - 2].getSubElements();
              MenuElement nextChild =
                findEnabledChild(subs, path[len - 1], dir);
              if (nextChild == null)
                {
                  nextChild = findEnabledChild(subs, -1, dir);
                }
              if (nextChild != null)
                {
                  path[len-1] = nextChild;
                  msm.setSelectedPath(path);
                }
            }
        }
    }

    private MenuElement findEnabledChild(MenuElement[] children,
                                         MenuElement start, boolean dir)
    {
      MenuElement found = null;
      for (int i = 0; i < children.length && found == null; i++)
        {
          if (children[i] == start)
            {
              found = findEnabledChild(children, i, dir);
            }
        }
      return found;
    }

    /**
     * Searches the next or previous enabled child menu element.
     *
     * @param children the children to search through
     * @param start the index at which to start
     * @param dir the direction (true == forward, false == backward)
     *
     * @return the found element or null
     */
    private MenuElement findEnabledChild(MenuElement[] children,
                                         int start, boolean dir)
    {
      MenuElement result = null;
      if (dir)
        {
          result = findNextEnabledChild(children, start + 1, children.length-1);
          if (result == null)
            result = findNextEnabledChild(children, 0, start - 1);
        }
      else
        {
          result = findPreviousEnabledChild(children, start - 1, 0);
          if (result == null)
            result = findPreviousEnabledChild(children, children.length-1,
                                          start + 1);
        }
      return result;
    }

    /**
     * Finds the next child element that is enabled and visible.
     * 
     * @param children the children to search through
     * @param start the start index
     * @param end the end index
     *
     * @return the found child, or null
     */
    private MenuElement findNextEnabledChild(MenuElement[] children, int start,
                                             int end)
    {
      MenuElement found = null;
      for (int i = start; i <= end && found == null; i++)
        {
          if (children[i] != null)
            {
              Component comp = children[i].getComponent();
              if (comp != null && comp.isEnabled() && comp.isVisible())
                {
                  found = children[i];
                }
            }
        }
      return found;
    }

    /**
     * Finds the previous child element that is enabled and visible.
     * 
     * @param children the children to search through
     * @param start the start index
     * @param end the end index
     *
     * @return the found child, or null
     */
    private MenuElement findPreviousEnabledChild(MenuElement[] children,
                                                 int start, int end)
    {
      MenuElement found = null;
      for (int i = start; i >= end && found == null; i--)
        {
          if (children[i] != null)
            {
              Component comp = children[i].getComponent();
              if (comp != null && comp.isEnabled() && comp.isVisible())
                {
                  found = children[i];
                }
            }
        }
      return found;
    }

    /**
     * Navigates to the parent or child menu item.
     *
     * @param selectChild <code>true</code>: navigate to child,
     *        <code>false</code>: navigate to parent
     */
    private void navigateParentChild(boolean selectChild)
    {
      MenuSelectionManager msm = MenuSelectionManager.defaultManager();
      MenuElement path[] = msm.getSelectedPath();
      int len = path.length;

      if (selectChild)
        {
          if (len > 0 && path[len - 1] instanceof JMenu
              && ! ((JMenu) path[len-1]).isTopLevelMenu())
            {
              // We have a submenu, open it.
              JMenu menu = (JMenu) path[len - 1];
              JPopupMenu popup = menu.getPopupMenu();
              MenuElement[] subs = popup.getSubElements();
              MenuElement item = findEnabledChild(subs, -1, true);
              MenuElement[] newPath;

              if (item == null)
                {
                  newPath = new MenuElement[len + 1];
                }
              else
                {
                  newPath = new MenuElement[len + 2];
                  newPath[len + 1] = item;
                }
              System.arraycopy(path, 0, newPath, 0, len);
              newPath[len] = popup;
              msm.setSelectedPath(newPath);
              return;
            }
        }
      else
        {
          int popupIndex = len-1;
          if (len > 2 
              && (path[popupIndex] instanceof JPopupMenu
                  || path[--popupIndex] instanceof JPopupMenu)
                  && ! ((JMenu) path[popupIndex - 1]).isTopLevelMenu())
            {
              // We have a submenu, close it.
              MenuElement newPath[] = new MenuElement[popupIndex];
              System.arraycopy(path, 0, newPath, 0, popupIndex);
              msm.setSelectedPath(newPath);
              return;
            }
        }

      // If we got here, we have not selected a child or parent.
      // Check if we have a toplevel menu selected. If so, then select
      // another one.
      if (len > 1 && path[0] instanceof JMenuBar)
        {
          MenuElement currentMenu = path[1];
          MenuElement nextMenu = findEnabledChild(path[0].getSubElements(),
                                                  currentMenu, selectChild);

          if (nextMenu != null && nextMenu != currentMenu)
            {
              MenuElement newSelection[];
              if (len == 2)
                {
                  // Menu is selected but its popup not shown.
                  newSelection = new MenuElement[2];
                  newSelection[0] = path[0];
                  newSelection[1] = nextMenu;
                }
              else
                {
                  // Menu is selected and its popup is shown.
                  newSelection = new MenuElement[3];
                  newSelection[0] = path[0];
                  newSelection[1] = nextMenu;
                  newSelection[2] = ((JMenu) nextMenu).getPopupMenu();
                }
              msm.setSelectedPath(newSelection);
            }
        }
    }

    /**
     * Handles cancel requests (ESC key).
     */
    private void cancel()
    {
      // Fire popup menu cancelled event. Unfortunately the
      // firePopupMenuCancelled() is protected in JPopupMenu so we work
      // around this limitation by fetching the listeners and notifying them
      // directly.
      JPopupMenu lastPopup = (JPopupMenu) getLastPopup();
      EventListener[] ll = lastPopup.getListeners(PopupMenuListener.class);
      for (int i = 0; i < ll.length; i++)
        {
          PopupMenuEvent ev = new PopupMenuEvent(lastPopup);
          ((PopupMenuListener) ll[i]).popupMenuCanceled(ev);
        }

      // Close the last popup or the whole selection if there's only one
      // popup left.
      MenuSelectionManager msm = MenuSelectionManager.defaultManager();
      MenuElement path[] = msm.getSelectedPath();
      if(path.length > 4)
        {
          MenuElement newPath[] = new MenuElement[path.length - 2];
          System.arraycopy(path,0,newPath,0,path.length-2);
          MenuSelectionManager.defaultManager().setSelectedPath(newPath);
        }
      else
          msm.clearSelectedPath();
    }

    /**
     * Returns the last popup menu in the current selection or null.
     *
     * @return the last popup menu in the current selection or null
     */
    private JPopupMenu getLastPopup()
    {
      MenuSelectionManager msm = MenuSelectionManager.defaultManager();
      MenuElement[] p = msm.getSelectedPath();
      JPopupMenu popup = null;
      for(int i = p.length - 1; popup == null && i >= 0; i--)
        {
          if (p[i] instanceof JPopupMenu)
            popup = (JPopupMenu) p[i];
        }
      return popup;
    }

    /**
     * Handles ENTER key requests. This normally opens submenus on JMenu
     * items, or activates the menu item as if it's been clicked on it.
     */
    private void doReturn()
    {
      KeyboardFocusManager fmgr =
        KeyboardFocusManager.getCurrentKeyboardFocusManager();
      Component focusOwner = fmgr.getFocusOwner();
      if((focusOwner == null || (focusOwner instanceof JRootPane)))
        {
          MenuSelectionManager msm = MenuSelectionManager.defaultManager();
          MenuElement path[] = msm.getSelectedPath();
          MenuElement lastElement;
          if(path.length > 0)
            {
              lastElement = path[path.length - 1];
              if(lastElement instanceof JMenu)
                {
                  MenuElement newPath[] = new MenuElement[path.length + 1];
                  System.arraycopy(path,0,newPath,0,path.length);
                  newPath[path.length] = ((JMenu) lastElement).getPopupMenu();
                  msm.setSelectedPath(newPath);
                }
              else if(lastElement instanceof JMenuItem)
                {
                  JMenuItem mi = (JMenuItem)lastElement;
                  if (mi.getUI() instanceof BasicMenuItemUI)
                    {
                      ((BasicMenuItemUI)mi.getUI()).doClick(msm);
                    }
                  else
                    {
                      msm.clearSelectedPath();
                      mi.doClick(0);
                    }
                }
            }
        }
    }
  }

  /**
   * Installs keyboard actions when a popup is opened, and uninstalls the
   * keyboard actions when closed. This listens on the default
   * MenuSelectionManager.
   */
  private class KeyboardHelper
    implements ChangeListener
  {
    private MenuElement[] lastSelectedPath = new MenuElement[0];
    private Component lastFocused;
    private JRootPane invokerRootPane;

    public void stateChanged(ChangeEvent event)
    {
      MenuSelectionManager msm = (MenuSelectionManager) event.getSource();
      MenuElement[] p = msm.getSelectedPath();
      JPopupMenu popup = getActivePopup(p);
      if (popup == null || popup.isFocusable())
        {
          if (lastSelectedPath.length != 0 && p.length != 0 )
            {
              if (! invokerEquals(p[0], lastSelectedPath[0]))
                {
                  uninstallKeyboardActionsImpl();
                  lastSelectedPath = new MenuElement[0];
                }
            }

          if (lastSelectedPath.length == 0 && p.length > 0)
            {
              JComponent invoker;
              if (popup == null)
                {
                  if (p.length == 2 && p[0] instanceof JMenuBar
                      && p[1] instanceof JMenu)
                    {
                      // A menu has been selected but not opened.
                      invoker = (JComponent)p[1];
                      popup = ((JMenu)invoker).getPopupMenu();
                    }
                  else
                    {
                      return;
                    }
                }
              else
                {
                Component c = popup.getInvoker();
                if(c instanceof JFrame)
                  {
                    invoker = ((JFrame) c).getRootPane();
                  }
                else if(c instanceof JApplet)
                  {
                    invoker = ((JApplet) c).getRootPane();
                  }
                else
                  {
                    while (!(c instanceof JComponent))
                      {
                        if (c == null)
                          {
                            return;
                          }
                        c = c.getParent();
                      }
                    invoker = (JComponent)c;
                  }
                }

              // Remember current focus owner.
              lastFocused = KeyboardFocusManager.
                             getCurrentKeyboardFocusManager().getFocusOwner();

              // Install keybindings used for menu navigation.
              invokerRootPane = SwingUtilities.getRootPane(invoker);
              if (invokerRootPane != null)
                {
                  invokerRootPane.requestFocus(true);
                  installKeyboardActionsImpl();
                }
            }
          else if (lastSelectedPath.length != 0 && p.length == 0)
            {
              // menu hidden -- return focus to where it had been before
              // and uninstall menu keybindings
              uninstallKeyboardActionsImpl();
            }
        }

      // Remember the last path selected
      lastSelectedPath = p;
    }

    private JPopupMenu getActivePopup(MenuElement[] path)
    {
      JPopupMenu active = null;
      for (int i = path.length - 1; i >= 0 && active == null; i--)
        {
          MenuElement elem = path[i];
          if (elem instanceof JPopupMenu)
            {
              active = (JPopupMenu) elem;
            }
        }
      return active;
    }

    private boolean invokerEquals(MenuElement el1, MenuElement el2)
    {
      Component invoker1 = el1.getComponent();
      Component invoker2 = el2.getComponent();
      if (invoker1 instanceof JPopupMenu)
        invoker1 = ((JPopupMenu) invoker1).getInvoker();
      if (invoker2 instanceof JPopupMenu)
        invoker2 = ((JPopupMenu) invoker2).getInvoker();
      return invoker1 == invoker2;
    }
  }

  /* popupMenu for which this UI delegate is for*/
  protected JPopupMenu popupMenu;

  /* PopupMenuListener listens to popup menu events fired by JPopupMenu*/
  private transient PopupMenuListener popupMenuListener;

  /* ComponentListener listening to popupMenu's invoker.
   * This is package-private to avoid an accessor method.  */
  TopWindowListener topWindowListener;

  /**
   * Counts how many popup menus are handled by this UI or a subclass.
   * This is used to install a KeyboardHelper on the MenuSelectionManager
   * for the first popup, and uninstall this same KeyboardHelper when the
   * last popup is uninstalled.
   */
  private static int numPopups;

  /**
   * This is the KeyboardHelper that listens on the MenuSelectionManager.
   */
  private static KeyboardHelper keyboardHelper;

  /**
   * Creates a new BasicPopupMenuUI object.
   */
  public BasicPopupMenuUI()
  {
    popupMenuListener = new PopupMenuHandler();
    topWindowListener = new TopWindowListener();
  }

  /**
   * Factory method to create a BasicPopupMenuUI for the given {@link
   * JComponent}, which should be a {@link JMenuItem}.
   *
   * @param x The {@link JComponent} a UI is being created for.
   *
   * @return A BasicPopupMenuUI for the {@link JComponent}.
   */
  public static ComponentUI createUI(JComponent x)
  {
    return new BasicPopupMenuUI();
  }

  /**
   * Installs and initializes all fields for this UI delegate. Any properties
   * of the UI that need to be initialized and/or set to defaults will be
   * done now. It will also install any listeners necessary.
   *
   * @param c The {@link JComponent} that is having this UI installed.
   */
  public void installUI(JComponent c)
  {
    super.installUI(c);

    // Install KeyboardHelper when the first popup is initialized.
    if (numPopups == 0)
      {
        keyboardHelper = new KeyboardHelper();
        MenuSelectionManager msm = MenuSelectionManager.defaultManager();
        msm.addChangeListener(keyboardHelper);
      }
    numPopups++;

    popupMenu = (JPopupMenu) c;
    popupMenu.setLayout(new DefaultMenuLayout(popupMenu, BoxLayout.Y_AXIS));
    popupMenu.setBorderPainted(true);
    JPopupMenu.setDefaultLightWeightPopupEnabled(true);

    installDefaults();
    installListeners();
    installKeyboardActions();
  }

  /**
   * This method installs the defaults that are defined in  the Basic look
   * and feel for this {@link JPopupMenu}.
   */
  public void installDefaults()
  {
    LookAndFeel.installColorsAndFont(popupMenu, "PopupMenu.background",
                                     "PopupMenu.foreground", "PopupMenu.font");
    LookAndFeel.installBorder(popupMenu, "PopupMenu.border");
    popupMenu.setOpaque(true);
  }

  /**
   * This method installs the listeners for the {@link JMenuItem}.
   */
  protected void installListeners()
  {
    popupMenu.addPopupMenuListener(popupMenuListener);
  }

  /**
   * This method installs the keyboard actions for this {@link JPopupMenu}.
   */
  protected void installKeyboardActions()
  {
    // We can't install the keyboard actions here, because then all
    // popup menus would have their actions registered in the KeyboardManager.
    // So we install it when the popup menu is opened, and uninstall it
    // when it's closed. This is done in the KeyboardHelper class.
    // Install InputMap.
  }

  /**
   * Called by the KeyboardHandler when a popup is made visible.
   */
  void installKeyboardActionsImpl()
  {
    Object[] bindings;
    if (popupMenu.getComponentOrientation().isLeftToRight())
      {
        bindings = (Object[])
             SharedUIDefaults.get("PopupMenu.selectedWindowInputMapBindings");
      }
    else
      {
        bindings = (Object[]) SharedUIDefaults.get
                      ("PopupMenu.selectedWindowInputMapBindings.RightToLeft");
      }
    InputMap inputMap = LookAndFeel.makeComponentInputMap(popupMenu, bindings);
    SwingUtilities.replaceUIInputMap(popupMenu,
                                     JComponent.WHEN_IN_FOCUSED_WINDOW,
                                     inputMap);

    // Install ActionMap.
    SwingUtilities.replaceUIActionMap(popupMenu, getActionMap());
  }

  /**
   * Creates and returns the shared action map for JTrees.
   *
   * @return the shared action map for JTrees
   */
  private ActionMap getActionMap()
  {
    ActionMap am = (ActionMap) UIManager.get("PopupMenu.actionMap");
    if (am == null)
      {
        am = createDefaultActions();
        UIManager.getLookAndFeelDefaults().put("PopupMenu.actionMap", am);
      }
    return am;
  }

  /**
   * Creates the default actions when there are none specified by the L&F.
   *
   * @return the default actions
   */
  private ActionMap createDefaultActions()
  {
    ActionMapUIResource am = new ActionMapUIResource();
    Action action = new NavigateAction("selectNext");
    am.put(action.getValue(Action.NAME), action);
    action = new NavigateAction("selectPrevious");
    am.put(action.getValue(Action.NAME), action);
    action = new NavigateAction("selectParent");
    am.put(action.getValue(Action.NAME), action);
    action = new NavigateAction("selectChild");
    am.put(action.getValue(Action.NAME), action);
    action = new NavigateAction("return");
    am.put(action.getValue(Action.NAME), action);
    action = new NavigateAction("cancel");
    am.put(action.getValue(Action.NAME), action);
    
    return am;
  }

  /**
   * Performs the opposite of installUI. Any properties or resources that need
   * to be cleaned up will be done now. It will also uninstall any listeners
   * it has. In addition, any properties of this UI will be nulled.
   *
   * @param c The {@link JComponent} that is having this UI uninstalled.
   */
  public void uninstallUI(JComponent c)
  {
    uninstallListeners();
    uninstallDefaults();
    uninstallKeyboardActions();
    popupMenu = null;

    // Install KeyboardHelper when the first popup is initialized.
    numPopups--;
    if (numPopups == 0)
      {
        MenuSelectionManager msm = MenuSelectionManager.defaultManager();
        msm.removeChangeListener(keyboardHelper);
      }

  }

  /**
   * This method uninstalls the defaults and sets any objects created during
   * install to null
   */
  protected void uninstallDefaults()
  {
    popupMenu.setBackground(null);
    popupMenu.setBorder(null);
    popupMenu.setFont(null);
    popupMenu.setForeground(null);
  }

  /**
   * Unregisters all the listeners that this UI delegate was using.
   */
  protected void uninstallListeners()
  {
    popupMenu.removePopupMenuListener(popupMenuListener);
  }

  /**
   * Uninstalls any keyboard actions.
   */
  protected void uninstallKeyboardActions()
  {
    // We can't install the keyboard actions here, because then all
    // popup menus would have their actions registered in the KeyboardManager.
    // So we install it when the popup menu is opened, and uninstall it
    // when it's closed. This is done in the KeyboardHelper class.
    // Install InputMap.
  }

  /**
   * Called by the KeyboardHandler when a popup is made invisible.
   */
  void uninstallKeyboardActionsImpl()
  {
    SwingUtilities.replaceUIInputMap(popupMenu,
                                     JComponent.WHEN_IN_FOCUSED_WINDOW, null);
    SwingUtilities.replaceUIActionMap(popupMenu, null);
  }

  /**
   * This method returns the minimum size of the JPopupMenu.
   *
   * @param c The JComponent to find a size for.
   *
   * @return The minimum size.
   */
  public Dimension getMinimumSize(JComponent c)
  {
    return null;
  }

  /**
   * This method returns the preferred size of the JPopupMenu.
   *
   * @param c The JComponent to find a size for.
   *
   * @return The preferred size.
   */
  public Dimension getPreferredSize(JComponent c)
  {
    return null;
  }

  /**
   * This method returns the minimum size of the JPopupMenu.
   *
   * @param c The JComponent to find a size for.
   *
   * @return The minimum size.
   */
  public Dimension getMaximumSize(JComponent c)
  {
    return null;
  }

  /**
   * Return true if given mouse event is a platform popup trigger, and false
   * otherwise
   *
   * @param e MouseEvent that is to be checked for popup trigger event
   *
   * @return true if given mouse event is a platform popup trigger, and false
   *         otherwise
   */
  public boolean isPopupTrigger(MouseEvent e)
  {
    return false;
  }

  /**
   * This listener handles PopupMenuEvents fired by JPopupMenu
   */
  private class PopupMenuHandler implements PopupMenuListener
  {
    /**
     * This method is invoked when JPopupMenu is cancelled.
     *
     * @param event the PopupMenuEvent
     */
    public void popupMenuCanceled(PopupMenuEvent event)
    {
      MenuSelectionManager manager = MenuSelectionManager.defaultManager();
      manager.clearSelectedPath();
    }

    /**
     * This method is invoked when JPopupMenu becomes invisible
     *
     * @param event the PopupMenuEvent
     */
    public void popupMenuWillBecomeInvisible(PopupMenuEvent event)
    {
      // remove listener that listens to component events fired 
      // by the top - level window that this popup belongs to.
      Component invoker = popupMenu.getInvoker();
      Component rootContainer = SwingUtilities.getRoot(invoker);
      if (rootContainer != null)
        rootContainer.removeComponentListener(topWindowListener);
    }

    /**
     * This method is invoked when JPopupMenu becomes visible
     *
     * @param event the PopupMenuEvent
     */
    public void popupMenuWillBecomeVisible(PopupMenuEvent event)
    {
      // Adds topWindowListener to top-level window to listener to 
      // ComponentEvents fired by it. We need to cancel this popup menu
      // if topWindow to which this popup belongs was resized or moved.
      Component invoker = popupMenu.getInvoker();            
      Component rootContainer = SwingUtilities.getRoot(invoker);
      if (rootContainer != null)
        rootContainer.addComponentListener(topWindowListener);

      // if this popup menu is a free floating popup menu,
      // then by default its first element should be always selected when
      // this popup menu becomes visible. 
      MenuSelectionManager manager = MenuSelectionManager.defaultManager();

      if (manager.getSelectedPath().length == 0)
        {
	  // Set selected path to point to the first item in the popup menu
	  MenuElement[] path = new MenuElement[2];
	  path[0] = popupMenu;
	  Component[] comps = popupMenu.getComponents();
	  if (comps.length != 0 && comps[0] instanceof MenuElement)
	    {
	      path[1] = (MenuElement) comps[0];
	      manager.setSelectedPath(path);
	    }
        }
    }
  }

  /**
   * ComponentListener that listens to Component Events fired by the top -
   * level window to which popup menu belongs. If top-level window was
   * resized, moved or hidded then popup menu will be hidded and selected
   * path of current menu hierarchy will be set to null.
   */
  private class TopWindowListener implements ComponentListener
  {
    /**
     * This method is invoked when top-level window is resized. This method
     * closes current menu hierarchy.
     *
     * @param e The ComponentEvent
     */
    public void componentResized(ComponentEvent e)
    {
      MenuSelectionManager manager = MenuSelectionManager.defaultManager();
      manager.clearSelectedPath();
    }

    /**
     * This method is invoked when top-level window is moved. This method
     * closes current menu hierarchy.
     *
     * @param e The ComponentEvent
     */
    public void componentMoved(ComponentEvent e)
    {
      MenuSelectionManager manager = MenuSelectionManager.defaultManager();
      manager.clearSelectedPath();
    }

    /**
     * This method is invoked when top-level window is shown This method
     * does nothing by default.
     *
     * @param e The ComponentEvent
     */
    public void componentShown(ComponentEvent e)
    {
      MenuSelectionManager manager = MenuSelectionManager.defaultManager();
      manager.clearSelectedPath();
    }

    /**
     * This method is invoked when top-level window is hidden This method
     * closes current menu hierarchy.
     *
     * @param e The ComponentEvent
     */
    public void componentHidden(ComponentEvent e)
    {
      MenuSelectionManager manager = MenuSelectionManager.defaultManager();
      manager.clearSelectedPath();
    }
  }

}
