/* JMenu.java --
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
import java.awt.Point;
import java.awt.event.KeyEvent;
import java.awt.event.WindowAdapter;
import java.awt.event.WindowEvent;
import java.beans.PropertyChangeListener;
import java.io.IOException;
import java.io.ObjectOutputStream;
import java.io.Serializable;
import java.util.EventListener;
import java.util.Hashtable;
import javax.accessibility.Accessible;
import javax.accessibility.AccessibleContext;
import javax.accessibility.AccessibleRole;
import javax.accessibility.AccessibleSelection;
import javax.swing.event.ChangeListener;
import javax.swing.event.MenuEvent;
import javax.swing.event.MenuListener;
import javax.swing.plaf.MenuItemUI;


/**
 * DOCUMENT ME!
 */
public class JMenu extends JMenuItem implements Accessible, MenuElement
{
  static final long serialVersionUID = 4227225638931828014L;
  private static final String uiClassID = "MenuUI";
  private static Hashtable listenerRegistry = null;
  private JPopupMenu popupMenu = new JPopupMenu();
  private ChangeListener menuChangeListener;
  private MenuEvent menuEvent;
  private int delay;
  protected JMenu.WinListener popupListener;

  /**
   * Creates a new JMenu object.
   */
  public JMenu()
  {
    super();
  }

  /**
   * Creates a new JMenu object.
   *
   * @param text DOCUMENT ME!
   */
  public JMenu(String text)
  {
    super(text);
  }

  /**
   * Creates a new JMenu object.
   *
   * @param action DOCUMENT ME!
   */
  public JMenu(Action action)
  {
    super(action);
  }

  /**
   * Creates a new JMenu object.
   *
   * @param text DOCUMENT ME!
   * @param tearoff DOCUMENT ME!
   */
  public JMenu(String text, boolean tearoff)
  {
  }

  /**
   * DOCUMENT ME!
   *
   * @param stream DOCUMENT ME!
   *
   * @throws IOException DOCUMENT ME!
   */
  private void writeObject(ObjectOutputStream stream) throws IOException
  {
  }

  /**
   * DOCUMENT ME!
   *
   * @param item DOCUMENT ME!
   *
   * @return DOCUMENT ME!
   */
  public JMenuItem add(JMenuItem item)
  {
    return popupMenu.add(item);
  }

  /**
   * DOCUMENT ME!
   *
   * @param component DOCUMENT ME!
   *
   * @return DOCUMENT ME!
   */
  public Component add(Component component)
  {
    return popupMenu.add(component);
  }

  /**
   * DOCUMENT ME!
   *
   * @param component DOCUMENT ME!
   * @param index DOCUMENT ME!
   *
   * @return DOCUMENT ME!
   */
  public Component add(Component component, int index)
  {
    return popupMenu.add(component, index);
  }

  /**
   * DOCUMENT ME!
   *
   * @param text DOCUMENT ME!
   *
   * @return DOCUMENT ME!
   */
  public JMenuItem add(String text)
  {
    return popupMenu.add(text);
  }

  /**
   * DOCUMENT ME!
   *
   * @param action DOCUMENT ME!
   *
   * @return DOCUMENT ME!
   */
  public JMenuItem add(Action action)
  {
    return popupMenu.add(action);
  }

  /**
   * DOCUMENT ME!
   *
   * @param item DOCUMENT ME!
   */
  public void remove(JMenuItem item)
  {
    popupMenu.remove(item);
  }

  /**
   * DOCUMENT ME!
   *
   * @param index DOCUMENT ME!
   */
  public void remove(int index)
  {
    popupMenu.remove(index);
  }

  /**
   * DOCUMENT ME!
   *
   * @param component DOCUMENT ME!
   */
  public void remove(Component component)
  {
    int index = popupMenu.getComponentIndex(component);
    popupMenu.remove(index);
  }

  /**
   * DOCUMENT ME!
   */
  public void removeAll()
  {
    popupMenu.removeAll();
  }

  /**
   * DOCUMENT ME!
   *
   * @param text DOCUMENT ME!
   * @param index DOCUMENT ME!
   */
  public void insert(String text, int index)
  {
    popupMenu.insert(new JMenuItem(text), index);
  }

  /**
   * DOCUMENT ME!
   *
   * @param item DOCUMENT ME!
   * @param index DOCUMENT ME!
   *
   * @return DOCUMENT ME!
   */
  public JMenuItem insert(JMenuItem item, int index)
  {
    popupMenu.insert(item, index);

    return item;
  }

  /**
   * DOCUMENT ME!
   *
   * @param action DOCUMENT ME!
   * @param index DOCUMENT ME!
   *
   * @return DOCUMENT ME!
   */
  public JMenuItem insert(Action action, int index)
  {
    JMenuItem item = new JMenuItem(action);
    popupMenu.insert(item, index);

    return item;
  }

  /**
   * DOCUMENT ME!
   */
  public void updateUI()
  {
    super.setUI((MenuItemUI) UIManager.getUI(this));
    invalidate();
  }

  /**
   * DOCUMENT ME!
   *
   * @return DOCUMENT ME!
   */
  public String getUIClassID()
  {
    return uiClassID;
  }

  /**
   * DOCUMENT ME!
   *
   * @param model DOCUMENT ME!
   */
  public void setModel(ButtonModel model)
  {
    super.setModel(model);
  }

  /**
   * DOCUMENT ME!
   *
   * @return DOCUMENT ME!
   */
  public boolean isSelected()
  {
    return super.isSelected();
  }

  /**
   * DOCUMENT ME!
   *
   * @param selected DOCUMENT ME!
   */
  public void setSelected(boolean selected)
  {
    super.setSelected(selected);
  }

  /**
   * DOCUMENT ME!
   *
   * @return DOCUMENT ME!
   */
  public boolean isPopupMenuVisible()
  {
    return popupMenu.isVisible();
  }

  /**
   * DOCUMENT ME!
   *
   * @param popup DOCUMENT ME!
   */
  public void setPopupMenuVisible(boolean popup)
  {
    if (isEnabled())
      popupMenu.setVisible(popup);
  }

  /**
   * DOCUMENT ME!
   *
   * @return DOCUMENT ME!
   */
  protected Point getPopupMenuOrigin()
  {
    // if menu in the menu bar
    if (isTopLevelMenu())
      return new Point(0, this.getHeight());

    // if submenu            
    return new Point(this.getWidth(), 0);
  }

  /**
   * DOCUMENT ME!
   *
   * @return DOCUMENT ME!
   */
  public int getDelay()
  {
    return delay;
  }

  /**
   * DOCUMENT ME!
   *
   * @param delay DOCUMENT ME!
   */
  public void setDelay(int delay)
  {
    this.delay = delay;
  }

  /**
   * DOCUMENT ME!
   *
   * @param x DOCUMENT ME!
   * @param y DOCUMENT ME!
   */
  public void setMenuLocation(int x, int y)
  {
    popupMenu.setLocation(x, y);
  }

  /**
   * DOCUMENT ME!
   *
   * @param action DOCUMENT ME!
   *
   * @return DOCUMENT ME!
   */
  protected JMenuItem createActionComponent(Action action)
  {
    return null;
  }

  /**
   * DOCUMENT ME!
   *
   * @param item DOCUMENT ME!
   *
   * @return DOCUMENT ME!
   */
  protected PropertyChangeListener createActionChangeListener(JMenuItem item)
  {
    return null;
  }

  /**
   * DOCUMENT ME!
   */
  public void addSeparator()
  {
  }

  /**
   * DOCUMENT ME!
   *
   * @param index DOCUMENT ME!
   */
  public void insertSeparator(int index)
  {
  }

  /**
   * DOCUMENT ME!
   *
   * @param index DOCUMENT ME!
   *
   * @return DOCUMENT ME!
   */
  public JMenuItem getItem(int index)
  {
    Component c = popupMenu.getComponentAtIndex(index);

    if (c instanceof JMenuItem)
      return (JMenuItem) c;
    else
      return null;
  }

  /**
   * DOCUMENT ME!
   *
   * @return DOCUMENT ME!
   */
  public int getItemCount()
  {
    return 0;
  }

  /**
   * DOCUMENT ME!
   *
   * @return DOCUMENT ME!
   */
  public boolean isTearOff()
  {
    return false;
  }

  /**
   * DOCUMENT ME!
   *
   * @return DOCUMENT ME!
   */
  public int getMenuComponentCount()
  {
    return popupMenu.getComponentCount();
  }

  /**
   * DOCUMENT ME!
   *
   * @param index DOCUMENT ME!
   *
   * @return DOCUMENT ME!
   */
  public Component getMenuComponent(int index)
  {
    return (Component) popupMenu.getComponentAtIndex(index);
  }

  /**
   * DOCUMENT ME!
   *
   * @return DOCUMENT ME!
   */
  public Component[] getMenuComponents()
  {
    return popupMenu.getComponents();
  }

  /**
   * DOCUMENT ME!
   *
   * @return DOCUMENT ME!
   */
  public boolean isTopLevelMenu()
  {
    if (getParent() instanceof JMenuBar)
      return true;
    else
      return false;
  }

  /**
   * DOCUMENT ME!
   *
   * @param component DOCUMENT ME!
   *
   * @return DOCUMENT ME!
   */
  public boolean isMenuComponent(Component component)
  {
    return false;
  }

  /**
   * DOCUMENT ME!
   *
   * @return DOCUMENT ME!
   */
  public JPopupMenu getPopupMenu()
  {
    return popupMenu;
  }

  /**
   * DOCUMENT ME!
   *
   * @param listener DOCUMENT ME!
   */
  public void addMenuListener(MenuListener listener)
  {
    listenerList.add(MenuListener.class, listener);
  }

  /**
   * DOCUMENT ME!
   *
   * @param listener DOCUMENT ME!
   */
  public void removeMenuListener(MenuListener listener)
  {
    listenerList.remove(MenuListener.class, listener);
  }

  /**
   * DOCUMENT ME!
   */
  protected void fireMenuSelected()
  {
    EventListener[] ll = listenerList.getListeners(MenuListener.class);

    for (int i = 0; i < ll.length; i++)
      ((MenuListener) ll[i]).menuSelected(new MenuEvent(this));
  }

  /**
   * DOCUMENT ME!
   */
  protected void fireMenuDeselected()
  {
    EventListener[] ll = listenerList.getListeners(MenuListener.class);

    for (int i = 0; i < ll.length; i++)
      ((MenuListener) ll[i]).menuDeselected(new MenuEvent(this));
  }

  /**
   * DOCUMENT ME!
   */
  protected void fireMenuCanceled()
  {
    EventListener[] ll = listenerList.getListeners(MenuListener.class);

    for (int i = 0; i < ll.length; i++)
      ((MenuListener) ll[i]).menuCanceled(new MenuEvent(this));
  }

  /**
   * DOCUMENT ME!
   *
   * @return DOCUMENT ME!
   */
  private ChangeListener createMenuChangeListener()
  {
    return null;
  }

  /**
   * DOCUMENT ME!
   *
   * @param popup DOCUMENT ME!
   *
   * @return DOCUMENT ME!
   */
  protected JMenu.WinListener createWinListener(JPopupMenu popup)
  {
    return null;
  }

  /**
   * DOCUMENT ME!
   *
   * @param changed DOCUMENT ME!
   */
  public void menuSelectionChanged(boolean changed)
  {
  }

  /**
   * DOCUMENT ME!
   *
   * @return DOCUMENT ME!
   */
  public MenuElement[] getSubElements()
  {
    return new MenuElement[] { popupMenu };
  }

  /**
   * DOCUMENT ME!
   *
   * @return DOCUMENT ME!
   */
  public Component getComponent()
  {
    return this;
  }

  /**
   * DOCUMENT ME!
   *
   * @param keystroke DOCUMENT ME!
   */
  public void setAccelerator(KeyStroke keystroke)
  {
    super.setAccelerator(keystroke);
  }

  /**
   * DOCUMENT ME!
   *
   * @param event DOCUMENT ME!
   */
  protected void processKeyEvent(KeyEvent event)
  {
  }

  /**
   * DOCUMENT ME!
   *
   * @param time DOCUMENT ME!
   */
  public void doClick(int time)
  {
  }

  /**
   * DOCUMENT ME!
   *
   * @return DOCUMENT ME!
   */
  protected String paramString()
  {
    return "JMenu";
  }

  /**
   * DOCUMENT ME!
   *
   * @return DOCUMENT ME!
   */
  public AccessibleContext getAccessibleContext()
  {
    if (accessibleContext == null)
      accessibleContext = new AccessibleJMenu(this);

    return accessibleContext;
  }

  /**
   * DOCUMENT ME!
   */
  protected class AccessibleJMenu extends AccessibleJMenuItem
    implements AccessibleSelection
  {
    /**
     * Creates a new AccessibleJMenu object.
     *
     * @param component DOCUMENT ME!
     */
    protected AccessibleJMenu(JMenu component)
    {
      super(component);
    }

    /**
     * DOCUMENT ME!
     *
     * @return DOCUMENT ME!
     */
    public int getAccessibleChildrenCount()
    {
      return 0;
    }

    /**
     * DOCUMENT ME!
     *
     * @param value0 DOCUMENT ME!
     *
     * @return DOCUMENT ME!
     */
    public Accessible getAccessibleChild(int value0)
    {
      return null;
    }

    /**
     * DOCUMENT ME!
     *
     * @return DOCUMENT ME!
     */
    public AccessibleSelection getAccessibleSelection()
    {
      return null;
    }

    /**
     * DOCUMENT ME!
     *
     * @param value0 DOCUMENT ME!
     *
     * @return DOCUMENT ME!
     */
    public Accessible getAccessibleSelection(int value0)
    {
      return null;
    }

    /**
     * DOCUMENT ME!
     *
     * @param value0 DOCUMENT ME!
     *
     * @return DOCUMENT ME!
     */
    public boolean isAccessibleChildSelected(int value0)
    {
      return false;
    }

    /**
     * DOCUMENT ME!
     *
     * @return DOCUMENT ME!
     */
    public AccessibleRole getAccessibleRole()
    {
      return AccessibleRole.MENU;
    }

    /**
     * DOCUMENT ME!
     *
     * @return DOCUMENT ME!
     */
    public int getAccessibleSelectionCount()
    {
      return 0;
    }

    /**
     * DOCUMENT ME!
     *
     * @param value0 DOCUMENT ME!
     */
    public void addAccessibleSelection(int value0)
    {
    }

    /**
     * DOCUMENT ME!
     *
     * @param value0 DOCUMENT ME!
     */
    public void removeAccessibleSelection(int value0)
    {
    }

    /**
     * DOCUMENT ME!
     */
    public void clearAccessibleSelection()
    {
    }

    /**
     * DOCUMENT ME!
     */
    public void selectAllAccessibleSelection()
    {
    }
  }

  /**
   * DOCUMENT ME!
   */
  protected class WinListener extends WindowAdapter implements Serializable
  {
    JPopupMenu popupMenu;

    /**
     * Creates a new WinListener object.
     *
     * @param value0 DOCUMENT ME!
     * @param value1 DOCUMENT ME!
     */
    public WinListener(JMenu value0, JPopupMenu value1)
    {
    }

    /**
     * DOCUMENT ME!
     *
     * @param value0 DOCUMENT ME!
     */
    public void windowClosing(WindowEvent value0)
    {
    }
  }
}
