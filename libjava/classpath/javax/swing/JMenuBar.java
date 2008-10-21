/* JMenuBar.java --
   Copyright (C) 2002, 2004, 2005, 2006  Free Software Foundation, Inc.

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

import gnu.java.lang.CPStringBuilder;

import java.awt.Component;
import java.awt.Graphics;
import java.awt.Insets;
import java.awt.event.KeyEvent;
import java.awt.event.MouseEvent;

import javax.accessibility.Accessible;
import javax.accessibility.AccessibleContext;
import javax.accessibility.AccessibleRole;
import javax.accessibility.AccessibleSelection;
import javax.accessibility.AccessibleStateSet;
import javax.swing.plaf.MenuBarUI;

import javax.swing.border.Border;

/**
 * JMenuBar is a container for menu's. For a menu bar to be seen on the
 * screen, at least one menu should be added to it. Just like adding
 * components to container, one can use add() to add menu's to the menu bar.
 * Menu's will be displayed in the menu  bar in the order they were added.
 * The JMenuBar uses selectionModel to keep track of selected menu index.
 * JMenuBar's selectionModel will fire ChangeEvents to its registered 
 * listeners when the selected index changes.
 */
public class JMenuBar extends JComponent implements Accessible, MenuElement
{
  /**
   * Provides accessibility support for <code>JMenuBar</code>.
   * 
   * @author Roman Kennke (kennke@aicas.com)
   */
  protected class AccessibleJMenuBar extends AccessibleJComponent
    implements AccessibleSelection
  {

    /**
     * Returns the number of selected items in the menu bar. Possible values
     * are <code>0</code> if nothing is selected, or <code>1</code> if one
     * item is selected.
     *
     * @return the number of selected items in the menu bar
     */
    public int getAccessibleSelectionCount()
    {
      int count = 0;
      if (getSelectionModel().getSelectedIndex() != -1)
        count = 1;
      return count;
    }

    /**
     * Returns the selected with index <code>i</code> menu, or
     * <code>null</code> if the specified menu is not selected.
     *
     * @param i the index of the menu to return
     *
     * @return the selected with index <code>i</code> menu, or
     *         <code>null</code> if the specified menu is not selected
     */
    public Accessible getAccessibleSelection(int i)
    {
      if (getSelectionModel().getSelectedIndex() != i)
        return null;
      return getMenu(i);
    }

    /**
     * Returns <code>true</code> if the specified menu is selected,
     * <code>false</code> otherwise.
     *
     * @param i the index of the menu to check
     *
     *@return <code>true</code> if the specified menu is selected,
     *        <code>false</code> otherwise
     */
    public boolean isAccessibleChildSelected(int i)
    {
      return getSelectionModel().getSelectedIndex() == i;
    }

    /**
     * Selects the menu with index <code>i</code>. If another menu is already
     * selected, this will be deselected.
     *
     * @param i the menu to be selected
     */
    public void addAccessibleSelection(int i)
    {
      getSelectionModel().setSelectedIndex(i);
    }

    /**
     * Deselects the menu with index <code>i</code>.
     *
     * @param i the menu index to be deselected
     */
    public void removeAccessibleSelection(int i)
    {
      if (getSelectionModel().getSelectedIndex() == i)
        getSelectionModel().clearSelection();
    }

    /**
     * Deselects all possibly selected menus.
     */
    public void clearAccessibleSelection()
    {
      getSelectionModel().clearSelection();
    }

    /**
     * In menu bars it is not possible to select all items, so this method
     * does nothing.
     */
    public void selectAllAccessibleSelection()
    {
      // In menu bars it is not possible to select all items, so this method
      // does nothing.
    }

    /**
     * Returns the accessible role of <code>JMenuBar</code>, which is
     * {@link AccessibleRole#MENU_BAR}.
     *
     * @return the accessible role of <code>JMenuBar</code>, which is
     *         {@link AccessibleRole#MENU_BAR}
     */
    public AccessibleRole getAccessibleRole()
    {
      return AccessibleRole.MENU_BAR;
    }

    /**
     * Returns the <code>AccessibleSelection</code> for this object. This
     * method returns <code>this</code>, since the
     * <code>AccessibleJMenuBar</code> manages its selection itself.
     *
     * @return the <code>AccessibleSelection</code> for this object
     */
    public AccessibleSelection getAccessibleSelection()
    {
      return this;
    }

    /**
     * Returns the state of this <code>AccessibleJMenuBar</code>.
     *
     * @return the state of this <code>AccessibleJMenuBar</code>.
     */
    public AccessibleStateSet getAccessibleStateSet()
    {
      AccessibleStateSet stateSet = super.getAccessibleStateSet();
      // TODO: Figure out what state must be added to the super state set.
      return stateSet;
    }
  }

  private static final long serialVersionUID = -8191026883931977036L;

  /** JMenuBar's model. It keeps track of selected menu's index */
  private transient SingleSelectionModel selectionModel;

  /* borderPainted property indicating if the menuBar's border will be painted*/
  private boolean borderPainted;

  /* margin between menu bar's border and its menues*/
  private Insets margin;

  /**
   * Creates a new JMenuBar object.
   */
  public JMenuBar()
  {
    selectionModel = new DefaultSingleSelectionModel();
    borderPainted = true;
    updateUI();
  }

  /**
   * Adds menu to the menu bar
   *
   * @param c menu to add
   *
   * @return reference to the added menu
   */
  public JMenu add(JMenu c)
  {
    c.setAlignmentX(Component.LEFT_ALIGNMENT);
    super.add(c);
    return c;
  }

  /**
   * This method overrides addNotify() in the Container to register
   * this menu bar with the current keyboard manager.
   */
  public void addNotify()
  {
    super.addNotify();
    KeyboardManager.getManager().registerJMenuBar(this);
  }

  public AccessibleContext getAccessibleContext()
  {
    if (accessibleContext == null)
      accessibleContext = new AccessibleJMenuBar();
    return accessibleContext;
  }

  /**
   * Returns reference to this menu bar
   *
   * @return reference to this menu bar
   */
  public Component getComponent()
  {
    return this;
  }

  /**
   * Returns component at the specified index.
   *
   * @param i index of the component to get
   *
   * @return component at the specified index. Null is returned if
   * component at the specified index doesn't exist.
   * @deprecated Replaced by getComponent(int)
   */
  public Component getComponentAtIndex(int i)
  {
    return getComponent(i);
  }

  /**
   * Returns index of the specified component
   *
   * @param c Component to search for
   *
   * @return index of the specified component. -1 is returned if
   * specified component doesnt' exist in the menu bar.
   */
  public int getComponentIndex(Component c)
  {
    Component[] comps = getComponents();

    int index = -1;

    for (int i = 0; i < comps.length; i++)
      {
	if (comps[i].equals(c))
	  {
	    index = i;
	    break;
	  }
      }

    return index;
  }

  /**
   * This method is not implemented and will throw an {@link Error} if called.
   *
   * @return This method never returns anything, it throws an exception.
   */
  public JMenu getHelpMenu()
  {
    // the following error matches the behaviour of the reference 
    // implementation...
    throw new Error("getHelpMenu() is not implemented");
  }

  /**
   * Returns the margin between the menu bar's border and its menus.  If the
   * margin is <code>null</code>, this method returns 
   * <code>new Insets(0, 0, 0, 0)</code>.
   *
   * @return The margin (never <code>null</code>).
   * 
   * @see #setMargin(Insets)
   */
  public Insets getMargin()
  {
    if (margin == null)
      return new Insets(0, 0, 0, 0);
    else
      return margin;
  }

  /**
   * Return menu at the specified index. If component at the
   * specified index is not a menu, then null is returned.
   *
   * @param index index to look for the menu
   *
   * @return menu at specified index, or null if menu doesn't exist
   * at the specified index.
   */
  public JMenu getMenu(int index)
  {
    if (getComponentAtIndex(index) instanceof JMenu)
      return (JMenu) getComponentAtIndex(index);
    else
      return null;
  }

  /**
   * Returns number of menu's in this menu bar
   *
   * @return number of menu's in this menu bar
   */
  public int getMenuCount()
  {
    return getComponentCount();
  }

  /**
   * Returns selection model for this menu bar. SelectionModel
   * keeps track of the selected menu in the menu bar. Whenever
   * selected property of selectionModel changes, the ChangeEvent
   * will be fired its ChangeListeners.
   *
   * @return selection model for this menu bar.
   */
  public SingleSelectionModel getSelectionModel()
  {
    return selectionModel;
  }

  /**
   * Method of MenuElement interface. It returns subcomponents
   * of the menu bar, which are all the menues that it contains.
   *
   * @return MenuElement[] array containing menues in this menu bar
   */
  public MenuElement[] getSubElements()
  {
    MenuElement[] subElements = new MenuElement[getComponentCount()];

    int j = 0;
    boolean doResize = false;
    MenuElement menu;
    for (int i = 0; i < getComponentCount(); i++)
      {
        menu = getMenu(i);
        if (menu != null)
          {
            subElements[j++] = (MenuElement) menu;
          }
        else
          doResize = true;
      }

    if (! doResize)
      return subElements;
    else
      {
        MenuElement[] subElements2 = new MenuElement[j];
        for (int i = 0; i < j; i++)
          subElements2[i] = subElements[i];

        return subElements2;
      }
  }

  /**
    * Set the "UI" property of the menu bar, which is a look and feel class
    * responsible for handling the menuBar's input events and painting it.
    *
    * @return The current "UI" property
    */
  public MenuBarUI getUI()
  {
    return (MenuBarUI) ui;
  }

  /**
   * This method returns a name to identify which look and feel class will be
   * the UI delegate for the menu bar.
   *
   * @return The Look and Feel classID. "MenuBarUI"
   */
  public String getUIClassID()
  {
    return "MenuBarUI";
  }

  /**
   * Returns true if menu bar paints its border and false otherwise
   *
   * @return true if menu bar paints its border and false otherwise
   */
  public boolean isBorderPainted()
  {
    return borderPainted;
  }

  /**
   * Returns true if some menu in menu bar is selected.
   *
   * @return true if some menu in menu bar is selected and false otherwise
   */
  public boolean isSelected()
  {
    return selectionModel.isSelected();
  }

  /**
   * This method does nothing by default. This method is need for the
   * MenuElement interface to be implemented.
   *
   * @param isIncluded true if menuBar is included in the selection
   * and false otherwise
   */
  public void menuSelectionChanged(boolean isIncluded)
  {
    // Do nothing - needed for implementation of MenuElement interface
  }

  /**
   * Paints border of the menu bar, if its borderPainted property is set to
   * true.
   *
   * @param g The graphics context with which to paint the border
   */
  protected void paintBorder(Graphics g)
  {
    if (borderPainted)
      {
        Border border = getBorder();
        if (border != null)
          getBorder().paintBorder(this, g, 0, 0, getSize(null).width,
                                  getSize(null).height);
      }
  }

  /**
   * A string that describes this JMenuBar. Normally only used
   * for debugging.
   *
   * @return A string describing this JMenuBar
   */
  protected String paramString()
  {
    CPStringBuilder sb = new CPStringBuilder();
    sb.append(super.paramString());
    sb.append(",margin=");
    if (getMargin() != null)
      sb.append(getMargin());
    sb.append(",paintBorder=").append(isBorderPainted());
    return sb.toString();
  }

  /**
   * Process key events forwarded from MenuSelectionManager. This method
   * doesn't do anything. It is here to conform to the MenuElement interface.
   *
   * @param e event forwarded from MenuSelectionManager
   * @param path path to the menu element from which event was generated
   * @param manager MenuSelectionManager for the current menu hierarchy
   *
   */
  public void processKeyEvent(KeyEvent e, MenuElement[] path,
                              MenuSelectionManager manager)
  {
    // Do nothing - needed for implementation of MenuElement interface
  }

  /**
   * This method overrides JComponent.processKeyBinding to allow the 
   * JMenuBar to check all the child components (recursiveley) to see 
   * if they'll consume the event.
   * 
   * @param ks the KeyStroke for the event
   * @param e the KeyEvent for the event
   * @param condition the focus condition for the binding
   * @param pressed true if the key is pressed 
   */
  protected boolean processKeyBinding(KeyStroke ks, KeyEvent e, int condition,
                                      boolean pressed)
  {
    // See if the regular JComponent behavior consumes the event
    if (super.processKeyBinding(ks, e, condition, pressed))
      return true;
    
    // If not, have to recursively check all the child menu elements to see 
    // if they want it    
    MenuElement[] children = getSubElements();
    for (int i = 0; i < children.length; i++)
      if (processKeyBindingHelper(children[i], ks, e, condition, pressed))
        return true;
    return false;
  }
  
  /**
   * This is a helper method to recursively check the children of this
   * JMenuBar to see if they will consume a key event via key bindings.  
   * This is used for menu accelerators.
   * @param menuElement the menuElement to check (and check all its children)
   * @param ks the KeyStroke for the event
   * @param e the KeyEvent that may be consumed
   * @param condition the focus condition for the binding
   * @param pressed true if the key was pressed
   * @return true <code>menuElement</code> or one of its children consume
   * the event (processKeyBinding returns true for menuElement or one of
   * its children).
   */
  static boolean processKeyBindingHelper(MenuElement menuElement, KeyStroke ks,
                                         KeyEvent e, int condition,
                                         boolean pressed)
  {
    if (menuElement == null)
      return false;

    // First check the menuElement itself, if it's a JComponent
    if (menuElement instanceof JComponent
        && ((JComponent) menuElement).processKeyBinding(ks, e, condition,
                                                        pressed))
      return true;
    
    // If that didn't consume it, check all the children recursively
    MenuElement[] children = menuElement.getSubElements();
    for (int i = 0; i < children.length; i++)
      if (processKeyBindingHelper(children[i], ks, e, condition, pressed))
        return true;
    return false;
  }
  
  /**
   * Process mouse events forwarded from MenuSelectionManager. This method
   * doesn't do anything. It is here to conform to the MenuElement interface.
   *
   * @param event event forwarded from MenuSelectionManager
   * @param path path to the menu element from which event was generated
   * @param manager MenuSelectionManager for the current menu hierarchy
   *
   */
  public void processMouseEvent(MouseEvent event, MenuElement[] path,
                                MenuSelectionManager manager)
  {
    // Do nothing - needed for implementation of MenuElement interface
  }

  /**
   * This method overrides removeNotify() in the Container to
   * unregister this menu bar from the current keyboard manager.
   */
  public void removeNotify()
  {
    KeyboardManager.getManager().unregisterJMenuBar(this);
    super.removeNotify();
  }

  /**
   * Sets painting status of the border. If 'b' is true then menu bar's
   * border will be painted, and it will not be painted otherwise.
   *
   * @param b indicates if menu bar's border should be painted.
   */
  public void setBorderPainted(boolean b)
  {
    if (b != borderPainted)
      {
	boolean old = borderPainted;
	borderPainted = b;
	firePropertyChange("borderPainted", old, b);
	revalidate();
	repaint();
      }
  }

  /**
   * Sets help menu for this menu bar
   *
   * @param menu help menu
   *
   * @specnote The specification states that this method is not yet implemented
   *           and should throw an exception.
   */
  public void setHelpMenu(JMenu menu)
  {
    // We throw an Error here, just as Sun's JDK does.
    throw new Error("setHelpMenu() not yet implemented.");
  }

  /**
   * Sets the margin between the menu bar's border and its menus (this is a
   * bound property with the name 'margin').
   *
   * @param m  the margin (<code>null</code> permitted).
   * 
   * @see #getMargin()
   */
  public void setMargin(Insets m)
  {
    if (m != margin)
      {
        Insets oldMargin = margin;
        margin = m;
        firePropertyChange("margin", oldMargin, margin);
      }
  }

  /**
   * Changes menu bar's selection to the specified menu.
   * This method updates selected index of menu bar's selection model,
   * which results in a model firing change event.
   *
   * @param sel menu to select
   */
  public void setSelected(Component sel)
  {
    int index = getComponentIndex(sel);
    selectionModel.setSelectedIndex(index);
  }

  /**
   * Sets menuBar's selection model to the one specified
   *
   * @param model SingleSelectionModel that needs to be set for this menu bar
   */
  public void setSelectionModel(SingleSelectionModel model)
  {
    if (selectionModel != model)
      {
	SingleSelectionModel oldModel = selectionModel;
	selectionModel = model;
	firePropertyChange("model", oldModel, selectionModel);
      }
  }

  /**
   * Set the "UI" property of the menu bar, which is a look and feel class
   * responsible for handling menuBar's input events and painting it.
   *
   * @param ui The new "UI" property
   */
  public void setUI(MenuBarUI ui)
  {
    super.setUI(ui);
  }

  /**
   * Set the "UI" property to a class constructed, via the {@link
   * UIManager}, from the current look and feel.
   */
  public void updateUI()
  {
    setUI((MenuBarUI) UIManager.getUI(this));
  }
}
