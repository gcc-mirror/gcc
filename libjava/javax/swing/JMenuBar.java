/* JMenuBar.java --
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
import java.awt.Graphics;
import java.awt.Insets;
import java.awt.event.KeyEvent;
import java.awt.event.MouseEvent;

import javax.accessibility.Accessible;
import javax.accessibility.AccessibleContext;
import javax.swing.plaf.MenuBarUI;

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
    // FIXME: Should register this menu bar with the keyboard manager     
    super.addNotify();
  }

  public AccessibleContext getAccessibleContext()
  {
    return null;
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
   * DOCUMENT ME!
   *
   * @return DOCUMENT ME!
   */
  public JMenu getHelpMenu()
  {
    return null;
  }

  /**
   * Returns margin betweeen menu bar's border and its menues
   *
   * @return margin between menu bar's border and its menues
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

    for (int i = 0; i < getComponentCount(); i++)
      subElements[i] = (MenuElement) getMenu(i);

    return subElements;
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
   * @return The Look and Feel classID. "MenuItemUI"
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
      getBorder().paintBorder(this, g, 0, 0, getSize(null).width,
                              getSize(null).height);
  }

  /**
   * A string that describes this JMenuBar. Normally only used
   * for debugging.
   *
   * @return A string describing this JMenuBar
   */
  protected String paramString()
  {
    StringBuffer sb = new StringBuffer();
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
   * @param event event forwarded from MenuSelectionManager
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
    // Must unregister this menu bar with the current keyboard manager.
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
   */
  public void setHelpMenu(JMenu menu)
  {
  }

  /**
   * Sets the menu bar's "margin" bound property,  which represents
   * distance between the menubar's border and its menus.
   * icon. When marging property is modified, PropertyChangeEvent will
   * be fired to menuBar's PropertyChangeListener's.
   *
   * @param m distance between the menubar's border and its menus.
   *
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
    invalidate();
  }
}
