/* JMenuBar.java -- 
   Copyright (C) 2002, 2004  Free Software Foundation, Inc.

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

import java.awt.Color;
import java.awt.Component;
import java.awt.Dimension;
import java.awt.Font;
import java.awt.FontMetrics;
import java.awt.Graphics;
import java.awt.Graphics2D;
import java.awt.Insets;
import java.awt.Point;
import java.awt.event.KeyEvent;
import java.awt.event.MouseEvent;
import javax.accessibility.Accessible;
import javax.accessibility.AccessibleContext;
import javax.accessibility.AccessibleRole;
import javax.accessibility.AccessibleSelection;
import javax.accessibility.AccessibleStateSet;
import javax.accessibility.AccessibleValue;
import javax.swing.UIManager;
import javax.swing.event.ChangeEvent;
import javax.swing.event.ChangeListener;
import javax.swing.event.MenuDragMouseEvent;
import javax.swing.event.MenuDragMouseListener;
import javax.swing.event.MenuKeyEvent;
import javax.swing.event.MenuKeyListener;
import javax.swing.plaf.MenuBarUI;
import javax.swing.plaf.MenuItemUI;


/**
 * DOCUMENT ME!
 */
public class JMenuBar extends JComponent implements Accessible, MenuElement
{
  private transient SingleSelectionModel selectionModel;
  private boolean paintBorder;
  private Insets margin;

  /**
   * Creates a new JMenuBar object.
   */
  public JMenuBar()
  {
    selectionModel = new DefaultSingleSelectionModel();
    paintBorder = true;
    updateUI();
  }

  /**
   * DOCUMENT ME!
   *
   * @param c DOCUMENT ME!
   *
   * @return DOCUMENT ME!
   */
  public JMenu add(JMenu c)
  {
    c.setAlignmentX(Component.LEFT_ALIGNMENT);
    super.add(c);
    return c;
  }

  /**
   * DOCUMENT ME!
   */
  public void addNotify()
  {
    // FIXME: Should register this menu bar with the keyboard manager     
    super.addNotify();
  }

  /**
   * DOCUMENT ME!
   *
   * @return DOCUMENT ME!
   */
  public AccessibleContext getAccessibleContext()
  {
    return null;
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
   * @param i DOCUMENT ME!
   *
   * @return DOCUMENT ME!
   */
  public Component getComponentAtIndex(int i)
  {
    return getComponentAt(i);
  }

  /**
   * DOCUMENT ME!
   *
   * @param c DOCUMENT ME!
   *
   * @return DOCUMENT ME!
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
   * DOCUMENT ME!
   *
   * @return DOCUMENT ME!
   */
  public Insets getMargin()
  {
    if (margin == null)
      return new Insets(0, 0, 0, 0);
    else
      return margin;
  }

  /**
   * DOCUMENT ME!
   *
   * @param index DOCUMENT ME!
   *
   * @return DOCUMENT ME!
   */
  public JMenu getMenu(int index)
  {
    if (getComponentAtIndex(index) instanceof JMenu)
      return (JMenu) getComponentAtIndex(index);
    else
      return null;
  }

  /**
   * DOCUMENT ME!
   *
   * @return DOCUMENT ME!
   */
  public int getMenuCount()
  {
    return getComponentCount();
  }

  /**
   * DOCUMENT ME!
   *
   * @return DOCUMENT ME!
   */
  public SingleSelectionModel getSelectionModel()
  {
    return selectionModel;
  }

  /**
   * DOCUMENT ME!
   *
   * @return DOCUMENT ME!
   */
  public MenuElement[] getSubElements()
  {
    MenuElement[] subElements = new MenuElement[getComponentCount()];

    for (int i = 0; i < getComponentCount(); i++)
      subElements[i] = (MenuElement) getMenu(i);

    return subElements;
  }

  /**
   * DOCUMENT ME!
   *
   * @return DOCUMENT ME!
   */
  public MenuBarUI getUI()
  {
    return (MenuBarUI) ui;
  }

  /**
   * DOCUMENT ME!
   *
   * @return DOCUMENT ME!
   */
  public String getUIClassID()
  {
    return "MenuBarUI";
  }

  /**
   * DOCUMENT ME!
   *
   * @return DOCUMENT ME!
   */
  public boolean isBorderPainted()
  {
    return paintBorder;
  }

  /**
   * DOCUMENT ME!
   *
   * @return DOCUMENT ME!
   */
  public boolean isManagingFocus()
  {
    return true;
  }

  /**
   * DOCUMENT ME!
   *
   * @return DOCUMENT ME!
   */
  public boolean isSelected()
  {
    return false;
  }

  /**
   * DOCUMENT ME!
   *
   * @param isIncluded DOCUMENT ME!
   */
  public void menuSelectionChanged(boolean isIncluded)
  {
    // Do nothing - needed for implementation of MenuElement interface
  }

  /**
   * DOCUMENT ME!
   *
   * @param g DOCUMENT ME!
   */
  protected void paintBorder(Graphics g)
  {
    if (paintBorder)
      getBorder().paintBorder(this, g, 0, 0, getSize(null).width,
                              getSize(null).height);
  }

  /**
   * DOCUMENT ME!
   *
   * @return DOCUMENT ME!
   */
  protected String paramString()
  {
    return "JMenuBar";
  }

  /**
   * DOCUMENT ME!
   *
   * @param e DOCUMENT ME!
   * @param path DOCUMENT ME!
   * @param manager DOCUMENT ME!
   */
  public void processKeyEvent(KeyEvent e, MenuElement[] path,
                              MenuSelectionManager manager)
  {
    // Do nothing - needed for implementation of MenuElement interface
  }

  /**
   * DOCUMENT ME!
   *
   * @param event DOCUMENT ME!
   * @param path DOCUMENT ME!
   * @param manager DOCUMENT ME!
   */
  public void processMouseEvent(MouseEvent event, MenuElement[] path,
                                MenuSelectionManager manager)
  {
    // Do nothing - needed for implementation of MenuElement interface
  }

  /**
   * DOCUMENT ME!
   */
  public void removeNotify()
  {
    // Must unregister this menu bar with the current keyboard manager.
    super.removeNotify();
  }

  /**
   * DOCUMENT ME!
   *
   * @param b DOCUMENT ME!
   */
  public void setBorderPainted(boolean b)
  {
    paintBorder = b;
  }

  /**
   * DOCUMENT ME!
   *
   * @param menu DOCUMENT ME!
   */
  public void setHelpMenu(JMenu menu)
  {
  }

  /**
   * DOCUMENT ME!
   *
   * @param m DOCUMENT ME!
   */
  public void setMargin(Insets m)
  {
    this.margin = m;
  }

  /**
   * DOCUMENT ME!
   *
   * @param sel DOCUMENT ME!
   */
  public void setSelected(Component sel)
  {
  }

  /**
   * DOCUMENT ME!
   *
   * @param model DOCUMENT ME!
   */
  public void setSelectionModel(SingleSelectionModel model)
  {
  }

  /**
   * DOCUMENT ME!
   *
   * @param ui DOCUMENT ME!
   */
  public void setUI(MenuBarUI ui)
  {
    super.setUI(ui);
  }

  /**
   * DOCUMENT ME!
   */
  public void updateUI()
  {
    MenuBarUI ui = ((MenuBarUI) UIManager.getUI(this));
    setUI(ui);
    invalidate();
  }
}
