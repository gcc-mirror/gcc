/* JMenuItem.java --
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

import java.awt.Component;
import java.awt.event.KeyEvent;
import java.awt.event.MouseEvent;
import java.beans.PropertyChangeListener;
import java.io.IOException;
import java.io.ObjectInputStream;
import java.io.ObjectOutputStream;

import javax.accessibility.Accessible;
import javax.accessibility.AccessibleContext;
import javax.accessibility.AccessibleRole;
import javax.swing.event.ChangeEvent;
import javax.swing.event.ChangeListener;
import javax.swing.event.MenuDragMouseEvent;
import javax.swing.event.MenuDragMouseListener;
import javax.swing.event.MenuKeyEvent;
import javax.swing.event.MenuKeyListener;
import javax.swing.plaf.MenuItemUI;

public class JMenuItem extends AbstractButton implements Accessible,
                                                         MenuElement
{
  //-------------------------------------------------------------
  // Variables --------------------------------------------------
  //-------------------------------------------------------------
  private static final String uiClassID = "MenuItemUI";
  private KeyStroke accelerator;

  //-------------------------------------------------------------
  // Initialization ---------------------------------------------
  //-------------------------------------------------------------
  public JMenuItem()
  {
    this(null, null);
  } // JMenuItem()

  public JMenuItem(Icon icon)
  {
    this(null, icon);
  } // JMenuItem()

  public JMenuItem(String text)
  {
    this(text, null);
  } // JMenuItem()

  public JMenuItem(Action action)
  {
    // TODO		
  } // JMenuItem()

  public JMenuItem(String text, Icon icon)
  {
    super(text, icon);
  } // JMenuItem()

  public JMenuItem(String text, int mnemonic)
  {
    super(text, null);
    setMnemonic(mnemonic);
  } // JMenuItem()

  //-------------------------------------------------------------
  // Methods ----------------------------------------------------
  //-------------------------------------------------------------
  private void readObject(ObjectInputStream stream)
                   throws IOException, ClassNotFoundException
  {
    // TODO
  } // readObject()

  private void writeObject(ObjectOutputStream stream) throws IOException
  {
    // TODO
  } // writeObject()

  protected void init(String text, Icon icon)
  {
    // TODO
  } // init()

  public void setUI(MenuItemUI ui)
  {
    super.setUI(ui);
  } // setUI()

  public void updateUI()
  {
    MenuItemUI mi = ((MenuItemUI) UIManager.getUI(this));
    setUI(mi);
    invalidate();
  } // updateUI()

  public String getUIClassID()
  {
    return uiClassID;
  } // getUIClassID()

  public boolean isArmed()
  {
    return getModel().isArmed();
  } // isArmed()

  public void setArmed(boolean armed)
  {
    getModel().setArmed(armed);
  } // setArmed()

  public void setEnabled(boolean enabled)
  {
    setEnabled(enabled);
  } // setEnabled()

  public KeyStroke getAccelerator()
  {
    return accelerator;
  } // getAccelerator()

  public void setAccelerator(KeyStroke keystroke)
  {
    this.accelerator = keystroke;
  } // setAccelerator()

  protected void configurePropertiesFromAction(Action action)
  {
    super.configurePropertiesFromAction(action);

    if (action == null)
      setAccelerator(null);
    else
      setAccelerator((KeyStroke) (action.getValue(Action.ACCELERATOR_KEY)));
  
  } // configurePropertiesFromAction()

  protected PropertyChangeListener createActionPropertyChangeListener(Action action)
  {
    return null;
  } // createActionPropertyChangeListener()

  public void processMouseEvent(MouseEvent event, MenuElement[] path,
                                MenuSelectionManager manager)
  {
    // TODO
  } // processMouseEvent()

  public void processKeyEvent(KeyEvent event, MenuElement[] path,
                              MenuSelectionManager manager)
  {
    // TODO
  } // processKeyEvent()

  public void processMenuDragMouseEvent(MenuDragMouseEvent event)
  {
  } // processMenuDragMouseEvent()

  public void processMenuKeyEvent(MenuKeyEvent event)
  {
    // TODO
  } // processMenuKeyEvent()

  protected void fireMenuDragMouseEntered(MenuDragMouseEvent event)
  {
    // TODO
  } // fireMenuDragMouseEntered()

  protected void fireMenuDragMouseExited(MenuDragMouseEvent event)
  {
    // TODO
  } // fireMenuDragMouseExited()

  protected void fireMenuDragMouseDragged(MenuDragMouseEvent event)
  {
    // TODO
  } // fireMenuDragMouseDragged()

  protected void fireMenuDragMouseReleased(MenuDragMouseEvent event)
  {
    // TODO
  } // fireMenuDragMouseReleased()

  protected void fireMenuKeyPressed(MenuKeyEvent event)
  {
    // TODO
  } // fireMenuKeyPressed()

  protected void fireMenuKeyReleased(MenuKeyEvent event)
  {
    // TODO
  } // fireMenuKeyReleased()

  protected void fireMenuKeyTyped(MenuKeyEvent event)
  {
    // TODO
  } // fireMenuKeyTyped()

  public void menuSelectionChanged(boolean changed)
  {
    // TODO
  } // menuSelectionChanged()

  public MenuElement[] getSubElements()
  {
    return null; // TODO
  } // getSubElements()

  public Component getComponent()
  {
    return null; // TODO
  } // getComponent()

  public void addMenuDragMouseListener(MenuDragMouseListener listener)
  {
    // TODO
  } // addMenuDragMouseListener()

  public void removeMenuDragMouseListener(MenuDragMouseListener listener)
  {
  } // removeMenuDragMouseListener()

  public void addMenuKeyListener(MenuKeyListener listener)
  {
  } // addMenuKeyListener()

  public void removeMenuKeyListener(MenuKeyListener listener)
  {
  } // removeMenuKeyListener()

  protected String paramString()
  {
    return "JMenuItem";
  } // paramString()

  public AccessibleContext getAccessibleContext()
  {
    if (accessibleContext == null)
      {
        accessibleContext = new AccessibleJMenuItem(this);
      }

    return accessibleContext;
  } // getAccessibleContext()

  //-------------------------------------------------------------
  // Classes ----------------------------------------------------
  //-------------------------------------------------------------
  protected class AccessibleJMenuItem extends AccessibleAbstractButton
    implements ChangeListener
  {
    //-------------------------------------------------------------
    // Variables --------------------------------------------------
    //-------------------------------------------------------------
    //-------------------------------------------------------------
    // Initialization ---------------------------------------------
    //-------------------------------------------------------------
    AccessibleJMenuItem(JMenuItem component)
    {
      super(component);

      // TODO
    } // AccessibleJMenuItem()

    //-------------------------------------------------------------
    // Methods ----------------------------------------------------
    //-------------------------------------------------------------
    public void stateChanged(ChangeEvent event)
    {
      // TODO
    } // stateChanged()

    public AccessibleRole getAccessibleRole()
    {
      return AccessibleRole.MENU_ITEM;
    } // getAccessibleRole()
  } // AccessibleJMenuItem
} // JMenuItem
