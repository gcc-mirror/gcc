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
import java.awt.event.InputEvent;
import java.awt.event.KeyEvent;
import java.awt.event.MouseEvent;
import java.beans.PropertyChangeEvent;
import java.beans.PropertyChangeListener;
import java.io.IOException;
import java.io.ObjectInputStream;
import java.io.ObjectOutputStream;
import java.util.EventListener;
import javax.accessibility.Accessible;
import javax.accessibility.AccessibleContext;
import javax.accessibility.AccessibleRole;
import javax.swing.UIManager;
import javax.swing.event.ChangeEvent;
import javax.swing.event.ChangeListener;
import javax.swing.event.MenuDragMouseEvent;
import javax.swing.event.MenuDragMouseListener;
import javax.swing.event.MenuKeyEvent;
import javax.swing.event.MenuKeyListener;
import javax.swing.plaf.MenuItemUI;


/**
 * DOCUMENT ME!
 */
public class JMenuItem extends AbstractButton implements Accessible,
                                                         MenuElement
{
  private static final String uiClassID = "MenuItemUI";
  private KeyStroke accelerator;

  /**
   * Creates a new JMenuItem object.
   */
  public JMenuItem()
  {
    this(null, null);
  }

  /**
   * Creates a new JMenuItem object.
   *
   * @param icon DOCUMENT ME!
   */
  public JMenuItem(Icon icon)
  {
    this(null, icon);
  }

  /**
   * Creates a new JMenuItem object.
   *
   * @param text DOCUMENT ME!
   */
  public JMenuItem(String text)
  {
    this(text, null);
  }

  /**
   * Creates a new JMenuItem object.
   *
   * @param action DOCUMENT ME!
   */
  public JMenuItem(Action action)
  {
    // TODO		
  }

  /**
   * Creates a new JMenuItem object.
   *
   * @param text DOCUMENT ME!
   * @param icon DOCUMENT ME!
   */
  public JMenuItem(String text, Icon icon)
  {
    super(text, icon);
  }

  /**
   * Creates a new JMenuItem object.
   *
   * @param text DOCUMENT ME!
   * @param mnemonic DOCUMENT ME!
   */
  public JMenuItem(String text, int mnemonic)
  {
    super(text, null);
    setMnemonic(mnemonic);
  }

  //-------------------------------------------------------------
  // Methods ----------------------------------------------------
  //-------------------------------------------------------------
  private void readObject(ObjectInputStream stream)
                   throws IOException, ClassNotFoundException
  {
    // TODO
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
    // TODO
  }

  /**
   * DOCUMENT ME!
   *
   * @param text DOCUMENT ME!
   * @param icon DOCUMENT ME!
   */
  protected void init(String text, Icon icon)
  {
    // TODO
  }

  /**
   * DOCUMENT ME!
   *
   * @param ui DOCUMENT ME!
   */
  public void setUI(MenuItemUI ui)
  {
    super.setUI(ui);
  }

  /**
   * DOCUMENT ME!
   */
  public void updateUI()
  {
    MenuItemUI mi = ((MenuItemUI) UIManager.getUI(this));
    setUI(mi);
    invalidate();
  }

  /**
   * DOCUMENT ME!
   *
   * @return $returnType$ DOCUMENT ME!
   */
  public String getUIClassID()
  {
    return uiClassID;
  }

  /**
   * DOCUMENT ME!
   *
   * @return $returnType$ DOCUMENT ME!
   */
  public boolean isArmed()
  {
    return getModel().isArmed();
  }

  /**
   * DOCUMENT ME!
   *
   * @param armed DOCUMENT ME!
   */
  public void setArmed(boolean armed)
  {
    getModel().setArmed(armed);
  }

  /**
   * DOCUMENT ME!
   *
   * @param enabled DOCUMENT ME!
   */
  public void setEnabled(boolean enabled)
  {
    setEnabled(enabled);
  }

  /**
   * DOCUMENT ME!
   *
   * @return $returnType$ DOCUMENT ME!
   */
  public KeyStroke getAccelerator()
  {
    return accelerator;
  }

  /**
   * DOCUMENT ME!
   *
   * @param keystroke DOCUMENT ME!
   */
  public void setAccelerator(KeyStroke keystroke)
  {
    this.accelerator = keystroke;
  }

  /**
   * DOCUMENT ME!
   *
   * @param action DOCUMENT ME!
   */
  protected void configurePropertiesFromAction(Action action)
  {
    super.configurePropertiesFromAction(action);

    if (action == null)
      setAccelerator(null);
    else
      setAccelerator((KeyStroke) (action.getValue(Action.ACCELERATOR_KEY)));
  }

  /**
   * DOCUMENT ME!
   *
   * @param action DOCUMENT ME!
   *
   * @return $returnType$ DOCUMENT ME!
   */
  protected PropertyChangeListener createActionPropertyChangeListener(Action action)
  {
    return new PropertyChangeListener()
      {
	public void propertyChange(PropertyChangeEvent e)
	{
	  Action act = (Action) (e.getSource());
	  configurePropertiesFromAction(act);
	}
      };
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
    switch (event.getID())
      {
      case MouseEvent.MOUSE_CLICKED:
	doClick();
	break;
      case MouseEvent.MOUSE_ENTERED:
	if (event.getSource() instanceof JMenuItem)
	  {
	    JMenuItem item = (JMenuItem) event.getSource();
	    ButtonModel model = item.getModel();

	    if (item.isRolloverEnabled())
	      model.setRollover(true);

	    if (model.isPressed()
	        && (event.getModifiers() & InputEvent.BUTTON1_MASK) != 0)
	      model.setArmed(true);
	    else
	      model.setArmed(false);
	  }
	break;
      case MouseEvent.MOUSE_EXITED:
	if (event.getSource() instanceof JMenuItem)
	  {
	    JMenuItem item = (JMenuItem) event.getSource();
	    ButtonModel model = item.getModel();
	    if (item.isRolloverEnabled())
	      model.setRollover(false);
	    model.setArmed(false);
	  }
	break;
      case MouseEvent.MOUSE_PRESSED:
	if (event.getSource() instanceof JMenuItem)
	  {
	    if ((event.getModifiers() & InputEvent.BUTTON1_MASK) != 0)
	      {
		model.setArmed(true);
		model.setPressed(true);
	      }
	  }
	break;
      case MouseEvent.MOUSE_RELEASED:
	if (event.getSource() instanceof JMenuItem)
	  {
	    JMenuItem item = (JMenuItem) event.getSource();
	    ButtonModel model = item.getModel();
	    if ((event.getModifiers() & InputEvent.BUTTON1_MASK) != 0)
	      {
		model.setPressed(false);
		model.setArmed(false);
		manager.clearSelectedPath();
	      }
	  }
	break;
      case MouseEvent.MOUSE_MOVED:
	break;
      case MouseEvent.MOUSE_DRAGGED:
	MenuDragMouseEvent e = new MenuDragMouseEvent((Component) event
	                                              .getSource(),
	                                              event.getID(),
	                                              event.getWhen(),
	                                              event.getModifiers(),
	                                              event.getX(),
	                                              event.getY(),
	                                              event.getClickCount(),
	                                              event.isPopupTrigger(),
	                                              path, manager);
	processMenuDragMouseEvent(e);
	break;
      }
  }

  /**
   * DOCUMENT ME!
   *
   * @param event DOCUMENT ME!
   * @param path DOCUMENT ME!
   * @param manager DOCUMENT ME!
   */
  public void processKeyEvent(KeyEvent event, MenuElement[] path,
                              MenuSelectionManager manager)
  {
    // TODO
  }

  /**
   * DOCUMENT ME!
   *
   * @param event DOCUMENT ME!
   */
  public void processMenuDragMouseEvent(MenuDragMouseEvent event)
  {
    switch (event.getID())
      {
      case MouseEvent.MOUSE_ENTERED:
	fireMenuDragMouseEntered(event);
	break;
      case MouseEvent.MOUSE_EXITED:
	fireMenuDragMouseExited(event);
	break;
      case MouseEvent.MOUSE_DRAGGED:
	fireMenuDragMouseDragged(event);
	break;
      case MouseEvent.MOUSE_RELEASED:
	fireMenuDragMouseReleased(event);
	break;
      }
  }

  /**
   * DOCUMENT ME!
   *
   * @param event DOCUMENT ME!
   */
  public void processMenuKeyEvent(MenuKeyEvent event)
  {
    // TODO
  }

  /**
   * DOCUMENT ME!
   *
   * @param event DOCUMENT ME!
   */
  protected void fireMenuDragMouseEntered(MenuDragMouseEvent event)
  {
    EventListener[] ll = listenerList.getListeners(MenuDragMouseListener.class);

    for (int i = 0; i < ll.length; i++)
      ((MenuDragMouseListener) ll[i]).menuDragMouseEntered(event);
  }

  /**
   * DOCUMENT ME!
   *
   * @param event DOCUMENT ME!
   */
  protected void fireMenuDragMouseExited(MenuDragMouseEvent event)
  {
    EventListener[] ll = listenerList.getListeners(MenuDragMouseListener.class);

    for (int i = 0; i < ll.length; i++)
      ((MenuDragMouseListener) ll[i]).menuDragMouseExited(event);
  }

  /**
   * DOCUMENT ME!
   *
   * @param event DOCUMENT ME!
   */
  protected void fireMenuDragMouseDragged(MenuDragMouseEvent event)
  {
    EventListener[] ll = listenerList.getListeners(MenuDragMouseListener.class);

    for (int i = 0; i < ll.length; i++)
      ((MenuDragMouseListener) ll[i]).menuDragMouseDragged(event);
  }

  /**
   * DOCUMENT ME!
   *
   * @param event DOCUMENT ME!
   */
  protected void fireMenuDragMouseReleased(MenuDragMouseEvent event)
  {
    EventListener[] ll = listenerList.getListeners(MenuDragMouseListener.class);

    for (int i = 0; i < ll.length; i++)
      ((MenuDragMouseListener) ll[i]).menuDragMouseReleased(event);
  }

  /**
   * DOCUMENT ME!
   *
   * @param event DOCUMENT ME!
   */
  protected void fireMenuKeyPressed(MenuKeyEvent event)
  {
    // TODO
  }

  /**
   * DOCUMENT ME!
   *
   * @param event DOCUMENT ME!
   */
  protected void fireMenuKeyReleased(MenuKeyEvent event)
  {
    // TODO
  }

  /**
   * DOCUMENT ME!
   *
   * @param event DOCUMENT ME!
   */
  protected void fireMenuKeyTyped(MenuKeyEvent event)
  {
    // TODO
  }

  /**
   * DOCUMENT ME!
   *
   * @param changed DOCUMENT ME!
   */
  public void menuSelectionChanged(boolean changed)
  {
    if (changed)
      model.setArmed(true);
  }

  /**
   * DOCUMENT ME!
   *
   * @return $returnType$ DOCUMENT ME!
   */
  public MenuElement[] getSubElements()
  {
    return new MenuElement[0];
  }

  /**
   * DOCUMENT ME!
   *
   * @return $returnType$ DOCUMENT ME!
   */
  public Component getComponent()
  {
    return this;
  }

  /**
   * DOCUMENT ME!
   *
   * @param listener DOCUMENT ME!
   */
  public void addMenuDragMouseListener(MenuDragMouseListener listener)
  {
    listenerList.add(MenuDragMouseListener.class, listener);
  }

  /**
   * DOCUMENT ME!
   *
   * @param listener DOCUMENT ME!
   */
  public void removeMenuDragMouseListener(MenuDragMouseListener listener)
  {
    listenerList.remove(MenuDragMouseListener.class, listener);
  }

  /**
   * DOCUMENT ME!
   *
   * @param listener DOCUMENT ME!
   */
  public void addMenuKeyListener(MenuKeyListener listener)
  {
    listenerList.add(MenuKeyListener.class, listener);
  }

  /**
   * DOCUMENT ME!
   *
   * @param listener DOCUMENT ME!
   */
  public void removeMenuKeyListener(MenuKeyListener listener)
  {
    listenerList.remove(MenuKeyListener.class, listener);
  }

  /**
   * DOCUMENT ME!
   *
   * @return $returnType$ DOCUMENT ME!
   */
  protected String paramString()
  {
    return "JMenuItem";
  }

  /**
   * DOCUMENT ME!
   *
   * @return $returnType$ DOCUMENT ME!
   */
  public AccessibleContext getAccessibleContext()
  {
    if (accessibleContext == null)
      accessibleContext = new AccessibleJMenuItem(this);

    return accessibleContext;
  }

  /**
   * DOCUMENT ME!
   */
  protected class AccessibleJMenuItem extends AccessibleAbstractButton
    implements ChangeListener
  {
    /**
     * Creates a new AccessibleJMenuItem object.
     *
     * @param component DOCUMENT ME!
     */
    AccessibleJMenuItem(JMenuItem component)
    {
      super(component);

      // TODO
    }

    /**
     * DOCUMENT ME!
     *
     * @param event DOCUMENT ME!
     */
    public void stateChanged(ChangeEvent event)
    {
      // TODO
    }

    /**
     * DOCUMENT ME!
     *
     * @return $returnType$ DOCUMENT ME!
     */
    public AccessibleRole getAccessibleRole()
    {
      return AccessibleRole.MENU_ITEM;
    }
  }
}
