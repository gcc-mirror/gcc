/* MenuSelectionManager.java --
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

import java.awt.Component;
import java.awt.Dimension;
import java.awt.Point;
import java.awt.event.KeyEvent;
import java.awt.event.MouseEvent;
import java.util.ArrayList;
import java.util.Vector;

import javax.swing.event.ChangeEvent;
import javax.swing.event.ChangeListener;
import javax.swing.event.EventListenerList;

/**
 * This class manages current menu selectection. It provides
 * methods to clear and set current selected menu path.
 * It also fires StateChange event to its registered
 * listeners whenever selected path of the current menu hierarchy
 * changes.
 *
 */
public class MenuSelectionManager
{
  /** ChangeEvent fired when selected path changes*/
  protected ChangeEvent changeEvent = new ChangeEvent(this);

  /** List of listeners for this MenuSelectionManager */
  protected EventListenerList listenerList = new EventListenerList();

  /** Default manager for the current menu hierarchy*/
  private static final MenuSelectionManager manager = new MenuSelectionManager();

  /** Path to the currently selected menu */
  private Vector selectedPath = new Vector();

  /**
   * Fires StateChange event to registered listeners
   */
  protected void fireStateChanged()
  {
    ChangeListener[] listeners = getChangeListeners();

    for (int i = 0; i < listeners.length; i++)
      listeners[i].stateChanged(changeEvent);
  }

  /**
   * Adds ChangeListener to this MenuSelectionManager
   *
   * @param listener ChangeListener to add
   */
  public void addChangeListener(ChangeListener listener)
  {
    listenerList.add(ChangeListener.class, listener);
  }

  /**
   * Removes ChangeListener from the list of registered listeners
   * for this MenuSelectionManager.
   *
   * @param listener ChangeListner to remove
   */
  public void removeChangeListener(ChangeListener listener)
  {
    listenerList.remove(ChangeListener.class, listener);
  }

  /**
   * Returns list of registered listeners with MenuSelectionManager
   *
   * @since 1.4
   */
  public ChangeListener[] getChangeListeners()
  {
    return (ChangeListener[]) listenerList.getListeners(ChangeListener.class);
  }

  /**
   * Unselects all the menu elements on the selection path
   */
  public void clearSelectedPath()
  {
    // Send events from the bottom most item in the menu - hierarchy to the
    // top most
    for (int i = selectedPath.size() - 1; i >= 0; i--)
      ((MenuElement) selectedPath.get(i)).menuSelectionChanged(false);

    // clear selected path
    selectedPath.clear();

    // notify all listeners that the selected path was changed    
    fireStateChanged();
  }

  /**
   * This method returns menu element on the selected path that contains
   * given source point. If no menu element on the selected path contains this
   * point, then null is returned.
   *
   * @param source Component relative to which sourcePoint is given
   * @param sourcePoint point for which we want to find menu element that contains it
   *
   * @return Returns menu element that contains given source point and belongs
   * to the currently selected path. Null is return if no such menu element found.
   */
  public Component componentForPoint(Component source, Point sourcePoint)
  {
    // Convert sourcePoint to screen coordinates.
    Point sourcePointOnScreen = sourcePoint;
    
    if (source.isShowing())
      SwingUtilities.convertPointToScreen(sourcePointOnScreen, source);

    Point compPointOnScreen;
    Component resultComp = null;

    // For each menu element on the selected path, express its location 
    // in terms of screen coordinates and check if there is any 
    // menu element on the selected path that contains given source point.
    for (int i = 0; i < selectedPath.size(); i++)
      {
	Component comp = ((Component) selectedPath.get(i));
	Dimension size = comp.getSize();

	// convert location of this menu item to screen coordinates
	compPointOnScreen = comp.getLocationOnScreen();

	if (compPointOnScreen.x <= sourcePointOnScreen.x
	    && sourcePointOnScreen.x < compPointOnScreen.x + size.width
	    && compPointOnScreen.y <= sourcePointOnScreen.y
	    && sourcePointOnScreen.y < compPointOnScreen.y + size.height)
	  {
	    Point p = sourcePointOnScreen;
        
        if (comp.isShowing())
          SwingUtilities.convertPointFromScreen(p, comp);
        
	    resultComp = SwingUtilities.getDeepestComponentAt(comp, p.x, p.y);
	    break;
	  }
      }
    return resultComp;
  }

  /**
   * Returns shared instance of MenuSelection Manager
   *
   * @return default Manager
   */
  public static MenuSelectionManager defaultManager()
  {
    return manager;
  }

  /**
   * Returns path representing current menu selection
   *
   * @return Current selection path
   */
  public MenuElement[] getSelectedPath()
  {
    MenuElement[] path = new MenuElement[selectedPath.size()];

    for (int i = 0; i < path.length; i++)
      path[i] = (MenuElement) selectedPath.get(i);

    return path;
  }

  /**
   * Returns true if specified component is part of current menu
   * heirarchy and false otherwise
   *
   * @param c Component for which to check
   * @return True if specified component is part of current menu
   */
  public boolean isComponentPartOfCurrentMenu(Component c)
  {
    MenuElement[] subElements;
    boolean ret = false;
    for (int i = 0; i < selectedPath.size(); i++)
      {
        // Check first element.
        MenuElement first = (MenuElement) selectedPath.get(i);
        if (SwingUtilities.isDescendingFrom(c, first.getComponent()))
          {
            ret = true;
            break;
          }
        else
          {
            // Check sub elements.
            subElements = first.getSubElements();
            for (int j = 0; j < subElements.length; j++)
              {
                MenuElement me = subElements[j]; 
                if (me != null
                    && (SwingUtilities.isDescendingFrom(c, me.getComponent())))
                  {
                    ret = true;
                    break;
                  }
              }
          }
      }

      return ret;
  }

  /**
   * Processes key events on behalf of the MenuElements. MenuElement
   * instances should always forward their key events to this method and
   * get their {@link MenuElement#processKeyEvent(KeyEvent, MenuElement[],
   * MenuSelectionManager)} eventually called back.
   *
   * @param e the key event
   */
  public void processKeyEvent(KeyEvent e)
  {
    MenuElement[] selection = (MenuElement[])
                    selectedPath.toArray(new MenuElement[selectedPath.size()]);
    if (selection.length == 0)
      return;

    MenuElement[] path;
    for (int index = selection.length - 1; index >= 0; index--)
      {
        MenuElement el = selection[index];
        // This method's main purpose is to forward key events to the
        // relevant menu items, so that they can act in response to their
        // mnemonics beeing typed. So we also need to forward the key event
        // to all the subelements of the currently selected menu elements
        // in the path.
        MenuElement[] subEls = el.getSubElements();
        path = null;
        for (int subIndex = 0; subIndex < subEls.length; subIndex++)
          {
            MenuElement sub = subEls[subIndex];
            // Skip elements that are not showing or not enabled.
            if (sub == null || ! sub.getComponent().isShowing()
                || ! sub.getComponent().isEnabled())
              {
                continue;
              }

            if (path == null)
              {
                path = new MenuElement[index + 2];
                System.arraycopy(selection, 0, path, 0, index + 1);
              }
            path[index + 1] = sub;
            sub.processKeyEvent(e, path, this);
            if (e.isConsumed())
              break;
          }
        if (e.isConsumed())
          break;
      }

    // Dispatch to first element in selection if it hasn't been consumed.
    if (! e.isConsumed())
      {
        path = new MenuElement[1];
        path[0] = selection[0];
        path[0].processKeyEvent(e, path, this);
      }
  }

  /**
   * Forwards given mouse event to all of the source subcomponents.
   *
   * @param event Mouse event
   */
  public void processMouseEvent(MouseEvent event)
  {
    Component source = ((Component) event.getSource());

    // In the case of drag event, event.getSource() returns component
    // where drag event originated. However menu element processing this 
    // event should be the one over which mouse is currently located, 
    // which is not necessary the source of the drag event.     
    Component mouseOverMenuComp;

    // find over which menu element the mouse is currently located
    if (event.getID() == MouseEvent.MOUSE_DRAGGED
        || event.getID() == MouseEvent.MOUSE_RELEASED)
      mouseOverMenuComp = componentForPoint(source, event.getPoint());
    else
      mouseOverMenuComp = source;

    // Process this event only if mouse is located over some menu element
    if (mouseOverMenuComp != null && (mouseOverMenuComp instanceof MenuElement))
      {
	MenuElement[] path = getPath(mouseOverMenuComp);
	((MenuElement) mouseOverMenuComp).processMouseEvent(event, path,
	                                                    manager);

	// FIXME: Java specification says that mouse events should be
	// forwarded to subcomponents. The code below does it, but
	// menu's work fine without it. This code is commented for now.	  

	/*
	MenuElement[] subComponents = ((MenuElement) mouseOverMenuComp)
	                              .getSubElements();

	for (int i = 0; i < subComponents.length; i++)
	 {
	      subComponents[i].processMouseEvent(event, path, manager);
	 }
	*/
      }
    else
      {
	if (event.getID() == MouseEvent.MOUSE_RELEASED)
	  clearSelectedPath();
      }
  }

  /**
   * Sets menu selection to the specified path
   *
   * @param path new selection path
   */
  public void setSelectedPath(MenuElement[] path)
  {
    if (path == null)
      {
	clearSelectedPath();
	return;
      }

    int minSize = path.length; // size of the smaller path.
    int currentSize = selectedPath.size();
    int firstDiff = 0;

    // Search first item that is different in the current and new path.
    for (int i = 0; i < minSize; i++)
      {
        if (i < currentSize && (MenuElement) selectedPath.get(i) == path[i])
          firstDiff++;
        else
          break;
      }

    // Remove items from selection and send notification.
    for (int i = currentSize - 1; i >= firstDiff; i--)
      {
        MenuElement el = (MenuElement) selectedPath.get(i);
        selectedPath.remove(i);
        el.menuSelectionChanged(false);
      }

    // Add new items to selection and send notification.
    for (int i = firstDiff; i < minSize; i++)
      {
        if (path[i] != null)
          {
            selectedPath.add(path[i]);
            path[i].menuSelectionChanged(true);
          }
      }

    fireStateChanged();
  }

  /**
   * Returns path to the specified component
   *
   * @param c component for which to find path for
   *
   * @return path to the specified component
   */
  private MenuElement[] getPath(Component c)
  {
    // FIXME: There is the same method in BasicMenuItemUI. However I
    // cannot use it here instead of this method, since I cannot assume that 
    // all the menu elements on the selected path are JMenuItem or JMenu.
    // For now I've just duplicated it here. Please 
    // fix me or delete me if another better approach will be found, and 
    // this method will not be necessary.
    ArrayList path = new ArrayList();

    // if given component is JMenu, we also need to include 
    // it's popup menu in the path 
    if (c instanceof JMenu)
      path.add(((JMenu) c).getPopupMenu());
    while (c instanceof MenuElement)
      {
	path.add(0, (MenuElement) c);

	if (c instanceof JPopupMenu)
	  c = ((JPopupMenu) c).getInvoker();
	else
	  c = c.getParent();
      }

    MenuElement[] pathArray = new MenuElement[path.size()];
    path.toArray(pathArray);
    return pathArray;
  }
}
