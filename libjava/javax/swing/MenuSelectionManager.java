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
import java.awt.event.MouseEvent;

import java.util.Vector;

import javax.swing.JMenu;
import javax.swing.JMenuItem;
import javax.swing.event.ChangeEvent;
import javax.swing.event.ChangeListener;
import javax.swing.event.EventListenerList;

public class MenuSelectionManager
{
  protected ChangeEvent changeEvent;
  
  protected EventListenerList listenerList = new EventListenerList ();

  private static final MenuSelectionManager manager = new MenuSelectionManager();
  
  private Vector selection = new Vector();
  
  protected void fireStateChanged ()
  {
    ChangeListener[] listeners = getChangeListeners ();

    for (int i = 0; i < listeners.length; i++)
      {
        listeners [i].stateChanged (new ChangeEvent (this));
      }
  }

  public void addChangeListener (ChangeListener listener)
  {
    listenerList.add (ChangeListener.class, listener);
  }

  public void removeChangeListener (ChangeListener listener)
  {
    listenerList.remove (ChangeListener.class, listener);
  }

  /** @since 1.4 */
  public ChangeListener[] getChangeListeners ()
  {
    return (ChangeListener[]) listenerList.getListeners (ChangeListener.class);
  }
  
  /**
   * Unselects all the menu elements on the selection path 
   */
  public void clearSelectedPath ()
  {
    for (int i = 0; i < selection.size (); i++)
      ((MenuElement) selection.get (i)).menuSelectionChanged (false);

    selection.clear ();
  }
  
  public Component componentForPoint (Component source, Point sourcePoint)
  {
    throw new UnsupportedOperationException("not implemented");
  }

  /**
   * Returns shared instance of MenuSelection Manager
   *
   * @return default Manager
   */
  public static MenuSelectionManager defaultManager ()
  {
    return manager;
  }

  /**
   * Returns path representing current menu selection
   *
   * @return Current selection path
   */
  public MenuElement[] getSelectedPath ()
  {
    MenuElement[] path = new MenuElement[selection.size ()];

    for (int i = 0; i < path.length; i++)
      path[i] = (MenuElement) selection.get (i);

    return path;
  }

  /**
   * Returns true if specified component is part of current menu
   * heirarchy and false otherwise
   *
   * @param c Component for which to check
   * @return True if specified component is part of current menu
   */
  boolean isComponentPartOfCurrentMenu (Component c)
  {
    MenuElement[] subElements;
    for (int i = 0; i < selection.size (); i++)
      {
        subElements = ((MenuElement) selection.get (i)).getSubElements ();
        for (int j = 0; j < subElements.length; j++)
          {
            if ((subElements[j].getComponent ()).equals (c))
              return true;
          }
      }

    return false;
  }

  /**
   * DOCUMENT ME!
   *
   * @param e DOCUMENT ME!
   */
  public void processKeyEvent (KeyEvent e)
  {
    throw new UnsupportedOperationException("not implemented");
  }

  /**
   * Forwards given mouse event to all of the source subcomponents.
   *
   * @param event Mouse event
   */
  public void processMouseEvent (MouseEvent event)
  {
    
    Component c = ((MenuElement) event.getSource ()).getComponent ();
    if (selection.size () == 0)
      {
        ((MenuElement) event.getSource ()).processMouseEvent (event,
                                                              getPath (c),
                                                              manager);
        return;
      }

    // find the index of the source component in the current menu hierarchy
    int i = 0;
    for (i = 0; i < selection.size (); i++)
      {
        MenuElement me = (MenuElement) selection.get (i);
        if (me.getComponent ().equals (c))
          break;
      }

    // Forward event to all subcomponents of the source 
    Component subComp;
    for (int j = i; j < selection.size (); j++)
      {
         subComp = ((MenuElement)selection.get (j)).getComponent ();
        ((MenuElement) selection.get (j)).processMouseEvent (event,
                                                             getPath (subComp),
                                                             manager);
      }
  }

  /**
   * Sets menu selection to the specified path
   *
   * @param path new selection path
   */
  public void setSelectedPath (MenuElement[] path)
  {
    if (path == null)
      {
        clearSelectedPath ();
        return;
      }

    int i;
    int minSize = path.length; // size of the smaller path. 

    if (path.length > selection.size ())
      {
        // if new selected path contains more elements then current
        // selection then first add all elements at 
        // the indexes > selection.size 
	
        for (i = selection.size (); i < path.length; i++)
          {
            selection.add (path[i]);
            path[i].menuSelectionChanged (true);
          }

        minSize = selection.size ();
      }

    else if (path.length < selection.size ())
      {
        // if new selected path contains less elements then current 
        // selection then first remove all elements from the selection
        // at the indexes > path.length
	
        for (i = selection.size () - 1; i >= path.length; i--)
          {
            ((MenuElement) selection.get (i)).menuSelectionChanged (false);
            selection.remove (i);
          }

        minSize = path.length;
      }

    // Now compare elements in new and current selection path at the 
    // same location and adjust selection until 
    // same menu elements will be encountered at the
    // same index in both current and new selection path.
    
    MenuElement oldSelection;

    for (i = minSize - 1; i >= 0; i--)
      {
        oldSelection = (MenuElement) selection.get (i);

        if (path[i].equals (oldSelection))
          break;

        oldSelection.menuSelectionChanged (false);
        path[i].menuSelectionChanged (true);
        selection.setElementAt (path[i], i);
      }
  }


  /**
   * Returns path to the specified component
   *
   * @param c component for which to find path for
   *
   * @return path to the specified component
   */
  private MenuElement[] getPath (Component c)
  {
    Vector path = new Vector();
    path.add (c);

    Component parent = c.getParent ();

    while (parent instanceof JMenu 
           || parent instanceof JPopupMenu 
           || parent instanceof JMenuItem 
           || parent instanceof JMenuBar)
      {
        path.add (parent);
        parent = parent.getParent ();
      }

    MenuElement[] pathArray = new MenuElement[path.size ()];

    for (int i = 0; i < path.size (); i++)
      pathArray[i] = (MenuElement) path.get (path.size () - i - 1);
    return pathArray;
  }
  
} // class MenuSelectionManager
