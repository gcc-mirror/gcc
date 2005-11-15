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

import java.awt.AWTEvent;
import java.awt.Component;
import java.awt.Container;
import java.awt.Cursor;
import java.awt.Dimension;
import java.awt.Point;
import java.awt.event.ComponentEvent;
import java.awt.event.ComponentListener;
import java.awt.event.MouseEvent;

import javax.swing.BoxLayout;
import javax.swing.JComponent;
import javax.swing.JLayeredPane;
import javax.swing.JMenu;
import javax.swing.JMenuItem;
import javax.swing.JPopupMenu;
import javax.swing.LookAndFeel;
import javax.swing.MenuElement;
import javax.swing.MenuSelectionManager;
import javax.swing.RootPaneContainer;
import javax.swing.SwingUtilities;
import javax.swing.event.MouseInputListener;
import javax.swing.event.PopupMenuEvent;
import javax.swing.event.PopupMenuListener;
import javax.swing.plaf.ComponentUI;
import javax.swing.plaf.PopupMenuUI;


/**
 * UI Delegate for JPopupMenu
 */
public class BasicPopupMenuUI extends PopupMenuUI
{
  /* popupMenu for which this UI delegate is for*/
  protected JPopupMenu popupMenu;

  /* MouseInputListener listens to mouse events. Package private for inner classes. */
  static transient MouseInputListener mouseInputListener;

  /* PopupMenuListener listens to popup menu events fired by JPopupMenu*/
  private transient PopupMenuListener popupMenuListener;

  /* ComponentListener listening to popupMenu's invoker.
   * This is package-private to avoid an accessor method.  */
  TopWindowListener topWindowListener;

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
    popupMenu = (JPopupMenu) c;
    popupMenu.setLayout(new DefaultMenuLayout(popupMenu, BoxLayout.Y_AXIS));
    popupMenu.setBorderPainted(true);
    JPopupMenu.setDefaultLightWeightPopupEnabled(true);

    installDefaults();
    installListeners();
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
    // FIXME: Need to implement
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
    popupMenu = null;
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
    // FIXME: Need to implement
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

      RootPaneContainer rootContainer = (RootPaneContainer) SwingUtilities
                                        .getRoot(invoker);
      if (rootContainer != null)
        {
          ((Container) rootContainer).removeComponentListener(topWindowListener);

          // If this popup menu is the last popup menu visible on the screen,
          // then
          // stop interrupting mouse events in the glass pane before hiding this
          // last popup menu.
          boolean topLevelMenu = (popupMenu.getInvoker() instanceof JMenu)
                                 && ((JMenu) popupMenu.getInvoker()).isTopLevelMenu();

          if (topLevelMenu || !(popupMenu.getInvoker() instanceof MenuElement))
            {
              // set glass pane not to interrupt mouse events and remove
              // mouseInputListener
              Container glassPane = (Container) rootContainer.getGlassPane();
              glassPane.setVisible(false);
              glassPane.removeMouseListener(mouseInputListener);
              mouseInputListener = null;
            }
        }
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
      RootPaneContainer rootContainer = (RootPaneContainer) SwingUtilities
                                        .getRoot(invoker);
      ((Container) rootContainer).addComponentListener(topWindowListener);

      // Set the glass pane to interrupt all mouse events originating in root 
      // container
      if (mouseInputListener == null)
        {
	  Container glassPane = (Container) rootContainer.getGlassPane();
	  glassPane.setVisible(true);
	  mouseInputListener = new MouseInputHandler(rootContainer);
	  glassPane.addMouseListener(mouseInputListener);
	  glassPane.addMouseMotionListener(mouseInputListener);
        }

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

  /**
   * MouseInputHandler listens to all mouse events originated in the root
   * container. This class is responsible for closing menu hierarchy when the
   * user presses mouse over any component that do not belong to the current 
   * menu hierarchy. This is acomplished by interrupting all mouse event in 
   * the glass pane and checking if other component was pressed while menu 
   * was open, before redestributing events further to intended components
   */
  private class MouseInputHandler implements MouseInputListener
  {
    private JLayeredPane layeredPane;
    private Container glassPane;
    private Cursor nativeCursor;
    private transient Component mouseEventTarget;
    private transient Component pressedComponent;
    private transient Component lastComponentEntered;
    private transient Component tempComponent;
    private transient int pressCount;

    /**
     * Creates a new MouseInputHandler object.
     *
     * @param c the top most root container
     */
    public MouseInputHandler(RootPaneContainer c)
    {
      layeredPane = c.getLayeredPane();
      glassPane = (Container) c.getGlassPane();
    }

    /**
     * Handles mouse clicked event
     *
     * @param e Mouse event
     */
    public void mouseClicked(MouseEvent e)
    {
      handleEvent(e);
    }

    /**
     * Handles mouseDragged event
     *
     * @param e MouseEvent
     */
    public void mouseDragged(MouseEvent e)
    {
      handleEvent(e);
    }

    /**
     * Handles mouseEntered event
     *
     * @param e MouseEvent
     */
    public void mouseEntered(MouseEvent e)
    {
      handleEvent(e);
    }

    /**
     * Handles mouseExited event
     *
     * @param e MouseEvent
     */
    public void mouseExited(MouseEvent e)
    {
      handleEvent(e);
    }

    /**
     * Handles mouse moved event
     *
     * @param e MouseEvent
     */
    public void mouseMoved(MouseEvent e)
    {
      handleEvent(e);
    }

    /**
     * Handles mouse pressed event
     *
     * @param e MouseEvent
     */
    public void mousePressed(MouseEvent e)
    {
      handleEvent(e);
    }

    /**
     * Handles mouse released event
     *
     * @param e MouseEvent
     */
    public void mouseReleased(MouseEvent e)
    {
      handleEvent(e);
    }

    /*
     * This method determines component that was intended to received mouse
     * event, before it was interrupted within the glass pane. This method
     * also redispatches mouse entered and mouse exited events to the
     * appropriate components. This code is slightly modified code from
     * Container.LightweightDispatcher class, which is private inside
     * Container class and cannot be used here.
     */
    public void acquireComponentForMouseEvent(MouseEvent me)
    {
      int x = me.getX();
      int y = me.getY();

      // Find the candidate which should receive this event.
      Component parent = layeredPane;
      Component candidate = null;
      Point p = me.getPoint();
      while ((candidate == null) && (parent != null))
        {
	  p = SwingUtilities.convertPoint(glassPane, p.x, p.y, parent);
	  candidate = SwingUtilities.getDeepestComponentAt(parent, p.x, p.y);

	  if (candidate == null)
	    {
	      p = SwingUtilities.convertPoint(parent, p.x, p.y,
	                                      parent.getParent());
	      parent = parent.getParent();
	    }
        }

      // If the only candidate we found was the native container itself,
      // don't dispatch any event at all.  We only care about the lightweight
      // children here.
      if (candidate == layeredPane)
	candidate = null;

      // If our candidate is new, inform the old target we're leaving.
      if ((lastComponentEntered != null) && lastComponentEntered.isShowing()
          && (lastComponentEntered != candidate))
        {
	  // Old candidate could have been removed from 
	  // the layeredPane so we check first.
	  if (SwingUtilities.isDescendingFrom(lastComponentEntered, layeredPane))
	    {
	      Point tp = SwingUtilities.convertPoint(layeredPane, x, y,
	                                             lastComponentEntered);
	      MouseEvent exited = new MouseEvent(lastComponentEntered,
	                                         MouseEvent.MOUSE_EXITED,
	                                         me.getWhen(),
	                                         me.getModifiersEx(), tp.x,
	                                         tp.y, me.getClickCount(),
	                                         me.isPopupTrigger(),
	                                         me.getButton());

              tempComponent = lastComponentEntered;
              lastComponentEntered = null;
	      tempComponent.dispatchEvent(exited);
	    }

	  lastComponentEntered = null;
        }

      // If we have a candidate, maybe enter it.
      if (candidate != null)
        {
	  mouseEventTarget = candidate;

	  if (candidate.isLightweight() && candidate.isShowing()
	      && (candidate != layeredPane)
	      && (candidate != lastComponentEntered))
	    {
	      lastComponentEntered = mouseEventTarget;

	      Point cp = SwingUtilities.convertPoint(layeredPane, x, y,
	                                             lastComponentEntered);
	      MouseEvent entered = new MouseEvent(lastComponentEntered,
	                                          MouseEvent.MOUSE_ENTERED,
	                                          me.getWhen(),
	                                          me.getModifiersEx(), cp.x,
	                                          cp.y, me.getClickCount(),
	                                          me.isPopupTrigger(),
	                                          me.getButton());
	      lastComponentEntered.dispatchEvent(entered);
	    }
        }

      if ((me.getID() == MouseEvent.MOUSE_RELEASED)
          || ((me.getID() == MouseEvent.MOUSE_PRESSED) && (pressCount > 0))
          || (me.getID() == MouseEvent.MOUSE_DRAGGED))
        {
	  // If any of the following events occur while a button is held down,
	  // they should be dispatched to the same component to which the
	  // original MOUSE_PRESSED event was dispatched:
	  //   - MOUSE_RELEASED
	  //   - MOUSE_PRESSED: another button pressed while the first is held down
	  //   - MOUSE_DRAGGED
	  if (SwingUtilities.isDescendingFrom(pressedComponent, layeredPane))
	    mouseEventTarget = pressedComponent;
	  else if (me.getID() == MouseEvent.MOUSE_CLICKED)
	    {
	      // Don't dispatch CLICKED events whose target is not the same as the
	      // target for the original PRESSED event.
	      if (candidate != pressedComponent)
		mouseEventTarget = null;
	      else if (pressCount == 0)
		pressedComponent = null;
	    }
        }
    }

    /*
     * This method handles mouse events interrupted by glassPane. It
     * redispatches the mouse events appropriately to the intended components.
     * The code in this method is also taken from
     * Container.LightweightDispatcher class. The code is slightly modified
     * to handle the case when mouse is released over non-menu component. In
     * this case this method closes current menu hierarchy before 
     * redispatching the event further.
     */
    public void handleEvent(AWTEvent e)
    {
      if (e instanceof MouseEvent)
        {
	  MouseEvent me = (MouseEvent) e;

	  acquireComponentForMouseEvent(me);

	  // Avoid dispatching ENTERED and EXITED events twice.
	  if (mouseEventTarget != null && mouseEventTarget.isShowing()
	      && (e.getID() != MouseEvent.MOUSE_ENTERED)
	      && (e.getID() != MouseEvent.MOUSE_EXITED))
	    {
	      MouseEvent newEvt = SwingUtilities.convertMouseEvent(glassPane,
	                                                           me,
	                                                           mouseEventTarget);

	      mouseEventTarget.dispatchEvent(newEvt);

	      // If mouse was clicked over the component that is not part 
	      // of menu hierarchy,then must close the menu hierarchy */
	      if (e.getID() == MouseEvent.MOUSE_RELEASED)
	        {
		  boolean partOfMenuHierarchy = false;
		  MenuSelectionManager manager = MenuSelectionManager
		                                 .defaultManager();

		  partOfMenuHierarchy = manager.isComponentPartOfCurrentMenu(mouseEventTarget);

		  if (! partOfMenuHierarchy)
		    manager.clearSelectedPath();
	        }

	      switch (e.getID())
	        {
		case MouseEvent.MOUSE_PRESSED:
		  if (pressCount++ == 0)
		    pressedComponent = mouseEventTarget;
		  break;
		case MouseEvent.MOUSE_RELEASED:
		  // Clear our memory of the original PRESSED event, only if
		  // we're not expecting a CLICKED event after this. If
		  // there is a CLICKED event after this, it will do clean up.
		  if ((--pressCount == 0)
		      && (mouseEventTarget != pressedComponent))
		    pressedComponent = null;
		  break;
	        }
	    }
        }
    }
  }
}
