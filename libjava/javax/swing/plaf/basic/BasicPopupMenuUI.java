/* BasicPopupMenuUI.java
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

package javax.swing.plaf.basic;

import java.awt.Component;
import java.awt.Container;
import java.awt.Dimension;
import java.awt.GridBagLayout;
import java.awt.event.ComponentEvent;
import java.awt.event.ComponentListener;
import java.awt.event.MouseEvent;

import javax.swing.JComponent;
import javax.swing.JPopupMenu;
import javax.swing.MenuElement;
import javax.swing.MenuSelectionManager;
import javax.swing.SwingUtilities;
import javax.swing.UIDefaults;
import javax.swing.UIManager;
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

  /* MouseInputListener listens to mouse events */
  private static transient MouseInputListener mouseInputListener;

  /* PopupMenuListener listens to popup menu events fired by JPopupMenu*/
  private transient PopupMenuListener popupMenuListener;

  /* ComponentListener listening to popupMenu's invoker. */
  private TopWindowListener topWindowListener;

  /**
   * Creates a new BasicPopupMenuUI object.
   */
  public BasicPopupMenuUI()
  {
    popupMenuListener = new PopupMenuHandler();
    mouseInputListener = new MouseInputHandler();
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
    popupMenu.setLayout(new GridBagLayout());
    popupMenu.setBorderPainted(true);
    JPopupMenu.setDefaultLightWeightPopupEnabled(true);

    installDefaults();
    installListeners();
  }

  /**
   * This method installs the defaults that are defined in  the Basic look and
   * feel for this {@link JPopupMenu}.
   */
  public void installDefaults()
  {
    UIDefaults defaults = UIManager.getLookAndFeelDefaults();

    popupMenu.setBackground(defaults.getColor("PopupMenu.background"));
    popupMenu.setBorder(defaults.getBorder("PopupMenu.border"));
    popupMenu.setFont(defaults.getFont("PopupMenu.font"));
    popupMenu.setForeground(defaults.getColor("PopupMenu.foreground"));
    popupMenu.setOpaque(true);
  }

  /**
   * This method installs the listeners for the {@link JMenuItem}.
   */
  protected void installListeners()
  {
    popupMenu.addMouseListener(mouseInputListener);
    popupMenu.addMouseMotionListener(mouseInputListener);
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
    popupMenu.removeMouseListener(mouseInputListener);
    popupMenu.removeMouseMotionListener(mouseInputListener);
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

      Container rootContainer = (Container) SwingUtilities.getRoot(invoker);
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
      Container rootContainer = (Container) SwingUtilities.getRoot(invoker);
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
     * This method is invoked when top-level window is shown This method does
     * nothing by default.
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

  private class MouseInputHandler implements MouseInputListener
  {
    public void mouseClicked(MouseEvent e)
    {
    }

    public void mouseDragged(MouseEvent e)
    {
    }

    public void mouseEntered(MouseEvent e)
    {
    }

    public void mouseExited(MouseEvent e)
    {
    }

    public void mouseMoved(MouseEvent e)
    {
    }

    public void mousePressed(MouseEvent e)
    {
    }

    public void mouseReleased(MouseEvent e)
    {
    }
  }
}
