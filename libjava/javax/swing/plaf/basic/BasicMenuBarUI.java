/* BasicMenuUI.java
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

import java.awt.event.ContainerEvent;
import java.awt.event.ContainerListener;
import java.beans.PropertyChangeEvent;
import java.beans.PropertyChangeListener;
import java.awt.Dimension;
import javax.swing.BoxLayout;
import javax.swing.ButtonModel;
import javax.swing.Icon;
import javax.swing.JCheckBoxMenuItem;
import javax.swing.JComponent;
import javax.swing.JMenu;
import javax.swing.JMenuBar;
import javax.swing.JMenuItem;
import javax.swing.JPopupMenu;
import javax.swing.JRadioButtonMenuItem;
import javax.swing.KeyStroke;
import javax.swing.MenuElement;
import javax.swing.MenuSelectionManager;
import javax.swing.SwingUtilities;
import javax.swing.UIDefaults;
import javax.swing.UIManager;
import javax.swing.event.ChangeEvent;
import javax.swing.event.ChangeListener;
import javax.swing.event.MenuDragMouseEvent;
import javax.swing.event.MenuDragMouseListener;
import javax.swing.event.MenuEvent;
import javax.swing.event.MenuKeyEvent;
import javax.swing.event.MenuKeyListener;
import javax.swing.event.MenuListener;
import javax.swing.event.MouseInputListener;
import javax.swing.plaf.ComponentUI;
import javax.swing.plaf.MenuBarUI;
import javax.swing.plaf.MenuItemUI;
import java.awt.Insets;
import java.awt.GridLayout;


/**
 * DOCUMENT ME!
 */
public class BasicMenuBarUI extends MenuBarUI
{
  protected ChangeListener changeListener;
  protected ContainerListener containerListener;
  protected PropertyChangeListener propertyChangeListener;
  protected JMenuBar menuBar;

  /**
   * Creates a new BasicMenuBarUI object.
   */
  public BasicMenuBarUI()
  {
    changeListener = createChangeListener();
    containerListener = createContainerListener();
    propertyChangeListener = new PropertyChangeHandler();
  }

  /**
   * DOCUMENT ME!
   *
   * @return DOCUMENT ME!
   */
  protected ChangeListener createChangeListener()
  {
    return new ChangeHandler();
  }

  /**
   * DOCUMENT ME!
   *
   * @return DOCUMENT ME!
   */
  protected ContainerListener  createContainerListener()
  {
    return new ContainerHandler();
  }

  /**
   * DOCUMENT ME!
   *
   * @param x DOCUMENT ME!
   *
   * @return DOCUMENT ME!
   */
  public static ComponentUI createUI(JComponent x)
  {
    return new BasicMenuBarUI();
  }

  /**
   * DOCUMENT ME!
   *
   * @param c DOCUMENT ME!
   *
   * @return DOCUMENT ME!
   */
  public Dimension getMaximumSize(JComponent c)
  {
    // let layout manager calculate its size
    return null;
  }

  /**
   * DOCUMENT ME!
   *
   * @param c DOCUMENT ME!
   *
   * @return DOCUMENT ME!
   */
  public Dimension getMinimumSize(JComponent c)
  {
    // let layout manager calculate its size
    return null;
  }

  /**
   * DOCUMENT ME!
   *
   * @param c DOCUMENT ME!
   *
   * @return DOCUMENT ME!
   */
  public Dimension getPreferredSize(JComponent c)
  {
    // let layout manager calculate its size
    return null;
  }

  /**
   * DOCUMENT ME!
   */
  protected void installDefaults()
  {
    UIDefaults defaults = UIManager.getLookAndFeelDefaults();

    menuBar.setBackground(defaults.getColor("MenuBar.background"));
    menuBar.setBorder(defaults.getBorder("MenuBar.border"));
    menuBar.setFont(defaults.getFont("MenuBar.font"));
    menuBar.setForeground(defaults.getColor("MenuBar.foreground"));
  }

  /**
   * DOCUMENT ME!
   */
  protected void installKeyboardActions()
  {
  }

  /**
   * DOCUMENT ME!
   */
  protected void installListeners()
  {        
    menuBar.addContainerListener(containerListener);
    menuBar.addPropertyChangeListener(propertyChangeListener);    
  }

  /**
   * DOCUMENT ME!
   *
   * @param c DOCUMENT ME!
   */
  public void installUI(JComponent c)
  {
    super.installUI(c);
    menuBar = (JMenuBar) c;
    menuBar.setLayout(new BoxLayout(menuBar, BoxLayout.X_AXIS));
    installDefaults();
    installListeners();
  }

  /**
   * DOCUMENT ME!
   */
  protected void uninstallDefaults()
  {
    menuBar.setBackground(null);
    menuBar.setBorder(null);
    menuBar.setFont(null);
    menuBar.setForeground(null);
  }

  /**
   * DOCUMENT ME!
   */
  protected void uninstallKeyboardActions()
  {
  }

  /**
   * DOCUMENT ME!
   */
  protected void uninstallListeners()
  {
    menuBar.removeContainerListener(containerListener);
    menuBar.removePropertyChangeListener(propertyChangeListener);
  }

  /**
   * DOCUMENT ME!
   *
   * @param c DOCUMENT ME!
   */
  public void uninstallUI(JComponent c)
  {
    uninstallDefaults();
    uninstallListeners();    
    menuBar= null;
  }

  protected class ChangeHandler implements ChangeListener
  {
    public void stateChanged(ChangeEvent event)
    {
    }
  }

  protected class ContainerHandler implements ContainerListener
  {
    public void componentAdded(ContainerEvent e)
    {
    }

    public void componentRemoved(ContainerEvent e)
    {
    }
  }

  protected class PropertyChangeHandler implements PropertyChangeListener
  {
    public void propertyChange(PropertyChangeEvent e)
    {
    }
  }
}
