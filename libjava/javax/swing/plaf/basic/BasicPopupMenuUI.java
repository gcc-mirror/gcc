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

import java.awt.AWTKeyStroke;
import java.awt.BasicStroke;
import java.awt.Color;
import java.awt.Dimension;
import java.awt.Font;
import java.awt.FontMetrics;
import java.awt.Graphics;
import java.awt.Graphics2D;
import java.awt.GridBagLayout;
import java.awt.Insets;
import java.awt.Rectangle;
import java.awt.Stroke;
import java.awt.event.FocusAdapter;
import java.awt.event.FocusEvent;
import java.awt.event.FocusListener;
import java.awt.event.InputEvent;
import java.awt.event.KeyEvent;
import java.awt.event.MouseAdapter;
import java.awt.event.MouseEvent;
import java.beans.PropertyChangeEvent;
import java.beans.PropertyChangeListener;
import java.util.EventListener;
import javax.swing.AbstractButton;
import javax.swing.ButtonModel;
import javax.swing.Icon;
import javax.swing.JCheckBoxMenuItem;
import javax.swing.JComponent;
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
import javax.swing.event.MenuKeyEvent;
import javax.swing.event.MenuKeyListener;
import javax.swing.event.MouseInputListener;
import javax.swing.event.PopupMenuEvent;
import javax.swing.event.PopupMenuListener;
import javax.swing.plaf.ComponentUI;
import javax.swing.plaf.MenuItemUI;
import javax.swing.plaf.PopupMenuUI;


/**
 * DOCUMENT ME!
 *
 * @author $author$
 * @version $Revision: 1.1.2.1 $
 */
public class BasicPopupMenuUI extends PopupMenuUI
{
  protected JPopupMenu popupMenu;
  private static transient MouseInputListener mouseInputListener;
  private transient PopupMenuListener popupMenuListener;

  /**
   * Creates a new BasicPopupMenuUI object.
   */
  public BasicPopupMenuUI()
  {
    popupMenuListener = new PopupMenuHandler();
    mouseInputListener = new MouseInputHandler();
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
    return new BasicPopupMenuUI();
  }

  /**
   * DOCUMENT ME!
   *
   * @param c DOCUMENT ME!
   */
  public void installUI(JComponent c)
  {
    super.installUI(c);
    popupMenu = (JPopupMenu) c;
    popupMenu.setLayout(new GridBagLayout());
    popupMenu.setBorderPainted(true);
    popupMenu.setDefaultLightWeightPopupEnabled(true);

    installDefaults();
    installListeners();
  }

  /**
   * DOCUMENT ME!
   */
  public void installDefaults()
  {
    UIDefaults defaults = UIManager.getLookAndFeelDefaults();

    popupMenu.setBackground(defaults.getColor("PopupMenu.background"));
    popupMenu.setBorder(defaults.getBorder("PopupMenu.border"));
    popupMenu.setFont(defaults.getFont("PopupMenu.font"));
    popupMenu.setForeground(defaults.getColor("PopupMenu.foreground"));
  }

  /**
   * DOCUMENT ME!
   */
  protected void installListeners()
  {
    popupMenu.addMouseListener(mouseInputListener);
    popupMenu.addMouseMotionListener(mouseInputListener);
    popupMenu.addPopupMenuListener(popupMenuListener);
  }

  /**
   * DOCUMENT ME!
   */
  protected void installKeyboardActions()
  {
  }

  /**
   * DOCUMENT ME!
   *
   * @param c DOCUMENT ME!
   */
  public void uninstallUI(JComponent c)
  {
    uninstallListeners();
    uninstallDefaults();
    popupMenu = null;
  }

  /**
   * DOCUMENT ME!
   */
  protected void uninstallDefaults()
  {
    popupMenu.setBackground(null);
    popupMenu.setBorder(null);
    popupMenu.setFont(null);
    popupMenu.setForeground(null);
  }

  /**
   * DOCUMENT ME!
   */
  protected void uninstallListeners()
  {
  }

  /**
   * DOCUMENT ME!
   */
  protected void uninstallKeyboardActions()
  {
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
    return null;
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
    return null;
  }

  /**
   * DOCUMENT ME!
   *
   * @param e DOCUMENT ME!
   *
   * @return DOCUMENT ME!
   */
  public boolean isPopupTrigger(MouseEvent e)
  {
    return false;
  }

  /**
   * DOCUMENT ME!
   *
   * @author $author$
   * @version $Revision: 1.1.2.1 $
   */
  protected class PopupMenuHandler implements PopupMenuListener
  {
    /**
     * DOCUMENT ME!
     *
     * @param event DOCUMENT ME!
     */
    public void popupMenuCanceled(PopupMenuEvent event)
    {
    }

    /**
     * DOCUMENT ME!
     *
     * @param event DOCUMENT ME!
     */
    public void popupMenuWillBecomeInvisible(PopupMenuEvent event)
    {
    }

    /**
     * DOCUMENT ME!
     *
     * @param event DOCUMENT ME!
     */
    public void popupMenuWillBecomeVisible(PopupMenuEvent event)
    {
    }
  }

  /**
   * DOCUMENT ME!
   *
   * @author $author$
   * @version $Revision: 1.1.2.1 $
   */
  protected class MouseInputHandler implements MouseInputListener
  {
    /**
     * DOCUMENT ME!
     *
     * @param e DOCUMENT ME!
     */
    public void mouseClicked(MouseEvent e)
    {
    }

    /**
     * DOCUMENT ME!
     *
     * @param e DOCUMENT ME!
     */
    public void mouseDragged(MouseEvent e)
    {
    }

    /**
     * DOCUMENT ME!
     *
     * @param e DOCUMENT ME!
     */
    public void mouseEntered(MouseEvent e)
    {
    }

    /**
     * DOCUMENT ME!
     *
     * @param e DOCUMENT ME!
     */
    public void mouseExited(MouseEvent e)
    {
    }

    /**
     * DOCUMENT ME!
     *
     * @param e DOCUMENT ME!
     */
    public void mouseMoved(MouseEvent e)
    {
    }

    /**
     * DOCUMENT ME!
     *
     * @param e DOCUMENT ME!
     */
    public void mousePressed(MouseEvent e)
    {
    }

    /**
     * DOCUMENT ME!
     *
     * @param e DOCUMENT ME!
     */
    public void mouseReleased(MouseEvent e)
    {
    }
  }
}
