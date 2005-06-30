/* BasicDesktopPaneUI.java --
   Copyright (C) 2004 Free Software Foundation, Inc.

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

import java.awt.Dimension;
import java.awt.event.ActionEvent;
import java.beans.PropertyVetoException;

import javax.swing.AbstractAction;
import javax.swing.DefaultDesktopManager;
import javax.swing.DesktopManager;
import javax.swing.JComponent;
import javax.swing.JDesktopPane;
import javax.swing.JInternalFrame;
import javax.swing.KeyStroke;
import javax.swing.UIDefaults;
import javax.swing.UIManager;
import javax.swing.plaf.ComponentUI;
import javax.swing.plaf.DesktopPaneUI;

/**
 * This class is the UI delegate for JDesktopPane for the Basic look and feel.
 */
public class BasicDesktopPaneUI extends DesktopPaneUI
{
  /**
   * This helper class is used to handle key events that cause JInternalFrames
   * to be closed.
   */
  protected class CloseAction extends AbstractAction
  {
    /**
     * This method is called when the action is performed.
     *
     * @param e The ActionEvent.
     */
    public void actionPerformed(ActionEvent e)
    {
      if (desktop.getSelectedFrame() != null)
        {
	  try
	    {
	      desktop.getSelectedFrame().setClosed(true);
	    }
	  catch (PropertyVetoException pve)
	    {
	    }
        }
    }

    /**
     * This method returns whether the action is enabled.
     *
     * @return Whether the action is enabled.
     */
    public boolean isEnabled()
    {
      if (desktop.getSelectedFrame() != null)
	return desktop.getSelectedFrame().isClosable();
      return false;
    }
  }

  /**
   * This helper class is used to handle key events that cause JInternalFrames
   * to be maximized.
   */
  protected class MaximizeAction extends AbstractAction
  {
    /**
     * This method is called when the action is performed.
     *
     * @param e The ActionEvent.
     */
    public void actionPerformed(ActionEvent e)
    {
      if (desktop.getSelectedFrame() != null)
        {
	  try
	    {
	      desktop.getSelectedFrame().setMaximum(true);
	    }
	  catch (PropertyVetoException pve)
	    {
	    }
        }
    }

    /**
     * This method returns whether the action is enabled.
     *
     * @return Whether the action is enabled.
     */
    public boolean isEnabled()
    {
      if (desktop.getSelectedFrame() != null)
	return desktop.getSelectedFrame().isMaximizable();
      return false;
    }
  }

  /**
   * This helper class is used to handle key events that cause JInternalFrames
   * to be minimized.
   */
  protected class MinimizeAction extends AbstractAction
  {
    /**
     * This method is called when the action is performed.
     *
     * @param e The ActionEvent.
     */
    public void actionPerformed(ActionEvent e)
    {
      if (desktop.getSelectedFrame() != null)
        {
	  try
	    {
	      desktop.getSelectedFrame().setIcon(true);
	    }
	  catch (PropertyVetoException pve)
	    {
	    }
        }
    }

    /**
     * This method returns whether the action is enabled.
     *
     * @return Whether the action is enabled.
     */
    public boolean isEnabled()
    {
      if (desktop.getSelectedFrame() != null)
	return desktop.getSelectedFrame().isIconifiable();
      return false;
    }
  }

  /**
   * This helper class is used to handle key events that pass the SELECTED
   * property to the next JInternalFrame in the JDesktopPane's list of
   * children.
   */
  protected class NavigateAction extends AbstractAction
  {
    /**
     * This method is called when the action is performed.
     *
     * @param e The ActionEvent.
     */
    public void actionPerformed(ActionEvent e)
    {
      // This is supposed to set the next selected frame. 
      JInternalFrame[] frames = desktop.getAllFrames();
      if (frames.length == 0)
	return;

      JInternalFrame sFrame = frames[0];
      if (desktop.getSelectedFrame() != null)
	sFrame = desktop.getSelectedFrame();

      int i = 0;
      for (; i < frames.length; i++)
	if (frames[i] == sFrame)
	  break;

      // FIXME: Navigate actions go reverse too.	  
      if (i == frames.length)
	i = 0;

      desktop.setSelectedFrame(frames[i]);
    }

    /**
     * This method returns whether the action is enabled.
     *
     * @return Whether this action is enabled.
     */
    public boolean isEnabled()
    {
      // Always true.
      return true;
    }
  }

  /**
   * This helper class is used to restore the JInternalFrame to its original
   * size before maximizing or iconifying.
   */
  protected class OpenAction extends AbstractAction
  {
    /**
     * This method is called when the action is performed.
     *
     * @param e The ActionEvent.
     */
    public void actionPerformed(ActionEvent e)
    {
      JInternalFrame frame = desktop.getSelectedFrame();
      if (frame != null)
        {
	  try
	    {
	      if (frame.isIcon())
		frame.setIcon(false);
	      else if (frame.isMaximum())
		frame.setMaximum(false);
	    }
	  catch (PropertyVetoException pve)
	    {
	    }
        }
    }

    /**
     * This method returns whether the action is enabled.
     *
     * @return Whether this action is enabled.
     */
    public boolean isEnabled()
    {
      // JInternalFrames are always restorable.
      return true;
    }
  }

  /**
   * The KeyStroke associated with closing JInternalFrames.
   * @deprecated
   */
  protected KeyStroke closeKey;

  /**
   * The KeyStroke associated with maximizing JInternalFrames.
   * @deprecated
   */
  protected KeyStroke maximizeKey;

  /**
   * The KeyStroke associated with minimizing JInternalFrames.
   * @deprecated
   */
  protected KeyStroke minimizeKey;

  /**
   * The KeyStroke associated with navigating (forward?) through
   * JInternalFrames.
   * @deprecated
   */
  protected KeyStroke navigateKey;

  /**
   * The KeyStroke associated with navigating (backward?) through
   * JInternalFrames.
   * @deprecated
   */
  protected KeyStroke navigateKey2;

  /** The default desktop manager used with JDesktopPane. */
  protected DesktopManager desktopManager;

  /** The JDesktopPane this UI is used with. */
  protected JDesktopPane desktop;

  /**
   * Creates a new BasicDesktopPaneUI object.
   */
  public BasicDesktopPaneUI()
  {
  }

  /**
   * This method creates a BasicDesktopPaneUI for the given JComponent.
   *
   * @param c The JComponent to create a UI for.
   *
   * @return A new BasicDesktopPaneUI.
   */
  public static ComponentUI createUI(JComponent c)
  {
    return new BasicDesktopPaneUI();
  }

  /**
   * This method returns the maximum size for the given JComponent.
   *
   * @param c The JComponent to find a maximum size for.
   *
   * @return The maximum size for the given JComponent.
   */
  public Dimension getMaximumSize(JComponent c)
  {
    return getPreferredSize(c);
  }

  /**
   * This method returns the minimum size for the given JComponent.
   *
   * @param c The JComponent to find a minimum size for.
   *
   * @return The minimum size for the given JComponent.
   */
  public Dimension getMinimumSize(JComponent c)
  {
    return getPreferredSize(c);
  }

  /**
   * This method returns the preferred size for the given JComponent.
   *
   * @param c The JComponent to find a preferred size for.
   *
   * @return The preferred size for the given JComponent.
   */
  public Dimension getPreferredSize(JComponent c)
  {
    // return null because JDesktopPanes don't have preferred sizes.
    return null;
  }

  /**
   * This method installs the defaults for the JDesktopPane provided by the
   * current look and feel.
   */
  protected void installDefaults()
  {
    UIDefaults defaults = UIManager.getLookAndFeelDefaults();

    desktop.setBackground(defaults.getColor("Desktop.background"));
  }

  /**
   * This method installs the desktop manager for the JDesktopPane.
   */
  protected void installDesktopManager()
  {
    desktopManager = new DefaultDesktopManager();
    desktop.setDesktopManager(desktopManager);
  }

  /**
   * This method installs the keyboard actions for the JDesktopPane.
   */
  protected void installKeyboardActions()
  {
    // FIXME: create actions and keystrokes.
    registerKeyboardAction();
  }

  /**
   * This method installs the UI for the given JComponent.
   *
   * @param c The JComponent to install this UI for.
   */
  public void installUI(JComponent c)
  {
    if (c instanceof JDesktopPane)
      {
	desktop = (JDesktopPane) c;

	installDefaults();
	installDesktopManager();
	installKeyboardActions();
      }
  }

  /**
   * This method registers the actions to the appropriate Action and Input
   * maps.
   */
  protected void registerKeyboardAction()
  {
    // FIXME: Do the binding.
    // XXX: the gtk windows tend to intercept a lot of the
    // key events for themselves. must figure a way past that
    // before binding
  }

  /**
   * This method reverses the work done by the installDefaults method.
   */
  protected void uninstallDefaults()
  {
    desktop.setBackground(null);
  }

  /**
   * This method reverses the work done by the installDesktopManager method.
   */
  protected void uninstallDesktopManager()
  {
    desktopManager = null;
    desktop.setDesktopManager(null);
  }

  /**
   * This method reverses the work done by the installKeyboardActions method.
   */
  protected void uninstallKeyboardActions()
  {
    unregisterKeyboardActions();
    // FIXME: null the actions and keystrokes.
  }

  /**
   * This method reverses the work done by the registerKeyboardActions method.
   */
  protected void unregisterKeyboardActions()
  {
    // FIXME: unmap the keystrokes
  }

  /**
   * This method uninstalls the UI for the given JComponent. It should reverse
   * all the work done by the installUI method.
   *
   * @param c The JComponent to uninstall this UI for.
   */
  public void uninstallUI(JComponent c)
  {
    uninstallKeyboardActions();
    uninstallDesktopManager();
    uninstallDefaults();

    desktop = null;
  }
}
