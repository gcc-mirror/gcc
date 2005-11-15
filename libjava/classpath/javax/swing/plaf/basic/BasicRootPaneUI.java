/* BasicPanelUI.java --
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

import java.beans.PropertyChangeEvent;
import java.beans.PropertyChangeListener;

import javax.swing.JComponent;
import javax.swing.JRootPane;
import javax.swing.UIManager;
import javax.swing.plaf.ComponentUI;
import javax.swing.plaf.RootPaneUI;

public class BasicRootPaneUI extends RootPaneUI
  implements PropertyChangeListener
{
  public static ComponentUI createUI(JComponent x) 
  {
    return new BasicRootPaneUI();
  }

  public void installUI(JComponent c)
  {
    super.installUI(c);
    if (c instanceof JRootPane)
      {
        JRootPane rp = (JRootPane) c;
        installDefaults(rp);
        installComponents(rp);
        installListeners(rp);
        installKeyboardActions(rp);
      }
  }

  /**
   * Installs the look and feel defaults for JRootPane.
   *
   * @param rp the root pane to install the defaults to
   */
  protected void installDefaults(JRootPane rp)
  {
    // Is this ok?
    rp.setBackground(UIManager.getColor("control"));
  }

  /**
   * Installs additional look and feel components to the root pane.
   *
   * @param rp the root pane to install the components to
   */
  protected void installComponents(JRootPane rp)
  {
    // All components are initialized in the JRootPane constructor, and since
    // the createXXXPane methods are protected, I see no reasonable way,
    // and no need to initialize them here. This method is here anyway
    // for compatibility and to provide the necessary hooks to subclasses.
  }

  /**
   * Installs any look and feel specific listeners on the root pane.
   *
   * @param rp the root pane to install the listeners to
   */
  protected void installListeners(JRootPane rp)
  {
    rp.addPropertyChangeListener(this);
  }

  /**
   * Installs look and feel keyboard actions on the root pane.
   *
   * @param rp the root pane to install the keyboard actions to
   */
  protected void installKeyboardActions(JRootPane rp)
  {
    // We currently do not install any keyboard actions here.
    // This method is here anyway for compatibility and to provide
    // the necessary hooks to subclasses.
  }

  public void propertyChange(PropertyChangeEvent event)
  {
    // TODO: Implement this properly.
  }

  /**
   * Uninstalls this UI from the root pane. This calls
   * {@link #uninstallDefaults}, {@link #uninstallComponents},
   * {@link #uninstallListeners}, {@link #uninstallKeyboardActions}
   * in this order.
   *
   * @param c the root pane to uninstall the UI from
   */
  public void uninstallUI(JComponent c)
  {
    super.uninstallUI(c);
    if (c instanceof JRootPane)
      {
        JRootPane rp = (JRootPane) c;
        uninstallDefaults(rp);
        uninstallComponents(rp);
        uninstallListeners(rp);
        uninstallKeyboardActions(rp);
      }
  }

  /**
   * Uninstalls the look and feel defaults that have been installed in
   * {@link #installDefaults}.
   *
   * @param rp the root pane to uninstall the defaults from
   */
  protected void uninstallDefaults(JRootPane rp)
  {
    // We do nothing here.
  }

  /**
   * Uninstalls look and feel components from the root pane.
   *
   * @param rp the root pane to uninstall the components from
   */
  protected void uninstallComponents(JRootPane rp)
  {
    // We do nothing here.
  }

  /**
   * Uninstalls any look and feel specific listeners from the root pane.
   *
   * @param rp the root pane to uninstall the listeners from
   */
  protected void uninstallListeners(JRootPane rp)
  {
    rp.removePropertyChangeListener(this);
  }

  /**
   * Uninstalls look and feel keyboard actions from the root pane.
   *
   * @param rp the root pane to uninstall the keyboard actions from
   */
  protected void uninstallKeyboardActions(JRootPane rp)
  {
    // We do nothing here.
  }
}
