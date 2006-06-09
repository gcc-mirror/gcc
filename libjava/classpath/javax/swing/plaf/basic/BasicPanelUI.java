/* BasicPanelUI.java
   Copyright (C) 2002, 2004, 2006, Free Software Foundation, Inc.

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

import javax.swing.JComponent;
import javax.swing.JPanel;
import javax.swing.LookAndFeel;
import javax.swing.plaf.ComponentUI;
import javax.swing.plaf.PanelUI;

/**
 * A UI delegate for the {@link JPanel} component.
 */
public class BasicPanelUI extends PanelUI
{
  /**
   * A UI delegate that can be shared by all panels (because the delegate is
   * stateless).
   */
  static BasicPanelUI sharedUI;
  
  /**
   * Returns a UI delegate for the specified component.
   * 
   * @param panel  the panel.
   */
  public static ComponentUI createUI(JComponent panel) 
  {
    if (sharedUI == null)
      sharedUI = new BasicPanelUI();
    return sharedUI;
  }

  /**
   * Installs this UI delegate in the specified component.
   * 
   * @param c  the component (should be a {@link JPanel}, <code>null</code> not
   *     permitted).
   */
  public void installUI(JComponent c)
  {
    super.installUI(c);
    if (c instanceof JPanel)
      {
        JPanel p = (JPanel) c;
        installDefaults(p);
      }
  }

  /**
   * Installs the defaults for this UI delegate in the specified panel.
   * 
   * @param p  the panel (<code>null</code> not permitted).
   */
  protected void installDefaults(JPanel p)
  {
    LookAndFeel.installColorsAndFont(p, "Panel.background", "Panel.foreground",
                                     "Panel.font");
    
    // A test against the reference implementation shows that this method will
    // install a border if one is defined in the UIDefaults table (even though
    // the BasicLookAndFeel doesn't actually define a "Panel.border").  This
    // test was written after discovering that a null argument to 
    // uninstallDefaults throws a NullPointerException in 
    // LookAndFeel.uninstallBorder()...
    LookAndFeel.installBorder(p, "Panel.border");
  }

  /**
   * Uninstalls this UI delegate from the specified component.
   *
   * @param c the component (<code>null</code> not permitted).
   */
  public void uninstallUI(JComponent c)
  {
    uninstallDefaults((JPanel) c);
  }

  /**
   * Uninstalls the UI defaults for the specified panel.
   *
   * @param p  the panel (<code>null</code> not permitted).
   */
  protected void uninstallDefaults(JPanel p)
  {
    // Tests on the reference implementation showed this method:
    // (1) doesn't actually remove the installed colors and font installed
    //     by installDefaults(), it isn't necessary;
    // (2) throws a NullPointerException in LookAndFeel.uninstallBorder() if
    //     p is null.  Strangely, no border is installed by the 
    //     BasicLookAndFeel - perhaps this is needed by another LAF?
    
    LookAndFeel.uninstallBorder(p);
  }
  
}
