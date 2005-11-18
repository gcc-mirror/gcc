/* MetalMenuBarUI.java -- MenuBar UI for the Metal L&F
   Copyright (C) 2005 Free Software Foundation, Inc.

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


package javax.swing.plaf.metal;

import java.awt.Graphics;

import javax.swing.JComponent;
import javax.swing.SwingConstants;
import javax.swing.UIManager;
import javax.swing.plaf.ComponentUI;
import javax.swing.plaf.basic.BasicMenuBarUI;

/**
 * A UI implementation for MenuBar in the Metal Look &amp; Feel.
 * 
 * @author Roman Kennke (kennke@aicas.com)
 *
 * @since 1.5
 */
public class MetalMenuBarUI extends BasicMenuBarUI
{
  /**
   * Creates and returns a new instance of this UI for the specified component.
   *
   * @param c the component to create a UI for
   *
   * @return the UI for the component
   */
  public static ComponentUI createUI(JComponent c)
  {
    return new MetalMenuBarUI();
  }


  /**
   * If the property <code>MenuBar.gradient</code> is set, then a gradient
   * is painted as background, otherwise the normal superclass behaviour is
   * called.
   */
  public void update(Graphics g, JComponent c)
  {
    if (c.isOpaque() && UIManager.get("MenuBar.gradient") != null)
      {
        MetalUtils.paintGradient(g, 0, 0, c.getWidth(), c.getHeight(),
                                 SwingConstants.VERTICAL, "MenuBar.gradient");
        paint(g, c);
      }
    else
      super.update(g, c);
  }

}
