/* DemoDesktop.java -- A custom desktop for the demo
   Copyright (C) 2006 Free Software Foundation, Inc.

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


package gnu.classpath.examples.swing;

import java.awt.Graphics;

import javax.swing.ImageIcon;
import javax.swing.JDesktopPane;

/**
 * A customized Desktop for the GNU Classpath Swing demo that paints the
 * GNU Classpath Icon in the middle of the desktop.
 *
 * @author Roman Kennke (kennke@aicas.com)
 */
public class DemoDesktop
  extends JDesktopPane
{

  /**
   * The icon that's painted centered on the desktop.
   */
  private ImageIcon image;

  /**
   * Creates a new desktop.
   */
  DemoDesktop()
  {
    super();
    String badge = "/gnu/classpath/examples/icons/badge.png";
    image = new ImageIcon(getClass().getResource(badge));
  }

  /**
   * Paints the desktop including the icon.
   *
   * @param g the graphics to use for painting
   */
  protected void paintComponent(Graphics g)
  {
    super.paintComponent(g);
    image.paintIcon(this, g, (getWidth() - image.getIconWidth()) / 2,
                    (getHeight() - image.getIconHeight()) / 2);
  }
}
