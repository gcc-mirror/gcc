/* BasicPopupMenuSeparatorUI.java --
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

import java.awt.Color;
import java.awt.Dimension;
import java.awt.Graphics;
import java.awt.Rectangle;

import javax.swing.JComponent;
import javax.swing.JPopupMenu;
import javax.swing.SwingUtilities;
import javax.swing.plaf.ComponentUI;

/**
 * The Basic Look and Feel UI delegate for JPopupMenu.Separator.
 */
public class BasicPopupMenuSeparatorUI extends BasicSeparatorUI
{
  /**
   * Creates a new BasicPopupMenuSeparatorUI object.
   */
  public BasicPopupMenuSeparatorUI()
  {
    super();
  }

  /**
   * Creates a new UI delegate for the given JComponent.
   *
   * @param c The JComponent to create a delegate for.
   *
   * @return A new BasicPopupMenuSeparatorUI
   */
  public static ComponentUI createUI(JComponent c)
  {
    return new BasicPopupMenuSeparatorUI();
  }

  /**
   * The Popup Menu Separator has two lines. The top line will be
   * painted using highlight color and the bottom using shadow color.
   *
   * @param g The Graphics object to paint with
   * @param c The JComponent to paint.
   */
  public void paint(Graphics g, JComponent c)
  {
    if (! (c instanceof JPopupMenu.Separator))
      return;

    Rectangle r = new Rectangle();
    SwingUtilities.calculateInnerArea(c, r);
    Color saved = g.getColor();

    int midAB = r.width / 2 + r.x;
    int midAD = r.height / 2 + r.y;

    g.setColor(highlight);
    g.drawLine(r.x, midAD, r.x + r.width, midAD);

    g.setColor(shadow);
    g.drawLine(r.x, midAD + 1, r.x + r.width, midAD + 1);
  }

  /**
    * This method returns the preferred size of the
    * JComponent.
    *
    * @param c The JComponent to measure.
    *
    * @return The preferred size.
    */
  public Dimension getPreferredSize(JComponent c)
  {
    return super.getPreferredSize(c);
  }
}
