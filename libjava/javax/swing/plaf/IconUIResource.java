/* IconUIResource.java --
   Copyright (C) 2002, 2003, 2004  Free Software Foundation, Inc.

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


package javax.swing.plaf;

import java.awt.Component;
import java.awt.Graphics;
import java.io.Serializable;

import javax.swing.Icon;

/**
 * An icon that is marked as <code>UIResource</code>, which
 * indicates that it has been installed by a pluggable
 * LookAndFeel. Such icons are replaced when the LookAndFeel
 * changes.
 *
 * @author Andrew Selkirk (aselkirk@sympatico.ca)
 * @author Sascha Brawer (brawer@dandelis.ch)
 */
public class IconUIResource
  implements Icon, UIResource, Serializable
{
  /**
   * Verified using the <code>serialver</code> tool of Sun JDK 1.4.1_01
   * on GNU/Linux 2.4.18.
   */
  static final long serialVersionUID = 3327049506004830542L;


  /**
   * The icon that is wrapped by this <code>IconUIResource</code>.
   */
  private Icon delegate;


  /**
   * Constructs a <code>IconUIResource</code> that wraps another
   * icon. All messages are forwarded to the delegate icon.
   *
   * @param delegate the icon that is wrapped by this
   *        <code>IconUIResource</code>.
   */
  public IconUIResource(Icon delegate)
  {
    this.delegate = delegate;
  }


  /**
   * Paints the icon by asking the delegate icon to paint itself.
   *
   * @param c the Component whose icon is being painted. Some icons
   *        use this argument to retrieve properties like the
   *        background color.
   *
   * @param g the graphics into which the icon will be painted.
   *
   * @param x the horizontal position of the icon.
   *
   * @param y the vertical position of the icon.
   */
  public void paintIcon(Component c, Graphics g, int x, int y)
  {
    delegate.paintIcon(c, g, x, y);
  }


  /**
   * Returns the width of the icon in pixels. The implementation
   * determines and returns the width of the delegate icon.
   */
  public int getIconWidth()
  {
    return delegate.getIconWidth();
  }


  /**
   * Returns the height of the icon in pixels. The implementation
   * determines and returns the height of the delegate icon.
   */
  public int getIconHeight()
  {
    return delegate.getIconHeight();
  }
}
