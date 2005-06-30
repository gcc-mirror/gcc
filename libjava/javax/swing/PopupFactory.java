/* PopupFactory.java --
   Copyright (C) 2003 Free Software Foundation, Inc.

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


package javax.swing;

import java.awt.Component;


/**
 * A factory for <code>Popup</code> objects. These are used to
 * managed little windows that float over everything else,
 * typically containing a popup menu.
 *
 * @since 1.4
 *
 * @author Sascha Brawer (brawer@dandelis.ch)
 */
public class PopupFactory
{
  /**
   * The shared factory object.
   *
   * @see #getSharedFactory
   * @see #setSharedFactory
   */
  private static PopupFactory sharedFactory;


  /**
   * Constructs a new <code>PopupFactory</code>. Usually, a single
   * <code>PopupFactory</code> is shared among multiple consumers
   * of <code>Popup</code>. Use {@link #getSharedInstance} to retrieve
   * the current factory.
   */
  public PopupFactory()
  {
  }


  /**
   * Sets the shared factory.
   *
   * @param factory the PopupFactory that future invocations of
   *        {@link #getSharedInstance} will return.
   *
   * @throws IllegalArgumentException if <code>factory</code>
   *         is <code>null</code>.
   */
  public static void setSharedInstance(PopupFactory factory)
  {
    if (factory == null)
      throw new IllegalArgumentException();

    /* Swing is not designed to be thread-safe, so there is no
     * need to synchronize the access to the global variable.
     */
    sharedFactory = factory;
  }


  /**
   * Retrieves the shared factory, creating a new factory if
   * necessary.
   *
   * @return a <code>PopupFactory</code> that can be used
   *         to create <code>Popup</code> objects.
   */
  public static PopupFactory getSharedInstance()
  {
    /* Swing is not designed to be thread-safe, so there is no
     * need to synchronize the access to the global variable.
     */
    if (sharedFactory == null)
      sharedFactory = new PopupFactory();

    return sharedFactory;
  }


  /**
   * Creates a new <code>Popup</code> given its owner,
   * contents and the screen position where the popup
   * will appear.
   *
   * @param owner the Component to which <code>x</code> and
   *        <code>y</code> are relative, or <code>null</code> for
   *        placing the popup relative to the origin of the screen.
   *
   * @param contents the contents that will be displayed inside
   *        the <code>Popup</code>.
   *
   * @param x the horizontal position where the Popup will appear.
   *
   * @param y the vertical position where the Popup will appear.
   *
   * @throws IllegalArgumentException if <code>contents</code>
   *         is <code>null</code>.
   */
  public Popup getPopup(Component owner, Component contents,
                        int x, int y)
  {
    return new Popup.JWindowPopup(owner, contents, x, y);
  }
}
