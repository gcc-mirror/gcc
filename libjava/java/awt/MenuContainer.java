/* MenuContainer.java -- container for menu items
   Copyright (C) 1999, 2002 Free Software Foundation, Inc.

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


package java.awt;

/**
 * This interface is a container for menu components.
 *
 * @author Aaron M. Renn <arenn@urbanophile.com>
 * @since 1.0
 * @status updated to 1.4
 */
public interface MenuContainer
{
  /**
   * Returns the font in use by this container.
   *
   * @return the menu font
   */
  Font getFont();

  /**
   * Removes the specified menu component from the menu.
   *
   * @param component the menu component to remove
   */
  void remove(MenuComponent component);

  /**
   * Posts an event to the listeners.
   *
   * @param event the event to dispatch
   * @deprecated use {@link MenuComponent#dispatchEvent(AWTEvent)} instead
   */
  boolean postEvent(Event event);
} // interface MenuContainer
