/* Selector.java --
   Copyright (C) 2002 Free Software Foundation, Inc.

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

package java.nio.channels;

import java.io.IOException;
import java.nio.channels.spi.SelectorProvider;
import java.util.Set;


/**
 * @author Michael Koch
 * @since 1.4
 */
public abstract class Selector
{
  /**
   * Initializes the selector.
   */
  protected Selector()
  {
  }

  /**
   * Opens a selector.
   *
   * @exception IOException If an error occurs
   */
  public static Selector open() throws IOException
  {
    return SelectorProvider.provider().openSelector();
  }

  /**
   * Closes the selector.
   *
   * @exception IOException If an error occurs
   */
  public abstract void close() throws IOException;

  /**
   * Tells whether the selector is open or not.
   */
  public abstract boolean isOpen();

  /**
   * Returns this selector's key set.
   *
   * @exception ClosedSelectorException If this selector is closed.
   */
  public abstract Set<SelectionKey> keys();

  /**
   * Returns the SelectorProvider that created the selector.
   */
  public abstract SelectorProvider provider();

  /**
   * Selects a set of keys whose corresponding channels are ready
   * for I/O operations.
   *
   * @exception ClosedSelectorException If this selector is closed.
   * @exception IOException If an error occurs
   */
  public abstract int select() throws IOException;

  /**
   * Selects a set of keys whose corresponding channels are ready
   * for I/O operations.
   *
   * @param timeout The timeout to use.
   *
   * @exception ClosedSelectorException If this selector is closed.
   * @exception IllegalArgumentException If the timeout value is negative.
   * @exception IOException If an error occurs
   */
  public abstract int select(long timeout) throws IOException;

  /**
   * Returns this selector's selected-key set.
   *
   * @exception ClosedSelectorException If this selector is closed.
   */
  public abstract Set<SelectionKey> selectedKeys();

  /**
   * Selects a set of keys whose corresponding channels are ready
   * for I/O operations.
   *
   * @exception ClosedSelectorException If this selector is closed.
   * @exception IOException If an error occurs
   */
  public abstract int selectNow() throws IOException;

  /**
   * Causes the first selection operation that has not yet returned to
   * return immediately.
   */
  public abstract Selector wakeup();
}
