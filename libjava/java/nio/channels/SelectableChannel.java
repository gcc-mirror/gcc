/* SelectableChannel.java --
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

package java.nio.channels;

import java.io.IOException;
import java.nio.channels.spi.AbstractInterruptibleChannel;
import java.nio.channels.spi.SelectorProvider;


/**
 * @author Michael Koch
 * @since 1.4
 */
public abstract class SelectableChannel extends AbstractInterruptibleChannel
{
  /**
   * Initializes the channel.
   */
  protected SelectableChannel()
  {
  }

  /**
   * Returns the lock of this channel.
   */
  public abstract Object blockingLock();

  /**
   * Adjusts this channel's blocking mode.
   *
   * @exception ClosedChannelException If this channel is closed.
   * @exception IllegalBlockingModeException If block is true and this channel
   * is registered with one or more selectors.
   * @exception IOException If an error occurs.
   */
  public abstract SelectableChannel configureBlocking(boolean block)
    throws IOException;

  /**
   * Tells whether this channel is blocking or not.
   */
  public abstract boolean isBlocking();

  /**
   * Tells whether or not this channel is currently registered with
   * any selectors.
   */
  public abstract boolean isRegistered();

  /**
   * Retrieves the key representing the channel's registration with
   * the given selector.
   */
  public abstract SelectionKey keyFor(Selector sel);

  /**
   * Returns the provider that created this channel.
   */
  public abstract SelectorProvider provider();

  /**
   * Registers this channel with the given selector,
   * returning a selection key.
   *
   * @exception CancelledKeyException If this channel is currently registered
   * with the given selector but the corresponding key has already been cancelled
   * @exception ClosedChannelException If this channel is closed.
   * @exception IllegalArgumentException If a bit in ops does not correspond
   * to an operation that is supported by this channel, that is, if
   * set &amp; ~validOps() != 0.
   * @exception IllegalBlockingModeException If block is true and this channel
   * is registered with one or more selectors.
   * @exception IllegalSelectorException If this channel was not created by
   * the same provider as the given selector.
   */
  public final SelectionKey register(Selector sel, int ops)
    throws ClosedChannelException
  {
    return register(sel, ops, null);
  }

  /**
   * Registers this channel with the given selector,
   * returning a selection key.
   *
   * @exception CancelledKeyException If this channel is currently registered
   * with the given selector but the corresponding key has already been
   * cancelled.
   * @exception ClosedChannelException If this channel is closed.
   * @exception IllegalArgumentException If a bit in ops does not correspond
   * to an operation that is supported by this channel, that is, if
   * set &amp; ~validOps() != 0.
   * @exception IllegalBlockingModeException If block is true and this channel
   * is registered with one or more selectors.
   * @exception IllegalSelectorException If this channel was not created by
   * the same provider as the given selector.
   */
  public abstract SelectionKey register(Selector sel, int ops, Object att)
    throws ClosedChannelException;

  /**
   * Returns a set of valid operations on this channel.
   */
  public abstract int validOps();
}
