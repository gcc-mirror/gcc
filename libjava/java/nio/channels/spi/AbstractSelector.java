/* AbstractSelector.java --
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

package java.nio.channels.spi;

import java.io.IOException;
import java.nio.channels.ClosedSelectorException;
import java.nio.channels.SelectionKey;
import java.nio.channels.Selector;
import java.util.HashSet;
import java.util.Set;


public abstract class AbstractSelector extends Selector
{
  private boolean closed;
  private SelectorProvider provider;
  private HashSet cancelledKeys;

  /**
   * Initializes the slector.
   *
   * @param provider the provider that created this selector
   */
  protected AbstractSelector(SelectorProvider provider)
  {
    this.provider = provider;
    this.cancelledKeys = new HashSet();
  }

  /**
   * Closes the channel.
   *
   * @exception IOException If an error occurs
   */
  public final synchronized void close() throws IOException
  {
    if (closed)
      return;

    implCloseSelector();
    closed = true;
  }

  /**
   * Tells whether this channel is open or not.
   *
   * @return true if channel is open, false otherwise.
   */
  public final boolean isOpen()
  {
    return ! closed;
  }

  /**
   * Marks the beginning of an I/O operation that might block indefinitely.
   */
  protected final void begin()
  {
  }

  /**
   * Marks the end of an I/O operation that might block indefinitely.
   */
  protected final void end()
  {
  }

  /**
   * Returns the provider for this selector object.
   *
   * @return the SelectorProvider object that created this seletor
   */
  public final SelectorProvider provider()
  {
    return provider;
  }

  /**
   * Returns the cancelled keys set.
   *
   * @return the cancelled keys set
   */
  protected final Set cancelledKeys()
  {
    if (! isOpen())
      throw new ClosedSelectorException();

    return cancelledKeys;
  }

  /**
   * Cancels a selection key.
   */

  // This method is only called by AbstractSelectionKey.cancel().
  final void cancelKey(AbstractSelectionKey key)
  {
    synchronized (cancelledKeys)
      {
	cancelledKeys.add(key);
      }
  }

  /**
   * Closes the channel.
   *
   * @exception IOException if an error occurs
   */
  protected abstract void implCloseSelector() throws IOException;

  /**
   * Registers a channel for the selection process.
   *
   * @param ch the channel register
   * @param ops the interested operations
   * @param att an attachement to the selection key
   *
   * @return the registered selection key
   */
  protected abstract SelectionKey register(AbstractSelectableChannel ch,
                                           int ops, Object att);

  /**
   * Deregisters the given selection key.
   *
   * @param key the key to deregister
   */
  protected final void deregister(AbstractSelectionKey key)
  {
    ((AbstractSelectableChannel) key.channel()).removeSelectionKey(key);
  }
}
