/* AbstractSelectableChannel.java
   Copyright (C) 2002, 2003, 2004 Free Software Foundation, Inc.

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


package java.nio.channels.spi;

import java.io.IOException;
import java.nio.channels.ClosedChannelException;
import java.nio.channels.SelectableChannel;
import java.nio.channels.SelectionKey;
import java.nio.channels.Selector;
import java.nio.channels.IllegalBlockingModeException;
import java.util.Iterator;
import java.util.LinkedList;
import java.util.ListIterator;

public abstract class AbstractSelectableChannel extends SelectableChannel
{
  private boolean blocking = true;
  private Object LOCK = new Object();
  private SelectorProvider provider;
  private LinkedList keys = new LinkedList();

  /**
   * Initializes the channel
   *
   * @param provider the provider that created this channel
   */
  protected AbstractSelectableChannel(SelectorProvider provider)
  {
    this.provider = provider;
  }

  /**
   * Retrieves the object upon which the configureBlocking and register
   * methods synchronize.
   *
   * @return the blocking lock
   */
  public final Object blockingLock()
  {
    return LOCK;
  }

  /**
   * Adjusts this channel's blocking mode.
   *
   * @param blocking true if blocking should be enabled, false otherwise
   *
   * @return this channel
   *
   * @exception IOException If an error occurs
   */
  public final SelectableChannel configureBlocking(boolean blocking)
    throws IOException
  {
    synchronized (blockingLock())
      {
	if (this.blocking != blocking)
	  {
	    implConfigureBlocking(blocking);
	    this.blocking = blocking;
	  }
      }

    return this;
  }

  /**
   * Closes this channel.
   *
   * @exception IOException If an error occurs
   */
  protected final void implCloseChannel() throws IOException
  {
    try
      {
        implCloseSelectableChannel();
      }
    finally
      {
        for (Iterator it = keys.iterator(); it.hasNext(); )
          ((SelectionKey) it.next()).cancel();
      }
  }

  /**
   * Closes this selectable channel.
   *
   * @exception IOException If an error occurs
   */
  protected abstract void implCloseSelectableChannel()
    throws IOException;

  /**
   * Adjusts this channel's blocking mode.
   *
   * @param blocking true if blocking should be enabled, false otherwise
   *
   * @exception IOException If an error occurs
   */
  protected abstract void implConfigureBlocking(boolean blocking)
    throws IOException;

  /**
   * Tells whether or not every I/O operation on this channel will block
   * until it completes.
   *
   * @return true of this channel is blocking, false otherwise
   */
  public final boolean isBlocking()
  {
    return blocking;
  }

  /**
   * Tells whether or not this channel is currently registered with
   * any selectors.
   *
   * @return true if this channel is registered, false otherwise
   */
  public final boolean isRegistered()
  {
    return ! keys.isEmpty();
  }

  /**
   * Retrieves the key representing the channel's registration with the
   * given selector.
   *
   * @param selector the selector to get a selection key for
   *
   * @return the selection key this channel is registered with
   */
  public final SelectionKey keyFor(Selector selector)
  {
    if (! isOpen())
      return null;

    try
      {
	synchronized (blockingLock())
	  {
	    return locate(selector);
	  }
      }
    catch (Exception e)
      {
	return null;
      }
  }

  /**
   * Returns the provider that created this channel.
   *
   * @return the selector provider that created this channel
   */
  public final SelectorProvider provider()
  {
    return provider;
  }

  private SelectionKey locate(Selector selector)
  {
    ListIterator it = keys.listIterator();

    while (it.hasNext())
      {
	SelectionKey key = (SelectionKey) it.next();

	if (key.selector() == selector)
	  return key;
      }

    return null;
  }

  /**
   * Registers this channel with the given selector, returning a selection key.
   *
   * @param selin the seletor to use
   * @param ops the interested operations
   * @param att an attachment for the returned selection key
   *
   * @return the registered selection key
   * 
   * @exception ClosedChannelException If the channel is already closed.
   * @exception IllegalBlockingModeException If the channel is configured in
   * blocking mode.
   */
  public final SelectionKey register(Selector selin, int ops, Object att)
    throws ClosedChannelException
  {
    if (! isOpen())
      throw new ClosedChannelException();

    if ((ops & ~validOps()) != 0)
      throw new IllegalArgumentException();
    
    SelectionKey key = null;
    AbstractSelector selector = (AbstractSelector) selin;

    synchronized (blockingLock())
      {
	if (blocking)
	  throw new IllegalBlockingModeException();

	key = locate(selector);

	if (key != null && key.isValid())
	  {
            key.interestOps(ops);
            key.attach(att);
	  }
	else
	  {
	    key = selector.register(this, ops, att);

	    if (key != null)
	      addSelectionKey(key);
	  }
      }

    return key;
  }

  void addSelectionKey(SelectionKey key)
  {
    keys.add(key);
  }

  // This method gets called by AbstractSelector.deregister().
  void removeSelectionKey(SelectionKey key)
  {
    keys.remove(key);
  }
}
