/* AbstractSelectableChannel.java
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

package java.nio.channels.spi;

import java.io.IOException;
import java.nio.channels.ClosedChannelException;
import java.nio.channels.SelectableChannel;
import java.nio.channels.SelectionKey;
import java.nio.channels.Selector;
import java.util.LinkedList;
import java.util.List;
import java.util.ListIterator;

public abstract class AbstractSelectableChannel extends SelectableChannel
{
  int registered;
  boolean blocking = true;
  Object LOCK = new Object ();
  SelectorProvider provider;
  List keys;

  /**
   * Initializes the channel
   */
  protected AbstractSelectableChannel (SelectorProvider provider)
  {
    this.provider = provider;
  }

  /**
   * Retrieves the object upon which the configureBlocking and register
   * methods synchronize.
   */
  public final Object blockingLock ()
  {
    return LOCK;
  }
    
  /**
   * Adjusts this channel's blocking mode.
   */
  public final SelectableChannel configureBlocking (boolean block)
    throws IOException
  {
    synchronized (LOCK)
      {
        blocking = true;
        implConfigureBlocking (block);
      }
    
    return this;
  }

  /**
   * Closes this channel.
   *
   * @exception IOException If an error occurs
   */
  protected final void implCloseChannel () throws IOException
  {
    implCloseSelectableChannel ();
  }

  /**
   * Closes this selectable channel.
   */
  protected abstract void implCloseSelectableChannel () throws IOException;
  
  /**
   * Adjusts this channel's blocking mode.
   */
  protected abstract void implConfigureBlocking (boolean block)
    throws IOException;

  /**
   * Tells whether or not every I/O operation on this channel will block
   * until it completes.
   */
  public final boolean isBlocking()
  {
    return blocking;
  }

  /**
   * Tells whether or not this channel is currently registered with
   * any selectors.
   */
  public final boolean isRegistered()
  {
    return registered > 0;
  }

  /**
   * Retrieves the key representing the channel's registration with the
   * given selector.
   */
  public final SelectionKey keyFor(Selector selector)
  {
    try
      {
        return register (selector, 0, null);
      }
    catch (Exception e)
      {
        return null;
      }
  }

  /**
   * Returns the provider that created this channel.
   */
  public final SelectorProvider provider ()
  {
    return provider;
  }

  private SelectionKey locate (Selector selector)
  {
    if (keys == null)
      return null;
    
    SelectionKey k = null;
    ListIterator it = keys.listIterator ();
    
    while (it.hasNext ())
      {
    	k = (SelectionKey) it.next ();
    	if (k.selector () == selector)
          {
            return k;
          }
      }
    
    return k;
  }

  private void add (SelectionKey key)
  {
    if (keys == null)
      {
        keys = new LinkedList ();
      }
    
    keys.add (key);
  }

  /**
   * Registers this channel with the given selector, returning a selection key.
   *
   * @exception ClosedChannelException If the channel is already closed.
   */
  public final SelectionKey register (Selector selin, int ops, Object att)
    throws ClosedChannelException
  {
    if (!isOpen ())
      throw new ClosedChannelException();

    SelectionKey k = null;
    AbstractSelector selector = (AbstractSelector) selin;

    synchronized (LOCK)
      {
        k = locate (selector);

        if (k != null)
          {
            k.attach (att);
          }
        else
          {
            k = selector.register (this, ops, att);
    		
            if (k != null)
              add (k);
          }
      }

    return k;
  }
}
