/* SelectionKey.java --
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


/**
 * @author Michael Koch
 * @since 1.4
 */
public abstract class SelectionKey
{
  public static final int OP_ACCEPT = 16;
  public static final int OP_CONNECT = 8;
  public static final int OP_READ = 1;
  public static final int OP_WRITE = 4;
  Object attached;

  /**
   * Initializes the selection key.
   */
  protected SelectionKey()
  {
  }

  /**
   * Attaches obj to the key and returns the old attached object.
   */
  public final Object attach(Object obj)
  {
    Object old = attached;
    attached = obj;
    return old;
  }

  /**
   * Returns the object attached to the key.
   */
  public final Object attachment()
  {
    return attached;
  }

  /**
   * Tests if the channel attached to this key is ready to accept
   * a new socket connection.
   *
   * @exception CancelledKeyException If this key has been cancelled
   */
  public final boolean isAcceptable()
  {
    return (readyOps() & OP_ACCEPT) != 0;
  }

  /**
   * Tests whether this key's channel has either finished,
   * or failed to finish, its socket-connection operation.
   *
   * @exception CancelledKeyException If this key has been cancelled
   */
  public final boolean isConnectable()
  {
    return (readyOps() & OP_CONNECT) != 0;
  }

  /**
   * Tests if the channel attached to the key is readable.
   *
   * @exception CancelledKeyException If this key has been cancelled
   */
  public final boolean isReadable()
  {
    return (readyOps() & OP_READ) != 0;
  }

  /**
   * Tests if the channel attached to the key is writable.
   *
   * @exception CancelledKeyException If this key has been cancelled
   */
  public final boolean isWritable()
  {
    return (readyOps() & OP_WRITE) != 0;
  }

  /**
   * Requests that the registration of this key's channel with
   * its selector be cancelled.
   */
  public abstract void cancel();

  /**
   * return the channel attached to the key.
   */
  public abstract SelectableChannel channel();

  /**
   * Returns the key's interest set.
   *
   * @exception CancelledKeyException If this key has been cancelled
   */
  public abstract int interestOps();

  /**
   * Sets this key's interest set to the given value.
   *
   * @exception CancelledKeyException If this key has been cancelled
   * @exception IllegalArgumentException If a bit in the set does not
   * correspond to an operation that is supported by this key's channel,
   * that is, if set &amp; ~(channel().validOps()) != 0
   */
  public abstract SelectionKey interestOps(int ops);

  /**
   * Tells whether or not this key is valid.
   */
  public abstract boolean isValid();

  /**
   * Retrieves this key's ready-operation set.
   *
   * @exception CancelledKeyException If this key has been cancelled
   */
  public abstract int readyOps();

  /**
   * Returns the selector for which this key was created.
   */
  public abstract Selector selector();
}
