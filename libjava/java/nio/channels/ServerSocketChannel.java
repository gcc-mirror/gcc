/* ServerSocketChannel.java --
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
import java.net.ServerSocket;
import java.nio.channels.spi.AbstractSelectableChannel;
import java.nio.channels.spi.SelectorProvider;


/**
 * @author Michael Koch
 * @since 1.4
 */
public abstract class ServerSocketChannel extends AbstractSelectableChannel
{
  /**
   * Initializes this channel.
   */
  protected ServerSocketChannel(SelectorProvider provider)
  {
    super(provider);
  }

  /**
   * Accepts a connection made to this channel's socket.
   *
   * @exception IOException If an error occurs
   * @exception AsynchronousCloseException If another thread closes this
   * channel while the accept operation is in progress.
   * @exception ClosedByInterruptException If another thread interrupts the
   * current thread while the accept operation is in progress, thereby closing
   * the channel and setting the current thread's interrupt status.
   * @exception ClosedChannelException If the channel is closed.
   * @exception NotYetBoundException If the channel's socket is not yet bound.
   * @exception SecurityException If a security manager has been installed and
   * it does not permit access to the remote endpoint of the new connection.
   */
  public abstract SocketChannel accept() throws IOException;

  /**
   * Retrieves the channels socket.
   */
  public abstract ServerSocket socket();

  /**
   * Opens a server socket channel.
   *
   * @exception IOException If an error occurs
   */
  public static ServerSocketChannel open() throws IOException
  {
    return SelectorProvider.provider().openServerSocketChannel();
  }

  /**
   * Retrieves the valid operations for this channel.
   */
  public final int validOps()
  {
    return SelectionKey.OP_ACCEPT;
  }
}
