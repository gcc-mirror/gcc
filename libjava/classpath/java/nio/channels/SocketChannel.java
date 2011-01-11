/* SocketChannel.java --
   Copyright (C) 2002, 2004  Free Software Foundation, Inc.

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
import java.net.Socket;
import java.net.SocketAddress;
import java.nio.ByteBuffer;
import java.nio.channels.spi.AbstractSelectableChannel;
import java.nio.channels.spi.SelectorProvider;

/**
 * @author Michael Koch (konqueror@gmx.de)
 * @since 1.4
 */
public abstract class SocketChannel extends AbstractSelectableChannel
  implements ByteChannel, ScatteringByteChannel, GatheringByteChannel
{
  /**
   * Initializes this socket channel.
   */
  protected SocketChannel(SelectorProvider provider)
  {
    super(provider);
  }

  /**
   * Opens a socket channel.
   *
   * @return the new <code>SocketChannel</code> object
   *
   * @exception IOException If an error occurs
   */
  public static SocketChannel open() throws IOException
  {
    return SelectorProvider.provider().openSocketChannel();
  }

  /**
   * Opens a channel and connects it to a remote address.
   *
   * @return the new <code>SocketChannel</code> object
   *
   * @exception AsynchronousCloseException If this channel is already connected.
   * @exception ClosedByInterruptException If another thread interrupts the
   * current thread while the connect operation is in progress, thereby closing
   * the channel and setting the current thread's interrupt status.
   * @exception IOException If an error occurs
   * @exception SecurityException If a security manager has been installed and
   * it does not permit access to the given remote endpoint.
   * @exception UnresolvedAddressException If the given remote address is not
   * fully resolved.
   * @exception UnsupportedAddressTypeException If the type of the given remote
   * address is not supported.
   */
  public static SocketChannel open(SocketAddress remote)
    throws IOException
  {
    SocketChannel ch = open();
    ch.connect(remote);
    return ch;
  }

  /**
   * Reads data from the channel.
   *
   * @return the number of bytes read, zero is valid too, -1 if end of stream
   * is reached
   *
   * @exception IOException If an error occurs
   * @exception NotYetConnectedException If this channel is not yet connected.
   */
  public final long read(ByteBuffer[] dsts) throws IOException
  {
    long b = 0;

    for (int i = 0; i < dsts.length; i++)
      b += read(dsts[i]);

    return b;
  }

  /**
   * Writes data to the channel.
   *
   * @return the number of bytes written, zero is valid too
   *
   * @exception IOException If an error occurs
   * @exception NotYetConnectedException If this channel is not yet connected.
   */
  public final long write(ByteBuffer[] dsts) throws IOException
  {
    long b = 0;

    for (int i = 0; i < dsts.length; i++)
      b += write(dsts[i]);

    return b;
  }

  /**
   * Retrieves the valid operations for this channel.
   *
   * @return the valid operations
   */
  public final int validOps()
  {
    return SelectionKey.OP_CONNECT | SelectionKey.OP_READ
           | SelectionKey.OP_WRITE;
  }

  /**
   * Reads data from the channel.
   *
   * @return the number of bytes read, zero is valid too, -1 if end of stream
   * is reached
   *
   * @exception IOException If an error occurs
   * @exception NotYetConnectedException If this channel is not yet connected.
   */
  public abstract int read(ByteBuffer dst) throws IOException;

  /**
   * Connects the channel's socket to the remote address.
   *
   * @return <code>true</code> if the channel got successfully connected,
   * <code>false</code> if the channel is in non-blocking mode and connection
   * operation is still in progress.
   *
   * @exception AlreadyConnectedException If this channel is already connected.
   * @exception AsynchronousCloseException If this channel is already connected.
   * @exception ClosedByInterruptException If another thread interrupts the
   * current thread while the connect operation is in progress, thereby closing
   * the channel and setting the current thread's interrupt status.
   * @exception ClosedChannelException If this channel is closed.
   * @exception ConnectionPendingException If a non-blocking connection
   * operation is already in progress on this channel.
   * @exception IOException If an error occurs
   * @exception SecurityException If a security manager has been installed and
   * it does not permit access to the given remote endpoint.
   * @exception UnresolvedAddressException If the given remote address is not
   * fully resolved.
   * @exception UnsupportedAddressTypeException If the type of the given remote
   * address is not supported.
   */
  public abstract boolean connect(SocketAddress remote)
    throws IOException;

  /**
   * Finishes the process of connecting a socket channel.
   *
   * @exception AsynchronousCloseException If this channel is already connected.
   * @exception ClosedByInterruptException If another thread interrupts the
   * current thread while the connect operation is in progress, thereby closing
   * the channel and setting the current thread's interrupt status.
   * @exception ClosedChannelException If this channel is closed.
   * @exception IOException If an error occurs
   * @exception NoConnectionPendingException If this channel is not connected
   * and a connection operation has not been initiated.
   */
  public abstract boolean finishConnect() throws IOException;

  /**
   * Tells whether or not the channel's socket is connected.
   */
  public abstract boolean isConnected();

  /**
   * Tells whether or not a connection operation is in progress on this channel.
   */
  public abstract boolean isConnectionPending();

  /**
   * Reads data from the channel.
   *
   * @return the number of bytes read, zero is valid too, -1 if end of stream
   * is reached
   *
   * @exception IOException If an error occurs
   * @exception NotYetConnectedException If this channel is not yet connected.
   */
  public abstract long read(ByteBuffer[] dsts, int offset, int length)
    throws IOException;

  /**
   * Retrieves the channel's socket.
   *
   * @return the socket
   */
  public abstract Socket socket();

  /**
   * Writes data to the channel.
   *
   * @return the number of bytes written, zero is valid too
   *
   * @exception IOException If an error occurs
   * @exception NotYetConnectedException If this channel is not yet connected.
   */
  public abstract int write(ByteBuffer src) throws IOException;

  /**
   * Writes data to the channel.
   *
   * @return the number of bytes written, zero is valid too
   *
   * @exception IOException If an error occurs
   * @exception NotYetConnectedException If this channel is not yet connected.
   */
  public abstract long write(ByteBuffer[] srcs, int offset, int length)
    throws IOException;
}
