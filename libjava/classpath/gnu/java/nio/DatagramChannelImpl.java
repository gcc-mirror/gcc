/* DatagramChannelImpl.java --
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


package gnu.java.nio;

import gnu.java.net.PlainDatagramSocketImpl;
import java.io.IOException;
import java.net.DatagramSocket;
import java.net.InetSocketAddress;
import java.net.SocketAddress;
import java.nio.ByteBuffer;
import java.nio.channels.ClosedChannelException;
import java.nio.channels.DatagramChannel;
import java.nio.channels.NotYetConnectedException;
import java.nio.channels.spi.SelectorProvider;

/**
 * @author Michael Koch
 */
public final class DatagramChannelImpl extends DatagramChannel
  implements VMChannelOwner
{
  private NIODatagramSocket socket;
  private VMChannel channel;

  /**
   * Indicates whether this channel initiated whatever operation
   * is being invoked on our datagram socket.
   */
  private boolean inChannelOperation;

  protected DatagramChannelImpl (SelectorProvider provider)
    throws IOException
  {
    super (provider);
    socket = new NIODatagramSocket (new PlainDatagramSocketImpl(), this);
    channel = new VMChannel();
    channel.initSocket(false);
    configureBlocking(true);
  }

  /**
   * Indicates whether our datagram socket should ignore whether
   * we are set to non-blocking mode. Certain operations on our
   * socket throw an <code>IllegalBlockingModeException</code> if
   * we are in non-blocking mode, <i>except</i> if the operation
   * is initiated by us.
   */
  public final boolean isInChannelOperation()
  {
    return inChannelOperation;
  }

  /**
   * Sets our indicator of whether we are initiating an I/O operation
   * on our socket.
   */
  public final void setInChannelOperation(boolean b)
  {
    inChannelOperation = b;
  }

  public DatagramSocket socket ()
  {
    return socket;
  }

  protected void implCloseSelectableChannel ()
    throws IOException
  {
    channel.close();
  }

  protected void implConfigureBlocking (boolean blocking)
    throws IOException
  {
    channel.setBlocking(blocking);
  }

  public DatagramChannel connect (SocketAddress remote)
    throws IOException
  {
    if (!isOpen())
      throw new ClosedChannelException();

    try
      {
        channel.connect((InetSocketAddress) remote, 0);
      }
    catch (ClassCastException cce)
      {
        throw new IOException("unsupported socked address type");
      }
    return this;
  }

  public DatagramChannel disconnect ()
    throws IOException
  {
    channel.disconnect();
    return this;
  }

  public boolean isConnected()
  {
    try
      {
        return channel.getPeerAddress() != null;
      }
    catch (IOException ioe)
      {
        return false;
      }
  }

  public int write (ByteBuffer src)
    throws IOException
  {
    if (!isConnected ())
      throw new NotYetConnectedException ();

    return channel.write(src);
  }

  public long write (ByteBuffer[] srcs, int offset, int length)
    throws IOException
  {
    if (!isConnected())
      throw new NotYetConnectedException();

    if ((offset < 0)
        || (offset > srcs.length)
        || (length < 0)
        || (length > (srcs.length - offset)))
      throw new IndexOutOfBoundsException();

    /* We are connected, meaning we will write these bytes to
     * the host we connected to, so we don't need to explicitly
     * give the host. */
    return channel.writeGathering(srcs, offset, length);
  }

  public int read (ByteBuffer dst)
    throws IOException
  {
    if (!isConnected ())
      throw new NotYetConnectedException ();

    return channel.read(dst);
  }

  public long read (ByteBuffer[] dsts, int offset, int length)
    throws IOException
  {
    if (!isConnected())
      throw new NotYetConnectedException();

    if ((offset < 0)
        || (offset > dsts.length)
        || (length < 0)
        || (length > (dsts.length - offset)))
      throw new IndexOutOfBoundsException();

    /* Likewise, see the comment int write above. */
    return channel.readScattering(dsts, offset, length);
  }

  public SocketAddress receive (ByteBuffer dst)
    throws IOException
  {
    if (!isOpen())
      throw new ClosedChannelException();

    try
      {
        begin();
        return channel.receive(dst);
      }
    finally
      {
        end(true);
      }
  }

  public int send (ByteBuffer src, SocketAddress target)
    throws IOException
  {
    if (!isOpen())
      throw new ClosedChannelException();

    if (!(target instanceof InetSocketAddress))
      throw new IOException("can only send to inet socket addresses");

    InetSocketAddress dst = (InetSocketAddress) target;
    if (dst.isUnresolved())
      throw new IOException("Target address not resolved");

    return channel.send(src, dst);
  }

  public VMChannel getVMChannel()
  {
    return channel;
  }
}
