/* SocketChannelImpl.java -- 
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


package gnu.java.nio;

import java.io.IOException;
import java.net.InetSocketAddress;
import java.net.Socket;
import java.net.SocketAddress;
import java.nio.ByteBuffer;
import java.nio.channels.AlreadyConnectedException;
import java.nio.channels.ClosedChannelException;
import java.nio.channels.ConnectionPendingException;
import java.nio.channels.NoConnectionPendingException;
import java.nio.channels.NotYetConnectedException;
import java.nio.channels.SocketChannel;
import java.nio.channels.UnresolvedAddressException;
import java.nio.channels.UnsupportedAddressTypeException;
import java.nio.channels.spi.SelectorProvider;

public final class SocketChannelImpl extends SocketChannel
  implements VMChannelOwner
{
  private VMChannel channel;
  //private PlainSocketImpl impl;
  private NIOSocket socket;
  private boolean connectionPending;
  private boolean connected;
  private InetSocketAddress connectAddress;
  
  public SocketChannelImpl(boolean create) throws IOException
  {
    // XXX consider adding security check; this is used by
    // PlainSocketImpl.
    this(new SelectorProviderImpl(), create);
  }

  public SocketChannelImpl(VMChannel channel) throws IOException
  {
    this(new SelectorProviderImpl(), channel, false);
  }
  
  SocketChannelImpl(SelectorProvider provider) throws IOException
  {
    this(provider, true);
  }

  SocketChannelImpl(SelectorProvider provider, boolean create)
    throws IOException
  {
    this(provider, new VMChannel(), create);
  }
  
  SocketChannelImpl(SelectorProvider provider, VMChannel channel, boolean create)
    throws IOException
  {
    super (provider);
    this.channel = channel;
    if (create)
      channel.initSocket(true);
    socket = new NIOSocket(this);
    configureBlocking(true);
  }
  
  /*SocketChannelImpl (SelectorProvider provider,
                     NIOSocket socket)
    throws IOException
  {
    super (provider);
    this.impl = socket.getPlainSocketImpl();
    this.socket = socket;
  }*/

  public void finalizer()
  {
    if (isConnected())
      {
        try
          {
            close ();
          }
        catch (Exception e)
          {
          }
      }
  }

  //PlainSocketImpl getPlainSocketImpl()
  //{
  //  return null; // XXX
  //}

  protected void implCloseSelectableChannel() throws IOException
  {
    channel.close();
  }

  protected void implConfigureBlocking (boolean blocking) throws IOException
  {
    channel.setBlocking(blocking);
  }   

  public boolean connect (SocketAddress remote) throws IOException
  {
    return connect(remote, 0);
  }
  
  public boolean connect (SocketAddress remote, int timeout) throws IOException
  {
    if (!isOpen())
      throw new ClosedChannelException();
    
    if (isConnected())
      throw new AlreadyConnectedException();

    if (connectionPending)
      throw new ConnectionPendingException();

    if (!(remote instanceof InetSocketAddress))
      throw new UnsupportedAddressTypeException();
    
    connectAddress = (InetSocketAddress) remote;

    if (connectAddress.isUnresolved())
      throw new UnresolvedAddressException();
    
    connected = channel.connect(connectAddress, timeout);
    connectionPending = !connected;
    return connected;
  }

  public boolean finishConnect()
    throws IOException
  {
    if (!isOpen())
      throw new ClosedChannelException();

    InetSocketAddress remote = channel.getPeerAddress();
    if (remote != null)
      {
        connectionPending = false;
        return true;
      }
    
    if (!connectionPending)
      throw new NoConnectionPendingException();
    
    return false;
  }

  public boolean isConnected()
  {
    // Wait until finishConnect is called before transitioning to
    // connected.
    if (connectionPending)
      return false;
    try
      {
        InetSocketAddress remote = channel.getPeerAddress();
        return remote != null;
      }
    catch (IOException ioe)
      {
        ioe.printStackTrace(System.out);
        return false;
      }
  }
    
  public boolean isConnectionPending ()
  {
    return connectionPending;
  }
    
  public Socket socket ()
  {
    return socket;
  }

  public int read(ByteBuffer dst) throws IOException
  {
    if (!isConnected())
      throw new NotYetConnectedException();
    
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
    
    return channel.readScattering(dsts, offset, length);
  }
     
  public int write(ByteBuffer src) throws IOException
  {
    if (!isConnected())
      throw new NotYetConnectedException();

    return channel.write(src);
  }

  public long write(ByteBuffer[] srcs, int offset, int length)
    throws IOException
  {
    if (!isConnected())
      throw new NotYetConnectedException();
    
    if ((offset < 0)
        || (offset > srcs.length)
        || (length < 0)
        || (length > (srcs.length - offset)))
      throw new IndexOutOfBoundsException();

    return channel.writeGathering(srcs, offset, length);
  }
  
  public VMChannel getVMChannel()
  {
    // XXX security check?
    return channel;
  }
}
