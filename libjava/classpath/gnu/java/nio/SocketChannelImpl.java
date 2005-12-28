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

import gnu.java.net.PlainSocketImpl;

import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.net.InetSocketAddress;
import java.net.Socket;
import java.net.SocketAddress;
import java.net.SocketTimeoutException;
import java.nio.ByteBuffer;
import java.nio.channels.AlreadyConnectedException;
import java.nio.channels.ClosedChannelException;
import java.nio.channels.ConnectionPendingException;
import java.nio.channels.NoConnectionPendingException;
import java.nio.channels.NotYetConnectedException;
import java.nio.channels.SelectionKey;
import java.nio.channels.Selector;
import java.nio.channels.SocketChannel;
import java.nio.channels.UnresolvedAddressException;
import java.nio.channels.UnsupportedAddressTypeException;
import java.nio.channels.spi.SelectorProvider;

public final class SocketChannelImpl extends SocketChannel
{
  private PlainSocketImpl impl;
  private NIOSocket socket;
  private boolean connectionPending;

  SocketChannelImpl (SelectorProvider provider)
    throws IOException
  {
    super (provider);
    impl = new PlainSocketImpl();
    socket = new NIOSocket (impl, this);
    configureBlocking(true);
  }
  
  SocketChannelImpl (SelectorProvider provider,
                     NIOSocket socket)
    throws IOException
  {
    super (provider);
    this.impl = socket.getPlainSocketImpl();
    this.socket = socket;
  }

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

  PlainSocketImpl getPlainSocketImpl()
  {
    return impl;
  }

  protected void implCloseSelectableChannel () throws IOException
  {
    socket.close();
  }

  protected void implConfigureBlocking (boolean blocking) throws IOException
  {
    socket.setSoTimeout (blocking ? 0 : NIOConstants.DEFAULT_TIMEOUT);
  }   

  public boolean connect (SocketAddress remote) throws IOException
  {
    if (!isOpen())
      throw new ClosedChannelException();
    
    if (isConnected())
      throw new AlreadyConnectedException();

    if (connectionPending)
      throw new ConnectionPendingException();

    if (!(remote instanceof InetSocketAddress))
      throw new UnsupportedAddressTypeException();

    if (((InetSocketAddress) remote).isUnresolved())
      throw new UnresolvedAddressException();
    
    try
      {
        socket.getPlainSocketImpl().setInChannelOperation(true);
          // indicate that a channel is initiating the accept operation
          // so that the socket ignores the fact that we might be in
          // non-blocking mode.
        
        if (isBlocking())
          {
            // Do blocking connect.
            socket.connect (remote);
            return true;
          }

        // Do non-blocking connect.
        try
          {
            socket.connect (remote, NIOConstants.DEFAULT_TIMEOUT);
            return true;
          }
        catch (SocketTimeoutException e)
          {
            connectionPending = true;
            return false;
          }
      }
    finally
      {
        socket.getPlainSocketImpl().setInChannelOperation(false);
      }
  }
    
  public boolean finishConnect ()
    throws IOException
  {
    if (!isOpen())
      throw new ClosedChannelException();
    
    if (!isConnected() && !connectionPending)
      throw new NoConnectionPendingException();
    
    if (isConnected())
      return true;

    // FIXME: Handle blocking/non-blocking mode.

    Selector selector = provider().openSelector();
    register(selector, SelectionKey.OP_CONNECT);

    if (isBlocking())
      {
        selector.select(); // blocking until channel is connected.
        connectionPending = false;
        return true;
      }

    int ready = selector.selectNow(); // non-blocking
    if (ready == 1)
      {
        connectionPending = false;
        return true;
      }

    return false;
  }

  public boolean isConnected ()
  {
    return socket.isConnected();
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
    
    byte[] data;
    int offset = 0;
    InputStream input = socket.getInputStream();
    int available = input.available();
    int len = dst.remaining();
	
    if ((! isBlocking()) && available == 0)
      return 0;
    
    if (dst.hasArray())
      {
        offset = dst.arrayOffset() + dst.position();
        data = dst.array();
      }
    else
      {
        data = new byte [len];
      }

    int readBytes = 0;
    boolean completed = false;

    try
      {
        begin();
        socket.getPlainSocketImpl().setInChannelOperation(true);
        readBytes = input.read (data, offset, len);
        completed = true;
      }
    finally
      {
        end (completed);
        socket.getPlainSocketImpl().setInChannelOperation(false);
      }

    if (readBytes > 0)
      if (dst.hasArray())
	{
	  dst.position (dst.position() + readBytes);
	}
      else
        {
          dst.put (data, offset, readBytes);
        }

    return readBytes;
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
      
    long readBytes = 0;

    for (int index = offset; index < length; index++)
      readBytes += read (dsts [index]);

    return readBytes;
  }
     
  public int write (ByteBuffer src)
    throws IOException
  {
    if (!isConnected())
      throw new NotYetConnectedException();
    
    byte[] data;
    int offset = 0;
    int len = src.remaining();
    
    if (!src.hasArray())
      {
        data = new byte [len];
        src.get (data, 0, len);
      }
    else
      {
        offset = src.arrayOffset() + src.position();
        data = src.array();
      }

    OutputStream output = socket.getOutputStream();
    boolean completed = false;

    try
      {
        begin();
        socket.getPlainSocketImpl().setInChannelOperation(true);
        output.write (data, offset, len);
        completed = true;
      }
    finally
      {
        end (completed);
        socket.getPlainSocketImpl().setInChannelOperation(false);
      }

    if (src.hasArray())
      {
	src.position (src.position() + len);
      }
    
    return len;
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
      
    long writtenBytes = 0;

    for (int index = offset; index < length; index++)
      writtenBytes += write (srcs [index]);

    return writtenBytes;
  }
}
