/* SocketChannelImpl.java -- 
   Copyright (C) 2002, 2003 Free Software Foundation, Inc.

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


package gnu.java.nio;

import java.io.InputStream;
import java.io.IOException;
import java.io.OutputStream;
import java.net.InetAddress;
import java.net.InetSocketAddress;
import gnu.java.net.PlainSocketImpl;
import java.net.Socket;
import java.net.SocketAddress;
import java.net.SocketTimeoutException;
import java.nio.ByteBuffer;
import java.nio.channels.AlreadyConnectedException;
import java.nio.channels.ClosedChannelException;
import java.nio.channels.ConnectionPendingException;
import java.nio.channels.NoConnectionPendingException;
import java.nio.channels.NotYetConnectedException;
import java.nio.channels.UnresolvedAddressException;
import java.nio.channels.UnsupportedAddressTypeException;
import java.nio.channels.SocketChannel;
import java.nio.channels.Selector;
import java.nio.channels.SelectionKey;
import java.nio.channels.spi.SelectorProvider;
import gnu.classpath.Configuration;

public final class SocketChannelImpl extends SocketChannel
{
  private NIOSocket socket;
  private boolean blocking = true;
  private boolean connected = false;
  private boolean connectionPending = false;

  SocketChannelImpl (SelectorProvider provider)
    throws IOException
  {
    super (provider);
    socket = new NIOSocket (new PlainSocketImpl(), this);
  }
  
  SocketChannelImpl (SelectorProvider provider,
                     NIOSocket socket)
    throws IOException
  {
    super (provider);
    this.socket = socket;
    this.connected = socket.isConnected();
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

  protected void implCloseSelectableChannel () throws IOException
  {
    connected = false;
    socket.close();
  }

  protected void implConfigureBlocking (boolean blocking) throws IOException
  {
    socket.setSoTimeout (blocking ? 0 : NIOConstants.DEFAULT_TIMEOUT);
    this.blocking = blocking;
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
    
    if (blocking)
      {
        // Do blocking connect.
        socket.connect (remote);
        connected = true;
        return true;
      }

    // Do non-blocking connect.
    try
      {
        socket.connect (remote, NIOConstants.DEFAULT_TIMEOUT);
        connected = true;
        return true;
      }
    catch (SocketTimeoutException e)
      {
        connectionPending = true;
        return false;
      }
  }
    
  public boolean finishConnect ()
    throws IOException
  {
    if (!isOpen())
      throw new ClosedChannelException();
    
    if (!connectionPending)
      throw new NoConnectionPendingException();
    
    if (isConnected())
      return true;

    // FIXME: Handle blocking/non-blocking mode.

    Selector selector = provider().openSelector();
    register (selector, SelectionKey.OP_CONNECT);

    if (isBlocking())
      {
        selector.select(); // blocking until channel is connected.
        connected = true;
        connectionPending = false;
        return true;
      }

    int ready = selector.selectNow(); // non-blocking
    if (ready == 1)
      {
        connected = true;
        connectionPending = false;
        return true;
      }

    return false;
  }

  public boolean isConnected ()
  {
    return connected;
  }
    
  public boolean isConnectionPending ()
  {
    return connectionPending;
  }
    
  public Socket socket ()
  {
    return socket;
  }

  public int read (ByteBuffer dst) throws IOException
  {
    if (!connected)
      throw new NotYetConnectedException();
    
    byte[] data;
    int offset = 0;
    int len = dst.remaining();
	
    if (dst.hasArray())
      {
        offset = dst.arrayOffset() + dst.position();
        data = dst.array();
      }
    else
      {
        data = new byte [len];
      }

    InputStream input = socket.getInputStream();
    int available = input.available();

    if (available == 0)
      return 0;
    
    if (len > available)
      len = available;

    int readBytes = 0;
    boolean completed = false;

    try
      {
        begin();
        readBytes = input.read (data, offset, len);
        completed = true;
      }
    finally
      {
        end (completed);
      }

    if (readBytes > 0
        && !dst.hasArray())
      {
        dst.put (data);
      }

    return readBytes;
  }
    
  public long read (ByteBuffer[] dsts, int offset, int length)
    throws IOException
  {
    if (!connected)
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
    if (!connected)
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

    System.out.println ("INTERNAL: writing to socket outputstream");
    
    OutputStream output = socket.getOutputStream();
    output.write (data, offset, len);
    return len;
  }

  public long write (ByteBuffer[] srcs, int offset, int length)
    throws IOException
  {
    if (!connected)
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
