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

import gnu.java.net.PlainDatagramSocketImpl;
import java.io.IOException;
import java.net.DatagramPacket;
import java.net.DatagramSocket;
import java.net.InetSocketAddress;
import java.net.SocketAddress;
import java.net.SocketTimeoutException;
import java.nio.ByteBuffer;
import java.nio.channels.ClosedChannelException;
import java.nio.channels.DatagramChannel;
import java.nio.channels.NotYetConnectedException;
import java.nio.channels.spi.SelectorProvider;

/**
 * @author Michael Koch
 */
public final class DatagramChannelImpl extends DatagramChannel
{
  private NIODatagramSocket socket;
  
  /**
   * Indicates whether this channel initiated whatever operation
   * is being invoked on our datagram socket.
   */
  private boolean inChannelOperation;

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
 
  protected DatagramChannelImpl (SelectorProvider provider)
    throws IOException
  {
    super (provider);
    socket = new NIODatagramSocket (new PlainDatagramSocketImpl(), this);
    configureBlocking(true);
  }

  public int getNativeFD()
  {
    return socket.getPlainDatagramSocketImpl().getNativeFD();
  }
    
  public DatagramSocket socket ()
  {
    return socket;
  }
    
  protected void implCloseSelectableChannel ()
    throws IOException
  {
    socket.close ();
  }
    
  protected void implConfigureBlocking (boolean blocking)
    throws IOException
  {
    socket.setSoTimeout (blocking ? 0 : NIOConstants.DEFAULT_TIMEOUT);
  }

  public DatagramChannel connect (SocketAddress remote)
    throws IOException
  {
    if (!isOpen())
      throw new ClosedChannelException();
    
    socket.connect (remote);
    return this;
  }
    
  public DatagramChannel disconnect ()
    throws IOException
  {
    socket.disconnect ();
    return this;
  }
    
  public boolean isConnected ()
  {
    return socket.isConnected ();
  }
    
  public int write (ByteBuffer src)
    throws IOException
  {
    if (!isConnected ())
      throw new NotYetConnectedException ();
    
    return send (src, socket.getRemoteSocketAddress());
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
      
    long result = 0;

    for (int index = offset; index < offset + length; index++)
      result += write (srcs [index]);

    return result;
  }

  public int read (ByteBuffer dst)
    throws IOException
  {
    if (!isConnected ())
      throw new NotYetConnectedException ();
    
    int remaining = dst.remaining();
    receive (dst);
    return remaining - dst.remaining();
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
      
    long result = 0;

    for (int index = offset; index < offset + length; index++)
      result += read (dsts [index]);

    return result;
  }
    
  public SocketAddress receive (ByteBuffer dst)
    throws IOException
  {
    if (!isOpen())
      throw new ClosedChannelException();
    
    try
      {
        DatagramPacket packet;
        int len = dst.capacity() - dst.position();
        
        if (dst.hasArray())
          {
            packet = new DatagramPacket (dst.array(),
                                         dst.arrayOffset() + dst.position(),
                                         len);
          }
        else
          {
            packet = new DatagramPacket (new byte [len], len);
          }

        boolean completed = false;

        try
          {
            begin();
            setInChannelOperation(true);
            socket.receive (packet);
            completed = true;
          }
        finally
          {
            end (completed);
            setInChannelOperation(false);
          }

        if (!dst.hasArray())
          {
            dst.put (packet.getData(), packet.getOffset(), packet.getLength());
          }
        else
          {
            dst.position (dst.position() + packet.getLength());
          }

        return packet.getSocketAddress();
      }
    catch (SocketTimeoutException e)
      {
        return null;
      }
  }
    
  public int send (ByteBuffer src, SocketAddress target)
    throws IOException
  {
    if (!isOpen())
      throw new ClosedChannelException();
    
    if (target instanceof InetSocketAddress
	&& ((InetSocketAddress) target).isUnresolved())
      throw new IOException("Target address not resolved");

    byte[] buffer;
    int offset = 0;
    int len = src.remaining();
    
    if (src.hasArray())
      {
        buffer = src.array();
        offset = src.arrayOffset() + src.position();
      }
    else
      {
        buffer = new byte [len];
        src.get (buffer);
      }

    DatagramPacket packet = new DatagramPacket (buffer, offset, len, target);

    boolean completed = false;
    try
      {
        begin();
        setInChannelOperation(true);
        socket.send(packet);
        completed = true;
      }
    finally
      {
        end (completed);
        setInChannelOperation(false);
      }
      
    if (src.hasArray())
      {
	src.position (src.position() + len);
      }

    return len;
  }
}
