/* SocketChannel.java -- 
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

import java.nio.channels.spi.AbstractSelectableChannel;
import java.nio.channels.spi.SelectorProvider;
import java.nio.ByteBuffer;
import java.io.IOException;
import java.net.Socket;
import java.net.SocketAddress;

/**
 * @author Michael Koch
 * @since 1.4
 */
abstract public class SocketChannel extends AbstractSelectableChannel
{
  /**
   * Initializes this socket.
   */
  protected SocketChannel (SelectorProvider provider)
  {
    super (provider);
  }
 
  /**
   * Opens a socket channel.
   */
  public static SocketChannel open () throws IOException
  {
    return SelectorProvider.provider ().openSocketChannel ();
  }
  
  /**
   * Opens a channel and connects it to a remote address.
   */
  public static SocketChannel open (SocketAddress remote) throws IOException
  {
    SocketChannel ch = open ();
	
    if (ch.connect (remote))
      {
      }
    
    return ch;
  }
    
  /**
   * Reads data from the channel.
   */
  public final long read (ByteBuffer[] dsts)
  {
    long b = 0;
    
    for (int i = 0; i < dsts.length; i++)
      {
        b += read (dsts [i]);
      }
    
    return b;
  }
    
  /**
   * Writes data to the channel.
   */
  public final long write (ByteBuffer[] dsts)
  {
    long b = 0;

    for (int  i= 0; i < dsts.length; i++)
      {
        b += write (dsts [i]);
      }
    
    return b;
  }    
   
  /**
   * Retrieves the valid operations for this channel.
   */
  public final int validOps ()
  {
    return SelectionKey.OP_CONNECT | SelectionKey.OP_READ | SelectionKey.OP_WRITE;
  }

  /**
   * Reads data from the channel.
   */
  public abstract int read (ByteBuffer dst);

  /**
   * Connects the channel's socket to the remote address.
   */
  public abstract boolean connect (SocketAddress remote) throws IOException;
  
  /**
   * Finishes the process of connecting a socket channel.
   */
  public abstract boolean finishConnect ();
 
  /**
   * Tells whether or not the channel's socket is connected.
   */
  public abstract boolean isConnected ();
  
  /**
   * Tells whether or not a connection operation is in progress on this channel.
   */
  public abstract boolean isConnectionPending ();
  
  /**
   * Reads data from the channel.
   */
  public abstract long read (ByteBuffer[] dsts, int offset, int length);
 
  /**
   * Retrieves the channel's socket.
   */
  public abstract Socket socket ();
  
  /**
   * Writes data to the channel.
   */
  public abstract int write (ByteBuffer src);
  
  /**
   * Writes data to the channel.
   */
  public abstract long write (ByteBuffer[] srcs, int offset, int length);
}
