/* SocketChannelImpl.java -- 
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


package gnu.java.nio;

import java.io.IOException;
import java.net.InetAddress;
import java.net.InetSocketAddress;
import java.net.Socket;
import java.net.SocketAddress;
import java.nio.ByteBuffer;
import java.nio.channels.AlreadyConnectedException;
import java.nio.channels.SocketChannel;
import java.nio.channels.spi.SelectorProvider;
import gnu.classpath.Configuration;

public class SocketChannelImpl extends SocketChannel
{
  Socket socket;
  boolean blocking = true;
  boolean connected = false;

  public SocketChannelImpl (SelectorProvider provider)		      
  {
    super (provider);
    socket = new Socket ();
  }

  public void finalizer()
  {
    if (connected)
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
    this.blocking = blocking; // FIXME
  }   

  public boolean connect (SocketAddress remote) throws IOException
  {
    if (connected)
      throw new AlreadyConnectedException();
	
    socket.connect (remote, 50);
    connected = true;
    return blocking; // FIXME
  }
    
  public boolean finishConnect ()
  {
    return false;
  }

  public boolean isConnected ()
  {
    return connected;
  }
    
  public boolean isConnectionPending ()
  {
    return blocking ? true : false;
  }
    
  public Socket socket ()
  {
    return socket;
  }

  public int read (ByteBuffer dst) throws IOException
  {
    int bytes = 0;
    int len = 1024;
    byte[]b = new byte[len];
	
    /*
    bytes = SocketRead(fd, b, 0, len);
    dst.put(b, 0, bytes);

    if (bytes == 0)
      {
        // we've hit eof ?
        return -1;
      }
    */

    return bytes;
  }
    
  public long read (ByteBuffer[] dsts, int offset, int length)
    throws IOException
  {
    long bytes = 0;

    for (int i = offset; i < length; i++)
      {
        bytes += read (dsts [i]);
      }

    return bytes;
  }
     
  public int write (ByteBuffer src)
    throws IOException
  {
    int bytes = 0;
    int len = src.position();

    /*
    if (src.hasArray ())
      {
        byte[] b = src.array ();
        bytes = SocketWrite (fd, b, 0, len);
      }
    else
      {
        byte[] b = new byte [len];
        src.get (b, 0, len);
        bytes = SocketWrite (fd, b, 0, len);
      }
    */
		
    return bytes;
  }

  public long write (ByteBuffer[] srcs, int offset, int length)
    throws IOException
  {
    long bytes = 0;

    for (int i = offset; i < length; i++)
      {
        bytes += write (srcs [i]);
      }

    return bytes;
  }
}
