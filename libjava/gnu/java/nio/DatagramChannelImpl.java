/* DatagramChannelImpl.java -- 
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
import java.net.DatagramSocket;
import java.net.SocketAddress;
import java.nio.ByteBuffer;
import java.nio.channels.DatagramChannel;
import java.nio.channels.NotYetConnectedException;
import java.nio.channels.spi.SelectorProvider;

public class DatagramChannelImpl extends DatagramChannel
{
  boolean blocking = false;
  DatagramSocket socket;
  
  protected DatagramChannelImpl (SelectorProvider provider)
    throws IOException
  {
    super (provider);
    socket = new DatagramSocket ();
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
    this.blocking = blocking; // FIXME
  }

  public DatagramChannel connect (SocketAddress remote)
    throws IOException
  {
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
    
    throw new Error ("Not implemented");
  }

  public long write (ByteBuffer[] srcs, int offset, int length)
    throws IOException
  {
    // FIXME: Should we throw an exception if offset and/or length
    // have wrong values ?

    long result = 0;

    for (int i = offset; i < offset + length; i++)
      result += write (srcs [i]);

    return result;
  }

  public int read (ByteBuffer dst)
    throws IOException
  {
    if (!isConnected ())
      throw new NotYetConnectedException ();
    
    throw new Error ("Not implemented");
  }
    
  public long read (ByteBuffer[] dsts, int offset, int length)
    throws IOException
  {
    // FIXME: Should we throw an exception if offset and/or length
    // have wrong values ?

    long result = 0;

    for (int i = offset; i < offset + length; i++)
      result += read (dsts [i]);

    return result;
  }
    
  public SocketAddress receive (ByteBuffer dst)
    throws IOException
  {
    throw new Error ("Not implemented");
  }
    
  public int send (ByteBuffer src, SocketAddress target)
    throws IOException
  {
    throw new Error ("Not implemented");
  }
}
