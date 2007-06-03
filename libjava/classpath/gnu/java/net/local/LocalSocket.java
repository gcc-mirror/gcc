/* LocalSocket.java -- a unix domain client socket.
   Copyright (C) 2006  Free Software Foundation, Inc.

This file is a part of GNU Classpath.

GNU Classpath is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2 of the License, or (at
your option) any later version.

GNU Classpath is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
General Public License for more details.

You should have received a copy of the GNU General Public License
along with GNU Classpath; if not, write to the Free Software
Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA 02110-1301
USA

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
exception statement from your version.  */


package gnu.java.net.local;

import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;

import java.net.InetAddress;
import java.net.Socket;
import java.net.SocketAddress;
import java.net.SocketException;

import java.nio.channels.IllegalBlockingModeException;
import java.nio.channels.SocketChannel;

/**
 * A local, or unix-domain socket. Unix domain sockets are connected on the
 * local filesystem itself, rather than a remote address.
 */
public final class LocalSocket extends Socket
{

  // Fields.
  // -------------------------------------------------------------------------

  private final LocalSocketImpl localimpl;
  boolean localClosed;
  boolean localConnected;

  // Constructors.
  // -------------------------------------------------------------------------

  public LocalSocket () throws SocketException
  {
    super ();
    localimpl = new LocalSocketImpl ();
  }

  public LocalSocket (LocalSocketAddress addr) throws SocketException
  {
    this ();
    try
      {
        connect (addr);
      }
    catch (IOException ioe)
      {
        SocketException se = new SocketException ();
        se.initCause (ioe);
        throw se;
      }
  }

  LocalSocket (boolean nocreate) throws IOException
  {
    super ();
    localimpl = new LocalSocketImpl (nocreate);
  }

  // Instance methods.
  // -------------------------------------------------------------------------

  public void bind (SocketAddress bindpoint) throws IOException
  {
    throw new SocketException ("binding local client sockets is nonsensical");
  }

  public void connect (SocketAddress endpoint, int timeout) throws IOException
  {
    if (isClosed ())
      {
        throw new SocketException ("socket is closed");
      }
    if (! (endpoint instanceof LocalSocketAddress))
      {
        throw new IllegalArgumentException ("socket address is not a local address");
      }
    if (getChannel() != null && !getChannel().isBlocking())
      {
        throw new IllegalBlockingModeException ();
      }

    try
      {
        localimpl.doCreate ();
        localimpl.localConnect ((LocalSocketAddress) endpoint);
      }
    catch (IOException ioe)
      {
        close ();
        throw ioe;
      }
    localConnected = true;
  }

  public InetAddress getInetAddress ()
  {
    return null;
  }

  public InetAddress getLocalAddress ()
  {
    return null;
  }

  public int getPort ()
  {
    return -1;
  }

  public int getLocalPort ()
  {
    return -1;
  }

  public SocketChannel getChannel ()
  {
    return null;
  }

  public SocketAddress getLocalSocketAddress ()
  {
    return localimpl.getLocalAddress ();
  }

  public SocketAddress getRemoteSocketAddress ()
  {
    return localimpl.getRemoteAddress ();
  }

  public InputStream getInputStream () throws IOException
  {
    return localimpl.getInputStream ();
  }

  public OutputStream getOutputStream () throws IOException
  {
    return localimpl.getOutputStream ();
  }

  public void sendUrgentData (int b) throws IOException
  {
    localimpl.sendUrgentData (b);
  }

  public synchronized void close () throws IOException
  {
    localimpl.close ();
    localClosed = true;
  }

  public void shutdownInput () throws IOException
  {
    localimpl.shutdownInput ();
  }

  public void shutdownOutput () throws IOException
  {
    localimpl.shutdownOutput ();
  }

  public boolean isClosed ()
  {
    return localClosed;
  }

  public boolean isBound ()
  {
    return false;
  }

  public boolean isConnected ()
  {
    return localConnected;
  }

  // Unsupported methods.
  // -------------------------------------------------------------------------

  public void setTcpNoDelay (boolean b) throws SocketException
  {
    throw new SocketException ("local sockets do not support this option");
  }

  public boolean getTcpNoDelay() throws SocketException
  {
    throw new SocketException ("local sockets do not support this option");
  }

  public void setSoLinger (boolean b, int i) throws SocketException
  {
    throw new SocketException ("local sockets do not support this option");
  }

  public int getSoLinger () throws SocketException
  {
    throw new SocketException ("local sockets do not support this option");
  }

  public void setOOBInline (boolean b) throws SocketException
  {
    throw new SocketException ("local sockets do not support this option");
  }

  public boolean getOOBInline () throws SocketException
  {
    throw new SocketException ("local sockets do not support this option");
  }

  public void setSoTimeout (int i) throws SocketException
  {
    // Ignore.
  }

  public int getSoTimeout () throws SocketException
  {
    // We don't support timeout, so we return 0.
    return 0;
  }

  public void setSendBufferSize (int i) throws SocketException
  {
    throw new SocketException ("local sockets do not support this option");
  }

  public int getSendBufferSize() throws SocketException
  {
    throw new SocketException ("local sockets do not support this option");
  }

  public void setReceiveBufferSize (int i) throws SocketException
  {
    throw new SocketException ("local sockets do not support this option");
  }

  public int getReceiveBufferSize () throws SocketException
  {
    throw new SocketException ("local sockets do not support this option");
  }

  public void setKeepAlive (boolean b) throws SocketException
  {
    throw new SocketException ("local sockets do not support this option");
  }

  public boolean getKeepAlive () throws SocketException
  {
    throw new SocketException ("local sockets do not support this option");
  }

  public void setTrafficClass (int i) throws SocketException
  {
    throw new SocketException ("local sockets do not support this option");
  }

  public int getTrafficClass () throws SocketException
  {
    throw new SocketException ("local sockets do not support this option");
  }

  public void setReuseAddress (boolean b) throws SocketException
  {
    throw new SocketException ("local sockets do not support this option");
  }

  public boolean getReuseAddress () throws SocketException
  {
    throw new SocketException ("local sockets do not support this option");
  }

  LocalSocketImpl getLocalImpl ()
  {
    return localimpl;
  }
}
