/* LocalServerSocket.java -- a unix domain server socket.
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

import java.net.InetAddress;
import java.net.ServerSocket;
import java.net.Socket;
import java.net.SocketAddress;

public final class LocalServerSocket extends ServerSocket
{

  // Fields.
  // -------------------------------------------------------------------------

  private LocalSocketImpl myImpl;
  private boolean closed;

  // Constructors.
  // -------------------------------------------------------------------------

  public LocalServerSocket () throws IOException
  {
    myImpl = new LocalSocketImpl ();
  }

  public LocalServerSocket (SocketAddress bindPoint) throws IOException
  {
    this ();
    bind (bindPoint);
  }

  // Instance methods.
  // -------------------------------------------------------------------------

  public void bind (SocketAddress bindPoint) throws IOException
  {
    bind (bindPoint, 0);
  }

  public void bind (SocketAddress bindPoint, int backlog) throws IOException
  {
    myImpl.doCreate ();
    myImpl.bind (bindPoint);
    myImpl.listen (backlog);
  }

  public InetAddress getInetAddress ()
  {
    return null;
  }

  public int getLocalPort ()
  {
    return -1;
  }

  public SocketAddress getLocalSocketAddress ()
  {
    return myImpl.getLocalAddress ();
  }

  public Socket accept () throws IOException
  {
    LocalSocket s = new LocalSocket (true);
    myImpl.accept (s.getLocalImpl());
    s.localConnected = true;
    return s;
  }

  public void close () throws IOException
  {
    myImpl.close ();
    myImpl.unlink ();
    closed = true;
  }

  public boolean isBound ()
  {
    return myImpl.getLocalAddress () != null;
  }

  public boolean isClosed ()
  {
    return closed;
  }

  public void setSoTimeout (int timeout)
  {
    throw new UnsupportedOperationException ("local sockets do not support timeouts");
  }

  public int getSoTimeout ()
  {
    throw new UnsupportedOperationException ("local sockets do not support timeouts");
  }

  public void setReuseAddress (boolean b)
  {
    throw new UnsupportedOperationException ("local sockets do not support reuse address");
  }

  public boolean getReuseAddress ()
  {
    throw new UnsupportedOperationException ("local sockets do not support reuse address");
  }

  public String toString ()
  {
    return LocalServerSocket.class.getName() + " [ address="
      + myImpl.getLocalAddress() + " ]";
  }

  public void setReceiveBufferSize (int size)
  {
    throw new UnsupportedOperationException ("local sockets do not support buffer size");
  }

  public int getReceiveBufferSize ()
  {
    throw new UnsupportedOperationException ("local sockets do not support buffer size");
  }

  public void setSendBufferSize (int size)
  {
    throw new UnsupportedOperationException ("local sockets do not support buffer size");
  }

  public int getSendBufferSize ()
  {
    throw new UnsupportedOperationException ("local sockets do not support buffer size");
  }
}
