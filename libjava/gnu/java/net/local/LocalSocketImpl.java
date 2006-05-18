/* LocalSocketImpl.java -- a unix domain client socket implementation.
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

import java.io.FileDescriptor;
import java.io.InputStream;
import java.io.IOException;
import java.io.OutputStream;

import java.net.InetAddress;
import java.net.SocketAddress;
import java.net.SocketException;
import java.net.SocketImpl;

final class LocalSocketImpl extends SocketImpl
{

  // Fields.
  // -------------------------------------------------------------------------

  private boolean created;
  private InputStream in;
  private OutputStream out;
  private int socket_fd;
  private LocalSocketAddress local;
  private LocalSocketAddress remote;

  // Constructor.
  // -------------------------------------------------------------------------

  LocalSocketImpl ()
  {
    this (false);
  }

  LocalSocketImpl (boolean nocreate)
  {
    created = nocreate;
    socket_fd = -1;
    fd = new FileDescriptor ();
  }

  // Instance methods.
  // -------------------------------------------------------------------------

  public void setOption (int opt, Object value) throws SocketException
  {
    throw new SocketException ("local sockets do not support options");
  }

  public Object getOption (int opt) throws SocketException
  {
    throw new SocketException ("local sockets do not support options");
  }

  protected void create (boolean stream) throws IOException { }
  protected void listen (int timeout) throws IOException { }
  protected void accept (LocalSocketImpl socket) throws IOException { }
  protected int available () throws IOException { return -1; }
  protected void close () throws IOException { }
  protected void sendUrgentData (int data) throws IOException { }
  protected void shutdownInput () throws IOException { }
  protected void shutdownOutput () throws IOException { }

  void unlink () throws IOException { }
  void localBind (LocalSocketAddress addr) throws IOException { }
  void localConnect (LocalSocketAddress addr) throws IOException { }
  int read (byte[] buf, int off, int len) throws IOException { return -1; }
  void write (byte[] buf, int off, int len) throws IOException { }

  void doCreate () throws IOException
  {
    if (!created)
      {
        create (true);
      }
  }

  LocalSocketAddress getLocalAddress ()
  {
    return local;
  }

  LocalSocketAddress getRemoteAddress ()
  {
    return remote;
  }

  protected InputStream getInputStream()
  {
    if (in == null)
      {
        in = new LocalInputStream (this);
      }

    return in;
  }

  protected OutputStream getOutputStream()
  {
    if (out == null)
      {
        out = new LocalOutputStream (this);
      }

    return out;
  }

  protected void accept (SocketImpl impl) throws IOException
  {
    if (! (impl instanceof LocalSocketImpl))
      {
        throw new IllegalArgumentException ("not a local socket");
      }
    accept ((LocalSocketImpl) impl);
  }

  protected void connect (String host, int port) throws IOException
  {
    throw new SocketException ("this is a local socket");
  }

  protected void connect (InetAddress addr, int port) throws IOException
  {
    throw new SocketException ("this is a local socket");
  }

  protected void connect(SocketAddress addr, int timeout) throws IOException
  {
    if (! (addr instanceof LocalSocketAddress))
      {
        throw new SocketException ("address is not local");
      }
    localConnect ((LocalSocketAddress) addr);
  }

  protected void bind (InetAddress addr, int port) throws IOException
  {
    throw new SocketException ("this is a local socket");
  }

  protected void bind (SocketAddress addr) throws IOException
  {
    if (! (addr instanceof LocalSocketAddress))
      {
        throw new SocketException ("address is not local");
      }
    localBind ((LocalSocketAddress) addr);
  }

// Inner classes.
  // -------------------------------------------------------------------------

  class LocalInputStream extends InputStream
  {

    // Field.
    // -----------------------------------------------------------------------

    private final LocalSocketImpl impl;

    // Constructor.
    // -----------------------------------------------------------------------

    LocalInputStream (LocalSocketImpl impl)
    {
      this.impl = impl;
    }

    // Instance methods.
    // -----------------------------------------------------------------------

    public int available () throws IOException
    {
      return impl.available();
    }

    public boolean markSupported ()
    {
      return false;
    }

    public void mark (int readLimit)
    {
    }

    public void reset () throws IOException
    {
      throw new IOException ("mark/reset not supported");
    }

    public void close () throws IOException
    {
      impl.close();
    }

    public int read () throws IOException
    {
      byte[] buf = new byte[1];
      int ret = read (buf);
      if (ret != -1)
        {
          return buf[0] & 0xFF;
        }
      else
        {
          return -1;
        }
    }

    public int read (byte[] buf) throws IOException
    {
      return read (buf, 0, buf.length);
    }

    public int read (byte[] buf, int off, int len) throws IOException
    {
      int ret = impl.read (buf, off, len);

      if (ret == 0)
        {
          return -1;
        }

      return ret;
    }
  }

  class LocalOutputStream extends OutputStream
  {

    // Field.
    // -----------------------------------------------------------------------

    private final LocalSocketImpl impl;

    // Constructor.
    // -----------------------------------------------------------------------

    LocalOutputStream (LocalSocketImpl impl)
    {
      this.impl = impl;
    }

    // Instance methods.
    // -----------------------------------------------------------------------

    public void close () throws IOException
    {
      impl.close ();
    }

    public void flush () throws IOException
    {
    }

    public void write (int b) throws IOException
    {
      byte[] buf = new byte [1];
      buf[0] = (byte) b;
      write (buf);
    }

    public void write (byte[] buf) throws IOException
    {
      write (buf, 0, buf.length);
    }

    public void write (byte[] buf, int off, int len) throws IOException
    {
      impl.write (buf, off, len);
    }
  }
}
