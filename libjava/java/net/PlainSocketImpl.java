/* PlainSocketImpl.java -- Default socket implementation
   Copyright (C) 1998, 1999 Free Software Foundation, Inc.

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


package java.net;

import java.io.InputStream;
import java.io.IOException;
import java.io.OutputStream;

/**
 * The standard GCJ socket implementation.
 * Written using on-line Java Platform 1.2 API Specification, as well
 * as "The Java Class Libraries", 2nd edition (Addison-Wesley, 1998).
 * Status:  Believed complete and correct.
 *
 * @author Per Bothner <bothner@cygnus.com>
 * @author Nic Ferrier <nferrier@tapsellferrier.co.uk>
 */
class PlainSocketImpl extends SocketImpl
{
  // These fields are mirrored for use in native code to avoid cpp conflicts
  // when the #defines in system header files are the same as the public fields.
  static final int _Jv_TCP_NODELAY_ = SocketOptions.TCP_NODELAY,
                   _Jv_SO_BINDADDR_ = SocketOptions.SO_BINDADDR,
                   _Jv_SO_REUSEADDR_ = SocketOptions.SO_REUSEADDR,
                   _Jv_SO_BROADCAST_ = SocketOptions.SO_BROADCAST,
                   _Jv_SO_OOBINLINE_ = SocketOptions.SO_OOBINLINE,
                   _Jv_IP_MULTICAST_IF_ = SocketOptions.IP_MULTICAST_IF,
                   _Jv_IP_MULTICAST_IF2_ = SocketOptions.IP_MULTICAST_IF2,
                   _Jv_IP_MULTICAST_LOOP_ = SocketOptions.IP_MULTICAST_LOOP,
                   _Jv_IP_TOS_ = SocketOptions.IP_TOS,
                   _Jv_SO_LINGER_ = SocketOptions.SO_LINGER,
                   _Jv_SO_TIMEOUT_ = SocketOptions.SO_TIMEOUT,
                   _Jv_SO_SNDBUF_ = SocketOptions.SO_SNDBUF,
                   _Jv_SO_RCVBUF_ = SocketOptions.SO_RCVBUF,
                   _Jv_SO_KEEPALIVE_ = SocketOptions.SO_KEEPALIVE;

  /**
   * The OS file handle representing the socket.
   * This is used for reads and writes to/from the socket and
   * to close it.
   *
   * When the socket is closed this is reset to -1.
   */
  int fnum = -1;

  // This value is set/read by setOption/getOption.
  int timeout = 0;
  
  // localAddress cache
  InetAddress localAddress;

  public native void setOption(int optID, Object value) throws SocketException;

  public native Object getOption(int optID) throws SocketException;

  public native void shutdownInput () throws IOException;

  public native void shutdownOutput () throws IOException;

  protected native void create (boolean stream)  throws IOException;

  protected void connect (String host, int port) throws IOException
  {
    connect (new InetSocketAddress (InetAddress.getByName(host), port), 0);
  }

  protected void connect (InetAddress host, int port) throws IOException
  {
    connect (new InetSocketAddress (host, port), 0);
  }

  protected native void connect (SocketAddress addr, int timeout)
    throws IOException;

  protected native void bind (InetAddress host, int port) throws IOException;

  protected native void listen (int backlog) throws IOException;

  private native void accept (PlainSocketImpl s) throws IOException;

  protected void accept (SocketImpl s) throws IOException
  {
    accept((PlainSocketImpl) s);
  }

  protected native int available() throws IOException;

  protected native void close () throws IOException;

  protected native void sendUrgentData(int data)
    throws IOException;

  // Stream handling.

  /** A cached copy of the in stream for reading from the socket.  */
  private InputStream in;

  /** A cached copy of the out stream for writing to the socket.  */
  private OutputStream out;


  // The native read methods.

  private native int read() throws IOException;

  private native int read(byte[] buffer, int offset, int count)
    throws IOException;


  // The native write methods.

  private native void write(int c) throws IOException;

  private native void write(byte[] buffer, int offset, int count)
    throws IOException;

  protected void finalize() throws Throwable
  {
    synchronized (this)
      {
	if (fnum != -1)
	  try
	    {
	      close();
	    }
	  catch (IOException ex)
	    {
	      // ignore
	    }
      }
    super.finalize();
  }

  /** @return the input stream attached to the socket.
   */
  protected InputStream getInputStream() throws IOException
  {
    if (in == null)
      in = new SocketInputStream();
    return in;
  }

  /** @return the output stream attached to the socket.
   */
  protected OutputStream getOutputStream() throws IOException
  {
    if (out == null)
      out = new SocketOutputStream();
    return out;
  }

  /**
   * A stream which reads from the socket implementation.
   *
   * @author Nic Ferrier <nferrier@tapsellferrier.co.uk>
   */
  class SocketInputStream
    extends InputStream
  {
    SocketInputStream()
    {
    }
    
    public final void close() throws IOException
    {
      PlainSocketImpl.this.close();
    }

    public final int available() throws IOException
    {
      return PlainSocketImpl.this.available();
    }

    public final int read() throws IOException
    {
      return PlainSocketImpl.this.read();
    }

    public final int read(byte[] buffer, int offset, int length)
      throws IOException
    {
      return PlainSocketImpl.this.read(buffer, offset, length);
    }

    public final int read(byte[] buffer)
      throws IOException
    {
      return PlainSocketImpl.this.read(buffer, 0, buffer.length);
    }
  }

  /** A stream which writes to the socket implementation.
   *
   * @author Nic Ferrier  <nferrier@tapsellferrier.co.uk>
   */
  class SocketOutputStream
    extends OutputStream
  {
    public final void close() throws IOException
    {
      PlainSocketImpl.this.close();
    }

    public final void write(int c) throws IOException
    {
      PlainSocketImpl.this.write(c);
    }

    public final void write(byte[] buffer, int offset, int length)
      throws IOException
    {
      PlainSocketImpl.this.write(buffer, offset, length);
    }

    public final void write(byte[] buffer)
      throws IOException
    {
      PlainSocketImpl.this.write(buffer, 0, buffer.length);
    }
  }
}
