// PlainSocketImpl.java - Implementation of SocketImpl.

/* Copyright (C) 1999 , 2002 Free Software Foundation

   This file is part of libgcj.

This software is copyrighted work licensed under the terms of the
Libgcj License.  Please consult the file "LIBGCJ_LICENSE" for
details.  */

package java.net;
import java.io.*;


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
		   _Jv_IP_MULTICAST_IF_ = SocketOptions.IP_MULTICAST_IF,
                   _Jv_SO_LINGER_ = SocketOptions.SO_LINGER,
                   _Jv_SO_TIMEOUT_ = SocketOptions.SO_TIMEOUT,
                   _Jv_SO_SNDBUF_ = SocketOptions.SO_SNDBUF,
                   _Jv_SO_RCVBUF_ = SocketOptions.SO_RCVBUF;

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

  protected native void create (boolean stream)  throws IOException;

  protected void connect (String host, int port) throws IOException
  {
    connect(InetAddress.getByName(host), port);
  }

  protected native void connect (InetAddress host, int port)
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
