// PlainSocketImpl.java - Implementation of SocketImpl.

/* Copyright (C) 1999  Free Software Foundation

   This file is part of libgcj.

This software is copyrighted work licensed under the terms of the
Libgcj License.  Please consult the file "LIBGCJ_LICENSE" for
details.  */

package java.net;
import java.io.*;

/**
 * @author Per Bothner <bothner@cygnus.com>
 * @date February 22, 1999.
 */

/**
 * Written using on-line Java Platform 1.2 API Specification, as well
 * as "The Java Class Libraries", 2nd edition (Addison-Wesley, 1998).
 * Status:  Believed complete and correct.
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

  private InputStream in;
  private OutputStream out;

  protected InputStream getInputStream() throws IOException
  {
    // FIXME: TODO - Implement class SocketInputStream timeouts in read();
    if (in == null)
      in = new FileInputStream (fd);
    return in;
  }

  protected OutputStream getOutputStream() throws IOException
  {
    if (out == null)
      out = new FileOutputStream (fd);
    return out;
  }

  protected int available () throws IOException
  {
    return in.available();
  }

  protected void close () throws IOException
  {
    fd.close();
  }
}
