// ServerSocket.java

/* Copyright (C) 1999  Red Hat, Inc.

   This file is part of libgcj.

This software is copyrighted work licensed under the terms of the
Libgcj License.  Please consult the file "LIBGCJ_LICENSE" for
details.  */

/**
  * @author Per Bothner <bothner@cygnus.com>
  * @date January 6, 1999.
  */

/** Written using on-line Java Platform 1.2 API Specification.
  * Status:  I believe all methods are implemented.
  */

package java.net;
import java.io.*;

public class ServerSocket
{
  static SocketImplFactory factory;
  SocketImpl impl;

  static final byte[] zeros = {0,0,0,0};
  /* dummy InetAddress, used to bind socket to any (all) network interfaces */
  static final InetAddress ANY_IF = new InetAddress(zeros, null);

  public ServerSocket (int port)
    throws java.io.IOException
  {
    this(port, 50);
  }

  public ServerSocket (int port, int backlog)
    throws java.io.IOException
  {
    this(port, backlog, ANY_IF);
  }

  public ServerSocket (int port, int backlog, InetAddress bindAddr)
    throws java.io.IOException
  {
    if (factory == null)
      this.impl = new PlainSocketImpl();
    else
      this.impl = factory.createSocketImpl();
    SecurityManager s = System.getSecurityManager();
    if (s != null)
      s.checkListen(port);
    impl.create(true);
    impl.bind(bindAddr == null ? ANY_IF : bindAddr, port);
    impl.listen(backlog);
  }

  public InetAddress getInetAddress()
  {
    return impl.getInetAddress();
  }

  public int getLocalPort()
  {
    return impl.getLocalPort();
  }

  public Socket accept ()  throws IOException
  {
    Socket s = new Socket(Socket.factory == null ? new PlainSocketImpl()
			  : Socket.factory.createSocketImpl());
    implAccept (s);
    return s;
  }

  protected final void implAccept (Socket s)  throws IOException
  {
    impl.accept(s.impl);
  }

  public void close () throws IOException
  {
    impl.close();
  }

  public synchronized void setSoTimeout (int timeout) throws SocketException
  {
    if (timeout < 0)
      throw new IllegalArgumentException("Invalid timeout: " + timeout);

    impl.setOption(SocketOptions.SO_TIMEOUT, new Integer(timeout));
  }

  public synchronized int getSoTimeout () throws SocketException
  {
    Object timeout = impl.getOption(SocketOptions.SO_TIMEOUT);
    if (timeout instanceof Integer) 
      return ((Integer)timeout).intValue();
    else
      return 0;
  }

  public String toString ()
  {
    return "ServerSocket" + impl.toString();
  }

  public static synchronized void setSocketFactory (SocketImplFactory fac)
    throws IOException
  {
    factory = fac;
  }
}
