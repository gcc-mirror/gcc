// Socket.java

/* Copyright (C) 1999  Cygnus Solutions

   This file is part of libgcj.

This software is copyrighted work licensed under the terms of the
Libgcj License.  Please consult the file "LIBGCJ_LICENSE" for
details.  */

/**
  * @author Per Bothner <bothner@cygnus.com>
  * @date January 6, 1999.
  */

/** Written using on-line Java Platform 1.2 API Specification.
  * Status:  I believe all methods are implemented, but many
  * of them just throw an exception.
  */

package java.net;
import java.io.*;

public class ServerSocket
{
  static SocketImplFactory factory;
  SocketImpl impl;

  public ServerSocket (int port)
    throws java.io.IOException
  {
    this(port, 5);
  }

  public ServerSocket (int port, int backlog)
    throws java.io.IOException
  {
    this(port, backlog, InetAddress.getLocalHost());
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
    impl.bind(bindAddr, port);
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

  public void setSoTimeout (int timeout) throws SocketException
  {
    throw new InternalError("ServerSocket.setSoTimeout not implemented");
  }

  public int getSoTimeout () throws SocketException
  {
    throw new InternalError("ServerSocket.getSoTimeout not implemented");
  }

  public String toString ()
  {
    return impl.toString();
  }

  public static void setSocketFactory (SocketImplFactory fac)
    throws IOException
  {
    factory = fac;
  }

}
