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

public class Socket
{
  static SocketImplFactory factory;
  SocketImpl impl;

  protected Socket ()
  {
  }

  protected Socket (SocketImpl impl) throws SocketException
  {
    this.impl = impl;
  }

  public Socket (String host, int port)
    throws UnknownHostException, IOException
  {
    this(factory == null ? new PlainSocketImpl() : factory.createSocketImpl());
    SecurityManager s = System.getSecurityManager();
    if (s != null)
      s.checkConnect(host, port);
    impl.create(true);
    impl.connect(host, port);
  }

  public Socket (InetAddress address, int port)
    throws IOException 
  {
    this(factory == null ? new PlainSocketImpl() : factory.createSocketImpl());
    SecurityManager s = System.getSecurityManager();
    if (s != null)
      s.checkConnect(address.getHostName(), port);
    impl.create(true);
    impl.connect(address, port);
  }

  public Socket (String host, int port,
		 InetAddress localAddr, int localPort) throws IOException
  {
    this(factory == null ? new PlainSocketImpl() : factory.createSocketImpl());
    SecurityManager s = System.getSecurityManager();
    if (s != null)
      s.checkConnect(host, port);
    impl.create(true);
    impl.bind(localAddr, localPort);
    impl.connect(host, port);
  }

  public Socket (InetAddress address, int port,
		 InetAddress localAddr, int localPort) throws IOException
  {
    this(factory == null ? new PlainSocketImpl() : factory.createSocketImpl());
    SecurityManager s = System.getSecurityManager();
    if (s != null)
      s.checkConnect(address.getHostName(), port);
    impl.create(true);
    impl.bind(localAddr, localPort);
    impl.connect(address, port);
  }

  /**
   * @deprecated Use DatagramSocket instead for UDP transport.
   */
  public Socket (String host, int port, boolean stream) throws IOException
  {
    impl = factory == null ? new PlainSocketImpl()
      : factory.createSocketImpl();
    impl.create(stream);
    SecurityManager s = System.getSecurityManager();
    if (s != null)
      s.checkConnect(host, port);
    impl.connect(host, port);
  }

  /**
   * @deprecated Use DatagramSocket instead for UDP transport.
   */
  public Socket (InetAddress host, int port, boolean stream) throws IOException
  {
    impl = factory == null ? new PlainSocketImpl()
      : factory.createSocketImpl();
    impl.create(stream);
    SecurityManager s = System.getSecurityManager();
    if (s != null)
      s.checkConnect(host.getHostName(), port);
    impl.connect(host, port);
  }

  public InetAddress getInetAddress ()
  {
    return impl.getInetAddress();
  }

  public InetAddress getLocalAddress ()
  {
    // There doesn't seem to be any way to implement this
    // using a (generic) SocketImpl ...  What am I missing?
    throw new InternalError("Socket.getLocalAddres not implemented");
  }

  public int getPort ()
  {
    return impl.getPort();
  }

  public int getLocalPort ()
  {
    return impl.getLocalPort();
  }

  public InputStream getInputStream () throws IOException
  {
    return impl.getInputStream();
  }

  public OutputStream getOutputStream () throws IOException
  {
    return impl.getOutputStream();
  }

  public void setTcpNoDelay (boolean on)  throws SocketException
  {
    throw new InternalError("Socket.setTcpNoDelay not implemented");
  }

  public boolean getTcpNoDelay() throws SocketException
  {
    throw new InternalError("Socket.getTcpNoDelay not implemented");
  }

  public void setSoLinger(boolean on, int linger) throws SocketException
  {
    throw new InternalError("Socket.setSoLinger not implemented");
  }

  public boolean getSoLinger() throws SocketException
  {
    throw new InternalError("Socket.getSoLinger not implemented");
  }

  public void setSoTimeout (int timeout) throws SocketException
  {
    throw new InternalError("Socket.setSoTimeout not implemented");
  }

  public int getSoTimeout () throws SocketException
  {
    throw new InternalError("Socket.getSoTimeout not implemented");
  }

  public void setSendBufferSize (int size) throws SocketException
  {
    throw new InternalError("Socket.setSendBufferSize not implemented");
  }

  public int getSendBufferSize () throws SocketException
  {
    throw new InternalError("Socket.getSendBufferSize not implemented");
  }

  public void setReceiveBufferSize (int size) throws SocketException
  {
    throw new InternalError("Socket.setReceiveBufferSize not implemented");
  }

  public int getReceiveBufferSize () throws SocketException
  {
    throw new InternalError("Socket.getReceiveBufferSize not implemented");
  }

  public void close ()  throws IOException
  {
    impl.close();
  }

  public String toString ()
  {
    return impl.toString();
  }

  public static void setSocketImplFactory (SocketImplFactory fac)
    throws IOException
  {
    factory = fac;
  }
}
