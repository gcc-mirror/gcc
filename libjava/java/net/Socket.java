// Socket.java

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
    // FIXME: JCL p. 1586 says if localPort is unspecified, bind to any port,
    // i.e. '0' and if localAddr is unspecified, use getLocalAddress() as
    // that default.  JDK 1.2 doc infers not to do a bind.
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
    // FIXME: JCL p. 1586 says if localPort is unspecified, bind to any port,
    // i.e. '0' and if localAddr is unspecified, use getLocalAddress() as
    // that default.  JDK 1.2 doc infers not to do a bind.
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
    // FIXME: JCL p. 1587 says if localAddr is null, use getLocalAddress().
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
    // FIXME: JCL p. 1587 says if localAddr is null, use getLocalAddress().
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
    // FIXME: JCL p. 1586 says if localPort is unspecified, bind to any port,
    // i.e. '0' and if localAddr is unspecified, use getLocalAddress() as
    // that default.  JDK 1.2 doc infers not to do a bind.
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
    // FIXME: JCL p. 1586 says if localPort is unspecified, bind to any port,
    // i.e. '0' and if localAddr is unspecified, use getLocalAddress() as
    // that default.  JDK 1.2 doc infers not to do a bind.
    impl.connect(host, port);
  }

  public InetAddress getInetAddress ()
  {
    return impl.getInetAddress();
  }

  public InetAddress getLocalAddress ()
  {
    // FIXME: see note in DatagramSocket.java about checkConnect() and security
    try
      {
	return (InetAddress)impl.getOption(SocketOptions.SO_BINDADDR);
      }
    catch (SocketException x)
      {
	// (hopefully) shouldn't happen
	System.err.println(x);
        throw new java.lang.InternalError("Error in PlainSocketImpl.getOption");
      }
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
    impl.setOption( SocketOptions.TCP_NODELAY, new Boolean(on) );
  }

  public boolean getTcpNoDelay() throws SocketException
  {
    Boolean bool = (Boolean)impl.getOption( SocketOptions.TCP_NODELAY );
    return bool.booleanValue();
  }

  public void setSoLinger(boolean on, int linger) throws SocketException
  {
    if ( on && (linger >= 0) ) 
      {
	if (linger > 65535)
	  linger = 65535;
	impl.setOption( SocketOptions.SO_LINGER, new Integer(linger) );
      } 
    else if ( on && (linger < 0) ) 
      throw new IllegalArgumentException("SO_LINGER must be >= 0");
    else
      impl.setOption( SocketOptions.SO_LINGER, new Boolean(false) );
  }

  public int getSoLinger() throws SocketException
  {
    Object linger = impl.getOption(SocketOptions.SO_LINGER);    
    if (linger instanceof Integer) 
      return ((Integer)linger).intValue();
    else
      return -1;
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

  // JDK1.2
  public void setSendBufferSize (int size) throws SocketException
  {
    if (size <= 0)
      throw new IllegalArgumentException("Invalid buffer size: " + size);

    impl.setOption(SocketOptions.SO_SNDBUF, new Integer(size));
  }

  // JDK1.2
  public int getSendBufferSize () throws SocketException
  {
    Integer buf = (Integer)impl.getOption(SocketOptions.SO_SNDBUF);
    return buf.intValue();
  }

  // JDK1.2
  public void setReceiveBufferSize (int size) throws SocketException
  {
    if (size <= 0)
      throw new IllegalArgumentException("Invalid buffer size: " + size);

    impl.setOption(SocketOptions.SO_RCVBUF, new Integer(size));
  }

  // JDK1.2
  public int getReceiveBufferSize () throws SocketException
  {
    Integer buf = (Integer)impl.getOption(SocketOptions.SO_RCVBUF);
    return buf.intValue();
  }

  public synchronized void close ()  throws IOException
  {
    impl.close();
  }

  public String toString ()
  {
    return "Socket" + impl.toString();
  }

  public static synchronized void setSocketImplFactory (SocketImplFactory fac)
    throws IOException
  {
    factory = fac;
  }
}
