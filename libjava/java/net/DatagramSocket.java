// DatagramSocket.java

/* Copyright (C) 1999  Cygnus Solutions

   This file is part of libgcj.

This software is copyrighted work licensed under the terms of the
Libgcj License.  Please consult the file "LIBGCJ_LICENSE" for
details.  */

package java.net;
import java.io.IOException;

/**
 * @author Warren Levy <warrenl@cygnus.com>
 * @date May 3, 1999.
 */

/**
 * Written using on-line Java Platform 1.2 API Specification, as well
 * as "The Java Class Libraries", 2nd edition (Addison-Wesley, 1998).
 * Status:  Believed complete and correct.
 */

public class DatagramSocket
{
  DatagramSocketImpl impl;
  // FIXME: Shouldn't this be determined by getsockname() instead?
  InetAddress laddr;

  public DatagramSocket() throws SocketException
  {
    this(0, null);
  }

  public DatagramSocket(int port) throws SocketException
  {
    this(port, null);
  }

  public DatagramSocket(int port, InetAddress laddr) throws SocketException
  {
    if (port < 0 || port > 65535)
      throw new IllegalArgumentException("Invalid port: " + port);

    SecurityManager s = System.getSecurityManager();
    if (s != null)
      s.checkListen(port);

    String propVal = System.getProperty("impl.prefix");
    if (propVal == null || propVal.equals(""))
      propVal = "Plain";
    impl = (DatagramSocketImpl) Class.forName("java.net." + propVal +
					"DatagramSocketImpl").newInstance();
    impl.create();
    // TBD: if this is right then the same should be done in Socket().
    try
    {
      if (laddr == null)
	laddr = InetAddress.getLocalHost();
    }
    catch (UnknownHostException e)
    {
      throw new BindException(e.getMessage());
    }

    // For multicasting, set the socket to be reused (Stevens pp. 195-6).
    if (this instanceof MulticastSocket)
      impl.setOption(SocketOptions.SO_REUSEADDR, new Boolean(true));

    impl.bind(port, laddr);
    this.laddr = laddr;
  }

  public void close()
  {
    impl.close();
  }

  public InetAddress getLocalAddress()
  {
    return laddr;
  }

  public int getLocalPort()
  {
    return impl.getLocalPort();
  }

  public synchronized int getSoTimeout() throws SocketException
  {
    Object timeout = impl.getOption(SocketOptions.SO_TIMEOUT);
    if (timeout instanceof Integer) 
      return ((Integer)timeout).intValue();
    else
      return 0;
  }

  public synchronized void receive(DatagramPacket p) throws IOException
  {
    SecurityManager s = System.getSecurityManager();
    if (s != null)
      s.checkAccept(p.getAddress().getHostAddress(), p.getPort());

    impl.receive(p);
  }

  public void send(DatagramPacket p) throws IOException
  {
    // JDK1.2: Don't do security checks if socket is connected; see jdk1.2 api.
    SecurityManager s = System.getSecurityManager();
    if (s != null)
      {
	InetAddress addr = p.getAddress();
	if (addr.isMulticastAddress())
	  s.checkMulticast(addr);
	else
	  s.checkConnect(addr.getHostAddress(), p.getPort());
      }

    // FIXME: if this is a subclass of MulticastSocket, use getTTL for TTL val.
    impl.send(p);
  }

  public synchronized void setSoTimeout(int timeout) throws SocketException
  {
    if (timeout < 0)
      throw new IllegalArgumentException("Invalid timeout: " + timeout);

    impl.setOption(SocketOptions.SO_TIMEOUT, new Integer(timeout));
  }

  // JDK1.2
  // public void connect(InetAddress address, int port)
  // {
  // }

  // JDK1.2
  // public void disconnect()
  // {
  // }

  // JDK1.2
  // public InetAddress getInetAddress()
  // {
  // }

  // JDK1.2
  // public int getPort()
  // {
  // }

  // JDK1.2
  // public int getReceiveBufferSize() throws SocketException
  // {
  // }

  // JDK1.2
  // public int getSendBufferSize() throws SocketException
  // {
  // }

  // JDK1.2
  // public void setReceiveBufferSize(int size) throws SocketException
  // {
  // }

  // JDK1.2
  // public void setSendBufferSize(int size) throws SocketException
  // {
  // }
}
