// DatagramSocket.java

/* Copyright (C) 1999, 2000  Free Software Foundation

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
      impl = new PlainDatagramSocketImpl();
    else
      try
	{
	  impl = (DatagramSocketImpl) Class.forName("java.net." + propVal +
					"DatagramSocketImpl").newInstance();
	}
      catch (Exception e)
	{
	  System.err.println("Could not instantiate class: java.net." +
	    propVal + "DatagramSocketImpl");
	  impl = new PlainDatagramSocketImpl();
	}
    impl.create();

    // For multicasting, set the socket to be reused (Stevens pp. 195-6).
    if (this instanceof MulticastSocket)
      impl.setOption(SocketOptions.SO_REUSEADDR, new Boolean(true));

    impl.bind(port, laddr == null ? InetAddress.ANY_IF : laddr);
  }

  public void close()
  {
    impl.close();
  }

  public InetAddress getLocalAddress()
  {
    SecurityManager s = System.getSecurityManager();
    // FIXME: JCL p. 510 says this should call checkConnect.  But what
    // string should be used as the hostname?  Maybe this is just a side
    // effect of calling InetAddress.getLocalHost.
    //
    // And is getOption with SO_BINDADDR the right way to get the address?
    // Doesn't seem to be since this method doesn't throw a SocketException
    // and SO_BINADDR can throw one.
    //
    // Also see RETURNS section in JCL p. 510 about returning any local
    // addr "if the current execution context is not allowed to connect to
    // the network interface that is actually bound to this datagram socket."
    // How is that done?  via InetAddress.getLocalHost?  But that throws
    // an UnknownHostException and this method doesn't.
    //
    // if (s != null)
    //   s.checkConnect("localhost", -1);
    try
      {
	return (InetAddress)impl.getOption(SocketOptions.SO_BINDADDR);
      }
    catch (SocketException ex)
      {
      }

    try
      {
	return InetAddress.getLocalHost();
      }
    catch (UnknownHostException ex)
      {
	// FIXME: This should never happen, so how can we avoid this construct?
	return null;
      }
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

  /**
   * This method returns the value of the system level socket option
   * SO_RCVBUF, which is used by the operating system to tune buffer
   * sizes for data transfers.
   *
   * @return The receive buffer size.
   *
   * @exception SocketException If an error occurs.
   *
   * @since 1.2
   */
  public int getReceiveBufferSize() throws SocketException
  {
    Object obj = impl.getOption(SocketOptions.SO_RCVBUF);
  
    if (obj instanceof Integer)
      return(((Integer)obj).intValue());
    else 
      throw new SocketException("Unexpected type");
  }

  /**
   * This method returns the value of the system level socket option
   * SO_SNDBUF, which is used by the operating system to tune buffer
   * sizes for data transfers.
   *
   * @return The send buffer size.
   *
   * @exception SocketException If an error occurs.
   *
   * @since 1.2
   */
  public int getSendBufferSize() throws SocketException
  {
    Object obj = impl.getOption(SocketOptions.SO_SNDBUF);

    if (obj instanceof Integer)
      return(((Integer)obj).intValue());
    else
      throw new SocketException("Unexpected type");
  }

  /**
   * This method sets the value for the system level socket option
   * SO_RCVBUF to the specified value.  Note that valid values for this
   * option are specific to a given operating system.
   *
   * @param size The new receive buffer size.
   *
   * @exception SocketException If an error occurs.
   *  
   * @since 1.2
   */
  public void setReceiveBufferSize(int size) throws SocketException
  {
    if (size < 0)
      throw new IllegalArgumentException("Buffer size is less than 0");

    impl.setOption(SocketOptions.SO_RCVBUF, new Integer(size));
  }

  /**
   * This method sets the value for the system level socket option
   * SO_SNDBUF to the specified value.  Note that valid values for this
   * option are specific to a given operating system.
   *
   * @param size The new send buffer size.
   *
   * @exception SocketException If an error occurs.
   *
   * @since 1.2
   */
  public void setSendBufferSize(int size) throws SocketException
  {
    if (size < 0)
      throw new IllegalArgumentException("Buffer size is less than 0");
  
    impl.setOption(SocketOptions.SO_SNDBUF, new Integer(size));
  }
}
