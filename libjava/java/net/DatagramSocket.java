// DatagramSocket.java

/* Copyright (C) 1999, 2000, 2002  Free Software Foundation

   This file is part of libgcj.

This software is copyrighted work licensed under the terms of the
Libgcj License.  Please consult the file "LIBGCJ_LICENSE" for
details.  */

package java.net;

import java.io.IOException;
import java.nio.channels.DatagramChannel;
import java.nio.channels.IllegalBlockingModeException;

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
  /**
   * This is the user DatagramSocketImplFactory for this class.  If this
   * variable is null, a default factory is used.
   */
  static DatagramSocketImplFactory factory;
	  
  DatagramSocketImpl impl;

  DatagramChannel ch;

  /**
   * Creates a DatagramSocket
   *
   * @exception SocketException If an error occurs
   * @exception SecurityException If a security manager exists and
   * its checkListen method doesn't allow the operation
   */
  public DatagramSocket() throws SocketException
  {
    this(0, null);
  }

  /**
   * Creates a DatagramSocket from a specified DatagramSocketImpl instance
   *
   * @param impl The DatagramSocketImpl the socket will be created from
   * 
   * @since 1.4
   */
  protected DatagramSocket (DatagramSocketImpl impl)
  {
    this.impl = impl;
  }

  /**
   * Creates a datagram socket that is bound to a given socket address
   *
   * @param bindaddr The socket address to bind to
   *
   * @exception SocketException If an error occurs
   * @exception SecurityException If a security manager exists and
   * its checkListen method doesn't allow the operation
   * 
   * @since 1.4
   */
  public DatagramSocket (SocketAddress bindaddr)
    throws SocketException
  {
    this (((InetSocketAddress) bindaddr).getPort (),
          ((InetSocketAddress) bindaddr).getAddress ());
  }

  /**
   * Creates a datagram socket that is bound to a specific port
   *
   * @param port The port number to bind to
   *
   * @exception SocketException If an error occurs
   * @exception SecurityException If a security manager exists and
   * its checkListen method doesn't allow the operation
   */
  public DatagramSocket(int port) throws SocketException
  {
    this(port, null);
  }

  /**
   * Creates a datagram socket that is bound to a specific port/inet address
   *
   * @param port The port number to bind to
   * @param laddr The local address to bind to
   *
   * @exception SocketException If an error occurs
   * @exception SecurityException If a security manager exists and
   * its checkListen method doesn't allow the operation
   */
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

  /**
   * Binds the socket to the given socket addres
   *
   * @param address The socket address to bind to
   *
   * @exception SocketException If an error occurs
   * @exception SecurityException If a security manager exists and
   * its checkListen method doesn't allow the operation
   * @exception IllegalArgumentException If address type is not supported
   *
   * @since 1.4
   */
  public void bind (SocketAddress address)
    throws SocketException
  {
    if (! (address instanceof InetSocketAddress))
      throw new IllegalArgumentException ();

    InetSocketAddress tmp = (InetSocketAddress) address;

    SecurityManager s = System.getSecurityManager ();
    if (s != null)
      s.checkListen(tmp.getPort ());

    impl.bind (tmp.getPort (), tmp.getAddress ());
  }
  
  /**
   * Closes the datagram socket
   */
  public void close()
  {
    impl.close();
  }

  /**
   * Checks if the datagram socket is closed
   * 
   * @since 1.4
   */
  public boolean isClosed()
  {
    return !impl.getFileDescriptor().valid();
  }

  /**
   * Gets a datagram channel assoziated with the socket
   * 
   * @since 1.4
   */
  public DatagramChannel getChannel()
  {
    return ch;
  }

  /**
   * Returns the local address of the datagram socket
   * 
   * @since 1.1
   */
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

  /**
   * Returns the local port this socket uses
   *
   * @return The local port number
   */
  public int getLocalPort()
  {
    return impl.getLocalPort();
  }

  /**
   * Gets the SO_TIMEOUT value
   *
   * @return The current timeout in milliseconds
   *
   * @exception SocketException If an error occurs
   * 
   * @since 1.1
   */
  public synchronized int getSoTimeout() throws SocketException
  {
    if (impl == null)
      throw new SocketException ("Cannot initialize Socket implementation");

    Object timeout = impl.getOption(SocketOptions.SO_TIMEOUT);
    if (timeout instanceof Integer) 
      return ((Integer)timeout).intValue();
    else
      return 0;
  }

  /**
   * Receive a datagram packet
   *
   * @param p The datagram packet to put the incoming data into
   * 
   * @exception IOException If an error occurs
   * @exception SocketTimeoutException If setSoTimeout was previously called
   * and the timeout has expired
   * @exception PortUnreachableException If the socket is connected to a
   * currently unreachable destination. Note, there is no guarantee that the
   * exception will be thrown
   * @exception IllegalBlockingModeException If this socket has an associated
   * channel, and the channel is in non-blocking mode
   */
  public synchronized void receive(DatagramPacket p) throws IOException
  {
    SecurityManager s = System.getSecurityManager();
    if (s != null)
      s.checkAccept (p.getAddress().getHostName (), p.getPort ());
		 
    if (impl == null)
      throw new IOException ("Cannot initialize Socket implementation");

    if (ch != null && !ch.isBlocking ())
      throw new IllegalBlockingModeException ();

    impl.receive(p);
  }

  /**
   * Sends a datagram packet
   *
   * @param p The datagram packet to send
   *
   * @exception IOException If an error occurs
   * @exception SecurityException If a security manager exists and its
   * checkMulticast or checkConnect method doesn't allow the send
   * @exception PortUnreachableException If the socket is connected to a
   * currently unreachable destination. Note, there is no guarantee that the
   * exception will be thrown
   * @exception IllegalBlockingModeException If this socket has an associated
   * channel, and the channel is in non-blocking mode
   */
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
	    
    // FIXME: if this is a subclass of MulticastSocket,
    // use getTimeToLive for TTL val.

    if (ch != null && !ch.isBlocking ())
      throw new IllegalBlockingModeException ();

    impl.send(p);
  }

  /**
   * Sets a new value for SO_TIMEOUT
   *
   * @param timeout The timeout in milliseconds
   *
   * @exception SocketException If an error occurs
   *
   * @since 1.1
   */
  public synchronized void setSoTimeout(int timeout) throws SocketException
  {
    if (timeout < 0)
      throw new IllegalArgumentException("Invalid timeout: " + timeout);

    impl.setOption(SocketOptions.SO_TIMEOUT, new Integer(timeout));
  }

  /**
   * Connects the datagrem socket to a specified address/port
   *
   * @param address The address to connect to
   * @param port The port to connect to
   *
   * @exception SocketException If an error occurs
   * @exception IllegalArgumentException If address is null
   * or the port number is illegal
   * @exception SecurityException If the caller is not allowed to send
   * datagrams to and receive datagrams from the address and port
   *
   * @since 1.2
   */
  public void connect(InetAddress address, int port)
    throws SocketException
  {
    //impl.connect(address, port);
  }

  /**
   * Connects the datagram socket to a specified socket address.
   *
   * @param address The socket address to connect to
   *
   * @exception SocketException If an error occurs
   * @exception IllegalArgumentException If address type is not supported
   *
   * @since 1.4
   */
  public void connect (SocketAddress address) throws SocketException
  {
    if ( !(address instanceof InetSocketAddress) )
      throw new IllegalArgumentException (
		      "SocketAddress is not InetSocketAddress");

    InetSocketAddress tmp = (InetSocketAddress) address;
    connect( tmp.getAddress(), tmp.getPort());
  }
  
  /**
   * Disconnects the datagram socket
   *
   * @since 1.2
   */
  public void disconnect()
  {
    impl.disconnect();
  }

  /**
   * Returns the binding state of the socket
   * 
   * @since 1.4
   */
  public boolean isBound()
  {
    try
      {
        Object bindaddr = impl.getOption (SocketOptions.SO_BINDADDR);
      }
    catch (SocketException e)
      {
        return false;
      }

    return true;
  }

  /**
   * Returns the InetAddress the socket is connected to
   * or null if the socket is not connected
   * 
   * @since 1.2
   */
  public InetAddress getInetAddress()
  {
    // FIXME:
    return null;
  }

  /**
   * Returns the local port number of the socket
   * 
   * @since 1.2
   */
  public int getPort()
  {
    return impl.localPort;
  }

  /**
   * Returns the local SocketAddress this socket is bound to
   * or null if it is not bound
   * 
   * @since 1.4
   */
  public SocketAddress getLocalSocketAddress()
  {
    InetAddress addr;
    
    try
      {
        addr = (InetAddress) impl.getOption (SocketOptions.SO_BINDADDR);
      }
    catch (SocketException e)
      {
        return null;
      }

    return new InetSocketAddress (addr, impl.localPort);
  }

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
    if (impl == null)
      throw new SocketException ("Cannot initialize Socket implementation");

    Object obj = impl.getOption(SocketOptions.SO_RCVBUF);
  
    if (obj instanceof Integer)
      return(((Integer)obj).intValue());
    else 
      throw new SocketException("Unexpected type");
  }

  /**
   * Enables/Disables SO_REUSEADDR
   * 
   * @param on Whether or not to have SO_REUSEADDR turned on
   *
   * @exception SocketException If an error occurs
   *
   * @since 1.4
   */
  public void setReuseAddress(boolean on) throws SocketException
  {
    if (impl == null)
      throw new SocketException ("Cannot initialize Socket implementation");

    impl.setOption (SocketOptions.SO_REUSEADDR, new Boolean (on));
  }

  /**
   * Checks if SO_REUSEADDR is enabled
   *
   * @exception SocketException If an error occurs
   * 
   * @since 1.4
   */
  public boolean getReuseAddress() throws SocketException
  {
    if (impl == null)
      throw new SocketException ("Cannot initialize Socket implementation");

    Object obj = impl.getOption (SocketOptions.SO_REUSEADDR);
  
    if (obj instanceof Boolean)
      return(((Boolean) obj).booleanValue ());
    else 
      throw new SocketException ("Unexpected type");
  }

  /**
   * Enables/Disables SO_BROADCAST
   * 
   * @param on Whether or not to have SO_BROADCAST turned on
   *
   * @exception SocketException If an error occurs
   *
   * @since 1.4
   */
  public void setBroadcast(boolean on) throws SocketException
  {
    if (impl == null)
      throw new SocketException ("Cannot initialize Socket implementation");

    impl.setOption (SocketOptions.SO_BROADCAST, new Boolean (on));
  }

  /**
   * Checks if SO_BROADCAST is enabled
   * 
   * @exception SocketException If an error occurs
   * 
   * @since 1.4
   */
  public boolean getBroadcast() throws SocketException
  {
    if (impl == null)
      throw new SocketException ("Cannot initialize Socket implementation");

    Object obj = impl.getOption (SocketOptions.SO_BROADCAST);
  
    if (obj instanceof Boolean)
      return ((Boolean) obj).booleanValue ();
    else 
      throw new SocketException ("Unexpected type");
  }

  /**
   * Sets the traffic class value
   *
   * @param tc The traffic class
   *
   * @exception SocketException If an error occurs
   * @exception IllegalArgumentException If tc value is illegal
   *
   * @see DatagramSocket:getTrafficClass
   * 
   * @since 1.4
   */
  public void setTrafficClass(int tc)
    throws SocketException
  {
    if (impl == null)
      throw new SocketException ("Cannot initialize Socket implementation");

    if (tc < 0 || tc > 255)
      throw new IllegalArgumentException();

    impl.setOption (SocketOptions.IP_TOS, new Integer (tc));
  }
  
  /**
   * Returns the current traffic class
   * 
   * @see DatagramSocket:setTrafficClass
   *
   * @exception SocketException If an error occurs
   * 
   * @since 1.4
   */
  public int getTrafficClass() throws SocketException
  {
    if (impl == null)
      throw new SocketException( "Cannot initialize Socket implementation");

    Object obj = impl.getOption(SocketOptions.IP_TOS);

    if (obj instanceof Integer)
      return ((Integer) obj).intValue ();
    else
      throw new SocketException ("Unexpected type");
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
    if (impl == null)
      throw new SocketException ("Cannot initialize Socket implementation");

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
   * @exception IllegalArgumentException If size is 0 or negative
   *  
   * @since 1.2
   */
  public void setReceiveBufferSize(int size) throws SocketException
  {
    if (impl == null)
      throw new SocketException ("Cannot initialize Socket implementation");

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
   * @exception IllegalArgumentException If size is 0 or negative
   *
   * @since 1.2
   */
  public void setSendBufferSize(int size) throws SocketException
  {
    if (size < 0)
      throw new IllegalArgumentException("Buffer size is less than 0");
  
    impl.setOption(SocketOptions.SO_SNDBUF, new Integer(size));
  }

  /**
   * Sets the datagram socket implementation factory for the application
   *
   * @param fac The factory to set
   *
   * @exception IOException If an error occurs
   * @exception SocketException If the factory is already defined
   * @exception SecurityException If a security manager exists and its
   * checkSetFactory method doesn't allow the operation
   */
  public static void setDatagramSocketImplFactory
    (DatagramSocketImplFactory fac) throws IOException
  {
    if (factory != null)
      throw new SocketException ("DatagramSocketImplFactory already defined");

    SecurityManager sm = System.getSecurityManager();
    if (sm != null)
      sm.checkSetFactory();

    factory = fac;
  }
}
