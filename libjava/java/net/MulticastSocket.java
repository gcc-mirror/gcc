// MulticastSocket.java

/* Copyright (C) 1999  Red Hat, Inc.

   This file is part of libgcj.

This software is copyrighted work licensed under the terms of the
Libgcj License.  Please consult the file "LIBGCJ_LICENSE" for
details.  */

package java.net;
import java.io.IOException;

/**
 * @author Warren Levy <warrenl@cygnus.com>
 * @date May 18, 1999.
 */

/**
 * Written using on-line Java Platform 1.2 API Specification, as well
 * as "The Java Class Libraries", 2nd edition (Addison-Wesley, 1998).
 * Status:  Believed complete and correct.
 */

public class MulticastSocket extends DatagramSocket
{
  // FIXME: the local addr bound to the multicast socket can be reused;
  // unlike unicast sockets.  It binds to any available network interface.
  // See p.1159 JCL book.

  public MulticastSocket() throws IOException
  {
    super(0, null);
  }

  public MulticastSocket(int port) throws IOException
  {
    super(port, null);
  }

  public InetAddress getInterface() throws SocketException
  {
    // FIXME: Is it possible that an InetAddress wasn't returned from getOption?
    return (InetAddress) impl.getOption(SocketOptions.IP_MULTICAST_IF);
  }

  // Deprecated in JDK1.2
  public byte getTTL() throws IOException
  {
    // Use getTTL here rather than getTimeToLive in case we're using an impl
    // other than the default PlainDatagramSocketImpl and it doesn't have
    // getTimeToLive yet.
    return impl.getTTL();
  }

  // JDK1.2
  public int getTimeToLive() throws IOException
  {
    return impl.getTimeToLive();
  }

  public void setInterface(InetAddress inf) throws SocketException
  {
    impl.setOption(SocketOptions.IP_MULTICAST_IF, inf);
  }

  // Deprecated in JDK1.2
  public void setTTL(byte ttl) throws IOException
  {
    // Use setTTL here rather than setTimeToLive in case we're using an impl
    // other than the default PlainDatagramSocketImpl and it doesn't have
    // setTimeToLive yet.
    impl.setTTL(ttl);
  }

  // JDK1.2
  public void setTimeToLive(int ttl) throws IOException
  {
    if (ttl <= 0 || ttl > 255)
      throw new IllegalArgumentException("Invalid ttl: " + ttl);

    impl.setTimeToLive(ttl);
  }

  public void joinGroup(InetAddress mcastaddr) throws IOException
  {
    // FIXME: We can't currently rely on NullPointerException being
    // thrown when we invoke a method on a null object.
    if (mcastaddr == null)
      throw new NullPointerException("Null address");
    if (! mcastaddr.isMulticastAddress())
      throw new IOException("Not a Multicast address");

    SecurityManager s = System.getSecurityManager();
    if (s != null)
      s.checkMulticast(mcastaddr);

    impl.join(mcastaddr);
  }

  public void leaveGroup(InetAddress mcastaddr) throws IOException
  {
    // FIXME: We can't currently rely on NullPointerException being
    // thrown when we invoke a method on a null object.
    if (mcastaddr == null)
      throw new NullPointerException("Null address");
    if (! mcastaddr.isMulticastAddress())
      throw new IOException("Not a Multicast address");

    SecurityManager s = System.getSecurityManager();
    if (s != null)
      s.checkMulticast(mcastaddr);

    impl.leave(mcastaddr);
  }

  public synchronized void send(DatagramPacket p, byte ttl) throws IOException
  {
    SecurityManager s = System.getSecurityManager();
    if (s != null)
      {
	InetAddress addr = p.getAddress();
	if (addr.isMulticastAddress())
	  s.checkMulticast(addr, ttl);
	else
	  s.checkConnect(addr.getHostAddress(), p.getPort());
      }

    int oldttl = impl.getTimeToLive();
    impl.setTimeToLive(((int) ttl) & 0xFF);
    impl.send(p);
    impl.setTimeToLive(oldttl);
  }
}
