// MulticastSocket.java

/* Copyright (C) 1999  Cygnus Solutions

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
  // unlike unicast sockets.  see p.1159 JCL book.

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
    // FIXME: TODO - MulticastSocket.getInterface
    throw new SocketException("MulticastSocket.getInterface - not yet implemented");
  }

  // Deprecated in JDK1.2
  public byte getTTL() throws IOException
  {
    return impl.getTTL();
  }

  // JDK1.2
  public int getTimeToLive() throws IOException
  {
    return impl.getTimeToLive();
  }

  public void setInterface(InetAddress inf) throws SocketException
  {
    // FIXME: TODO - MulticastSocket.setInterface
    throw new SocketException("MulticastSocket.setInterface - not yet implemented");
  }

  // Deprecated in JDK1.2
  public void setTTL(byte ttl) throws IOException
  {
    impl.setTTL(ttl);
  }

  // JDK1.2
  public void setTimeToLive(int ttl) throws IOException
  {
    impl.setTimeToLive(ttl);
  }

  public void joinGroup(InetAddress mcastaddr) throws IOException
  {
    impl.join(mcastaddr);
  }

  public void leaveGroup(InetAddress mcastaddr) throws IOException
  {
    impl.leave(mcastaddr);
  }

  public void send(DatagramPacket p, byte ttl) throws IOException
  {
    // FIXME:  use ttl instead of getTTL() for time to live.
    impl.send(p);
  }
}
