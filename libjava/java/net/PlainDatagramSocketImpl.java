// PlainDatagramSocketImpl.java - Implementation of DatagramSocketImpl.

/* Copyright (C) 1999, 2002  Free Software Foundation

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

class PlainDatagramSocketImpl extends DatagramSocketImpl
{
  // These fields are mirrored for use in native code to avoid cpp conflicts
  // when the #defines in system header files are the same as the public fields.
  static final int _Jv_TCP_NODELAY_ = SocketOptions.TCP_NODELAY,
                   _Jv_SO_BINDADDR_ = SocketOptions.SO_BINDADDR,
                   _Jv_SO_REUSEADDR_ = SocketOptions.SO_REUSEADDR,
                   _Jv_SO_BROADCAST_ = SocketOptions.SO_BROADCAST,
                   _Jv_SO_OOBINLINE_ = SocketOptions.SO_OOBINLINE,
                   _Jv_IP_MULTICAST_IF_ = SocketOptions.IP_MULTICAST_IF,
                   _Jv_IP_MULTICAST_IF2_ = SocketOptions.IP_MULTICAST_IF2,
                   _Jv_IP_MULTICAST_LOOP_ = SocketOptions.IP_MULTICAST_LOOP,
                   _Jv_IP_TOS_ = SocketOptions.IP_TOS,
                   _Jv_SO_LINGER_ = SocketOptions.SO_LINGER,
                   _Jv_SO_TIMEOUT_ = SocketOptions.SO_TIMEOUT,
                   _Jv_SO_SNDBUF_ = SocketOptions.SO_SNDBUF,
                   _Jv_SO_RCVBUF_ = SocketOptions.SO_RCVBUF,
                   _Jv_SO_KEEPALIVE_ = SocketOptions.SO_KEEPALIVE;

  int fnum = -1;

  // FIXME: Is this necessary?  Could it help w/ DatagramSocket.getLocalAddress?
  // InetAddress address;
  
  // localAddress cache  
  InetAddress localAddress;

  // 'timeout' is set/read by setOption/getOption.
  int timeout = 0;

  // FIXME: Probably should have bind (and create?) calls from DatagramSocket
  // constuctor.  If so, then same change should be made to the corresponding
  // Socket (non-datagram) classes.  This allows the implementation more
  // complete control over how the socket is set up and used (e.g. connect,
  // setting options, etc.).
  public PlainDatagramSocketImpl()
  {
  }

  protected native void bind(int lport, InetAddress laddr)
	throws SocketException;
  protected native void connect (InetAddress i, int port)
	throws SocketException;
  protected native void disconnect ();
  protected native void create() throws SocketException;
  protected native int peek(InetAddress i) throws IOException;
  protected native int peekData (DatagramPacket dp) throws IOException;
  protected native void setTimeToLive(int ttl) throws IOException;
  protected native int getTimeToLive() throws IOException;
  protected native void send(DatagramPacket p) throws IOException;
  protected native void receive(DatagramPacket p) throws IOException;
  public native void setOption(int optID, Object value) throws SocketException;
  public native Object getOption(int optID) throws SocketException;
  private native void mcastGrp(InetAddress inetaddr, NetworkInterface netIf,
		               boolean join) throws IOException;
  protected native void close();

  // Deprecated in JDK 1.2.
  protected byte getTTL() throws IOException
  {
    return (byte) getTimeToLive();
  }

  // Deprecated in JDK 1.2.
  protected void setTTL(byte ttl) throws IOException
  {
    setTimeToLive(((int) ttl) & 0xFF);
  }

  protected void join(InetAddress inetaddr) throws IOException
  {
    mcastGrp(inetaddr, null, true);
  }

  protected void leave(InetAddress inetaddr) throws IOException
  {
    mcastGrp(inetaddr, null, false);
  }

  protected void joinGroup (SocketAddress mcastaddr, NetworkInterface netIf)
	  throws IOException
  {
    mcastGrp(((InetSocketAddress)mcastaddr).getAddress(), netIf, true);
  }

  protected void leaveGroup (SocketAddress mcastaddr, NetworkInterface netIf)
	  throws IOException
  {
    mcastGrp(((InetSocketAddress)mcastaddr).getAddress(), netIf, false);
  }

  protected void finalize() throws Throwable
  {
    synchronized (this)
      {
	if (fnum != -1)
	  close();
      }
    super.finalize();
  }
}
