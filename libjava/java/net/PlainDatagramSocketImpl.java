// PlainDatagramSocketImpl.java - Implementation of DatagramSocketImpl.

/* Copyright (C) 1999  Free Software Foundation

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
		   _Jv_IP_MULTICAST_IF_ = SocketOptions.IP_MULTICAST_IF,
                   _Jv_SO_LINGER_ = SocketOptions.SO_LINGER,
                   _Jv_SO_TIMEOUT_ = SocketOptions.SO_TIMEOUT,
                   _Jv_SO_SNDBUF_ = SocketOptions.SO_SNDBUF,
                   _Jv_SO_RCVBUF_ = SocketOptions.SO_RCVBUF;

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
  protected native void create() throws SocketException;
  protected native int peek(InetAddress i) throws IOException;
  protected native void setTimeToLive(int ttl) throws IOException;
  protected native int getTimeToLive() throws IOException;
  protected native void send(DatagramPacket p) throws IOException;
  protected native void receive(DatagramPacket p) throws IOException;
  public native void setOption(int optID, Object value) throws SocketException;
  public native Object getOption(int optID) throws SocketException;
  private native void mcastGrp(InetAddress inetaddr, boolean join)
	throws IOException;

  protected void close()
  {
    // FIXME: The close method in each of the DatagramSocket* classes does
    // not throw an IOException.  The issue is that FileDescriptor.close()
    // in natFileDescriptorPosix.cc can throw one, so we have to catch
    // it here.  It seems that FileDescriptor.close is properly throwing
    // the IOException on errors since many of the java.io classes depend
    // on that.  This probably requires a bit more research but for now,
    // we'll catch the IOException here.
    try
      {
        fd.close();
      }
    catch (IOException e)
      {
	System.err.println("PlainDatagramSocketImpl.close: Error closing - " +
	  e.getMessage());
      }
  }

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
    mcastGrp(inetaddr, true);
  }

  protected void leave(InetAddress inetaddr) throws IOException
  {
    mcastGrp(inetaddr, false);
  }
}
