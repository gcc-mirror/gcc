/* PlainDatagramSocketImpl.java -- Default DatagramSocket implementation
   Copyright (C) 1998, 1999, 2001 Free Software Foundation, Inc.

This file is part of GNU Classpath.

GNU Classpath is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2, or (at your option)
any later version.
 
GNU Classpath is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
General Public License for more details.

You should have received a copy of the GNU General Public License
along with GNU Classpath; see the file COPYING.  If not, write to the
Free Software Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA
02111-1307 USA.

Linking this library statically or dynamically with other modules is
making a combined work based on this library.  Thus, the terms and
conditions of the GNU General Public License cover the whole
combination.

As a special exception, the copyright holders of this library give you
permission to link this library with independent modules to produce an
executable, regardless of the license terms of these independent
modules, and to copy and distribute the resulting executable under
terms of your choice, provided that you also meet, for each linked
independent module, the terms and conditions of the license of that
module.  An independent module is a module which is not derived from
or based on this library.  If you modify this library, you may extend
this exception to your version of the library, but you are not
obligated to do so.  If you do not wish to do so, delete this
exception statement from your version. */


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
