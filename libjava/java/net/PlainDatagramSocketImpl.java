// PlainDatagramSocketImpl.java - Implementation of DatagramSocketImpl.

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

class PlainDatagramSocketImpl extends DatagramSocketImpl
{
  int fnum = -1;
  InetAddress address;	// TBD: DatagramSocket.getLocalAddress()?

  // FIXME: Probably should have bind (and create?) calls from DatagramSocket
  // constuctor.  If so, then same change should be made to the corresponding
  // Socket (non-datagram) classes.  This allows the implementation more
  // compleete control over how the socket is set up and used (e.g. connect,
  // setting options, etc.).
  public PlainDatagramSocketImpl()
  {
  }

  protected native void bind(int lport, InetAddress laddr)
	throws SocketException;
  protected native void create() throws SocketException;
  protected native int peek(InetAddress i) throws IOException;
  protected native void setTTL(byte ttl) throws IOException;
  protected native byte getTTL() throws IOException;
  protected native void setTimeToLive(int ttl) throws IOException;
  protected native int getTimeToLive() throws IOException;
  protected native void join(InetAddress inetaddr) throws IOException;
  protected native void leave(InetAddress inetaddr) throws IOException;
  protected native void send(DatagramPacket p) throws IOException;
  protected native void receive(DatagramPacket p) throws IOException;

  protected void close() throws IOException
  {
    fd.close();
  }
}
