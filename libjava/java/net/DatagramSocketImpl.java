// DatagramSocketImpl.java - Abstract datagram socket implementation.

/* Copyright (C) 1999  Red Hat, Inc.

   This file is part of libgcj.

This software is copyrighted work licensed under the terms of the
Libgcj License.  Please consult the file "LIBGCJ_LICENSE" for
details.  */

package java.net;
import java.io.IOException;
import java.io.FileDescriptor;

/**
 * @author Warren Levy <warrenl@cygnus.com>
 * @date May 3, 1999.
 */

/**
 * Written using on-line Java Platform 1.2 API Specification, as well
 * as "The Java Class Libraries", 2nd edition (Addison-Wesley, 1998).
 * Status:  Believed complete and correct.
 */

public abstract class DatagramSocketImpl implements SocketOptions
{
  protected int localPort;
  protected FileDescriptor fd;

  public DatagramSocketImpl()
  {
  }

  protected abstract void bind(int lport, InetAddress laddr)
  	throws SocketException;
  protected abstract void close();
  protected abstract void create() throws SocketException;
  protected abstract int peek(InetAddress i) throws IOException;
  protected abstract void send(DatagramPacket p) throws IOException;
  protected abstract void receive(DatagramPacket p) throws IOException;
  protected abstract void setTTL(byte ttl) throws IOException;
  protected abstract byte getTTL() throws IOException;
  protected abstract void setTimeToLive(int ttl) throws IOException;
  protected abstract int getTimeToLive() throws IOException;
  protected abstract void join(InetAddress inetaddr) throws IOException;
  protected abstract void leave(InetAddress inetaddr) throws IOException;

  public abstract Object getOption(int optID) throws SocketException;
  public abstract void setOption(int optID, Object value)
    throws SocketException;

  protected FileDescriptor getFileDescriptor()
  {
    return fd;
  }

  protected int getLocalPort()
  {
    return localPort;
  }
}
