// SocketImpl.java - Abstract socket implementation.

/* Copyright (C) 1999  Free Software Foundation

   This file is part of libgcj.

This software is copyrighted work licensed under the terms of the
Libgcj License.  Please consult the file "LIBGCJ_LICENSE" for
details.  */

package java.net;
import java.io.*;

/**
  * @author Per Bothner <bothner@cygnus.com>
  * @date January 6, 1999.
  */

/** Written using on-line Java Platform 1.2 API Specification.
  * Believed complete and correct.
  */

public abstract class SocketImpl implements SocketOptions
{
  protected InetAddress address;

  protected FileDescriptor fd;

  protected int localport;

  protected int port;

  public SocketImpl ()
  {
  }

  protected abstract void create (boolean stream) throws IOException;

  protected abstract void connect (String host, int port) throws IOException;

  protected abstract void connect (InetAddress host, int port)
    throws IOException;

  protected abstract void bind (InetAddress host, int port) throws IOException;

  protected abstract void listen (int backlog) throws IOException;

  protected abstract void accept (SocketImpl s) throws IOException;

  protected abstract InputStream getInputStream() throws IOException;

  protected abstract OutputStream getOutputStream() throws IOException;

  protected abstract int available () throws IOException;

  protected abstract void close () throws IOException;

  protected FileDescriptor getFileDescriptor () { return fd; }

  protected InetAddress getInetAddress () { return address; }

  protected int getPort () { return port; }

  protected int getLocalPort () { return localport; }

  public abstract Object getOption(int optID) throws SocketException;

  public abstract void setOption(int optID, Object value)
    throws SocketException;

  public String toString ()
  {
    return "[addr=" + address.toString() + ",port=" + Integer.toString(port) +
      ",localport=" + Integer.toString(localport) + "]";
  }
}
