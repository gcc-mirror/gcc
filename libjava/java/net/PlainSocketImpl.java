// natClass.cc - Implementation of java.lang.Class native methods.

/* Copyright (C) 1998, 1999  Cygnus Solutions

   This file is part of libgcj.

This software is copyrighted work licensed under the terms of the
Libgcj License.  Please consult the file "LIBGCJ_LICENSE" for
details.  */

package java.net;
import java.io.*;

class PlainSocketImpl extends SocketImpl
{
  int fnum = -1;

  protected native void create (boolean stream)  throws IOException;

  protected void connect (String host, int port) throws IOException
  {
    connect(InetAddress.getByName(host), port);
  }

  protected native void connect (InetAddress host, int port)
    throws IOException;

  protected native void bind (InetAddress host, int port) throws IOException;

  protected native void listen (int backlog) throws IOException;

  private native void accept (PlainSocketImpl s) throws IOException;
  protected void accept (SocketImpl s) throws IOException
  {
    accept((PlainSocketImpl) s);
  }

  private InputStream in;
  private OutputStream out;

  protected InputStream getInputStream() throws IOException
  {
    if (in == null)
      in = new FileInputStream (fd);
    return in;
  }

  protected OutputStream getOutputStream() throws IOException
  {
    if (out == null)
      out = new FileOutputStream (fd);
    return out;
  }

  protected int available () throws IOException
  {
    return in.available();
  }

  protected void close () throws IOException
  {
    fd.close();
  }
}
