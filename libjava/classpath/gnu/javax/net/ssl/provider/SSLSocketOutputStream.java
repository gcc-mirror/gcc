/* SSLSocketOutputStream.java -- output stream for SSL sockets.
   Copyright (C) 2006  Free Software Foundation, Inc.

This file is a part of GNU Classpath.

GNU Classpath is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2 of the License, or (at
your option) any later version.

GNU Classpath is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
General Public License for more details.

You should have received a copy of the GNU General Public License
along with GNU Classpath; if not, write to the Free Software
Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA 02110-1301
USA

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
exception statement from your version.  */


package gnu.javax.net.ssl.provider;

import java.io.FilterOutputStream;
import java.io.IOException;
import java.io.OutputStream;
import javax.net.ssl.SSLException;

class SSLSocketOutputStream extends FilterOutputStream
{

  // Fields.
  // -------------------------------------------------------------------------

  private final SSLSocket socket;
  private final boolean checkHandshake;

  // Constructor.
  // -------------------------------------------------------------------------

  SSLSocketOutputStream(OutputStream out, SSLSocket socket)
  {
    this(out, socket, true);
  }

  SSLSocketOutputStream(OutputStream out, SSLSocket socket,
                        boolean checkHandshake)
  {
    super(out);
    this.socket = socket;
    this.checkHandshake = checkHandshake;
  }

  // Instance methods.
  // -------------------------------------------------------------------------

  public void write(int b) throws IOException
  {
    if (checkHandshake)
      {
        socket.checkHandshakeDone();
      }
    checkAlert();
    out.write(b);
    checkAlert();
  }

  public void write(byte[] buf) throws IOException
  {
    write(buf, 0, buf.length);
  }

  public void write(byte[] buf, int off, int len) throws IOException
  {
    if (checkHandshake)
      {
        socket.checkHandshakeDone();
      }
    if (buf == null)
      throw new NullPointerException();
    if (off < 0 || len < 0 || off + len > buf.length)
      throw new ArrayIndexOutOfBoundsException();
    checkAlert();
    out.write(buf, off, len);
    checkAlert();
  }

  // Own methods.
  // -------------------------------------------------------------------------

  private synchronized void checkAlert() throws SSLException
  {
    Alert alert = socket.checkAlert();
    if (alert == null) return;
    if (alert.getLevel().equals(Alert.Level.FATAL))
      throw new AlertException(alert, false);
  }
}
