/* LimitedLengthInputStream.java --
   Copyright (C) 2005, 2008 Free Software Foundation, Inc.

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
Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
02110-1301 USA.

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


package gnu.java.net.protocol.http;

import java.io.IOException;
import java.io.InputStream;

/**
 * InputStream that limits the total number of bytes that can be read
 * from an underlying stream.  In addition to limiting the number of
 * bytes read, close() is not propagated to the underlying stream.
 *
 * @author David Daney (ddaney@avtrex.com)
 */
class LimitedLengthInputStream
  extends InputStream
{
  private long remainingLen;
  private boolean restrictLen;
  private HTTPConnection connection;
  private boolean eof;
  private InputStream in;
  private boolean doClose;

  private void handleClose()
    throws IOException
  {
    eof = true;

    if (doClose)
      in.close();
    else
      connection.release();

    in = null;
    connection = null;
  }

  /**
   * Constructor.
   *
   * @param in the underlying stream
   *
   * @param maxLen the maximum number of bytes to read
   *
   * @param restrictLen if true the number of bytes that can be read
   * from this stream will be limited to maxLen, otherwise the number
   * of bytes is not restricted.
   *
   * @param con the HTTPConnection associated with this stream
   *
   * @param doClose if true con will be closed when finished reading,
   * else it will be placed back in the connection pool.
   *
   */
  LimitedLengthInputStream(InputStream in,
                           long maxLen,
                           boolean restrictLen,
                           HTTPConnection con,
                           boolean doClose)
    throws IOException
  {
    this.in = in;
    this.remainingLen = maxLen;
    this.restrictLen = restrictLen;
    this.connection = con;
    this.doClose = doClose;

    if (restrictLen)
      {
        if (maxLen < 0)
          throw new IllegalArgumentException();
        else if (maxLen == 0)
          handleClose(); // Nothing to do, release the connection.
      }
  }

  public synchronized int read()
    throws IOException
  {
    if (eof)
      return -1; // EOF

    int r;

    if (restrictLen)
      {
        r = in.read();
        if (-1 != r)
          remainingLen--;

        if (0 == remainingLen)
          handleClose();
      }
    else
      {
        r = in.read();
        if (r == -1)
          handleClose();
      }

    return r;
  }

  public int read(byte[] buffer)
    throws IOException
  {
    return read(buffer, 0, buffer.length);
  }

  public synchronized int read(byte[] buffer, int offset, int length)
    throws IOException
  {
    if (eof)
      return -1; // EOF

    if (restrictLen && length > remainingLen)
      length = (int) remainingLen;

    int r = in.read(buffer, offset, length);

    if (-1 == r)
      handleClose();

    if (restrictLen && r > 0)
      {
        remainingLen -= r;
        if (0 == remainingLen)
          handleClose();
      }
    return r;
  }

  public synchronized long skip(long n)
    throws IOException
  {

    if (eof)
      return 0;

    if (restrictLen && n > remainingLen)
      n = remainingLen;

    long r = in.skip(n);

    if (restrictLen)
      {
        remainingLen -= r;
        if (0 == remainingLen)
          handleClose();
      }
    return r;
  }

  public synchronized int available()
    throws IOException
  {
    if (eof)
      return 0;

    int a = in.available();
    if (restrictLen && a > remainingLen)
      a = (int)remainingLen;
    return a;
  }

  public synchronized void close()
    throws IOException
  {
    if (eof)
      return;

    // If we get to here, the stream was not fully read.  Just throw
    // it away.

    doClose = true;

    handleClose();
  }
}
