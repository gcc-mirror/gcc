/* ChunkedInputStream.java --
   Copyright (C) 2004 Free Software Foundation, Inc.

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

import java.io.FilterInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.net.ProtocolException;

/**
 * Input stream wrapper for the "chunked" transfer-coding.
 *
 * @author Chris Burdess (dog@gnu.org)
 */
public class ChunkedInputStream
  extends FilterInputStream
{

  private static final byte CR = 0x0d;
  private static final byte LF = 0x0a;

  int size;
  int count;
  boolean meta;
  boolean eof;
  Headers headers;

  /**
   * Constructor.
   * @param in the response socket input stream
   * @param headers the headers to receive additional header lines
   */
  public ChunkedInputStream(InputStream in, Headers headers)
  {
    super(in);
    this.headers = headers;
    size = -1;
    count = 0;
    meta = true;
  }

  public int read()
    throws IOException
  {
    byte[] buf = new byte[1];
    int len = read(buf, 0, 1);
    if (len == -1)
      {
        return -1;
      }
    int ret = (int) buf[0];
    if (ret < 0)
      {
        ret += 0x100;
      }
    return ret;
  }

  public int read(byte[] buffer)
    throws IOException
  {
    return read(buffer, 0, buffer.length);
  }

  public int read(byte[] buffer, int offset, int length)
    throws IOException
  {
    if (eof)
      {
        return -1;
      }
    if (meta)
      {
        // Read chunk header
        int c, last = 0;
        boolean seenSemi = false;
        StringBuilder buf = new StringBuilder();
        do
          {
            c = in.read();
            if (c == 0x3b) // ;
              {
                seenSemi = true;
              }
            else if (c == 0x0a && last == 0x0d) // CRLF
              {
                size = Integer.parseInt(buf.toString(), 16);
                break;
              }
            else if (!seenSemi && c >= 0x30)
              {
                buf.append ((char) c);
              }
            last = c;
          }
        while(c != -1);
        count = 0;
        meta = false;
      }
    if (size == 0)
      {
        // Read trailer
        headers.parse(in);
        eof = true;
        return -1;
      }
    else
      {
        int diff = length - offset;
        int max = size - count;
        max = (diff < max) ? diff : max;
        int len = (max > 0) ? in.read(buffer, offset, max) : 0;
        count += len;
        if (count == size)
          {
            // Read CRLF
            int c1 = in.read();
            int c2 = in.read();
            if (c1 == -1 && c2 == -1)
              {
                // EOF before CRLF: bad, but ignore
                eof = true;
                return -1;
              }
            if (c1 != 0x0d || c2 != 0x0a)
              {
                throw new ProtocolException("expecting CRLF: " + c1 + "," + c2);
              }
            meta = true;
          }
        return len;
      }
  }
  
}

