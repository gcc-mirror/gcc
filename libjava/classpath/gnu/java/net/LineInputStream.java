/* LineInputStream.java --
   Copyright (C) 2002, 2003, 2004, 2005  Free Software Foundation, Inc.

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


package gnu.java.net;

import java.io.BufferedInputStream;
import java.io.ByteArrayOutputStream;
import java.io.FilterInputStream;
import java.io.IOException;
import java.io.InputStream;

/**
 * An input stream that can read lines of input.
 *
 * @author Chris Burdess (dog@gnu.org)
 */
public class LineInputStream
  extends FilterInputStream
{
  /*
   * Line buffer.
   */
  private ByteArrayOutputStream buf;

  /*
   * Encoding to use when translating bytes to characters.
   */
  private String encoding;

  /*
   * End-of-stream flag.
   */
  private boolean eof;

  /**
   * Whether we can use block reads.
   */
  private final boolean blockReads;

  /**
   * Constructor using the US-ASCII character encoding.
   * @param in the underlying input stream
   */
  public LineInputStream(InputStream in)
  {
    this(in, "US-ASCII");
  }

  /**
   * Constructor.
   * @param in the underlying input stream
   * @param encoding the character encoding to use
   */
  public LineInputStream(InputStream in, String encoding)
  {
    super(in);
    buf = new ByteArrayOutputStream();
    this.encoding = encoding;
    eof = false;
    // If it is already buffered, additional buffering gains nothing.
    blockReads = !(in instanceof BufferedInputStream) && in.markSupported();
  }

  /**
   * Read a line of input.
   */
  public String readLine()
    throws IOException
  {
    if (eof)
      {
        return null;
      }
    do
      {
        if (blockReads)
          {
            // Use mark and reset to read chunks of bytes
            final int MAX_LENGTH = 1024;
            int len, pos;

            len = in.available();
            if (len == 0 || len > MAX_LENGTH)
              len = MAX_LENGTH;
            byte[] b = new byte[len];
            in.mark(len);
            // Read into buffer b
            len = in.read(b, 0, len);
            // Handle EOF
            if (len == -1)
              {
                eof = true;
                if (buf.size() == 0)
                  {
                    return null;
                  }
                else
                  {
                    // We don't care about resetting buf
                    return buf.toString(encoding);
                  }
              }
            // Get index of LF in b
            pos = indexOf(b, len, (byte) 0x0a);
            if (pos != -1)
              {
                // Write pos bytes to buf
                buf.write(b, 0, pos);
                // Reset stream, and read pos + 1 bytes
                in.reset();
                pos += 1;
                while (pos > 0)
                  {
                    len = in.read(b, 0, pos);
                    pos = (len == -1) ? -1 : pos - len;
                  }
                // Return line
                String ret = buf.toString(encoding);
                buf.reset();
                return ret;
              }
            else
              {
                // Append everything to buf and fall through to re-read.
                buf.write(b, 0, len);
              }
          }
        else
          {
            // We must use character reads in order not to read too much
            // from the underlying stream.
            int c = in.read();
            switch (c)
              {
              case -1:
                eof = true;
                if (buf.size() == 0)
                  {
                    return null;
                  }
                // Fall through and return contents of buffer.
              case 0x0a:                // LF
                String ret = buf.toString(encoding);
                buf.reset();
                return ret;
              default:
                buf.write(c);
              }
          }
      }
    while (true);
  }

  private int indexOf(byte[] b, int len, byte c)
  {
    for (int pos = 0; pos < len; pos++)
      {
        if (b[pos] == c)
          {
            return pos;
          }
      }
    return -1;
  }
}

