/* Base64InputStream.java -- base-64 input stream.
   Copyright (C) 2003, 2004 Free Software Foundation, Inc.

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


package gnu.java.io;

import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.FilterInputStream;
import java.io.IOException;
import java.io.InputStream;

/**
 * A filter input stream that decodes data encoded in the Base-64
 * encoding scheme.
 *
 * @author Casey Marshall (rsdio@metastatic.org)
 */
public class Base64InputStream extends FilterInputStream
{

  // Constants and fields.
  // ------------------------------------------------------------------------

  /** Base-64 digits. */
  private static final String BASE_64 = 
    "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/";

  /** Base-64 padding character. */
  private static final char BASE_64_PAD = '=';

  /** Decoding state. */
  private int state;

  /** Intermediate decoded value. */
  private int temp;

  /** EOF flag. */
  private boolean eof;

  private final byte[] one = new byte[1];

  // Constructors.
  // ------------------------------------------------------------------------

  /**
   * Create a new Base-64 input stream. The input bytes must be the
   * ASCII characters A-Z, a-z, 0-9, + and /, with optional whitespace,
   * and will be decoded into a byte stream.
   *
   * @param in The source of Base-64 input.
   */
  public Base64InputStream(InputStream in)
  {
    super(in);
    state = 0;
    temp = 0;
    eof = false;
  }

  // Class method.
  // ------------------------------------------------------------------------

  /**
   * Decode a single Base-64 string to a byte array.
   *
   * @param base64 The Base-64 encoded data.
   * @return The decoded bytes.
   * @throws IOException If the given data do not compose a valid Base-64
   *  sequence.
   */
  public static byte[] decode(String base64) throws IOException
  {
    Base64InputStream in =
      new Base64InputStream(new ByteArrayInputStream(base64.getBytes()));
    ByteArrayOutputStream out =
      new ByteArrayOutputStream((int) (base64.length() / 0.666));
    byte[] buf = new byte[1024];
    int len;
    while ((len = in.read(buf)) != -1)
      out.write(buf, 0, len);
    return out.toByteArray();
  }

  // Instance methods.
  // ------------------------------------------------------------------------

  public int available()
  {
    return 0;
  }

  public int read() throws IOException
  {
    if (read(one) == 1)
      return one[0];
    return -1;
  }

  public int read(byte[] buf, int off, int len) throws IOException
  {
    if (eof)
      return -1;
    int count = 0;
    while (count < len)
      {
        int i;
        while (Character.isWhitespace((char) (i = in.read())));
        int pos = BASE_64.indexOf((char) i);
        if (pos >= 0)
          {
            switch (state)
              {
                case 0:
                  temp = pos << 2;
                  state = 1;
                  break;
                case 1:
                  buf[count++] = (byte) (temp | (pos >>> 4));
                  temp = (pos & 0x0F) << 4;
                  state = 2;
                  break;
                case 2:
                  buf[count++] = (byte) (temp | (pos >>> 2));
                  temp = (pos & 0x03) << 6;
                  state = 3;
                  break;
                case 3:
                  buf[count++] = (byte) (temp | pos);
                  state = 0;
                  break;
              }
          }
        else if (i == BASE_64_PAD)
          {
            switch (state)
              {
                case 0:
                case 1:
                  throw new IOException("malformed Base-64 input");
                case 2:
                  while (Character.isWhitespace((char) (i = in.read())));
                  if (i != BASE_64_PAD)
                    throw new IOException("malformed Base-64 input");
                case 3:
                  while (Character.isWhitespace((char) (i = in.read())));
              }
            eof = true;
            break;
          }
        else  // First non-Base-64 character, consider it end-of-stream.
          {
            if (state != 0)
              throw new IOException("malformed Base-64 input");
            eof = true;
            break;
          }
      }
      return count;
  }

  public boolean markSupported()
  {
    return false;
  }

  public void mark(int markLimit) { }

  public void reset() throws IOException
  {
    throw new IOException("reset not supported");
  }

  public long skip(long n) throws IOException
  {
    long skipped;
    for (skipped = 0; skipped < n; skipped++)
      if (read() == -1)
        break;
    return skipped;
  }
}
