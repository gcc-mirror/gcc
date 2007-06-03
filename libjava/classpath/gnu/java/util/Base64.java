/* Base64.java -- Base64 encoding and decoding.
   Copyright (C) 2006, 2007  Free Software Foundation, Inc.

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
exception statement from your version.

--
Base64 encoding derived from ISC's DHCP. Copyright notices from DHCP
follow. See http://www.isc.org/products/DHCP/.

Copyright (c) 1996 by Internet Software Consortium.

Permission to use, copy, modify, and distribute this software for any
purpose with or without fee is hereby granted, provided that the above
copyright notice and this permission notice appear in all copies.

THE SOFTWARE IS PROVIDED "AS IS" AND INTERNET SOFTWARE CONSORTIUM
DISCLAIMS ALL WARRANTIES WITH REGARD TO THIS SOFTWARE INCLUDING ALL
IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL
INTERNET SOFTWARE CONSORTIUM BE LIABLE FOR ANY SPECIAL, DIRECT,
INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES WHATSOEVER RESULTING
FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN ACTION OF CONTRACT,
NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF OR IN CONNECTION
WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.

--
Portions Copyright (c) 1995 by International Business Machines, Inc.

International Business Machines, Inc. (hereinafter called IBM) grants
permission under its copyrights to use, copy, modify, and distribute
this Software with or without fee, provided that the above copyright
notice and all paragraphs of this notice appear in all copies, and
that the name of IBM not be used in connection with the marketing of
any product incorporating the Software or modifications thereof,
without specific, written prior permission.

To the extent it has a right to do so, IBM grants an immunity from
suit under its patents, if any, for the use, sale or manufacture of
products to the extent that such products are used for performing
Domain Name System dynamic updates in TCP/IP networks by means of the
Software.  No immunity is granted for any product per se or for any
other function of any product.

THE SOFTWARE IS PROVIDED "AS IS", AND IBM DISCLAIMS ALL WARRANTIES,
INCLUDING ALL IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A
PARTICULAR PURPOSE.  IN NO EVENT SHALL IBM BE LIABLE FOR ANY SPECIAL,
DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES WHATSOEVER
ARISING OUT OF OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS
SOFTWARE, EVEN IF IBM IS APPRISED OF THE POSSIBILITY OF SUCH
DAMAGES.  */


package gnu.java.util;

import java.io.ByteArrayOutputStream;
import java.io.IOException;

public final class Base64
{

  // No constructor.
  private Base64() { }

  // Class methods.
  // -------------------------------------------------------------------------

  /** Base-64 characters. */
  private static final String BASE_64 =
    "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/";

  /** Base-64 padding character. */
  private static final char BASE_64_PAD = '=';

  /**
   * Base64 encode a byte array, with no line wrapping.
   *
   * @param buf The byte array to encode.
   * @return <tt>buf</tt> encoded in Base64.
   */
  public static String encode(byte[] buf)
  {
    return encode(buf, 0);
  }
  
  /**
   * Base64 encode a byte array, returning the returning string.
   *
   * @param buf The byte array to encode.
   * @param tw  The total length of any line, 0 for unlimited.
   * @return <tt>buf</tt> encoded in Base64.
   */
  public static String encode(byte[] buf, int tw)
  {
    return encode(buf, 0, buf.length, tw);
  }

  /**
   * Base64 encode a byte array, returning the returning string.
   * 
   * @param buf The byte array to encode.
   * @param offset The offset in the byte array to start.
   * @param length The number of bytes to encode.
   * @param tw The total length of any line, 0 for unlimited.
   * @return <tt>buf</tt> encoded in Base64.
   */
  public static String encode(byte[] buf, int offset, int length, int tw)
  {
    if (offset < 0 || length < 0 || offset + length > buf.length)
      throw new ArrayIndexOutOfBoundsException(buf.length  + " "
                                               + offset + " "
                                               + length);
    int srcLength = buf.length - offset;
    byte[] input = new byte[3];
    int[] output = new int[4];
    StringBuffer out = new StringBuffer();
    int i = offset;
    int chars = 0;

    while (srcLength > 2)
      {
        input[0] = buf[i++];
        input[1] = buf[i++];
        input[2] = buf[i++];
        srcLength -= 3;

        output[0] = (input[0] & 0xff) >>> 2;
        output[1] = ((input[0] & 0x03) << 4) + ((input[1] & 0xff) >>> 4);
        output[2] = ((input[1] & 0x0f) << 2) + ((input[2] & 0xff) >>> 6);
        output[3] = input[2] & 0x3f;

        out.append(BASE_64.charAt(output[0]));
        if (tw > 0 && ++chars % tw == 0)
          {
            out.append("\n");
          }
        out.append(BASE_64.charAt(output[1]));
        if (tw > 0 && ++chars % tw == 0)
          {
            out.append("\n");
          }
        out.append(BASE_64.charAt(output[2]));
        if (tw > 0 && ++chars % tw == 0)
          {
            out.append("\n");
          }
        out.append(BASE_64.charAt(output[3]));
        if (tw > 0 && ++chars % tw == 0)
          {
            out.append("\n");
          }
      }

    if (srcLength != 0)
      {
        input[0] = input[1] = input[2] = 0;
        for (int j = 0; j < srcLength; j++)
          {
            input[j] = buf[i+j];
          }
        output[0] = (input[0] & 0xff) >>> 2;
        output[1] = ((input[0] & 0x03) << 4) + ((input[1] & 0xff) >>> 4);
        output[2] = ((input[1] & 0x0f) << 2) + ((input[2] & 0xff) >>> 6);

        out.append(BASE_64.charAt(output[0]));
        if (tw > 0 && ++chars % tw == 0)
          {
            out.append("\n");
          }
        out.append(BASE_64.charAt(output[1]));
        if (tw > 0 && ++chars % tw == 0)
          {
            out.append("\n");
          }
        if (srcLength == 1)
          {
            out.append(BASE_64_PAD);
          }
        else
          {
            out.append(BASE_64.charAt(output[2]));
          }
        if (tw > 0 && ++chars % tw == 0)
          {
            out.append("\n");
          }
        out.append(BASE_64_PAD);
        if (tw > 0 && ++chars % tw == 0)
          {
            out.append("\n");
          }
      }
    if (tw > 0)
      {
        out.append("\n");
      }

    return out.toString();
  }

  /**
   * Decode a Base-64 string into a byte array.
   *
   * @param b64 The Base-64 encoded string.
   * @return The decoded bytes.
   * @throws java.io.IOException If the argument is not a valid Base-64
   *    encoding.
   */
  public static byte[] decode(String b64) throws IOException
  {
    ByteArrayOutputStream result = new ByteArrayOutputStream(b64.length() / 3);
    int state = 0, i;
    byte temp = 0;

    for (i = 0; i < b64.length(); i++)
      {
        if (Character.isWhitespace(b64.charAt(i)))
          {
            continue;
          }
        if (b64.charAt(i) == BASE_64_PAD)
          {
            break;
          }

        int pos = BASE_64.indexOf(b64.charAt(i));
        if (pos < 0)
          {
            throw new IOException("non-Base64 character " + b64.charAt(i));
          }
        switch (state)
          {
          case 0:
            temp = (byte) (pos - BASE_64.indexOf('A') << 2);
            state = 1;
            break;

          case 1:
            temp |= (byte) (pos - BASE_64.indexOf('A') >>> 4);
            result.write(temp);
            temp = (byte) ((pos - BASE_64.indexOf('A') & 0x0f) << 4);
            state = 2;
            break;

          case 2:
            temp |= (byte) ((pos - BASE_64.indexOf('A') & 0x7f) >>> 2);
            result.write(temp);
            temp = (byte) ((pos - BASE_64.indexOf('A') & 0x03) << 6);
            state = 3;
            break;

          case 3:
            temp |= (byte) (pos - BASE_64.indexOf('A') & 0xff);
            result.write(temp);
            state = 0;
            break;

          default:
            throw new Error("this statement should be unreachable");
          }
      }

    if (i < b64.length() && b64.charAt(i) == BASE_64_PAD)
      {
        switch (state)
          {
          case 0:
          case 1:
            throw new IOException("malformed Base64 sequence");

          case 2:
            i++;
            for ( ; i < b64.length(); i++)
              {
                if (!Character.isWhitespace(b64.charAt(i)))
                  {
                    break;
                  }
              }
            // We must see a second pad character here.
            if (b64.charAt(i) != BASE_64_PAD)
              {
                throw new IOException("malformed Base64 sequence");
              }
            i++;
            // Fall-through.

          case 3:
            i++;
            for ( ; i < b64.length(); i++)
              {
                // We should only see whitespace after this.
                if (!Character.isWhitespace(b64.charAt(i)))
                  {
                    throw new IOException("malformed Base64 sequence");
                  }
              }
          }
      }
    else
      {
        if (state != 0)
          {
            throw new IOException("malformed Base64 sequence");
          }
      }

    return result.toByteArray();
  }
}
