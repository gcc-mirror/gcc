/* UTF_16Decoder.java -- 
   Copyright (C) 2002 Free Software Foundation, Inc.

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

package gnu.java.nio.charset;

import java.nio.ByteBuffer;
import java.nio.CharBuffer;
import java.nio.charset.Charset;
import java.nio.charset.CharsetDecoder;
import java.nio.charset.CoderResult;

/**
 * Decoder for UTF-16, UTF-15LE, and UTF-16BE.
 *
 * @author Jesse Rosenstock
 */
final class UTF_16Decoder extends CharsetDecoder
{
  // byte orders
  static final int BIG_ENDIAN = 0;
  static final int LITTLE_ENDIAN = 1;
  static final int UNKNOWN_ENDIAN = 2;

  private static final char BYTE_ORDER_MARK = 0xFEFF;
  private static final char REVERSED_BYTE_ORDER_MARK = 0xFFFE;

  private final int originalByteOrder;
  private int byteOrder;

  UTF_16Decoder (Charset cs, int byteOrder)
  {
    super (cs, 0.5f, 1.0f);
    this.originalByteOrder = byteOrder;
    this.byteOrder = byteOrder;
  }

  protected CoderResult decodeLoop (ByteBuffer in, CharBuffer out)
  {
    // TODO: Optimize this in the case in.hasArray() / out.hasArray()

    int inPos = in.position ();
    try
      {
        while (in.remaining () >= 2)
          {
            byte b1 = in.get ();
            byte b2 = in.get ();

            // handle byte order mark
            if (byteOrder == UNKNOWN_ENDIAN)
              {
                char c = (char) (((b1 & 0xFF) << 8) | (b2 & 0xFF));
                if (c == BYTE_ORDER_MARK)
                  {
                    byteOrder = BIG_ENDIAN;
                    inPos += 2;
                    continue;
                  }
                else if (c == REVERSED_BYTE_ORDER_MARK)
                  {
                    byteOrder = LITTLE_ENDIAN;
                    inPos += 2;
                    continue;
                  }
                else
                  {
                    // assume big endian, do not consume bytes,
                    // continue with normal processing
                    byteOrder = BIG_ENDIAN;
                  }
              }

	    // FIXME: Change so you only do a single comparison here.
            char c = byteOrder == BIG_ENDIAN ? (char) ((b1 << 8) | b2)
                                             : (char) ((b2 << 8) | b1);

            if (0xD800 <= c && c <= 0xDFFF)
              {
                // c is a surrogate
                
                // make sure c is a high surrogate
                if (c > 0xDBFF)
                  return CoderResult.malformedForLength (2);
                if (in.remaining () < 2)
                  return CoderResult.UNDERFLOW;
                byte b3 = in.get ();
                byte b4 = in.get ();
                char d = byteOrder == BIG_ENDIAN ? (char) ((b3 << 8) | b4)
                                                 : (char) ((b4 << 8) | b3);
                // make sure d is a low surrogate
                if (d < 0xDC00 || d > 0xDFFF)
                  return CoderResult.malformedForLength (2);
                out.put (c);
                out.put (d);
                inPos += 4;
              }
            else
              {
                if (!out.hasRemaining ())
                  return CoderResult.UNDERFLOW;
                out.put (c);
                inPos += 2;
              }
          }

        return CoderResult.UNDERFLOW;
      }
    finally
      {
        in.position (inPos);
      }
  }

  protected void implReset ()
  {
    byteOrder = originalByteOrder;
  }
}
