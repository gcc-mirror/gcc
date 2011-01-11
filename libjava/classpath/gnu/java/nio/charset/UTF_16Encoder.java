/* UTF_16Encoder.java --
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
import java.nio.ByteOrder;
import java.nio.CharBuffer;
import java.nio.charset.Charset;
import java.nio.charset.CharsetEncoder;
import java.nio.charset.CoderResult;

/**
 * Encoder for UTF-16, UTF-15LE, and UTF-16BE.
 *
 * @author Jesse Rosenstock
 */
final class UTF_16Encoder extends CharsetEncoder
{
  // byte orders
  static final int BIG_ENDIAN = 0;
  static final int LITTLE_ENDIAN = 1;

  private static final char BYTE_ORDER_MARK = 0xFEFF;

  private final ByteOrder byteOrder;
  private final boolean useByteOrderMark;
  private boolean needsByteOrderMark;

  UTF_16Encoder (Charset cs, int byteOrder, boolean useByteOrderMark)
  {
    super (cs, 2.0f,
           useByteOrderMark ? 4.0f : 2.0f,
           byteOrder == BIG_ENDIAN
             ? new byte[] { (byte) 0xFF, (byte) 0xFD }
             : new byte[] { (byte) 0xFD, (byte) 0xFF });
    this.byteOrder = (byteOrder == BIG_ENDIAN) ?
        ByteOrder.BIG_ENDIAN : ByteOrder.LITTLE_ENDIAN;
    this.useByteOrderMark = useByteOrderMark;
    this.needsByteOrderMark = useByteOrderMark;
  }

  protected CoderResult encodeLoop (CharBuffer in, ByteBuffer out)
  {
    // TODO: Optimize this in the case in.hasArray() / out.hasArray()

    ByteOrder originalBO = out.order();
    out.order(byteOrder);

    if (needsByteOrderMark)
      {
        if (out.remaining () < 2)
            {
                out.order(originalBO);
                return CoderResult.OVERFLOW;
            }
        out.putChar (BYTE_ORDER_MARK);
        needsByteOrderMark = false;
      }

    int inPos = in.position ();
    try
      {
        while (in.hasRemaining ())
          {
            char c = in.get ();
            if (0xD800 <= c && c <= 0xDFFF)
              {
                // c is a surrogate

                // make sure c is a high surrogate
                if (c > 0xDBFF)
                  return CoderResult.malformedForLength (1);
                if (in.remaining () < 1)
                  return CoderResult.UNDERFLOW;
                char d = in.get ();
                // make sure d is a low surrogate
                if (d < 0xDC00 || d > 0xDFFF)
                  return CoderResult.malformedForLength (1);
                out.putChar (c);
                out.putChar (d);
                inPos += 2;
              }
            else
              {
                if (out.remaining () < 2)
                  {
                    out.order(originalBO);
                    return CoderResult.OVERFLOW;
                  }
                out.putChar (c);
                inPos++;
              }
          }
        out.order(originalBO);
        return CoderResult.UNDERFLOW;
      }
    finally
      {
        in.position (inPos);
      }
  }

  protected void implReset ()
  {
    needsByteOrderMark = useByteOrderMark;
  }

  // TODO: override canEncode(char) and canEncode(CharSequence)
  // for performance
}
