/* UTF_8.java -- 
   Copyright (C) 2002, 2004, 2005  Free Software Foundation, Inc.

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

package gnu.java.nio.charset;

import java.nio.ByteBuffer;
import java.nio.CharBuffer;
import java.nio.charset.Charset;
import java.nio.charset.CharsetDecoder;
import java.nio.charset.CharsetEncoder;
import java.nio.charset.CoderResult;

/**
 * UTF-8 charset.
 * 
 * <p> UTF-8 references:
 * <ul>
 *   <li> <a href="http://ietf.org/rfc/rfc2279.txt">RFC 2279</a>
 *   <li> The <a href="http://www.unicode.org/unicode/standard/standard.html">
 *     Unicode standard</a> and 
 *     <a href="http://www.unicode.org/versions/corrigendum1.html">
 *      Corrigendum</a>
 * </ul>
 *
 * @author Jesse Rosenstock
 */
final class UTF_8 extends Charset
{
  UTF_8 ()
  {
    super ("UTF-8", new String[] {
        /* These names are provided by
         * http://oss.software.ibm.com/cgi-bin/icu/convexp?s=ALL
         */
        "ibm-1208", "ibm-1209", "ibm-5304", "ibm-5305",
        "windows-65001", "cp1208",
        // see http://java.sun.com/j2se/1.5.0/docs/guide/intl/encoding.doc.html
        "UTF8"
    });
  }

  public boolean contains (Charset cs)
  {
    return cs instanceof US_ASCII || cs instanceof ISO_8859_1
      || cs instanceof UTF_8 || cs instanceof UTF_16BE
      || cs instanceof UTF_16LE || cs instanceof UTF_16;
  }

  public CharsetDecoder newDecoder ()
  {
    return new Decoder (this);
  }

  public CharsetEncoder newEncoder ()
  {
    return new Encoder (this);
  }

  private static final class Decoder extends CharsetDecoder
  {
    // Package-private to avoid a trampoline constructor.
    Decoder (Charset cs)
    {
      super (cs, 1.0f, 1.0f);
    }

    protected CoderResult decodeLoop (ByteBuffer in, CharBuffer out)
    {
      // TODO: Optimize this in the case in.hasArray() / out.hasArray()
      int inPos = in.position(); 
      try
        {
          while (in.hasRemaining ())
            {
              char c;
              byte b1 = in.get ();
              int highNibble = (b1 >> 4) & 0xF;

              switch (highNibble)
                {
                  case 0: case 1: case 2: case 3:
                  case 4: case 5: case 6: case 7:
                    if (out.remaining () < 1)
                      return CoderResult.OVERFLOW;
                    out.put ((char) b1);
                    inPos++;
                    break;

                  case 0xC: case 0xD:
                    byte b2;
                    if (in.remaining () < 1)
                      return CoderResult.UNDERFLOW;
                    if (out.remaining () < 1)
                      return CoderResult.OVERFLOW;
                    if (!isContinuation (b2 = in.get ()))
                      return CoderResult.malformedForLength (1);
                    c = (char) (((b1 & 0x1F) << 6) | (b2 & 0x3F));
                    // check that we had the shortest encoding
                    if (c <= 0x7F)
                      return CoderResult.malformedForLength (2);
                    out.put (c);
                    inPos += 2;
                    break;

                  case 0xE:
                    byte b3;
                    if (in.remaining () < 2)
                      return CoderResult.UNDERFLOW;
                    if (out.remaining () < 1)
                      return CoderResult.OVERFLOW;
                    if (!isContinuation (b2 = in.get ()))
                      return CoderResult.malformedForLength (1);
                    if (!isContinuation (b3 = in.get ()))
                      return CoderResult.malformedForLength (1);
                    c = (char) (((b1 & 0x0F) << 12)
                                | ((b2 & 0x3F) << 6)
                                | (b3 & 0x3F));
                    // check that we had the shortest encoding
                    if (c <= 0x7FF)
                      return CoderResult.malformedForLength (3);
                    out.put (c);
                    inPos += 3;
                    break;

                  default:
                    return CoderResult.malformedForLength (1);
                }
            }

          return CoderResult.UNDERFLOW;
        }
      finally
        {
          // In case we did a get(), then encountered an error, reset the
          // position to before the error.  If there was no error, this
          // will benignly reset the position to the value it already has.
          in.position (inPos);
        }
    }

    private static boolean isContinuation (byte b)
    {
      return (b & 0xC0) == 0x80;
    }
  }

  private static final class Encoder extends CharsetEncoder
  {
    // Package-private to avoid a trampoline constructor.
    Encoder (Charset cs)
    {
      // According to
      // http://www-106.ibm.com/developerworks/unicode/library/utfencodingforms/index.html
      //   On average, English takes slightly over one unit per code point.
      //   Most Latin-script languages take about 1.1 bytes. Greek, Russian,
      //   Arabic and Hebrew take about 1.7 bytes, and most others (including
      //   Japanese, Chinese, Korean and Hindi) take about 3 bytes.
      // We assume we will be dealing with latin scripts, and use 1.1 
      // for averageBytesPerChar.
      super (cs, 1.1f, 4.0f);
    }

    protected CoderResult encodeLoop (CharBuffer in, ByteBuffer out)
    {
      int inPos = in.position();
      try
        {
          // TODO: Optimize this in the case in.hasArray() / out.hasArray()
          while (in.hasRemaining ())
          {
            int remaining = out.remaining ();
            char c = in.get ();

            // UCS-4 range (hex.)           UTF-8 octet sequence (binary)
            // 0000 0000-0000 007F   0xxxxxxx
            // 0000 0080-0000 07FF   110xxxxx 10xxxxxx
            // 0000 0800-0000 FFFF   1110xxxx 10xxxxxx 10xxxxxx

            //        Scalar Value          UTF-16                byte 1     byte 2     byte 3     byte 4
            //        0000 0000 0xxx xxxx   0000 0000 0xxx xxxx   0xxx xxxx
            //        0000 0yyy yyxx xxxx   0000 0yyy yyxx xxxx   110y yyyy  10xx xxxx
            //        zzzz yyyy yyxx xxxx   zzzz yyyy yyxx xxxx   1110 zzzz  10yy yyyy  10xx xxxx
            // u uuuu zzzz yyyy yyxx xxxx   1101 10ww wwzz zzyy   1111 0uuu  10uu zzzz  10yy yyyy  10xx xxxx
            //                            + 1101 11yy yyxx xxxx
            // Note: uuuuu = wwww + 1

            if (c <= 0x7F)
              {
                if (remaining < 1)
                  return CoderResult.OVERFLOW;
                out.put ((byte) c);
                inPos++;
              }
            else if (c <= 0x7FF)
              {
                if (remaining < 2)
                  return CoderResult.OVERFLOW;
                out.put ((byte) (0xC0 | (c >> 6)));
                out.put ((byte) (0x80 | (c & 0x3F)));
                inPos++;
              }
            else if (0xD800 <= c && c <= 0xDFFF)
              {
                if (remaining < 4)
                  return CoderResult.OVERFLOW;

                // we got a low surrogate without a preciding high one
                if (c > 0xDBFF)
                  return CoderResult.malformedForLength (1);

                // high surrogates
                if (!in.hasRemaining ())
                  return CoderResult.UNDERFLOW;

                char d = in.get ();

                // make sure d is a low surrogate
                if (d < 0xDC00 || d > 0xDFFF)
                  return CoderResult.malformedForLength (1);

                // make the 32 bit value
                // int value2 = (c - 0xD800) * 0x400 + (d - 0xDC00) + 0x10000;
                int value = (((c & 0x3FF) << 10) | (d & 0x3FF)) + 0x10000;
                // assert value == value2;
                out.put ((byte) (0xF0 | (value >> 18)));
                out.put ((byte) (0x80 | ((value >> 12) & 0x3F)));
                out.put ((byte) (0x80 | ((value >>  6) & 0x3F)));
                out.put ((byte) (0x80 | ((value      ) & 0x3F)));
                
                inPos += 2;
              }
            else
              {
                if (remaining < 3)
                  return CoderResult.OVERFLOW;

                out.put ((byte) (0xE0 | (c >> 12)));
                out.put ((byte) (0x80 | ((c >> 6) & 0x3F)));
                out.put ((byte) (0x80 | (c & 0x3F)));
                inPos++;
              }
          }

          return CoderResult.UNDERFLOW;
        }
      finally
        {
          // In case we did a get(), then encountered an error, reset the
          // position to before the error.  If there was no error, this
          // will benignly reset the position to the value it already has.
          in.position (inPos);
        }
    }
  }
}
