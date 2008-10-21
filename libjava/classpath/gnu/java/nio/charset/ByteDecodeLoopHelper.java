/* ByteCharset.java -- Abstract class for generic 1-byte encodings.
   Copyright (C) 2005 Free Software Foundation, Inc.

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
import java.nio.charset.CoderResult;

/**
 * Helper class to deal with decoding loops that read a byte at a time
 * 
 * @author Ian Rogers
 */
public abstract class ByteDecodeLoopHelper
{
  /**
   * @return can the given byte be encoded
   */
  protected abstract boolean isMappable(byte b);

  /**
   * Map the given byte to a char, the given byte is guaranteed to be mappable
   */
  protected abstract char mapToChar(byte b);

  /**
   * Encodes one or more characters into one or more bytes, mapping each
   * character to only one byte
   * 
   * @param in character buffer to read from
   * @param out byte buffer to write to
   * @return the result state of the encoder
   */
  CoderResult decodeLoop(ByteBuffer in, CharBuffer out)
  {
    if (in.hasArray() && out.hasArray())
      {
        return arrayDecodeLoop(in, out);
      } else
      {
        return normalDecodeLoop(in, out);
      }
  }

  /**
   * Encode loop using get and put operations
   */
  private CoderResult normalDecodeLoop(ByteBuffer in, CharBuffer out)
  {
    int outRemaining = out.remaining();
    int inRemaining = in.remaining();
    while (inRemaining > 0 && outRemaining > 0)
      {
        byte b = in.get();
        inRemaining--;

        if (!isMappable(b))
          {
            in.position(in.position() - 1);
            return CoderResult.unmappableForLength(1);
          }
        char c = mapToChar(b);
        out.put(c);
        outRemaining--;
      }
    if (inRemaining > 0)
      {
        return CoderResult.OVERFLOW;
      } else
      {
        return CoderResult.UNDERFLOW;
      }
  }

  /**
   * Encode loop using array read and write operations
   */
  private CoderResult arrayDecodeLoop(ByteBuffer in, CharBuffer out)
  {
    byte[] inArray = in.array();
    char[] outArray = out.array();
    int inPos = in.arrayOffset() + in.position();
    int outPos = out.arrayOffset() + out.position();
    int inRemaining = in.remaining();
    int outRemaining = out.remaining();
    CoderResult result;

	bailOut:
    if (inRemaining <= outRemaining)
      {
        for (int i = 0; i < inRemaining; i++)
          {
            byte b = inArray[inPos];
            inPos++;
            if (!isMappable(b))
              {
                inPos--;
                result = CoderResult.unmappableForLength(1);
				break bailOut;
              }
            char c = mapToChar(b);
            outArray[outPos] = c;
            outPos++;
          }
        result = CoderResult.UNDERFLOW;
      }
    else
      {
        for (int i = 0; i < outRemaining; i++)
          {
            byte b = inArray[inPos];
            inPos++;
            if (!isMappable(b))
              {
                inPos--;
                result = CoderResult.unmappableForLength(1);
				break bailOut;
              }
            char c = mapToChar(b);
            outArray[outPos] = c;
            outPos++;
          }
        result = CoderResult.OVERFLOW;
      }
    in.position(inPos - in.arrayOffset());
    out.position(outPos - out.arrayOffset());
    return result;
  }
}
