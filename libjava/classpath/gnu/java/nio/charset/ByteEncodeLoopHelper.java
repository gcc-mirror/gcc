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
 * Helper class to deal with encoding loops that write a byte at a time
 * 
 * @author Ian Rogers
 */
public abstract class ByteEncodeLoopHelper
{
  /**
   * @return can the given character be encoded
   */
  protected abstract boolean isMappable(char c);

  /**
   * Map the given character to a byte, the given character is guaranteed to be
   * mappable
   */
  protected abstract byte mapToByte(char c);

  /**
   * Encodes one or more characters into one or more bytes, mapping each
   * character to only one byte
   * 
   * @param in character buffer to read from
   * @param out byte buffer to write to
   * @return the result state of the encoder
   */
  CoderResult encodeLoop(CharBuffer in, ByteBuffer out)
  {
    if (in.hasArray() && out.hasArray())
      {
        return arrayEncodeLoop(in, out);
      } else
      {
        return normalEncodeLoop(in, out);
      }
  }

  /**
   * Encode loop using get and put operations
   */
  private CoderResult normalEncodeLoop(CharBuffer in, ByteBuffer out)
  {
    int outRemaining = out.remaining();
    int inRemaining = in.remaining();
    while (inRemaining > 0 && outRemaining > 0)
      {
        char c = in.get();
        inRemaining--;

        if (!isMappable(c))
          {
            in.position(in.position() - 1);
            return CoderResult.unmappableForLength(1);
          }
        byte b = mapToByte(c);
        out.put(b);
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
  private CoderResult arrayEncodeLoop(CharBuffer in, ByteBuffer out)
  {
    char[] inArray = in.array();
    byte[] outArray = out.array();
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
            char inChar = inArray[inPos];
            inPos++;
            if (!isMappable(inChar))
              {
                inPos--;
                result = CoderResult.unmappableForLength(1);
                break bailOut;
              }
            byte b = mapToByte(inChar);
            outArray[outPos] = b;
            outPos++;
          }
        result = CoderResult.UNDERFLOW;
      }
    else
      {
        for (int i = 0; i < outRemaining; i++)
          {
            char inChar = inArray[inPos];
            inPos++;
            if (!isMappable(inChar))
              {
                inPos--;
                result = CoderResult.unmappableForLength(1);
                break bailOut;
              }
            byte b = mapToByte(inChar);
            outArray[outPos] = b;
            outPos++;
          }
        result = CoderResult.OVERFLOW;
      }
    in.position(inPos - in.arrayOffset());
    out.position(outPos - out.arrayOffset());
    return result;
  }
}
