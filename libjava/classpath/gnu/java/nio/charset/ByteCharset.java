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
import java.nio.charset.Charset;
import java.nio.charset.CharsetDecoder;
import java.nio.charset.CharsetEncoder;
import java.nio.charset.CoderResult;

/**
 * A generic encoding framework for single-byte encodings, utilizing a look-up
 * table.
 *
 * This replaces the gnu.java.io.EncoderEightBitLookup class, created by Aron
 * Renn.
 *
 * @author Sven de Marothy
 * @modified Ian Rogers
 */
abstract class ByteCharset extends Charset
{
  protected final char[] lookupTable;
  /**
   * Char to signify the character in the table is undefined
   */
  protected static final char NONE = (char) 0xFFFD;

  ByteCharset(String canonicalName, String[] aliases, char[] lookup)
  {
    super(canonicalName, aliases);
    lookupTable = lookup;
  }

  /**
   * Most western charsets include ASCII, but this should be overloaded for
   * others.
   */
  public boolean contains(Charset cs)
  {
    return cs instanceof US_ASCII || (cs.getClass() == getClass());
  }

  char[] getLookupTable()
  {
    return lookupTable;
  }

  public CharsetDecoder newDecoder()
  {
    return new Decoder(this);
  }

  public CharsetEncoder newEncoder()
  {
    return new Encoder(this);
  }

  private static final class Decoder extends CharsetDecoder
  {
    /** Lookup of byte to char mappings */
    private final char[] lookup;

    /** Helper to decode loops */
    private final ByteDecodeLoopHelper helper = new ByteDecodeLoopHelper()
    {
      protected boolean isMappable(byte b)
      {
        return lookup[(int) (b & 0xFF)] != NONE;
      }
      protected char mapToChar(byte b)
      {
        return lookup[(int) (b & 0xFF)];
      }
    };

    // Package-private to avoid a trampoline constructor.
    Decoder(ByteCharset cs)
    {
      super(cs, 1.0f, 1.0f);
      lookup = cs.getLookupTable();
    }

    protected CoderResult decodeLoop(ByteBuffer in, CharBuffer out)
    {
      return helper.decodeLoop(in, out);
    }
  }

  private static final class Encoder extends CharsetEncoder
  {
    /** Lookup of char to byte mappings */
    private final byte[] lookup;

    /** Helper to encode loops */
    private final ByteEncodeLoopHelper helper = new ByteEncodeLoopHelper()
    {
      protected boolean isMappable(char c)
      {
        return canEncode(c);
      }
      protected byte mapToByte(char c)
      {
        return lookup[c];
      }
    };

    // Package-private to avoid a trampoline constructor.
    Encoder(ByteCharset cs)
    {
      super(cs, 1.0f, 1.0f);

      char[] lookup_table = cs.getLookupTable();

      // Create the inverse look-up table.
      // determine required size of encoding_table:
      int max = 0;
      for (int i = 0; i < lookup_table.length; i++)
        {
          int c = (int) lookup_table[i];
          max = (c > max && c < NONE) ? c : max;
        }

      lookup = new byte[max + 1];

      for (int i = 0; i < lookup_table.length; i++)
        {
          int c = (int) lookup_table[i];
          if (c != 0 && c < NONE)
            {
              lookup[c] = (byte) i;
            }
        }
    }

    public boolean canEncode(char c)
    {
      byte b = (c < lookup.length) ? lookup[c] : 0;
      return b != 0 || c == 0;
    }

    public boolean canEncode(CharSequence cs)
    {
      for (int i = 0; i < cs.length(); ++i)
        {
          if (!canEncode(cs.charAt(i)))
            return false;
        }
      return true;
    }

    protected CoderResult encodeLoop(CharBuffer in, ByteBuffer out)
    {
      return helper.encodeLoop(in, out);
    }
  }
}
