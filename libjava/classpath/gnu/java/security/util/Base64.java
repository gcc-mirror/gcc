/* Base64.java -- 
   Copyright (C) 2003, 2006 Free Software Foundation, Inc.

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
exception statement from your version.  */


package gnu.java.security.util;

import java.io.PrintWriter;
import java.io.UnsupportedEncodingException;

/**
 * Most of this implementation is from Robert Harder's public domain Base64
 * code (version 1.4.1 available from &lt;http://iharder.net/xmlizable>).
 */
public class Base64
{

  // Debugging methods and variables
  // -------------------------------------------------------------------------

  private static final String NAME = "Base64";

  private static final boolean DEBUG = true;

  private static final int debuglevel = 9;

  private static final PrintWriter err = new PrintWriter(System.out, true);

  private static void debug(String s)
  {
    err.println(">>> " + NAME + ": " + s);
  }

  // Constants and variables
  // -------------------------------------------------------------------------

  /** Maximum line length (76) of Base64 output. */
  private static final int MAX_LINE_LENGTH = 76;

  /** The new line character (\n) as one byte. */
  private static final byte NEW_LINE = (byte) '\n';

  /** The equals sign (=) as a byte. */
  private static final byte EQUALS_SIGN = (byte) '=';

  private static final byte WHITE_SPACE_ENC = -5; // white space in encoding

  private static final byte EQUALS_SIGN_ENC = -1; // equals sign in encoding

  /** The 64 valid Base64 values. */
  private static final byte[] ALPHABET = { (byte) 'A', (byte) 'B', (byte) 'C',
                                          (byte) 'D', (byte) 'E', (byte) 'F',
                                          (byte) 'G', (byte) 'H', (byte) 'I',
                                          (byte) 'J', (byte) 'K', (byte) 'L',
                                          (byte) 'M', (byte) 'N', (byte) 'O',
                                          (byte) 'P', (byte) 'Q', (byte) 'R',
                                          (byte) 'S', (byte) 'T', (byte) 'U',
                                          (byte) 'V', (byte) 'W', (byte) 'X',
                                          (byte) 'Y', (byte) 'Z', (byte) 'a',
                                          (byte) 'b', (byte) 'c', (byte) 'd',
                                          (byte) 'e', (byte) 'f', (byte) 'g',
                                          (byte) 'h', (byte) 'i', (byte) 'j',
                                          (byte) 'k', (byte) 'l', (byte) 'm',
                                          (byte) 'n', (byte) 'o', (byte) 'p',
                                          (byte) 'q', (byte) 'r', (byte) 's',
                                          (byte) 't', (byte) 'u', (byte) 'v',
                                          (byte) 'w', (byte) 'x', (byte) 'y',
                                          (byte) 'z', (byte) '0', (byte) '1',
                                          (byte) '2', (byte) '3', (byte) '4',
                                          (byte) '5', (byte) '6', (byte) '7',
                                          (byte) '8', (byte) '9', (byte) '+',
                                          (byte) '/' };

  /**
   * Translates a Base64 value to either its 6-bit reconstruction value or a
   * negative number indicating some other meaning.
   */
  private static final byte[] DECODABET = { -9, -9, -9, -9, -9, -9, -9, -9, -9, // Decimal  0 -  8
                                           -5, -5, // Whitespace: Tab and Linefeed
                                           -9, -9, // Decimal 11 - 12
                                           -5, // Whitespace: Carriage Return
                                           -9, -9, -9, -9, -9, -9, -9, -9, -9,
                                           -9, -9, -9, -9, // Decimal 14 - 26
                                           -9, -9, -9, -9, -9, // Decimal 27 - 31
                                           -5, // Whitespace: Space
                                           -9, -9, -9, -9, -9, -9, -9, -9, -9,
                                           -9, // Decimal 33 - 42
                                           62, // Plus sign at decimal 43
                                           -9, -9, -9, // Decimal 44 - 46
                                           63, // Slash at decimal 47
                                           52, 53, 54, 55, 56, 57, 58, 59, 60,
                                           61, // Numbers zero through nine
                                           -9, -9, -9, // Decimal 58 - 60
                                           -1, // Equals sign at decimal 61
                                           -9, -9, -9, // Decimal 62 - 64
                                           0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10,
                                           11, 12, 13, // Letters 'A' through 'N'
                                           14, 15, 16, 17, 18, 19, 20, 21, 22,
                                           23, 24, 25, // Letters 'O' through 'Z'
                                           -9, -9, -9, -9, -9, -9, // Decimal 91 - 96
                                           26, 27, 28, 29, 30, 31, 32, 33, 34,
                                           35, 36, 37, 38, // Letters 'a' through 'm'
                                           39, 40, 41, 42, 43, 44, 45, 46, 47,
                                           48, 49, 50, 51, // Letters 'n' through 'z'
                                           -9, -9, -9, -9 // Decimal 123 - 126
  };

  // Constructor(s)
  // -------------------------------------------------------------------------

  /** Trivial private ctor to enfore Singleton pattern. */
  private Base64()
  {
    super();
  }

  // Class methods
  // -------------------------------------------------------------------------

  /**
   * Encodes a byte array into Base64 notation. Equivalent to calling
   * <code>encode(source, 0, source.length)</code>.
   *
   * @param src the data to convert.
   */
  public static final String encode(final byte[] src)
  {
    return encode(src, 0, src.length, true);
  }

  /**
   * Encodes a byte array into Base64 notation.
   *
   * @param src the data to convert.
   * @param off offset in array where conversion should begin.
   * @param len length of data to convert.
   * @param breakLines break lines at 80 characters or less.
   */
  public static final String encode(final byte[] src, final int off,
                                    final int len, final boolean breakLines)
  {
    final int len43 = len * 4 / 3;
    final byte[] outBuff = new byte[len43 // Main 4:3
                                    + ((len % 3) > 0 ? 4 : 0) // Account for padding
                                    + (breakLines ? (len43 / MAX_LINE_LENGTH)
                                                 : 0)]; // New lines
    int d = 0;
    int e = 0;
    final int len2 = len - 2;
    int lineLength = 0;
    for (; d < len2; d += 3, e += 4)
      {
        encode3to4(src, d + off, 3, outBuff, e);
        lineLength += 4;
        if (breakLines && lineLength == MAX_LINE_LENGTH)
          {
            outBuff[e + 4] = NEW_LINE;
            e++;
            lineLength = 0;
          }
      }

    if (d < len)
      { // padding needed
        encode3to4(src, d + off, len - d, outBuff, e);
        e += 4;
      }

    return new String(outBuff, 0, e);
  }

  /**
   * Decodes data from Base64 notation.
   *
   * @param s the string to decode.
   * @return the decoded data.
   */
  public static final byte[] decode(final String s)
      throws UnsupportedEncodingException
  {
    final byte[] bytes;
    bytes = s.getBytes("US-ASCII");
    return decode(bytes, 0, bytes.length);
  }

  /**
   * Decodes Base64 content in byte array format and returns the decoded byte
   * array.
   *
   * @param src the Base64 encoded data.
   * @param off the offset of where to begin decoding.
   * @param len the length of characters to decode.
   * @return the decoded data.
   * @throws IllegalArgumentException if <code>src</code> contains an illegal
   * Base-64 character.
   */
  public static byte[] decode(final byte[] src, final int off, final int len)
  {
    final int len34 = len * 3 / 4;
    final byte[] outBuff = new byte[len34]; // Upper limit on size of output
    int outBuffPosn = 0;
    final byte[] b4 = new byte[4];
    int b4Posn = 0;
    int i;
    byte sbiCrop, sbiDecode;
    for (i = off; i < off + len; i++)
      {
        sbiCrop = (byte) (src[i] & 0x7F); // Only the low seven bits
        sbiDecode = DECODABET[sbiCrop];
        if (sbiDecode >= WHITE_SPACE_ENC)
          { // White space, Equals sign or better
            if (sbiDecode >= EQUALS_SIGN_ENC)
              {
                b4[b4Posn++] = sbiCrop;
                if (b4Posn > 3)
                  {
                    outBuffPosn += decode4to3(b4, 0, outBuff, outBuffPosn);
                    b4Posn = 0;
                    // If that was the equals sign, break out of 'for' loop
                    if (sbiCrop == EQUALS_SIGN)
                      break;
                  } // end if: quartet built
              } // end if: equals sign or better
          }
        else
          {
            throw new IllegalArgumentException("Illegal BASE-64 character at #"
                                               + i + ": " + src[i]
                                               + "(decimal)");
          }
      }

    final byte[] result = new byte[outBuffPosn];
    System.arraycopy(outBuff, 0, result, 0, outBuffPosn);
    return result;
  }

  /**
   * <p>Encodes up to three bytes of the array <code>src</code> and writes
   * the resulting four Base64 bytes to <code>dest</code>. The source and
   * destination arrays can be manipulated anywhere along their length by
   * specifying <code>sOffset</code> and <code>dOffset</code>.</p>
   *
   * <p>This method does not check to make sure the arrays are large enough to
   * accomodate <code>sOffset + 3</code> for the <code>src</code> array or
   * <code>dOffset + 4</code> for the <code>dest</code> array. The actual
   * number of significant bytes in the input array is given by
   * <code>numBytes</code>.</p>
   *
   * @param src the array to convert.
   * @param sOffset the index where conversion begins.
   * @param numBytes the number of significant bytes in your array.
   * @param dest the array to hold the conversion.
   * @param dOffset the index where output will be put.
   * @return the <code>destination</code> array.
   */
  private static final byte[] encode3to4(final byte[] src, final int sOffset,
                                         final int numBytes, final byte[] dest,
                                         final int dOffset)
  {
    //           1         2         3
    // 01234567890123456789012345678901 Bit position
    // --------000000001111111122222222 Array position from threeBytes
    // --------|    ||    ||    ||    | Six bit groups to index ALPHABET
    //          >>18  >>12  >> 6  >> 0  Right shift necessary
    //                0x3F  0x3F  0x3F  Additional AND

    // Create buffer with zero-padding if there are only one or two
    // significant bytes passed in the array.
    // We have to shift left 24 in order to flush out the 1's that appear
    // when Java treats a value as negative that is cast from a byte to an int.
    final int inBuff = (numBytes > 0 ? ((src[sOffset] << 24) >>> 8) : 0)
                       | (numBytes > 1 ? ((src[sOffset + 1] << 24) >>> 16) : 0)
                       | (numBytes > 2 ? ((src[sOffset + 2] << 24) >>> 24) : 0);
    switch (numBytes)
      {
      case 3:
        dest[dOffset] = ALPHABET[(inBuff >>> 18)];
        dest[dOffset + 1] = ALPHABET[(inBuff >>> 12) & 0x3F];
        dest[dOffset + 2] = ALPHABET[(inBuff >>> 6) & 0x3F];
        dest[dOffset + 3] = ALPHABET[(inBuff) & 0x3F];
        break;
      case 2:
        dest[dOffset] = ALPHABET[(inBuff >>> 18)];
        dest[dOffset + 1] = ALPHABET[(inBuff >>> 12) & 0x3F];
        dest[dOffset + 2] = ALPHABET[(inBuff >>> 6) & 0x3F];
        dest[dOffset + 3] = EQUALS_SIGN;
        break;
      case 1:
        dest[dOffset] = ALPHABET[(inBuff >>> 18)];
        dest[dOffset + 1] = ALPHABET[(inBuff >>> 12) & 0x3F];
        dest[dOffset + 2] = EQUALS_SIGN;
        dest[dOffset + 3] = EQUALS_SIGN;
        break;
      }
    return dest;
  }

  /**
   * <p>Decodes four bytes from array <code>src</code> and writes the
   * resulting bytes (up to three of them) to <code>dest</code>.</p>
   *
   * <p>The source and destination arrays can be manipulated anywhere along
   * their length by specifying <code>sOffset</code> and <code>dOffset</code>.
   * </p>
   *
   * <p>This method does not check to make sure your arrays are large enough
   * to accomodate <code>sOffset + 4</code> for the <code>src</code> array or
   * <code>dOffset + 3</code> for the <code>dest</code> array. This method
   * returns the actual number of bytes that were converted from the Base64
   * encoding.</p>
   *
   * @param src the array to convert.
   * @param sOffset the index where conversion begins.
   * @param dest the array to hold the conversion.
   * @param dOffset the index where output will be put.
   * @return the number of decoded bytes converted.
   */
  private static final int decode4to3(final byte[] src, final int sOffset,
                                      final byte[] dest, final int dOffset)
  {
    if (src[sOffset + 2] == EQUALS_SIGN)
      { // Example: Dk==
        final int outBuff = ((DECODABET[src[sOffset]] & 0xFF) << 18)
                            | ((DECODABET[src[sOffset + 1]] & 0xFF) << 12);
        dest[dOffset] = (byte) (outBuff >>> 16);
        return 1;
      }

    if (src[sOffset + 3] == EQUALS_SIGN)
      { // Example: DkL=
        final int outBuff = ((DECODABET[src[sOffset]] & 0xFF) << 18)
                            | ((DECODABET[src[sOffset + 1]] & 0xFF) << 12)
                            | ((DECODABET[src[sOffset + 2]] & 0xFF) << 6);
        dest[dOffset] = (byte) (outBuff >>> 16);
        dest[dOffset + 1] = (byte) (outBuff >>> 8);
        return 2;
      }

    try
      { // Example: DkLE
        final int outBuff = ((DECODABET[src[sOffset]] & 0xFF) << 18)
                            | ((DECODABET[src[sOffset + 1]] & 0xFF) << 12)
                            | ((DECODABET[src[sOffset + 2]] & 0xFF) << 6)
                            | ((DECODABET[src[sOffset + 3]] & 0xFF));
        dest[dOffset] = (byte) (outBuff >> 16);
        dest[dOffset + 1] = (byte) (outBuff >> 8);
        dest[dOffset + 2] = (byte) outBuff;
        return 3;
      }
    catch (Exception x)
      {
        if (DEBUG && debuglevel > 8)
          {
            debug("" + src[sOffset] + ": " + (DECODABET[src[sOffset]]));
            debug("" + src[sOffset + 1] + ": " + (DECODABET[src[sOffset + 1]]));
            debug("" + src[sOffset + 2] + ": " + (DECODABET[src[sOffset + 2]]));
            debug("" + src[sOffset + 3] + ": " + (DECODABET[src[sOffset + 3]]));
          }
        return -1;
      }
  }
}
