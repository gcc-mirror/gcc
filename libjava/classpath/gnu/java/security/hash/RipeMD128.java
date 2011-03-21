/* RipeMD128.java --
   Copyright (C) 2001, 2002, 2006 Free Software Foundation, Inc.

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


package gnu.java.security.hash;

import gnu.java.security.Registry;
import gnu.java.security.util.Util;

/**
 * RIPEMD-128 is a 128-bit message digest.
 * <p>
 * References:
 * <ol>
 *    <li><a href="http://www.esat.kuleuven.ac.be/~bosselae/ripemd160.html">
 *    RIPEMD160</a>: A Strengthened Version of RIPEMD.<br>
 *    Hans Dobbertin, Antoon Bosselaers and Bart Preneel.</li>
 * </ol>
 */
public class RipeMD128
    extends BaseHash
{
  private static final int BLOCK_SIZE = 64; // inner block size in bytes

  private static final String DIGEST0 = "CDF26213A150DC3ECB610F18F6B38B46";

  /** Constants for the transform method. */
  // selection of message word
  private static final int[] R = {
       0,  1,  2,  3,  4,  5,  6, 7,  8, 9, 10, 11, 12, 13, 14, 15,
       7,  4, 13,  1, 10,  6, 15, 3, 12, 0,  9,  5,  2, 14, 11,  8,
       3, 10, 14,  4,  9, 15,  8, 1,  2, 7,  0,  6, 13, 11,  5, 12,
       1,  9, 11, 10,  0,  8, 12, 4, 13, 3,  7, 15, 14,  5,  6,  2 };

  private static final int[] Rp = {
       5, 14, 7, 0, 9,  2, 11,  4, 13,  6, 15,  8, 1, 10,  3, 12,
       6, 11, 3, 7, 0, 13,  5, 10, 14, 15,  8, 12, 4,  9,  1,  2,
      15,  5, 1, 3, 7, 14,  6,  9, 11,  8, 12,  2, 10, 0,  4, 13,
       8,  6, 4, 1, 3, 11, 15,  0,  5, 12,  2, 13,  9, 7, 10, 14 };

  // amount for rotate left (rol)
  private static final int[] S = {
      11, 14, 15, 12,  5,  8,  7,  9, 11, 13, 14, 15,  6,  7,  9,  8,
       7,  6,  8, 13, 11,  9,  7, 15,  7, 12, 15,  9, 11,  7, 13, 12,
      11, 13,  6,  7, 14,  9, 13, 15, 14,  8, 13,  6,  5, 12,  7,  5,
      11, 12, 14, 15, 14, 15,  9,  8,  9, 14,  5,  6,  8,  6,  5, 12 };

  private static final int[] Sp = {
       8,  9,  9, 11, 13, 15, 15,  5,  7,  7,  8, 11, 14, 14, 12,  6,
       9, 13, 15,  7, 12,  8,  9, 11,  7,  7, 12,  7,  6, 15, 13, 11,
       9,  7, 15, 11,  8,  6,  6, 14, 12, 13,  5, 14, 13, 13,  7,  5,
      15,  5,  8, 11, 14, 14,  6, 14,  6,  9, 12,  9, 12,  5, 15,  8 };

  /** caches the result of the correctness test, once executed. */
  private static Boolean valid;

  /** 128-bit h0, h1, h2, h3 (interim result) */
  private int h0, h1, h2, h3;

  /** 512 bits work buffer = 16 x 32-bit words */
  private int[] X = new int[16];

  /** Trivial 0-arguments constructor. */
  public RipeMD128()
  {
    super(Registry.RIPEMD128_HASH, 16, BLOCK_SIZE);
  }

  /**
   * Private constructor for cloning purposes.
   *
   * @param md the instance to clone.
   */
  private RipeMD128(RipeMD128 md)
  {
    this();

    this.h0 = md.h0;
    this.h1 = md.h1;
    this.h2 = md.h2;
    this.h3 = md.h3;
    this.count = md.count;
    this.buffer = (byte[]) md.buffer.clone();
  }

  public Object clone()
  {
    return new RipeMD128(this);
  }

  protected void transform(byte[] in, int offset)
  {
    int A, B, C, D, Ap, Bp, Cp, Dp, T, s, i;
    // encode 64 bytes from input block into an array of 16 unsigned integers.
    for (i = 0; i < 16; i++)
      X[i] = (in[offset++] & 0xFF)
           | (in[offset++] & 0xFF) << 8
           | (in[offset++] & 0xFF) << 16
           |  in[offset++]         << 24;
    A = Ap = h0;
    B = Bp = h1;
    C = Cp = h2;
    D = Dp = h3;
    for (i = 0; i < 16; i++) // rounds 0...15
      {
        s = S[i];
        T = A + (B ^ C ^ D) + X[i];
        A = D;
        D = C;
        C = B;
        B = T << s | T >>> (32 - s);

        s = Sp[i];
        T = Ap + ((Bp & Dp) | (Cp & ~Dp)) + X[Rp[i]] + 0x50A28BE6;
        Ap = Dp;
        Dp = Cp;
        Cp = Bp;
        Bp = T << s | T >>> (32 - s);
      }
    for (; i < 32; i++) // rounds 16...31
      {
        s = S[i];
        T = A + ((B & C) | (~B & D)) + X[R[i]] + 0x5A827999;
        A = D;
        D = C;
        C = B;
        B = T << s | T >>> (32 - s);

        s = Sp[i];
        T = Ap + ((Bp | ~Cp) ^ Dp) + X[Rp[i]] + 0x5C4DD124;
        Ap = Dp;
        Dp = Cp;
        Cp = Bp;
        Bp = T << s | T >>> (32 - s);
      }
    for (; i < 48; i++) // rounds 32...47
      {
        s = S[i];
        T = A + ((B | ~C) ^ D) + X[R[i]] + 0x6ED9EBA1;
        A = D;
        D = C;
        C = B;
        B = T << s | T >>> (32 - s);

        s = Sp[i];
        T = Ap + ((Bp & Cp) | (~Bp & Dp)) + X[Rp[i]] + 0x6D703EF3;
        Ap = Dp;
        Dp = Cp;
        Cp = Bp;
        Bp = T << s | T >>> (32 - s);
      }
    for (; i < 64; i++) // rounds 48...63
      {
        s = S[i];
        T = A + ((B & D) | (C & ~D)) + X[R[i]] + 0x8F1BBCDC;
        A = D;
        D = C;
        C = B;
        B = T << s | T >>> (32 - s);

        s = Sp[i];
        T = Ap + (Bp ^ Cp ^ Dp) + X[Rp[i]];
        Ap = Dp;
        Dp = Cp;
        Cp = Bp;
        Bp = T << s | T >>> (32 - s);
      }
    T = h1 + C + Dp;
    h1 = h2 + D + Ap;
    h2 = h3 + A + Bp;
    h3 = h0 + B + Cp;
    h0 = T;
  }

  protected byte[] padBuffer()
  {
    int n = (int)(count % BLOCK_SIZE);
    int padding = (n < 56) ? (56 - n) : (120 - n);
    byte[] result = new byte[padding + 8];
    // padding is always binary 1 followed by binary 0s
    result[0] = (byte) 0x80;
    // save number of bits, casting the long to an array of 8 bytes
    long bits = count << 3;
    result[padding++] = (byte) bits;
    result[padding++] = (byte)(bits >>> 8);
    result[padding++] = (byte)(bits >>> 16);
    result[padding++] = (byte)(bits >>> 24);
    result[padding++] = (byte)(bits >>> 32);
    result[padding++] = (byte)(bits >>> 40);
    result[padding++] = (byte)(bits >>> 48);
    result[padding  ] = (byte)(bits >>> 56);
    return result;
  }

  protected byte[] getResult()
  {
    return new byte[] {
        (byte) h0, (byte)(h0 >>> 8), (byte)(h0 >>> 16), (byte)(h0 >>> 24),
        (byte) h1, (byte)(h1 >>> 8), (byte)(h1 >>> 16), (byte)(h1 >>> 24),
        (byte) h2, (byte)(h2 >>> 8), (byte)(h2 >>> 16), (byte)(h2 >>> 24),
        (byte) h3, (byte)(h3 >>> 8), (byte)(h3 >>> 16), (byte)(h3 >>> 24)
    };
  }

  protected void resetContext()
  {
    // magic RIPEMD128 initialisation constants
    h0 = 0x67452301;
    h1 = 0xEFCDAB89;
    h2 = 0x98BADCFE;
    h3 = 0x10325476;
  }

  public boolean selfTest()
  {
    if (valid == null)
      {
        String d = Util.toString(new RipeMD128().digest());
        valid = Boolean.valueOf(DIGEST0.equals(d));
      }
    return valid.booleanValue();
  }
}
