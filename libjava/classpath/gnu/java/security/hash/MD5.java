/* MD5.java -- 
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
 * The MD5 message-digest algorithm takes as input a message of arbitrary
 * length and produces as output a 128-bit "fingerprint" or "message digest" of
 * the input. It is conjectured that it is computationally infeasible to
 * produce two messages having the same message digest, or to produce any
 * message having a given prespecified target message digest.
 * <p>
 * References:
 * <ol>
 *    <li>The <a href="http://www.ietf.org/rfc/rfc1321.txt">MD5</a> Message-
 *    Digest Algorithm.<br>
 *    R. Rivest.</li>
 * </ol>
 */
public class MD5
    extends BaseHash
{
  private static final int BLOCK_SIZE = 64; // inner block size in bytes

  private static final String DIGEST0 = "D41D8CD98F00B204E9800998ECF8427E";

  /** caches the result of the correctness test, once executed. */
  private static Boolean valid;

  /** 128-bit interim result. */
  private int h0, h1, h2, h3;

  /** Trivial 0-arguments constructor. */
  public MD5()
  {
    super(Registry.MD5_HASH, 16, BLOCK_SIZE);
  }

  /**
   * Private constructor for cloning purposes.
   *
   * @param md the instance to clone.
   */
  private MD5(MD5 md)
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
    return new MD5(this);
  }

  protected synchronized void transform(byte[] in, int i)
  {
    int X0 = (in[i++] & 0xFF)
           | (in[i++] & 0xFF) << 8
           | (in[i++] & 0xFF) << 16
           |  in[i++]         << 24;
    int X1 = (in[i++] & 0xFF)
           | (in[i++] & 0xFF) << 8
           | (in[i++] & 0xFF) << 16
           |  in[i++]         << 24;
    int X2 = (in[i++] & 0xFF)
           | (in[i++] & 0xFF) << 8
           | (in[i++] & 0xFF) << 16
           |  in[i++]         << 24;
    int X3 = (in[i++] & 0xFF)
           | (in[i++] & 0xFF) << 8
           | (in[i++] & 0xFF) << 16
           |  in[i++]         << 24;
    int X4 = (in[i++] & 0xFF)
           | (in[i++] & 0xFF) << 8
           | (in[i++] & 0xFF) << 16
           |  in[i++]         << 24;
    int X5 = (in[i++] & 0xFF)
           | (in[i++] & 0xFF) << 8
           | (in[i++] & 0xFF) << 16
           |  in[i++]         << 24;
    int X6 = (in[i++] & 0xFF)
           | (in[i++] & 0xFF) << 8
           | (in[i++] & 0xFF) << 16
           |  in[i++]         << 24;
    int X7 = (in[i++] & 0xFF)
           | (in[i++] & 0xFF) << 8
           | (in[i++] & 0xFF) << 16
           |  in[i++]         << 24;
    int X8 = (in[i++] & 0xFF)
           | (in[i++] & 0xFF) << 8
           | (in[i++] & 0xFF) << 16
           |  in[i++]         << 24;
    int X9 = (in[i++] & 0xFF)
           | (in[i++] & 0xFF) << 8
           | (in[i++] & 0xFF) << 16
           |  in[i++]         << 24;
    int X10 = (in[i++] & 0xFF)
            | (in[i++] & 0xFF) << 8
            | (in[i++] & 0xFF) << 16
            |  in[i++]         << 24;
    int X11 = (in[i++] & 0xFF)
            | (in[i++] & 0xFF) << 8
            | (in[i++] & 0xFF) << 16
            |  in[i++]         << 24;
    int X12 = (in[i++] & 0xFF)
            | (in[i++] & 0xFF) << 8
            | (in[i++] & 0xFF) << 16
            |  in[i++]         << 24;
    int X13 = (in[i++] & 0xFF)
            | (in[i++] & 0xFF) << 8
            | (in[i++] & 0xFF) << 16
            |  in[i++]         << 24;
    int X14 = (in[i++] & 0xFF)
            | (in[i++] & 0xFF) << 8
            | (in[i++] & 0xFF) << 16
            |  in[i++]         << 24;
    int X15 = (in[i++] & 0xFF)
            | (in[i++] & 0xFF) << 8
            | (in[i++] & 0xFF) << 16
            |  in[i]           << 24;
    int A = h0;
    int B = h1;
    int C = h2;
    int D = h3;
    // hex constants are from md5.c in FSF Gnu Privacy Guard 0.9.2
    // round 1
    A += ((B & C) | (~B & D)) + X0 + 0xD76AA478;
    A = B + (A << 7 | A >>> -7);
    D += ((A & B) | (~A & C)) + X1 + 0xE8C7B756;
    D = A + (D << 12 | D >>> -12);
    C += ((D & A) | (~D & B)) + X2 + 0x242070DB;
    C = D + (C << 17 | C >>> -17);
    B += ((C & D) | (~C & A)) + X3 + 0xC1BDCEEE;
    B = C + (B << 22 | B >>> -22);

    A += ((B & C) | (~B & D)) + X4 + 0xF57C0FAF;
    A = B + (A << 7 | A >>> -7);
    D += ((A & B) | (~A & C)) + X5 + 0x4787C62A;
    D = A + (D << 12 | D >>> -12);
    C += ((D & A) | (~D & B)) + X6 + 0xA8304613;
    C = D + (C << 17 | C >>> -17);
    B += ((C & D) | (~C & A)) + X7 + 0xFD469501;
    B = C + (B << 22 | B >>> -22);

    A += ((B & C) | (~B & D)) + X8 + 0x698098D8;
    A = B + (A << 7 | A >>> -7);
    D += ((A & B) | (~A & C)) + X9 + 0x8B44F7AF;
    D = A + (D << 12 | D >>> -12);
    C += ((D & A) | (~D & B)) + X10 + 0xFFFF5BB1;
    C = D + (C << 17 | C >>> -17);
    B += ((C & D) | (~C & A)) + X11 + 0x895CD7BE;
    B = C + (B << 22 | B >>> -22);

    A += ((B & C) | (~B & D)) + X12 + 0x6B901122;
    A = B + (A << 7 | A >>> -7);
    D += ((A & B) | (~A & C)) + X13 + 0xFD987193;
    D = A + (D << 12 | D >>> -12);
    C += ((D & A) | (~D & B)) + X14 + 0xA679438E;
    C = D + (C << 17 | C >>> -17);
    B += ((C & D) | (~C & A)) + X15 + 0x49B40821;
    B = C + (B << 22 | B >>> -22);

    // round 2
    A += ((B & D) | (C & ~D)) + X1 + 0xF61E2562;
    A = B + (A << 5 | A >>> -5);
    D += ((A & C) | (B & ~C)) + X6 + 0xC040B340;
    D = A + (D << 9 | D >>> -9);
    C += ((D & B) | (A & ~B)) + X11 + 0x265E5A51;
    C = D + (C << 14 | C >>> -14);
    B += ((C & A) | (D & ~A)) + X0 + 0xE9B6C7AA;
    B = C + (B << 20 | B >>> -20);

    A += ((B & D) | (C & ~D)) + X5 + 0xD62F105D;
    A = B + (A << 5 | A >>> -5);
    D += ((A & C) | (B & ~C)) + X10 + 0x02441453;
    D = A + (D << 9 | D >>> -9);
    C += ((D & B) | (A & ~B)) + X15 + 0xD8A1E681;
    C = D + (C << 14 | C >>> -14);
    B += ((C & A) | (D & ~A)) + X4 + 0xE7D3FBC8;
    B = C + (B << 20 | B >>> -20);

    A += ((B & D) | (C & ~D)) + X9 + 0x21E1CDE6;
    A = B + (A << 5 | A >>> -5);
    D += ((A & C) | (B & ~C)) + X14 + 0xC33707D6;
    D = A + (D << 9 | D >>> -9);
    C += ((D & B) | (A & ~B)) + X3 + 0xF4D50D87;
    C = D + (C << 14 | C >>> -14);
    B += ((C & A) | (D & ~A)) + X8 + 0x455A14ED;
    B = C + (B << 20 | B >>> -20);

    A += ((B & D) | (C & ~D)) + X13 + 0xA9E3E905;
    A = B + (A << 5 | A >>> -5);
    D += ((A & C) | (B & ~C)) + X2 + 0xFCEFA3F8;
    D = A + (D << 9 | D >>> -9);
    C += ((D & B) | (A & ~B)) + X7 + 0x676F02D9;
    C = D + (C << 14 | C >>> -14);
    B += ((C & A) | (D & ~A)) + X12 + 0x8D2A4C8A;
    B = C + (B << 20 | B >>> -20);

    // round 3
    A += (B ^ C ^ D) + X5 + 0xFFFA3942;
    A = B + (A << 4 | A >>> -4);
    D += (A ^ B ^ C) + X8 + 0x8771F681;
    D = A + (D << 11 | D >>> -11);
    C += (D ^ A ^ B) + X11 + 0x6D9D6122;
    C = D + (C << 16 | C >>> -16);
    B += (C ^ D ^ A) + X14 + 0xFDE5380C;
    B = C + (B << 23 | B >>> -23);

    A += (B ^ C ^ D) + X1 + 0xA4BEEA44;
    A = B + (A << 4 | A >>> -4);
    D += (A ^ B ^ C) + X4 + 0x4BDECFA9;
    D = A + (D << 11 | D >>> -11);
    C += (D ^ A ^ B) + X7 + 0xF6BB4B60;
    C = D + (C << 16 | C >>> -16);
    B += (C ^ D ^ A) + X10 + 0xBEBFBC70;
    B = C + (B << 23 | B >>> -23);

    A += (B ^ C ^ D) + X13 + 0x289B7EC6;
    A = B + (A << 4 | A >>> -4);
    D += (A ^ B ^ C) + X0 + 0xEAA127FA;
    D = A + (D << 11 | D >>> -11);
    C += (D ^ A ^ B) + X3 + 0xD4EF3085;
    C = D + (C << 16 | C >>> -16);
    B += (C ^ D ^ A) + X6 + 0x04881D05;
    B = C + (B << 23 | B >>> -23);

    A += (B ^ C ^ D) + X9 + 0xD9D4D039;
    A = B + (A << 4 | A >>> -4);
    D += (A ^ B ^ C) + X12 + 0xE6DB99E5;
    D = A + (D << 11 | D >>> -11);
    C += (D ^ A ^ B) + X15 + 0x1FA27CF8;
    C = D + (C << 16 | C >>> -16);
    B += (C ^ D ^ A) + X2 + 0xC4AC5665;
    B = C + (B << 23 | B >>> -23);

    // round 4
    A += (C ^ (B | ~D)) + X0 + 0xF4292244;
    A = B + (A << 6 | A >>> -6);
    D += (B ^ (A | ~C)) + X7 + 0x432AFF97;
    D = A + (D << 10 | D >>> -10);
    C += (A ^ (D | ~B)) + X14 + 0xAB9423A7;
    C = D + (C << 15 | C >>> -15);
    B += (D ^ (C | ~A)) + X5 + 0xFC93A039;
    B = C + (B << 21 | B >>> -21);

    A += (C ^ (B | ~D)) + X12 + 0x655B59C3;
    A = B + (A << 6 | A >>> -6);
    D += (B ^ (A | ~C)) + X3 + 0x8F0CCC92;
    D = A + (D << 10 | D >>> -10);
    C += (A ^ (D | ~B)) + X10 + 0xFFEFF47D;
    C = D + (C << 15 | C >>> -15);
    B += (D ^ (C | ~A)) + X1 + 0x85845dd1;
    B = C + (B << 21 | B >>> -21);

    A += (C ^ (B | ~D)) + X8 + 0x6FA87E4F;
    A = B + (A << 6 | A >>> -6);
    D += (B ^ (A | ~C)) + X15 + 0xFE2CE6E0;
    D = A + (D << 10 | D >>> -10);
    C += (A ^ (D | ~B)) + X6 + 0xA3014314;
    C = D + (C << 15 | C >>> -15);
    B += (D ^ (C | ~A)) + X13 + 0x4E0811A1;
    B = C + (B << 21 | B >>> -21);

    A += (C ^ (B | ~D)) + X4 + 0xF7537E82;
    A = B + (A << 6 | A >>> -6);
    D += (B ^ (A | ~C)) + X11 + 0xBD3AF235;
    D = A + (D << 10 | D >>> -10);
    C += (A ^ (D | ~B)) + X2 + 0x2AD7D2BB;
    C = D + (C << 15 | C >>> -15);
    B += (D ^ (C | ~A)) + X9 + 0xEB86D391;
    B = C + (B << 21 | B >>> -21);

    h0 += A;
    h1 += B;
    h2 += C;
    h3 += D;
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
        (byte) h3, (byte)(h3 >>> 8), (byte)(h3 >>> 16), (byte)(h3 >>> 24) };
  }

  protected void resetContext()
  {
    // magic MD5/RIPEMD128 initialisation constants
    h0 = 0x67452301;
    h1 = 0xEFCDAB89;
    h2 = 0x98BADCFE;
    h3 = 0x10325476;
  }

  public boolean selfTest()
  {
    if (valid == null)
      {
        String d = Util.toString(new MD5().digest());
        valid = Boolean.valueOf(DIGEST0.equals(d));
      }
    return valid.booleanValue();
  }
}
