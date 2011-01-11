/* MD4.java --
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
 * An implementation of Ron Rivest's MD4 message digest algorithm.
 * <p>
 * MD4 was the precursor to the stronger {@link gnu.java.security.hash.MD5}
 * algorithm, and while not considered cryptograpically secure itself, MD4 is
 * in use in various applications. It is slightly faster than MD5.
 * <p>
 * References:
 * <ol>
 *    <li>The <a href="http://www.ietf.org/rfc/rfc1320.txt">MD4</a>
 *    Message-Digest Algorithm.<br>
 *    R. Rivest.</li>
 * </ol>
 *
 * @author Casey Marshall (rsdio@metastatic.org)
 */
public class MD4
    extends BaseHash
{
  /** An MD4 message digest is always 128-bits long, or 16 bytes. */
  private static final int DIGEST_LENGTH = 16;

  /** The MD4 algorithm operates on 512-bit blocks, or 64 bytes. */
  private static final int BLOCK_LENGTH = 64;

  private static final int A = 0x67452301;

  private static final int B = 0xefcdab89;

  private static final int C = 0x98badcfe;

  private static final int D = 0x10325476;

  /** The output of this message digest when no data has been input. */
  private static final String DIGEST0 = "31D6CFE0D16AE931B73C59D7E0C089C0";

  /** caches the result of the correctness test, once executed. */
  private static Boolean valid;

  private int a, b, c, d;

  /**
   * Public constructor. Initializes the chaining variables, sets the byte
   * count to <code>0</code>, and creates a new block of <code>512</code> bits.
   */
  public MD4()
  {
    super(Registry.MD4_HASH, DIGEST_LENGTH, BLOCK_LENGTH);
  }

  /**
   * Trivial private constructor for cloning purposes.
   *
   * @param that the instance to clone.
   */
  private MD4(MD4 that)
  {
    this();

    this.a = that.a;
    this.b = that.b;
    this.c = that.c;
    this.d = that.d;
    this.count = that.count;
    this.buffer = (byte[]) that.buffer.clone();
  }

  public Object clone()
  {
    return new MD4(this);
  }

  protected byte[] getResult()
  {
    return new byte[] {
        (byte) a, (byte)(a >>> 8), (byte)(a >>> 16), (byte)(a >>> 24),
        (byte) b, (byte)(b >>> 8), (byte)(b >>> 16), (byte)(b >>> 24),
        (byte) c, (byte)(c >>> 8), (byte)(c >>> 16), (byte)(c >>> 24),
        (byte) d, (byte)(d >>> 8), (byte)(d >>> 16), (byte)(d >>> 24) };
  }

  protected void resetContext()
  {
    a = A;
    b = B;
    c = C;
    d = D;
  }

  public boolean selfTest()
  {
    if (valid == null)
      {
        String d = Util.toString(new MD4().digest());
        valid = Boolean.valueOf(DIGEST0.equals(d));
      }
    return valid.booleanValue();
  }

  protected byte[] padBuffer()
  {
    int n = (int)(count % BLOCK_LENGTH);
    int padding = (n < 56) ? (56 - n) : (120 - n);
    byte[] pad = new byte[padding + 8];
    pad[0] = (byte) 0x80;
    long bits = count << 3;
    pad[padding++] = (byte) bits;
    pad[padding++] = (byte)(bits >>> 8);
    pad[padding++] = (byte)(bits >>> 16);
    pad[padding++] = (byte)(bits >>> 24);
    pad[padding++] = (byte)(bits >>> 32);
    pad[padding++] = (byte)(bits >>> 40);
    pad[padding++] = (byte)(bits >>> 48);
    pad[padding  ] = (byte)(bits >>> 56);
    return pad;
  }

  protected void transform(byte[] in, int i)
  {
    int X0 = (in[i++] & 0xFF)
           | (in[i++] & 0xFF) << 8
           | (in[i++] & 0xFF) << 16
           | in[i++] << 24;
    int X1 = (in[i++] & 0xFF)
           | (in[i++] & 0xFF) << 8
           | (in[i++] & 0xFF) << 16
           | in[i++] << 24;
    int X2 = (in[i++] & 0xFF)
           | (in[i++] & 0xFF) << 8
           | (in[i++] & 0xFF) << 16
           | in[i++] << 24;
    int X3 = (in[i++] & 0xFF)
           | (in[i++] & 0xFF) << 8
           | (in[i++] & 0xFF) << 16
           | in[i++] << 24;
    int X4 = (in[i++] & 0xFF)
           | (in[i++] & 0xFF) << 8
           | (in[i++] & 0xFF) << 16
           | in[i++] << 24;
    int X5 = (in[i++] & 0xFF)
           | (in[i++] & 0xFF) << 8
           | (in[i++] & 0xFF) << 16
           | in[i++] << 24;
    int X6 = (in[i++] & 0xFF)
           | (in[i++] & 0xFF) << 8
           | (in[i++] & 0xFF) << 16
           | in[i++] << 24;
    int X7 = (in[i++] & 0xFF)
           | (in[i++] & 0xFF) << 8
           | (in[i++] & 0xFF) << 16
           | in[i++] << 24;
    int X8 = (in[i++] & 0xFF)
           | (in[i++] & 0xFF) << 8
           | (in[i++] & 0xFF) << 16
           | in[i++] << 24;
    int X9 = (in[i++] & 0xFF)
           | (in[i++] & 0xFF) << 8
           | (in[i++] & 0xFF) << 16
           | in[i++] << 24;
    int X10 = (in[i++] & 0xFF)
            | (in[i++] & 0xFF) << 8
            | (in[i++] & 0xFF) << 16
            | in[i++] << 24;
    int X11 = (in[i++] & 0xFF)
            | (in[i++] & 0xFF) << 8
            | (in[i++] & 0xFF) << 16
            | in[i++] << 24;
    int X12 = (in[i++] & 0xFF)
            | (in[i++] & 0xFF) << 8
            | (in[i++] & 0xFF) << 16
            | in[i++] << 24;
    int X13 = (in[i++] & 0xFF)
            | (in[i++] & 0xFF) << 8
            | (in[i++] & 0xFF) << 16
            | in[i++] << 24;
    int X14 = (in[i++] & 0xFF)
            | (in[i++] & 0xFF) << 8
            | (in[i++] & 0xFF) << 16
            | in[i++] << 24;
    int X15 = (in[i++] & 0xFF)
            | (in[i++] & 0xFF) << 8
            | (in[i++] & 0xFF) << 16
            | in[i] << 24;
    int aa, bb, cc, dd;
    aa = a;
    bb = b;
    cc = c;
    dd = d;

    aa += ((bb & cc) | ((~bb) & dd)) + X0;
    aa = aa << 3 | aa >>> -3;
    dd += ((aa & bb) | ((~aa) & cc)) + X1;
    dd = dd << 7 | dd >>> -7;
    cc += ((dd & aa) | ((~dd) & bb)) + X2;
    cc = cc << 11 | cc >>> -11;
    bb += ((cc & dd) | ((~cc) & aa)) + X3;
    bb = bb << 19 | bb >>> -19;
    aa += ((bb & cc) | ((~bb) & dd)) + X4;
    aa = aa << 3 | aa >>> -3;
    dd += ((aa & bb) | ((~aa) & cc)) + X5;
    dd = dd << 7 | dd >>> -7;
    cc += ((dd & aa) | ((~dd) & bb)) + X6;
    cc = cc << 11 | cc >>> -11;
    bb += ((cc & dd) | ((~cc) & aa)) + X7;
    bb = bb << 19 | bb >>> -19;
    aa += ((bb & cc) | ((~bb) & dd)) + X8;
    aa = aa << 3 | aa >>> -3;
    dd += ((aa & bb) | ((~aa) & cc)) + X9;
    dd = dd << 7 | dd >>> -7;
    cc += ((dd & aa) | ((~dd) & bb)) + X10;
    cc = cc << 11 | cc >>> -11;
    bb += ((cc & dd) | ((~cc) & aa)) + X11;
    bb = bb << 19 | bb >>> -19;
    aa += ((bb & cc) | ((~bb) & dd)) + X12;
    aa = aa << 3 | aa >>> -3;
    dd += ((aa & bb) | ((~aa) & cc)) + X13;
    dd = dd << 7 | dd >>> -7;
    cc += ((dd & aa) | ((~dd) & bb)) + X14;
    cc = cc << 11 | cc >>> -11;
    bb += ((cc & dd) | ((~cc) & aa)) + X15;
    bb = bb << 19 | bb >>> -19;

    aa += ((bb & (cc | dd)) | (cc & dd)) + X0 + 0x5a827999;
    aa = aa << 3 | aa >>> -3;
    dd += ((aa & (bb | cc)) | (bb & cc)) + X4 + 0x5a827999;
    dd = dd << 5 | dd >>> -5;
    cc += ((dd & (aa | bb)) | (aa & bb)) + X8 + 0x5a827999;
    cc = cc << 9 | cc >>> -9;
    bb += ((cc & (dd | aa)) | (dd & aa)) + X12 + 0x5a827999;
    bb = bb << 13 | bb >>> -13;
    aa += ((bb & (cc | dd)) | (cc & dd)) + X1 + 0x5a827999;
    aa = aa << 3 | aa >>> -3;
    dd += ((aa & (bb | cc)) | (bb & cc)) + X5 + 0x5a827999;
    dd = dd << 5 | dd >>> -5;
    cc += ((dd & (aa | bb)) | (aa & bb)) + X9 + 0x5a827999;
    cc = cc << 9 | cc >>> -9;
    bb += ((cc & (dd | aa)) | (dd & aa)) + X13 + 0x5a827999;
    bb = bb << 13 | bb >>> -13;
    aa += ((bb & (cc | dd)) | (cc & dd)) + X2 + 0x5a827999;
    aa = aa << 3 | aa >>> -3;
    dd += ((aa & (bb | cc)) | (bb & cc)) + X6 + 0x5a827999;
    dd = dd << 5 | dd >>> -5;
    cc += ((dd & (aa | bb)) | (aa & bb)) + X10 + 0x5a827999;
    cc = cc << 9 | cc >>> -9;
    bb += ((cc & (dd | aa)) | (dd & aa)) + X14 + 0x5a827999;
    bb = bb << 13 | bb >>> -13;
    aa += ((bb & (cc | dd)) | (cc & dd)) + X3 + 0x5a827999;
    aa = aa << 3 | aa >>> -3;
    dd += ((aa & (bb | cc)) | (bb & cc)) + X7 + 0x5a827999;
    dd = dd << 5 | dd >>> -5;
    cc += ((dd & (aa | bb)) | (aa & bb)) + X11 + 0x5a827999;
    cc = cc << 9 | cc >>> -9;
    bb += ((cc & (dd | aa)) | (dd & aa)) + X15 + 0x5a827999;
    bb = bb << 13 | bb >>> -13;

    aa += (bb ^ cc ^ dd) + X0 + 0x6ed9eba1;
    aa = aa << 3 | aa >>> -3;
    dd += (aa ^ bb ^ cc) + X8 + 0x6ed9eba1;
    dd = dd << 9 | dd >>> -9;
    cc += (dd ^ aa ^ bb) + X4 + 0x6ed9eba1;
    cc = cc << 11 | cc >>> -11;
    bb += (cc ^ dd ^ aa) + X12 + 0x6ed9eba1;
    bb = bb << 15 | bb >>> -15;
    aa += (bb ^ cc ^ dd) + X2 + 0x6ed9eba1;
    aa = aa << 3 | aa >>> -3;
    dd += (aa ^ bb ^ cc) + X10 + 0x6ed9eba1;
    dd = dd << 9 | dd >>> -9;
    cc += (dd ^ aa ^ bb) + X6 + 0x6ed9eba1;
    cc = cc << 11 | cc >>> -11;
    bb += (cc ^ dd ^ aa) + X14 + 0x6ed9eba1;
    bb = bb << 15 | bb >>> -15;
    aa += (bb ^ cc ^ dd) + X1 + 0x6ed9eba1;
    aa = aa << 3 | aa >>> -3;
    dd += (aa ^ bb ^ cc) + X9 + 0x6ed9eba1;
    dd = dd << 9 | dd >>> -9;
    cc += (dd ^ aa ^ bb) + X5 + 0x6ed9eba1;
    cc = cc << 11 | cc >>> -11;
    bb += (cc ^ dd ^ aa) + X13 + 0x6ed9eba1;
    bb = bb << 15 | bb >>> -15;
    aa += (bb ^ cc ^ dd) + X3 + 0x6ed9eba1;
    aa = aa << 3 | aa >>> -3;
    dd += (aa ^ bb ^ cc) + X11 + 0x6ed9eba1;
    dd = dd << 9 | dd >>> -9;
    cc += (dd ^ aa ^ bb) + X7 + 0x6ed9eba1;
    cc = cc << 11 | cc >>> -11;
    bb += (cc ^ dd ^ aa) + X15 + 0x6ed9eba1;
    bb = bb << 15 | bb >>> -15;

    a += aa;
    b += bb;
    c += cc;
    d += dd;
  }
}
