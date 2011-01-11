/* Sha512.java --
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


package gnu.java.security.hash;

import gnu.java.security.Registry;
import gnu.java.security.util.Util;

/**
 * Implementation of SHA2-3 [SHA-512] per the IETF Draft Specification.
 * <p>
 * References:
 * <ol>
 *    <li><a href="http://ftp.ipv4.heanet.ie/pub/ietf/internet-drafts/draft-ietf-ipsec-ciph-aes-cbc-03.txt">
 *    Descriptions of SHA-256, SHA-384, and SHA-512</a>,</li>
 *    <li>http://csrc.nist.gov/cryptval/shs/sha256-384-512.pdf</li>
 * </ol>
 */
public class Sha512
    extends BaseHash
{
  private static final long[] k = {
      0x428a2f98d728ae22L, 0x7137449123ef65cdL, 0xb5c0fbcfec4d3b2fL,
      0xe9b5dba58189dbbcL, 0x3956c25bf348b538L, 0x59f111f1b605d019L,
      0x923f82a4af194f9bL, 0xab1c5ed5da6d8118L, 0xd807aa98a3030242L,
      0x12835b0145706fbeL, 0x243185be4ee4b28cL, 0x550c7dc3d5ffb4e2L,
      0x72be5d74f27b896fL, 0x80deb1fe3b1696b1L, 0x9bdc06a725c71235L,
      0xc19bf174cf692694L, 0xe49b69c19ef14ad2L, 0xefbe4786384f25e3L,
      0x0fc19dc68b8cd5b5L, 0x240ca1cc77ac9c65L, 0x2de92c6f592b0275L,
      0x4a7484aa6ea6e483L, 0x5cb0a9dcbd41fbd4L, 0x76f988da831153b5L,
      0x983e5152ee66dfabL, 0xa831c66d2db43210L, 0xb00327c898fb213fL,
      0xbf597fc7beef0ee4L, 0xc6e00bf33da88fc2L, 0xd5a79147930aa725L,
      0x06ca6351e003826fL, 0x142929670a0e6e70L, 0x27b70a8546d22ffcL,
      0x2e1b21385c26c926L, 0x4d2c6dfc5ac42aedL, 0x53380d139d95b3dfL,
      0x650a73548baf63deL, 0x766a0abb3c77b2a8L, 0x81c2c92e47edaee6L,
      0x92722c851482353bL, 0xa2bfe8a14cf10364L, 0xa81a664bbc423001L,
      0xc24b8b70d0f89791L, 0xc76c51a30654be30L, 0xd192e819d6ef5218L,
      0xd69906245565a910L, 0xf40e35855771202aL, 0x106aa07032bbd1b8L,
      0x19a4c116b8d2d0c8L, 0x1e376c085141ab53L, 0x2748774cdf8eeb99L,
      0x34b0bcb5e19b48a8L, 0x391c0cb3c5c95a63L, 0x4ed8aa4ae3418acbL,
      0x5b9cca4f7763e373L, 0x682e6ff3d6b2b8a3L, 0x748f82ee5defb2fcL,
      0x78a5636f43172f60L, 0x84c87814a1f0ab72L, 0x8cc702081a6439ecL,
      0x90befffa23631e28L, 0xa4506cebde82bde9L, 0xbef9a3f7b2c67915L,
      0xc67178f2e372532bL, 0xca273eceea26619cL, 0xd186b8c721c0c207L,
      0xeada7dd6cde0eb1eL, 0xf57d4f7fee6ed178L, 0x06f067aa72176fbaL,
      0x0a637dc5a2c898a6L, 0x113f9804bef90daeL, 0x1b710b35131c471bL,
      0x28db77f523047d84L, 0x32caab7b40c72493L, 0x3c9ebe0a15c9bebcL,
      0x431d67c49c100d4cL, 0x4cc5d4becb3e42b6L, 0x597f299cfc657e2aL,
      0x5fcb6fab3ad6faecL, 0x6c44198c4a475817L };

  private static final int BLOCK_SIZE = 128; // inner block size in bytes

  private static final String DIGEST0 =
      "DDAF35A193617ABACC417349AE20413112E6FA4E89A97EA20A9EEEE64B55D39A"
    + "2192992A274FC1A836BA3C23A3FEEBBD454D4423643CE80E2A9AC94FA54CA49F";

  private static final long[] w = new long[80];

  /** caches the result of the correctness test, once executed. */
  private static Boolean valid;

  /** 512-bit interim result. */
  private long h0, h1, h2, h3, h4, h5, h6, h7;

  /** Trivial 0-arguments constructor. */
  public Sha512()
  {
    super(Registry.SHA512_HASH, 64, BLOCK_SIZE);
  }

  /**
   * Private constructor for cloning purposes.
   *
   * @param md the instance to clone.
   */
  private Sha512(Sha512 md)
  {
    this();

    this.h0 = md.h0;
    this.h1 = md.h1;
    this.h2 = md.h2;
    this.h3 = md.h3;
    this.h4 = md.h4;
    this.h5 = md.h5;
    this.h6 = md.h6;
    this.h7 = md.h7;
    this.count = md.count;
    this.buffer = (byte[]) md.buffer.clone();
  }

  public static final long[] G(long hh0, long hh1, long hh2, long hh3,
                               long hh4, long hh5, long hh6, long hh7,
                               byte[] in, int offset)
  {
    return sha(hh0, hh1, hh2, hh3, hh4, hh5, hh6, hh7, in, offset);
  }

  public Object clone()
  {
    return new Sha512(this);
  }

  protected void transform(byte[] in, int offset)
  {
    long[] result = sha(h0, h1, h2, h3, h4, h5, h6, h7, in, offset);
    h0 = result[0];
    h1 = result[1];
    h2 = result[2];
    h3 = result[3];
    h4 = result[4];
    h5 = result[5];
    h6 = result[6];
    h7 = result[7];
  }

  protected byte[] padBuffer()
  {
    int n = (int)(count % BLOCK_SIZE);
    int padding = (n < 112) ? (112 - n) : (240 - n);
    byte[] result = new byte[padding + 16];
    // padding is always binary 1 followed by binary 0s
    result[0] = (byte) 0x80;
    // save number of bits, casting the long to an array of 8 bytes
    // TODO: FIX Only ~35 bits of 128 bit counter usable this way
    long bits = count << 3;
    padding += 8;
    result[padding++] = (byte)(bits >>> 56);
    result[padding++] = (byte)(bits >>> 48);
    result[padding++] = (byte)(bits >>> 40);
    result[padding++] = (byte)(bits >>> 32);
    result[padding++] = (byte)(bits >>> 24);
    result[padding++] = (byte)(bits >>> 16);
    result[padding++] = (byte)(bits >>> 8);
    result[padding  ] = (byte) bits;
    return result;
  }

  protected byte[] getResult()
  {
    return new byte[] {
        (byte)(h0 >>> 56), (byte)(h0 >>> 48), (byte)(h0 >>> 40), (byte)(h0 >>> 32),
        (byte)(h0 >>> 24), (byte)(h0 >>> 16), (byte)(h0 >>>  8), (byte) h0,
        (byte)(h1 >>> 56), (byte)(h1 >>> 48), (byte)(h1 >>> 40), (byte)(h1 >>> 32),
        (byte)(h1 >>> 24), (byte)(h1 >>> 16), (byte)(h1 >>>  8), (byte) h1,
        (byte)(h2 >>> 56), (byte)(h2 >>> 48), (byte)(h2 >>> 40), (byte)(h2 >>> 32),
        (byte)(h2 >>> 24), (byte)(h2 >>> 16), (byte)(h2 >>> 8), (byte) h2,
        (byte)(h3 >>> 56), (byte)(h3 >>> 48), (byte)(h3 >>> 40), (byte)(h3 >>> 32),
        (byte)(h3 >>> 24), (byte)(h3 >>> 16), (byte)(h3 >>>  8), (byte) h3,
        (byte)(h4 >>> 56), (byte)(h4 >>> 48), (byte)(h4 >>> 40), (byte)(h4 >>> 32),
        (byte)(h4 >>> 24), (byte)(h4 >>> 16), (byte)(h4 >>>  8), (byte) h4,
        (byte)(h5 >>> 56), (byte)(h5 >>> 48), (byte)(h5 >>> 40), (byte)(h5 >>> 32),
        (byte)(h5 >>> 24), (byte)(h5 >>> 16), (byte)(h5 >>> 8), (byte) h5,
        (byte)(h6 >>> 56), (byte)(h6 >>> 48), (byte)(h6 >>> 40), (byte)(h6 >>> 32),
        (byte)(h6 >>> 24), (byte)(h6 >>> 16), (byte)(h6 >>>  8), (byte) h6,
        (byte)(h7 >>> 56), (byte)(h7 >>> 48), (byte)(h7 >>> 40), (byte)(h7 >>> 32),
        (byte)(h7 >>> 24), (byte)(h7 >>> 16), (byte)(h7 >>>  8), (byte) h7 };
  }

  protected void resetContext()
  {
    // magic SHA-512 initialisation constants
    h0 = 0x6a09e667f3bcc908L;
    h1 = 0xbb67ae8584caa73bL;
    h2 = 0x3c6ef372fe94f82bL;
    h3 = 0xa54ff53a5f1d36f1L;
    h4 = 0x510e527fade682d1L;
    h5 = 0x9b05688c2b3e6c1fL;
    h6 = 0x1f83d9abfb41bd6bL;
    h7 = 0x5be0cd19137e2179L;
  }

  public boolean selfTest()
  {
    if (valid == null)
      {
        Sha512 md = new Sha512();
        md.update((byte) 0x61); // a
        md.update((byte) 0x62); // b
        md.update((byte) 0x63); // c
        String result = Util.toString(md.digest());
        valid = Boolean.valueOf(DIGEST0.equals(result));
      }
    return valid.booleanValue();
  }

  private static synchronized final long[] sha(long hh0, long hh1, long hh2,
                                               long hh3, long hh4, long hh5,
                                               long hh6, long hh7, byte[] in,
                                               int offset)
  {
    long A = hh0;
    long B = hh1;
    long C = hh2;
    long D = hh3;
    long E = hh4;
    long F = hh5;
    long G = hh6;
    long H = hh7;
    long T, T2;
    int r;
    for (r = 0; r < 16; r++)
      w[r] =  (long) in[offset++]         << 56
           | ((long) in[offset++] & 0xFF) << 48
           | ((long) in[offset++] & 0xFF) << 40
           | ((long) in[offset++] & 0xFF) << 32
           | ((long) in[offset++] & 0xFF) << 24
           | ((long) in[offset++] & 0xFF) << 16
           | ((long) in[offset++] & 0xFF) << 8
           | ((long) in[offset++] & 0xFF);
    for (r = 16; r < 80; r++)
      {
        T = w[r - 2];
        T2 = w[r - 15];
        w[r] = (((T >>> 19) | (T << 45)) ^ ((T >>> 61) | (T << 3)) ^ (T >>> 6))
               + w[r - 7]
               + (((T2 >>> 1) | (T2 << 63))
                   ^ ((T2 >>> 8) | (T2 << 56))
                   ^ (T2 >>> 7))
               + w[r - 16];
      }
    for (r = 0; r < 80; r++)
      {
        T = H
            + (((E >>> 14) | (E << 50))
                ^ ((E >>> 18) | (E << 46))
                ^ ((E >>> 41) | (E << 23)))
            + ((E & F) ^ ((~E) & G)) + k[r] + w[r];
        T2 = (((A >>> 28) | (A << 36))
               ^ ((A >>> 34) | (A << 30))
               ^ ((A >>> 39) | (A << 25)))
             + ((A & B) ^ (A & C) ^ (B & C));
        H = G;
        G = F;
        F = E;
        E = D + T;
        D = C;
        C = B;
        B = A;
        A = T + T2;
      }
    return new long[] {
        hh0 + A, hh1 + B, hh2 + C, hh3 + D,
        hh4 + E, hh5 + F, hh6 + G, hh7 + H };
  }
}
