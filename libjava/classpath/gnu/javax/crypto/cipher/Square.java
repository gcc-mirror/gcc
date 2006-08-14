/* Square.java -- 
   Copyright (C) 2001, 2002, 2003, 2006 Free Software Foundation, Inc.

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


package gnu.javax.crypto.cipher;

import gnu.java.security.Registry;
import gnu.java.security.util.Util;

import java.security.InvalidKeyException;
import java.util.ArrayList;
import java.util.Collections;
import java.util.Iterator;

/**
 * Square is a 128-bit key, 128-bit block cipher algorithm developed by Joan
 * Daemen, Lars Knudsen and Vincent Rijmen.
 * <p>
 * References:
 * <ol>
 * <li><a href="http://www.esat.kuleuven.ac.be/~rijmen/square/">The block
 * cipher Square</a>.<br>
 * <a href="mailto:daemen.j@protonworld.com">Joan Daemen</a>, <a
 * href="mailto:lars.knudsen@esat.kuleuven.ac.be">Lars Knudsen</a> and <a
 * href="mailto:vincent.rijmen@esat.kuleuven.ac.be">Vincent Rijmen</a>.</li>
 * </ol>
 */
public final class Square
    extends BaseCipher
{
  private static final int DEFAULT_BLOCK_SIZE = 16; // in bytes
  private static final int DEFAULT_KEY_SIZE = 16; // in bytes
  private static final int ROUNDS = 8;
  private static final int ROOT = 0x1F5; // for generating GF(2**8)
  private static final int[] OFFSET = new int[ROUNDS];
  private static final String Sdata =
      "\uB1CE\uC395\u5AAD\uE702\u4D44\uFB91\u0C87\uA150"
    + "\uCB67\u54DD\u468F\uE14E\uF0FD\uFCEB\uF9C4\u1A6E"
    + "\u5EF5\uCC8D\u1C56\u43FE\u0761\uF875\u59FF\u0322"
    + "\u8AD1\u13EE\u8800\u0E34\u1580\u94E3\uEDB5\u5323"
    + "\u4B47\u17A7\u9035\uABD8\uB8DF\u4F57\u9A92\uDB1B"
    + "\u3CC8\u9904\u8EE0\uD77D\u85BB\u402C\u3A45\uF142"
    + "\u6520\u4118\u7225\u9370\u3605\uF20B\uA379\uEC08"
    + "\u2731\u32B6\u7CB0\u0A73\u5B7B\uB781\uD20D\u6A26"
    + "\u9E58\u9C83\u74B3\uAC30\u7A69\u770F\uAE21\uDED0"
    + "\u2E97\u10A4\u98A8\uD468\u2D62\u296D\u1649\u76C7"
    + "\uE8C1\u9637\uE5CA\uF4E9\u6312\uC2A6\u14BC\uD328"
    + "\uAF2F\uE624\u52C6\uA009\uBD8C\uCF5D\u115F\u01C5"
    + "\u9F3D\uA29B\uC93B\uBE51\u191F\u3F5C\uB2EF\u4ACD"
    + "\uBFBA\u6F64\uD9F3\u3EB4\uAADC\uD506\uC07E\uF666"
    + "\u6C84\u7138\uB91D\u7F9D\u488B\u2ADA\uA533\u8239"
    + "\uD678\u86FA\uE42B\uA91E\u8960\u6BEA\u554C\uF7E2";
  /** Substitution boxes for encryption and decryption. */
  private static final byte[] Se = new byte[256];
  private static final byte[] Sd = new byte[256];
  /** Transposition boxes for encryption and decryption. */
  private static final int[] Te = new int[256];
  private static final int[] Td = new int[256];
  /**
   * KAT vector (from ecb_vk): I=87 KEY=00000000000000000000020000000000
   * CT=A9DF031B4E25E89F527EFFF89CB0BEBA
   */
  private static final byte[] KAT_KEY =
      Util.toBytesFromString("00000000000000000000020000000000");
  private static final byte[] KAT_CT =
      Util.toBytesFromString("A9DF031B4E25E89F527EFFF89CB0BEBA");
  /** caches the result of the correctness test, once executed. */
  private static Boolean valid;
  static
    {
      int i, j;
      // re-construct Se box values
      int limit = Sdata.length();
      char c1;
      for (i = 0, j = 0; i < limit; i++)
        {
          c1 = Sdata.charAt(i);
          Se[j++] = (byte)(c1 >>> 8);
          Se[j++] = (byte) c1;
        }
      // compute Sd box values
      for (i = 0; i < 256; i++)
        Sd[Se[i] & 0xFF] = (byte) i;
      // generate OFFSET values
      OFFSET[0] = 1;
      for (i = 1; i < ROUNDS; i++)
        {
          OFFSET[i] = mul(OFFSET[i - 1], 2);
          OFFSET[i - 1] <<= 24;
        }
      OFFSET[ROUNDS - 1] <<= 24;
      // generate Te and Td boxes if we're not reading their values
      // Notes:
      // (1) The function mul() computes the product of two elements of GF(2**8)
      // with ROOT as reduction polynomial.
      // (2) the values used in computing the Te and Td are the GF(2**8)
      // coefficients of the diffusion polynomial c(x) and its inverse
      // (modulo x**4 + 1) d(x), defined in sections 2.1 and 4 of the Square
      // paper.
      for (i = 0; i < 256; i++)
        {
          j = Se[i] & 0xFF;
          Te[i] = (Se[i & 3] == 0) ? 0
                                   : mul(j, 2) << 24
                                   | j << 16
                                   | j << 8
                                   | mul(j, 3);
          j = Sd[i] & 0xFF;
          Td[i] = (Sd[i & 3] == 0) ? 0
                                   : mul(j, 14) << 24
                                   | mul(j,  9) << 16
                                   | mul(j, 13) << 8
                                   | mul(j, 11);
        }
    }

  /** Trivial 0-arguments constructor. */
  public Square()
  {
    super(Registry.SQUARE_CIPHER, DEFAULT_BLOCK_SIZE, DEFAULT_KEY_SIZE);
  }

  private static void square(byte[] in, int i, byte[] out, int j, int[][] K,
                             int[] T, byte[] S)
  {
    int a = ((in[i++])        << 24
           | (in[i++] & 0xFF) << 16
           | (in[i++] & 0xFF) <<  8
           | (in[i++] & 0xFF)      ) ^ K[0][0];
    int b = ((in[i++])        << 24
           | (in[i++] & 0xFF) << 16
           | (in[i++] & 0xFF) <<  8
           | (in[i++] & 0xFF)      ) ^ K[0][1];
    int c = ((in[i++])        << 24
           | (in[i++] & 0xFF) << 16
           | (in[i++] & 0xFF) <<  8
           | (in[i++] & 0xFF)      ) ^ K[0][2];
    int d = ((in[i++])        << 24
           | (in[i++] & 0xFF) << 16
           | (in[i++] & 0xFF) <<  8
           | (in[i  ] & 0xFF)      ) ^ K[0][3];
    int r, aa, bb, cc, dd;
    for (r = 1; r < ROUNDS; r++)
      { // R - 1 full rounds
        aa =        T[(a >>> 24)       ]
           ^ rot32R(T[(b >>> 24)       ], 8)
           ^ rot32R(T[(c >>> 24)       ], 16)
           ^ rot32R(T[(d >>> 24)       ], 24) ^ K[r][0];
        bb =        T[(a >>> 16) & 0xFF]
           ^ rot32R(T[(b >>> 16) & 0xFF], 8)
           ^ rot32R(T[(c >>> 16) & 0xFF], 16)
           ^ rot32R(T[(d >>> 16) & 0xFF], 24) ^ K[r][1];
        cc =        T[(a >>>  8) & 0xFF]
           ^ rot32R(T[(b >>>  8) & 0xFF], 8)
           ^ rot32R(T[(c >>>  8) & 0xFF], 16)
           ^ rot32R(T[(d >>>  8) & 0xFF], 24) ^ K[r][2];
        dd =        T[ a         & 0xFF]
           ^ rot32R(T[ b         & 0xFF], 8)
           ^ rot32R(T[ c         & 0xFF], 16)
           ^ rot32R(T[ d         & 0xFF], 24) ^ K[r][3];
        a = aa;
        b = bb;
        c = cc;
        d = dd;
      }
    // last round (diffusion becomes only transposition)
    aa = ((S[(a >>> 24)       ]       ) << 24
        | (S[(b >>> 24)       ] & 0xFF) << 16
        | (S[(c >>> 24)       ] & 0xFF) <<  8
        | (S[(d >>> 24)       ] & 0xFF)      ) ^ K[r][0];
    bb = ((S[(a >>> 16) & 0xFF]       ) << 24
        | (S[(b >>> 16) & 0xFF] & 0xFF) << 16
        | (S[(c >>> 16) & 0xFF] & 0xFF) <<  8
        | (S[(d >>> 16) & 0xFF] & 0xFF)      ) ^ K[r][1];
    cc = ((S[(a >>>  8) & 0xFF]       ) << 24
        | (S[(b >>>  8) & 0xFF] & 0xFF) << 16
        | (S[(c >>>  8) & 0xFF] & 0xFF) <<  8
        | (S[(d >>>  8) & 0xFF] & 0xFF)      ) ^ K[r][2];
    dd = ((S[ a         & 0xFF]       ) << 24
        | (S[ b         & 0xFF] & 0xFF) << 16
        | (S[ c         & 0xFF] & 0xFF) <<  8
        | (S[ d         & 0xFF] & 0xFF)      ) ^ K[r][3];
    out[j++] = (byte)(aa >>> 24);
    out[j++] = (byte)(aa >>> 16);
    out[j++] = (byte)(aa >>> 8);
    out[j++] = (byte) aa;
    out[j++] = (byte)(bb >>> 24);
    out[j++] = (byte)(bb >>> 16);
    out[j++] = (byte)(bb >>> 8);
    out[j++] = (byte) bb;
    out[j++] = (byte)(cc >>> 24);
    out[j++] = (byte)(cc >>> 16);
    out[j++] = (byte)(cc >>> 8);
    out[j++] = (byte) cc;
    out[j++] = (byte)(dd >>> 24);
    out[j++] = (byte)(dd >>> 16);
    out[j++] = (byte)(dd >>> 8);
    out[j  ] = (byte) dd;
  }

  /**
   * Applies the Theta function to an input <i>in</i> in order to produce in
   * <i>out</i> an internal session sub-key.
   * <p>
   * Both <i>in</i> and <i>out</i> are arrays of four ints.
   * <p>
   * Pseudo-code is:
   * <pre>
   * for (i = 0; i &lt; 4; i++)
   *   {
   *     out[i] = 0;
   *     for (j = 0, n = 24; j &lt; 4; j++, n -= 8)
   *       {
   *         k = mul(in[i] &gt;&gt;&gt; 24, G[0][j]) &circ; mul(in[i] &gt;&gt;&gt; 16, G[1][j])
   *             &circ; mul(in[i] &gt;&gt;&gt; 8, G[2][j]) &circ; mul(in[i], G[3][j]);
   *         out[i] &circ;= k &lt;&lt; n;
   *       }
   *   }
   * </pre>
   */
  private static void transform(int[] in, int[] out)
  {
    int l3, l2, l1, l0, m;
    for (int i = 0; i < 4; i++)
      {
        l3 = in[i];
        l2 = l3 >>> 8;
        l1 = l3 >>> 16;
        l0 = l3 >>> 24;
        m = ((mul(l0, 2) ^ mul(l1, 3) ^ l2 ^ l3) & 0xFF) << 24;
        m ^= ((l0 ^ mul(l1, 2) ^ mul(l2, 3) ^ l3) & 0xFF) << 16;
        m ^= ((l0 ^ l1 ^ mul(l2, 2) ^ mul(l3, 3)) & 0xFF) << 8;
        m ^= ((mul(l0, 3) ^ l1 ^ l2 ^ mul(l3, 2)) & 0xFF);
        out[i] = m;
      }
  }

  /**
   * Left rotate a 32-bit chunk.
   * 
   * @param x the 32-bit data to rotate
   * @param s number of places to left-rotate by
   * @return the newly permutated value.
   */
  private static int rot32L(int x, int s)
  {
    return x << s | x >>> (32 - s);
  }

  /**
   * Right rotate a 32-bit chunk.
   * 
   * @param x the 32-bit data to rotate
   * @param s number of places to right-rotate by
   * @return the newly permutated value.
   */
  private static int rot32R(int x, int s)
  {
    return x >>> s | x << (32 - s);
  }

  /**
   * Returns the product of two binary numbers a and b, using the generator ROOT
   * as the modulus: p = (a * b) mod ROOT. ROOT Generates a suitable Galois
   * Field in GF(2**8).
   * <p>
   * For best performance call it with abs(b) &lt; abs(a).
   * 
   * @param a operand for multiply.
   * @param b operand for multiply.
   * @return the result of (a * b) % ROOT.
   */
  private static final int mul(int a, int b)
  {
    if (a == 0)
      return 0;
    a &= 0xFF;
    b &= 0xFF;
    int result = 0;
    while (b != 0)
      {
        if ((b & 0x01) != 0)
          result ^= a;
        b >>>= 1;
        a <<= 1;
        if (a > 0xFF)
          a ^= ROOT;
      }
    return result & 0xFF;
  }

  public Object clone()
  {
    Square result = new Square();
    result.currentBlockSize = this.currentBlockSize;

    return result;
  }

  public Iterator blockSizes()
  {
    ArrayList al = new ArrayList();
    al.add(Integer.valueOf(DEFAULT_BLOCK_SIZE));

    return Collections.unmodifiableList(al).iterator();
  }

  public Iterator keySizes()
  {
    ArrayList al = new ArrayList();
    al.add(Integer.valueOf(DEFAULT_KEY_SIZE));

    return Collections.unmodifiableList(al).iterator();
  }

  public Object makeKey(byte[] uk, int bs) throws InvalidKeyException
  {
    if (bs != DEFAULT_BLOCK_SIZE)
      throw new IllegalArgumentException();
    if (uk == null)
      throw new InvalidKeyException("Empty key");
    if (uk.length != DEFAULT_KEY_SIZE)
      throw new InvalidKeyException("Key is not 128-bit.");
    int[][] Ke = new int[ROUNDS + 1][4];
    int[][] Kd = new int[ROUNDS + 1][4];
    int[][] tK = new int[ROUNDS + 1][4];
    int i = 0;
    Ke[0][0] = (uk[i++] & 0xFF) << 24
             | (uk[i++] & 0xFF) << 16
             | (uk[i++] & 0xFF) << 8
             | (uk[i++] & 0xFF);
    tK[0][0] = Ke[0][0];
    Ke[0][1] = (uk[i++] & 0xFF) << 24
             | (uk[i++] & 0xFF) << 16
             | (uk[i++] & 0xFF) << 8
             | (uk[i++] & 0xFF);
    tK[0][1] = Ke[0][1];
    Ke[0][2] = (uk[i++] & 0xFF) << 24
             | (uk[i++] & 0xFF) << 16
             | (uk[i++] & 0xFF) << 8
             | (uk[i++] & 0xFF);
    tK[0][2] = Ke[0][2];
    Ke[0][3] = (uk[i++] & 0xFF) << 24
             | (uk[i++] & 0xFF) << 16
             | (uk[i++] & 0xFF) << 8
             | (uk[i  ] & 0xFF);
    tK[0][3] = Ke[0][3];
    int j;
    for (i = 1, j = 0; i < ROUNDS + 1; i++, j++)
      {
        tK[i][0] = tK[j][0] ^ rot32L(tK[j][3], 8) ^ OFFSET[j];
        tK[i][1] = tK[j][1] ^ tK[i][0];
        tK[i][2] = tK[j][2] ^ tK[i][1];
        tK[i][3] = tK[j][3] ^ tK[i][2];
        System.arraycopy(tK[i], 0, Ke[i], 0, 4);
        transform(Ke[j], Ke[j]);
      }
    for (i = 0; i < ROUNDS; i++)
      System.arraycopy(tK[ROUNDS - i], 0, Kd[i], 0, 4);
    transform(tK[0], Kd[ROUNDS]);
    return new Object[] { Ke, Kd };
  }

  public void encrypt(byte[] in, int i, byte[] out, int j, Object k, int bs)
  {
    if (bs != DEFAULT_BLOCK_SIZE)
      throw new IllegalArgumentException();
    int[][] K = (int[][])((Object[]) k)[0];
    square(in, i, out, j, K, Te, Se);
  }

  public void decrypt(byte[] in, int i, byte[] out, int j, Object k, int bs)
  {
    if (bs != DEFAULT_BLOCK_SIZE)
      throw new IllegalArgumentException();
    int[][] K = (int[][])((Object[]) k)[1];
    square(in, i, out, j, K, Td, Sd);
  }

  public boolean selfTest()
  {
    if (valid == null)
      {
        boolean result = super.selfTest(); // do symmetry tests
        if (result)
          result = testKat(KAT_KEY, KAT_CT);
        valid = Boolean.valueOf(result);
      }
    return valid.booleanValue();
  }
}
