/* Whirlpool.java --
   Copyright (C) 2001, 2002, 2006, 2010 Free Software Foundation, Inc.

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

import gnu.java.lang.CPStringBuilder;

import gnu.java.security.Configuration;
import gnu.java.security.Registry;
import gnu.java.security.util.Util;

import java.util.logging.Logger;

/**
 * Whirlpool, a new 512-bit hashing function operating on messages less than
 * 2 ** 256 bits in length. The function structure is designed according to the
 * Wide Trail strategy and permits a wide variety of implementation trade-offs.
 * <p>
 * This implementation is of Whirlpool Version 3, described in [1] last revised
 * on May 24th, 2003.
 * <p>
 * <b>IMPORTANT</b>: This implementation is not thread-safe.
 * <p>
 * References:
 * <ol>
 *    <li><a href="http://planeta.terra.com.br/informatica/paulobarreto/WhirlpoolPage.html">
 *    The WHIRLPOOL Hashing Function</a>.<br>
 *    <a href="mailto:paulo.barreto@terra.com.br">Paulo S.L.M. Barreto</a> and
 *    <a href="mailto:vincent.rijmen@iaik.tugraz.at">Vincent Rijmen</a>.</li>
 * </ol>
 */
public final class Whirlpool
    extends BaseHash
{
  private static final Logger log = Configuration.DEBUG ?
                        Logger.getLogger(Whirlpool.class.getName()) : null;

  private static final int BLOCK_SIZE = 64; // inner block size in bytes

  /** The digest of the 0-bit long message. */
  private static final String DIGEST0 =
      "19FA61D75522A4669B44E39C1D2E1726C530232130D407F89AFEE0964997F7A7"
    + "3E83BE698B288FEBCF88E3E03C4F0757EA8964E59B63D93708B138CC42A66EB3";

  /** Default number of rounds. */
  private static final int R = 10;

  /** Whirlpool S-box; p. 19. */
  private static final String S_box = // p. 19 [WHIRLPOOL]
      "\u1823\uc6E8\u87B8\u014F\u36A6\ud2F5\u796F\u9152"
    + "\u60Bc\u9B8E\uA30c\u7B35\u1dE0\ud7c2\u2E4B\uFE57"
    + "\u1577\u37E5\u9FF0\u4AdA\u58c9\u290A\uB1A0\u6B85"
    + "\uBd5d\u10F4\ucB3E\u0567\uE427\u418B\uA77d\u95d8"
    + "\uFBEE\u7c66\udd17\u479E\ucA2d\uBF07\uAd5A\u8333"
    + "\u6302\uAA71\uc819\u49d9\uF2E3\u5B88\u9A26\u32B0"
    + "\uE90F\ud580\uBEcd\u3448\uFF7A\u905F\u2068\u1AAE"
    + "\uB454\u9322\u64F1\u7312\u4008\uc3Ec\udBA1\u8d3d"
    + "\u9700\ucF2B\u7682\ud61B\uB5AF\u6A50\u45F3\u30EF"
    + "\u3F55\uA2EA\u65BA\u2Fc0\udE1c\uFd4d\u9275\u068A"
    + "\uB2E6\u0E1F\u62d4\uA896\uF9c5\u2559\u8472\u394c"
    + "\u5E78\u388c\ud1A5\uE261\uB321\u9c1E\u43c7\uFc04"
    + "\u5199\u6d0d\uFAdF\u7E24\u3BAB\ucE11\u8F4E\uB7EB"
    + "\u3c81\u94F7\uB913\u2cd3\uE76E\uc403\u5644\u7FA9"
    + "\u2ABB\uc153\udc0B\u9d6c\u3174\uF646\uAc89\u14E1"
    + "\u163A\u6909\u70B6\ud0Ed\ucc42\u98A4\u285c\uF886";

  /** The 64-bit lookup tables; section 7.1 p. 13. */
  private static final long[] T0 = new long[256];
  private static final long[] T1 = new long[256];
  private static final long[] T2 = new long[256];
  private static final long[] T3 = new long[256];
  private static final long[] T4 = new long[256];
  private static final long[] T5 = new long[256];
  private static final long[] T6 = new long[256];
  private static final long[] T7 = new long[256];

  /** The round constants. */
  private static final long[] rc = new long[R];

  /** caches the result of the correctness test, once executed. */
  private static Boolean valid;

  /** The 512-bit context as 8 longs. */
  private long H0, H1, H2, H3, H4, H5, H6, H7;

  /** Work area for computing the round key schedule. */
  private long k00, k01, k02, k03, k04, k05, k06, k07;
  private long Kr0, Kr1, Kr2, Kr3, Kr4, Kr5, Kr6, Kr7;

  /** work area for transforming the 512-bit buffer. */
  private long n0, n1, n2, n3, n4, n5, n6, n7;
  private long nn0, nn1, nn2, nn3, nn4, nn5, nn6, nn7;

  /** work area for holding block cipher's intermediate values. */
  private long w0, w1, w2, w3, w4, w5, w6, w7;

  static
    {
      long time = System.currentTimeMillis();
      int ROOT = 0x11D; // para. 2.1 [WHIRLPOOL]
      int i, r, j;
      long s1, s2, s4, s5, s8, s9, t;
      char c;
      final byte[] S = new byte[256];
      for (i = 0; i < 256; i++)
        {
          c = S_box.charAt(i >>> 1);

          s1 = ((i & 1) == 0 ? c >>> 8 : c) & 0xFFL;
          s2 = s1 << 1;
          if (s2 > 0xFFL)
            s2 ^= ROOT;

          s4 = s2 << 1;
          if (s4 > 0xFFL)
            s4 ^= ROOT;

          s5 = s4 ^ s1;
          s8 = s4 << 1;
          if (s8 > 0xFFL)
            s8 ^= ROOT;

          s9 = s8 ^ s1;

          T0[i] = t = s1 << 56 | s1 << 48 | s4 << 40 | s1 << 32
                    | s8 << 24 | s5 << 16 | s2 <<  8 | s9;
          T1[i] = t >>>  8 | t << 56;
          T2[i] = t >>> 16 | t << 48;
          T3[i] = t >>> 24 | t << 40;
          T4[i] = t >>> 32 | t << 32;
          T5[i] = t >>> 40 | t << 24;
          T6[i] = t >>> 48 | t << 16;
          T7[i] = t >>> 56 | t <<  8;
        }
      for (r = 0, i = 0; r < R; )
        rc[r++] = (T0[i++] & 0xFF00000000000000L)
                ^ (T1[i++] & 0x00FF000000000000L)
                ^ (T2[i++] & 0x0000FF0000000000L)
                ^ (T3[i++] & 0x000000FF00000000L)
                ^ (T4[i++] & 0x00000000FF000000L)
                ^ (T5[i++] & 0x0000000000FF0000L)
                ^ (T6[i++] & 0x000000000000FF00L)
                ^ (T7[i++] & 0x00000000000000FFL);
      time = System.currentTimeMillis() - time;
      if (Configuration.DEBUG)
        {
          log.fine("Static data");
          log.fine("T0[]:");
          CPStringBuilder sb;
          for (i = 0; i < 64; i++)
            {
              sb = new CPStringBuilder();
              for (j = 0; j < 4; j++)
                sb.append("0x").append(Util.toString(T0[i * 4 + j])).append(", ");

              log.fine(sb.toString());
            }
          log.fine("T1[]:");
          for (i = 0; i < 64; i++)
            {
              sb = new CPStringBuilder();
              for (j = 0; j < 4; j++)
                sb.append("0x").append(Util.toString(T1[i * 4 + j])).append(", ");

              log.fine(sb.toString());
            }
          log.fine("T2[]:");
          for (i = 0; i < 64; i++)
            {
              sb = new CPStringBuilder();
              for (j = 0; j < 4; j++)
                sb.append("0x").append(Util.toString(T2[i * 4 + j])).append(", ");

              log.fine(sb.toString());
            }
          log.fine("T3[]:");
          for (i = 0; i < 64; i++)
            {
              sb = new CPStringBuilder();
              for (j = 0; j < 4; j++)
                sb.append("0x").append(Util.toString(T3[i * 4 + j])).append(", ");

              log.fine(sb.toString());
            }
          log.fine("\nT4[]:");
          for (i = 0; i < 64; i++)
            {
              sb = new CPStringBuilder();
              for (j = 0; j < 4; j++)
                sb.append("0x").append(Util.toString(T4[i * 4 + j])).append(", ");

              log.fine(sb.toString());
            }
          log.fine("T5[]:");
          for (i = 0; i < 64; i++)
            {
              sb = new CPStringBuilder();
              for (j = 0; j < 4; j++)
                sb.append("0x").append(Util.toString(T5[i * 4 + j])).append(", ");

              log.fine(sb.toString());
            }
          log.fine("T6[]:");
          for (i = 0; i < 64; i++)
            {
              sb = new CPStringBuilder();
              for (j = 0; j < 4; j++)
                sb.append("0x").append(Util.toString(T5[i * 4 + j])).append(", ");

              log.fine(sb.toString());
            }
          log.fine("T7[]:");
          for (i = 0; i < 64; i++)
            {
              sb = new CPStringBuilder();
              for (j = 0; j < 4; j++)
                sb.append("0x").append(Util.toString(T5[i * 4 + j])).append(", ");

              log.fine(sb.toString());
            }
          log.fine("rc[]:");
          for (i = 0; i < R; i++)
            log.fine("0x" + Util.toString(rc[i]));

          log.fine("Total initialization time: " + time + " ms.");
        }
    }

  /** Trivial 0-arguments constructor. */
  public Whirlpool()
  {
    super(Registry.WHIRLPOOL_HASH, 20, BLOCK_SIZE);
  }

  /**
   * Private constructor for cloning purposes.
   *
   * @param md the instance to clone.
   */
  private Whirlpool(Whirlpool md)
  {
    this();

    this.H0 = md.H0;
    this.H1 = md.H1;
    this.H2 = md.H2;
    this.H3 = md.H3;
    this.H4 = md.H4;
    this.H5 = md.H5;
    this.H6 = md.H6;
    this.H7 = md.H7;
    this.count = md.count;
    this.buffer = (byte[]) md.buffer.clone();
  }

  public Object clone()
  {
    return (new Whirlpool(this));
  }

  protected void transform(byte[] in, int offset)
  {
    // apply mu to the input
    n0 = (in[offset++] & 0xFFL) << 56
       | (in[offset++] & 0xFFL) << 48
       | (in[offset++] & 0xFFL) << 40
       | (in[offset++] & 0xFFL) << 32
       | (in[offset++] & 0xFFL) << 24
       | (in[offset++] & 0xFFL) << 16
       | (in[offset++] & 0xFFL) <<  8
       | (in[offset++] & 0xFFL);
    n1 = (in[offset++] & 0xFFL) << 56
       | (in[offset++] & 0xFFL) << 48
       | (in[offset++] & 0xFFL) << 40
       | (in[offset++] & 0xFFL) << 32
       | (in[offset++] & 0xFFL) << 24
       | (in[offset++] & 0xFFL) << 16
       | (in[offset++] & 0xFFL) <<  8
       | (in[offset++] & 0xFFL);
    n2 = (in[offset++] & 0xFFL) << 56
       | (in[offset++] & 0xFFL) << 48
       | (in[offset++] & 0xFFL) << 40
       | (in[offset++] & 0xFFL) << 32
       | (in[offset++] & 0xFFL) << 24
       | (in[offset++] & 0xFFL) << 16
       | (in[offset++] & 0xFFL) <<  8
       | (in[offset++] & 0xFFL);
    n3 = (in[offset++] & 0xFFL) << 56
       | (in[offset++] & 0xFFL) << 48
       | (in[offset++] & 0xFFL) << 40
       | (in[offset++] & 0xFFL) << 32
       | (in[offset++] & 0xFFL) << 24
       | (in[offset++] & 0xFFL) << 16
       | (in[offset++] & 0xFFL) <<  8
       | (in[offset++] & 0xFFL);
    n4 = (in[offset++] & 0xFFL) << 56
       | (in[offset++] & 0xFFL) << 48
       | (in[offset++] & 0xFFL) << 40
       | (in[offset++] & 0xFFL) << 32
       | (in[offset++] & 0xFFL) << 24
       | (in[offset++] & 0xFFL) << 16
       | (in[offset++] & 0xFFL) <<  8
       | (in[offset++] & 0xFFL);
    n5 = (in[offset++] & 0xFFL) << 56
       | (in[offset++] & 0xFFL) << 48
       | (in[offset++] & 0xFFL) << 40
       | (in[offset++] & 0xFFL) << 32
       | (in[offset++] & 0xFFL) << 24
       | (in[offset++] & 0xFFL) << 16
       | (in[offset++] & 0xFFL) <<  8
       | (in[offset++] & 0xFFL);
    n6 = (in[offset++] & 0xFFL) << 56
       | (in[offset++] & 0xFFL) << 48
       | (in[offset++] & 0xFFL) << 40
       | (in[offset++] & 0xFFL) << 32
       | (in[offset++] & 0xFFL) << 24
       | (in[offset++] & 0xFFL) << 16
       | (in[offset++] & 0xFFL) <<  8
       | (in[offset++] & 0xFFL);
    n7 = (in[offset++] & 0xFFL) << 56
       | (in[offset++] & 0xFFL) << 48
       | (in[offset++] & 0xFFL) << 40
       | (in[offset++] & 0xFFL) << 32
       | (in[offset++] & 0xFFL) << 24
       | (in[offset++] & 0xFFL) << 16
       | (in[offset++] & 0xFFL) <<  8
       | (in[offset++] & 0xFFL);
    // transform K into the key schedule Kr; 0 <= r <= R
    k00 = H0;
    k01 = H1;
    k02 = H2;
    k03 = H3;
    k04 = H4;
    k05 = H5;
    k06 = H6;
    k07 = H7;
    nn0 = n0 ^ k00;
    nn1 = n1 ^ k01;
    nn2 = n2 ^ k02;
    nn3 = n3 ^ k03;
    nn4 = n4 ^ k04;
    nn5 = n5 ^ k05;
    nn6 = n6 ^ k06;
    nn7 = n7 ^ k07;
    // intermediate cipher output
    w0 = w1 = w2 = w3 = w4 = w5 = w6 = w7 = 0L;
    for (int r = 0; r < R; r++)
      {
        // 1. compute intermediate round key schedule by applying ro[rc]
        // to the previous round key schedule --rc being the round constant
        Kr0 = T0[(int)((k00 >> 56) & 0xFFL)]
            ^ T1[(int)((k07 >> 48) & 0xFFL)]
            ^ T2[(int)((k06 >> 40) & 0xFFL)]
            ^ T3[(int)((k05 >> 32) & 0xFFL)]
            ^ T4[(int)((k04 >> 24) & 0xFFL)]
            ^ T5[(int)((k03 >> 16) & 0xFFL)]
            ^ T6[(int)((k02 >>  8) & 0xFFL)]
            ^ T7[(int)( k01        & 0xFFL)] ^ rc[r];
        Kr1 = T0[(int)((k01 >> 56) & 0xFFL)]
            ^ T1[(int)((k00 >> 48) & 0xFFL)]
            ^ T2[(int)((k07 >> 40) & 0xFFL)]
            ^ T3[(int)((k06 >> 32) & 0xFFL)]
            ^ T4[(int)((k05 >> 24) & 0xFFL)]
            ^ T5[(int)((k04 >> 16) & 0xFFL)]
            ^ T6[(int)((k03 >>  8) & 0xFFL)]
            ^ T7[(int)( k02        & 0xFFL)];
        Kr2 = T0[(int)((k02 >> 56) & 0xFFL)]
            ^ T1[(int)((k01 >> 48) & 0xFFL)]
            ^ T2[(int)((k00 >> 40) & 0xFFL)]
            ^ T3[(int)((k07 >> 32) & 0xFFL)]
            ^ T4[(int)((k06 >> 24) & 0xFFL)]
            ^ T5[(int)((k05 >> 16) & 0xFFL)]
            ^ T6[(int)((k04 >>  8) & 0xFFL)]
            ^ T7[(int)( k03        & 0xFFL)];
        Kr3 = T0[(int)((k03 >> 56) & 0xFFL)]
            ^ T1[(int)((k02 >> 48) & 0xFFL)]
            ^ T2[(int)((k01 >> 40) & 0xFFL)]
            ^ T3[(int)((k00 >> 32) & 0xFFL)]
            ^ T4[(int)((k07 >> 24) & 0xFFL)]
            ^ T5[(int)((k06 >> 16) & 0xFFL)]
            ^ T6[(int)((k05 >>  8) & 0xFFL)]
            ^ T7[(int)( k04        & 0xFFL)];
        Kr4 = T0[(int)((k04 >> 56) & 0xFFL)]
            ^ T1[(int)((k03 >> 48) & 0xFFL)]
            ^ T2[(int)((k02 >> 40) & 0xFFL)]
            ^ T3[(int)((k01 >> 32) & 0xFFL)]
            ^ T4[(int)((k00 >> 24) & 0xFFL)]
            ^ T5[(int)((k07 >> 16) & 0xFFL)]
            ^ T6[(int)((k06 >>  8) & 0xFFL)]
            ^ T7[(int)( k05        & 0xFFL)];
        Kr5 = T0[(int)((k05 >> 56) & 0xFFL)]
            ^ T1[(int)((k04 >> 48) & 0xFFL)]
            ^ T2[(int)((k03 >> 40) & 0xFFL)]
            ^ T3[(int)((k02 >> 32) & 0xFFL)]
            ^ T4[(int)((k01 >> 24) & 0xFFL)]
            ^ T5[(int)((k00 >> 16) & 0xFFL)]
            ^ T6[(int)((k07 >>  8) & 0xFFL)]
            ^ T7[(int)( k06        & 0xFFL)];
        Kr6 = T0[(int)((k06 >> 56) & 0xFFL)]
            ^ T1[(int)((k05 >> 48) & 0xFFL)]
            ^ T2[(int)((k04 >> 40) & 0xFFL)]
            ^ T3[(int)((k03 >> 32) & 0xFFL)]
            ^ T4[(int)((k02 >> 24) & 0xFFL)]
            ^ T5[(int)((k01 >> 16) & 0xFFL)]
            ^ T6[(int)((k00 >>  8) & 0xFFL)]
            ^ T7[(int)( k07        & 0xFFL)];
        Kr7 = T0[(int)((k07 >> 56) & 0xFFL)]
            ^ T1[(int)((k06 >> 48) & 0xFFL)]
            ^ T2[(int)((k05 >> 40) & 0xFFL)]
            ^ T3[(int)((k04 >> 32) & 0xFFL)]
            ^ T4[(int)((k03 >> 24) & 0xFFL)]
            ^ T5[(int)((k02 >> 16) & 0xFFL)]
            ^ T6[(int)((k01 >>  8) & 0xFFL)]
            ^ T7[(int)( k00        & 0xFFL)];
        k00 = Kr0;
        k01 = Kr1;
        k02 = Kr2;
        k03 = Kr3;
        k04 = Kr4;
        k05 = Kr5;
        k06 = Kr6;
        k07 = Kr7;
        // 2. incrementally compute the cipher output
        w0 = T0[(int)((nn0 >> 56) & 0xFFL)]
           ^ T1[(int)((nn7 >> 48) & 0xFFL)]
           ^ T2[(int)((nn6 >> 40) & 0xFFL)]
           ^ T3[(int)((nn5 >> 32) & 0xFFL)]
           ^ T4[(int)((nn4 >> 24) & 0xFFL)]
           ^ T5[(int)((nn3 >> 16) & 0xFFL)]
           ^ T6[(int)((nn2 >>  8) & 0xFFL)]
           ^ T7[(int)( nn1        & 0xFFL)] ^ Kr0;
        w1 = T0[(int)((nn1 >> 56) & 0xFFL)]
           ^ T1[(int)((nn0 >> 48) & 0xFFL)]
           ^ T2[(int)((nn7 >> 40) & 0xFFL)]
           ^ T3[(int)((nn6 >> 32) & 0xFFL)]
           ^ T4[(int)((nn5 >> 24) & 0xFFL)]
           ^ T5[(int)((nn4 >> 16) & 0xFFL)]
           ^ T6[(int)((nn3 >>  8) & 0xFFL)]
           ^ T7[(int)( nn2        & 0xFFL)] ^ Kr1;
        w2 = T0[(int)((nn2 >> 56) & 0xFFL)]
           ^ T1[(int)((nn1 >> 48) & 0xFFL)]
           ^ T2[(int)((nn0 >> 40) & 0xFFL)]
           ^ T3[(int)((nn7 >> 32) & 0xFFL)]
           ^ T4[(int)((nn6 >> 24) & 0xFFL)]
           ^ T5[(int)((nn5 >> 16) & 0xFFL)]
           ^ T6[(int)((nn4 >>  8) & 0xFFL)]
           ^ T7[(int)( nn3        & 0xFFL)] ^ Kr2;
        w3 = T0[(int)((nn3 >> 56) & 0xFFL)]
           ^ T1[(int)((nn2 >> 48) & 0xFFL)]
           ^ T2[(int)((nn1 >> 40) & 0xFFL)]
           ^ T3[(int)((nn0 >> 32) & 0xFFL)]
           ^ T4[(int)((nn7 >> 24) & 0xFFL)]
           ^ T5[(int)((nn6 >> 16) & 0xFFL)]
           ^ T6[(int)((nn5 >>  8) & 0xFFL)]
           ^ T7[(int)( nn4        & 0xFFL)] ^ Kr3;
        w4 = T0[(int)((nn4 >> 56) & 0xFFL)]
           ^ T1[(int)((nn3 >> 48) & 0xFFL)]
           ^ T2[(int)((nn2 >> 40) & 0xFFL)]
           ^ T3[(int)((nn1 >> 32) & 0xFFL)]
           ^ T4[(int)((nn0 >> 24) & 0xFFL)]
           ^ T5[(int)((nn7 >> 16) & 0xFFL)]
           ^ T6[(int)((nn6 >>  8) & 0xFFL)]
           ^ T7[(int)( nn5        & 0xFFL)] ^ Kr4;
        w5 = T0[(int)((nn5 >> 56) & 0xFFL)]
           ^ T1[(int)((nn4 >> 48) & 0xFFL)]
           ^ T2[(int)((nn3 >> 40) & 0xFFL)]
           ^ T3[(int)((nn2 >> 32) & 0xFFL)]
           ^ T4[(int)((nn1 >> 24) & 0xFFL)]
           ^ T5[(int)((nn0 >> 16) & 0xFFL)]
           ^ T6[(int)((nn7 >>  8) & 0xFFL)]
           ^ T7[(int)( nn6        & 0xFFL)] ^ Kr5;
        w6 = T0[(int)((nn6 >> 56) & 0xFFL)]
           ^ T1[(int)((nn5 >> 48) & 0xFFL)]
           ^ T2[(int)((nn4 >> 40) & 0xFFL)]
           ^ T3[(int)((nn3 >> 32) & 0xFFL)]
           ^ T4[(int)((nn2 >> 24) & 0xFFL)]
           ^ T5[(int)((nn1 >> 16) & 0xFFL)]
           ^ T6[(int)((nn0 >>  8) & 0xFFL)]
           ^ T7[(int)( nn7        & 0xFFL)] ^ Kr6;
        w7 = T0[(int)((nn7 >> 56) & 0xFFL)]
           ^ T1[(int)((nn6 >> 48) & 0xFFL)]
           ^ T2[(int)((nn5 >> 40) & 0xFFL)]
           ^ T3[(int)((nn4 >> 32) & 0xFFL)]
           ^ T4[(int)((nn3 >> 24) & 0xFFL)]
           ^ T5[(int)((nn2 >> 16) & 0xFFL)]
           ^ T6[(int)((nn1 >>  8) & 0xFFL)]
           ^ T7[(int)( nn0        & 0xFFL)] ^ Kr7;
        nn0 = w0;
        nn1 = w1;
        nn2 = w2;
        nn3 = w3;
        nn4 = w4;
        nn5 = w5;
        nn6 = w6;
        nn7 = w7;
      }
    // apply the Miyaguchi-Preneel hash scheme
    H0 ^= w0 ^ n0;
    H1 ^= w1 ^ n1;
    H2 ^= w2 ^ n2;
    H3 ^= w3 ^ n3;
    H4 ^= w4 ^ n4;
    H5 ^= w5 ^ n5;
    H6 ^= w6 ^ n6;
    H7 ^= w7 ^ n7;
  }

  protected byte[] padBuffer()
  {
    // [WHIRLPOOL] p. 6:
    // "...padded with a 1-bit, then with as few 0-bits as necessary to
    // obtain a bit string whose length is an odd multiple of 256, and
    // finally with the 256-bit right-justified binary representation of L."
    // in this implementation we use 'count' as the number of bytes hashed
    // so far. hence the minimal number of bytes added to the message proper
    // are 33 (1 for the 1-bit followed by the 0-bits and the encoding of
    // the count framed in a 256-bit block). our formula is then:
    //          count + 33 + padding = 0 (mod BLOCK_SIZE)
    int n = (int)((count + 33) % BLOCK_SIZE);
    int padding = n == 0 ? 33 : BLOCK_SIZE - n + 33;
    byte[] result = new byte[padding];
    // padding is always binary 1 followed by binary 0s
    result[0] = (byte) 0x80;
    // save (right justified) the number of bits hashed
    long bits = count * 8;
    int i = padding - 8;
    result[i++] = (byte)(bits >>> 56);
    result[i++] = (byte)(bits >>> 48);
    result[i++] = (byte)(bits >>> 40);
    result[i++] = (byte)(bits >>> 32);
    result[i++] = (byte)(bits >>> 24);
    result[i++] = (byte)(bits >>> 16);
    result[i++] = (byte)(bits >>>  8);
    result[i  ] = (byte) bits;
    return result;
  }

  protected byte[] getResult()
  {
    // apply inverse mu to the context
    return new byte[] {
      (byte)(H0 >>> 56), (byte)(H0 >>> 48), (byte)(H0 >>> 40), (byte)(H0 >>> 32),
      (byte)(H0 >>> 24), (byte)(H0 >>> 16), (byte)(H0 >>>  8), (byte) H0,
      (byte)(H1 >>> 56), (byte)(H1 >>> 48), (byte)(H1 >>> 40), (byte)(H1 >>> 32),
      (byte)(H1 >>> 24), (byte)(H1 >>> 16), (byte)(H1 >>>  8), (byte) H1,
      (byte)(H2 >>> 56), (byte)(H2 >>> 48), (byte)(H2 >>> 40), (byte)(H2 >>> 32),
      (byte)(H2 >>> 24), (byte)(H2 >>> 16), (byte)(H2 >>>  8), (byte) H2,
      (byte)(H3 >>> 56), (byte)(H3 >>> 48), (byte)(H3 >>> 40), (byte)(H3 >>> 32),
      (byte)(H3 >>> 24), (byte)(H3 >>> 16), (byte)(H3 >>>  8), (byte) H3,
      (byte)(H4 >>> 56), (byte)(H4 >>> 48), (byte)(H4 >>> 40), (byte)(H4 >>> 32),
      (byte)(H4 >>> 24), (byte)(H4 >>> 16), (byte)(H4 >>>  8), (byte) H4,
      (byte)(H5 >>> 56), (byte)(H5 >>> 48), (byte)(H5 >>> 40), (byte)(H5 >>> 32),
      (byte)(H5 >>> 24), (byte)(H5 >>> 16), (byte)(H5 >>>  8), (byte) H5,
      (byte)(H6 >>> 56), (byte)(H6 >>> 48), (byte)(H6 >>> 40), (byte)(H6 >>> 32),
      (byte)(H6 >>> 24), (byte)(H6 >>> 16), (byte)(H6 >>>  8), (byte) H6,
      (byte)(H7 >>> 56), (byte)(H7 >>> 48), (byte)(H7 >>> 40), (byte)(H7 >>> 32),
      (byte)(H7 >>> 24), (byte)(H7 >>> 16), (byte)(H7 >>>  8), (byte) H7 };

  }

  protected void resetContext()
  {
    H0 = H1 = H2 = H3 = H4 = H5 = H6 = H7 = 0L;
  }

  public boolean selfTest()
  {
    if (valid == null)
      {
        String d = Util.toString(new Whirlpool().digest());
        valid = Boolean.valueOf(DIGEST0.equals(d));
      }
    return valid.booleanValue();
  }
}
