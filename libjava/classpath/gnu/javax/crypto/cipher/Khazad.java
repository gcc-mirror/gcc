/* Khazad.java --
   Copyright (C) 2001, 2002, 2003, 2006, 2010 Free Software Foundation, Inc.

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

import gnu.java.security.Configuration;
import gnu.java.security.Registry;
import gnu.java.security.util.Util;

import java.security.InvalidKeyException;
import java.util.ArrayList;
import java.util.Collections;
import java.util.Iterator;
import java.util.logging.Logger;

/**
 * Khazad is a 64-bit (legacy-level) block cipher that accepts a 128-bit key.
 * The cipher is a uniform substitution-permutation network whose inverse only
 * differs from the forward operation in the key schedule. The overall cipher
 * design follows the Wide Trail strategy, favours component reuse, and permits
 * a wide variety of implementation trade-offs.
 * <p>
 * References:
 * <ol>
 * <li><a
 * href="http://planeta.terra.com.br/informatica/paulobarreto/KhazadPage.html">The
 * Khazad Block Cipher</a>.<br>
 * <a href="mailto:paulo.barreto@terra.com.br">Paulo S.L.M. Barreto</a> and <a
 * href="mailto:vincent.rijmen@esat.kuleuven.ac.be">Vincent Rijmen</a>.</li>
 * </ol>
 */
public final class Khazad
    extends BaseCipher
{
  private static final Logger log = Configuration.DEBUG ?
                        Logger.getLogger(Khazad.class.getName()) : null;
  private static final int DEFAULT_BLOCK_SIZE = 8; // in bytes
  private static final int DEFAULT_KEY_SIZE = 16; // in bytes
  private static final int R = 8; // standard number of rounds; para. 3.7
  private static final String Sd = // p. 20 [KHAZAD]
      "\uBA54\u2F74\u53D3\uD24D\u50AC\u8DBF\u7052\u9A4C"
    + "\uEAD5\u97D1\u3351\u5BA6\uDE48\uA899\uDB32\uB7FC"
    + "\uE39E\u919B\uE2BB\u416E\uA5CB\u6B95\uA1F3\uB102"
    + "\uCCC4\u1D14\uC363\uDA5D\u5FDC\u7DCD\u7F5A\u6C5C"
    + "\uF726\uFFED\uE89D\u6F8E\u19A0\uF089\u0F07\uAFFB"
    + "\u0815\u0D04\u0164\uDF76\u79DD\u3D16\u3F37\u6D38"
    + "\uB973\uE935\u5571\u7B8C\u7288\uF62A\u3E5E\u2746"
    + "\u0C65\u6861\u03C1\u57D6\uD958\uD866\uD73A\uC83C"
    + "\uFA96\uA798\uECB8\uC7AE\u694B\uABA9\u670A\u47F2"
    + "\uB522\uE5EE\uBE2B\u8112\u831B\u0E23\uF545\u21CE"
    + "\u492C\uF9E6\uB628\u1782\u1A8B\uFE8A\u09C9\u874E"
    + "\uE12E\uE4E0\uEB90\uA41E\u8560\u0025\uF4F1\u940B"
    + "\uE775\uEF34\u31D4\uD086\u7EAD\uFD29\u303B\u9FF8"
    + "\uC613\u0605\uC511\u777C\u7A78\u361C\u3959\u1856"
    + "\uB3B0\u2420\uB292\uA3C0\u4462\u10B4\u8443\u93C2"
    + "\u4ABD\u8F2D\uBC9C\u6A40\uCFA2\u804F\u1FCA\uAA42";
  private static final byte[] S = new byte[256];
  private static final int[] T0 = new int[256];
  private static final int[] T1 = new int[256];
  private static final int[] T2 = new int[256];
  private static final int[] T3 = new int[256];
  private static final int[] T4 = new int[256];
  private static final int[] T5 = new int[256];
  private static final int[] T6 = new int[256];
  private static final int[] T7 = new int[256];
  private static final int[][] rc = new int[R + 1][2]; // round constants
  /**
   * KAT vector (from ecb_vk): I=120 KEY=00000000000000000000000000000100
   * CT=A0C86A1BBE2CBF4C
   */
  private static final byte[] KAT_KEY =
      Util.toBytesFromString("00000000000000000000000000000100");
  private static final byte[] KAT_CT = Util.toBytesFromString("A0C86A1BBE2CBF4C");
  /** caches the result of the correctness test, once executed. */
  private static Boolean valid;

  static
    {
      long time = System.currentTimeMillis();
      long ROOT = 0x11d; // para. 2.1 [KHAZAD]
      int i, j;
      int s, s2, s3, s4, s5, s6, s7, s8, sb;
      char c;
      for (i = 0; i < 256; i++)
        {
          c = Sd.charAt(i >>> 1);
          s = ((i & 1) == 0 ? c >>> 8 : c) & 0xFF;
          S[i] = (byte) s;
          s2 = s << 1;
          if (s2 > 0xFF)
            s2 ^= ROOT;
          s3 = s2 ^ s;
          s4 = s2 << 1;
          if (s4 > 0xFF)
            s4 ^= ROOT;
          s5 = s4 ^ s;
          s6 = s4 ^ s2;
          s7 = s6 ^ s;
          s8 = s4 << 1;
          if (s8 > 0xFF)
            s8 ^= ROOT;
          sb = s8 ^ s2 ^ s;
          T0[i] = s  << 24 | s3 << 16 | s4 << 8 | s5;
          T1[i] = s3 << 24 | s  << 16 | s5 << 8 | s4;
          T2[i] = s4 << 24 | s5 << 16 | s  << 8 | s3;
          T3[i] = s5 << 24 | s4 << 16 | s3 << 8 | s;
          T4[i] = s6 << 24 | s8 << 16 | sb << 8 | s7;
          T5[i] = s8 << 24 | s6 << 16 | s7 << 8 | sb;
          T6[i] = sb << 24 | s7 << 16 | s6 << 8 | s8;
          T7[i] = s7 << 24 | sb << 16 | s8 << 8 | s6;
        }
      for (i = 0, j = 0; i < R + 1; i++) // compute round constant
        {
          rc[i][0] =  S[j++]         << 24
                   | (S[j++] & 0xFF) << 16
                   | (S[j++] & 0xFF) << 8
                   | (S[j++] & 0xFF);
          rc[i][1] =  S[j++]         << 24
                   | (S[j++] & 0xFF) << 16
                   | (S[j++] & 0xFF) << 8
                   | (S[j++] & 0xFF);
        }
      time = System.currentTimeMillis() - time;
      if (Configuration.DEBUG)
        {
          log.fine("Static data");
          log.fine("T0[]:");
          StringBuilder b;
          for (i = 0; i < 64; i++)
            {
              b = new StringBuilder();
              for (j = 0; j < 4; j++)
                b.append("0x").append(Util.toString(T0[i * 4 + j])).append(", ");
              log.fine(b.toString());
            }
          log.fine("T1[]:");
          for (i = 0; i < 64; i++)
            {
              b = new StringBuilder();
              for (j = 0; j < 4; j++)
                b.append("0x").append(Util.toString(T1[i * 4 + j])).append(", ");
              log.fine(b.toString());
            }
          log.fine("T2[]:");
          for (i = 0; i < 64; i++)
            {
              b = new StringBuilder();
              for (j = 0; j < 4; j++)
                b.append("0x").append(Util.toString(T2[i * 4 + j])).append(", ");
              log.fine(b.toString());
            }
          log.fine("T3[]:");
          for (i = 0; i < 64; i++)
            {
              b = new StringBuilder();
              for (j = 0; j < 4; j++)
                b.append("0x").append(Util.toString(T3[i * 4 + j])).append(", ");
              log.fine(b.toString());
            }
          log.fine("T4[]:");
          for (i = 0; i < 64; i++)
            {
              b = new StringBuilder();
              for (j = 0; j < 4; j++)
                b.append("0x").append(Util.toString(T4[i * 4 + j])).append(", ");
              log.fine(b.toString());
            }
          log.fine("T5[]:");
          for (i = 0; i < 64; i++)
            {
              b = new StringBuilder();
              for (j = 0; j < 4; j++)
                b.append("0x").append(Util.toString(T5[i * 4 + j])).append(", ");
              log.fine(b.toString());
            }
          log.fine("T6[]:");
          for (i = 0; i < 64; i++)
            {
              b = new StringBuilder();
              for (j = 0; j < 4; j++)
                b.append("0x").append(Util.toString(T6[i * 4 + j])).append(", ");
              log.fine(b.toString());
            }
          log.fine("T7[]:");
          for (i = 0; i < 64; i++)
            {
              b = new StringBuilder();
              for (j = 0; j < 4; j++)
                b.append("0x").append(Util.toString(T7[i * 4 + j])).append(", ");
              log.fine(b.toString());
            }
          log.fine("rc[]:");
          for (i = 0; i < R + 1; i++)
            log.fine("0x" + Util.toString(rc[i][0]) + Util.toString(rc[i][1]));
          log.fine("Total initialization time: " + time + " ms.");
        }
    }

  /** Trivial 0-arguments constructor. */
  public Khazad()
  {
    super(Registry.KHAZAD_CIPHER, DEFAULT_BLOCK_SIZE, DEFAULT_KEY_SIZE);
  }

  private static void khazad(byte[] in, int i, byte[] out, int j, int[][] K)
  {
    // sigma(K[0])
    int k0 = K[0][0];
    int k1 = K[0][1];
    int a0 = (in[i++]         << 24
           | (in[i++] & 0xFF) << 16
           | (in[i++] & 0xFF) <<  8
           | (in[i++] & 0xFF)      ) ^ k0;
    int a1 = (in[i++]         << 24
           | (in[i++] & 0xFF) << 16
           | (in[i++] & 0xFF) <<  8
           | (in[i  ] & 0xFF)      ) ^ k1;
    int b0, b1;
    // round function
    for (int r = 1; r < R; r++)
      {
        k0 = K[r][0];
        k1 = K[r][1];
        b0 = T0[ a0 >>> 24        ]
           ^ T1[(a0 >>> 16) & 0xFF]
           ^ T2[(a0 >>>  8) & 0xFF]
           ^ T3[ a0         & 0xFF]
           ^ T4[ a1 >>> 24        ]
           ^ T5[(a1 >>> 16) & 0xFF]
           ^ T6[(a1 >>>  8) & 0xFF]
           ^ T7[ a1         & 0xFF] ^ k0;
        b1 = T0[ a1 >>> 24        ]
           ^ T1[(a1 >>> 16) & 0xFF]
           ^ T2[(a1 >>>  8) & 0xFF]
           ^ T3[ a1         & 0xFF]
           ^ T4[ a0 >>> 24        ]
           ^ T5[(a0 >>> 16) & 0xFF]
           ^ T6[(a0 >>>  8) & 0xFF]
           ^ T7[ a0         & 0xFF] ^ k1;
        a0 = b0;
        a1 = b1;
        if (Configuration.DEBUG)
          log.fine("T" + r + "=" + Util.toString(a0) + Util.toString(a1));
      }
    // sigma(K[R]) o gamma applied to previous output
    k0 = K[R][0];
    k1 = K[R][1];
    out[j++] = (byte)(S[ a0 >>> 24        ] ^ (k0 >>> 24));
    out[j++] = (byte)(S[(a0 >>> 16) & 0xFF] ^ (k0 >>> 16));
    out[j++] = (byte)(S[(a0 >>>  8) & 0xFF] ^ (k0 >>>  8));
    out[j++] = (byte)(S[ a0         & 0xFF] ^  k0        );
    out[j++] = (byte)(S[ a1 >>> 24        ] ^ (k1 >>> 24));
    out[j++] = (byte)(S[(a1 >>> 16) & 0xFF] ^ (k1 >>> 16));
    out[j++] = (byte)(S[(a1 >>>  8) & 0xFF] ^ (k1 >>>  8));
    out[j  ] = (byte)(S[ a1         & 0xFF] ^  k1        );
    if (Configuration.DEBUG)
      log.fine("T=" + Util.toString(out, j - 7, 8) + "\n");
  }

  public Object clone()
  {
    Khazad result = new Khazad();
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

  /**
   * Expands a user-supplied key material into a session key for a designated
   * <i>block size</i>.
   *
   * @param uk the 128-bit user-supplied key material.
   * @param bs the desired block size in bytes.
   * @return an Object encapsulating the session key.
   * @exception IllegalArgumentException if the block size is not 16 (128-bit).
   * @exception InvalidKeyException if the key data is invalid.
   */
  public Object makeKey(byte[] uk, int bs) throws InvalidKeyException
  {
    if (bs != DEFAULT_BLOCK_SIZE)
      throw new IllegalArgumentException();
    if (uk == null)
      throw new InvalidKeyException("Empty key");
    if (uk.length != 16)
      throw new InvalidKeyException("Key is not 128-bit.");
    int[][] Ke = new int[R + 1][2]; // encryption round keys
    int[][] Kd = new int[R + 1][2]; // decryption round keys
    int r, i;
    int k20, k21, k10, k11, rc0, rc1, kr0, kr1;
    i = 0;
    k20 =  uk[i++]         << 24
        | (uk[i++] & 0xFF) << 16
        | (uk[i++] & 0xFF) << 8
        | (uk[i++] & 0xFF);
    k21 =  uk[i++]         << 24
        | (uk[i++] & 0xFF) << 16
        | (uk[i++] & 0xFF) << 8
        | (uk[i++] & 0xFF);
    k10 =  uk[i++]         << 24
        | (uk[i++] & 0xFF) << 16
        | (uk[i++] & 0xFF) << 8
        | (uk[i++] & 0xFF);
    k11 =  uk[i++]         << 24
        | (uk[i++] & 0xFF) << 16
        | (uk[i++] & 0xFF) << 8
        | (uk[i++] & 0xFF);
    for (r = 0, i = 0; r <= R; r++)
      {
        rc0 = rc[r][0];
        rc1 = rc[r][1];
        kr0 = T0[ k10 >>> 24        ]
            ^ T1[(k10 >>> 16) & 0xFF]
            ^ T2[(k10 >>>  8) & 0xFF]
            ^ T3[ k10         & 0xFF]
            ^ T4[(k11 >>> 24) & 0xFF]
            ^ T5[(k11 >>> 16) & 0xFF]
            ^ T6[(k11 >>>  8) & 0xFF]
            ^ T7[ k11         & 0xFF] ^ rc0 ^ k20;
        kr1 = T0[ k11 >>> 24        ]
            ^ T1[(k11 >>> 16) & 0xFF]
            ^ T2[(k11 >>>  8) & 0xFF]
            ^ T3[ k11         & 0xFF]
            ^ T4[(k10 >>> 24) & 0xFF]
            ^ T5[(k10 >>> 16) & 0xFF]
            ^ T6[(k10 >>>  8) & 0xFF]
            ^ T7[ k10         & 0xFF] ^ rc1 ^ k21;
        Ke[r][0] = kr0;
        Ke[r][1] = kr1;
        k20 = k10;
        k21 = k11;
        k10 = kr0;
        k11 = kr1;
        if (r == 0 || r == R)
          {
            Kd[R - r][0] = kr0;
            Kd[R - r][1] = kr1;
          }
        else
          {
            Kd[R - r][0] = T0[S[ kr0 >>> 24        ] & 0xFF]
                         ^ T1[S[(kr0 >>> 16) & 0xFF] & 0xFF]
                         ^ T2[S[(kr0 >>>  8) & 0xFF] & 0xFF]
                         ^ T3[S[ kr0         & 0xFF] & 0xFF]
                         ^ T4[S[ kr1 >>> 24        ] & 0xFF]
                         ^ T5[S[(kr1 >>> 16) & 0xFF] & 0xFF]
                         ^ T6[S[(kr1 >>>  8) & 0xFF] & 0xFF]
                         ^ T7[S[ kr1         & 0xFF] & 0xFF];
            Kd[R - r][1] = T0[S[ kr1 >>> 24        ] & 0xFF]
                         ^ T1[S[(kr1 >>> 16) & 0xFF] & 0xFF]
                         ^ T2[S[(kr1 >>>  8) & 0xFF] & 0xFF]
                         ^ T3[S[ kr1         & 0xFF] & 0xFF]
                         ^ T4[S[ kr0 >>> 24        ] & 0xFF]
                         ^ T5[S[(kr0 >>> 16) & 0xFF] & 0xFF]
                         ^ T6[S[(kr0 >>>  8) & 0xFF] & 0xFF]
                         ^ T7[S[ kr0         & 0xFF] & 0xFF];
          }
      }
    if (Configuration.DEBUG)
      {
        log.fine("Key schedule");
        log.fine("Ke[]:");
        for (r = 0; r < R + 1; r++)
          log.fine("#" + r + ": 0x" + Util.toString(Ke[r][0])
                   + Util.toString(Ke[r][1]));
        log.fine("Kd[]:");
        for (r = 0; r < R + 1; r++)
          log.fine("#" + r + ": 0x" + Util.toString(Kd[r][0])
                   + Util.toString(Kd[r][1]));
      }
    return new Object[] { Ke, Kd };
  }

  public void encrypt(byte[] in, int i, byte[] out, int j, Object k, int bs)
  {
    if (bs != DEFAULT_BLOCK_SIZE)
      throw new IllegalArgumentException();
    int[][] K = (int[][])((Object[]) k)[0];
    khazad(in, i, out, j, K);
  }

  public void decrypt(byte[] in, int i, byte[] out, int j, Object k, int bs)
  {
    if (bs != DEFAULT_BLOCK_SIZE)
      throw new IllegalArgumentException();
    int[][] K = (int[][])((Object[]) k)[1];
    khazad(in, i, out, j, K);
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
