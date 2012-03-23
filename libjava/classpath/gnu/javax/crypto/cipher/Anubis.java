/* Anubis.java --
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
 * Anubis is a 128-bit block cipher that accepts a variable-length key. The
 * cipher is a uniform substitution-permutation network whose inverse only
 * differs from the forward operation in the key schedule. The design of both
 * the round transformation and the key schedule is based upon the Wide Trail
 * strategy and permits a wide variety of implementation trade-offs.
 * <p>
 * References:
 * <ol>
 * <li><a
 * href="http://planeta.terra.com.br/informatica/paulobarreto/AnubisPage.html">The
 * ANUBIS Block Cipher</a>.<br>
 * <a href="mailto:paulo.barreto@terra.com.br">Paulo S.L.M. Barreto</a> and <a
 * href="mailto:vincent.rijmen@esat.kuleuven.ac.be">Vincent Rijmen</a>.</li>
 * </ol>
 */
public final class Anubis
    extends BaseCipher
{
  private static final Logger log = Configuration.DEBUG ?
                        Logger.getLogger(Anubis.class.getName()) : null;
  private static final int DEFAULT_BLOCK_SIZE = 16; // in bytes
  private static final int DEFAULT_KEY_SIZE = 16; // in bytes
  private static final String Sd = // p. 25 [ANUBIS]
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
  /**
   * Anubis round constants. This is the largest possible considering that we
   * always use R values, R = 8 + N, and 4 &lt;= N &lt;= 10.
   */
  private static final int[] rc = new int[18];
  /**
   * KAT vector (from ecb_vk): I=83
   * KEY=000000000000000000002000000000000000000000000000
   * CT=2E66AB15773F3D32FB6C697509460DF4
   */
  private static final byte[] KAT_KEY =
      Util.toBytesFromString("000000000000000000002000000000000000000000000000");
  private static final byte[] KAT_CT =
      Util.toBytesFromString("2E66AB15773F3D32FB6C697509460DF4");
  /** caches the result of the correctness test, once executed. */
  private static Boolean valid;

  static
    {
      long time = System.currentTimeMillis();
      int ROOT = 0x11d; // para. 2.1 [ANUBIS]
      int i, s, s2, s4, s6, s8, t;
      char c;
      for (i = 0; i < 256; i++)
        {
          c = Sd.charAt(i >>> 1);
          s = ((i & 1) == 0 ? c >>> 8 : c) & 0xFF;
          S[i] = (byte) s;
          s2 = s << 1;
          if (s2 > 0xFF)
            s2 ^= ROOT;
          s4 = s2 << 1;
          if (s4 > 0xFF)
            s4 ^= ROOT;
          s6 = s4 ^ s2;
          s8 = s4 << 1;
          if (s8 > 0xFF)
            s8 ^= ROOT;
          T0[i] = s  << 24 | s2 << 16 | s4 << 8 | s6;
          T1[i] = s2 << 24 | s  << 16 | s6 << 8 | s4;
          T2[i] = s4 << 24 | s6 << 16 | s  << 8 | s2;
          T3[i] = s6 << 24 | s4 << 16 | s2 << 8 | s;
          T4[i] = s  << 24 | s  << 16 | s  << 8 | s;
          T5[s] = s  << 24 | s2 << 16 | s6 << 8 | s8;
        }
      // compute round constant
      for (i = 0, s = 0; i < 18;)
        rc[i++] =  S[(s++) & 0xFF]         << 24
                | (S[(s++) & 0xFF] & 0xFF) << 16
                | (S[(s++) & 0xFF] & 0xFF) << 8
                | (S[(s++) & 0xFF] & 0xFF);
      time = System.currentTimeMillis() - time;
      if (Configuration.DEBUG)
        {
          log.fine("Static data");
          log.fine("T0[]:");
          StringBuilder sb;
          for (i = 0; i < 64; i++)
            {
              sb = new StringBuilder();
              for (t = 0; t < 4; t++)
                sb.append("0x").append(Util.toString(T0[i * 4 + t])).append(", ");
              log.fine(sb.toString());
            }
          log.fine("T1[]:");
          for (i = 0; i < 64; i++)
            {
              sb = new StringBuilder();
              for (t = 0; t < 4; t++)
                sb.append("0x").append(Util.toString(T1[i * 4 + t])).append(", ");
              log.fine(sb.toString());
            }
          log.fine("T2[]:");
          for (i = 0; i < 64; i++)
            {
              sb = new StringBuilder();
              for (t = 0; t < 4; t++)
                sb.append("0x").append(Util.toString(T2[i * 4 + t])).append(", ");
              log.fine(sb.toString());
            }
          log.fine("T3[]:");
          for (i = 0; i < 64; i++)
            {
              sb = new StringBuilder();
              for (t = 0; t < 4; t++)
                sb.append("0x").append(Util.toString(T3[i * 4 + t])).append(", ");
              log.fine(sb.toString());
            }
          log.fine("T4[]:");
          for (i = 0; i < 64; i++)
            {
              sb = new StringBuilder();
              for (t = 0; t < 4; t++)
                sb.append("0x").append(Util.toString(T4[i * 4 + t])).append(", ");
              log.fine(sb.toString());
            }
          log.fine("T5[]:");
          for (i = 0; i < 64; i++)
            {
              sb = new StringBuilder();
              for (t = 0; t < 4; t++)
                sb.append("0x").append(Util.toString(T5[i * 4 + t])).append(", ");
              log.fine(sb.toString());
            }
          log.fine("rc[]:");
          for (i = 0; i < 18; i++)
            log.fine("0x" + Util.toString(rc[i]));
          log.fine("Total initialization time: " + time + " ms.");
        }
    }

  /** Trivial 0-arguments constructor. */
  public Anubis()
  {
    super(Registry.ANUBIS_CIPHER, DEFAULT_BLOCK_SIZE, DEFAULT_KEY_SIZE);
  }

  private static void anubis(byte[] in, int i, byte[] out, int j, int[][] K)
  {
    // extract encryption round keys
    int R = K.length - 1;
    int[] Ker = K[0];
    // mu function + affine key addition
    int a0 = (in[i++]         << 24
           | (in[i++] & 0xFF) << 16
           | (in[i++] & 0xFF) <<  8
           | (in[i++] & 0xFF)      ) ^ Ker[0];
    int a1 = (in[i++]         << 24
           | (in[i++] & 0xFF) << 16
           | (in[i++] & 0xFF) <<  8
           | (in[i++] & 0xFF)      ) ^ Ker[1];
    int a2 = (in[i++]         << 24
           | (in[i++] & 0xFF) << 16
           | (in[i++] & 0xFF) <<  8
           | (in[i++] & 0xFF)      ) ^ Ker[2];
    int a3 = (in[i++]         << 24
           | (in[i++] & 0xFF) << 16
           | (in[i++] & 0xFF) <<  8
           | (in[i] & 0xFF)        ) ^ Ker[3];
    int b0, b1, b2, b3;
    // round function
    for (int r = 1; r < R; r++)
      {
        Ker = K[r];
        b0 = T0[ a0 >>> 24        ]
           ^ T1[ a1 >>> 24        ]
           ^ T2[ a2 >>> 24        ]
           ^ T3[ a3 >>> 24        ] ^ Ker[0];
        b1 = T0[(a0 >>> 16) & 0xFF]
           ^ T1[(a1 >>> 16) & 0xFF]
           ^ T2[(a2 >>> 16) & 0xFF]
           ^ T3[(a3 >>> 16) & 0xFF] ^ Ker[1];
        b2 = T0[(a0 >>>  8) & 0xFF]
           ^ T1[(a1 >>>  8) & 0xFF]
           ^ T2[(a2 >>>  8) & 0xFF]
           ^ T3[(a3 >>>  8) & 0xFF] ^ Ker[2];
        b3 = T0[ a0         & 0xFF]
           ^ T1[ a1         & 0xFF]
           ^ T2[ a2         & 0xFF]
           ^ T3[ a3         & 0xFF] ^ Ker[3];
        a0 = b0;
        a1 = b1;
        a2 = b2;
        a3 = b3;
        if (Configuration.DEBUG)
          log.fine("T" + r + "=" + Util.toString(a0) + Util.toString(a1)
                   + Util.toString(a2) + Util.toString(a3));
      }
    // last round function
    Ker = K[R];
    int tt = Ker[0];
    out[j++] = (byte)(S[ a0 >>> 24        ] ^ (tt >>> 24));
    out[j++] = (byte)(S[ a1 >>> 24        ] ^ (tt >>> 16));
    out[j++] = (byte)(S[ a2 >>> 24        ] ^ (tt >>> 8));
    out[j++] = (byte)(S[ a3 >>> 24        ] ^  tt);
    tt = Ker[1];
    out[j++] = (byte)(S[(a0 >>> 16) & 0xFF] ^ (tt >>> 24));
    out[j++] = (byte)(S[(a1 >>> 16) & 0xFF] ^ (tt >>> 16));
    out[j++] = (byte)(S[(a2 >>> 16) & 0xFF] ^ (tt >>> 8));
    out[j++] = (byte)(S[(a3 >>> 16) & 0xFF] ^  tt);
    tt = Ker[2];
    out[j++] = (byte)(S[(a0 >>>  8) & 0xFF] ^ (tt >>> 24));
    out[j++] = (byte)(S[(a1 >>>  8) & 0xFF] ^ (tt >>> 16));
    out[j++] = (byte)(S[(a2 >>>  8) & 0xFF] ^ (tt >>> 8));
    out[j++] = (byte)(S[(a3 >>>  8) & 0xFF] ^  tt);
    tt = Ker[3];
    out[j++] = (byte)(S[ a0         & 0xFF] ^ (tt >>> 24));
    out[j++] = (byte)(S[ a1         & 0xFF] ^ (tt >>> 16));
    out[j++] = (byte)(S[ a2         & 0xFF] ^ (tt >>> 8));
    out[j  ] = (byte)(S[ a3         & 0xFF] ^  tt);
    if (Configuration.DEBUG)
      log.fine("T=" + Util.toString(out, j - 15, 16) + "\n");
  }

  public Object clone()
  {
    Anubis result = new Anubis();
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
    for (int n = 4; n < 10; n++)
      al.add(Integer.valueOf(n * 32 / 8));
    return Collections.unmodifiableList(al).iterator();
  }

  /**
   * Expands a user-supplied key material into a session key for a designated
   * <i>block size</i>.
   *
   * @param uk the 32N-bit user-supplied key material; 4 &lt;= N &lt;= 10.
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
    if ((uk.length % 4) != 0)
      throw new InvalidKeyException("Key is not multiple of 32-bit.");
    int N = uk.length / 4;
    if (N < 4 || N > 10)
      throw new InvalidKeyException("Key is not 32N; 4 <= N <= 10");
    int R = 8 + N;
    int[][] Ke = new int[R + 1][4]; // encryption round keys
    int[][] Kd = new int[R + 1][4]; // decryption round keys
    int[] tk = new int[N];
    int[] kk = new int[N];
    int r, i, j, k, k0, k1, k2, k3, tt;
    // apply mu to k0
    for (r = 0, i = 0; r < N;)
      tk[r++] =  uk[i++]         << 24
              | (uk[i++] & 0xFF) << 16
              | (uk[i++] & 0xFF) << 8
              | (uk[i++] & 0xFF);
    for (r = 0; r <= R; r++)
      {
        if (r > 0)
          {
            // psi = key evolution function
            kk[0] = T0[(tk[0    ] >>> 24)       ]
                  ^ T1[(tk[N - 1] >>> 16) & 0xFF]
                  ^ T2[(tk[N - 2] >>>  8) & 0xFF]
                  ^ T3[ tk[N - 3]         & 0xFF];
            kk[1] = T0[(tk[1    ] >>> 24)       ]
                  ^ T1[(tk[0    ] >>> 16) & 0xFF]
                  ^ T2[(tk[N - 1] >>>  8) & 0xFF]
                  ^ T3[ tk[N - 2]         & 0xFF];
            kk[2] = T0[(tk[2    ] >>> 24)       ]
                  ^ T1[(tk[1    ] >>> 16) & 0xFF]
                  ^ T2[(tk[0    ] >>>  8) & 0xFF]
                  ^ T3[ tk[N - 1]         & 0xFF];
            kk[3] = T0[(tk[3    ] >>> 24)       ]
                  ^ T1[(tk[2    ] >>> 16) & 0xFF]
                  ^ T2[(tk[1    ] >>>  8) & 0xFF]
                  ^ T3[ tk[0    ]         & 0xFF];
            for (i = 4; i < N; i++)
              kk[i] = T0[ tk[i    ] >>> 24        ]
                    ^ T1[(tk[i - 1] >>> 16) & 0xFF]
                    ^ T2[(tk[i - 2] >>>  8) & 0xFF]
                    ^ T3[ tk[i - 3]         & 0xFF];
            // apply sigma (affine addition) to round constant
            tk[0] = rc[r - 1] ^ kk[0];
            for (i = 1; i < N; i++)
              tk[i] = kk[i];
          }
        // phi = key selection function
        tt = tk[N - 1];
        k0 = T4[ tt >>> 24        ];
        k1 = T4[(tt >>> 16) & 0xFF];
        k2 = T4[(tt >>>  8) & 0xFF];
        k3 = T4[ tt         & 0xFF];
        for (k = N - 2; k >= 0; k--)
          {
            tt = tk[k];
            k0 =  T4[ tt >>> 24        ]
               ^ (T5[(k0 >>> 24) & 0xFF] & 0xFF000000)
               ^ (T5[(k0 >>> 16) & 0xFF] & 0x00FF0000)
               ^ (T5[(k0 >>>  8) & 0xFF] & 0x0000FF00)
               ^ (T5 [k0         & 0xFF] & 0x000000FF);
            k1 =  T4[(tt >>> 16) & 0xFF]
               ^ (T5[(k1 >>> 24) & 0xFF] & 0xFF000000)
               ^ (T5[(k1 >>> 16) & 0xFF] & 0x00FF0000)
               ^ (T5[(k1 >>>  8) & 0xFF] & 0x0000FF00)
               ^ (T5[ k1         & 0xFF] & 0x000000FF);
            k2 =  T4[(tt >>>  8) & 0xFF]
               ^ (T5[(k2 >>> 24) & 0xFF] & 0xFF000000)
               ^ (T5[(k2 >>> 16) & 0xFF] & 0x00FF0000)
               ^ (T5[(k2 >>>  8) & 0xFF] & 0x0000FF00)
               ^ (T5[ k2         & 0xFF] & 0x000000FF);
            k3 =  T4[ tt         & 0xFF]
               ^ (T5[(k3 >>> 24) & 0xFF] & 0xFF000000)
               ^ (T5[(k3 >>> 16) & 0xFF] & 0x00FF0000)
               ^ (T5[(k3 >>>  8) & 0xFF] & 0x0000FF00)
               ^ (T5[ k3         & 0xFF] & 0x000000FF);
          }
        Ke[r][0] = k0;
        Ke[r][1] = k1;
        Ke[r][2] = k2;
        Ke[r][3] = k3;
        if (r == 0 || r == R)
          {
            Kd[R - r][0] = k0;
            Kd[R - r][1] = k1;
            Kd[R - r][2] = k2;
            Kd[R - r][3] = k3;
          }
        else
          {
            Kd[R - r][0] = T0[S[ k0 >>> 24        ] & 0xFF]
                         ^ T1[S[(k0 >>> 16) & 0xFF] & 0xFF]
                         ^ T2[S[(k0 >>>  8) & 0xFF] & 0xFF]
                         ^ T3[S[ k0         & 0xFF] & 0xFF];
            Kd[R - r][1] = T0[S[ k1 >>> 24        ] & 0xFF]
                         ^ T1[S[(k1 >>> 16) & 0xFF] & 0xFF]
                         ^ T2[S[(k1 >>>  8) & 0xFF] & 0xFF]
                         ^ T3[S[ k1         & 0xFF] & 0xFF];
            Kd[R - r][2] = T0[S[ k2 >>> 24        ] & 0xFF]
                         ^ T1[S[(k2 >>> 16) & 0xFF] & 0xFF]
                         ^ T2[S[(k2 >>>  8) & 0xFF] & 0xFF]
                         ^ T3[S[ k2         & 0xFF] & 0xFF];
            Kd[R - r][3] = T0[S[ k3 >>> 24        ] & 0xFF]
                         ^ T1[S[(k3 >>> 16) & 0xFF] & 0xFF]
                         ^ T2[S[(k3 >>>  8) & 0xFF] & 0xFF]
                         ^ T3[S[ k3         & 0xFF] & 0xFF];
          }
      }
    if (Configuration.DEBUG)
      {
        log.fine("Key schedule");
        log.fine("Ke[]:");
        StringBuilder sb;
        for (r = 0; r < R + 1; r++)
          {
            sb = new StringBuilder("#").append(r).append(": ");
            for (j = 0; j < 4; j++)
              sb.append("0x").append(Util.toString(Ke[r][j])).append(", ");
            log.fine(sb.toString());
          }
        log.fine("Kd[]:");
        for (r = 0; r < R + 1; r++)
          {
            sb = new StringBuilder("#").append(r).append(": ");
            for (j = 0; j < 4; j++)
              sb.append("0x").append(Util.toString(Kd[r][j])).append(", ");
            log.fine(sb.toString());
          }
      }
    return new Object[] { Ke, Kd };
  }

  public void encrypt(byte[] in, int i, byte[] out, int j, Object k, int bs)
  {
    if (bs != DEFAULT_BLOCK_SIZE)
      throw new IllegalArgumentException();
    int[][] K = (int[][])((Object[]) k)[0];
    anubis(in, i, out, j, K);
  }

  public void decrypt(byte[] in, int i, byte[] out, int j, Object k, int bs)
  {
    if (bs != DEFAULT_BLOCK_SIZE)
      throw new IllegalArgumentException();
    int[][] K = (int[][])((Object[]) k)[1];
    anubis(in, i, out, j, K);
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
