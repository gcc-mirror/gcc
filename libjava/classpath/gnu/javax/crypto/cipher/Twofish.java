/* Twofish.java --
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

import gnu.java.security.Configuration;
import gnu.java.security.Registry;
import gnu.java.security.util.Util;

import java.security.InvalidKeyException;
import java.util.ArrayList;
import java.util.Collections;
import java.util.Iterator;
import java.util.logging.Logger;

/**
 * Twofish is a balanced 128-bit Feistel cipher, consisting of 16 rounds. In
 * each round, a 64-bit S-box value is computed from 64 bits of the block, and
 * this value is xored into the other half of the block. The two half-blocks are
 * then exchanged, and the next round begins. Before the first round, all input
 * bits are xored with key-dependent "whitening" subkeys, and after the final
 * round the output bits are xored with other key-dependent whitening subkeys;
 * these subkeys are not used anywhere else in the algorithm.
 * <p>
 * Twofish is designed by Bruce Schneier, Doug Whiting, John Kelsey, Chris
 * Hall, David Wagner and Niels Ferguson.
 * <p>
 * References:
 * <ol>
 *    <li><a href="http://www.counterpane.com/twofish-paper.html">Twofish: A
 *    128-bit Block Cipher</a>.</li>
 * </ol>
 */
public final class Twofish
    extends BaseCipher
{
  private static final Logger log = Logger.getLogger(Twofish.class.getName());
  private static final int DEFAULT_BLOCK_SIZE = 16; // in bytes
  private static final int DEFAULT_KEY_SIZE = 16; // in bytes
  private static final int MAX_ROUNDS = 16; // max # rounds (for allocating subkeys)
  private static final int ROUNDS = MAX_ROUNDS;
  // subkey array indices
  private static final int INPUT_WHITEN = 0;
  private static final int OUTPUT_WHITEN = INPUT_WHITEN + DEFAULT_BLOCK_SIZE / 4;
  private static final int ROUND_SUBKEYS = OUTPUT_WHITEN + DEFAULT_BLOCK_SIZE / 4;
  private static final int SK_STEP = 0x02020202;
  private static final int SK_BUMP = 0x01010101;
  private static final int SK_ROTL = 9;
  private static final String[] Pm = new String[] {
      // p0
      "\uA967\uB3E8\u04FD\uA376\u9A92\u8078\uE4DD\uD138"
    + "\u0DC6\u3598\u18F7\uEC6C\u4375\u3726\uFA13\u9448"
    + "\uF2D0\u8B30\u8454\uDF23\u195B\u3D59\uF3AE\uA282"
    + "\u6301\u832E\uD951\u9B7C\uA6EB\uA5BE\u160C\uE361"
    + "\uC08C\u3AF5\u732C\u250B\uBB4E\u896B\u536A\uB4F1"
    + "\uE1E6\uBD45\uE2F4\uB666\uCC95\u0356\uD41C\u1ED7"
    + "\uFBC3\u8EB5\uE9CF\uBFBA\uEA77\u39AF\u33C9\u6271"
    + "\u8179\u09AD\u24CD\uF9D8\uE5C5\uB94D\u4408\u86E7"
    + "\uA11D\uAAED\u0670\uB2D2\u417B\uA011\u31C2\u2790"
    + "\u20F6\u60FF\u965C\uB1AB\u9E9C\u521B\u5F93\u0AEF"
    + "\u9185\u49EE\u2D4F\u8F3B\u4787\u6D46\uD63E\u6964"
    + "\u2ACE\uCB2F\uFC97\u057A\uAC7F\uD51A\u4B0E\uA75A"
    + "\u2814\u3F29\u883C\u4C02\uB8DA\uB017\u551F\u8A7D"
    + "\u57C7\u8D74\uB7C4\u9F72\u7E15\u2212\u5807\u9934"
    + "\u6E50\uDE68\u65BC\uDBF8\uC8A8\u2B40\uDCFE\u32A4"
    + "\uCA10\u21F0\uD35D\u0F00\u6F9D\u3642\u4A5E\uC1E0",
      // p1
      "\u75F3\uC6F4\uDB7B\uFBC8\u4AD3\uE66B\u457D\uE84B"
    + "\uD632\uD8FD\u3771\uF1E1\u300F\uF81B\u87FA\u063F"
    + "\u5EBA\uAE5B\u8A00\uBC9D\u6DC1\uB10E\u805D\uD2D5"
    + "\uA084\u0714\uB590\u2CA3\uB273\u4C54\u9274\u3651"
    + "\u38B0\uBD5A\uFC60\u6296\u6C42\uF710\u7C28\u278C"
    + "\u1395\u9CC7\u2446\u3B70\uCAE3\u85CB\u11D0\u93B8"
    + "\uA683\u20FF\u9F77\uC3CC\u036F\u08BF\u40E7\u2BE2"
    + "\u790C\uAA82\u413A\uEAB9\uE49A\uA497\u7EDA\u7A17"
    + "\u6694\uA11D\u3DF0\uDEB3\u0B72\uA71C\uEFD1\u533E"
    + "\u8F33\u265F\uEC76\u2A49\u8188\uEE21\uC41A\uEBD9"
    + "\uC539\u99CD\uAD31\u8B01\u1823\uDD1F\u4E2D\uF948"
    + "\u4FF2\u658E\u785C\u5819\u8DE5\u9857\u677F\u0564"
    + "\uAF63\uB6FE\uF5B7\u3CA5\uCEE9\u6844\uE04D\u4369"
    + "\u292E\uAC15\u59A8\u0A9E\u6E47\uDF34\u356A\uCFDC"
    + "\u22C9\uC09B\u89D4\uEDAB\u12A2\u0D52\uBB02\u2FA9"
    + "\uD761\u1EB4\u5004\uF6C2\u1625\u8656\u5509\uBE91" };
  /** Fixed 8x8 permutation S-boxes */
  private static final byte[][] P = new byte[2][256]; // blank final
  /**
   * Define the fixed p0/p1 permutations used in keyed S-box lookup. By
   * changing the following constant definitions, the S-boxes will
   * automatically get changed in the Twofish engine.
   */
  private static final int P_00 = 1;
  private static final int P_01 = 0;
  private static final int P_02 = 0;
  private static final int P_03 = P_01 ^ 1;
  private static final int P_04 = 1;
  private static final int P_10 = 0;
  private static final int P_11 = 0;
  private static final int P_12 = 1;
  private static final int P_13 = P_11 ^ 1;
  private static final int P_14 = 0;
  private static final int P_20 = 1;
  private static final int P_21 = 1;
  private static final int P_22 = 0;
  private static final int P_23 = P_21 ^ 1;
  private static final int P_24 = 0;
  private static final int P_30 = 0;
  private static final int P_31 = 1;
  private static final int P_32 = 1;
  private static final int P_33 = P_31 ^ 1;
  private static final int P_34 = 1;
  /** Primitive polynomial for GF(256) */
  private static final int GF256_FDBK_2 = 0x169 / 2;
  private static final int GF256_FDBK_4 = 0x169 / 4;
  /** MDS matrix */
  private static final int[][] MDS = new int[4][256]; // blank final
  private static final int RS_GF_FDBK = 0x14D; // field generator
  /**
   * KAT vector (from ecb_vk):
   * I=183
   * KEY=0000000000000000000000000000000000000000000002000000000000000000
   * CT=F51410475B33FBD3DB2117B5C17C82D4
   */
  private static final byte[] KAT_KEY = Util.toBytesFromString(
      "0000000000000000000000000000000000000000000002000000000000000000");
  private static final byte[] KAT_CT =
      Util.toBytesFromString("F51410475B33FBD3DB2117B5C17C82D4");
  /** caches the result of the correctness test, once executed. */
  private static Boolean valid;
  static
    {
      long time = System.currentTimeMillis();
      // expand the P arrays
      int i;
      char c;
      for (i = 0; i < 256; i++)
        {
          c = Pm[0].charAt(i >>> 1);
          P[0][i] = (byte)((i & 1) == 0 ? c >>> 8 : c);
          c = Pm[1].charAt(i >>> 1);
          P[1][i] = (byte)((i & 1) == 0 ? c >>> 8 : c);
        }
      // precompute the MDS matrix
      int[] m1 = new int[2];
      int[] mX = new int[2];
      int[] mY = new int[2];
      int j;
      for (i = 0; i < 256; i++)
        {
          j = P[0][i] & 0xFF; // compute all the matrix elements
          m1[0] = j;
          mX[0] = Mx_X(j) & 0xFF;
          mY[0] = Mx_Y(j) & 0xFF;
          j = P[1][i] & 0xFF;
          m1[1] = j;
          mX[1] = Mx_X(j) & 0xFF;
          mY[1] = Mx_Y(j) & 0xFF;
          MDS[0][i] = m1[P_00] << 0
                    | mX[P_00] << 8
                    | mY[P_00] << 16
                    | mY[P_00] << 24;
          MDS[1][i] = mY[P_10] << 0
                    | mY[P_10] << 8
                    | mX[P_10] << 16
                    | m1[P_10] << 24;
          MDS[2][i] = mX[P_20] << 0
                    | mY[P_20] << 8
                    | m1[P_20] << 16
                    | mY[P_20] << 24;
          MDS[3][i] = mX[P_30] << 0
                    | m1[P_30] << 8
                    | mY[P_30] << 16
                    | mX[P_30] << 24;
        }
      time = System.currentTimeMillis() - time;
      if (Configuration.DEBUG)
        {
          log.fine("Static Data");
          log.fine("MDS[0][]:");
          StringBuilder sb;
          for (i = 0; i < 64; i++)
            {
              sb = new StringBuilder();
              for (j = 0; j < 4; j++)
                sb.append("0x").append(Util.toString(MDS[0][i * 4 + j])).append(", ");
              log.fine(sb.toString());
            }
          log.fine("MDS[1][]:");
          for (i = 0; i < 64; i++)
            {
              sb = new StringBuilder();
              for (j = 0; j < 4; j++)
                sb.append("0x").append(Util.toString(MDS[1][i * 4 + j])).append(", ");
              log.fine(sb.toString());
            }
          log.fine("MDS[2][]:");
          for (i = 0; i < 64; i++)
            {
              sb = new StringBuilder();
              for (j = 0; j < 4; j++)
                sb.append("0x").append(Util.toString(MDS[2][i * 4 + j])).append(", ");
              log.fine(sb.toString());
            }
          log.fine("MDS[3][]:");
          for (i = 0; i < 64; i++)
            {
              sb = new StringBuilder();
              for (j = 0; j < 4; j++)
                sb.append("0x").append(Util.toString(MDS[3][i * 4 + j])).append(", ");
              log.fine(sb.toString());
            }
          log.fine("Total initialization time: " + time + " ms.");
        }
    }

  private static final int LFSR1(int x)
  {
    return (x >> 1) ^ ((x & 0x01) != 0 ? GF256_FDBK_2 : 0);
  }

  private static final int LFSR2(int x)
  {
    return (x >> 2)
        ^ ((x & 0x02) != 0 ? GF256_FDBK_2 : 0)
        ^ ((x & 0x01) != 0 ? GF256_FDBK_4 : 0);
  }

  private static final int Mx_X(int x)
  { // 5B
    return x ^ LFSR2(x);
  }

  private static final int Mx_Y(int x)
  { // EF
    return x ^ LFSR1(x) ^ LFSR2(x);
  }

  /** Trivial 0-arguments constructor. */
  public Twofish()
  {
    super(Registry.TWOFISH_CIPHER, DEFAULT_BLOCK_SIZE, DEFAULT_KEY_SIZE);
  }

  private static final int b0(int x)
  {
    return x & 0xFF;
  }

  private static final int b1(int x)
  {
    return (x >>> 8) & 0xFF;
  }

  private static final int b2(int x)
  {
    return (x >>> 16) & 0xFF;
  }

  private static final int b3(int x)
  {
    return (x >>> 24) & 0xFF;
  }

  /**
   * Use (12, 8) Reed-Solomon code over GF(256) to produce a key S-box 32-bit
   * entity from two key material 32-bit entities.
   *
   * @param k0 1st 32-bit entity.
   * @param k1 2nd 32-bit entity.
   * @return remainder polynomial generated using RS code
   */
  private static final int RS_MDS_Encode(int k0, int k1)
  {
    int r = k1;
    int i;
    for (i = 0; i < 4; i++) // shift 1 byte at a time
      r = RS_rem(r);
    r ^= k0;
    for (i = 0; i < 4; i++)
      r = RS_rem(r);
    return r;
  }

  /**
   * Reed-Solomon code parameters: (12, 8) reversible code:<p>
   * <pre>
   *   g(x) = x**4 + (a + 1/a) x**3 + a x**2 + (a + 1/a) x + 1
   * </pre>
   * where a = primitive root of field generator 0x14D
   */
  private static final int RS_rem(int x)
  {
    int b = (x >>> 24) & 0xFF;
    int g2 = ((b << 1) ^ ((b & 0x80) != 0 ? RS_GF_FDBK : 0)) & 0xFF;
    int g3 = (b >>> 1) ^ ((b & 0x01) != 0 ? (RS_GF_FDBK >>> 1) : 0) ^ g2;
    int result = (x << 8) ^ (g3 << 24) ^ (g2 << 16) ^ (g3 << 8) ^ b;
    return result;
  }

  private static final int F32(int k64Cnt, int x, int[] k32)
  {
    int b0 = b0(x);
    int b1 = b1(x);
    int b2 = b2(x);
    int b3 = b3(x);
    int k0 = k32[0];
    int k1 = k32[1];
    int k2 = k32[2];
    int k3 = k32[3];
    int result = 0;
    switch (k64Cnt & 3)
      {
      case 1:
        result = MDS[0][(P[P_01][b0] & 0xFF) ^ b0(k0)]
               ^ MDS[1][(P[P_11][b1] & 0xFF) ^ b1(k0)]
               ^ MDS[2][(P[P_21][b2] & 0xFF) ^ b2(k0)]
               ^ MDS[3][(P[P_31][b3] & 0xFF) ^ b3(k0)];
        break;
      case 0: // same as 4
        b0 = (P[P_04][b0] & 0xFF) ^ b0(k3);
        b1 = (P[P_14][b1] & 0xFF) ^ b1(k3);
        b2 = (P[P_24][b2] & 0xFF) ^ b2(k3);
        b3 = (P[P_34][b3] & 0xFF) ^ b3(k3);
      case 3:
        b0 = (P[P_03][b0] & 0xFF) ^ b0(k2);
        b1 = (P[P_13][b1] & 0xFF) ^ b1(k2);
        b2 = (P[P_23][b2] & 0xFF) ^ b2(k2);
        b3 = (P[P_33][b3] & 0xFF) ^ b3(k2);
      case 2: // 128-bit keys (optimize for this case)
        result = MDS[0][(P[P_01][(P[P_02][b0] & 0xFF) ^ b0(k1)] & 0xFF) ^ b0(k0)]
               ^ MDS[1][(P[P_11][(P[P_12][b1] & 0xFF) ^ b1(k1)] & 0xFF) ^ b1(k0)]
               ^ MDS[2][(P[P_21][(P[P_22][b2] & 0xFF) ^ b2(k1)] & 0xFF) ^ b2(k0)]
               ^ MDS[3][(P[P_31][(P[P_32][b3] & 0xFF) ^ b3(k1)] & 0xFF) ^ b3(k0)];
        break;
      }
    return result;
  }

  private static final int Fe32(int[] sBox, int x, int R)
  {
    return sBox[        2 * _b(x, R    )    ]
         ^ sBox[        2 * _b(x, R + 1) + 1]
         ^ sBox[0x200 + 2 * _b(x, R + 2)    ]
         ^ sBox[0x200 + 2 * _b(x, R + 3) + 1];
  }

  private static final int _b(int x, int N)
  {
    switch (N % 4)
      {
      case 0:
        return x & 0xFF;
      case 1:
        return (x >>> 8) & 0xFF;
      case 2:
        return (x >>> 16) & 0xFF;
      default:
        return x >>> 24;
      }
  }

  public Object clone()
  {
    Twofish result = new Twofish();
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
    al.add(Integer.valueOf(8)); //   64-bit
    al.add(Integer.valueOf(16)); // 128-bit
    al.add(Integer.valueOf(24)); // 192-bit
    al.add(Integer.valueOf(32)); // 256-bit
    return Collections.unmodifiableList(al).iterator();
  }

  /**
   * Expands a user-supplied key material into a session key for a designated
   * <i>block size</i>.
   *
   * @param k the 64/128/192/256-bit user-key to use.
   * @param bs the desired block size in bytes.
   * @return an Object encapsulating the session key.
   * @exception IllegalArgumentException if the block size is not 16 (128-bit).
   * @exception InvalidKeyException if the key data is invalid.
   */
  public Object makeKey(byte[] k, int bs) throws InvalidKeyException
  {
    if (bs != DEFAULT_BLOCK_SIZE)
      throw new IllegalArgumentException();
    if (k == null)
      throw new InvalidKeyException("Empty key");
    int length = k.length;
    if (! (length == 8 || length == 16 || length == 24 || length == 32))
      throw new InvalidKeyException("Incorrect key length");
    int k64Cnt = length / 8;
    int subkeyCnt = ROUND_SUBKEYS + 2 * ROUNDS;
    int[] k32e = new int[4]; // even 32-bit entities
    int[] k32o = new int[4]; // odd 32-bit entities
    int[] sBoxKey = new int[4];
    // split user key material into even and odd 32-bit entities and
    // compute S-box keys using (12, 8) Reed-Solomon code over GF(256)
    int i, j, offset = 0;
    for (i = 0, j = k64Cnt - 1; i < 4 && offset < length; i++, j--)
      {
        k32e[i] = (k[offset++] & 0xFF)
                | (k[offset++] & 0xFF) << 8
                | (k[offset++] & 0xFF) << 16
                | (k[offset++] & 0xFF) << 24;
        k32o[i] = (k[offset++] & 0xFF)
                | (k[offset++] & 0xFF) << 8
                | (k[offset++] & 0xFF) << 16
                | (k[offset++] & 0xFF) << 24;
        sBoxKey[j] = RS_MDS_Encode(k32e[i], k32o[i]); // reverse order
      }
    // compute the round decryption subkeys for PHT. these same subkeys
    // will be used in encryption but will be applied in reverse order.
    int q, A, B;
    int[] subKeys = new int[subkeyCnt];
    for (i = q = 0; i < subkeyCnt / 2; i++, q += SK_STEP)
      {
        A = F32(k64Cnt, q, k32e); // A uses even key entities
        B = F32(k64Cnt, q + SK_BUMP, k32o); // B uses odd  key entities
        B = B << 8 | B >>> 24;
        A += B;
        subKeys[2 * i] = A; // combine with a PHT
        A += B;
        subKeys[2 * i + 1] = A << SK_ROTL | A >>> (32 - SK_ROTL);
      }
    // fully expand the table for speed
    int k0 = sBoxKey[0];
    int k1 = sBoxKey[1];
    int k2 = sBoxKey[2];
    int k3 = sBoxKey[3];
    int b0, b1, b2, b3;
    int[] sBox = new int[4 * 256];
    for (i = 0; i < 256; i++)
      {
        b0 = b1 = b2 = b3 = i;
        switch (k64Cnt & 3)
          {
          case 1:
            sBox[        2 * i    ] = MDS[0][(P[P_01][b0] & 0xFF) ^ b0(k0)];
            sBox[        2 * i + 1] = MDS[1][(P[P_11][b1] & 0xFF) ^ b1(k0)];
            sBox[0x200 + 2 * i    ] = MDS[2][(P[P_21][b2] & 0xFF) ^ b2(k0)];
            sBox[0x200 + 2 * i + 1] = MDS[3][(P[P_31][b3] & 0xFF) ^ b3(k0)];
            break;
          case 0: // same as 4
            b0 = (P[P_04][b0] & 0xFF) ^ b0(k3);
            b1 = (P[P_14][b1] & 0xFF) ^ b1(k3);
            b2 = (P[P_24][b2] & 0xFF) ^ b2(k3);
            b3 = (P[P_34][b3] & 0xFF) ^ b3(k3);
          case 3:
            b0 = (P[P_03][b0] & 0xFF) ^ b0(k2);
            b1 = (P[P_13][b1] & 0xFF) ^ b1(k2);
            b2 = (P[P_23][b2] & 0xFF) ^ b2(k2);
            b3 = (P[P_33][b3] & 0xFF) ^ b3(k2);
          case 2: // 128-bit keys
            sBox[        2 * i    ] = MDS[0][(P[P_01][(P[P_02][b0] & 0xFF)
                                                      ^ b0(k1)] & 0xFF) ^ b0(k0)];
            sBox[        2 * i + 1] = MDS[1][(P[P_11][(P[P_12][b1] & 0xFF)
                                                      ^ b1(k1)] & 0xFF) ^ b1(k0)];
            sBox[0x200 + 2 * i    ] = MDS[2][(P[P_21][(P[P_22][b2] & 0xFF)
                                                      ^ b2(k1)] & 0xFF) ^ b2(k0)];
            sBox[0x200 + 2 * i + 1] = MDS[3][(P[P_31][(P[P_32][b3] & 0xFF)
                                                      ^ b3(k1)] & 0xFF) ^ b3(k0)];
          }
      }
    if (Configuration.DEBUG)
      {
        StringBuilder sb;
        log.fine("S-box[]:");
        for (i = 0; i < 64; i++)
          {
            sb = new StringBuilder();
            for (j = 0; j < 4; j++)
              sb.append("0x").append(Util.toString(sBox[i * 4 + j])).append(", ");
            log.fine(sb.toString());
          }
        log.fine("");
        for (i = 0; i < 64; i++)
          {
            sb = new StringBuilder();
            for (j = 0; j < 4; j++)
              sb.append("0x").append(Util.toString(sBox[256 + i * 4 + j])).append(", ");
            log.fine(sb.toString());
          }
        log.fine("");
        for (i = 0; i < 64; i++)
          {
            sb = new StringBuilder();
            for (j = 0; j < 4; j++)
              sb.append("0x").append(Util.toString(sBox[512 + i * 4 + j])).append(", ");
            log.fine(sb.toString());
          }
        log.fine("");
        for (i = 0; i < 64; i++)
          {
            sb = new StringBuilder();
            for (j = 0; j < 4; j++)
              sb.append("0x").append(Util.toString(sBox[768 + i * 4 + j])).append(", ");
            log.fine(sb.toString());
          }
        log.fine("User (odd, even) keys  --> S-Box keys:");
        for (i = 0; i < k64Cnt; i++)
          log.fine("0x" + Util.toString(k32o[i])
                   + "  0x" + Util.toString(k32e[i])
                   + " --> 0x" + Util.toString(sBoxKey[k64Cnt - 1 - i]));
        log.fine("Round keys:");
        for (i = 0; i < ROUND_SUBKEYS + 2 * ROUNDS; i += 2)
          log.fine("0x" + Util.toString(subKeys[i])
                   + "  0x" + Util.toString(subKeys[i + 1]));
      }
    return new Object[] { sBox, subKeys };
  }

  public void encrypt(byte[] in, int inOffset, byte[] out, int outOffset,
                      Object sessionKey, int bs)
  {
    if (bs != DEFAULT_BLOCK_SIZE)
      throw new IllegalArgumentException();
    Object[] sk = (Object[]) sessionKey; // extract S-box and session key
    int[] sBox = (int[]) sk[0];
    int[] sKey = (int[]) sk[1];
    if (Configuration.DEBUG)
      log.fine("PT=" + Util.toString(in, inOffset, bs));
    int x0 = (in[inOffset++] & 0xFF)
           | (in[inOffset++] & 0xFF) << 8
           | (in[inOffset++] & 0xFF) << 16
           | (in[inOffset++] & 0xFF) << 24;
    int x1 = (in[inOffset++] & 0xFF)
           | (in[inOffset++] & 0xFF) << 8
           | (in[inOffset++] & 0xFF) << 16
           | (in[inOffset++] & 0xFF) << 24;
    int x2 = (in[inOffset++] & 0xFF)
           | (in[inOffset++] & 0xFF) << 8
           | (in[inOffset++] & 0xFF) << 16
           | (in[inOffset++] & 0xFF) << 24;
    int x3 = (in[inOffset++] & 0xFF)
           | (in[inOffset++] & 0xFF) << 8
           | (in[inOffset++] & 0xFF) << 16
           | (in[inOffset++] & 0xFF) << 24;
    x0 ^= sKey[INPUT_WHITEN];
    x1 ^= sKey[INPUT_WHITEN + 1];
    x2 ^= sKey[INPUT_WHITEN + 2];
    x3 ^= sKey[INPUT_WHITEN + 3];
    if (Configuration.DEBUG)
      log.fine("PTw=" + Util.toString(x0) + Util.toString(x1)
               + Util.toString(x2) + Util.toString(x3));
    int t0, t1;
    int k = ROUND_SUBKEYS;
    for (int R = 0; R < ROUNDS; R += 2)
      {
        t0 = Fe32(sBox, x0, 0);
        t1 = Fe32(sBox, x1, 3);
        x2 ^= t0 + t1 + sKey[k++];
        x2 = x2 >>> 1 | x2 << 31;
        x3 = x3 << 1 | x3 >>> 31;
        x3 ^= t0 + 2 * t1 + sKey[k++];
        if (Configuration.DEBUG)
          log.fine("CT" + (R) + "=" + Util.toString(x0) + Util.toString(x1)
                   + Util.toString(x2) + Util.toString(x3));
        t0 = Fe32(sBox, x2, 0);
        t1 = Fe32(sBox, x3, 3);
        x0 ^= t0 + t1 + sKey[k++];
        x0 = x0 >>> 1 | x0 << 31;
        x1 = x1 << 1 | x1 >>> 31;
        x1 ^= t0 + 2 * t1 + sKey[k++];
        if (Configuration.DEBUG)
          log.fine("CT" + (R + 1) + "=" + Util.toString(x0) + Util.toString(x1)
                   + Util.toString(x2) + Util.toString(x3));
      }
    x2 ^= sKey[OUTPUT_WHITEN];
    x3 ^= sKey[OUTPUT_WHITEN + 1];
    x0 ^= sKey[OUTPUT_WHITEN + 2];
    x1 ^= sKey[OUTPUT_WHITEN + 3];
    if (Configuration.DEBUG)
      log.fine("CTw=" + Util.toString(x0) + Util.toString(x1)
               + Util.toString(x2) + Util.toString(x3));
    out[outOffset++] = (byte) x2;
    out[outOffset++] = (byte)(x2 >>> 8);
    out[outOffset++] = (byte)(x2 >>> 16);
    out[outOffset++] = (byte)(x2 >>> 24);
    out[outOffset++] = (byte) x3;
    out[outOffset++] = (byte)(x3 >>> 8);
    out[outOffset++] = (byte)(x3 >>> 16);
    out[outOffset++] = (byte)(x3 >>> 24);
    out[outOffset++] = (byte) x0;
    out[outOffset++] = (byte)(x0 >>> 8);
    out[outOffset++] = (byte)(x0 >>> 16);
    out[outOffset++] = (byte)(x0 >>> 24);
    out[outOffset++] = (byte) x1;
    out[outOffset++] = (byte)(x1 >>> 8);
    out[outOffset++] = (byte)(x1 >>> 16);
    out[outOffset  ] = (byte)(x1 >>> 24);
    if (Configuration.DEBUG)
      log.fine("CT=" + Util.toString(out, outOffset - 15, 16) + "\n");
  }

  public void decrypt(byte[] in, int inOffset, byte[] out, int outOffset,
                      Object sessionKey, int bs)
  {
    if (bs != DEFAULT_BLOCK_SIZE)
      throw new IllegalArgumentException();
    Object[] sk = (Object[]) sessionKey; // extract S-box and session key
    int[] sBox = (int[]) sk[0];
    int[] sKey = (int[]) sk[1];
    if (Configuration.DEBUG)
      log.fine("CT=" + Util.toString(in, inOffset, bs));
    int x2 = (in[inOffset++] & 0xFF)
           | (in[inOffset++] & 0xFF) << 8
           | (in[inOffset++] & 0xFF) << 16
           | (in[inOffset++] & 0xFF) << 24;
    int x3 = (in[inOffset++] & 0xFF)
           | (in[inOffset++] & 0xFF) << 8
           | (in[inOffset++] & 0xFF) << 16
           | (in[inOffset++] & 0xFF) << 24;
    int x0 = (in[inOffset++] & 0xFF)
           | (in[inOffset++] & 0xFF) << 8
           | (in[inOffset++] & 0xFF) << 16
           | (in[inOffset++] & 0xFF) << 24;
    int x1 = (in[inOffset++] & 0xFF)
           | (in[inOffset++] & 0xFF) << 8
           | (in[inOffset++] & 0xFF) << 16
           | (in[inOffset++] & 0xFF) << 24;
    x2 ^= sKey[OUTPUT_WHITEN];
    x3 ^= sKey[OUTPUT_WHITEN + 1];
    x0 ^= sKey[OUTPUT_WHITEN + 2];
    x1 ^= sKey[OUTPUT_WHITEN + 3];
    if (Configuration.DEBUG)
      log.fine("CTw=" + Util.toString(x2) + Util.toString(x3)
               + Util.toString(x0) + Util.toString(x1));
    int k = ROUND_SUBKEYS + 2 * ROUNDS - 1;
    int t0, t1;
    for (int R = 0; R < ROUNDS; R += 2)
      {
        t0 = Fe32(sBox, x2, 0);
        t1 = Fe32(sBox, x3, 3);
        x1 ^= t0 + 2 * t1 + sKey[k--];
        x1 = x1 >>> 1 | x1 << 31;
        x0 = x0 << 1 | x0 >>> 31;
        x0 ^= t0 + t1 + sKey[k--];
        if (Configuration.DEBUG)
          log.fine("PT" + (ROUNDS - R) + "=" + Util.toString(x2)
                   + Util.toString(x3) + Util.toString(x0) + Util.toString(x1));
        t0 = Fe32(sBox, x0, 0);
        t1 = Fe32(sBox, x1, 3);
        x3 ^= t0 + 2 * t1 + sKey[k--];
        x3 = x3 >>> 1 | x3 << 31;
        x2 = x2 << 1 | x2 >>> 31;
        x2 ^= t0 + t1 + sKey[k--];
        if (Configuration.DEBUG)
          log.fine("PT" + (ROUNDS - R - 1) + "=" + Util.toString(x2)
                   + Util.toString(x3) + Util.toString(x0) + Util.toString(x1));
      }
    x0 ^= sKey[INPUT_WHITEN];
    x1 ^= sKey[INPUT_WHITEN + 1];
    x2 ^= sKey[INPUT_WHITEN + 2];
    x3 ^= sKey[INPUT_WHITEN + 3];
    if (Configuration.DEBUG)
      log.fine("PTw=" + Util.toString(x2) + Util.toString(x3)
               + Util.toString(x0) + Util.toString(x1));
    out[outOffset++] = (byte) x0;
    out[outOffset++] = (byte)(x0 >>> 8);
    out[outOffset++] = (byte)(x0 >>> 16);
    out[outOffset++] = (byte)(x0 >>> 24);
    out[outOffset++] = (byte) x1;
    out[outOffset++] = (byte)(x1 >>> 8);
    out[outOffset++] = (byte)(x1 >>> 16);
    out[outOffset++] = (byte)(x1 >>> 24);
    out[outOffset++] = (byte) x2;
    out[outOffset++] = (byte)(x2 >>> 8);
    out[outOffset++] = (byte)(x2 >>> 16);
    out[outOffset++] = (byte)(x2 >>> 24);
    out[outOffset++] = (byte) x3;
    out[outOffset++] = (byte)(x3 >>> 8);
    out[outOffset++] = (byte)(x3 >>> 16);
    out[outOffset  ] = (byte)(x3 >>> 24);
    if (Configuration.DEBUG)
      log.fine("PT=" + Util.toString(out, outOffset - 15, 16) + "\n");
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
