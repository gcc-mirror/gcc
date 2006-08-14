/* Serpent.java -- 
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
 * Serpent is a 32-round substitution-permutation network block cipher,
 * operating on 128-bit blocks and accepting keys of 128, 192, and 256 bits in
 * length. At each round the plaintext is XORed with a 128 bit portion of the
 * session key -- a 4224 bit key computed from the input key -- then one of
 * eight S-boxes are applied, and finally a simple linear transformation is
 * done. Decryption does the exact same thing in reverse order, and using the
 * eight inverses of the S-boxes.
 * <p>
 * Serpent was designed by Ross Anderson, Eli Biham, and Lars Knudsen as a
 * proposed cipher for the Advanced Encryption Standard.
 * <p>
 * Serpent can be sped up greatly by replacing S-box substitution with a
 * sequence of binary operations, and the optimal implementation depends upon
 * finding the fastest sequence of binary operations that reproduce this
 * substitution. This implementation uses the S-boxes discovered by <a
 * href="http://www.ii.uib.no/~osvik/">Dag Arne Osvik</a>, which are optimized
 * for the Pentium family of processors.
 * <p>
 * References:
 * <ol>
 * <li><a href="http://www.cl.cam.ac.uk/~rja14/serpent.html">Serpent: A
 * Candidate Block Cipher for the Advanced Encryption Standard.</a></li>
 * </ol>
 */
public class Serpent
    extends BaseCipher
{
  private static final int DEFAULT_KEY_SIZE = 16;
  private static final int DEFAULT_BLOCK_SIZE = 16;
  private static final int ROUNDS = 32;
  /** The fractional part of the golden ratio, (sqrt(5)+1)/2. */
  private static final int PHI = 0x9e3779b9;
  /**
   * KAT vector (from ecb_vk): I=9
   * KEY=008000000000000000000000000000000000000000000000
   * CT=5587B5BCB9EE5A28BA2BACC418005240
   */
  private static final byte[] KAT_KEY = Util.toReversedBytesFromString(
      "008000000000000000000000000000000000000000000000");
  private static final byte[] KAT_CT =
      Util.toReversedBytesFromString("5587B5BCB9EE5A28BA2BACC418005240");
  /** caches the result of the correctness test, once executed. */
  private static Boolean valid;
  private int x0, x1, x2, x3, x4;

  /** Trivial zero-argument constructor. */
  public Serpent()
  {
    super(Registry.SERPENT_CIPHER, DEFAULT_BLOCK_SIZE, DEFAULT_KEY_SIZE);
  }

  public Object clone()
  {
    Serpent result = new Serpent();
    result.currentBlockSize = this.currentBlockSize;
    return result;
  }

  public Iterator blockSizes()
  {
    return Collections.singleton(Integer.valueOf(DEFAULT_BLOCK_SIZE)).iterator();
  }

  public Iterator keySizes()
  {
    ArrayList keySizes = new ArrayList();
    keySizes.add(Integer.valueOf(16));
    keySizes.add(Integer.valueOf(24));
    keySizes.add(Integer.valueOf(32));
    return Collections.unmodifiableList(keySizes).iterator();
  }

  public Object makeKey(byte[] kb, int blockSize) throws InvalidKeyException
  {
    // Not strictly true, but here to conform with the AES proposal.
    // This restriction can be removed if deemed necessary.
    if (kb.length != 16 && kb.length != 24 && kb.length != 32)
      throw new InvalidKeyException("Key length is not 16, 24, or 32 bytes");
    Key key = new Key();
    // Here w is our "pre-key".
    int[] w = new int[4 * (ROUNDS + 1)];
    int i, j;
    for (i = 0, j = 0; i < 8 && j < kb.length; i++)
      w[i] = (kb[j++] & 0xff)
           | (kb[j++] & 0xff) << 8
           | (kb[j++] & 0xff) << 16
           | (kb[j++] & 0xff) << 24;
    // Pad key if < 256 bits.
    if (i != 8)
      w[i] = 1;
    // Transform using w_i-8 ... w_i-1
    for (i = 8, j = 0; i < 16; i++)
      {
        int t = w[j] ^ w[i - 5] ^ w[i - 3] ^ w[i - 1] ^ PHI ^ j++;
        w[i] = t << 11 | t >>> 21;
      }
    // Translate by 8.
    for (i = 0; i < 8; i++)
      w[i] = w[i + 8];
    // Transform the rest of the key.
    for (; i < w.length; i++)
      {
        int t = w[i - 8] ^ w[i - 5] ^ w[i - 3] ^ w[i - 1] ^ PHI ^ i;
        w[i] = t << 11 | t >>> 21;
      }
    // After these s-boxes the pre-key (w, above) will become the
    // session key (key, below).
    sbox3(w[0], w[1], w[2], w[3]);
    key.k0 = x0;
    key.k1 = x1;
    key.k2 = x2;
    key.k3 = x3;
    sbox2(w[4], w[5], w[6], w[7]);
    key.k4 = x0;
    key.k5 = x1;
    key.k6 = x2;
    key.k7 = x3;
    sbox1(w[8], w[9], w[10], w[11]);
    key.k8 = x0;
    key.k9 = x1;
    key.k10 = x2;
    key.k11 = x3;
    sbox0(w[12], w[13], w[14], w[15]);
    key.k12 = x0;
    key.k13 = x1;
    key.k14 = x2;
    key.k15 = x3;
    sbox7(w[16], w[17], w[18], w[19]);
    key.k16 = x0;
    key.k17 = x1;
    key.k18 = x2;
    key.k19 = x3;
    sbox6(w[20], w[21], w[22], w[23]);
    key.k20 = x0;
    key.k21 = x1;
    key.k22 = x2;
    key.k23 = x3;
    sbox5(w[24], w[25], w[26], w[27]);
    key.k24 = x0;
    key.k25 = x1;
    key.k26 = x2;
    key.k27 = x3;
    sbox4(w[28], w[29], w[30], w[31]);
    key.k28 = x0;
    key.k29 = x1;
    key.k30 = x2;
    key.k31 = x3;
    sbox3(w[32], w[33], w[34], w[35]);
    key.k32 = x0;
    key.k33 = x1;
    key.k34 = x2;
    key.k35 = x3;
    sbox2(w[36], w[37], w[38], w[39]);
    key.k36 = x0;
    key.k37 = x1;
    key.k38 = x2;
    key.k39 = x3;
    sbox1(w[40], w[41], w[42], w[43]);
    key.k40 = x0;
    key.k41 = x1;
    key.k42 = x2;
    key.k43 = x3;
    sbox0(w[44], w[45], w[46], w[47]);
    key.k44 = x0;
    key.k45 = x1;
    key.k46 = x2;
    key.k47 = x3;
    sbox7(w[48], w[49], w[50], w[51]);
    key.k48 = x0;
    key.k49 = x1;
    key.k50 = x2;
    key.k51 = x3;
    sbox6(w[52], w[53], w[54], w[55]);
    key.k52 = x0;
    key.k53 = x1;
    key.k54 = x2;
    key.k55 = x3;
    sbox5(w[56], w[57], w[58], w[59]);
    key.k56 = x0;
    key.k57 = x1;
    key.k58 = x2;
    key.k59 = x3;
    sbox4(w[60], w[61], w[62], w[63]);
    key.k60 = x0;
    key.k61 = x1;
    key.k62 = x2;
    key.k63 = x3;
    sbox3(w[64], w[65], w[66], w[67]);
    key.k64 = x0;
    key.k65 = x1;
    key.k66 = x2;
    key.k67 = x3;
    sbox2(w[68], w[69], w[70], w[71]);
    key.k68 = x0;
    key.k69 = x1;
    key.k70 = x2;
    key.k71 = x3;
    sbox1(w[72], w[73], w[74], w[75]);
    key.k72 = x0;
    key.k73 = x1;
    key.k74 = x2;
    key.k75 = x3;
    sbox0(w[76], w[77], w[78], w[79]);
    key.k76 = x0;
    key.k77 = x1;
    key.k78 = x2;
    key.k79 = x3;
    sbox7(w[80], w[81], w[82], w[83]);
    key.k80 = x0;
    key.k81 = x1;
    key.k82 = x2;
    key.k83 = x3;
    sbox6(w[84], w[85], w[86], w[87]);
    key.k84 = x0;
    key.k85 = x1;
    key.k86 = x2;
    key.k87 = x3;
    sbox5(w[88], w[89], w[90], w[91]);
    key.k88 = x0;
    key.k89 = x1;
    key.k90 = x2;
    key.k91 = x3;
    sbox4(w[92], w[93], w[94], w[95]);
    key.k92 = x0;
    key.k93 = x1;
    key.k94 = x2;
    key.k95 = x3;
    sbox3(w[96], w[97], w[98], w[99]);
    key.k96 = x0;
    key.k97 = x1;
    key.k98 = x2;
    key.k99 = x3;
    sbox2(w[100], w[101], w[102], w[103]);
    key.k100 = x0;
    key.k101 = x1;
    key.k102 = x2;
    key.k103 = x3;
    sbox1(w[104], w[105], w[106], w[107]);
    key.k104 = x0;
    key.k105 = x1;
    key.k106 = x2;
    key.k107 = x3;
    sbox0(w[108], w[109], w[110], w[111]);
    key.k108 = x0;
    key.k109 = x1;
    key.k110 = x2;
    key.k111 = x3;
    sbox7(w[112], w[113], w[114], w[115]);
    key.k112 = x0;
    key.k113 = x1;
    key.k114 = x2;
    key.k115 = x3;
    sbox6(w[116], w[117], w[118], w[119]);
    key.k116 = x0;
    key.k117 = x1;
    key.k118 = x2;
    key.k119 = x3;
    sbox5(w[120], w[121], w[122], w[123]);
    key.k120 = x0;
    key.k121 = x1;
    key.k122 = x2;
    key.k123 = x3;
    sbox4(w[124], w[125], w[126], w[127]);
    key.k124 = x0;
    key.k125 = x1;
    key.k126 = x2;
    key.k127 = x3;
    sbox3(w[128], w[129], w[130], w[131]);
    key.k128 = x0;
    key.k129 = x1;
    key.k130 = x2;
    key.k131 = x3;
    return key;
  }

  public synchronized void encrypt(byte[] in, int i, byte[] out, int o,
                                   Object K, int bs)
  {
    Key key = (Key) K;
    x0 = (in[i     ] & 0xff)
       | (in[i +  1] & 0xff) << 8
       | (in[i +  2] & 0xff) << 16
       | (in[i +  3] & 0xff) << 24;
    x1 = (in[i +  4] & 0xff)
       | (in[i +  5] & 0xff) << 8
       | (in[i +  6] & 0xff) << 16
       | (in[i +  7] & 0xff) << 24;
    x2 = (in[i +  8] & 0xff)
       | (in[i +  9] & 0xff) << 8
       | (in[i + 10] & 0xff) << 16
       | (in[i + 11] & 0xff) << 24;
    x3 = (in[i + 12] & 0xff)
       | (in[i + 13] & 0xff) << 8
       | (in[i + 14] & 0xff) << 16
       | (in[i + 15] & 0xff) << 24;
    x0 ^= key.k0;
    x1 ^= key.k1;
    x2 ^= key.k2;
    x3 ^= key.k3;
    sbox0();
    x1 ^= key.k4;
    x4 ^= key.k5;
    x2 ^= key.k6;
    x0 ^= key.k7;
    sbox1();
    x0 ^= key.k8;
    x4 ^= key.k9;
    x2 ^= key.k10;
    x1 ^= key.k11;
    sbox2();
    x2 ^= key.k12;
    x1 ^= key.k13;
    x4 ^= key.k14;
    x3 ^= key.k15;
    sbox3();
    x1 ^= key.k16;
    x4 ^= key.k17;
    x3 ^= key.k18;
    x0 ^= key.k19;
    sbox4();
    x4 ^= key.k20;
    x2 ^= key.k21;
    x1 ^= key.k22;
    x0 ^= key.k23;
    sbox5();
    x2 ^= key.k24;
    x0 ^= key.k25;
    x4 ^= key.k26;
    x1 ^= key.k27;
    sbox6();
    x2 ^= key.k28;
    x0 ^= key.k29;
    x3 ^= key.k30;
    x4 ^= key.k31;
    sbox7();
    x0 = x3;
    x3 = x2;
    x2 = x4;
    x0 ^= key.k32;
    x1 ^= key.k33;
    x2 ^= key.k34;
    x3 ^= key.k35;
    sbox0();
    x1 ^= key.k36;
    x4 ^= key.k37;
    x2 ^= key.k38;
    x0 ^= key.k39;
    sbox1();
    x0 ^= key.k40;
    x4 ^= key.k41;
    x2 ^= key.k42;
    x1 ^= key.k43;
    sbox2();
    x2 ^= key.k44;
    x1 ^= key.k45;
    x4 ^= key.k46;
    x3 ^= key.k47;
    sbox3();
    x1 ^= key.k48;
    x4 ^= key.k49;
    x3 ^= key.k50;
    x0 ^= key.k51;
    sbox4();
    x4 ^= key.k52;
    x2 ^= key.k53;
    x1 ^= key.k54;
    x0 ^= key.k55;
    sbox5();
    x2 ^= key.k56;
    x0 ^= key.k57;
    x4 ^= key.k58;
    x1 ^= key.k59;
    sbox6();
    x2 ^= key.k60;
    x0 ^= key.k61;
    x3 ^= key.k62;
    x4 ^= key.k63;
    sbox7();
    x0 = x3;
    x3 = x2;
    x2 = x4;
    x0 ^= key.k64;
    x1 ^= key.k65;
    x2 ^= key.k66;
    x3 ^= key.k67;
    sbox0();
    x1 ^= key.k68;
    x4 ^= key.k69;
    x2 ^= key.k70;
    x0 ^= key.k71;
    sbox1();
    x0 ^= key.k72;
    x4 ^= key.k73;
    x2 ^= key.k74;
    x1 ^= key.k75;
    sbox2();
    x2 ^= key.k76;
    x1 ^= key.k77;
    x4 ^= key.k78;
    x3 ^= key.k79;
    sbox3();
    x1 ^= key.k80;
    x4 ^= key.k81;
    x3 ^= key.k82;
    x0 ^= key.k83;
    sbox4();
    x4 ^= key.k84;
    x2 ^= key.k85;
    x1 ^= key.k86;
    x0 ^= key.k87;
    sbox5();
    x2 ^= key.k88;
    x0 ^= key.k89;
    x4 ^= key.k90;
    x1 ^= key.k91;
    sbox6();
    x2 ^= key.k92;
    x0 ^= key.k93;
    x3 ^= key.k94;
    x4 ^= key.k95;
    sbox7();
    x0 = x3;
    x3 = x2;
    x2 = x4;
    x0 ^= key.k96;
    x1 ^= key.k97;
    x2 ^= key.k98;
    x3 ^= key.k99;
    sbox0();
    x1 ^= key.k100;
    x4 ^= key.k101;
    x2 ^= key.k102;
    x0 ^= key.k103;
    sbox1();
    x0 ^= key.k104;
    x4 ^= key.k105;
    x2 ^= key.k106;
    x1 ^= key.k107;
    sbox2();
    x2 ^= key.k108;
    x1 ^= key.k109;
    x4 ^= key.k110;
    x3 ^= key.k111;
    sbox3();
    x1 ^= key.k112;
    x4 ^= key.k113;
    x3 ^= key.k114;
    x0 ^= key.k115;
    sbox4();
    x4 ^= key.k116;
    x2 ^= key.k117;
    x1 ^= key.k118;
    x0 ^= key.k119;
    sbox5();
    x2 ^= key.k120;
    x0 ^= key.k121;
    x4 ^= key.k122;
    x1 ^= key.k123;
    sbox6();
    x2 ^= key.k124;
    x0 ^= key.k125;
    x3 ^= key.k126;
    x4 ^= key.k127;
    sbox7noLT();
    x0 = x3;
    x3 = x2;
    x2 = x4;
    x0 ^= key.k128;
    x1 ^= key.k129;
    x2 ^= key.k130;
    x3 ^= key.k131;
    out[o     ] = (byte) x0;
    out[o +  1] = (byte)(x0 >>> 8);
    out[o +  2] = (byte)(x0 >>> 16);
    out[o +  3] = (byte)(x0 >>> 24);
    out[o +  4] = (byte) x1;
    out[o +  5] = (byte)(x1 >>> 8);
    out[o +  6] = (byte)(x1 >>> 16);
    out[o +  7] = (byte)(x1 >>> 24);
    out[o +  8] = (byte) x2;
    out[o +  9] = (byte)(x2 >>> 8);
    out[o + 10] = (byte)(x2 >>> 16);
    out[o + 11] = (byte)(x2 >>> 24);
    out[o + 12] = (byte) x3;
    out[o + 13] = (byte)(x3 >>> 8);
    out[o + 14] = (byte)(x3 >>> 16);
    out[o + 15] = (byte)(x3 >>> 24);
  }

  public synchronized void decrypt(byte[] in, int i, byte[] out, int o,
                                   Object K, int bs)
  {
    Key key = (Key) K;
    x0 = (in[i     ] & 0xff)
       | (in[i +  1] & 0xff) << 8
       | (in[i +  2] & 0xff) << 16
       | (in[i +  3] & 0xff) << 24;
    x1 = (in[i +  4] & 0xff)
       | (in[i +  5] & 0xff) << 8
       | (in[i +  6] & 0xff) << 16
       | (in[i +  7] & 0xff) << 24;
    x2 = (in[i +  8] & 0xff)
       | (in[i +  9] & 0xff) << 8
       | (in[i + 10] & 0xff) << 16
       | (in[i + 11] & 0xff) << 24;
    x3 = (in[i + 12] & 0xff)
       | (in[i + 13] & 0xff) << 8
       | (in[i + 14] & 0xff) << 16
       | (in[i + 15] & 0xff) << 24;
    x0 ^= key.k128;
    x1 ^= key.k129;
    x2 ^= key.k130;
    x3 ^= key.k131;
    sboxI7noLT();
    x3 ^= key.k124;
    x0 ^= key.k125;
    x1 ^= key.k126;
    x4 ^= key.k127;
    sboxI6();
    x0 ^= key.k120;
    x1 ^= key.k121;
    x2 ^= key.k122;
    x4 ^= key.k123;
    sboxI5();
    x1 ^= key.k116;
    x3 ^= key.k117;
    x4 ^= key.k118;
    x2 ^= key.k119;
    sboxI4();
    x1 ^= key.k112;
    x2 ^= key.k113;
    x4 ^= key.k114;
    x0 ^= key.k115;
    sboxI3();
    x0 ^= key.k108;
    x1 ^= key.k109;
    x4 ^= key.k110;
    x2 ^= key.k111;
    sboxI2();
    x1 ^= key.k104;
    x3 ^= key.k105;
    x4 ^= key.k106;
    x2 ^= key.k107;
    sboxI1();
    x0 ^= key.k100;
    x1 ^= key.k101;
    x2 ^= key.k102;
    x4 ^= key.k103;
    sboxI0();
    x0 ^= key.k96;
    x3 ^= key.k97;
    x1 ^= key.k98;
    x4 ^= key.k99;
    sboxI7();
    x1 = x3;
    x3 = x4;
    x4 = x2;
    x3 ^= key.k92;
    x0 ^= key.k93;
    x1 ^= key.k94;
    x4 ^= key.k95;
    sboxI6();
    x0 ^= key.k88;
    x1 ^= key.k89;
    x2 ^= key.k90;
    x4 ^= key.k91;
    sboxI5();
    x1 ^= key.k84;
    x3 ^= key.k85;
    x4 ^= key.k86;
    x2 ^= key.k87;
    sboxI4();
    x1 ^= key.k80;
    x2 ^= key.k81;
    x4 ^= key.k82;
    x0 ^= key.k83;
    sboxI3();
    x0 ^= key.k76;
    x1 ^= key.k77;
    x4 ^= key.k78;
    x2 ^= key.k79;
    sboxI2();
    x1 ^= key.k72;
    x3 ^= key.k73;
    x4 ^= key.k74;
    x2 ^= key.k75;
    sboxI1();
    x0 ^= key.k68;
    x1 ^= key.k69;
    x2 ^= key.k70;
    x4 ^= key.k71;
    sboxI0();
    x0 ^= key.k64;
    x3 ^= key.k65;
    x1 ^= key.k66;
    x4 ^= key.k67;
    sboxI7();
    x1 = x3;
    x3 = x4;
    x4 = x2;
    x3 ^= key.k60;
    x0 ^= key.k61;
    x1 ^= key.k62;
    x4 ^= key.k63;
    sboxI6();
    x0 ^= key.k56;
    x1 ^= key.k57;
    x2 ^= key.k58;
    x4 ^= key.k59;
    sboxI5();
    x1 ^= key.k52;
    x3 ^= key.k53;
    x4 ^= key.k54;
    x2 ^= key.k55;
    sboxI4();
    x1 ^= key.k48;
    x2 ^= key.k49;
    x4 ^= key.k50;
    x0 ^= key.k51;
    sboxI3();
    x0 ^= key.k44;
    x1 ^= key.k45;
    x4 ^= key.k46;
    x2 ^= key.k47;
    sboxI2();
    x1 ^= key.k40;
    x3 ^= key.k41;
    x4 ^= key.k42;
    x2 ^= key.k43;
    sboxI1();
    x0 ^= key.k36;
    x1 ^= key.k37;
    x2 ^= key.k38;
    x4 ^= key.k39;
    sboxI0();
    x0 ^= key.k32;
    x3 ^= key.k33;
    x1 ^= key.k34;
    x4 ^= key.k35;
    sboxI7();
    x1 = x3;
    x3 = x4;
    x4 = x2;
    x3 ^= key.k28;
    x0 ^= key.k29;
    x1 ^= key.k30;
    x4 ^= key.k31;
    sboxI6();
    x0 ^= key.k24;
    x1 ^= key.k25;
    x2 ^= key.k26;
    x4 ^= key.k27;
    sboxI5();
    x1 ^= key.k20;
    x3 ^= key.k21;
    x4 ^= key.k22;
    x2 ^= key.k23;
    sboxI4();
    x1 ^= key.k16;
    x2 ^= key.k17;
    x4 ^= key.k18;
    x0 ^= key.k19;
    sboxI3();
    x0 ^= key.k12;
    x1 ^= key.k13;
    x4 ^= key.k14;
    x2 ^= key.k15;
    sboxI2();
    x1 ^= key.k8;
    x3 ^= key.k9;
    x4 ^= key.k10;
    x2 ^= key.k11;
    sboxI1();
    x0 ^= key.k4;
    x1 ^= key.k5;
    x2 ^= key.k6;
    x4 ^= key.k7;
    sboxI0();
    x2 = x1;
    x1 = x3;
    x3 = x4;
    x0 ^= key.k0;
    x1 ^= key.k1;
    x2 ^= key.k2;
    x3 ^= key.k3;
    out[o     ] = (byte) x0;
    out[o +  1] = (byte)(x0 >>> 8);
    out[o +  2] = (byte)(x0 >>> 16);
    out[o +  3] = (byte)(x0 >>> 24);
    out[o +  4] = (byte) x1;
    out[o +  5] = (byte)(x1 >>> 8);
    out[o +  6] = (byte)(x1 >>> 16);
    out[o +  7] = (byte)(x1 >>> 24);
    out[o +  8] = (byte) x2;
    out[o +  9] = (byte)(x2 >>> 8);
    out[o + 10] = (byte)(x2 >>> 16);
    out[o + 11] = (byte)(x2 >>> 24);
    out[o + 12] = (byte) x3;
    out[o + 13] = (byte)(x3 >>> 8);
    out[o + 14] = (byte)(x3 >>> 16);
    out[o + 15] = (byte)(x3 >>> 24);
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

  // These first few S-boxes operate directly on the "registers",
  // x0..x4, and perform the linear transform.
  private void sbox0()
  {
    x3 ^= x0;
    x4 = x1;
    x1 &= x3;
    x4 ^= x2;
    x1 ^= x0;
    x0 |= x3;
    x0 ^= x4;
    x4 ^= x3;
    x3 ^= x2;
    x2 |= x1;
    x2 ^= x4;
    x4 ^= -1;
    x4 |= x1;
    x1 ^= x3;
    x1 ^= x4;
    x3 |= x0;
    x1 ^= x3;
    x4 ^= x3;

    x1 = (x1 << 13) | (x1 >>> 19);
    x4 ^= x1;
    x3 = x1 << 3;
    x2 = (x2 << 3) | (x2 >>> 29);
    x4 ^= x2;
    x0 ^= x2;
    x4 = (x4 << 1) | (x4 >>> 31);
    x0 ^= x3;
    x0 = (x0 << 7) | (x0 >>> 25);
    x3 = x4;
    x1 ^= x4;
    x3 <<= 7;
    x1 ^= x0;
    x2 ^= x0;
    x2 ^= x3;
    x1 = (x1 << 5) | (x1 >>> 27);
    x2 = (x2 << 22) | (x2 >>> 10);
  }

  private void sbox1()
  {
    x4 = ~x4;
    x3 = x1;
    x1 ^= x4;
    x3 |= x4;
    x3 ^= x0;
    x0 &= x1;
    x2 ^= x3;
    x0 ^= x4;
    x0 |= x2;
    x1 ^= x3;
    x0 ^= x1;
    x4 &= x2;
    x1 |= x4;
    x4 ^= x3;
    x1 ^= x2;
    x3 |= x0;
    x1 ^= x3;
    x3 = ~x3;
    x4 ^= x0;
    x3 &= x2;
    x4 = ~x4;
    x3 ^= x1;
    x4 ^= x3;

    x0 = (x0 << 13) | (x0 >>> 19);
    x4 ^= x0;
    x3 = x0 << 3;
    x2 = (x2 << 3) | (x2 >>> 29);
    x4 ^= x2;
    x1 ^= x2;
    x4 = (x4 << 1) | (x4 >>> 31);
    x1 ^= x3;
    x1 = (x1 << 7) | (x1 >>> 25);
    x3 = x4;
    x0 ^= x4;
    x3 <<= 7;
    x0 ^= x1;
    x2 ^= x1;
    x2 ^= x3;
    x0 = (x0 << 5) | (x0 >>> 27);
    x2 = (x2 << 22) | (x2 >>> 10);
  }

  private void sbox2()
  {
    x3 = x0;
    x0 = x0 & x2;
    x0 = x0 ^ x1;
    x2 = x2 ^ x4;
    x2 = x2 ^ x0;
    x1 = x1 | x3;
    x1 = x1 ^ x4;
    x3 = x3 ^ x2;
    x4 = x1;
    x1 = x1 | x3;
    x1 = x1 ^ x0;
    x0 = x0 & x4;
    x3 = x3 ^ x0;
    x4 = x4 ^ x1;
    x4 = x4 ^ x3;
    x3 = ~x3;

    x2 = (x2 << 13) | (x2 >>> 19);
    x1 ^= x2;
    x0 = x2 << 3;
    x4 = (x4 << 3) | (x4 >>> 29);
    x1 ^= x4;
    x3 ^= x4;
    x1 = (x1 << 1) | (x1 >>> 31);
    x3 ^= x0;
    x3 = (x3 << 7) | (x3 >>> 25);
    x0 = x1;
    x2 ^= x1;
    x0 <<= 7;
    x2 ^= x3;
    x4 ^= x3;
    x4 ^= x0;
    x2 = (x2 << 5) | (x2 >>> 27);
    x4 = (x4 << 22) | (x4 >>> 10);
  }

  private void sbox3()
  {
    x0 = x2;
    x2 = x2 | x3;
    x3 = x3 ^ x1;
    x1 = x1 & x0;
    x0 = x0 ^ x4;
    x4 = x4 ^ x3;
    x3 = x3 & x2;
    x0 = x0 | x1;
    x3 = x3 ^ x0;
    x2 = x2 ^ x1;
    x0 = x0 & x2;
    x1 = x1 ^ x3;
    x0 = x0 ^ x4;
    x1 = x1 | x2;
    x1 = x1 ^ x4;
    x2 = x2 ^ x3;
    x4 = x1;
    x1 = x1 | x3;
    x1 = x1 ^ x2;

    x1 = (x1 << 13) | (x1 >>> 19);
    x4 ^= x1;
    x2 = x1 << 3;
    x3 = (x3 << 3) | (x3 >>> 29);
    x4 ^= x3;
    x0 ^= x3;
    x4 = (x4 << 1) | (x4 >>> 31);
    x0 ^= x2;
    x0 = (x0 << 7) | (x0 >>> 25);
    x2 = x4;
    x1 ^= x4;
    x2 <<= 7;
    x1 ^= x0;
    x3 ^= x0;
    x3 ^= x2;
    x1 = (x1 << 5) | (x1 >>> 27);
    x3 = (x3 << 22) | (x3 >>> 10);
  }

  private void sbox4()
  {
    x4 = x4 ^ x0;
    x0 = ~x0;
    x3 = x3 ^ x0;
    x0 = x0 ^ x1;
    x2 = x4;
    x4 = x4 & x0;
    x4 = x4 ^ x3;
    x2 = x2 ^ x0;
    x1 = x1 ^ x2;
    x3 = x3 & x2;
    x3 = x3 ^ x1;
    x1 = x1 & x4;
    x0 = x0 ^ x1;
    x2 = x2 | x4;
    x2 = x2 ^ x1;
    x1 = x1 | x0;
    x1 = x1 ^ x3;
    x3 = x3 & x0;
    x1 = ~x1;
    x2 = x2 ^ x3;

    x4 = (x4 << 13) | (x4 >>> 19);
    x2 ^= x4;
    x3 = x4 << 3;
    x1 = (x1 << 3) | (x1 >>> 29);
    x2 ^= x1;
    x0 ^= x1;
    x2 = (x2 << 1) | (x2 >>> 31);
    x0 ^= x3;
    x0 = (x0 << 7) | (x0 >>> 25);
    x3 = x2;
    x4 ^= x2;
    x3 <<= 7;
    x4 ^= x0;
    x1 ^= x0;
    x1 ^= x3;
    x4 = (x4 << 5) | (x4 >>> 27);
    x1 = (x1 << 22) | (x1 >>> 10);
  }

  private void sbox5()
  {
    x4 = x4 ^ x2;
    x2 = x2 ^ x0;
    x0 = ~x0;
    x3 = x2;
    x2 = x2 & x4;
    x1 = x1 ^ x0;
    x2 = x2 ^ x1;
    x1 = x1 | x3;
    x3 = x3 ^ x0;
    x0 = x0 & x2;
    x0 = x0 ^ x4;
    x3 = x3 ^ x2;
    x3 = x3 ^ x1;
    x1 = x1 ^ x4;
    x4 = x4 & x0;
    x1 = ~x1;
    x4 = x4 ^ x3;
    x3 = x3 | x0;
    x1 = x1 ^ x3;

    x2 = (x2 << 13) | (x2 >>> 19);
    x0 ^= x2;
    x3 = x2 << 3;
    x4 = (x4 << 3) | (x4 >>> 29);
    x0 ^= x4;
    x1 ^= x4;
    x0 = (x0 << 1) | (x0 >>> 31);
    x1 ^= x3;
    x1 = (x1 << 7) | (x1 >>> 25);
    x3 = x0;
    x2 ^= x0;
    x3 <<= 7;
    x2 ^= x1;
    x4 ^= x1;
    x4 ^= x3;
    x2 = (x2 << 5) | (x2 >>> 27);
    x4 = (x4 << 22) | (x4 >>> 10);
  }

  private void sbox6()
  {
    x4 = ~x4;
    x3 = x1;
    x1 = x1 & x2;
    x2 = x2 ^ x3;
    x1 = x1 ^ x4;
    x4 = x4 | x3;
    x0 = x0 ^ x1;
    x4 = x4 ^ x2;
    x2 = x2 | x0;
    x4 = x4 ^ x0;
    x3 = x3 ^ x2;
    x2 = x2 | x1;
    x2 = x2 ^ x4;
    x3 = x3 ^ x1;
    x3 = x3 ^ x2;
    x1 = ~x1;
    x4 = x4 & x3;
    x4 = x4 ^ x1;
    x2 = (x2 << 13) | (x2 >>> 19);
    x0 ^= x2;
    x1 = x2 << 3;
    x3 = (x3 << 3) | (x3 >>> 29);
    x0 ^= x3;
    x4 ^= x3;
    x0 = (x0 << 1) | (x0 >>> 31);
    x4 ^= x1;
    x4 = (x4 << 7) | (x4 >>> 25);
    x1 = x0;
    x2 ^= x0;
    x1 <<= 7;
    x2 ^= x4;
    x3 ^= x4;
    x3 ^= x1;
    x2 = (x2 << 5) | (x2 >>> 27);
    x3 = (x3 << 22) | (x3 >>> 10);
  }

  private void sbox7()
  {
    x1 = x3;
    x3 = x3 & x0;
    x3 = x3 ^ x4;
    x4 = x4 & x0;
    x1 = x1 ^ x3;
    x3 = x3 ^ x0;
    x0 = x0 ^ x2;
    x2 = x2 | x1;
    x2 = x2 ^ x3;
    x4 = x4 ^ x0;
    x3 = x3 ^ x4;
    x4 = x4 & x2;
    x4 = x4 ^ x1;
    x1 = x1 ^ x3;
    x3 = x3 & x2;
    x1 = ~x1;
    x3 = x3 ^ x1;
    x1 = x1 & x2;
    x0 = x0 ^ x4;
    x1 = x1 ^ x0;
    x3 = (x3 << 13) | (x3 >>> 19);
    x1 ^= x3;
    x0 = x3 << 3;
    x4 = (x4 << 3) | (x4 >>> 29);
    x1 ^= x4;
    x2 ^= x4;
    x1 = (x1 << 1) | (x1 >>> 31);
    x2 ^= x0;
    x2 = (x2 << 7) | (x2 >>> 25);
    x0 = x1;
    x3 ^= x1;
    x0 <<= 7;
    x3 ^= x2;
    x4 ^= x2;
    x4 ^= x0;
    x3 = (x3 << 5) | (x3 >>> 27);
    x4 = (x4 << 22) | (x4 >>> 10);
  }

  /** The final S-box, with no transform. */
  private void sbox7noLT()
  {
    x1 = x3;
    x3 = x3 & x0;
    x3 = x3 ^ x4;
    x4 = x4 & x0;
    x1 = x1 ^ x3;
    x3 = x3 ^ x0;
    x0 = x0 ^ x2;
    x2 = x2 | x1;
    x2 = x2 ^ x3;
    x4 = x4 ^ x0;
    x3 = x3 ^ x4;
    x4 = x4 & x2;
    x4 = x4 ^ x1;
    x1 = x1 ^ x3;
    x3 = x3 & x2;
    x1 = ~x1;
    x3 = x3 ^ x1;
    x1 = x1 & x2;
    x0 = x0 ^ x4;
    x1 = x1 ^ x0;
  }

  private void sboxI7noLT()
  {
    x4 = x2;
    x2 ^= x0;
    x0 &= x3;
    x2 = ~x2;
    x4 |= x3;
    x3 ^= x1;
    x1 |= x0;
    x0 ^= x2;
    x2 &= x4;
    x1 ^= x2;
    x2 ^= x0;
    x0 |= x2;
    x3 &= x4;
    x0 ^= x3;
    x4 ^= x1;
    x3 ^= x4;
    x4 |= x0;
    x3 ^= x2;
    x4 ^= x2;
  }

  private void sboxI6()
  {
    x1 = (x1 >>> 22) | (x1 << 10);
    x3 = (x3 >>> 5) | (x3 << 27);
    x2 = x0;
    x1 ^= x4;
    x2 <<= 7;
    x3 ^= x4;
    x1 ^= x2;
    x3 ^= x0;
    x4 = (x4 >>> 7) | (x4 << 25);
    x0 = (x0 >>> 1) | (x0 << 31);
    x0 ^= x3;
    x2 = x3 << 3;
    x4 ^= x2;
    x3 = (x3 >>> 13) | (x3 << 19);
    x0 ^= x1;
    x4 ^= x1;
    x1 = (x1 >>> 3) | (x1 << 29);
    x3 ^= x1;
    x2 = x1;
    x1 &= x3;
    x2 ^= x4;
    x1 = ~x1;
    x4 ^= x0;
    x1 ^= x4;
    x2 |= x3;
    x3 ^= x1;
    x4 ^= x2;
    x2 ^= x0;
    x0 &= x4;
    x0 ^= x3;
    x3 ^= x4;
    x3 |= x1;
    x4 ^= x0;
    x2 ^= x3;
  }

  private void sboxI5()
  {
    x2 = (x2 >>> 22) | (x2 << 10);
    x0 = (x0 >>> 5) | (x0 << 27);
    x3 = x1;
    x2 ^= x4;
    x3 <<= 7;
    x0 ^= x4;
    x2 ^= x3;
    x0 ^= x1;
    x4 = (x4 >>> 7) | (x4 << 25);
    x1 = (x1 >>> 1) | (x1 << 31);
    x1 ^= x0;
    x3 = x0 << 3;
    x4 ^= x3;
    x0 = (x0 >>> 13) | (x0 << 19);
    x1 ^= x2;
    x4 ^= x2;
    x2 = (x2 >>> 3) | (x2 << 29);
    x1 = ~x1;
    x3 = x4;
    x2 ^= x1;
    x4 |= x0;
    x4 ^= x2;
    x2 |= x1;
    x2 &= x0;
    x3 ^= x4;
    x2 ^= x3;
    x3 |= x0;
    x3 ^= x1;
    x1 &= x2;
    x1 ^= x4;
    x3 ^= x2;
    x4 &= x3;
    x3 ^= x1;
    x4 ^= x0;
    x4 ^= x3;
    x3 = ~x3;
  }

  private void sboxI4()
  {
    x4 = (x4 >>> 22) | (x4 << 10);
    x1 = (x1 >>> 5) | (x1 << 27);
    x0 = x3;
    x4 ^= x2;
    x0 <<= 7;
    x1 ^= x2;
    x4 ^= x0;
    x1 ^= x3;
    x2 = (x2 >>> 7) | (x2 << 25);
    x3 = (x3 >>> 1) | (x3 << 31);
    x3 ^= x1;
    x0 = x1 << 3;
    x2 ^= x0;
    x1 = (x1 >>> 13) | (x1 << 19);
    x3 ^= x4;
    x2 ^= x4;
    x4 = (x4 >>> 3) | (x4 << 29);
    x0 = x4;
    x4 &= x2;
    x4 ^= x3;
    x3 |= x2;
    x3 &= x1;
    x0 ^= x4;
    x0 ^= x3;
    x3 &= x4;
    x1 = ~x1;
    x2 ^= x0;
    x3 ^= x2;
    x2 &= x1;
    x2 ^= x4;
    x1 ^= x3;
    x4 &= x1;
    x2 ^= x1;
    x4 ^= x0;
    x4 |= x2;
    x2 ^= x1;
    x4 ^= x3;
  }

  private void sboxI3()
  {
    x4 = (x4 >>> 22) | (x4 << 10);
    x1 = (x1 >>> 5) | (x1 << 27);
    x3 = x2;
    x4 ^= x0;
    x3 <<= 7;
    x1 ^= x0;
    x4 ^= x3;
    x1 ^= x2;
    x0 = (x0 >>> 7) | (x0 << 25);
    x2 = (x2 >>> 1) | (x2 << 31);
    x2 ^= x1;
    x3 = x1 << 3;
    x0 ^= x3;
    x1 = (x1 >>> 13) | (x1 << 19);
    x2 ^= x4;
    x0 ^= x4;
    x4 = (x4 >>> 3) | (x4 << 29);
    x3 = x4;
    x4 ^= x2;
    x2 &= x4;
    x2 ^= x1;
    x1 &= x3;
    x3 ^= x0;
    x0 |= x2;
    x0 ^= x4;
    x1 ^= x3;
    x4 ^= x1;
    x1 |= x0;
    x1 ^= x2;
    x3 ^= x4;
    x4 &= x0;
    x2 |= x0;
    x2 ^= x4;
    x3 ^= x1;
    x4 ^= x3;
  }

  private void sboxI2()
  {
    x4 = (x4 >>> 22) | (x4 << 10);
    x0 = (x0 >>> 5) | (x0 << 27);
    x3 = x1;
    x4 ^= x2;
    x3 <<= 7;
    x0 ^= x2;
    x4 ^= x3;
    x0 ^= x1;
    x2 = (x2 >>> 7) | (x2 << 25);
    x1 = (x1 >>> 1) | (x1 << 31);
    x1 ^= x0;
    x3 = x0 << 3;
    x2 ^= x3;
    x0 = (x0 >>> 13) | (x0 << 19);
    x1 ^= x4;
    x2 ^= x4;
    x4 = (x4 >>> 3) | (x4 << 29);
    x4 ^= x2;
    x2 ^= x0;
    x3 = x2;
    x2 &= x4;
    x2 ^= x1;
    x1 |= x4;
    x1 ^= x3;
    x3 &= x2;
    x4 ^= x2;
    x3 &= x0;
    x3 ^= x4;
    x4 &= x1;
    x4 |= x0;
    x2 = ~x2;
    x4 ^= x2;
    x0 ^= x2;
    x0 &= x1;
    x2 ^= x3;
    x2 ^= x0;
  }

  private void sboxI1()
  {
    x4 = (x4 >>> 22) | (x4 << 10);
    x1 = (x1 >>> 5) | (x1 << 27);
    x0 = x3;
    x4 ^= x2;
    x0 <<= 7;
    x1 ^= x2;
    x4 ^= x0;
    x1 ^= x3;
    x2 = (x2 >>> 7) | (x2 << 25);
    x3 = (x3 >>> 1) | (x3 << 31);
    x3 ^= x1;
    x0 = x1 << 3;
    x2 ^= x0;
    x1 = (x1 >>> 13) | (x1 << 19);
    x3 ^= x4;
    x2 ^= x4;
    x4 = (x4 >>> 3) | (x4 << 29);
    x0 = x3;
    x3 ^= x2;
    x2 &= x3;
    x0 ^= x4;
    x2 ^= x1;
    x1 |= x3;
    x4 ^= x2;
    x1 ^= x0;
    x1 |= x4;
    x3 ^= x2;
    x1 ^= x3;
    x3 |= x2;
    x3 ^= x1;
    x0 = ~x0;
    x0 ^= x3;
    x3 |= x1;
    x3 ^= x1;
    x3 |= x0;
    x2 ^= x3;
  }

  private void sboxI0()
  {
    x2 = (x2 >>> 22) | (x2 << 10);
    x0 = (x0 >>> 5) | (x0 << 27);
    x3 = x1;
    x2 ^= x4;
    x3 <<= 7;
    x0 ^= x4;
    x2 ^= x3;
    x0 ^= x1;
    x4 = (x4 >>> 7) | (x4 << 25);
    x1 = (x1 >>> 1) | (x1 << 31);
    x1 ^= x0;
    x3 = x0 << 3;
    x4 ^= x3;
    x0 = (x0 >>> 13) | (x0 << 19);
    x1 ^= x2;
    x4 ^= x2;
    x2 = (x2 >>> 3) | (x2 << 29);
    x2 = ~x2;
    x3 = x1;
    x1 |= x0;
    x3 = ~x3;
    x1 ^= x2;
    x2 |= x3;
    x1 ^= x4;
    x0 ^= x3;
    x2 ^= x0;
    x0 &= x4;
    x3 ^= x0;
    x0 |= x1;
    x0 ^= x2;
    x4 ^= x3;
    x2 ^= x1;
    x4 ^= x0;
    x4 ^= x1;
    x2 &= x4;
    x3 ^= x2;
  }

  private void sboxI7()
  {
    x1 = (x1 >>> 22) | (x1 << 10);
    x0 = (x0 >>> 5) | (x0 << 27);
    x2 = x3;
    x1 ^= x4;
    x2 <<= 7;
    x0 ^= x4;
    x1 ^= x2;
    x0 ^= x3;
    x4 = (x4 >>> 7) | (x4 << 25);
    x3 = (x3 >>> 1) | (x3 << 31);
    x3 ^= x0;
    x2 = x0 << 3;
    x4 ^= x2;
    x0 = (x0 >>> 13) | (x0 << 19);
    x3 ^= x1;
    x4 ^= x1;
    x1 = (x1 >>> 3) | (x1 << 29);
    x2 = x1;
    x1 ^= x0;
    x0 &= x4;
    x1 = ~x1;
    x2 |= x4;
    x4 ^= x3;
    x3 |= x0;
    x0 ^= x1;
    x1 &= x2;
    x3 ^= x1;
    x1 ^= x0;
    x0 |= x1;
    x4 &= x2;
    x0 ^= x4;
    x2 ^= x3;
    x4 ^= x2;
    x2 |= x0;
    x4 ^= x1;
    x2 ^= x1;
  }

  /** S-Box 0. */
  private void sbox0(int r0, int r1, int r2, int r3)
  {
    int r4 = r1 ^ r2;
    r3 ^= r0;
    r1 = r1 & r3 ^ r0;
    r0 = (r0 | r3) ^ r4;
    r4 ^= r3;
    r3 ^= r2;
    r2 = (r2 | r1) ^ r4;
    r4 = ~r4 | r1;
    r1 ^= r3 ^ r4;
    r3 |= r0;
    x0 = r1 ^ r3;
    x1 = r4 ^ r3;
    x2 = r2;
    x3 = r0;
  }

  /** S-Box 1. */
  private void sbox1(int r0, int r1, int r2, int r3)
  {
    r0 = ~r0;
    int r4 = r0;
    r2 = ~r2;
    r0 &= r1;
    r2 ^= r0;
    r0 |= r3;
    r3 ^= r2;
    r1 ^= r0;
    r0 ^= r4;
    r4 |= r1;
    r1 ^= r3;
    r2 = (r2 | r0) & r4;
    r0 ^= r1;
    x0 = r2;
    x1 = r0 & r2 ^ r4;
    x2 = r3;
    x3 = r1 & r2 ^ r0;
  }

  /** S-Box 2. */
  private void sbox2(int r0, int r1, int r2, int r3)
  {
    int r4 = r0;
    r0 = r0 & r2 ^ r3;
    r2 = r2 ^ r1 ^ r0;
    r3 = (r3 | r4) ^ r1;
    r4 ^= r2;
    r1 = r3;
    r3 = (r3 | r4) ^ r0;
    r0 &= r1;
    r4 ^= r0;
    x0 = r2;
    x1 = r3;
    x2 = r1 ^ r3 ^ r4;
    x3 = ~r4;
  }

  /** S-Box 3. */
  private void sbox3(int r0, int r1, int r2, int r3)
  {
    int r4 = r0;
    r0 |= r3;
    r3 ^= r1;
    r1 &= r4;
    r4 = r4 ^ r2 | r1;
    r2 ^= r3;
    r3 = r3 & r0 ^ r4;
    r0 ^= r1;
    r4 = r4 & r0 ^ r2;
    r1 = (r1 ^ r3 | r0) ^ r2;
    r0 ^= r3;
    x0 = (r1 | r3) ^ r0;
    x1 = r1;
    x2 = r3;
    x3 = r4;
  }

  /** S-Box 4. */
  private void sbox4(int r0, int r1, int r2, int r3)
  {
    r1 ^= r3;
    int r4 = r1;
    r3 = ~r3;
    r2 ^= r3;
    r3 ^= r0;
    r1 = r1 & r3 ^ r2;
    r4 ^= r3;
    r0 ^= r4;
    r2 = r2 & r4 ^ r0;
    r0 &= r1;
    r3 ^= r0;
    r4 = (r4 | r1) ^ r0;
    x0 = r1;
    x1 = r4 ^ (r2 & r3);
    x2 = ~((r0 | r3) ^ r2);
    x3 = r3;
  }

  /** S-Box 5. */
  private void sbox5(int r0, int r1, int r2, int r3)
  {
    r0 ^= r1;
    r1 ^= r3;
    int r4 = r1;
    r3 = ~r3;
    r1 &= r0;
    r2 ^= r3;
    r1 ^= r2;
    r2 |= r4;
    r4 ^= r3;
    r3 = r3 & r1 ^ r0;
    r4 = r4 ^ r1 ^ r2;
    x0 = r1;
    x1 = r3;
    x2 = r0 & r3 ^ r4;
    x3 = ~(r2 ^ r0) ^ (r4 | r3);
  }

  /** S-Box 6. */
  private void sbox6(int r0, int r1, int r2, int r3)
  {
    int r4 = r3;
    r2 = ~r2;
    r3 = r3 & r0 ^ r2;
    r0 ^= r4;
    r2 = (r2 | r4) ^ r0;
    r1 ^= r3;
    r0 |= r1;
    r2 ^= r1;
    r4 ^= r0;
    r0 = (r0 | r3) ^ r2;
    r4 = r4 ^ r3 ^ r0;
    x0 = r0;
    x1 = r1;
    x2 = r4;
    x3 = r2 & r4 ^ ~r3;
  }

  /** S-Box 7. */
  private void sbox7(int r0, int r1, int r2, int r3)
  {
    int r4 = r1;
    r1 = (r1 | r2) ^ r3;
    r4 ^= r2;
    r2 ^= r1;
    r3 = (r3 | r4) & r0;
    r4 ^= r2;
    r3 ^= r1;
    r1 = (r1 | r4) ^ r0;
    r0 = (r0 | r4) ^ r2;
    r1 ^= r4;
    r2 ^= r1;
    x0 = r4 ^ (~r2 | r0);
    x1 = r3;
    x2 = r1 & r0 ^ r4;
    x3 = r0;
  }

  private class Key
      implements Cloneable
  {
    int k0, k1, k2, k3, k4, k5, k6, k7, k8, k9, k10, k11, k12, k13, k14, k15,
        k16, k17, k18, k19, k20, k21, k22, k23, k24, k25, k26, k27, k28, k29,
        k30, k31, k32, k33, k34, k35, k36, k37, k38, k39, k40, k41, k42, k43,
        k44, k45, k46, k47, k48, k49, k50, k51, k52, k53, k54, k55, k56, k57,
        k58, k59, k60, k61, k62, k63, k64, k65, k66, k67, k68, k69, k70, k71,
        k72, k73, k74, k75, k76, k77, k78, k79, k80, k81, k82, k83, k84, k85,
        k86, k87, k88, k89, k90, k91, k92, k93, k94, k95, k96, k97, k98, k99,
        k100, k101, k102, k103, k104, k105, k106, k107, k108, k109, k110, k111,
        k112, k113, k114, k115, k116, k117, k118, k119, k120, k121, k122, k123,
        k124, k125, k126, k127, k128, k129, k130, k131;

    /** Trivial 0-arguments constructor. */
    Key()
    {
    }

    /** Cloning constructor. */
    private Key(Key that)
    {
      this.k0 = that.k0;
      this.k1 = that.k1;
      this.k2 = that.k2;
      this.k3 = that.k3;
      this.k4 = that.k4;
      this.k5 = that.k5;
      this.k6 = that.k6;
      this.k7 = that.k7;
      this.k8 = that.k8;
      this.k9 = that.k9;
      this.k10 = that.k10;
      this.k11 = that.k11;
      this.k12 = that.k12;
      this.k13 = that.k13;
      this.k14 = that.k14;
      this.k15 = that.k15;
      this.k16 = that.k16;
      this.k17 = that.k17;
      this.k18 = that.k18;
      this.k19 = that.k19;
      this.k20 = that.k20;
      this.k21 = that.k21;
      this.k22 = that.k22;
      this.k23 = that.k23;
      this.k24 = that.k24;
      this.k25 = that.k25;
      this.k26 = that.k26;
      this.k27 = that.k27;
      this.k28 = that.k28;
      this.k29 = that.k29;
      this.k30 = that.k30;
      this.k31 = that.k31;
      this.k32 = that.k32;
      this.k33 = that.k33;
      this.k34 = that.k34;
      this.k35 = that.k35;
      this.k36 = that.k36;
      this.k37 = that.k37;
      this.k38 = that.k38;
      this.k39 = that.k39;
      this.k40 = that.k40;
      this.k41 = that.k41;
      this.k42 = that.k42;
      this.k43 = that.k43;
      this.k44 = that.k44;
      this.k45 = that.k45;
      this.k46 = that.k46;
      this.k47 = that.k47;
      this.k48 = that.k48;
      this.k49 = that.k49;
      this.k50 = that.k50;
      this.k51 = that.k51;
      this.k52 = that.k52;
      this.k53 = that.k53;
      this.k54 = that.k54;
      this.k55 = that.k55;
      this.k56 = that.k56;
      this.k57 = that.k57;
      this.k58 = that.k58;
      this.k59 = that.k59;
      this.k60 = that.k60;
      this.k61 = that.k61;
      this.k62 = that.k62;
      this.k63 = that.k63;
      this.k64 = that.k64;
      this.k65 = that.k65;
      this.k66 = that.k66;
      this.k67 = that.k67;
      this.k68 = that.k68;
      this.k69 = that.k69;
      this.k70 = that.k70;
      this.k71 = that.k71;
      this.k72 = that.k72;
      this.k73 = that.k73;
      this.k74 = that.k74;
      this.k75 = that.k75;
      this.k76 = that.k76;
      this.k77 = that.k77;
      this.k78 = that.k78;
      this.k79 = that.k79;
      this.k80 = that.k80;
      this.k81 = that.k81;
      this.k82 = that.k82;
      this.k83 = that.k83;
      this.k84 = that.k84;
      this.k85 = that.k85;
      this.k86 = that.k86;
      this.k87 = that.k87;
      this.k88 = that.k88;
      this.k89 = that.k89;
      this.k90 = that.k90;
      this.k91 = that.k91;
      this.k92 = that.k92;
      this.k93 = that.k93;
      this.k94 = that.k94;
      this.k95 = that.k95;
      this.k96 = that.k96;
      this.k97 = that.k97;
      this.k98 = that.k98;
      this.k99 = that.k99;
      this.k100 = that.k100;
      this.k101 = that.k101;
      this.k102 = that.k102;
      this.k103 = that.k103;
      this.k104 = that.k104;
      this.k105 = that.k105;
      this.k106 = that.k106;
      this.k107 = that.k107;
      this.k108 = that.k108;
      this.k109 = that.k109;
      this.k110 = that.k110;
      this.k111 = that.k111;
      this.k112 = that.k112;
      this.k113 = that.k113;
      this.k114 = that.k114;
      this.k115 = that.k115;
      this.k116 = that.k116;
      this.k117 = that.k117;
      this.k118 = that.k118;
      this.k119 = that.k119;
      this.k120 = that.k120;
      this.k121 = that.k121;
      this.k122 = that.k122;
      this.k123 = that.k123;
      this.k124 = that.k124;
      this.k125 = that.k125;
      this.k126 = that.k126;
      this.k127 = that.k127;
      this.k128 = that.k128;
      this.k129 = that.k129;
      this.k130 = that.k130;
      this.k131 = that.k131;
    }

    public Object clone()
    {
      return new Key(this);
    }
  }
}
