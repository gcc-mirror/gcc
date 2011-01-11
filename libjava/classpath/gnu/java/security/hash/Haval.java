/* Haval.java --
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
 * The <i>HAVAL</i> message-digest algorithm is a variable output length, with
 * variable number of rounds. By default, this implementation allows <i>HAVAL</i>
 * to be used as a drop-in replacement for <i>MD5</i>.
 * <p>
 * References:
 * <ol>
 * <li>HAVAL - A One-Way Hashing Algorithm with Variable Length of Output<br>
 * Advances in Cryptology - AUSCRYPT'92, Lecture Notes in Computer Science,<br>
 * Springer-Verlag, 1993; <br>
 * Y. Zheng, J. Pieprzyk and J. Seberry.</li>
 * </ol>
 */
public class Haval
    extends BaseHash
{
  public static final int HAVAL_VERSION = 1;

  public static final int HAVAL_128_BIT = 16;

  public static final int HAVAL_160_BIT = 20;

  public static final int HAVAL_192_BIT = 24;

  public static final int HAVAL_224_BIT = 28;

  public static final int HAVAL_256_BIT = 32;

  public static final int HAVAL_3_ROUND = 3;

  public static final int HAVAL_4_ROUND = 4;

  public static final int HAVAL_5_ROUND = 5;

  private static final int BLOCK_SIZE = 128; // inner block size in bytes

  private static final String DIGEST0 = "C68F39913F901F3DDF44C707357A7D70";

  /** caches the result of the correctness test, once executed. */
  private static Boolean valid;

  /**
   * Number of HAVAL rounds. Allowed values are integers in the range <code>3
   * .. 5</code>.
   * The default is <code>3</code>.
   */
  private int rounds = HAVAL_3_ROUND;

  /** 128-bit interim result. */
  private int h0, h1, h2, h3, h4, h5, h6, h7;

  /**
   * Calls the constructor with two argument using {@link #HAVAL_128_BIT} as the
   * value for the output size (i.e. <code>128</code> bits, and
   * {@link #HAVAL_3_ROUND} for the value of number of rounds.
   */
  public Haval()
  {
    this(HAVAL_128_BIT, HAVAL_3_ROUND);
  }

  /**
   * Calls the constructor with two arguments using the designated output size,
   * and {@link #HAVAL_3_ROUND} for the value of number of rounds.
   *
   * @param size the output size in bytes of this instance.
   * @throws IllegalArgumentException if the designated output size is invalid.
   * @see #HAVAL_128_BIT
   * @see #HAVAL_160_BIT
   * @see #HAVAL_192_BIT
   * @see #HAVAL_224_BIT
   * @see #HAVAL_256_BIT
   */
  public Haval(int size)
  {
    this(size, HAVAL_3_ROUND);
  }

  /**
   * Constructs a <code>Haval</code> instance with the designated output size
   * (in bytes). Valid output <code>size</code> values are <code>16</code>,
   * <code>20</code>, <code>24</code>, <code>28</code> and
   * <code>32</code>. Valid values for <code>rounds</code> are in the range
   * <code>3..5</code> inclusive.
   *
   * @param size the output size in bytes of this instance.
   * @param rounds the number of rounds to apply when transforming data.
   * @throws IllegalArgumentException if the designated output size is invalid,
   *           or if the number of rounds is invalid.
   * @see #HAVAL_128_BIT
   * @see #HAVAL_160_BIT
   * @see #HAVAL_192_BIT
   * @see #HAVAL_224_BIT
   * @see #HAVAL_256_BIT
   * @see #HAVAL_3_ROUND
   * @see #HAVAL_4_ROUND
   * @see #HAVAL_5_ROUND
   */
  public Haval(int size, int rounds)
  {
    super(Registry.HAVAL_HASH, size, BLOCK_SIZE);

    if (size != HAVAL_128_BIT
        && size != HAVAL_160_BIT
        && size != HAVAL_192_BIT
        && size != HAVAL_224_BIT
        && size != HAVAL_256_BIT)
      throw new IllegalArgumentException("Invalid HAVAL output size");

    if (rounds != HAVAL_3_ROUND
        && rounds != HAVAL_4_ROUND
        && rounds != HAVAL_5_ROUND)
      throw new IllegalArgumentException("Invalid HAVAL number of rounds");

    this.rounds = rounds;
  }

  /**
   * Private constructor for cloning purposes.
   *
   * @param md the instance to clone.
   */
  private Haval(Haval md)
  {
    this(md.hashSize, md.rounds);

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

  public Object clone()
  {
    return new Haval(this);
  }

  protected synchronized void transform(byte[] in, int i)
  {
    int X0 = (in[i++] & 0xFF)
           | (in[i++] & 0xFF) << 8
           | (in[i++] & 0xFF) << 16
           | (in[i++] & 0xFF) << 24;
    int X1 = (in[i++] & 0xFF)
           | (in[i++] & 0xFF) << 8
           | (in[i++] & 0xFF) << 16
           | (in[i++] & 0xFF) << 24;
    int X2 = (in[i++] & 0xFF)
           | (in[i++] & 0xFF) << 8
           | (in[i++] & 0xFF) << 16
           | (in[i++] & 0xFF) << 24;
    int X3 = (in[i++] & 0xFF)
           | (in[i++] & 0xFF) << 8
           | (in[i++] & 0xFF) << 16
           | (in[i++] & 0xFF) << 24;
    int X4 = (in[i++] & 0xFF)
           | (in[i++] & 0xFF) << 8
           | (in[i++] & 0xFF) << 16
           | (in[i++] & 0xFF) << 24;
    int X5 = (in[i++] & 0xFF)
           | (in[i++] & 0xFF) << 8
           | (in[i++] & 0xFF) << 16
           | (in[i++] & 0xFF) << 24;
    int X6 = (in[i++] & 0xFF)
           | (in[i++] & 0xFF) << 8
           | (in[i++] & 0xFF) << 16
           | (in[i++] & 0xFF) << 24;
    int X7 = (in[i++] & 0xFF)
           | (in[i++] & 0xFF) << 8
           | (in[i++] & 0xFF) << 16
           | (in[i++] & 0xFF) << 24;
    int X8 = (in[i++] & 0xFF)
           | (in[i++] & 0xFF) << 8
           | (in[i++] & 0xFF) << 16
           | (in[i++] & 0xFF) << 24;
    int X9 = (in[i++] & 0xFF)
           | (in[i++] & 0xFF) << 8
           | (in[i++] & 0xFF) << 16
           | (in[i++] & 0xFF) << 24;
    int X10 = (in[i++] & 0xFF)
            | (in[i++] & 0xFF) << 8
            | (in[i++] & 0xFF) << 16
            | (in[i++] & 0xFF) << 24;
    int X11 = (in[i++] & 0xFF)
            | (in[i++] & 0xFF) << 8
            | (in[i++] & 0xFF) << 16
            | (in[i++] & 0xFF) << 24;
    int X12 = (in[i++] & 0xFF)
            | (in[i++] & 0xFF) << 8
            | (in[i++] & 0xFF) << 16
            | (in[i++] & 0xFF) << 24;
    int X13 = (in[i++] & 0xFF)
            | (in[i++] & 0xFF) << 8
            | (in[i++] & 0xFF) << 16
            | (in[i++] & 0xFF) << 24;
    int X14 = (in[i++] & 0xFF)
            | (in[i++] & 0xFF) << 8
            | (in[i++] & 0xFF) << 16
            | (in[i++] & 0xFF) << 24;
    int X15 = (in[i++] & 0xFF)
            | (in[i++] & 0xFF) << 8
            | (in[i++] & 0xFF) << 16
            | (in[i++] & 0xFF) << 24;
    int X16 = (in[i++] & 0xFF)
            | (in[i++] & 0xFF) << 8
            | (in[i++] & 0xFF) << 16
            | (in[i++] & 0xFF) << 24;
    int X17 = (in[i++] & 0xFF)
            | (in[i++] & 0xFF) << 8
            | (in[i++] & 0xFF) << 16
            | (in[i++] & 0xFF) << 24;
    int X18 = (in[i++] & 0xFF)
            | (in[i++] & 0xFF) << 8
            | (in[i++] & 0xFF) << 16
            | (in[i++] & 0xFF) << 24;
    int X19 = (in[i++] & 0xFF)
            | (in[i++] & 0xFF) << 8
            | (in[i++] & 0xFF) << 16
            | (in[i++] & 0xFF) << 24;
    int X20 = (in[i++] & 0xFF)
            | (in[i++] & 0xFF) << 8
            | (in[i++] & 0xFF) << 16
            | (in[i++] & 0xFF) << 24;
    int X21 = (in[i++] & 0xFF)
            | (in[i++] & 0xFF) << 8
            | (in[i++] & 0xFF) << 16
            | (in[i++] & 0xFF) << 24;
    int X22 = (in[i++] & 0xFF)
            | (in[i++] & 0xFF) << 8
            | (in[i++] & 0xFF) << 16
            | (in[i++] & 0xFF) << 24;
    int X23 = (in[i++] & 0xFF)
            | (in[i++] & 0xFF) << 8
            | (in[i++] & 0xFF) << 16
            | (in[i++] & 0xFF) << 24;
    int X24 = (in[i++] & 0xFF)
            | (in[i++] & 0xFF) << 8
            | (in[i++] & 0xFF) << 16
            | (in[i++] & 0xFF) << 24;
    int X25 = (in[i++] & 0xFF)
            | (in[i++] & 0xFF) << 8
            | (in[i++] & 0xFF) << 16
            | (in[i++] & 0xFF) << 24;
    int X26 = (in[i++] & 0xFF)
            | (in[i++] & 0xFF) << 8
            | (in[i++] & 0xFF) << 16
            | (in[i++] & 0xFF) << 24;
    int X27 = (in[i++] & 0xFF)
            | (in[i++] & 0xFF) << 8
            | (in[i++] & 0xFF) << 16
            | (in[i++] & 0xFF) << 24;
    int X28 = (in[i++] & 0xFF)
            | (in[i++] & 0xFF) << 8
            | (in[i++] & 0xFF) << 16
            | (in[i++] & 0xFF) << 24;
    int X29 = (in[i++] & 0xFF)
            | (in[i++] & 0xFF) << 8
            | (in[i++] & 0xFF) << 16
            | (in[i++] & 0xFF) << 24;
    int X30 = (in[i++] & 0xFF)
            | (in[i++] & 0xFF) << 8
            | (in[i++] & 0xFF) << 16
            | (in[i++] & 0xFF) << 24;
    int X31 = (in[i++] & 0xFF)
            | (in[i++] & 0xFF) << 8
            | (in[i++] & 0xFF) << 16
            | (in[i++] & 0xFF) << 24;
    int t0 = h0, t1 = h1, t2 = h2, t3 = h3, t4 = h4, t5 = h5, t6 = h6, t7 = h7;
    // Pass 1
    t7 = FF1(t7, t6, t5, t4, t3, t2, t1, t0, X0);
    t6 = FF1(t6, t5, t4, t3, t2, t1, t0, t7, X1);
    t5 = FF1(t5, t4, t3, t2, t1, t0, t7, t6, X2);
    t4 = FF1(t4, t3, t2, t1, t0, t7, t6, t5, X3);
    t3 = FF1(t3, t2, t1, t0, t7, t6, t5, t4, X4);
    t2 = FF1(t2, t1, t0, t7, t6, t5, t4, t3, X5);
    t1 = FF1(t1, t0, t7, t6, t5, t4, t3, t2, X6);
    t0 = FF1(t0, t7, t6, t5, t4, t3, t2, t1, X7);

    t7 = FF1(t7, t6, t5, t4, t3, t2, t1, t0, X8);
    t6 = FF1(t6, t5, t4, t3, t2, t1, t0, t7, X9);
    t5 = FF1(t5, t4, t3, t2, t1, t0, t7, t6, X10);
    t4 = FF1(t4, t3, t2, t1, t0, t7, t6, t5, X11);
    t3 = FF1(t3, t2, t1, t0, t7, t6, t5, t4, X12);
    t2 = FF1(t2, t1, t0, t7, t6, t5, t4, t3, X13);
    t1 = FF1(t1, t0, t7, t6, t5, t4, t3, t2, X14);
    t0 = FF1(t0, t7, t6, t5, t4, t3, t2, t1, X15);

    t7 = FF1(t7, t6, t5, t4, t3, t2, t1, t0, X16);
    t6 = FF1(t6, t5, t4, t3, t2, t1, t0, t7, X17);
    t5 = FF1(t5, t4, t3, t2, t1, t0, t7, t6, X18);
    t4 = FF1(t4, t3, t2, t1, t0, t7, t6, t5, X19);
    t3 = FF1(t3, t2, t1, t0, t7, t6, t5, t4, X20);
    t2 = FF1(t2, t1, t0, t7, t6, t5, t4, t3, X21);
    t1 = FF1(t1, t0, t7, t6, t5, t4, t3, t2, X22);
    t0 = FF1(t0, t7, t6, t5, t4, t3, t2, t1, X23);

    t7 = FF1(t7, t6, t5, t4, t3, t2, t1, t0, X24);
    t6 = FF1(t6, t5, t4, t3, t2, t1, t0, t7, X25);
    t5 = FF1(t5, t4, t3, t2, t1, t0, t7, t6, X26);
    t4 = FF1(t4, t3, t2, t1, t0, t7, t6, t5, X27);
    t3 = FF1(t3, t2, t1, t0, t7, t6, t5, t4, X28);
    t2 = FF1(t2, t1, t0, t7, t6, t5, t4, t3, X29);
    t1 = FF1(t1, t0, t7, t6, t5, t4, t3, t2, X30);
    t0 = FF1(t0, t7, t6, t5, t4, t3, t2, t1, X31);

    // Pass 2
    t7 = FF2(t7, t6, t5, t4, t3, t2, t1, t0, X5, 0x452821E6);
    t6 = FF2(t6, t5, t4, t3, t2, t1, t0, t7, X14, 0x38D01377);
    t5 = FF2(t5, t4, t3, t2, t1, t0, t7, t6, X26, 0xBE5466CF);
    t4 = FF2(t4, t3, t2, t1, t0, t7, t6, t5, X18, 0x34E90C6C);
    t3 = FF2(t3, t2, t1, t0, t7, t6, t5, t4, X11, 0xC0AC29B7);
    t2 = FF2(t2, t1, t0, t7, t6, t5, t4, t3, X28, 0xC97C50DD);
    t1 = FF2(t1, t0, t7, t6, t5, t4, t3, t2, X7, 0x3F84D5B5);
    t0 = FF2(t0, t7, t6, t5, t4, t3, t2, t1, X16, 0xB5470917);

    t7 = FF2(t7, t6, t5, t4, t3, t2, t1, t0, X0, 0x9216D5D9);
    t6 = FF2(t6, t5, t4, t3, t2, t1, t0, t7, X23, 0x8979FB1B);
    t5 = FF2(t5, t4, t3, t2, t1, t0, t7, t6, X20, 0xD1310BA6);
    t4 = FF2(t4, t3, t2, t1, t0, t7, t6, t5, X22, 0x98DFB5AC);
    t3 = FF2(t3, t2, t1, t0, t7, t6, t5, t4, X1, 0x2FFD72DB);
    t2 = FF2(t2, t1, t0, t7, t6, t5, t4, t3, X10, 0xD01ADFB7);
    t1 = FF2(t1, t0, t7, t6, t5, t4, t3, t2, X4, 0xB8E1AFED);
    t0 = FF2(t0, t7, t6, t5, t4, t3, t2, t1, X8, 0x6A267E96);

    t7 = FF2(t7, t6, t5, t4, t3, t2, t1, t0, X30, 0xBA7C9045);
    t6 = FF2(t6, t5, t4, t3, t2, t1, t0, t7, X3, 0xF12C7F99);
    t5 = FF2(t5, t4, t3, t2, t1, t0, t7, t6, X21, 0x24A19947);
    t4 = FF2(t4, t3, t2, t1, t0, t7, t6, t5, X9, 0xB3916CF7);
    t3 = FF2(t3, t2, t1, t0, t7, t6, t5, t4, X17, 0x0801F2E2);
    t2 = FF2(t2, t1, t0, t7, t6, t5, t4, t3, X24, 0x858EFC16);
    t1 = FF2(t1, t0, t7, t6, t5, t4, t3, t2, X29, 0x636920D8);
    t0 = FF2(t0, t7, t6, t5, t4, t3, t2, t1, X6, 0x71574E69);

    t7 = FF2(t7, t6, t5, t4, t3, t2, t1, t0, X19, 0xA458FEA3);
    t6 = FF2(t6, t5, t4, t3, t2, t1, t0, t7, X12, 0xF4933D7E);
    t5 = FF2(t5, t4, t3, t2, t1, t0, t7, t6, X15, 0x0D95748F);
    t4 = FF2(t4, t3, t2, t1, t0, t7, t6, t5, X13, 0x728EB658);
    t3 = FF2(t3, t2, t1, t0, t7, t6, t5, t4, X2, 0x718BCD58);
    t2 = FF2(t2, t1, t0, t7, t6, t5, t4, t3, X25, 0x82154AEE);
    t1 = FF2(t1, t0, t7, t6, t5, t4, t3, t2, X31, 0x7B54A41D);
    t0 = FF2(t0, t7, t6, t5, t4, t3, t2, t1, X27, 0xC25A59B5);

    // Pass 3
    t7 = FF3(t7, t6, t5, t4, t3, t2, t1, t0, X19, 0x9C30D539);
    t6 = FF3(t6, t5, t4, t3, t2, t1, t0, t7, X9, 0x2AF26013);
    t5 = FF3(t5, t4, t3, t2, t1, t0, t7, t6, X4, 0xC5D1B023);
    t4 = FF3(t4, t3, t2, t1, t0, t7, t6, t5, X20, 0x286085F0);
    t3 = FF3(t3, t2, t1, t0, t7, t6, t5, t4, X28, 0xCA417918);
    t2 = FF3(t2, t1, t0, t7, t6, t5, t4, t3, X17, 0xB8DB38EF);
    t1 = FF3(t1, t0, t7, t6, t5, t4, t3, t2, X8, 0x8E79DCB0);
    t0 = FF3(t0, t7, t6, t5, t4, t3, t2, t1, X22, 0x603A180E);

    t7 = FF3(t7, t6, t5, t4, t3, t2, t1, t0, X29, 0x6C9E0E8B);
    t6 = FF3(t6, t5, t4, t3, t2, t1, t0, t7, X14, 0xB01E8A3E);
    t5 = FF3(t5, t4, t3, t2, t1, t0, t7, t6, X25, 0xD71577C1);
    t4 = FF3(t4, t3, t2, t1, t0, t7, t6, t5, X12, 0xBD314B27);
    t3 = FF3(t3, t2, t1, t0, t7, t6, t5, t4, X24, 0x78AF2FDA);
    t2 = FF3(t2, t1, t0, t7, t6, t5, t4, t3, X30, 0x55605C60);
    t1 = FF3(t1, t0, t7, t6, t5, t4, t3, t2, X16, 0xE65525F3);
    t0 = FF3(t0, t7, t6, t5, t4, t3, t2, t1, X26, 0xAA55AB94);

    t7 = FF3(t7, t6, t5, t4, t3, t2, t1, t0, X31, 0x57489862);
    t6 = FF3(t6, t5, t4, t3, t2, t1, t0, t7, X15, 0x63E81440);
    t5 = FF3(t5, t4, t3, t2, t1, t0, t7, t6, X7, 0x55CA396A);
    t4 = FF3(t4, t3, t2, t1, t0, t7, t6, t5, X3, 0x2AAB10B6);
    t3 = FF3(t3, t2, t1, t0, t7, t6, t5, t4, X1, 0xB4CC5C34);
    t2 = FF3(t2, t1, t0, t7, t6, t5, t4, t3, X0, 0x1141E8CE);
    t1 = FF3(t1, t0, t7, t6, t5, t4, t3, t2, X18, 0xA15486AF);
    t0 = FF3(t0, t7, t6, t5, t4, t3, t2, t1, X27, 0x7C72E993);

    t7 = FF3(t7, t6, t5, t4, t3, t2, t1, t0, X13, 0xB3EE1411);
    t6 = FF3(t6, t5, t4, t3, t2, t1, t0, t7, X6, 0x636FBC2A);
    t5 = FF3(t5, t4, t3, t2, t1, t0, t7, t6, X21, 0x2BA9C55D);
    t4 = FF3(t4, t3, t2, t1, t0, t7, t6, t5, X10, 0x741831F6);
    t3 = FF3(t3, t2, t1, t0, t7, t6, t5, t4, X23, 0xCE5C3E16);
    t2 = FF3(t2, t1, t0, t7, t6, t5, t4, t3, X11, 0x9B87931E);
    t1 = FF3(t1, t0, t7, t6, t5, t4, t3, t2, X5, 0xAFD6BA33);
    t0 = FF3(t0, t7, t6, t5, t4, t3, t2, t1, X2, 0x6C24CF5C);

    if (rounds >= 4)
      {
        t7 = FF4(t7, t6, t5, t4, t3, t2, t1, t0, X24, 0x7A325381);
        t6 = FF4(t6, t5, t4, t3, t2, t1, t0, t7, X4, 0x28958677);
        t5 = FF4(t5, t4, t3, t2, t1, t0, t7, t6, X0, 0x3B8F4898);
        t4 = FF4(t4, t3, t2, t1, t0, t7, t6, t5, X14, 0x6B4BB9AF);
        t3 = FF4(t3, t2, t1, t0, t7, t6, t5, t4, X2, 0xC4BFE81B);
        t2 = FF4(t2, t1, t0, t7, t6, t5, t4, t3, X7, 0x66282193);
        t1 = FF4(t1, t0, t7, t6, t5, t4, t3, t2, X28, 0x61D809CC);
        t0 = FF4(t0, t7, t6, t5, t4, t3, t2, t1, X23, 0xFB21A991);
        t7 = FF4(t7, t6, t5, t4, t3, t2, t1, t0, X26, 0x487CAC60);
        t6 = FF4(t6, t5, t4, t3, t2, t1, t0, t7, X6, 0x5DEC8032);
        t5 = FF4(t5, t4, t3, t2, t1, t0, t7, t6, X30, 0xEF845D5D);
        t4 = FF4(t4, t3, t2, t1, t0, t7, t6, t5, X20, 0xE98575B1);
        t3 = FF4(t3, t2, t1, t0, t7, t6, t5, t4, X18, 0xDC262302);
        t2 = FF4(t2, t1, t0, t7, t6, t5, t4, t3, X25, 0xEB651B88);
        t1 = FF4(t1, t0, t7, t6, t5, t4, t3, t2, X19, 0x23893E81);
        t0 = FF4(t0, t7, t6, t5, t4, t3, t2, t1, X3, 0xD396ACC5);

        t7 = FF4(t7, t6, t5, t4, t3, t2, t1, t0, X22, 0x0F6D6FF3);
        t6 = FF4(t6, t5, t4, t3, t2, t1, t0, t7, X11, 0x83F44239);
        t5 = FF4(t5, t4, t3, t2, t1, t0, t7, t6, X31, 0x2E0B4482);
        t4 = FF4(t4, t3, t2, t1, t0, t7, t6, t5, X21, 0xA4842004);
        t3 = FF4(t3, t2, t1, t0, t7, t6, t5, t4, X8, 0x69C8F04A);
        t2 = FF4(t2, t1, t0, t7, t6, t5, t4, t3, X27, 0x9E1F9B5E);
        t1 = FF4(t1, t0, t7, t6, t5, t4, t3, t2, X12, 0x21C66842);
        t0 = FF4(t0, t7, t6, t5, t4, t3, t2, t1, X9, 0xF6E96C9A);
        t7 = FF4(t7, t6, t5, t4, t3, t2, t1, t0, X1, 0x670C9C61);
        t6 = FF4(t6, t5, t4, t3, t2, t1, t0, t7, X29, 0xABD388F0);
        t5 = FF4(t5, t4, t3, t2, t1, t0, t7, t6, X5, 0x6A51A0D2);
        t4 = FF4(t4, t3, t2, t1, t0, t7, t6, t5, X15, 0xD8542F68);
        t3 = FF4(t3, t2, t1, t0, t7, t6, t5, t4, X17, 0x960FA728);
        t2 = FF4(t2, t1, t0, t7, t6, t5, t4, t3, X10, 0xAB5133A3);
        t1 = FF4(t1, t0, t7, t6, t5, t4, t3, t2, X16, 0x6EEF0B6C);
        t0 = FF4(t0, t7, t6, t5, t4, t3, t2, t1, X13, 0x137A3BE4);

        if (rounds == 5)
          {
            t7 = FF5(t7, t6, t5, t4, t3, t2, t1, t0, X27, 0xBA3BF050);
            t6 = FF5(t6, t5, t4, t3, t2, t1, t0, t7, X3, 0x7EFB2A98);
            t5 = FF5(t5, t4, t3, t2, t1, t0, t7, t6, X21, 0xA1F1651D);
            t4 = FF5(t4, t3, t2, t1, t0, t7, t6, t5, X26, 0x39AF0176);
            t3 = FF5(t3, t2, t1, t0, t7, t6, t5, t4, X17, 0x66CA593E);
            t2 = FF5(t2, t1, t0, t7, t6, t5, t4, t3, X11, 0x82430E88);
            t1 = FF5(t1, t0, t7, t6, t5, t4, t3, t2, X20, 0x8CEE8619);
            t0 = FF5(t0, t7, t6, t5, t4, t3, t2, t1, X29, 0x456F9FB4);

            t7 = FF5(t7, t6, t5, t4, t3, t2, t1, t0, X19, 0x7D84A5C3);
            t6 = FF5(t6, t5, t4, t3, t2, t1, t0, t7, X0, 0x3B8B5EBE);
            t5 = FF5(t5, t4, t3, t2, t1, t0, t7, t6, X12, 0xE06F75D8);
            t4 = FF5(t4, t3, t2, t1, t0, t7, t6, t5, X7, 0x85C12073);
            t3 = FF5(t3, t2, t1, t0, t7, t6, t5, t4, X13, 0x401A449F);
            t2 = FF5(t2, t1, t0, t7, t6, t5, t4, t3, X8, 0x56C16AA6);
            t1 = FF5(t1, t0, t7, t6, t5, t4, t3, t2, X31, 0x4ED3AA62);
            t0 = FF5(t0, t7, t6, t5, t4, t3, t2, t1, X10, 0x363F7706);

            t7 = FF5(t7, t6, t5, t4, t3, t2, t1, t0, X5, 0x1BFEDF72);
            t6 = FF5(t6, t5, t4, t3, t2, t1, t0, t7, X9, 0x429B023D);
            t5 = FF5(t5, t4, t3, t2, t1, t0, t7, t6, X14, 0x37D0D724);
            t4 = FF5(t4, t3, t2, t1, t0, t7, t6, t5, X30, 0xD00A1248);
            t3 = FF5(t3, t2, t1, t0, t7, t6, t5, t4, X18, 0xDB0FEAD3);
            t2 = FF5(t2, t1, t0, t7, t6, t5, t4, t3, X6, 0x49F1C09B);
            t1 = FF5(t1, t0, t7, t6, t5, t4, t3, t2, X28, 0x075372C9);
            t0 = FF5(t0, t7, t6, t5, t4, t3, t2, t1, X24, 0x80991B7B);

            t7 = FF5(t7, t6, t5, t4, t3, t2, t1, t0, X2, 0x25D479D8);
            t6 = FF5(t6, t5, t4, t3, t2, t1, t0, t7, X23, 0xF6E8DEF7);
            t5 = FF5(t5, t4, t3, t2, t1, t0, t7, t6, X16, 0xE3FE501A);
            t4 = FF5(t4, t3, t2, t1, t0, t7, t6, t5, X22, 0xB6794C3B);
            t3 = FF5(t3, t2, t1, t0, t7, t6, t5, t4, X4, 0x976CE0BD);
            t2 = FF5(t2, t1, t0, t7, t6, t5, t4, t3, X1, 0x04C006BA);
            t1 = FF5(t1, t0, t7, t6, t5, t4, t3, t2, X25, 0xC1A94FB6);
            t0 = FF5(t0, t7, t6, t5, t4, t3, t2, t1, X15, 0x409F60C4);
          }
      }
    h7 += t7;
    h6 += t6;
    h5 += t5;
    h4 += t4;
    h3 += t3;
    h2 += t2;
    h1 += t1;
    h0 += t0;
  }

  protected byte[] padBuffer()
  {
    // pad out to 118 mod 128. other 10 bytes have special use.
    int n = (int)(count % BLOCK_SIZE);
    int padding = (n < 118) ? (118 - n) : (246 - n);
    byte[] result = new byte[padding + 10];
    result[0] = (byte) 0x01;
    // save the version number (LSB 3), the number of rounds (3 bits in the
    // middle), the fingerprint length (MSB 2 bits and next byte) and the
    // number of bits in the unpadded message.
    int bl = hashSize * 8;
    int sigByte = (bl & 0x03) << 6;
    sigByte |= (rounds & 0x07) << 3;
    sigByte |= HAVAL_VERSION & 0x07;
    result[padding++] = (byte) sigByte;
    result[padding++] = (byte)(bl >>> 2);
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
    tailorDigestBits(); // tailor context for the designated output size
    // cast enough top context values into an array of hashSize bytes
    byte[] result = new byte[hashSize];
    if (hashSize >= HAVAL_256_BIT)
      {
        result[31] = (byte)(h7 >>> 24);
        result[30] = (byte)(h7 >>> 16);
        result[29] = (byte)(h7 >>> 8);
        result[28] = (byte) h7;
      }
    if (hashSize >= HAVAL_224_BIT)
      {
        result[27] = (byte)(h6 >>> 24);
        result[26] = (byte)(h6 >>> 16);
        result[25] = (byte)(h6 >>> 8);
        result[24] = (byte) h6;
      }
    if (hashSize >= HAVAL_192_BIT)
      {
        result[23] = (byte)(h5 >>> 24);
        result[22] = (byte)(h5 >>> 16);
        result[21] = (byte)(h5 >>> 8);
        result[20] = (byte) h5;
      }
    if (hashSize >= HAVAL_160_BIT)
      {
        result[19] = (byte)(h4 >>> 24);
        result[18] = (byte)(h4 >>> 16);
        result[17] = (byte)(h4 >>> 8);
        result[16] = (byte) h4;
      }
    result[15] = (byte)(h3 >>> 24);
    result[14] = (byte)(h3 >>> 16);
    result[13] = (byte)(h3 >>> 8);
    result[12] = (byte) h3;
    result[11] = (byte)(h2 >>> 24);
    result[10] = (byte)(h2 >>> 16);
    result[ 9] = (byte)(h2 >>> 8);
    result[ 8] = (byte) h2;
    result[ 7] = (byte)(h1 >>> 24);
    result[ 6] = (byte)(h1 >>> 16);
    result[ 5] = (byte)(h1 >>> 8);
    result[ 4] = (byte) h1;
    result[ 3] = (byte)(h0 >>> 24);
    result[ 2] = (byte)(h0 >>> 16);
    result[ 1] = (byte)(h0 >>> 8);
    result[ 0] = (byte) h0;
    return result;
  }

  protected void resetContext()
  {
    h0 = 0x243F6A88;
    h1 = 0x85A308D3;
    h2 = 0x13198A2E;
    h3 = 0x03707344;
    h4 = 0xA4093822;
    h5 = 0x299F31D0;
    h6 = 0x082EFA98;
    h7 = 0xEC4E6C89;
  }

  public boolean selfTest()
  {
    if (valid == null)
      {
        String d = Util.toString(new Haval().digest());
        valid = Boolean.valueOf(DIGEST0.equals(d));
      }
    return valid.booleanValue();
  }

  /** Tailors the last output. */
  private void tailorDigestBits()
  {
    int t;
    switch (hashSize)
      {
      case HAVAL_128_BIT:
        t = (h7 & 0x000000FF)
          | (h6 & 0xFF000000)
          | (h5 & 0x00FF0000)
          | (h4 & 0x0000FF00);
        h0 += t >>> 8 | t << 24;
        t = (h7 & 0x0000FF00)
          | (h6 & 0x000000FF)
          | (h5 & 0xFF000000)
          | (h4 & 0x00FF0000);
        h1 += t >>> 16 | t << 16;
        t = (h7 & 0x00FF0000)
          | (h6 & 0x0000FF00)
          | (h5 & 0x000000FF)
          | (h4 & 0xFF000000);
        h2 += t >>> 24 | t << 8;
        t = (h7 & 0xFF000000)
          | (h6 & 0x00FF0000)
          | (h5 & 0x0000FF00)
          | (h4 & 0x000000FF);
        h3 += t;
        break;
      case HAVAL_160_BIT:
        t = (h7 & 0x3F) | (h6 & (0x7F << 25)) | (h5 & (0x3F << 19));
        h0 += t >>> 19 | t << 13;
        t = (h7 & (0x3F << 6)) | (h6 & 0x3F) | (h5 & (0x7F << 25));
        h1 += t >>> 25 | t << 7;
        t = (h7 & (0x7F << 12)) | (h6 & (0x3F << 6)) | (h5 & 0x3F);
        h2 += t;
        t = (h7 & (0x3F << 19)) | (h6 & (0x7F << 12)) | (h5 & (0x3F << 6));
        h3 += (t >>> 6);
        t = (h7 & (0x7F << 25)) | (h6 & (0x3F << 19)) | (h5 & (0x7F << 12));
        h4 += (t >>> 12);
        break;
      case HAVAL_192_BIT:
        t = (h7 & 0x1F) | (h6 & (0x3F << 26));
        h0 += t >>> 26 | t << 6;
        t = (h7 & (0x1F << 5)) | (h6 & 0x1F);
        h1 += t;
        t = (h7 & (0x3F << 10)) | (h6 & (0x1F << 5));
        h2 += (t >>> 5);
        t = (h7 & (0x1F << 16)) | (h6 & (0x3F << 10));
        h3 += (t >>> 10);
        t = (h7 & (0x1F << 21)) | (h6 & (0x1F << 16));
        h4 += (t >>> 16);
        t = (h7 & (0x3F << 26)) | (h6 & (0x1F << 21));
        h5 += (t >>> 21);
        break;
      case HAVAL_224_BIT:
        h0 += ((h7 >>> 27) & 0x1F);
        h1 += ((h7 >>> 22) & 0x1F);
        h2 += ((h7 >>> 18) & 0x0F);
        h3 += ((h7 >>> 13) & 0x1F);
        h4 += ((h7 >>>  9) & 0x0F);
        h5 += ((h7 >>>  4) & 0x1F);
        h6 +=  (h7         & 0x0F);
      }
  }

  /**
   * Permutations phi_{i,j}, i=3,4,5, j=1,...,i.
   *
   * rounds = 3:   6 5 4 3 2 1 0
   *               | | | | | | | (replaced by)
   *  phi_{3,1}:   1 0 3 5 6 2 4
   *  phi_{3,2}:   4 2 1 0 5 3 6
   *  phi_{3,3}:   6 1 2 3 4 5 0
   *
   * rounds = 4:   6 5 4 3 2 1 0
   *               | | | | | | | (replaced by)
   *  phi_{4,1}:   2 6 1 4 5 3 0
   *  phi_{4,2}:   3 5 2 0 1 6 4
   *  phi_{4,3}:   1 4 3 6 0 2 5
   *  phi_{4,4}:   6 4 0 5 2 1 3
   *
   * rounds = 5:   6 5 4 3 2 1 0
   *               | | | | | | | (replaced by)
   *  phi_{5,1}:   3 4 1 0 5 2 6
   *  phi_{5,2}:   6 2 1 0 3 4 5
   *  phi_{5,3}:   2 6 0 4 3 1 5
   *  phi_{5,4}:   1 5 3 2 0 4 6
   *  phi_{5,5}:   2 5 0 6 4 3 1
   */
  private int FF1(int x7, int x6, int x5, int x4, int x3, int x2, int x1,
                  int x0, int w)
  {
    int t;
    switch (rounds)
      {
      case 3:
        t = f1(x1, x0, x3, x5, x6, x2, x4);
        break;
      case 4:
        t = f1(x2, x6, x1, x4, x5, x3, x0);
        break;
      default:
        t = f1(x3, x4, x1, x0, x5, x2, x6);
      }
    return (t >>> 7 | t << 25) + (x7 >>> 11 | x7 << 21) + w;
  }

  private int FF2(int x7, int x6, int x5, int x4, int x3, int x2, int x1,
                  int x0, int w, int c)
  {
    int t;
    switch (rounds)
      {
      case 3:
        t = f2(x4, x2, x1, x0, x5, x3, x6);
        break;
      case 4:
        t = f2(x3, x5, x2, x0, x1, x6, x4);
        break;
      default:
        t = f2(x6, x2, x1, x0, x3, x4, x5);
      }
    return (t >>> 7 | t << 25) + (x7 >>> 11 | x7 << 21) + w + c;
  }

  private int FF3(int x7, int x6, int x5, int x4, int x3, int x2, int x1,
                  int x0, int w, int c)
  {
    int t;
    switch (rounds)
      {
      case 3:
        t = f3(x6, x1, x2, x3, x4, x5, x0);
        break;
      case 4:
        t = f3(x1, x4, x3, x6, x0, x2, x5);
        break;
      default:
        t = f3(x2, x6, x0, x4, x3, x1, x5);
      }
    return (t >>> 7 | t << 25) + (x7 >>> 11 | x7 << 21) + w + c;
  }

  private int FF4(int x7, int x6, int x5, int x4, int x3, int x2, int x1,
                  int x0, int w, int c)
  {
    int t;
    switch (rounds)
      {
      case 4:
        t = f4(x6, x4, x0, x5, x2, x1, x3);
        break;
      default:
        t = f4(x1, x5, x3, x2, x0, x4, x6);
      }
    return (t >>> 7 | t << 25) + (x7 >>> 11 | x7 << 21) + w + c;
  }

  private int FF5(int x7, int x6, int x5, int x4, int x3, int x2, int x1,
                  int x0, int w, int c)
  {
    int t = f5(x2, x5, x0, x6, x4, x3, x1);
    return (t >>> 7 | t << 25) + (x7 >>> 11 | x7 << 21) + w + c;
  }

  private int f1(int x6, int x5, int x4, int x3, int x2, int x1, int x0)
  {
    return x1 & (x0 ^ x4) ^ x2 & x5 ^ x3 & x6 ^ x0;
  }

  private int f2(int x6, int x5, int x4, int x3, int x2, int x1, int x0)
  {
    return x2 & (x1 & ~x3 ^ x4 & x5 ^ x6 ^ x0) ^ x4 & (x1 ^ x5) ^ x3 & x5 ^ x0;
  }

  private int f3(int x6, int x5, int x4, int x3, int x2, int x1, int x0)
  {
    return x3 & (x1 & x2 ^ x6 ^ x0) ^ x1 & x4 ^ x2 & x5 ^ x0;
  }

  private int f4(int x6, int x5, int x4, int x3, int x2, int x1, int x0)
  {
    return x4 & (x5 & ~x2 ^ x3 & ~x6 ^ x1 ^ x6 ^ x0) ^ x3
           & (x1 & x2 ^ x5 ^ x6) ^ x2 & x6 ^ x0;
  }

  private int f5(int x6, int x5, int x4, int x3, int x2, int x1, int x0)
  {
    return x0 & (x1 & x2 & x3 ^ ~x5) ^ x1 & x4 ^ x2 & x5 ^ x3 & x6;
  }
}
