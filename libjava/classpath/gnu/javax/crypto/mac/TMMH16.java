/* TMMH16.java --
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


package gnu.javax.crypto.mac;

import gnu.java.security.Registry;
import gnu.java.security.prng.IRandom;
import gnu.java.security.prng.LimitReachedException;

import java.security.InvalidKeyException;
import java.util.Map;

/**
 * <i>TMMH</i> is a <i>universal</i> hash function suitable for message
 * authentication in the Wegman-Carter paradigm, as in the Stream Cipher
 * Security Transform. It is simple, quick, and especially appropriate for
 * Digital Signal Processors and other processors with a fast multiply
 * operation, though a straightforward implementation requires storage equal in
 * length to the largest message to be hashed.
 * <p>
 * <i>TMMH</i> is a simple hash function which maps a key and a message to a
 * hash value. There are two versions of TMMH: TMMH/16 and TMMH/32. <i>TMMH</i>
 * can be used as a message authentication code, as described in Section 5 (see
 * References).
 * <p>
 * The key, message, and hash value are all octet strings, and the lengths of
 * these quantities are denoted as <code>KEY_LENGTH</code>,
 * <code>MESSAGE_LENGTH</code>, and <code>TAG_LENGTH</code>, respectively.
 * The values of <code>KEY_LENGTH</code> and <code>TAG_LENGTH</code>
 * <bold>MUST</bold> be fixed for any particular fixed value of the key, and
 * must obey the alignment restrictions described below.
 * <p>
 * The parameter <code>MAX_HASH_LENGTH</code>, which denotes the maximum
 * value which <code>MESSAGE_LENGTH</code> may take, is equal to
 * <code>KEY_LENGTH - TAG_LENGTH</code>.
 * <p>
 * References:
 * <ol>
 * <li><a
 * href="http://www.ietf.org/internet-drafts/draft-mcgrew-saag-tmmh-01.txt"> The
 * Truncated Multi-Modular Hash Function (TMMH)</a>, David A. McGrew.</li>
 * </ol>
 */
public class TMMH16
    extends BaseMac
    implements Cloneable
{
  public static final String TAG_LENGTH = "gnu.crypto.mac.tmmh.tag.length";
  public static final String KEYSTREAM = "gnu.crypto.mac.tmmh.keystream";
  public static final String PREFIX = "gnu.crypto.mac.tmmh.prefix";
  private static final int P = (1 << 16) + 1; // the TMMH/16 prime
  /** caches the result of the correctness test, once executed. */
  private static Boolean valid;
  private int tagWords = 0; // the tagLength expressed in words
  private IRandom keystream = null; // the keystream generator
  private byte[] prefix; // mask to use when operating as an authentication f.
  private long keyWords; // key words counter
  private long msgLength; // in bytes
  private long msgWords; // should be = msgLength * WORD_LENGTH
  private int[] context; // the tmmh running context; length == TAG_WORDS
  private int[] K0; // the first TAG_WORDS words of the keystream
  private int[] Ki; // the sliding TAG_WORDS words of the keystream
  private int Mi; // current message word being constructed

  /** Trivial 0-arguments constructor. */
  public TMMH16()
  {
    super(Registry.TMMH16);
  }

  public int macSize()
  {
    return tagWords * 2;
  }

  public void init(Map attributes) throws InvalidKeyException,
      IllegalStateException
  {
    int wantTagLength = 0;
    Integer tagLength = (Integer) attributes.get(TAG_LENGTH); // get tag length
    if (tagLength == null)
      {
        if (tagWords == 0) // was never set
          throw new IllegalArgumentException(TAG_LENGTH);
        // else re-use
      }
    else // check if positive and is divisible by WORD_LENGTH
      {
        wantTagLength = tagLength.intValue();
        if (wantTagLength < 2 || (wantTagLength % 2 != 0))
          throw new IllegalArgumentException(TAG_LENGTH);
        else if (wantTagLength > (512 / 8)) // 512-bits is our maximum
          throw new IllegalArgumentException(TAG_LENGTH);

        tagWords = wantTagLength / 2; // init local vars
        K0 = new int[tagWords];
        Ki = new int[tagWords];
        context = new int[tagWords];
      }

    prefix = (byte[]) attributes.get(PREFIX);
    if (prefix == null) // default to all-zeroes
      prefix = new byte[tagWords * 2];
    else // ensure it's as long as it should
      {
        if (prefix.length != tagWords * 2)
          throw new IllegalArgumentException(PREFIX);
      }

    IRandom prng = (IRandom) attributes.get(KEYSTREAM); // get keystream
    if (prng == null)
      {
        if (keystream == null)
          throw new IllegalArgumentException(KEYSTREAM);
        // else reuse
      }
    else
      keystream = prng;

    reset(); // reset context variables
    for (int i = 0; i < tagWords; i++) // init starting key words
      Ki[i] = K0[i] = getNextKeyWord(keystream);
  }

  // The words of the key are denoted as K[1], K[2], ..., K[KEY_WORDS], and the
  // words of the message (after zero padding, if needed) are denoted as M[1],
  // M[2], ..., M[MSG_WORDS], where MSG_WORDS is the smallest number such that
  // 2 * MSG_WORDS is at least MESSAGE_LENGTH, and KEY_WORDS is KEY_LENGTH / 2.
  //
  // If MESSAGE_LENGTH is greater than MAX_HASH_LENGTH, then the value of
  // TMMH/16 is undefined. Implementations MUST indicate an error if asked to
  // hash a message with such a length. Otherwise, the hash value is defined
  // to be the length TAG_WORDS sequence of words in which the j-th word in the
  // sequence is defined as
  //
  // [ [ K[j] * MESSAGE_LENGTH +32 K[j+1] * M[1] +32 K[j+2] * M[2]
  // +32 ... K[j+MSG_WORDS] * M[MSG_WORDS] ] modulo p ] modulo 2^16
  //
  // where j ranges from 1 to TAG_WORDS.
  public void update(byte b)
  {
    this.update(b, keystream);
  }

  public void update(byte[] b, int offset, int len)
  {
    for (int i = 0; i < len; i++)
      this.update(b[offset + i], keystream);
  }

  // For TMMH/16, KEY_LENGTH and TAG_LENGTH MUST be a multiple of two. The key,
  // message, and hash value are treated as a sequence of unsigned sixteen bit
  // integers in network byte order. (In this section, we call such an integer
  // a word.) If MESSAGE_LENGTH is odd, then a zero byte is appended to the
  // message to align it on a word boundary, though this process does not
  // change the value of MESSAGE_LENGTH.
  //
  // ... Otherwise, the hash value is defined to be the length TAG_WORDS
  // sequence of words in which the j-th word in the sequence is defined as
  //
  // [ [ K[j] * MESSAGE_LENGTH +32 K[j+1] * M[1] +32 K[j+2] * M[2]
  // +32 ... K[j+MSG_WORDS] * M[MSG_WORDS] ] modulo p ] modulo 2^16
  //
  // where j ranges from 1 to TAG_WORDS.
  //
  // Here, TAG_WORDS is equal to TAG_LENGTH / 2, and p is equal to 2^16 + 1.
  // The symbol * denotes multiplication and the symbol +32 denotes addition
  // modulo 2^32.
  public byte[] digest()
  {
    return this.digest(keystream);
  }

  public void reset()
  {
    msgLength = msgWords = keyWords = 0L;
    Mi = 0;
    for (int i = 0; i < tagWords; i++)
      context[i] = 0;
  }

  public boolean selfTest()
  {
    if (valid == null)
      {
        // TODO: compute and test equality with one known vector
        valid = Boolean.TRUE;
      }
    return valid.booleanValue();
  }

  public Object clone() throws CloneNotSupportedException
  {
    TMMH16 result = (TMMH16) super.clone();
    if (this.keystream != null)
      result.keystream = (IRandom) this.keystream.clone();
    if (this.prefix != null)
      result.prefix = (byte[]) this.prefix.clone();
    if (this.context != null)
      result.context = (int[]) this.context.clone();
    if (this.K0 != null)
      result.K0 = (int[]) this.K0.clone();
    if (this.Ki != null)
      result.Ki = (int[]) this.Ki.clone();
    return result;
  }

  /**
   * Similar to the same method with one argument, but uses the designated
   * random number generator to compute needed keying material.
   *
   * @param b the byte to process.
   * @param prng the source of randomness to use.
   */
  public void update(byte b, IRandom prng)
  {
    Mi <<= 8; // update message buffer
    Mi |= b & 0xFF;
    msgLength++; // update message length (bytes)
    if (msgLength % 2 == 0) // got a full word
      {
        msgWords++; // update message words counter
        System.arraycopy(Ki, 1, Ki, 0, tagWords - 1); // 1. shift Ki up by 1
        Ki[tagWords - 1] = getNextKeyWord(prng); // 2. fill last box of Ki
        long t; // temp var to allow working in modulo 2^32
        for (int i = 0; i < tagWords; i++) // 3. update context
          {
            t = context[i] & 0xFFFFFFFFL;
            t += Ki[i] * Mi;
            context[i] = (int) t;
          }
        Mi = 0; // reset message buffer
      }
  }

  /**
   * Similar to the same method with three arguments, but uses the designated
   * random number generator to compute needed keying material.
   *
   * @param b the byte array to process.
   * @param offset the starting offset in <code>b</code> to start considering
   *          the bytes to process.
   * @param len the number of bytes in <code>b</code> starting from
   *          <code>offset</code> to process.
   * @param prng the source of randomness to use.
   */
  public void update(byte[] b, int offset, int len, IRandom prng)
  {
    for (int i = 0; i < len; i++)
      this.update(b[offset + i], prng);
  }

  /**
   * Similar to the same method with no arguments, but uses the designated
   * random number generator to compute needed keying material.
   *
   * @param prng the source of randomness to use.
   * @return the final result of the algorithm.
   */
  public byte[] digest(IRandom prng)
  {
    doFinalRound(prng);
    byte[] result = new byte[tagWords * 2];
    for (int i = 0, j = 0; i < tagWords; i++)
      {
        result[j] = (byte)((context[i] >>> 8) ^ prefix[j]);
        j++;
        result[j] = (byte)(context[i] ^ prefix[j]);
        j++;
      }
    reset();
    return result;
  }

  private int getNextKeyWord(IRandom prng)
  {
    int result = 0;
    try
      {
        result = (prng.nextByte() & 0xFF) << 8 | (prng.nextByte() & 0xFF);
      }
    catch (LimitReachedException x)
      {
        throw new RuntimeException(String.valueOf(x));
      }
    keyWords++; // update key words counter
    return result;
  }

  private void doFinalRound(IRandom prng)
  {
    long limit = msgLength; // formula works on real message length
    while (msgLength % 2 != 0)
      update((byte) 0x00, prng);
    long t;
    for (int i = 0; i < tagWords; i++)
      {
        t = context[i] & 0xFFFFFFFFL;
        t += K0[i] * limit;
        t %= P;
        context[i] = (int) t;
      }
  }
}
