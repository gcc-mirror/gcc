/* UHash32.java -- 
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


package gnu.javax.crypto.mac;

import gnu.java.security.prng.IRandom;
import gnu.java.security.prng.LimitReachedException;
import gnu.javax.crypto.cipher.IBlockCipher;
import gnu.javax.crypto.prng.UMacGenerator;

import java.io.ByteArrayOutputStream;
import java.math.BigInteger;
import java.security.InvalidKeyException;
import java.util.HashMap;
import java.util.Map;

/**
 * <i>UHASH</i> is a keyed hash function, which takes as input a string of
 * arbitrary length, and produces as output a string of fixed length (such as 8
 * bytes). The actual output length depends on the parameter UMAC-OUTPUT-LEN.
 * <p>
 * <i>UHASH</i> has been shown to be <i>epsilon-ASU</i> ("Almost Strongly
 * Universal"), where epsilon is a small (parameter-dependent) real number.
 * Informally, saying that a keyed hash function is <i>epsilon-ASU</i> means
 * that for any two distinct fixed input strings, the two outputs of the hash
 * function with a random key "look almost like a pair of random strings". The
 * number epsilon measures how non-random the output strings may be.
 * <p>
 * <i>UHASH</i> has been designed to be fast by exploiting several
 * architectural features of modern commodity processors. It was specifically
 * designed for use in <i>UMAC</i>. But <i>UHASH</i> is useful beyond that
 * domain, and can be easily adopted for other purposes.
 * <p>
 * <i>UHASH</i> does its work in three layers. First, a hash function called
 * <code>NH</code> is used to compress input messages into strings which are
 * typically many times smaller than the input message. Second, the compressed
 * message is hashed with an optimized <i>polynomial hash function</i> into a
 * fixed-length 16-byte string. Finally, the 16-byte string is hashed using an
 * <i>inner-product hash</i> into a string of length WORD-LEN bytes. These
 * three layers are repeated (with a modified key) until the outputs total
 * UMAC-OUTPUT-LEN bytes.
 * <p>
 * References:
 * <ol>
 * <li><a href="http://www.ietf.org/internet-drafts/draft-krovetz-umac-01.txt">
 * UMAC</a>: Message Authentication Code using Universal Hashing.<br>
 * T. Krovetz, J. Black, S. Halevi, A. Hevia, H. Krawczyk, and P. Rogaway.</li>
 * </ol>
 */
public class UHash32
    extends BaseMac
{
  // UMAC prime values
  private static final BigInteger PRIME_19 = BigInteger.valueOf(0x7FFFFL);
  private static final BigInteger PRIME_32 = BigInteger.valueOf(0xFFFFFFFBL);
  private static final BigInteger PRIME_36 = BigInteger.valueOf(0xFFFFFFFFBL);
  private static final BigInteger PRIME_64 = new BigInteger(1, new byte[] {
      (byte) 0xFF, (byte) 0xFF, (byte) 0xFF, (byte) 0xFF,
      (byte) 0xFF, (byte) 0xFF, (byte) 0xFF, (byte) 0xC5 });
  private static final BigInteger PRIME_128 = new BigInteger(1, new byte[] {
      (byte) 0xFF, (byte) 0xFF, (byte) 0xFF, (byte) 0xFF,
      (byte) 0xFF, (byte) 0xFF, (byte) 0xFF, (byte) 0xFF,
      (byte) 0xFF, (byte) 0xFF, (byte) 0xFF, (byte) 0xFF,
      (byte) 0xFF, (byte) 0xFF, (byte) 0xFF, (byte) 0x61 });
  static final BigInteger TWO = BigInteger.valueOf(2L);
  static final long BOUNDARY = TWO.shiftLeft(17).longValue();
  // 2**64 - 2**32
  static final BigInteger LOWER_RANGE = TWO.pow(64).subtract(TWO.pow(32));
  // 2**128 - 2**96
  static final BigInteger UPPER_RANGE = TWO.pow(128).subtract(TWO.pow(96));
  static final byte[] ALL_ZEROES = new byte[32];
  int streams;
  L1Hash32[] l1hash;

  /** Trivial 0-arguments constructor. */
  public UHash32()
  {
    super("uhash32");
  }

  /**
   * Private constructor for cloning purposes.
   *
   * @param that the instance to clone.
   */
  private UHash32(UHash32 that)
  {
    this();

    this.streams = that.streams;
    if (that.l1hash != null)
      {
        this.l1hash = new L1Hash32[that.streams];
        for (int i = 0; i < that.streams; i++)
          if (that.l1hash[i] != null)
            this.l1hash[i] = (L1Hash32) that.l1hash[i].clone();
      }
  }

  /**
   * The prime numbers used in UMAC are:
   * <pre>
   *   +-----+--------------------+---------------------------------------+
   *   |  x  | prime(x) [Decimal] | prime(x) [Hexadecimal]                |
   *   +-----+--------------------+---------------------------------------+
   *   | 19  | 2^19  - 1          | 0x0007FFFF                            |
   *   | 32  | 2^32  - 5          | 0xFFFFFFFB                            |
   *   | 36  | 2^36  - 5          | 0x0000000F FFFFFFFB                   |
   *   | 64  | 2^64  - 59         | 0xFFFFFFFF FFFFFFC5                   |
   *   | 128 | 2^128 - 159        | 0xFFFFFFFF FFFFFFFF FFFFFFFF FFFFFF61 |
   *   +-----+--------------------+---------------------------------------+
   *</pre>
   *
   * @param n a number of bits.
   * @return the largest prime number less than 2**n.
   */
  static final BigInteger prime(int n)
  {
    switch (n)
      {
      case 19:
        return PRIME_19;
      case 32:
        return PRIME_32;
      case 36:
        return PRIME_36;
      case 64:
        return PRIME_64;
      case 128:
        return PRIME_128;
      default:
        throw new IllegalArgumentException("Undefined prime("
                                           + String.valueOf(n) + ")");
      }
  }

  public Object clone()
  {
    return new UHash32(this);
  }

  public int macSize()
  {
    return UMac32.OUTPUT_LEN;
  }

  public void init(Map attributes) throws InvalidKeyException,
      IllegalStateException
  {
    byte[] K = (byte[]) attributes.get(MAC_KEY_MATERIAL);
    if (K == null)
      throw new InvalidKeyException("Null Key");
    if (K.length != UMac32.KEY_LEN)
      throw new InvalidKeyException("Invalid Key length: "
                                    + String.valueOf(K.length));
    // Calculate iterations needed to make UMAC-OUTPUT-LEN bytes
    streams = (UMac32.OUTPUT_LEN + 3) / 4;
    // Define total key needed for all iterations using UMacGenerator.
    // L1Key and L3Key1 both reuse most key between iterations.
    IRandom kdf1 = new UMacGenerator();
    IRandom kdf2 = new UMacGenerator();
    IRandom kdf3 = new UMacGenerator();
    IRandom kdf4 = new UMacGenerator();
    Map map = new HashMap();
    map.put(IBlockCipher.KEY_MATERIAL, K);
    map.put(UMacGenerator.INDEX, Integer.valueOf(0));
    kdf1.init(map);
    map.put(UMacGenerator.INDEX, Integer.valueOf(1));
    kdf2.init(map);
    map.put(UMacGenerator.INDEX, Integer.valueOf(2));
    kdf3.init(map);
    map.put(UMacGenerator.INDEX, Integer.valueOf(3));
    kdf4.init(map);
    // need to generate all bytes for use later in a Toepliz construction
    byte[] L1Key = new byte[UMac32.L1_KEY_LEN + (streams - 1) * 16];
    try
      {
        kdf1.nextBytes(L1Key, 0, L1Key.length);
      }
    catch (LimitReachedException x)
      {
        x.printStackTrace(System.err);
        throw new RuntimeException("KDF for L1Key reached limit");
      }

    l1hash = new L1Hash32[streams];
    for (int i = 0; i < streams; i++)
      {
        byte[] k1 = new byte[UMac32.L1_KEY_LEN];
        System.arraycopy(L1Key, i * 16, k1, 0, UMac32.L1_KEY_LEN);
        byte[] k2 = new byte[24];
        try
          {
            kdf2.nextBytes(k2, 0, 24);
          }
        catch (LimitReachedException x)
          {
            x.printStackTrace(System.err);
            throw new RuntimeException("KDF for L2Key reached limit");
          }
        byte[] k31 = new byte[64];
        try
          {
            kdf3.nextBytes(k31, 0, 64);
          }
        catch (LimitReachedException x)
          {
            x.printStackTrace(System.err);
            throw new RuntimeException("KDF for L3Key1 reached limit");
          }
        byte[] k32 = new byte[4];
        try
          {
            kdf4.nextBytes(k32, 0, 4);
          }
        catch (LimitReachedException x)
          {
            x.printStackTrace(System.err);
            throw new RuntimeException("KDF for L3Key2 reached limit");
          }
        L1Hash32 mac = new L1Hash32();
        mac.init(k1, k2, k31, k32);
        l1hash[i] = mac;
      }
  }

  public void update(byte b)
  {
    for (int i = 0; i < streams; i++)
      l1hash[i].update(b);
  }

  public void update(byte[] b, int offset, int len)
  {
    for (int i = 0; i < len; i++)
      this.update(b[offset + i]);
  }

  public byte[] digest()
  {
    byte[] result = new byte[UMac32.OUTPUT_LEN];
    for (int i = 0; i < streams; i++)
      {
        byte[] partialResult = l1hash[i].digest();
        System.arraycopy(partialResult, 0, result, 4 * i, 4);
      }
    reset();
    return result;
  }

  public void reset()
  {
    for (int i = 0; i < streams; i++)
      l1hash[i].reset();
  }

  public boolean selfTest()
  {
    return true;
  }

  /**
   * First hash stage of the UHash32 algorithm.
   */
  class L1Hash32
      implements Cloneable
  {
    private int[] key; // key material as an array of 32-bit ints
    private byte[] buffer; // work buffer L1_KEY_LEN long
    private int count; // meaningful bytes in buffer
    private ByteArrayOutputStream Y;
    private long totalCount;
    private L2Hash32 l2hash;
    private L3Hash32 l3hash;

    /** Trivial 0-arguments constructor. */
    L1Hash32()
    {
      super();

      key = new int[UMac32.L1_KEY_LEN / 4];
      buffer = new byte[UMac32.L1_KEY_LEN];
      count = 0;
      Y = new ByteArrayOutputStream();
      totalCount = 0L;
    }

    /**
     * Private constructor for cloning purposes.
     *
     * @param that the instance to clone.
     */
    private L1Hash32(L1Hash32 that)
    {
      this();

      System.arraycopy(that.key, 0, this.key, 0, that.key.length);
      System.arraycopy(that.buffer, 0, this.buffer, 0, that.count);
      this.count = that.count;
      byte[] otherY = that.Y.toByteArray();
      this.Y.write(otherY, 0, otherY.length);
      this.totalCount = that.totalCount;
      if (that.l2hash != null)
        this.l2hash = (L2Hash32) that.l2hash.clone();
      if (that.l3hash != null)
        this.l3hash = (L3Hash32) that.l3hash.clone();
    }

    public Object clone()
    {
      return new L1Hash32(this);
    }

    public void init(byte[] k1, byte[] k2, byte[] k31, byte[] k32)
    {
      for (int i = 0, j = 0; i < (UMac32.L1_KEY_LEN / 4); i++)
        key[i] =  k1[j++]         << 24
               | (k1[j++] & 0xFF) << 16
               | (k1[j++] & 0xFF) << 8
               | (k1[j++] & 0xFF);
      l2hash = new L2Hash32(k2);
      l3hash = new L3Hash32(k31, k32);
    }

    public void update(byte b)
    {
      // Break M into L1_KEY_LEN byte chunks (final chunk may be shorter)

      // Let M_1, M_2, ..., M_t be strings so that M = M_1 || M_2 || .. ||
      // M_t, and length(M_i) = L1_KEY_LEN for all 0 < i < t.

      // For each chunk, except the last: endian-adjust, NH hash
      // and add bit-length.  Use results to build Y.
      buffer[count] = b;
      count++;
      totalCount++;
      if (count >= UMac32.L1_KEY_LEN)
        {
          byte[] y = nh32(UMac32.L1_KEY_LEN);
          Y.write(y, 0, 8);

          count = 0;

          // For each iteration, extract key and three-layer hash.
          // If length(M) <= L1_KEY_LEN, then skip L2-HASH.
          if (Y.size() == 16) // we already hashed twice L1_KEY_LEN
            {
              byte[] A = Y.toByteArray();
              Y.reset();
              l2hash.update(A, 0, 16);
            }
        }
    }

    public byte[] digest()
    {
      // For the last chunk: pad to 32-byte boundary, endian-adjust,
      // NH hash and add bit-length.  Concatenate the result to Y.
      if (count != 0)
        {
          if (count % 32 != 0)
            {
              int limit = 32 * ((count + 31) / 32);
              System.arraycopy(ALL_ZEROES, 0, buffer, count, limit - count);
              count += limit - count;
            }
          byte[] y = nh32(count);
          Y.write(y, 0, 8);
        }
      byte[] A = Y.toByteArray();
      Y.reset();
      byte[] B;
      if (totalCount <= UMac32.L1_KEY_LEN)
        {
          // we might have 'update'd the bytes already. check
          if (A.length == 0) // we did
            B = l2hash.digest();
          else // did not
            {
              B = new byte[16];
              System.arraycopy(A, 0, B, 8, 8);
            }
        }
      else
        {
          if (A.length != 0)
            l2hash.update(A, 0, A.length);
          B = l2hash.digest();
        }
      byte[] result = l3hash.digest(B);
      reset();
      return result;
    }

    public void reset()
    {
      count = 0;
      Y.reset();
      totalCount = 0L;
      if (l2hash != null)
        l2hash.reset();
    }

    /**
     * 5.1  NH-32: NH hashing with a 32-bit word size.
     *
     * @param len count of bytes, divisible by 32, in buffer to process
     * @return Y, string of length 8 bytes.
     */
    private byte[] nh32(int len)
    {
      // Break M and K into 4-byte chunks
      int t = len / 4;
      // Let M_1, M_2, ..., M_t be 4-byte strings
      // so that M = M_1 || M_2 || .. || M_t.
      // Let K_1, K_2, ..., K_t be 4-byte strings
      // so that K_1 || K_2 || .. || K_t  is a prefix of K.
      int[] m = new int[t];
      int i;
      int j = 0;
      for (i = 0, j = 0; i < t; i++)
        m[i] =  buffer[j++]         << 24
             | (buffer[j++] & 0xFF) << 16
             | (buffer[j++] & 0xFF) << 8
             | (buffer[j++] & 0xFF);
      // Perform NH hash on the chunks, pairing words for multiplication
      // which are 4 apart to accommodate vector-parallelism.
      long result = len * 8L;
      for (i = 0; i < t; i += 8)
        {
          result += ((m[i + 0] + key[i + 0]) & 0xFFFFFFFFL)
                  * ((m[i + 4] + key[i + 4]) & 0xFFFFFFFFL);
          result += ((m[i + 1] + key[i + 1]) & 0xFFFFFFFFL)
                  * ((m[i + 5] + key[i + 5]) & 0xFFFFFFFFL);
          result += ((m[i + 2] + key[i + 2]) & 0xFFFFFFFFL)
                  * ((m[i + 6] + key[i + 6]) & 0xFFFFFFFFL);
          result += ((m[i + 3] + key[i + 3]) & 0xFFFFFFFFL)
                  * ((m[i + 7] + key[i + 7]) & 0xFFFFFFFFL);
        }
      return new byte[] {
          (byte)(result >>> 56), (byte)(result >>> 48),
          (byte)(result >>> 40), (byte)(result >>> 32),
          (byte)(result >>> 24), (byte)(result >>> 16),
          (byte)(result >>>  8), (byte) result };
    }
  }

  /**
   * Second hash stage of the UHash32 algorithm.
   * <p>
   * 5.4 L2-HASH-32: Second-layer hash.
   * <ul>
   * <li>Input:<br>
   * K string of length 24 bytes.<br>
   * M string of length less than 2^64 bytes.</li>
   * <li>Returns:<br>
   * Y, string of length 16 bytes.</li>
   * </ul>
   */
  class L2Hash32
      implements Cloneable
  {
    private BigInteger k64, k128;
    private BigInteger y;
    private boolean highBound;
    private long bytesSoFar;
    private ByteArrayOutputStream buffer;

    L2Hash32(byte[] K)
    {
      super();

      if (K.length != 24)
        throw new ExceptionInInitializerError("K length is not 24");
      //  Extract keys and restrict to special key-sets
      //         Mask64  = uint2str(0x01FFFFFF01FFFFFF, 8);
      //         Mask128 = uint2str(0x01FFFFFF01FFFFFF01FFFFFF01FFFFFF, 16);
      //         k64    = str2uint(K[1..8]  and Mask64);
      //         k128   = str2uint(K[9..24] and Mask128);
      int i = 0;
      k64 = new BigInteger(1, new byte[] {
          (byte)(K[i++] & 0x01), (byte)(K[i++] & 0xFF),
          (byte)(K[i++] & 0xFF), (byte)(K[i++] & 0xFF),
          (byte)(K[i++] & 0x01), (byte)(K[i++] & 0xFF),
          (byte)(K[i++] & 0xFF), (byte)(K[i++] & 0xFF) });
      k128 = new BigInteger(1, new byte[] {
          (byte)(K[i++] & 0x01), (byte)(K[i++] & 0xFF),
          (byte)(K[i++] & 0xFF), (byte)(K[i++] & 0xFF),
          (byte)(K[i++] & 0x01), (byte)(K[i++] & 0xFF),
          (byte)(K[i++] & 0xFF), (byte)(K[i++] & 0xFF),
          (byte)(K[i++] & 0x01), (byte)(K[i++] & 0xFF),
          (byte)(K[i++] & 0xFF), (byte)(K[i++] & 0xFF),
          (byte)(K[i++] & 0x01), (byte)(K[i++] & 0xFF),
          (byte)(K[i++] & 0xFF), (byte)(K[i++] & 0xFF) });
      y = BigInteger.ONE;
      highBound = false;
      bytesSoFar = 0L;
    }

    private L2Hash32(L2Hash32 that)
    {
      super();

      this.k64 = that.k64;
      this.k128 = that.k128;
      this.y = that.y;
      this.highBound = that.highBound;
      this.bytesSoFar = that.bytesSoFar;
      if (that.buffer != null)
        {
          byte[] thatbuffer = that.buffer.toByteArray();
          this.buffer = new ByteArrayOutputStream();
          this.buffer.write(thatbuffer, 0, thatbuffer.length);
        }
    }

    public Object clone()
    {
      return new L2Hash32(this);
    }

    // this is called with either 8-bytes or 16-bytes
    void update(byte[] b, int offset, int len)
    {
      if (len == 0)
        return;

      if (! highBound) // do the first (only?) 8-bytes
        {
          poly(64, LOWER_RANGE, k64, b, offset, 8);
          bytesSoFar += 8L;
          highBound = (bytesSoFar > BOUNDARY);
          if (highBound) // if we just crossed the limit then process y
            {
              poly(128, UPPER_RANGE, k128, yTo16bytes(), 0, 16);
              buffer = new ByteArrayOutputStream();
            }
          // do the rest if any
          update(b, offset + 8, len - 8);
        }
      else
        { // we're already beyond the 2**17 bytes size limit
          // process in chuncks of 16
          buffer.write(b, offset, len);
          if (buffer.size() > 16)
            {
              byte[] bb = buffer.toByteArray();
              poly(128, UPPER_RANGE, k128, bb, 0, 16);
              if (bb.length > 16)
                buffer.write(bb, 16, bb.length - 16);
            }
        }
    }

    byte[] digest()
    {
      // If M no more than 2^17 bytes, hash under 64-bit prime,
      // otherwise, hash first 2^17 bytes under 64-bit prime and
      // remainder under 128-bit prime.
      if (! highBound) // y is up-to-date
        {
          // do nothing
        }
      else // we may have some bytes in buffer
        {
          byte[] bb = buffer.toByteArray();
          byte[] lastBlock = new byte[16];
          System.arraycopy(bb, 0, lastBlock, 0, bb.length);
          lastBlock[bb.length] = (byte) 0x80;
          poly(128, UPPER_RANGE, k128, lastBlock, 0, 16);
        }
      byte[] result = yTo16bytes();
      reset();
      return result;
    }

    void reset()
    {
      y = BigInteger.ONE;
      highBound = false;
      bytesSoFar = 0L;
      if (buffer != null)
        buffer.reset();
    }

    private byte[] yTo16bytes()
    {
      byte[] yy = y.toByteArray();
      byte[] result = new byte[16];
      if (yy.length > 16)
        System.arraycopy(yy, yy.length - 16, result, 0, 16);
      else
        System.arraycopy(yy, 0, result, 16 - yy.length, yy.length);

      return result;
    }

    /**
     * 5.3 POLY: Polynomial hash Function Name: POLY
     * 
     * @param wordbits positive integer divisible by 8: called with 64 or 128.
     * @param maxwordrange positive integer less than 2**wordbits.
     * @param k integer in the range 0 .. prime(wordbits) - 1.
     * @param M string with length divisible by (wordbits / 8) bytes. return y,
     *          integer in the range 0 .. prime(wordbits) - 1.
     */
    private void poly(int wordbits, BigInteger maxwordrange, BigInteger k,
                      byte[] M, int off, int len)
    {
      byte[] mag = new byte[len];
      System.arraycopy(M, off, mag, 0, len);
      // Define constants used for fixing out-of-range words
      BigInteger p = prime(wordbits);
      BigInteger offset = TWO.pow(wordbits).subtract(p); // 2^wordbits - p;
      BigInteger marker = p.subtract(BigInteger.ONE);
      // Break M into chunks of length wordbytes bytes
      //         long n = M.length / wordbytes;
      // Let M_1, M_2, ..., M_n be strings of length wordbytes bytes
      // so that M = M_1 || M_2 || .. || M_n

      // For each input word, compare it with maxwordrange.  If larger
      // then hash the words 'marker' and (m - offset), both in range.
      //         for (int i = 0; i < n; i++) {
      BigInteger m = new BigInteger(1, mag);
      if (m.compareTo(maxwordrange) >= 0) // m >= maxwordrange
        {
          y = y.multiply(k).add(marker).mod(p); // (k * y + marker) % p;
          y = y.multiply(k).add(m.subtract(offset)).mod(p); // (k * y + (m - offset)) % p;
        }
      else
        y = y.multiply(k).add(m).mod(p); // (k * y + m) % p;
    }
  }

  /**
   * Third hash stage of the UHash32 algorithm.
   * <ul>
   * <li>Input:<br/>
   * K1 string of length 64 bytes.<br/>
   * K2 string of length 4 bytes.<br/>
   * M string of length 16 bytes.</li>
   * <li>Returns:<br/>
   * Y, string of length 4 bytes.</li>
   * </ul>
   */
  class L3Hash32
      implements Cloneable
  {
    private static final long PRIME_36 = 0x0000000FFFFFFFFBL;
    private int[] k = new int[9];

    /**
     * @param K1 string of length 64 bytes.
     * @param K2 string of length 4 bytes.
     */
    L3Hash32(byte[] K1, byte[] K2)
    {
      super();

      // pre-conditions
      if (K1.length != 64)
        throw new ExceptionInInitializerError("K1 length is not 64");
      if (K2.length != 4)
        throw new ExceptionInInitializerError("K2 length is not 4");
      // Break K1 into 8 chunks and convert to integers
      for (int i = 0, j = 0; i < 8; i++)
        {
          long kk = (K1[j++] & 0xFFL) << 56
                  | (K1[j++] & 0xFFL) << 48
                  | (K1[j++] & 0xFFL) << 40
                  | (K1[j++] & 0xFFL) << 32
                  | (K1[j++] & 0xFFL) << 24
                  | (K1[j++] & 0xFFL) << 16
                  | (K1[j++] & 0xFFL) <<  8
                  | (K1[j++] & 0xFFL);
          k[i] = (int)(kk % PRIME_36);
        }
      k[8] =  K2[0]         << 24
           | (K2[1] & 0xFF) << 16
           | (K2[2] & 0xFF) << 8
           | (K2[3] & 0xFF);
    }

    private L3Hash32(int[] k)
    {
      super();

      this.k = k;
    }

    public Object clone()
    {
      return new L3Hash32((int[]) k.clone());
    }

    /**
     * @param M string of length 16 bytes.
     * @return Y, string of length 4 bytes.
     */
    byte[] digest(byte[] M)
    {
      if (M.length != 16)
        throw new IllegalArgumentException("M length is not 16");

      long m, y = 0L;
      for (int i = 0, j = 0; i < 8; i++)
        {
          // Break M into 8 chunks and convert to integers
          m = (M[j++] & 0xFFL) << 8 | (M[j++] & 0xFFL);
          // Inner-product hash, extract last 32 bits and affine-translate
          //            y = (m_1 * k_1 + ... + m_8 * k_8) mod prime(36);
          //            y = y mod 2^32;
          y += (m * (k[i] & 0xFFFFFFFFL)) % PRIME_36;
        }
      int Y = ((int) y) ^ k[8];
      return new byte[] {
          (byte)(Y >>> 24),
          (byte)(Y >>> 16),
          (byte)(Y >>> 8),
          (byte) Y };
    }
  }
}
