/* ICMGenerator.java -- 
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


package gnu.javax.crypto.prng;

import gnu.java.security.Registry;
import gnu.java.security.prng.BasePRNG;
import gnu.java.security.prng.LimitReachedException;
import gnu.javax.crypto.cipher.CipherFactory;
import gnu.javax.crypto.cipher.IBlockCipher;

import java.math.BigInteger;
import java.security.InvalidKeyException;
import java.util.HashMap;
import java.util.Map;

/**
 * Counter Mode is a way to define a pseudorandom keystream generator using a
 * block cipher. The keystream can be used for additive encryption, key
 * derivation, or any other application requiring pseudorandom data.
 * <p>
 * In ICM, the keystream is logically broken into segments. Each segment is
 * identified with a segment index, and the segments have equal lengths. This
 * segmentation makes ICM especially appropriate for securing packet-based
 * protocols.
 * <p>
 * This implementation adheres to the definition of the ICM keystream generation
 * function that allows for any symetric key block cipher algorithm
 * (initialisation parameter <code>gnu.crypto.prng.icm.cipher.name</code>
 * taken to be an instance of {@link java.lang.String}) to be used. If such a
 * parameter is not defined/included in the initialisation <code>Map</code>,
 * then the "Rijndael" algorithm is used. Furthermore, if the initialisation
 * parameter <code>gnu.crypto.cipher.block.size</code> (taken to be a instance
 * of {@link java.lang.Integer}) is missing or undefined in the initialisation
 * <code>Map</code>, then the cipher's <em>default</em> block size is used.
 * <p>
 * The practical limits and constraints of such generator are:
 * <ul>
 * <li>The number of blocks in any segment <b>MUST NOT</b> exceed <code>
 *    256 ** BLOCK_INDEX_LENGTH</code>.
 * The number of segments <b>MUST NOT</b> exceed
 * <code>256 ** SEGMENT_INDEX_LENGTH</code>. These restrictions ensure the
 * uniqueness of each block cipher input.</li>
 * <li>Each segment contains <code>SEGMENT_LENGTH</code> octets; this value
 * <b>MUST NOT</b> exceed the value <code>(256 ** BLOCK_INDEX_LENGTH) *
 *    BLOCK_LENGTH</code>.</li>
 * <li>The sum of <code>SEGMENT_INDEX_LENGTH</code> and
 * <code>BLOCK_INDEX_LENGTH</code> <b>MUST NOT</b> exceed <code>BLOCK_LENGTH
 *    / 2</code>.
 * This requirement protects the ICM keystream generator from potentially
 * failing to be pseudorandom.</li>
 * </ul>
 * <p>
 * <b>NOTE</b>: Rijndael is used as the default symmetric key block cipher
 * algorithm because, with its default block and key sizes, it is the AES. Yet
 * being Rijndael, the algorithm offers more versatile block and key sizes which
 * may prove to be useful for generating <em>longer</em> key streams.
 * <p>
 * References:
 * <ol>
 * <li><a
 * href="http://www.ietf.org/internet-drafts/draft-mcgrew-saag-icm-00.txt">
 * Integer Counter Mode</a>, David A. McGrew.</li>
 * </ol>
 */
public class ICMGenerator
    extends BasePRNG
    implements Cloneable
{
  /** Property name of underlying block cipher for this ICM generator. */
  public static final String CIPHER = "gnu.crypto.prng.icm.cipher.name";
  /** Property name of ICM's block index length. */
  public static final String BLOCK_INDEX_LENGTH =
      "gnu.crypto.prng.icm.block.index.length";
  /** Property name of ICM's segment index length. */
  public static final String SEGMENT_INDEX_LENGTH =
      "gnu.crypto.prng.icm.segment.index.length";
  /** Property name of ICM's offset. */
  public static final String OFFSET = "gnu.crypto.prng.icm.offset";
  /** Property name of ICM's segment index. */
  public static final String SEGMENT_INDEX = "gnu.crypto.prng.icm.segment.index";
  /** The integer value 256 as a BigInteger. */
  private static final BigInteger TWO_FIFTY_SIX = new BigInteger("256");
  /** The underlying cipher implementation. */
  private IBlockCipher cipher;
  /** This keystream block index length in bytes. */
  private int blockNdxLength = -1;
  /** This keystream segment index length in bytes. */
  private int segmentNdxLength = -1;
  /** The index of the next block for a given keystream segment. */
  private BigInteger blockNdx = BigInteger.ZERO;
  /** The segment index for this keystream. */
  private BigInteger segmentNdx;
  /** The initial counter for a given keystream segment. */
  private BigInteger C0;

  /** Trivial 0-arguments constructor. */
  public ICMGenerator()
  {
    super(Registry.ICM_PRNG);
  }

  // Conceptually, ICM is a keystream generator that takes a secret key and a
  // segment index as an input and then outputs a keystream segment. The
  // segmentation lends itself to packet encryption, as each keystream segment
  // can be used to encrypt a distinct packet.
  //
  // An ICM key consists of the block cipher key and an Offset. The Offset is
  // an integer with BLOCK_LENGTH octets...
  public void setup(Map attributes)
  {
    // find out which cipher algorithm to use
    boolean newCipher = true;
    String underlyingCipher = (String) attributes.get(CIPHER);
    if (underlyingCipher == null)
      if (cipher == null) // happy birthday
        // ensure we have a reliable implementation of this cipher
        cipher = CipherFactory.getInstance(Registry.RIJNDAEL_CIPHER);
      else
        // we already have one. use it as is
        newCipher = false;
    else // ensure we have a reliable implementation of this cipher
      cipher = CipherFactory.getInstance(underlyingCipher);

    // find out what block size we should use it in
    int cipherBlockSize = 0;
    Integer bs = (Integer) attributes.get(IBlockCipher.CIPHER_BLOCK_SIZE);
    if (bs != null)
      cipherBlockSize = bs.intValue();
    else
      {
        if (newCipher) // assume we'll use its default block size
          cipherBlockSize = cipher.defaultBlockSize();
        // else use as is
      }
    // get the key material
    byte[] key = (byte[]) attributes.get(IBlockCipher.KEY_MATERIAL);
    if (key == null)
      throw new IllegalArgumentException(IBlockCipher.KEY_MATERIAL);
    // now initialise the cipher
    HashMap map = new HashMap();
    if (cipherBlockSize != 0) // only needed if new or changed
      map.put(IBlockCipher.CIPHER_BLOCK_SIZE, Integer.valueOf(cipherBlockSize));
    map.put(IBlockCipher.KEY_MATERIAL, key);
    try
      {
        cipher.init(map);
      }
    catch (InvalidKeyException x)
      {
        throw new IllegalArgumentException(IBlockCipher.KEY_MATERIAL);
      }
    // at this point we have an initialised (new or otherwise) cipher
    // ensure that remaining params make sense
    cipherBlockSize = cipher.currentBlockSize();
    BigInteger counterRange = TWO_FIFTY_SIX.pow(cipherBlockSize);
    // offset, like the underlying cipher key is not cloneable
    // always look for it and throw an exception if it's not there
    Object obj = attributes.get(OFFSET);
    // allow either a byte[] or a BigInteger
    BigInteger r;
    if (obj instanceof BigInteger)
      r = (BigInteger) obj;
    else // assume byte[]. should be same length as cipher block size
      {
        byte[] offset = (byte[]) obj;
        if (offset.length != cipherBlockSize)
          throw new IllegalArgumentException(OFFSET);
        r = new BigInteger(1, offset);
      }
    int wantBlockNdxLength = -1; // number of octets in the block index
    Integer i = (Integer) attributes.get(BLOCK_INDEX_LENGTH);
    if (i != null)
      {
        wantBlockNdxLength = i.intValue();
        if (wantBlockNdxLength < 1)
          throw new IllegalArgumentException(BLOCK_INDEX_LENGTH);
      }
    int wantSegmentNdxLength = -1; // number of octets in the segment index
    i = (Integer) attributes.get(SEGMENT_INDEX_LENGTH);
    if (i != null)
      {
        wantSegmentNdxLength = i.intValue();
        if (wantSegmentNdxLength < 1)
          throw new IllegalArgumentException(SEGMENT_INDEX_LENGTH);
      }
    // if both are undefined check if it's a reuse
    if ((wantBlockNdxLength == -1) && (wantSegmentNdxLength == -1))
      {
        if (blockNdxLength == -1) // new instance
          throw new IllegalArgumentException(BLOCK_INDEX_LENGTH + ", "
                                             + SEGMENT_INDEX_LENGTH);
        // else reuse old values
      }
    else // only one is undefined, set it to BLOCK_LENGTH/2 minus the other
      {
        int limit = cipherBlockSize / 2;
        if (wantBlockNdxLength == -1)
          wantBlockNdxLength = limit - wantSegmentNdxLength;
        else if (wantSegmentNdxLength == -1)
          wantSegmentNdxLength = limit - wantBlockNdxLength;
        else if ((wantSegmentNdxLength + wantBlockNdxLength) > limit)
          throw new IllegalArgumentException(BLOCK_INDEX_LENGTH + ", "
                                             + SEGMENT_INDEX_LENGTH);
        // save new values
        blockNdxLength = wantBlockNdxLength;
        segmentNdxLength = wantSegmentNdxLength;
      }
    // get the segment index as a BigInteger
    BigInteger s = (BigInteger) attributes.get(SEGMENT_INDEX);
    if (s == null)
      {
        if (segmentNdx == null) // segment index was never set
          throw new IllegalArgumentException(SEGMENT_INDEX);
        // reuse; check if still valid
        if (segmentNdx.compareTo(TWO_FIFTY_SIX.pow(segmentNdxLength)) > 0)
          throw new IllegalArgumentException(SEGMENT_INDEX);
      }
    else
      {
        if (s.compareTo(TWO_FIFTY_SIX.pow(segmentNdxLength)) > 0)
          throw new IllegalArgumentException(SEGMENT_INDEX);
        segmentNdx = s;
      }
    // The initial counter of the keystream segment with segment index s is
    // defined as follows, where r denotes the Offset:
    //
    // C[0] = (s * (256^BLOCK_INDEX_LENGTH) + r) modulo (256^BLOCK_LENGTH)
    C0 = segmentNdx.multiply(TWO_FIFTY_SIX.pow(blockNdxLength))
                   .add(r).modPow(BigInteger.ONE, counterRange);
  }

  public void fillBlock() throws LimitReachedException
  {
    if (C0 == null)
      throw new IllegalStateException();
    if (blockNdx.compareTo(TWO_FIFTY_SIX.pow(blockNdxLength)) >= 0)
      throw new LimitReachedException();
    int cipherBlockSize = cipher.currentBlockSize();
    BigInteger counterRange = TWO_FIFTY_SIX.pow(cipherBlockSize);
    // encrypt the counter for the current blockNdx
    // C[i] = (C[0] + i) modulo (256^BLOCK_LENGTH).
    BigInteger Ci = C0.add(blockNdx).modPow(BigInteger.ONE, counterRange);
    buffer = Ci.toByteArray();
    int limit = buffer.length;
    if (limit < cipherBlockSize)
      {
        byte[] data = new byte[cipherBlockSize];
        System.arraycopy(buffer, 0, data, cipherBlockSize - limit, limit);
        buffer = data;
      }
    else if (limit > cipherBlockSize)
      {
        byte[] data = new byte[cipherBlockSize];
        System.arraycopy(buffer, limit - cipherBlockSize, data, 0,
                         cipherBlockSize);
        buffer = data;
      }
    cipher.encryptBlock(buffer, 0, buffer, 0);
    blockNdx = blockNdx.add(BigInteger.ONE); // increment blockNdx
  }
}
