/* DESKeySpec -- Keys for DES.
   Copyright (C) 2004  Free Software Foundation, Inc.

This file is part of GNU Classpath.

GNU Classpath is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2, or (at your option)
any later version.

GNU Classpath is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
General Public License for more details.

You should have received a copy of the GNU General Public License
along with GNU Classpath; see the file COPYING.  If not, write to the
Free Software Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA
02111-1307 USA.

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
exception statement from your version. */


package javax.crypto.spec;

import java.security.InvalidKeyException;
import java.security.spec.KeySpec;

/**
 * This class is a transparent wrapper for DES keys, which are arrays
 * of 8 bytes.
 *
 * @author Casey Marshall (csm@gnu.org)
 * @since 1.4
 */
public class DESKeySpec implements KeySpec
{

  // Constants.
  // ------------------------------------------------------------------------

  /**
   * The length of a DES key, in bytes.
   */
  public static final int DES_KEY_LEN = 8;

  /**
   * The key bytes.
   */
  private byte[] key;

  // Constructors.
  // ------------------------------------------------------------------------

  /**
   * Create a new DES key spec, copying the first 8 bytes from the
   * byte array.
   *
   * @param key The key bytes.
   * @throws java.security.InvalidKeyException If there are less than 8
   *         bytes in the array.
   */
  public DESKeySpec(byte[] key) throws InvalidKeyException
  {
    this(key, 0);
  }

  /**
   * Create a new DES key spec, starting at <code>offset</code> in
   * the byte array. The first 8 bytes starting at <code>offset</code>
   * are copied.
   *
   * @param key    The key bytes.
   * @param offset The offset into the byte array at which to begin.
   * @throws java.security.InvalidKeyException If there are less than 8
   *         bytes starting at <code>offset</code>.
   */
  public DESKeySpec(byte[] key, int offset) throws InvalidKeyException
  {
    if (key.length - offset < DES_KEY_LEN)
      {
        throw new InvalidKeyException("DES keys must be 8 bytes long");
      }
    this.key = new byte[DES_KEY_LEN];
    System.arraycopy(key, offset, this.key, 0, DES_KEY_LEN);
  }

  // Class methods.
  // ------------------------------------------------------------------------

  /**
   * Returns whether or not the given key is <i>parity adjusted</i>;
   * i.e. every byte in the key has an odd number of "1" bits.
   *
   * @param key    The key bytes, considered between <code>[offset,
   *               offset+7]</code>
   * @param offset The offset into the byte array at which to begin.
   * @return True if all bytes have an odd number of "1" bits.
   * @throws java.security.InvalidKeyException If there are not enough
   *         bytes in the array.
   */
  public static boolean isParityAdjusted(byte[] key, int offset)
    throws InvalidKeyException
  {
    if (key.length - offset < DES_KEY_LEN)
      {
        throw new InvalidKeyException("DES keys must be 8 bytes long");
      }
    boolean parity = false;
    boolean oddbits = false;
    for (int i = 0; i < DES_KEY_LEN; i++)
      {
        oddbits = false;
        for (int j = 0; j < 8; j++)
          {
            oddbits ^= (key[i+offset] & 1 << j) != 0;
          }
        parity &= oddbits;
      }
    return parity;
  }

  /**
   * One-half of the weak and semiweak DES keys (the other half are the
   * complements of these).
   */
  private static final byte[][] WEAK_KEYS = new byte[][] {
    {   0,   0,   0,   0,   0,   0,   0,   0 }, // 0000 0000 0000 0000
    {  -1,  -1,  -1,  -1,   0,   0,   0,   0 }, // ffff ffff 0000 0000
    {   1,   1,   1,   1,   1,   1,   1,   1 }, // 0101 0101 0101 0101
    {  31,  31,  31,  31,  14,  14,  14,  14 }, // 1f1f 1f1f 0e0e 0e0e
    {   1,  -2,   1,  -2,   1,  -2,   1,  -2 }, // 01fe 01fe 01fe 01fe
    {  31, -32,  31, -32, -32,  31, -32,  31 }, // 1fe0 1fe0 0e1f 0e1f
    {   1, -32,   1, -32,   1, -15,   1, -15 }, // 01e0 01e0 01f1 01f1
    {  31,  -2,  31,  -2,  14,  -2,  14,  -2 }, // 1ffe 1ffe 0efe 0efe
    {   1,  31,   1,  31,   1,  14,   1,  14 }, // 011f 011f 010e 010e
    { -32,  -2, -32,  -2, -15,  -2, -15,  -2 }, // e0fe e0fe f1fe f1fe
  };

  /**
   * Tests if the bytes between <code>[offset, offset+7]</code>
   * constitute a weak or semi-weak DES key.
   *
   * @param key    The key bytes to check.
   * @param offset The offset in the byte array to start.
   * @return true If the key bytes are a weak key.
   */
  public static boolean isWeak(byte[] key, int offset)
    throws InvalidKeyException
  {
    if (key.length - offset < DES_KEY_LEN)
      {
        throw new InvalidKeyException("DES keys must be 8 bytes long");
      }
    for (int i = 0; i < WEAK_KEYS.length; i++)
      {
        if (equalsOrComplementEquals(key, offset, WEAK_KEYS[i]))
          {
            return true;
          }
      }
    return false;
  }

  /**
   * This method returns true if the first 8 bytes starting at
   * <code>off</code> in <code>a</code> equal the first 8 bytes in
   * <code>b</code>, or equal the <i>complement</i> of the first 8 bytes
   * in <code>b</code>.
   *
   * @param a   The first byte array.
   * @param off The index into the first byte array.
   * @param b   The second byte array.
   * @return <code>a == b || a == ~b</code>
   */
  private static boolean equalsOrComplementEquals(byte[] a, int off, byte[] b)
  {
    boolean result = true;
    for (int i = 0; i < DES_KEY_LEN; i++)
      {
        result &= a[off+i] == b[i];
      }
    if (result) return true;
    result = true;
    for (int i = 0; i < DES_KEY_LEN; i++)
      {
        result &= a[off+i] == (~b[i]);
      }
    return result;
  }

  // Instance methods.
  // ------------------------------------------------------------------------

  /**
   * Return the key as a byte array. This method does not copy the byte
   * array.
   *
   * @return The key bytes.
   */
  public byte[] getKey()
  {
    return key;
  }
}
