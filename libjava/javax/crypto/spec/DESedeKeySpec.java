/* DESedeKeySpec.java -- Keys for triple-DES.
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
 * This class is a transparent wrapper for DES-EDE (Triple-DES) keys,
 * which are arrays of 24 bytes.
 *
 * @author Casey Marshall (csm@gnu.org)
 * @since 1.4
 */
public class DESedeKeySpec implements KeySpec
{

  // Constants.
  // ------------------------------------------------------------------------

  /**
   * The length of a triple-DES key, in bytes.
   */
  public static final int DES_EDE_KEY_LEN = 24;

  /**
   * The key bytes.
   */
  private byte[] key;

  // Constructors.
  // ------------------------------------------------------------------------

  /**
   * Create a new DES-EDE key spec, copying the first 24 bytes from the
   * byte array.
   *
   * @param key The key bytes.
   * @throws java.security.InvalidKeyException If there are less than 24
   *         bytes in the array.
   */
  public DESedeKeySpec(byte[] key) throws InvalidKeyException
  {
    this(key, 0);
  }

  /**
   * Create a new DES-EDE key spec, starting at <code>offset</code> in
   * the byte array. The first 24 bytes starting at <code>offset</code>
   * are copied.
   *
   * @param key    The key bytes.
   * @param offset The offset into the byte array at which to begin.
   * @throws java.security.InvalidKeyException If there are less than 24
   *         bytes starting at <code>offset</code>.
   */
  public DESedeKeySpec(byte[] key, int offset) throws InvalidKeyException
  {
    if (key.length - offset < DES_EDE_KEY_LEN)
      {
        throw new InvalidKeyException("DES-EDE keys must be 24 bytes long");
      }
    this.key = new byte[DES_EDE_KEY_LEN];
    System.arraycopy(key, offset, this.key, 0, DES_EDE_KEY_LEN);
  }

  // Class methods.
  // ------------------------------------------------------------------------

  /**
   * Returns whether or not the given key is <i>parity adjusted</i>;
   * i.e. every byte in the key has an odd number of "1" bits.
   *
   * @param key    The key bytes, considered between <code>[offset,
   *               offset+23]</code>
   * @param offset The offset into the byte array at which to begin.
   * @return True if all bytes have an odd number of "1" bits.
   * @throws java.security.InvalidKeyException If there are not enough
   *         bytes in the array.
   */
  public static boolean isParityAdjusted(byte[] key, int offset)
    throws InvalidKeyException
  {
    if (key.length - offset < DES_EDE_KEY_LEN)
      {
        throw new InvalidKeyException("DES-EDE keys must be 24 bytes long");
      }
    boolean parity = false;
    boolean oddbits = false;
    for (int i = 0; i < DES_EDE_KEY_LEN; i++)
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
