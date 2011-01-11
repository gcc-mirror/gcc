/* BlockCipherParameterSpec.java --
   Copyright (C) 2002, 2006  Free Software Foundation, Inc.

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


package gnu.javax.crypto.jce.spec;

import gnu.java.security.util.Util;

import java.security.spec.AlgorithmParameterSpec;

/**
 * Block cipher parameters in GNU are the cipher's name, its block and key
 * sizes, and an optional initialization vector.
 */
public class BlockCipherParameterSpec
    implements AlgorithmParameterSpec
{
  /** The initialization vector. */
  protected byte[] iv;
  /** The cipher's block size, in bytes. */
  protected int blockSize;
  /** The cipher's key size, in bytes. */
  protected int keySize;

  /**
   * Create a new parameter specification.
   *
   * @param iv The initialization vector, or <code>null</code> if there is no
   *          IV.
   * @param blockSize The cipher's block size, in bytes.
   * @param keySize The cipher's key size, in bytes.
   */
  public BlockCipherParameterSpec(byte[] iv, int blockSize, int keySize)
  {
    this.iv = (iv != null) ? (byte[]) iv.clone() : null;
    this.blockSize = blockSize;
    this.keySize = keySize;
  }

  /**
   * Create a new parameter specification with no IV.
   *
   * @param blockSize The cipher's block size, in bytes.
   * @param keySize The cipher's key size, in bytes.
   */
  public BlockCipherParameterSpec(int blockSize, int keySize)
  {
    this(null, blockSize, keySize);
  }

  /**
   * Get the initialization vector for the cipher, or <code>null</code> if
   * there is no IV.
   *
   * @return The IV.
   */
  public byte[] getIV()
  {
    return iv;
  }

  /**
   * Get the block size of the cipher these parameters are for.
   *
   * @return The block size.
   */
  public int getBlockSize()
  {
    return blockSize;
  }

  /**
   * Get the key size of the cipher these parameters are for.
   *
   * @return The block size.
   */
  public int getKeySize()
  {
    return keySize;
  }

  public String toString()
  {
    return getClass().getName() + " { "
           + ((iv != null) ? ("IV=" + Util.toString(iv)) + ", " : "")
           + "BS=" + blockSize + ", KS=" + keySize + " }";
  }
}
