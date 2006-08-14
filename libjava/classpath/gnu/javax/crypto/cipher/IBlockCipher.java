/* IBlockCipher.java -- 
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


package gnu.javax.crypto.cipher;

import java.security.InvalidKeyException;
import java.util.Iterator;
import java.util.Map;

/**
 * The basic visible methods of any symmetric key block cipher.
 * <p>
 * A symmetric key block cipher is a function that maps n-bit plaintext blocks
 * to n-bit ciphertext blocks; n being the cipher's <i>block size</i>. This
 * encryption function is parameterised by a k-bit key, and is invertible. Its
 * inverse is the decryption function.
 * <p>
 * Possible initialisation values for an instance of this type are:
 * <ul>
 * <li>The block size in which to operate this block cipher instance. This
 * value is <b>optional</b>, if unspecified, the block cipher's default block
 * size shall be used.</li>
 * <li>The byte array containing the user supplied key material to use for
 * generating the cipher's session key(s). This value is <b>mandatory</b> and
 * should be included in the initialisation parameters. If it isn't, an
 * {@link IllegalStateException} will be thrown if any method, other than
 * <code>reset()</code> is invoked on the instance. Furthermore, the size of
 * this key material shall be taken as an indication on the key size in which to
 * operate this instance.</li>
 * </ul>
 * <p>
 * <b>IMPLEMENTATION NOTE</b>: Although all the concrete classes in this
 * package implement the {@link Cloneable} interface, it is important to note
 * here that such an operation <b>DOES NOT</b> clone any session key material
 * that may have been used in initialising the source cipher (the instance to be
 * cloned). Instead a clone of an already initialised cipher is another instance
 * that operates with the <b>same block size</b> but without any knowledge of
 * neither key material nor key size.
 */
public interface IBlockCipher
    extends Cloneable
{
  /**
   * Property name of the block size in which to operate a block cipher. The
   * value associated with this property name is taken to be an {@link Integer}.
   */
  String CIPHER_BLOCK_SIZE = "gnu.crypto.cipher.block.size";
  /**
   * Property name of the user-supplied key material. The value associated to
   * this property name is taken to be a byte array.
   */
  String KEY_MATERIAL = "gnu.crypto.cipher.key.material";

  /**
   * Returns the canonical name of this instance.
   * 
   * @return the canonical name of this instance.
   */
  String name();

  /**
   * Returns the default value, in bytes, of the algorithm's block size.
   * 
   * @return the default value, in bytes, of the algorithm's block size.
   */
  int defaultBlockSize();

  /**
   * Returns the default value, in bytes, of the algorithm's key size.
   * 
   * @return the default value, in bytes, of the algorithm's key size.
   */
  int defaultKeySize();

  /**
   * Returns an {@link Iterator} over the supported block sizes. Each element
   * returned by this object is an {@link Integer}.
   * 
   * @return an {@link Iterator} over the supported block sizes.
   */
  Iterator blockSizes();

  /**
   * Returns an {@link Iterator} over the supported key sizes. Each element
   * returned by this object is an {@link Integer}.
   * 
   * @return an {@link Iterator} over the supported key sizes.
   */
  Iterator keySizes();

  /**
   * Returns a clone of this instance.
   * 
   * @return a clone copy of this instance.
   */
  Object clone();

  /**
   * Initialises the algorithm with designated attributes. Permissible names and
   * values are described in the class documentation above.
   * 
   * @param attributes a set of name-value pairs that describes the desired
   *          future behaviour of this instance.
   * @exception InvalidKeyException if the key data is invalid.
   * @exception IllegalStateException if the instance is already initialised.
   * @see #KEY_MATERIAL
   * @see #CIPHER_BLOCK_SIZE
   */
  void init(Map attributes) throws InvalidKeyException, IllegalStateException;

  /**
   * Returns the currently set block size for this instance.
   * 
   * @return the current block size for this instance.
   * @exception IllegalStateException if the instance is not initialised.
   */
  int currentBlockSize() throws IllegalStateException;

  /**
   * Resets the algorithm instance for re-initialisation and use with other
   * characteristics. This method always succeeds.
   */
  void reset();

  /**
   * Encrypts exactly one block of plaintext.
   * 
   * @param in the plaintext.
   * @param inOffset index of <code>in</code> from which to start considering
   *          data.
   * @param out the ciphertext.
   * @param outOffset index of <code>out</code> from which to store result.
   * @exception IllegalStateException if the instance is not initialised.
   */
  void encryptBlock(byte[] in, int inOffset, byte[] out, int outOffset)
      throws IllegalStateException;

  /**
   * Decrypts exactly one block of ciphertext.
   * 
   * @param in the plaintext.
   * @param inOffset index of <code>in</code> from which to start considering
   *          data.
   * @param out the ciphertext.
   * @param outOffset index of <code>out</code> from which to store result.
   * @exception IllegalStateException if the instance is not initialised.
   */
  void decryptBlock(byte[] in, int inOffset, byte[] out, int outOffset)
      throws IllegalStateException;

  /**
   * A <i>correctness</i> test that consists of basic symmetric encryption /
   * decryption test(s) for all supported block and key sizes, as well as one
   * (1) variable key Known Answer Test (KAT).
   * 
   * @return <code>true</code> if the implementation passes simple
   *         <i>correctness</i> tests. Returns <code>false</code> otherwise.
   */
  boolean selfTest();
}
