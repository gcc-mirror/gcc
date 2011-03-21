/* IPad.java --
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


package gnu.javax.crypto.pad;

import java.util.Map;

/**
 * The basic visible methods, and attribute names, of every padding algorithm.
 * <p>
 * Padding algorithms serve to <i>pad</i> and <i>unpad</i> byte arrays usually
 * as the last step in an <i>encryption</i> or respectively a <i>decryption</i>
 * operation. Their input buffers are usually those processed by instances of
 * {@link gnu.javax.crypto.mode.IMode} and/or
 * {@link gnu.javax.crypto.cipher.IBlockCipher}.
 */
public interface IPad
{
  /**
   * Property name of the block size in which to operate the padding algorithm.
   * The value associated with this property name is taken to be a positive
   * {@link Integer} greater than zero.
   */
  String PADDING_BLOCK_SIZE = "gnu.crypto.pad.block.size";

  /** @return the canonical name of this instance. */
  String name();

  /**
   * Initialises the padding scheme with a designated block size.
   *
   * @param bs the designated block size.
   * @exception IllegalStateException if the instance is already initialised.
   * @exception IllegalArgumentException if the block size value is invalid.
   */
  void init(int bs) throws IllegalStateException;

  /**
   * Initialises the algorithm with designated attributes. Names, valid and/or
   * recognisable by all concrete implementations are described in the class
   * documentation above. Other algorithm-specific attributes MUST be documented
   * in the implementation class of that padding algorithm.
   *
   * @param attributes a set of name-value pairs that describes the desired
   *          future behaviour of this instance.
   * @exception IllegalStateException if the instance is already initialised.
   * @exception IllegalArgumentException if the block size value is invalid.
   */
  void init(Map attributes) throws IllegalStateException;

  /**
   * Returns the byte sequence that should be appended to the designated input.
   *
   * @param in the input buffer containing the bytes to pad.
   * @param offset the starting index of meaningful data in <i>in</i>.
   * @param length the number of meaningful bytes in <i>in</i>.
   * @return the possibly 0-byte long sequence to be appended to the designated
   *         input.
   */
  byte[] pad(byte[] in, int offset, int length);

  /**
   * Returns the number of bytes to discard from a designated input buffer.
   *
   * @param in the input buffer containing the bytes to unpad.
   * @param offset the starting index of meaningful data in <i>in</i>.
   * @param length the number of meaningful bytes in <i>in</i>.
   * @return the number of bytes to discard, to the left of index position
   *         <code>offset + length</code> in <i>in</i>. In other words, if
   *         the return value of a successful invocation of this method is
   *         <code>result</code>, then the unpadded byte sequence will be
   *         <code>offset + length - result</code> bytes in <i>in</i>,
   *         starting from index position <code>offset</code>.
   * @exception WrongPaddingException if the data is not terminated with the
   *              expected padding bytes.
   */
  int unpad(byte[] in, int offset, int length) throws WrongPaddingException;

  /**
   * Resets the scheme instance for re-initialisation and use with other
   * characteristics. This method always succeeds.
   */
  void reset();

  /**
   * A basic symmetric pad/unpad test.
   *
   * @return <code>true</code> if the implementation passes a basic symmetric
   *         self-test. Returns <code>false</code> otherwise.
   */
  boolean selfTest();
}
