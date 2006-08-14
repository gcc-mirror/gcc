/* IMessageDigest.java -- 
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


package gnu.java.security.hash;

/**
 * The basic visible methods of any hash algorithm.
 * <p>
 * A hash (or message digest) algorithm produces its output by iterating a basic
 * compression function on blocks of data.
 */
public interface IMessageDigest
    extends Cloneable
{
  /**
   * Returns the canonical name of this algorithm.
   * 
   * @return the canonical name of this instance.
   */
  String name();

  /**
   * Returns the output length in bytes of this message digest algorithm.
   * 
   * @return the output length in bytes of this message digest algorithm.
   */
  int hashSize();

  /**
   * Returns the algorithm's (inner) block size in bytes.
   * 
   * @return the algorithm's inner block size in bytes.
   */
  int blockSize();

  /**
   * Continues a message digest operation using the input byte.
   * 
   * @param b the input byte to digest.
   */
  void update(byte b);

  /**
   * Continues a message digest operation, by filling the buffer, processing
   * data in the algorithm's HASH_SIZE-bit block(s), updating the context and
   * count, and buffering the remaining bytes in buffer for the next operation.
   * 
   * @param in the input block.
   */
  void update(byte[] in);

  /**
   * Continues a message digest operation, by filling the buffer, processing
   * data in the algorithm's HASH_SIZE-bit block(s), updating the context and
   * count, and buffering the remaining bytes in buffer for the next operation.
   * 
   * @param in the input block.
   * @param offset start of meaningful bytes in input block.
   * @param length number of bytes, in input block, to consider.
   */
  void update(byte[] in, int offset, int length);

  /**
   * Completes the message digest by performing final operations such as padding
   * and resetting the instance.
   * 
   * @return the array of bytes representing the hash value.
   */
  byte[] digest();

  /**
   * Resets the current context of this instance clearing any eventually cached
   * intermediary values.
   */
  void reset();

  /**
   * A basic test. Ensures that the digest of a pre-determined message is equal
   * to a known pre-computed value.
   * 
   * @return <code>true</code> if the implementation passes a basic self-test.
   *         Returns <code>false</code> otherwise.
   */
  boolean selfTest();

  /**
   * Returns a clone copy of this instance.
   * 
   * @return a clone copy of this instance.
   */
  Object clone();
}
