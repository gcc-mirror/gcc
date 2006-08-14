/* IKeyWrappingAlgorithm.java -- FIXME: briefly describe file purpose
   Copyright (C) 2006 Free Software Foundation, Inc.

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
Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
02110-1301 USA.

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


package gnu.javax.crypto.kwa;

import java.security.InvalidKeyException;
import java.security.SecureRandom;
import java.util.Map;

import javax.crypto.ShortBufferException;

/**
 * Constants and visible methods available to all GNU Key Wrapping Algorithm
 * implementations.
 */
public interface IKeyWrappingAlgorithm
{
  /**
   * Name of the property, in the attributes map, that references the Key
   * Wrapping Algorithm KEK (Key Encryption Key) material. The object referenced
   * by this property is a byte array containing the keying material for the
   * underlying block cipher.
   */
  String KEY_ENCRYPTION_KEY_MATERIAL = "gnu.crypto.kwa.kek";
  /**
   * Name of the property, in the attributes map, that references the Initial
   * Value (IV) material. The object referenced by this property is a byte array
   * containing the initial integrity check register value.
   */
  String INITIAL_VALUE = "gnu.crypto.kwa.iv";
  /**
   * Property name of an optional {@link SecureRandom} instance to use. The
   * default is to use a {@link gnu.java.security.util.PRNG} instance.
   */
  String SOURCE_OF_RANDOMNESS = "gnu.crypto.kwa.prng";

  /**
   * Returns the canonical name of this Key Wrapping Algorithm.
   * 
   * @return the canonical name of this Key Wrapping Algorithm.
   */
  String name();

  /**
   * Initializes this instance with the designated algorithm specific
   * attributes.
   * 
   * @param attributes a map of name-to-value pairs the Key Wrapping Algorithm
   *          must use for its setup.
   * @throws InvalidKeyException if an exception is encountered while seting up
   *           the Key Wrapping Algorithm keying material (KEK).
   */
  void init(Map attributes) throws InvalidKeyException;

  /**
   * Wraps the designated plain text bytes.
   * 
   * @param in the input byte array containing the plain text.
   * @param inOffset the offset into <code>in</code> where the first byte of
   *          the plain text (key material) to wrap is located.
   * @param length the number of bytes to wrap.
   * @param out the output byte array where the wrapped key material will be
   *          stored.
   * @param outOffset the offset into <code>out</code> of the first wrapped
   *          byte.
   * @return the number of bytes of the wrapped key material; i.e. the length,
   *         in <code>out</code>, starting from <code>outOffset</code>
   *         where the cipher text (wrapped key material) are stored.
   * @throws ShortBufferException if the output buffer is not long enough to
   *           accomodate the number of bytes resulting from wrapping the plain
   *           text.
   */
  int wrap(byte[] in, int inOffset, int length, byte[] out, int outOffset)
      throws ShortBufferException;

  /**
   * Wraps the designated plain text bytes.
   * 
   * @param in the input byte array containing the plain text.
   * @param inOffset the offset into <code>in</code> where the first byte of
   *          the plain text (key material) to wrap is located.
   * @param length the number of bytes to wrap.
   * @return a newly allocated byte array containing the cipher text.
   */
  byte[] wrap(byte[] in, int inOffset, int length);

  /**
   * Unwraps the designated cipher text bytes.
   * 
   * @param in the input byte array containing the cipher text.
   * @param inOffset the offset into <code>in</code> where the first byte of
   *          the cipher text (already wrapped key material) to unwrap is
   *          located.
   * @param length the number of bytes to unwrap.
   * @param out the output byte array where the unwrapped key material will be
   *          stored.
   * @param outOffset the offset into <code>out</code> of the first unwrapped
   *          byte.
   * @return the number of bytes of the unwrapped key material; i.e. the length,
   *         in <code>out</code>, starting from <code>outOffset</code>
   *         where the plain text (unwrapped key material) are stored.
   * @throws ShortBufferException if the output buffer is not long enough to
   *           accomodate the number of bytes resulting from unwrapping the
   *           cipher text.
   * @throws KeyUnwrappingException if after unwrapping the cipher text, the
   *           bytes at the begining did not match the initial value.
   */
  int unwrap(byte[] in, int inOffset, int length, byte[] out, int outOffset)
      throws ShortBufferException, KeyUnwrappingException;

  /**
   * Unwraps the designated cipher text bytes.
   * 
   * @param in the input byte array containing the cipher text.
   * @param inOffset the offset into <code>in</code> where the first byte of
   *          the cipher text (already wrapped key material) to unwrap is
   *          located.
   * @param length the number of bytes to unwrap.
   * @return a newly allocated byte array containing the plain text.
   * @throws KeyUnwrappingException if after unwrapping the cipher text, the
   *           bytes at the begining did not match the initial value.
   */
  byte[] unwrap(byte[] in, int inOffset, int length)
      throws KeyUnwrappingException;
}
