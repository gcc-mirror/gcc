/* ISignature.java --
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


package gnu.java.security.sig;

import java.util.Map;

/**
 * The visible methods of every signature-with-appendix scheme.
 * <p>
 * The Handbook of Applied Cryptography (HAC), by A. Menezes &amp; al. states:
 * "Digital signature schemes which require the message as input to the
 * verification algorithm are called <i>digital signature schemes with appendix</i>.
 * ... They rely on cryptographic hash functions rather than customised
 * redundancy functions, and are less prone to existential forgery attacks."
 * <p>
 * References:
 * <ol>
 * <li><a href="http://www.cacr.math.uwaterloo.ca/hac/">Handbook of Applied
 * Cryptography</a>, Alfred J. Menezes, Paul C. van Oorschot and Scott A.
 * Vanstone. Section 11.2.2 Digital signature schemes with appendix.</li>
 * </ol>
 */
public interface ISignature
    extends Cloneable
{
  /** Property name of the verifier's public key. */
  public static final String VERIFIER_KEY = "gnu.crypto.sig.public.key";

  /** Property name of the signer's private key. */
  public static final String SIGNER_KEY = "gnu.crypto.sig.private.key";

  /**
   * Property name of an optional {@link java.security.SecureRandom},
   * {@link java.util.Random}, or {@link gnu.java.security.prng.IRandom}
   * instance to use. The default is to use a classloader singleton from
   * {@link gnu.java.security.util.PRNG}.
   */
  public static final String SOURCE_OF_RANDOMNESS = "gnu.crypto.sig.prng";

  /**
   * Returns the canonical name of this signature scheme.
   *
   * @return the canonical name of this instance.
   */
  String name();

  /**
   * Initialises this instance for signature verification.
   *
   * @param attributes the attributes to use for setting up this instance.
   * @throws IllegalArgumentException if the designated public key is not
   *           appropriate for this signature scheme.
   * @see #SOURCE_OF_RANDOMNESS
   * @see #VERIFIER_KEY
   */
  void setupVerify(Map attributes) throws IllegalArgumentException;

  /**
   * Initialises this instance for signature generation.
   *
   * @param attributes the attributes to use for setting up this instance.
   * @throws IllegalArgumentException if the designated private key is not
   *           appropriate for this signature scheme.
   * @see #SOURCE_OF_RANDOMNESS
   * @see #SIGNER_KEY
   */
  void setupSign(Map attributes) throws IllegalArgumentException;

  /**
   * Digests one byte of a message for signing or verification purposes.
   *
   * @param b the message byte to digest.
   * @throws IllegalStateException if this instance was not setup for signature
   *           generation/verification.
   */
  void update(byte b) throws IllegalStateException;

  /**
   * Digests a sequence of bytes from a message for signing or verification
   * purposes.
   *
   * @param buffer the byte sequence to consider.
   * @param offset the byte poisition in <code>buffer</code> of the first byte
   *          to consider.
   * @param length the number of bytes in <code>buffer</code> starting from
   *          the byte at index <code>offset</code> to digest.
   * @throws IllegalStateException if this instance was not setup for signature
   *           generation/verification.
   */
  void update(byte[] buffer, int offset, int length)
      throws IllegalStateException;

  /**
   * Terminates a signature generation phase by digesting and processing the
   * context of the underlying message digest algorithm instance.
   *
   * @return a {@link Object} representing the native output of the signature
   *         scheme implementation.
   * @throws IllegalStateException if this instance was not setup for signature
   *           generation.
   */
  Object sign() throws IllegalStateException;

  /**
   * Terminates a signature verification phase by digesting and processing the
   * context of the underlying message digest algorithm instance.
   *
   * @param signature a native signature object previously generated by an
   *          invocation of the <code>sign()</code> method.
   * @return <code>true</code> iff the outpout of the verification phase
   *         confirms that the designated signature object has been generated
   *         using the corresponding public key of the recepient.
   * @throws IllegalStateException if this instance was not setup for signature
   *           verification.
   */
  boolean verify(Object signature) throws IllegalStateException;

  /**
   * Returns a clone copy of this instance.
   *
   * @return a clone copy of this instance.
   */
  Object clone();
}
