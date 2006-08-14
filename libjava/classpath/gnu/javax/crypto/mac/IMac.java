/* IMac.java -- 
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


package gnu.javax.crypto.mac;

import java.security.InvalidKeyException;
import java.util.Map;

/**
 * The basic visible methods of any MAC (Message Authentication Code) algorithm.
 * <p>
 * A <i>MAC</i> provides a way to check the integrity of information
 * transmitted over, or stored in, an unreliable medium, based on a secret key.
 * Typically, <i>MAC</i>s are used between two parties, that share a common
 * secret key, in order to validate information transmitted between them.
 * <p>
 * When a <i>MAC</i> algorithm is based on a cryptographic hash function, it is
 * then called to a <i>HMAC</i> (Hashed Message Authentication Code) --see <a
 * href="http://www.ietf.org/rfc/rfc-2104.txt">RFC-2104</a>.
 * <p>
 * Another type of <i>MAC</i> algorithms exist: UMAC or <i>Universal Message
 * Authentication Code</i>, described in <a
 * href="http://www.ietf.org/internet-drafts/draft-krovetz-umac-01.txt">
 * draft-krovetz-umac-01.txt</a>.
 * <p>
 * With <i>UMAC</i>s, the sender and receiver share a common secret key (the
 * <i>MAC</i> key) which determines:
 * <ul>
 * <li>The key for a <i>universal hash function</i>. This hash function is
 * <i>non-cryptographic</i>, in the sense that it does not need to have any
 * cryptographic <i>hardness</i> property. Rather, it needs to satisfy some
 * combinatorial property, which can be proven to hold without relying on
 * unproven hardness assumptions.</li>
 * <li>The key for a <i>pseudorandom function</i>. This is where one needs a
 * cryptographic hardness assumption. The pseudorandom function may be obtained
 * from a <i>block cipher</i> or a <i>cryptographic hash function</i>. </li>
 * </ul>
 * <p>
 * References:
 * <ol>
 * <li><a href="http://www.ietf.org/rfc/rfc-2104.txt">RFC 2104</a>HMAC:
 * Keyed-Hashing for Message Authentication.<br>
 * H. Krawczyk, M. Bellare, and R. Canetti.</li>
 * <li><a href="http://www.ietf.org/internet-drafts/draft-krovetz-umac-01.txt">
 * UMAC</a>: Message Authentication Code using Universal Hashing.<br>
 * T. Krovetz, J. Black, S. Halevi, A. Hevia, H. Krawczyk, and P. Rogaway.</li>
 * </ol>
 */
public interface IMac
{
  /**
   * Property name of the user-supplied key material. The value associated to
   * this property name is taken to be a byte array.
   */
  String MAC_KEY_MATERIAL = "gnu.crypto.mac.key.material";
  /**
   * Property name of the desired truncated output size in bytes. The value
   * associated to this property name is taken to be an integer. If no value is
   * specified in the attributes map at initialisation time, then all bytes of
   * the underlying hash algorithm's output are emitted.
   * <p>
   * This implementation, follows the recommendation of the <i>RFC 2104</i>
   * authors; specifically:
   * <pre>
   *     We recommend that the output length t be not less than half the
   *     length of the hash output (to match the birthday attack bound)
   *     and not less than 80 bits (a suitable lower bound on the number
   *     of bits that need to be predicted by an attacker).
   * </pre>
   */
  String TRUNCATED_SIZE = "gnu.crypto.mac.truncated.size";

  /**
   * Returns the canonical name of this algorithm.
   * 
   * @return the canonical name of this algorithm.
   */
  String name();

  /**
   * Returns the output length in bytes of this <i>MAC</i> algorithm.
   * 
   * @return the output length in bytes of this <i>MAC</i> algorithm.
   */
  int macSize();

  /**
   * Initialises the algorithm with designated attributes. Permissible names and
   * values are described in the class documentation above.
   * 
   * @param attributes a set of name-value pairs that describe the desired
   *          future instance behaviour.
   * @exception InvalidKeyException if the key data is invalid.
   * @exception IllegalStateException if the instance is already initialised.
   * @see #MAC_KEY_MATERIAL
   */
  void init(Map attributes) throws InvalidKeyException, IllegalStateException;

  /**
   * Continues a <i>MAC</i> operation using the input byte.
   * 
   * @param b the input byte to digest.
   */
  void update(byte b);

  /**
   * Continues a <i>MAC</i> operation, by filling the buffer, processing data
   * in the algorithm's MAC_SIZE-bit block(s), updating the context and count,
   * and buffering the remaining bytes in buffer for the next operation.
   * 
   * @param in the input block.
   * @param offset start of meaningful bytes in input block.
   * @param length number of bytes, in input block, to consider.
   */
  void update(byte[] in, int offset, int length);

  /**
   * Completes the <i>MAC</i> by performing final operations such as padding
   * and resetting the instance.
   * 
   * @return the array of bytes representing the <i>MAC</i> value.
   */
  byte[] digest();

  /**
   * Resets the algorithm instance for re-initialisation and use with other
   * characteristics. This method always succeeds.
   */
  void reset();

  /**
   * A basic test. Ensures that the MAC of a pre-determined message is equal to
   * a known pre-computed value.
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
  Object clone() throws CloneNotSupportedException;
}
