/* SRP6KeyAgreement.java -- 
   Copyright (C) 2003, 2006 Free Software Foundation, Inc.

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


package gnu.javax.crypto.key.srp6;

import gnu.java.security.Registry;
import gnu.java.security.hash.IMessageDigest;
import gnu.java.security.util.Util;

import gnu.javax.crypto.key.BaseKeyAgreementParty;
import gnu.javax.crypto.key.KeyAgreementException;
import gnu.javax.crypto.sasl.srp.SRP;

import java.math.BigInteger;

/**
 * The Secure Remote Password (SRP) key agreement protocol, also known as SRP-6,
 * is designed by Thomas J. Wu (see references). The protocol, and its elements
 * are described as follows:
 * <pre>
 *  N    A large safe prime (N = 2q+1, where q is prime)
 *       All arithmetic is done modulo N.
 *  g    A generator modulo N
 *  s    User's salt
 *  I    Username
 *  p    Cleartext Password
 *  H()  One-way hash function
 *  &circ;    (Modular) Exponentiation
 *  u    Random scrambling parameter
 *  a,b  Secret ephemeral values
 *  A,B  Public ephemeral values
 *  x    Private key (derived from p and s)
 *  v    Password verifier
 * 
 *  The host stores passwords using the following formula:
 *  x = H(s | H(I &quot;:&quot; p))           (s is chosen randomly)
 *  v = g&circ;x                         (computes password verifier)
 * 
 *  The host then keeps {I, s, v} in its password database.
 * 
 *  The authentication protocol itself goes as follows:
 *  User -&gt; Host:  I, A = g&circ;a         (identifies self, a = random number)
 *  Host -&gt; User:  s, B = 3v + g&circ;b    (sends salt, b = random number)
 * 
 *  Both:  u = H(A, B)
 * 
 *  User:  x = H(s, p)               (user enters password)
 *  User:  S = (B - 3g&circ;x) &circ; (a + ux) (computes session key)
 *  User:  K = H(S)
 * 
 *  Host:  S = (Av&circ;u) &circ; b            (computes session key)
 *  Host:  K = H(S)
 * </pre>
 * <p>
 * Reference:
 * <ol>
 * <li><a href="http://srp.stanford.edu/design.html">SRP Protocol Design</a><br>
 * Thomas J. Wu.</li>
 * </ol>
 */
public abstract class SRP6KeyAgreement
    extends BaseKeyAgreementParty
{
  public static final String SOURCE_OF_RANDOMNESS = "gnu.crypto.srp6.ka.prng";
  public static final String SHARED_MODULUS = "gnu.crypto.srp6.ka.N";
  public static final String GENERATOR = "gnu.crypto.srp6.ka.g";
  public static final String HASH_FUNCTION = "gnu.crypto.srp6.ka.H";
  public static final String USER_IDENTITY = "gnu.crypto.srp6.ka.I";
  public static final String USER_PASSWORD = "gnu.crypto.srp6.ka.p";
  public static final String HOST_PASSWORD_DB = "gnu.crypto.srp6.ka.password.db";
  protected static final BigInteger THREE = BigInteger.valueOf(3L);
  protected SRP srp;
  protected BigInteger N;
  protected BigInteger g;
  /** The shared secret key. */
  protected BigInteger K;

  protected SRP6KeyAgreement()
  {
    super(Registry.SRP6_KA);
  }

  protected byte[] engineSharedSecret() throws KeyAgreementException
  {
    return Util.trim(K);
  }

  protected void engineReset()
  {
    srp = null;
    N = null;
    g = null;
    K = null;
  }

  protected BigInteger uValue(final BigInteger A, final BigInteger B)
  {
    final IMessageDigest hash = srp.newDigest();
    byte[] b;
    b = Util.trim(A);
    hash.update(b, 0, b.length);
    b = Util.trim(B);
    hash.update(b, 0, b.length);
    return new BigInteger(1, hash.digest());
  }
}
