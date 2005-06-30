/* DHParameterSpec.java -- Parameters for Diffie-Hellman keys.
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


package javax.crypto.spec;

import java.math.BigInteger;
import java.security.spec.AlgorithmParameterSpec;

/**
 * The base set of parameters necessary to perform Diffie-Hellman key
 * exchange. Each party in the key exchange shares these parameters.
 *
 * <p>Each set of parameters consists of a <i>base generator</i>
 * <code>g</code>, a <i>prime modulus</i> <code>p</code>, and an
 * optional length, in bits, of the private exponent.
 *
 * <p>See <a href="http://www.rsasecurity.com/rsalabs/pkcs/pkcs-3/">PKCS
 * #3 - Diffie-Hellman Key Agreement Standard</a> for more information.
 *
 * @author Casey Marshall (csm@gnu.org)
 * @since 1.4
 * @see javax.crypto.KeyAgreement
 */
public class DHParameterSpec implements AlgorithmParameterSpec
{

  // Variables.
  // ------------------------------------------------------------------------

  /** The base generator g. */
  private BigInteger g;

  /** The prime modulus p. */
  private BigInteger p;

  /** The length, in bits, of the private exponent. */
  private int l;

  // Constructors.
  // ------------------------------------------------------------------------

  /**
   * Create a new set of Diffie-Hellman parameters.
   *
   * @param p The prime modulus.
   * @param g The base generator.
   */
  public DHParameterSpec(BigInteger p, BigInteger g)
  {
    this(p, g, 0);
  }

  /**
   * Create a new set of Diffie-Hellman parameters.
   *
   * @param p The prime modulus.
   * @param g The base generator.
   * @param l The size of the private exponent, in bits.
   */
  public DHParameterSpec(BigInteger p, BigInteger g, int l)
  {
    this.p = p;
    this.g = g;
    this.l = l;
  }

  // Instance methods.
  // ------------------------------------------------------------------------

  /**
   * Get the base generator, <i>g</i>.
   *
   * @return The base generator <i>g</i>.
   */
  public BigInteger getG()
  {
    return g;
  }

  /**
   * Get the length of the private exponent, in bits.
   *
   * @return The length of the private exponent, in bits, or 0 if this
   *         has not been explicitly set.
   */
  public int getL()
  {
    return l;
  }

  /**
   * Get the prime modulus, <i>p</i>.
   *
   * @return The prime modulus, <i>p</i>.
   */
  public BigInteger getP()
  {
    return p;
  }
}
