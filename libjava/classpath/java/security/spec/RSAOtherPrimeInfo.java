/* RSAOtherPrimeInfo.java --
   Copyright (C) 2003, Free Software Foundation, Inc.

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

package java.security.spec;

import java.math.BigInteger;

/**
 * An in-memory representation of the RSA triplet (prime, exponent, and
 * coefficient) inside a PKCS#1 v2.1 <i>OtherPrimeInfo</i> structure.
 *
 * @since 1.4
 * @see RSAPrivateCrtKeySpec
 * @see java.security.interfaces.RSAMultiPrimePrivateCrtKey
 */
public class RSAOtherPrimeInfo
{
  // Constants and fields
  // --------------------------------------------------------------------------

  private BigInteger prime;
  private BigInteger primeExponent;
  private BigInteger crtCoefficient;

  // Constructor(s)
  // --------------------------------------------------------------------------

  /**
   * Constructs a new <code>RSAOtherPrimeInfo</code> given the PKCS#1 MPIs.
   *
   * @param prime
   *          the prime factor of n.
   * @param primeExponent
   *          the exponent.
   * @param crtCoefficient
   *          the Chinese Remainder Theorem coefficient.
   * @throws NullPointerException
   *           if any of the parameters is <code>null</code>.
   */
  public RSAOtherPrimeInfo(BigInteger prime, BigInteger primeExponent,
                           BigInteger crtCoefficient)
  {
    super();

    if (prime == null)
      throw new NullPointerException("prime");
    if (primeExponent == null)
      throw new NullPointerException("primeExponent");
    if (crtCoefficient == null)
      throw new NullPointerException("crtCoefficient");

    this.prime = prime;
    this.primeExponent = primeExponent;
    this.crtCoefficient = crtCoefficient;
  }

  // Class methods
  // --------------------------------------------------------------------------

  // Instance methods
  // --------------------------------------------------------------------------

  /**
   * Returns the prime.
   *
   * @return the prime.
   */
  public final BigInteger getPrime()
  {
    return this.prime;
  }

  /**
   * Returns the prime's exponent.
   *
   * @return the primeExponent.
   */
  public final BigInteger getExponent()
  {
    return this.primeExponent;
  }

  /**
   * Returns the CRT Coefficient.
   *
   * @return the CRT Coefficient.
   */
  public final BigInteger getCrtCoefficient()
  {
    return this.crtCoefficient;
  }
}
