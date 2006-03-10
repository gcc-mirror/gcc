/* PSSParameterSpec.java --
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
 * This class represents an RSA multi-prime private key, as defined in the
 * PKCS#1 v2.1, using the <i>Chinese Remainder Theorem</i> (CRT) information
 * values.
 *
 * @since 1.4
 * @see java.security.Key
 * @see java.security.KeyFactory
 * @see KeySpec
 * @see PKCS8EncodedKeySpec
 * @see RSAPrivateKeySpec
 * @see RSAPublicKeySpec
 * @see RSAOtherPrimeInfo
 */
public class RSAMultiPrimePrivateCrtKeySpec extends RSAPrivateKeySpec
{
  // Constants and fields
  // --------------------------------------------------------------------------

  private BigInteger publicExponent;
  private BigInteger primeP;
  private BigInteger primeQ;
  private BigInteger primeExponentP;
  private BigInteger primeExponentQ;
  private BigInteger crtCoefficient;
  private RSAOtherPrimeInfo[] otherPrimeInfo;

  // Constructor(s)
  // --------------------------------------------------------------------------

  /**
   * Constructs a new instance of <code>RSAMultiPrimePrivateCrtKeySpec</code>
   * given the various PKCS#1 v2.1 parameters.
   * 
   * <p>Note that <code>otherPrimeInfo</code> is cloned when constructing this
   * object.</p>
   * 
   * @param modulus
   *          the modulus n.
   * @param publicExponent
   *          the public exponent e.
   * @param privateExponent
   *          the private exponent d.
   * @param primeP
   *          the prime factor p of n.
   * @param primeQ
   *          the prime factor q of n.
   * @param primeExponentP
   *          this is d mod (p-1).
   * @param primeExponentQ
   *          this is d mod (q-1).
   * @param crtCoefficient
   *          the Chinese Remainder Theorem coefficient q-1 mod p.
   * @param otherPrimeInfo
   *          triplets of the rest of primes, <code>null</code> can be
   *          specified if there are only two prime factors (p and q).
   * @throws NullPointerException
   *           if any of the parameters is <code>null</code>.
   * @throws IllegalArgumentException
   *           if an empty <code>otherPrimeInfo</code> is specified.
   */
  public RSAMultiPrimePrivateCrtKeySpec(BigInteger modulus,
                                        BigInteger publicExponent,
                                        BigInteger privateExponent,
                                        BigInteger primeP,
                                        BigInteger primeQ,
                                        BigInteger primeExponentP,
                                        BigInteger primeExponentQ,
                                        BigInteger crtCoefficient,
                                        RSAOtherPrimeInfo[] otherPrimeInfo)
  {
    super(modulus, privateExponent);

    if (modulus == null)
      throw new NullPointerException("modulus");
    if (publicExponent == null)
      throw new NullPointerException("publicExponent");
    if (privateExponent == null)
      throw new NullPointerException("privateExponent");
    if (primeP == null)
      throw new NullPointerException("primeP");
    if (primeQ == null)
      throw new NullPointerException("primeQ");
    if (primeExponentP == null)
      throw new NullPointerException("primeExponentP");
    if (primeExponentQ == null)
      throw new NullPointerException("primeExponentQ");
    if (crtCoefficient == null)
      throw new NullPointerException("crtCoefficient");
    if (otherPrimeInfo != null)
      if (otherPrimeInfo.length == 0)
        throw new IllegalArgumentException();
      else
        this.otherPrimeInfo = (RSAOtherPrimeInfo[]) otherPrimeInfo.clone();

    this.publicExponent = publicExponent;
    this.primeP = primeP;
    this.primeQ = primeQ;
    this.primeExponentP = primeExponentP;
    this.primeExponentQ = primeExponentQ;
    this.crtCoefficient = crtCoefficient;
  }

  // Class methods
  // --------------------------------------------------------------------------

  // Instance methods
  // --------------------------------------------------------------------------

  /**
   * Returns the public exponent.
   *
   * @return the public exponent.
   */
  public BigInteger getPublicExponent()
  {
    return this.publicExponent;
  }

  /**
   * Returns the prime p.
   *
   * @return the prime p.
   */
  public BigInteger getPrimeP()
  {
    return this.primeP;
  }

  /**
   * Returns the prime q.
   *
   * @return the prime q.
   */
  public BigInteger getPrimeQ()
  {
    return this.primeQ;
  }

  /**
   * Returns d mod (p-1).
   *
   * @return d mod (p-1).
   */
  public BigInteger getPrimeExponentP()
  {
    return this.primeExponentP;
  }

  /**
   * Returns d mod (q-1).
   *
   * @return d mod (q-1).
   */
  public BigInteger getPrimeExponentQ()
  {
    return this.primeExponentQ;
  }

  /**
   * Returns the CRT Coefficient q-1 mod p.
   *
   * @return the CRT Coefficient q-1 mod p.
   */
  public BigInteger getCrtCoefficient()
  {
    return this.crtCoefficient;
  }

  /**
   * Returns a clone of <code>otherPrimeInfo</code> or <code>null</code> if
   * it was <code>null</code> at construction time.
   *
   * @return a cloned copy of <code>otherPrimeInfo</code>.
   */
  public RSAOtherPrimeInfo[] getOtherPrimeInfo()
  {
    return this.otherPrimeInfo == null
        ? null
        : (RSAOtherPrimeInfo[]) this.otherPrimeInfo.clone();
  }
}
