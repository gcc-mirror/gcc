/* RSAMultiPrimePrivateCrtKey.java --
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

package java.security.interfaces;

import java.math.BigInteger;
import java.security.spec.RSAOtherPrimeInfo;

/**
 * The interface to an RSA multi-prime private key, as defined in the PKCS#1
 * v2.1, using the <i>Chinese Remainder Theorem</i> (CRT) information values.
 *
 * @since 1.4
 * @see java.security.spec.RSAPrivateKeySpec
 * @see java.security.spec.RSAMultiPrimePrivateCrtKeySpec
 * @see RSAPrivateKey
 * @see RSAPrivateCrtKey
 */
public interface RSAMultiPrimePrivateCrtKey extends RSAPrivateKey
{
  // Constants
  // --------------------------------------------------------------------------

  long serialVersionUID = 618058533534628008L;

  // Methods
  // --------------------------------------------------------------------------

  /**
   * Returns the public exponent.
   *
   * @return the public exponent.
   */
  BigInteger getPublicExponent();

  /**
   * Returns the prime p.
   *
   * @return the prime p.
   */
  BigInteger getPrimeP();

  /**
   * Returns the prime q.
   *
   * @return the prime q.
   */
  BigInteger getPrimeQ();

  /**
   * Returns the prime's exponent p.
   *
   * @return the prime's exponent p.
   */
  BigInteger getPrimeExponentP();

  /**
   * Returns the prime's exponent q.
   *
   * @return the prime's exponent q.
   */
  BigInteger getPrimeExponentQ();

  /**
   * Returns the CRT Coefficient.
   *
   * @return the CRT Coefficient.
   */
  BigInteger getCrtCoefficient();

  /**
   * Returns the <i>OtherPrimeInfo</i> triplet MPIs or <code>null</code> if
   * there are only two known prime factors (p and q).
   *
   * @return the <i>OtherPrimeInfo</i> INTEGERs.
   */
  RSAOtherPrimeInfo[] getOtherPrimeInfo();
}
