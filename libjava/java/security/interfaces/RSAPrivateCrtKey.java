/* RSAPrivateCrtKey.java -- An RSA private key in CRT format
   Copyright (C) 1998 Free Software Foundation, Inc.

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
Free Software Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA
02111-1307 USA.

As a special exception, if you link this library with other files to
produce an executable, this library does not by itself cause the
resulting executable to be covered by the GNU General Public License.
This exception does not however invalidate any other reasons why the
executable file might be covered by the GNU General Public License. */

package java.security.interfaces;

import java.math.BigInteger;

/**
 * This interface provides access to information about an RSA private
 * key in Chinese Remainder Theorem (CRT) format.
 *
 * @version 0.0
 *
 * @author Aaron M. Renn (arenn@urbanophile.com)
 */
public interface RSAPrivateCrtKey extends RSAPrivateKey
{
  /**
   * Returns the public exponent for this key
   *
   * @return The public exponent for this key
   */
  public abstract BigInteger getPublicExponent();

  /**
   * Returns the primeP value
   *
   * @return The primeP value
   */
  public abstract BigInteger getPrimeP();

  /**
   * Returns the primeQ value
   *
   * @return The primeQ value
   */
  public abstract BigInteger getPrimeQ();

  /**
   * Returns the primeExponentP
   *
   * @return The primeExponentP
   */
  public abstract BigInteger getPrimeExponentP();

  /**
   * Returns the primeExponentQ
   *
   * @return The primeExponentQ
   */
  public abstract BigInteger getPrimeExponentQ();

  /**
   * Returns the CRT coefficient
   *
   * @return The CRT coefficient
   */
  public abstract BigInteger getCrtCoefficient();
}
