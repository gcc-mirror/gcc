/* DSAKeyPairGenerator.java -- Initialize a DSA key generator
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

import java.security.SecureRandom;
import java.security.InvalidParameterException;

/**
 * This interface contains methods for intializing a Digital Signature
 * Algorithm key generation engine.  The initialize methods may be called
 * any number of times.  If no explicity initialization call is made, then
 * the engine defaults to generating 1024-bit keys using pre-calculated
 * base, prime, and subprime values.
 *
 * @version 0.0
 *
 * @author Aaron M. Renn (arenn@urbanophile.com)
 */
public interface DSAKeyPairGenerator
{
  /**
   * Initializes the key generator with the specified DSA parameters and
   * random bit source
   *
   * @param params The DSA parameters to use
   * @param random The random bit source to use
   *
   * @exception InvalidParameterException If the parameters passed are not valid
   */
  public abstract void initialize(DSAParams params, SecureRandom random)
    throws InvalidParameterException;

  /**
   * Initializes the key generator to a give modulus.  If the <code>genParams</code>
   * value is <code>true</code> then new base, prime, and subprime values
   * will be generated for the given modulus.  If not, the pre-calculated
   * values will be used.  If no pre-calculated values exist for the specified
   * modulus, an exception will be thrown.  It is guaranteed that there will
   * always be pre-calculated values for all modulus values between 512 and
   * 1024 bits inclusives.
   *
   * @param modlen The modulus length
   * @param genParams <code>true</code> to generate new DSA parameters, <code>false</code> otherwise
   * @param random The random bit source to use
   *
   * @exception InvalidParameterException If a parameter is invalid
   */
  public abstract void initialize(int modlen, boolean genParams, 
				  SecureRandom random)
    throws InvalidParameterException;
}
