/* RSAPrivateKeySpec.java --- RSA Private Key Specificaton class
   Copyright (C) 1999 Free Software Foundation, Inc.

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


package java.security.spec;
import java.math.BigInteger;

/**
	RSA Private Key class Specification. Used to maintain the RSA
	Private Keys.

	@since JDK 1.2

	@author Mark Benvenuto
*/
public class RSAPrivateKeySpec implements KeySpec
{
  private BigInteger modulus;
  private BigInteger privateExponent;

  /**
     Constructs a new RSAPrivateKeySpec with the specified
     modulus and privateExponent.

     @param modulus the RSA modulus
     @param privateExponent the private key exponent
  */
  public RSAPrivateKeySpec(BigInteger modulus, BigInteger privateExponent)
  {
    this.modulus = modulus;
    this.privateExponent = privateExponent;
  }

  /**
     Gets the RSA modulus.

     @return the RSA modulus
  */
  public BigInteger getModulus()
  {
    return this.modulus;
  }

  /**
     Gets the RSA private exponent.

     @return the RSA private exponent
  */
  public BigInteger getPrivateExponent()
  {
    return this.privateExponent;
  }

}
