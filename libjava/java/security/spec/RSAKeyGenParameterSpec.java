/* RSAKeyGenParameterSpec.java --- RSA Key Generator Parameter Spec Class
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
   This class generates a set of RSA Key parameters used in the generation
   of RSA keys.

   @since JDK 1.3

   @author Mark Benvenuto
*/
public class RSAKeyGenParameterSpec implements AlgorithmParameterSpec
{
  private int keysize;
  private BigInteger publicExponent;

  /**
     Public Exponent F0 = 3
  */
  public static final BigInteger F0 = new BigInteger("3");

  /**
     Public Exponent F4 = 3
  */
  public static final BigInteger F4 = new BigInteger("65537");

  /**
     Create a new RSAKeyGenParameterSpec to store the RSA key's keysize 
     and public exponent

     @param keysize Modulus size of key in bits
     @param publicExponent - the exponent
  */
  public RSAKeyGenParameterSpec(int keysize, BigInteger publicExponent)
  {
    this.keysize = keysize;
    this.publicExponent = publicExponent;
  }
    
  /**
     Return the size of the key.

     @return the size of the key.
  */
  public int getKeysize()
  {
    return keysize;
  }
    
  /**
     Return the public exponent.

     @return the public exponent.
  */
  public BigInteger getPublicExponent()
  {
    return publicExponent;
  }
}
