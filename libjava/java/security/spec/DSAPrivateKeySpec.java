/* DSAPrivateKeySpec.java --- DSA Private Key Specificaton class
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
	DSA Private Key class Specification. Used to maintain the DSA
	Private Keys.

	@since JDK 1.2

	@author Mark Benvenuto
*/
public class DSAPrivateKeySpec extends Object implements KeySpec 
{
  private BigInteger x = null;
  private BigInteger p = null;
  private BigInteger q = null;
  private BigInteger g = null;

  /**
     Constructs a new DSAPrivateKeySpec with the specified x, p, q, and g.

     @param x the private key
     @param p the prime
     @param q the sub-prime
     @param g the base
  */
  public DSAPrivateKeySpec(BigInteger x, BigInteger p, BigInteger q, BigInteger g) 
  {
    this.x = x;
    this.p = p;
    this.q = q;
    this.g = g;
  }

  /**
     Returns private key x for the DSA algorithm.

     @return Returns the requested BigInteger
  */
  public BigInteger getX() 
  {
    return this.x;
  }

  /**
     Returns p for the DSA algorithm.

     @return Returns the requested BigInteger
  */
  public BigInteger getP() 
  {
    return this.p;
  }

  /**
     Returns p for the DSA algorithm.

     @return Returns the requested BigInteger
  */
  public BigInteger getQ() 
  {
    return this.q;
  }

  /**
     Returns g for the DSA algorithm.

     @return Returns the requested BigInteger
  */
  public BigInteger getG() 
  {
    return this.g;
  }

}
