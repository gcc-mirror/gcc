/* DSAParameterGenerator.java --- DSA Parameter Generator Implementation
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


package gnu.java.security.provider;

import java.math.BigInteger;
import java.security.AlgorithmParameters;
import java.security.AlgorithmParameterGeneratorSpi;
import java.security.InvalidAlgorithmParameterException;
import java.security.NoSuchAlgorithmException;
import java.security.SecureRandom;
import java.security.spec.AlgorithmParameterSpec;
import java.security.spec.DSAParameterSpec;
import java.security.spec.InvalidParameterSpecException;
import java.util.Random;

import gnu.java.security.util.Prime;

public class DSAParameterGenerator extends AlgorithmParameterGeneratorSpi
{
  private int size;
  private SecureRandom random = null;

  public DSAParameterGenerator()
  {
    size = 1024;
  }

  public void engineInit(int size, SecureRandom random)
  {
    if( (size < 512) || (size > 1024) || ( (size % 64) != 0) )
      //throw new InvalidAlgorithmParameterException("Invalid Size");
      return;
    this.size = size;
    this.random = random;
  }

  public void engineInit(AlgorithmParameterSpec genParamSpec, SecureRandom random)
    throws InvalidAlgorithmParameterException
  {
    if( !( genParamSpec instanceof DSAParameterSpec ) )
      throw new InvalidAlgorithmParameterException("Must be DSAParameterSpec");

    DSAParameterSpec dsaparameterspec = (DSAParameterSpec)genParamSpec;
    int tmp = dsaparameterspec.getP().bitLength();

    if( (tmp < 512) || (tmp > 1024) || ( (tmp % 64) != 0) )
      throw new InvalidAlgorithmParameterException("Invalid Size");

    this.random = random;
  }

  //For more information see IEEE P1363 A.16.1 (10/05/98 Draft)
  public AlgorithmParameters engineGenerateParameters()
  {
    DSAParameterSpec dsaparameterspec;

    int L = size;
    BigInteger r, p, k, h, g;

    //q 2^159 < q < 2^160
    r = Prime.generateRandomPrime( 159, 160, BigInteger.valueOf(1));

    // 2^(L-1) < p < 2^L
    p = Prime.generateRandomPrime( r, BigInteger.valueOf(1), L - 1, L, BigInteger.valueOf(1));

    k = p.subtract( BigInteger.valueOf(1) );
    k = k.divide( r );

    Random rand = new Random();
    h = BigInteger.valueOf(1);

    for(;;) {
      h = h.add(BigInteger.valueOf( 1 ) );

      g = h.modPow(k, p);

      if( g.compareTo( BigInteger.valueOf(1) ) != 1 )
	break;
    }

    try {
      dsaparameterspec = new DSAParameterSpec(p, r, g);
      AlgorithmParameters ap = AlgorithmParameters.getInstance("DSA");
      ap.init( dsaparameterspec );
      return ap;
    } catch ( NoSuchAlgorithmException nsae ) {
      return null;
    } catch ( InvalidParameterSpecException ipse) {
      return null;
    }
  }
}
