/* Prime.java --- Prime number generation utilities
   Copyright (C) 1999, 2004 Free Software Foundation, Inc.

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


package gnu.java.security.util;
import java.math.BigInteger;
import java.util.Random;
//import java.security.SecureRandom;

public final class Prime
{

  /*
    See IEEE P1363 A.15.4 (10/05/98 Draft)
  */
  public static BigInteger generateRandomPrime( int pmin, int pmax, BigInteger f )
  {
    BigInteger d;

    //Step 1 - generate prime
    BigInteger p = new BigInteger( (pmax + pmin)/2, new Random() );
    if( p.compareTo( BigInteger.valueOf( 1 ).shiftLeft( pmin ) ) <= 0 )
      {
	p = p.add( BigInteger.valueOf( 1 ).shiftLeft( pmin ).subtract( p ) );
      }
	
    //Step 2 - test for even
    if( p.mod( BigInteger.valueOf(2) ).compareTo( BigInteger.valueOf( 0 )) == 0)
      p = p.add( BigInteger.valueOf( 1 ) );

    for(;;)
      {
	//Step 3
	if( p.compareTo( BigInteger.valueOf( 1 ).shiftLeft( pmax)) > 0)
	  {
	    //Step 3.1
	    p = p.subtract( BigInteger.valueOf( 1 ).shiftLeft( pmax) );
	    p = p.add( BigInteger.valueOf( 1 ).shiftLeft( pmin) );
	    p = p.subtract( BigInteger.valueOf( 1 ) );

	    //Step 3.2
	    // put step 2 code here so looping code is cleaner
	    //Step 2 - test for even
	    if( p.mod( BigInteger.valueOf(2) ).compareTo( BigInteger.valueOf( 0 )) == 0)
	      p = p.add( BigInteger.valueOf( 1 ) );
	    continue;
	  }
	
	//Step 4 - compute GCD
	d = p.subtract( BigInteger.valueOf(1) );
	d = d.gcd( f );

	//Step 5 - test d
	if( d.compareTo( BigInteger.valueOf( 1 ) ) == 0)
	  {
	    //Step 5.1 - test primality
	    if( p.isProbablePrime( 1 ) == true )
	      {
				//Step 5.2;
		return p;
	      }
	  }
	//Step 6
	p = p.add( BigInteger.valueOf( 2 ) );

	//Step 7
      }
  }


  /*
    See IEEE P1363 A.15.5 (10/05/98 Draft)
  */
  public static BigInteger generateRandomPrime( BigInteger r, BigInteger a, int pmin, int pmax, BigInteger f )
  {
    BigInteger d, w;

    //Step 1 - generate prime
    BigInteger p = new BigInteger( (pmax + pmin)/2, new Random() );

  steptwo:{ //Step 2
      w = p.mod( r.multiply( BigInteger.valueOf(2) ));

      //Step 3
      p = p.add( r.multiply( BigInteger.valueOf(2) ) );
      p = p.subtract( w );
      p = p.add(a);

      //Step 4 - test for even
      if( p.mod( BigInteger.valueOf(2) ).compareTo( BigInteger.valueOf( 0 )) == 0)
	p = p.add( r );

      for(;;)
	{
	  //Step 5
	  if( p.compareTo( BigInteger.valueOf( 1 ).shiftLeft( pmax)) > 0)
	    {
	      //Step 5.1
	      p = p.subtract( BigInteger.valueOf( 1 ).shiftLeft( pmax) );
	      p = p.add( BigInteger.valueOf( 1 ).shiftLeft( pmin) );
	      p = p.subtract( BigInteger.valueOf( 1 ) );

	      //Step 5.2 - goto to Step 2
	      break steptwo;
	    }

	  //Step 6
	  d = p.subtract( BigInteger.valueOf(1) );
	  d = d.gcd( f );

	  //Step 7 - test d
	  if( d.compareTo( BigInteger.valueOf( 1 ) ) == 0)
	    {
	      //Step 7.1 - test primality
	      if( p.isProbablePrime( 1 ) == true )
		{
				//Step 7.2;
		  return p;
		}
	    }
	  //Step 8
	  p = p.add( r.multiply( BigInteger.valueOf(2) ) );

	  //Step 9
	}
    }
    //Should never reach here but makes the compiler happy
    return BigInteger.valueOf(0);	
  }
}
