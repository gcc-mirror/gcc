/* java.math.BigDecimal -- Arbitrary precision decimals.
   Copyright (C) 1999, 2000 Free Software Foundation, Inc.

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

package java.math;

import java.math.BigInteger;

public class BigDecimal extends Number implements Comparable {
  BigInteger num;
  int scale;

  private final static BigDecimal ZERO = 
    new BigDecimal (BigInteger.valueOf (0), 0);

  private final static BigDecimal ONE = 
    new BigDecimal (BigInteger.valueOf (1), 0);

  public final static int ROUND_UP = 0;
  public final static int ROUND_DOWN = 1;
  public final static int ROUND_CEILING = 2;
  public final static int ROUND_FLOOR = 3;
  public final static int ROUND_HALF_UP = 4;
  public final static int ROUND_HALF_DOWN = 5;
  public final static int ROUND_HALF_EVEN = 6;
  public final static int ROUND_UNNECESSARY = 7;

  public BigDecimal (BigInteger num) 
  {
    this (num, 0);
  }

  public BigDecimal (BigInteger num, int scale) throws NumberFormatException 
  {
    if (scale < 0) 
      throw new NumberFormatException ("scale of " + scale + " is < 0");
    this.num = num;
    this.scale = scale;
  }

  public BigDecimal (double num) throws NumberFormatException 
  {
    this (Double.toString (num));
  }

  public BigDecimal (String num) throws NumberFormatException 
  {
    int point = num.indexOf('.');
    this.num = new BigInteger (point == -1 ? num :
			       num.substring (0, point) + 
			       num.substring (point + 1));
    scale = num.length() - (point == -1 ? num.length () : point + 1);
  }

  public static BigDecimal valueOf (long val) 
  {
    return valueOf (val, 0);
  }

  public static BigDecimal valueOf (long val, int scale) 
    throws NumberFormatException 
  {
    if (scale == 0)
      switch ((int) val)
	{
	case 0:
	  return ZERO;
	case 1:
	  return ONE;
	}

    return new BigDecimal (BigInteger.valueOf (val), scale);
  }

  public BigDecimal add (BigDecimal val) 
  {
    // For addition, need to line up decimals.  Note that the movePointRight
    // method cannot be used for this as it might return a BigDecimal with
    // scale == 0 instead of the scale we need.
    BigInteger op1 = num;
    BigInteger op2 = val.num;
    if (scale < val.scale)
      op1 = op1.multiply (BigInteger.valueOf (10).pow (val.scale - scale));
    else if (scale > val.scale)
      op2 = op2.multiply (BigInteger.valueOf (10).pow (scale - val.scale));

    return new BigDecimal (op1.add (op2), Math.max (scale, val.scale));
  }

  public BigDecimal subtract (BigDecimal val) 
  {
    return this.add(val.negate());
  }

  public BigDecimal multiply (BigDecimal val) 
  {
    return new BigDecimal (num.multiply (val.num), scale + val.scale);
  }

  public BigDecimal divide (BigDecimal val, int roundingMode) 
    throws ArithmeticException, IllegalArgumentException 
  {
    return divide (val, scale, roundingMode);
  }

  public BigDecimal divide(BigDecimal val, int newScale, int roundingMode)
    throws ArithmeticException, IllegalArgumentException 
  {
    if (roundingMode < 0 || roundingMode > 7)
      throw 
	new IllegalArgumentException("illegal rounding mode: " + roundingMode);

    if (scale < 0)
      throw new ArithmeticException ("scale is negative: " + scale);

    if (num.signum () == 0)	// handle special case of 0.0/0.0
      return ZERO;
    
    BigInteger dividend = num.multiply (BigInteger.valueOf (10).pow 
					(newScale + 1 - (scale - val.scale)));
    
    BigInteger parts[] = dividend.divideAndRemainder (val.num);
//      System.out.println("int: " + parts[0]);
//      System.out.println("rem: " + parts[1]);

    int roundDigit = parts[0].mod (BigInteger.valueOf (10)).intValue ();
    BigInteger unrounded = parts[0].divide (BigInteger.valueOf (10));

    if (roundDigit == 0 && parts[1].signum () == 0) // no rounding necessary
      return new BigDecimal (unrounded, newScale);

    int sign = unrounded.signum ();

    switch (roundingMode)
      {
      case ROUND_UNNECESSARY:
	throw new ArithmeticException ("newScale is not large enough");
      case ROUND_CEILING:
	roundingMode = (sign == 1) ? ROUND_UP : ROUND_DOWN;
	break;
      case ROUND_FLOOR:
	roundingMode = (sign == 1) ? ROUND_DOWN : ROUND_UP;
	break;
      case ROUND_HALF_UP:
	roundingMode = (roundDigit >= 5) ? ROUND_UP : ROUND_DOWN;
	break;
      case ROUND_HALF_DOWN:
	roundingMode = (roundDigit > 5) ? ROUND_UP : ROUND_DOWN;
	break;
      case ROUND_HALF_EVEN:
	if (roundDigit < 5)
	  roundingMode = ROUND_DOWN;
	else
	  {
	    int rightmost = 
	      unrounded.mod (BigInteger.valueOf (10)).intValue ();
	    if (rightmost % 2 == 1) // odd, then ROUND_HALF_UP
	      roundingMode = ROUND_UP;
	    else // even, then ROUND_HALF_DOWN
	      roundingMode = (roundDigit > 5) ? ROUND_UP : ROUND_DOWN;
	  }
	break;
      }

    if (roundingMode == ROUND_UP)
      return new BigDecimal (unrounded.add (BigInteger.valueOf (1)), newScale);

    // roundingMode == ROUND_DOWN
    return new BigDecimal (unrounded, newScale);
  }
    
  public int compareTo (BigDecimal val) 
  {
    if (scale == val.scale)
      return num.compareTo (val.num);

    BigInteger thisParts[] = 
      num.divideAndRemainder (BigInteger.valueOf (10).pow (scale));
    BigInteger valParts[] =
      val.num.divideAndRemainder (BigInteger.valueOf (10).pow (val.scale));
    
    int compare;
    if ((compare = thisParts[0].compareTo (valParts[0])) != 0)
      return compare;

    // quotients are the same, so compare remainders

    // remove trailing zeros
    if (thisParts[1].equals (BigInteger.valueOf (0)) == false)
      while (thisParts[1].mod (BigInteger.valueOf (10)).equals
	     (BigInteger.valueOf (0)))
      thisParts[1] = thisParts[1].divide (BigInteger.valueOf (10));
    // again...
    if (valParts[1].equals(BigInteger.valueOf (0)) == false)
      while (valParts[1].mod (BigInteger.valueOf (10)).equals
	     (BigInteger.valueOf (0)))
	valParts[1] = valParts[1].divide (BigInteger.valueOf (10));

    // and compare them
    return thisParts[1].compareTo (valParts[1]);
  }

  public int compareTo (Object val) 
  {
    return(compareTo((BigDecimal)val));
  }

  public boolean equals (Object o) 
  {
    return (o instanceof BigDecimal 
	    && scale == ((BigDecimal) o).scale
	    && compareTo ((BigDecimal) o) == 0);
  }

  public int hashCode() 
  {
    return intValue() ^ scale;
  }

  public BigDecimal max (BigDecimal val)
  {
    switch (compareTo (val)) 
      {
      case 1:
	return this;
      default:
	return val;
      }
  }

  public BigDecimal min (BigDecimal val) 
  {
    switch (compareTo (val)) 
      {
      case -1:
	return this;
      default:
	return val;
      }
  }

  public BigDecimal movePointLeft (int n)
  {
    return (n < 0) ? movePointRight (-n) : new BigDecimal (num, scale + n);
  }

  public BigDecimal movePointRight (int n)
  {
    if (n < 0)
      return movePointLeft (-n);

    if (scale >= n)
      return new BigDecimal (num, scale - n);

    return new BigDecimal (num.multiply 
			   (BigInteger.valueOf (10).pow (n - scale)), 0);
  }

  public int signum () 
  {
    return num.signum ();
  }

  public int scale () 
  {
    return scale;
  }
  
  public BigDecimal abs () 
  {
    return new BigDecimal (num.abs (), scale);
  }

  public BigDecimal negate () 
  {
    return new BigDecimal (num.negate (), scale);
  }

  public String toString () 
  {
    String bigStr = num.toString();
    if (scale == 0) 
      return bigStr;

    int point = bigStr.length() - scale;
    boolean negative = (bigStr.charAt(0) == '-');
    StringBuffer sb = new StringBuffer(bigStr.length() + 1 + 
				       (point <= 0 ? -point+1 : 0));
    if (negative)
      sb.append('-');
    while (point <= 0)
      {
	sb.append('0');
	point++;
      }
    sb.append(bigStr.substring(negative ? 1 : 0));
    sb.insert(point, '.');
    return sb.toString();
  }

  public BigInteger toBigInteger () 
  {
    return scale == 0 ? num : num.divide (BigInteger.valueOf (10).pow (scale));
  }


  public int intValue () 
  {
    return toBigInteger ().intValue ();
  }

  public long longValue ()
  {
    return toBigInteger().longValue();
  }

  public float floatValue() 
  {
    return Float.valueOf(toString()).floatValue();
  }

  public double doubleValue() 
  {
    return Double.valueOf(toString()).doubleValue();
  }
}
