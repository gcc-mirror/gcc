/* MathContext.java -- 
   Copyright (C) 1999, 2000, 2002, 2004, 2005  Free Software Foundation, Inc.

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


package java.math;

import java.io.Serializable;

/**
 * Immutable objects describing settings such as rounding mode and digit
 * precision for numerical operations such as those in the BigDecimal class.
 * @author Anthony Balkissoon abalkiss at redhat dot com
 *
 */
public final class MathContext implements Serializable
{
  
  /**
   * This is the serialVersionUID reported here:
   * java.sun.com/j2se/1.5.0/docs/api/serialized-form.html#java.math.MathContext
   */
  private static final long serialVersionUID = 5579720004786848255L;
  
  private int precision;
    
  /**
   * Constructs a new MathContext with the specified precision and with HALF_UP
   * rounding.
   * @param setPrecision the precision for the new MathContext
   * 
   * @throws IllegalArgumentException if precision is < 0.
   */
  public MathContext(int setPrecision)
  {
    if (setPrecision < 0)
      throw new IllegalArgumentException("Precision cannot be less than zero.");
    precision = setPrecision;
  }
    
  /**
   * Constructs a MathContext from a String that has the same form as one
   * produced by the toString() method.
   * @param val
   * 
   * @throws IllegalArgumentException if the String is not in the correct
   * format or if the precision specified is < 0.
   */
  public MathContext(String val)
  {
    try
    {
      int roundingModeIndex = val.indexOf("roundingMode", 10);
      precision = Integer.parseInt(val.substring(10, roundingModeIndex - 1));
    }
    catch (NumberFormatException nfe)
    {
      throw new IllegalArgumentException("String not in correct format");
    }
    catch (IllegalArgumentException iae)
    {
      throw new IllegalArgumentException("String not in correct format");
    }
    if (precision < 0)
      throw new IllegalArgumentException("Precision cannot be less than 0.");
  }
  
  /**
   * Returns true if x is a MathContext and has the same precision setting
   * and rounding mode as this MathContext.
   * 
   * @return true if the above conditions hold
   */
  public boolean equals(Object x)
  {
    if (!(x instanceof MathContext))
      return false;
    MathContext mc = (MathContext)x;
    return mc.precision == this.precision;
  }
  
  /**
   * Returns the precision setting.
   * @return the precision setting.
   */
  public int getPrecision()
  {
    return precision;
  }
  
  /**
   * Returns "precision=p roundingMode=MODE" where p is an int giving the 
   * precision and MODE is UP, DOWN, HALF_UP, HALF_DOWN, HALF_EVEN, CEILING,
   * FLOOR, or UNNECESSARY corresponding to rounding modes.
   * 
   * @return a String describing this MathContext
   */
  public String toString()
  {
    return "precision="+precision;
  }
  
  /**
   * Returns the hashcode for this MathContext.
   * @return the hashcode for this MathContext.
   */
  public int hashCode()
  {
    return precision;
  }
}
