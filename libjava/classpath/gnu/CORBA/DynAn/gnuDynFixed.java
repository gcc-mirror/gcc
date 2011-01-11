/* gnuDynFixed.java --
   Copyright (C) 2005 Free Software Foundation, Inc.

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


package gnu.CORBA.DynAn;

import org.omg.CORBA.Any;
import org.omg.CORBA.BAD_OPERATION;
import org.omg.CORBA.BAD_PARAM;
import org.omg.CORBA.ORB;
import org.omg.CORBA.TypeCode;
import org.omg.DynamicAny.DynAny;
import org.omg.DynamicAny.DynAnyPackage.InvalidValue;
import org.omg.DynamicAny.DynAnyPackage.TypeMismatch;
import org.omg.DynamicAny.DynFixed;
import org.omg.DynamicAny.DynFixedOperations;

import java.math.BigDecimal;

/**
 * Implements DynAny, holding CORBA <code>fixed</code>. This class is derived
 * from gnuDynEnm to avoid repetetive inclusion of unused DynAny methods.
 *
 * @author Audrius Meskauskas, Lithuania (AudriusA@Bioinformatics.org)
 */
public class gnuDynFixed extends UndivideableAny implements DynFixed
{
  /**
   * Use serialVersionUID for interoperability.
   */
  private static final long serialVersionUID = 1;

  /**
   * The default value, assigned in the new instance.
   */
  static final BigDecimal ZERO = new BigDecimal("0.0");

  /**
   * The content of the dyn fixed, wrapped in this DynAny.
   */
  BigDecimal value;

  /**
   * The number of digits after the decimal point.
   */
  final int scale;

  /**
   * The number of digits.
   */
  final int digits;

  /**
   * Create a new instance of the dyn fixed.
   */
  public gnuDynFixed(TypeCode oType, TypeCode aType,
    gnuDynAnyFactory aFactory, ORB anOrb
  )
  {
    super(oType, aType, aFactory, anOrb);
    try
      {
        digits = final_type.fixed_digits();
        scale = final_type.fixed_scale();
      }
    catch (Exception e)
      {
        throw new BAD_PARAM("Not a fixed");
      }
    value = ZERO;
  }

  /**
   * Clone the current instance.
   */
  public gnuDynFixed(gnuDynFixed from)
  {
    super(from.official_type, from.final_type, from.factory, from.orb);
    digits = from.digits;
    scale = from.scale;
    value = from.value;
  }

  /**
   * Get the value of the wrapped dyn fixed, as string.
   */
  public String get_value()
  {
    return value.toString();
  }

  /**
   * Set the value.
   */
  public boolean set_value(String fixed_value)
    throws TypeMismatch, InvalidValue
  {
    // Count the digits till decimal point.
    int digs = 0;
    char c;
    boolean leading0 = true;
    Digs:
    for (int i = 0; i < fixed_value.length(); i++)
      {
        c = fixed_value.charAt(i);
        if (Character.isDigit(c))
          {
            if (!(c == '0' && leading0))
              digs++;
            if (c != '0')
              leading0 = false;
          }
        else if (c == '.')
          break Digs;
      }
    if (digs > (digits - scale))
      throw new InvalidValue("Too many digits: " + digs + " for " + digits +
        "." + scale
      );

    try
      {
        value = new BigDecimal(fixed_value);
      }
    catch (NumberFormatException ex)
      {
        if (fixed_value.trim().length() == 0)
          throw new InvalidValue("Empty string passed");

        TypeMismatch inva =
          new TypeMismatch("Not a number: '" + fixed_value + "'");
        inva.initCause(ex);
        throw inva;
      }

    valueChanged();
    return value.scale() <= scale;
  }

  /**
   * Assign the value from another BigDecimal.
   */
  public void assign(DynAny from) throws TypeMismatch
  {
    checkType(official_type, from.type());

    if (from instanceof gnuDynFixed)
      {
        gnuDynFixed other = (gnuDynFixed) from;
        value = other.value;
      }
    else if (from instanceof DynFixedOperations)
      {
        value = new BigDecimal(((DynFixedOperations) from).get_value());
      }
    else
      throw new TypeMismatch("Not a DynFixed");
    valueChanged();
  }

  /**
   * Create a copy.
   */
  public DynAny copy()
  {
    return new gnuDynFixed(this);
  }

  /**
   * Compare for equality.
   */
  public boolean equal(DynAny other)
  {
    if (other instanceof gnuDynFixed)
      {
        // Normally, this code would be executed.
        return value.equals(((gnuDynFixed) other).value);
      }
    if (other instanceof DynFixedOperations)
      {
        // This may be involved when mixing implementations.
        return ((DynFixedOperations) other).get_value().equals(get_value());
      }
    else
      return false;
  }

  /**
   * Set the value from Any (must hold <code>fixed</code> with the matching
   * typecode.).
   */
  public void from_any(Any an_any) throws TypeMismatch, InvalidValue
  {
    try
      {
        checkType(official_type, an_any.type());

        value = an_any.extract_fixed();
        valueChanged();
      }
    catch (BAD_OPERATION e)
      {
        InvalidValue t = new InvalidValue();
        t.initCause(e);
        throw t;
      }
  }

  /**
   * Create and return Any, holding this DynFixed value.
   */
  public Any to_any()
  {
    Any g = createAny();
    g.insert_fixed(value, official_type);
    return g;
  }
}
