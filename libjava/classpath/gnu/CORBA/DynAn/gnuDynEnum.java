/* gnuDynEnum.java --
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

import gnu.CORBA.Unexpected;

import org.omg.CORBA.Any;
import org.omg.CORBA.BAD_PARAM;
import org.omg.CORBA.MARSHAL;
import org.omg.CORBA.ORB;
import org.omg.CORBA.TypeCode;
import org.omg.CORBA.portable.InputStream;
import org.omg.DynamicAny.DynAny;
import org.omg.DynamicAny.DynAnyPackage.InvalidValue;
import org.omg.DynamicAny.DynAnyPackage.TypeMismatch;
import org.omg.DynamicAny.DynEnum;

import java.io.IOException;

import java.util.Arrays;

/**
 * Our implementation of dynamic enumeration.
 *
 * @author Audrius Meskauskas, Lithuania (AudriusA@Bioinformatics.org)
 */
public class gnuDynEnum extends UndivideableAny implements DynEnum
{
  /**
   * Use serialVersionUID for interoperability.
   */
  private static final long serialVersionUID = 1;

  /**
   * The valid string values of the enumeration. Most of enumerations are short,
   * counting 2-5 memebers. With so small number of memebers, it seems not
   * reasonable to use hashtables.
   */
  final String[] values;

  /**
   * The current value of enum.
   */
  int current;

  /**
   * Create a new dyn enum from the given typecode.
   */
  public gnuDynEnum(TypeCode oType, TypeCode aType, gnuDynAnyFactory aFactory,
    ORB anOrb
  )
  {
    super(oType, aType, aFactory, anOrb);
    try
      {
        values = new String[ final_type.member_count() ];

        for (int i = 0; i < values.length; i++)
          {
            values [ i ] = final_type.member_name(i);
          }
      }
    catch (Exception e)
      {
        throw new BAD_PARAM("Not enum");
      }
  }

  /**
   * Create a clone of the given enum, sharing values and final_type.
   */
  public gnuDynEnum(gnuDynEnum from)
  {
    super(from.official_type, from.final_type, from.factory, from.orb);
    values = from.values;
  }

  /**
   * Assign the Enum from the passed value. The passed DynAny must hold the
   * enumeration of exactly the same final_type.
   */
  public void assign(DynAny from) throws TypeMismatch
  {
    checkType(official_type, from.type());
    if (!(from instanceof DynEnum))
      throw new TypeMismatch("Not a DynEnum");
    try
      {
        set_as_ulong(((DynEnum) from).get_as_ulong());
      }
    catch (InvalidValue e)
      {
        TypeMismatch t = new TypeMismatch();
        t.initCause(e);
        throw t;
      }
  }

  /**
   * Copy this DynEnum.
   */
  public DynAny copy()
  {
    gnuDynEnum other = new gnuDynEnum(this);
    other.current = current;
    return other;
  }

  /**
   * Compares for equality.
   */
  public boolean equal(DynAny other)
  {
    if (other instanceof gnuDynEnum)
      {
        gnuDynEnum oe = (gnuDynEnum) other;
        return current == oe.current &&
        (oe.values == values || Arrays.equals(values, oe.values));
      }
    else if (other instanceof DynEnum)
      {
        DynEnum oe = (DynEnum) other;
        return current == oe.get_as_ulong() && official_type.equal(oe.type());
      }
    else
      return false;
  }

  /**
   * Set value from any that must contain enumeration.
   */
  public void from_any(Any an_any) throws TypeMismatch, InvalidValue
  {
    checkType(official_type, an_any.type());
    try
      {
        InputStream in = an_any.create_input_stream();
        set_as_ulong(in.read_long());
        in.close();
      }
    catch (MARSHAL eof)
      {
        throw new InvalidValue();
      }
    catch (IOException ex)
      {
        throw new Unexpected(ex);
      }
  }

  /**
   * Get the value of this enumeration as string.
   */
  public String get_as_string()
  {
    return values [ current ];
  }

  /**
   * Get the value of this enumeration as int.
   */
  public int get_as_ulong()
  {
    return current;
  }

  /**
   * Set the value of this enumeration as string.
   */
  public void set_as_string(String value) throws InvalidValue
  {
    for (int i = 0; i < values.length; i++)
      {
        if (values [ i ].equals(value))
          {
            current = i;
            valueChanged();
            return;
          }
      }
    throw new InvalidValue(value);
  }

  /**
   * Set the value of this enumeration as int.
   */
  public void set_as_ulong(int value) throws InvalidValue
  {
    if (value < 0 || value >= values.length)
      throw new InvalidValue(value + " not in [0.." + values.length);
    else
      {
        current = value;
        valueChanged();
      }
  }

  /**
   * Wrap the enumeration value into any.
   */
  public Any to_any()
  {
    Any a = createAny();
    a.insert_long(current);
    a.type(official_type);
    return a;
  }
}
