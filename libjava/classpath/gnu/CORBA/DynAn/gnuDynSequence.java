/* gnuDynSequence.java --
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
import org.omg.CORBA.ORB;
import org.omg.CORBA.TypeCode;
import org.omg.CORBA.TypeCodePackage.BadKind;
import org.omg.DynamicAny.DynAny;
import org.omg.DynamicAny.DynAnyFactoryPackage.InconsistentTypeCode;
import org.omg.DynamicAny.DynAnyPackage.InvalidValue;
import org.omg.DynamicAny.DynAnyPackage.TypeMismatch;
import org.omg.DynamicAny.DynSequence;

import java.io.Serializable;

import java.lang.reflect.*;

public class gnuDynSequence
  extends gnuDynArray
  implements DynSequence, Serializable
{
  /**
   * Use serialVersionUID for interoperability.
   */
  private static final long serialVersionUID = 1;

  /**
   * The bound of the sequence, as defined in typecode.
   */
  final int bound;

  /**
   * Create a new gnuDynSequence with the given typecode.
   *
   * @throws BAD_PARAM if the passed typecode is probably not a sequence
   * typecode.
   */
  public gnuDynSequence(TypeCode oType, TypeCode aType,
                        gnuDynAnyFactory aFactory, ORB anOrb
                       )
                 throws BAD_PARAM
  {
    super(oType, aType, aFactory, anOrb, false);
    array = new DynAny[ 0 ];
    try
      {
        bound = final_type.length();
      }
    catch (BadKind ex)
      {
        throw new Unexpected(ex);
      }
  }

  /**
   * Get the length of the sequence.
   */
  public int get_length()
  {
    return array.length;
  }

  /**
   * Resize the sequence, preserving components.
   */
  public void set_length(int length)
                  throws InvalidValue
  {
    checkBound(length);
    if (length == array.length)
      return; // Nothing to do.
    else if (length < array.length)
      {
        // Truncate.
        DynAny[] d = new DynAny[ length ];
        for (int i = 0; i < d.length; i++)
          d [ i ] = array [ i ];
        array = d;
      }
    else
      {
        // Expand.
        DynAny[] d = new DynAny[ length ];
        for (int i = 0; i < array.length; i++)
          d [ i ] = array [ i ];

        for (int i = array.length; i < d.length; i++)
          {
            try
              {
                d [ i ] =
                  factory.create_dyn_any_from_type_code(official_components);
              }
            catch (InconsistentTypeCode e)
              {
                throw new Unexpected(e);
              }
          }
        array = d;
      }
    valueChanged();
  }

  /**
   * Copy one DynAny into another.
   */
  public void assign(DynAny from)
              throws TypeMismatch
  {
    checkType(official_type, from.type());
    if (from instanceof DynSequence)
      {
        DynSequence dyn = (DynSequence) from;
        array = dyn.get_elements_as_dyn_any();
      }
    else
      throw new TypeMismatch();
  }

  /*
   * Set the contenst of the sequence, resizing if required.
   */
  public void set_elements_as_dyn_any(DynAny[] value)
                               throws InvalidValue, TypeMismatch
  {
    checkBound(value.length);
    if (array.length != value.length)
      set_length(value.length);

    for (int i = 0; i < value.length; i++)
      {
        checkType(official_components, value [ i ].type());
        array [ i ].assign(value [ i ]);
      }
    valueChanged();
  }

  /**
   * Set the elements from array of Any's.
   */
  public void set_elements(Any[] value)
                    throws InvalidValue, TypeMismatch
  {
    checkBound(value.length);

    DynAny[] prev = array;

    array = new DynAny[ value.length ];
    try
      {
        super.set_elements(value);

        // valueChanged() is called in super.set_elements(value).
      }

    // On the problem, value does not change.
    catch (TypeMismatch ex)
      {
        array = prev;
        throw ex;
      }
    catch (InvalidValue ex)
      {
        array = prev;
        throw ex;
      }
    catch (RuntimeException rex)
      {
        array = prev;
        throw rex;
      }
  }

  /**
   * Create a copy.
   */
  public DynAny copy()
  {
    DynAny[] c = new DynAny[ array.length ];
    for (int i = 0; i < c.length; i++)
      {
        c [ i ] = array [ i ].copy();
      }

    gnuDynSequence d =
      new gnuDynSequence(official_type, final_type, factory, orb);
    d.array = c;
    return d;
  }

  /**
   * Check the bound.
   *
   * @param x the value to check.
   */
  void checkBound(int x)
           throws InvalidValue
  {
    if (bound != 0)
      if (x < 0 || x > bound)
        throw new InvalidValue(x + " out of bounds, valid [0.." + bound + "]");
  }

  /**
   * Check if array size is valid. Called from from_any.
   */
  protected void checkArrayValid(Object members)
                          throws TypeMismatch, InvalidValue
  {
    checkBound(Array.getLength(members));
    if (get_length() != Array.getLength(members))
      set_length(Array.getLength(members));
  }
}