/* gnuDynArray.java --
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
import gnu.CORBA.holderFactory;

import org.omg.CORBA.Any;
import org.omg.CORBA.BAD_PARAM;
import org.omg.CORBA.ORB;
import org.omg.CORBA.TCKind;
import org.omg.CORBA.TypeCode;
import org.omg.CORBA.TypeCodePackage.BadKind;
import org.omg.CORBA.portable.Streamable;
import org.omg.DynamicAny.DynAny;
import org.omg.DynamicAny.DynAnyFactoryPackage.InconsistentTypeCode;
import org.omg.DynamicAny.DynAnyPackage.InvalidValue;
import org.omg.DynamicAny.DynAnyPackage.TypeMismatch;
import org.omg.DynamicAny.DynArray;

import java.io.Serializable;

import java.lang.reflect.Array;
import java.lang.reflect.Field;

/**
 * Provides support for dynamic array or sequence, where all members have the
 * same final_type.
 *
 * @author Audrius Meskauskas, Lithuania (AudriusA@Bioinformatics.org)
 */
public class gnuDynArray
  extends anyDivideable
  implements DynArray, Serializable
{
  /**
   * Use serialVersionUID for interoperability.
   */
  private static final long serialVersionUID = 1;

  /**
   * The component "official" type (may be alias).
   */
  final TypeCode official_components;

  /**
   * The component "final" type, after resolving any aliases.
   */
  final TypeCode final_components;

  /**
   * Creates new array.
   *
   * @param aType the final_type of array.
   * @param aFactory the factory, used to initialise default values.
   * @param orb the ORB to that this DynAny belongs.
   * @param initialise_array if false, the array is not initialised in
   * constructor.
   *
   *
   * @throws BAD_PARAM if the passed typecode does not provide the length().
   */
  public gnuDynArray(TypeCode oType, TypeCode aType, gnuDynAnyFactory aFactory,
                     ORB anOrb, boolean initialise_array
                    )
              throws BAD_PARAM
  {
    super(oType, aType, aFactory, anOrb);

    try
      {
        official_components = final_type.content_type();

        TypeCode component = official_components;
        while (component.kind().value() == TCKind._tk_alias)
          component = component.content_type();
        final_components = component;

        if (initialise_array)
          {
            array = new DynAny[ aType.length() ];
            for (int i = 0; i < array.length; i++)
              {
                array [ i ] =
                  factory.create_dyn_any_from_type_code(official_components);
              }
          }
      }
    catch (Exception e)
      {
        BAD_PARAM bad = new BAD_PARAM("Unable to initialise array");
        bad.initCause(e);
        throw bad;
      }
  }

  /**
   * Copy one DynAny into another.
   */
  public void assign(DynAny from)
              throws TypeMismatch
  {
    checkType(official_type, from.type());
    if (from instanceof DynArray && from.component_count() == array.length)
      {
        DynArray dyn = (DynArray) from;
        array = dyn.get_elements_as_dyn_any();
      }
    else
      throw new TypeMismatch();
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

    gnuDynArray d =
      new gnuDynArray(official_type, final_type, factory, orb, false);
    d.array = c;
    return d;
  }

  /**
   * Get elements as array of anys.
   */
  public Any[] get_elements()
  {
    Any[] r = new Any[ array.length ];
    for (int i = 0; i < r.length; i++)
      r [ i ] = array [ i ].to_any();
    return r;
  }

  /** {@inheritDoc} */
  public DynAny[] get_elements_as_dyn_any()
  {
    DynAny[] a = new DynAny[ array.length ];
    for (int i = 0; i < a.length; i++)
      {
        a [ i ] = array [ i ].copy();
      }
    return a;
  }

  /**
   * Set elements when array of dyn anys is provided. This method can set nested
   * data structures as an array components.
   */
  public void set_elements_as_dyn_any(DynAny[] value)
                               throws InvalidValue, TypeMismatch
  {
    if (value.length != array.length)
      throw new InvalidValue(sizeMismatch(array.length, value.length));
    for (int i = 0; i < value.length; i++)
      {
        checkType(official_components, value [ i ].type());
        array [ i ].assign(value [ i ]);
      }
    pos = 0;
    valueChanged();
  }

  /**
   * Set elements when array of ordinary anys is provided.
   */
  public void set_elements(Any[] value)
                    throws InvalidValue, TypeMismatch
  {
    if (value.length != array.length)
      throw new InvalidValue(sizeMismatch(array.length, value.length));

    for (int i = 0; i < value.length; i++)
      {
        checkType(official_components, value [ i ].type());
        try
          {
            array [ i ] = factory.create_dyn_any(value [ i ]);
          }
        catch (InconsistentTypeCode e)
          {
            TypeMismatch t = new TypeMismatch();
            t.initCause(e);
            throw t;
          }
      }
    pos = 0;
    valueChanged();
  }

  /**
   * Done via reflection.
   */
  public Any to_any()
  {
    try
      {
        Streamable memberHolder =
          holderFactory.createHolder(official_components);

        if (memberHolder == null)
          memberHolder = holderFactory.createHolder(final_components);

        Class memberHolderClass = memberHolder.getClass();
        Class memberClass = memberHolderClass.getField("value").getType();

        Object members = Array.newInstance(memberClass, array.length);
        Object member;
        Any am;
        Field value = memberHolder.getClass().getField("value");

        for (int i = 0; i < array.length; i++)
          {
            // Recursive call should support multidimensional arrays.
            am = array [ i ].to_any();
            memberHolder = am.extract_Streamable();
            member = value.get(memberHolder);
            Array.set(members, i, member);
          }

        Streamable arrayHolder = holderFactory.createHolder(official_type);
        arrayHolder.getClass().getField("value").set(arrayHolder, members);

        Any g = createAny();
        g.insert_Streamable(arrayHolder);
        g.type(official_type);
        return g;
      }
    catch (Exception e)
      {
        throw new Unexpected(e);
      }
  }

  /**
   * Done via reflection.
   */
  public void from_any(Any an_any)
                throws TypeMismatch, InvalidValue
  {
    checkType(official_type, an_any.type());
    try
      {
        Streamable s = an_any.extract_Streamable();
        Object members = s.getClass().getField("value").get(s);

        checkArrayValid(members);

        Any member;
        Streamable holder;
        Class holderClass = null;

        for (int i = 0; i < array.length; i++)
          {
            if (holderClass == null)
              {
                holder = holderFactory.createHolder(official_components);
                if (holder == null)
                  holder = holderFactory.createHolder(final_components);
                holderClass = holder.getClass();
              }
            else
              holder = (Streamable) holderClass.newInstance();

            member = createAny();
            holder.getClass().getField("value").set(holder,
                                                    Array.get(members, i)
                                                   );
            member.insert_Streamable(holder);
            member.type(official_components);

            // This may lead to recursion, supporting multidimensional
            // arrays.
            array [ i ].from_any(member);
          }
      }
    catch (Exception ex)
      {
        TypeMismatch t = new TypeMismatch();
        t.initCause(ex);
        throw t;
      }
    valueChanged();
  }

  /**
   * Check if array size is valid and (for sequences) resized
   * if required. Called from from_any.
   */
  protected void checkArrayValid(Object members)
                          throws TypeMismatch, InvalidValue
  {
    if (array.length != Array.getLength(members))
      throw new InvalidValue(sizeMismatch(array.length, Array.getLength(members)));
  }
}