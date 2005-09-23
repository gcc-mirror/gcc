/* gnuDynValueBox.java --
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
import org.omg.CORBA.ORB;
import org.omg.CORBA.TCKind;
import org.omg.CORBA.TypeCode;
import org.omg.CORBA.TypeCodePackage.BadKind;
import org.omg.CORBA.portable.Streamable;
import org.omg.DynamicAny.DynAny;
import org.omg.DynamicAny.DynAnyFactoryPackage.InconsistentTypeCode;
import org.omg.DynamicAny.DynAnyPackage.InvalidValue;
import org.omg.DynamicAny.DynAnyPackage.TypeMismatch;
import org.omg.DynamicAny.DynValueBox;
import org.omg.DynamicAny.DynValueBoxOperations;
import org.omg.DynamicAny.DynValueCommon;

import java.io.Serializable;

import java.lang.reflect.Field;

/**
 * Implementation of the DynValueBox.
 *
 * @author Audrius Meskauskas, Lithuania (AudriusA@Bioinformatics.org)
 */
public class gnuDynValueBox
  extends anyDivideable
  implements DynValueBox, Serializable
{
  /**
   * Use serialVersionUID for interoperability.
   */
  private static final long serialVersionUID = 1;

  /**
   * The final_type of contents of this value box.
   */
  final TypeCode content;

  /**
   * The string for some TypeMismatch exceptions.
   */
  String CONTENT = "Box content final_type mismatch";

  /**
   * Create a new instance of gnuDynValueBox.
   */
  public gnuDynValueBox(TypeCode oType, TypeCode aType,
                        gnuDynAnyFactory aFactory, ORB anOrb
                       )
  {
    super(oType, aType, aFactory, anOrb);
    try
      {
        content = final_type.content_type();
        array = new DynAny[] { factory.create_dyn_any_from_type_code(content) };
        set_to_null();
      }
    catch (Exception e)
      {
        throw new Unexpected(e);
      }
  }

  /** @inheritDoc */
  public void assign(DynAny from)
              throws TypeMismatch
  {
    checkType(official_type, from.type());
    if (from instanceof DynValueBoxOperations)
      {
        DynValueBoxOperations other = (DynValueBoxOperations) from;
        if (other.is_null())
          set_to_null();
        else
          {
            DynAny inBox;
            try
              {
                inBox = other.get_boxed_value_as_dyn_any();
              }
            catch (InvalidValue e)
              {
                TypeMismatch t = new TypeMismatch("Invalid value");
                t.initCause(e);
                throw t;
              }
            if (!content.equal(inBox.type()))
              throw new TypeMismatch(CONTENT);
            array = new DynAny[] { inBox.copy() };
          }
      }
    valueChanged();
  }

  /** @inheritDoc */
  public DynAny copy()
  {
    gnuDynValueBox other =
      new gnuDynValueBox(official_type, final_type, factory, orb);
    if (is_null())
      other.set_to_null();
    else
      {
        try
          {
            other.array = new DynAny[] { array [ 0 ].copy() };
          }
        catch (Exception e)
          {
            throw new Unexpected(e);
          }
      }
    return other;
  }

  /**
   * Returns null for null value, delegates to super. otherwise.
   */
  public DynAny current_component()
                           throws TypeMismatch
  {
    if (is_null())
      return null;
    else
      return super.current_component();
  }

  /**
   * Compare for equality, minding null values.
   */
  public boolean equal(DynAny other)
  {
    if (other instanceof DynValueCommon)
      {
        DynValueCommon o = (DynValueCommon) other;
        if (is_null())
          return o.is_null() && o.type().equal(official_type);
        else
          return !o.is_null() && super.equal(other);
      }
    else
      return false;
  }

  /** @inheritDoc */
  public void from_any(Any an_any)
                throws TypeMismatch, InvalidValue
  {
    checkType(official_type, an_any.type());
    try
      {
        if (!an_any.type().content_type().equal(content))
          throw new InvalidValue(CONTENT);
      }
    catch (BadKind e)
      {
        TypeMismatch t = new TypeMismatch("Not a box");
        t.initCause(e);
        throw t;
      }

    Serializable s = an_any.extract_Value();
    if (s == null)
      set_to_null();
    else
      {
        try
          {
            Streamable holder = holderFactory.createHolder(content);
            Field v = holder.getClass().getField("value");
            v.set(holder, s);

            Any cont = createAny();
            cont.insert_Streamable(holder);

            array = new DynAny[] { factory.create_dyn_any(cont) };
          }
        catch (Exception ex)
          {
            throw new Unexpected(ex);
          }
      }
    valueChanged();
  }

  /** @inheritDoc */
  public Any get_boxed_value()
                      throws InvalidValue
  {
    try
      {
        if (is_null())
          throw new InvalidValue(ISNULL);
        else
          return array [ 0 ].to_any();
      }
    catch (Exception e)
      {
        InvalidValue t = new InvalidValue();
        t.initCause(e);
        throw t;
      }
  }

  /** @inheritDoc */
  public DynAny get_boxed_value_as_dyn_any()
                                    throws InvalidValue
  {
    if (is_null())
      throw new InvalidValue(ISNULL);
    else
      return array [ 0 ].copy();
  }

  /** {@inheritDoc} */
  public Serializable get_val()
                       throws TypeMismatch, InvalidValue
  {
    return to_any().extract_Value();
  }

  /** {@inheritDoc} */
  public void insert_val(Serializable a_x)
                  throws InvalidValue, TypeMismatch
  {
    Any a = to_any();
    a.insert_Value(a_x);
    from_any(a);
    valueChanged();
  }

  /** @inheritDoc */
  public boolean is_null()
  {
    return array.length == 0;
  }

  /** @inheritDoc */
  public void set_boxed_value(Any boxIt)
                       throws TypeMismatch
  {
    if (!content.equal(boxIt.type()))
      throw new TypeMismatch(CONTENT);
    try
      {
        if (is_null())
          {
            array = new DynAny[] { factory.create_dyn_any(boxIt) };
          }
        else
          {
            array [ 0 ].from_any(boxIt);
          }
      }
    catch (Exception e)
      {
        TypeMismatch t = new TypeMismatch();
        t.initCause(e);
        throw t;
      }
    valueChanged();
  }

  /** @inheritDoc */
  public void set_boxed_value_as_dyn_any(DynAny boxIt)
                                  throws TypeMismatch
  {
    if (!content.equal(boxIt.type()))
      throw new TypeMismatch(CONTENT);
    try
      {
        if (is_null())
          {
            array = new DynAny[] { boxIt.copy() };
          }
        else
          {
            array [ 0 ].assign(boxIt);
          }
      }
    catch (Exception e)
      {
        TypeMismatch t = new TypeMismatch();
        t.initCause(e);
        throw t;
      }
    valueChanged();
  }

  /** @inheritDoc */
  public void set_to_null()
  {
    array = new DynAny[ 0 ];
    valueChanged();
  }

  /** @inheritDoc */
  public void set_to_value()
  {
    try
      {
        if (array.length == 0)
          {
            array =
              new DynAny[] { factory.create_dyn_any_from_type_code(content) };
          }
      }
    catch (InconsistentTypeCode e)
      {
        throw new Unexpected(e);
      }
    valueChanged();
  }

  /** @inheritDoc */
  public Any to_any()
  {
    Any a = createAny();

    if (!is_null())
      {
        try
          {
            Streamable holder;
            if (array [ 0 ] instanceof gnuDynAny)
              holder = ((gnuDynAny) array [ 0 ]).holder;
            else
              {
                Any uan = array [ 0 ].to_any();
                holder = uan.extract_Streamable();
              }

            Field v = holder.getClass().getField("value");
            Serializable value = (Serializable) v.get(holder);
            a.type(official_type);
            a.insert_Value(value, content);
          }
        catch (Exception ex)
          {
            throw new Unexpected(ex);
          }
      }
    else
      a.type(orb.get_primitive_tc(TCKind.tk_null));
    return a;
  }
}