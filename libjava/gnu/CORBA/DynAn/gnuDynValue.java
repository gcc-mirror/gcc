/* gnuDynValue.java --
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

import gnu.CORBA.Minor;
import gnu.CORBA.Unexpected;

import org.omg.CORBA.Any;
import org.omg.CORBA.BAD_PARAM;
import org.omg.CORBA.MARSHAL;
import org.omg.CORBA.ORB;
import org.omg.CORBA.TCKind;
import org.omg.CORBA.TypeCode;
import org.omg.CORBA.VM_TRUNCATABLE;
import org.omg.CORBA.portable.OutputStream;
import org.omg.CORBA.portable.ValueFactory;
import org.omg.DynamicAny.DynAny;
import org.omg.DynamicAny.DynAnyPackage.InvalidValue;
import org.omg.DynamicAny.DynAnyPackage.TypeMismatch;
import org.omg.DynamicAny.DynStruct;
import org.omg.DynamicAny.DynValue;
import org.omg.DynamicAny.DynValueCommon;
import org.omg.DynamicAny.DynValueOperations;
import org.omg.DynamicAny.NameDynAnyPair;
import org.omg.DynamicAny.NameValuePair;

import java.io.Serializable;

/**
 * Implementation of DynValue.
 *
 * @author Audrius Meskauskas, Lithuania (AudriusA@Bioinformatics.org)
 */
public class gnuDynValue extends RecordAny implements DynValue,
  Serializable
{
  /**
   * Use serialVersionUID for interoperability.
   */
  private static final long serialVersionUID = 1;

  /**
   * If true, the value of this ValueType is set to null.
   */
  boolean isNull;

  /**
   * Create an instance.
   */
  public gnuDynValue(TypeCode oType, TypeCode aType,
    gnuDynAnyFactory aFactory, ORB anOrb
  )
  {
    super(oType, aType, aFactory, anOrb);

    // Initialise fields. The array of fields also includes all inherited
    // fields.
    try
      {
        array = new DynAny[ final_type.member_count() ];
        fNames = new String[ array.length ];
        for (int i = 0; i < array.length; i++)
          {
            array [ i ] =
              factory.create_dyn_any_from_type_code(final_type.member_type(i));
            fNames [ i ] = final_type.member_name(i);
          }

        // Search of inherited members.
        if (final_type.type_modifier() == VM_TRUNCATABLE.value)
          {
            TypeCode parent = final_type.concrete_base_type();
            DynAny ancestor = factory.create_dyn_any_from_type_code(parent);

            if (ancestor instanceof DynValue)
              {
                // Add members of ancestor in front of the curren members.
                DynValue anc = (DynValue) ancestor;
                anc.set_to_value();

                NameDynAnyPair[] aar = anc.get_members_as_dyn_any();
                inheritFields(aar);
              }
            else if (ancestor instanceof DynStruct)
              {
                // Add members of ancestor in front of the curren members.
                DynStruct anc = (DynStruct) ancestor;
                NameDynAnyPair[] aar = anc.get_members_as_dyn_any();
                inheritFields(aar);
              }
            else
              throw new BAD_PARAM("The parent of " + final_type.id() + ", " +
                parent.id() + ", is not structure nor value."
              );
          }
      }
    catch (Exception e)
      {
        throw new Unexpected(e);
      }

    set_to_null();
  }

  /**
   * Inherit the provided fields.
   */
  private void inheritFields(NameDynAnyPair[] aar)
  {
    DynAny[] nArray = new DynAny[ array.length + aar.length ];
    String[] nNames = new String[ array.length + aar.length ];
    int p = 0;
    for (int i = 0; i < aar.length; i++)
      {
        nArray [ p ] = aar [ i ].value;
        nNames [ p ] = aar [ i ].id;
        p++;
      }

    for (int i = 0; i < array.length; i++)
      {
        nArray [ p ] = array [ i ];
        nNames [ p ] = fNames [ i ];
        p++;
      }

    array = nArray;
    fNames = nNames;
  }

  /** @inheritDoc */
  public TCKind current_member_kind() throws TypeMismatch, InvalidValue
  {
    if (isNull)
      throw new TypeMismatch(ISNULL);
    else
      return super.current_member_kind();
  }
  ;

  /** @inheritDoc */
  public String current_member_name() throws TypeMismatch, InvalidValue
  {
    if (isNull)
      throw new TypeMismatch(ISNULL);
    else
      return super.current_member_name();
  }
  ;

  /** @inheritDoc */
  public NameDynAnyPair[] get_members_as_dyn_any() throws InvalidValue
  {
    if (isNull)
      throw new InvalidValue(ISNULL);
    return super.gnu_get_members_as_dyn_any();
  }
  ;

  /** @inheritDoc */
  public NameValuePair[] get_members() throws InvalidValue
  {
    if (isNull)
      throw new InvalidValue(ISNULL);
    else
      return super.gnu_get_members();
  }
  ;

  /** @inheritDoc */
  public void set_members_as_dyn_any(NameDynAnyPair[] value)
    throws TypeMismatch, InvalidValue
  {
    super.set_members_as_dyn_any(value);
    isNull = false;
  }
  ;

  /** @inheritDoc */
  public void set_members(NameValuePair[] value)
    throws TypeMismatch, InvalidValue
  {
    super.set_members(value);
    isNull = false;
  }
  ;

  /** @inheritDoc */
  public boolean is_null()
  {
    return isNull;
  }

  /** @inheritDoc */
  public void set_to_null()
  {
    isNull = true;
    valueChanged();
  }

  /** @inheritDoc */
  public void set_to_value()
  {
    isNull = false;
    valueChanged();
  }

  /**
   * Create a new instance.
   */
  protected RecordAny newInstance(TypeCode oType, TypeCode aType,
    gnuDynAnyFactory aFactory, ORB anOrb
  )
  {
    gnuDynValue v = new gnuDynValue(oType, aType, aFactory, anOrb);
    if (isNull)
      v.set_to_null();
    else
      v.set_to_value();
    return v;
  }

  /**
   * Compare for equality, minding null values.
   */
  public boolean equal(DynAny other)
  {
    if (other instanceof DynValueOperations)
      {
        DynValueCommon o = (DynValueCommon) other;
        if (isNull)
          return o.is_null() && o.type().equal(official_type);
        else
          return !o.is_null() && record_equal(other); // GCJ LOCAL bug #24938
      }
    else
      return false;
  }

  /**
   * Get the focused component, throwing exception if the current value is null.
   */
  protected DynAny focused() throws InvalidValue, TypeMismatch
  {
    if (isNull)
      throw new TypeMismatch(ISNULL);
    else
      return super.focused();
  }

  /**
   * Convert into Any.
   */
  public Any to_any()
  {
    if (isNull)
      {
        Any a0 = createAny();
        a0.type(orb.get_primitive_tc(TCKind.tk_null));
        return a0;
      }
    else
      {
        try
          {
            ValueFactory factory =
              ((org.omg.CORBA_2_3.ORB) orb).lookup_value_factory(official_type.id());
            if (factory == null)
              {
                MARSHAL m = new MARSHAL("Factory for " + official_type.id() +
                " not registered.");
                m.minor = Minor.Factory;
                throw m;
              }

            OutputStream out = orb.create_output_stream();

            for (int i = 0; i < array.length; i++)
              array [ i ].to_any().write_value(out);

            org.omg.CORBA_2_3.portable.InputStream in =
              (org.omg.CORBA_2_3.portable.InputStream) out.create_input_stream();
            Serializable v = factory.read_value(in);

            Any g = createAny();
            g.type(official_type);
            g.insert_Value(v, official_type);

            return g;
          }
        catch (Exception e)
          {
            throw new Unexpected(e);
          }
      }
  }

  /** @inheritDoc */
  public void assign(DynAny from) throws TypeMismatch
  {
    checkType(official_type, from.type());

    if (from instanceof DynValue)
      {
        DynValue other = (DynValue) from;
        if (other.is_null())
          set_to_null();
        else
          {
            set_to_value();
            try
              {
                DynValueOperations src = (DynValueOperations) from;
                set_members_as_dyn_any(src.get_members_as_dyn_any());
              }
            catch (InvalidValue e)
              {
                TypeMismatch t = new TypeMismatch("Invalid value");
                t.initCause(e);
                throw t;
              }
          }
      }
    else
      throw new TypeMismatch("Not a DynValue");
  }

  /**
   * Get the number of components.
   */
  public int component_count()
  {
    return isNull ? 0 : record_component_count(); // GCJ LOCAL bug #24938
  }

  /** {@inheritDoc} */
  public Serializable get_val() throws TypeMismatch, InvalidValue
  {
    return to_any().extract_Value();
  }

  /** {@inheritDoc} */
  public void insert_val(Serializable a_x) throws InvalidValue, TypeMismatch
  {
    Any a = to_any();
    a.insert_Value(a_x);
    from_any(a);
    valueChanged();
  }
}
