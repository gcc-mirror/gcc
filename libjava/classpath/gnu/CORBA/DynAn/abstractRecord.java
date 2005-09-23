/* abstractRecord.java --
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
import org.omg.CORBA.TypeCodePackage.Bounds;
import org.omg.CORBA.portable.Streamable;
import org.omg.DynamicAny.DynAny;
import org.omg.DynamicAny.DynAnyPackage.InvalidValue;
import org.omg.DynamicAny.DynAnyPackage.TypeMismatch;
import org.omg.DynamicAny.DynStruct;
import org.omg.DynamicAny.DynValueCommonOperations;
import org.omg.DynamicAny.NameDynAnyPair;
import org.omg.DynamicAny.NameValuePair;

import java.io.Serializable;

import java.lang.reflect.Field;

/**
 * A shared base for both dynamic structure an dynamic value final_type.
 *
 * @author Audrius Meskauskas, Lithuania (AudriusA@Bioinformatics.org)
 */
public abstract class abstractRecord
  extends anyDivideable
  implements DynAny, Serializable
{
  /**
   * Use serialVersionUID for interoperability.
   */
  private static final long serialVersionUID = 1;
  String[] fNames;

  /**
   * Creates the structure with the given typecode.
   *
   * @param fields The DynAny's, representing the fields of the structure.
   */
  public abstractRecord(TypeCode oType, TypeCode aType,
                        gnuDynAnyFactory aFactory, ORB anOrb
                       )
  {
    super(oType, aType, aFactory, anOrb);
  }

  /** @inheritDoc */
  public TCKind current_member_kind()
                             throws TypeMismatch, InvalidValue
  {
    if (array.length == 0)
      throw new TypeMismatch(EMPTY);
    try
      {
        return final_type.member_type(pos).kind();
      }
    catch (BadKind e)
      {
        TypeMismatch t = new TypeMismatch();
        t.initCause(e);
        throw t;
      }
    catch (Bounds e)
      {
        InvalidValue t = new InvalidValue();
        t.initCause(e);
        throw t;
      }
  }

  /** @inheritDoc */
  public String current_member_name()
                             throws TypeMismatch, InvalidValue
  {
    if (array.length == 0)
      throw new TypeMismatch(EMPTY);
    try
      {
        return final_type.member_name(pos);
      }
    catch (BadKind e)
      {
        TypeMismatch t = new TypeMismatch();
        t.initCause(e);
        throw t;
      }
    catch (Bounds e)
      {
        InvalidValue t = new InvalidValue();
        t.initCause(e);
        throw t;
      }
  }

  /**
   * Get content of the structure. This method must be defined on a different
   * name because get_members_as_dyn_any() throws exception only in some of the
   * supported interfaces.
   */
  public NameDynAnyPair[] gnu_get_members_as_dyn_any()
  {
    NameDynAnyPair[] r = new NameDynAnyPair[ array.length ];
    for (int i = 0; i < r.length; i++)
      {
        try
          {
            r [ i ] = new NameDynAnyPair(fNames [ i ], array [ i ]);
          }
        catch (Exception ex)
          {
            throw new Unexpected(ex);
          }
      }
    return r;
  }

  /**
   * Get content of the structure. This method must be defined on a different
   * name because get_members_as_dyn_any() throws exception only in some of the
   * supported interfaces.
   */
  public NameValuePair[] gnu_get_members()
  {
    NameValuePair[] r = new NameValuePair[ array.length ];
    for (int i = 0; i < r.length; i++)
      {
        try
          {
            r [ i ] = new NameValuePair(fNames [ i ], array [ i ].to_any());
          }
        catch (Exception ex)
          {
            throw new Unexpected(ex);
          }
      }
    return r;
  }

  /**
   * Set members from the provided array.
   */
  public void set_members_as_dyn_any(NameDynAnyPair[] value)
                              throws TypeMismatch, InvalidValue
  {
    if (value.length != array.length)
      throw new InvalidValue(sizeMismatch(array.length, value.length));

    for (int i = 0; i < value.length; i++)
      {
        DynAny dynAny = value [ i ].value;
        checkType(dynAny.type(), i);
        checkName(value [ i ].id, i);

        array [ i ] = dynAny;
      }
    pos = 0;
  }

  /**
   * Check the name at the given position ("" matches everything).
   */
  private void checkName(String xName, int i)
                  throws TypeMismatch
  {
    if (xName.length() > 0 && fNames [ i ].length() > 0)
      if (!xName.equals(fNames [ i ]))
        throw new TypeMismatch("Field name mismatch " + xName + " expected " +
                               fNames [ i ]
                              );
  }

  /**
   * Check the type at the given position.
   */
  private void checkType(TypeCode t, int i)
                  throws TypeMismatch
  {
    if (!array [ i ].type().equal(t))
      throw new TypeMismatch(typeMismatch(array [ i ].type(), t) + " field " +
                             i
                            );
  }

  /**
   * Set members from the provided array.
   */
  public void set_members(NameValuePair[] value)
                   throws TypeMismatch, InvalidValue
  {
    if (value.length != array.length)
      throw new InvalidValue(sizeMismatch(array.length, value.length));

    for (int i = 0; i < value.length; i++)
      {
        Any any = value [ i ].value;
        checkType(any.type(), i);
        checkName(value [ i ].id, i);

        array [ i ].from_any(any);
      }
    pos = 0;
  }

  /** @inheritDoc */
  public void assign(DynAny from)
              throws TypeMismatch
  {
    checkType(official_type, from.type());
    if (from instanceof DynStruct)
      {
        try
          {
            set_members_as_dyn_any(((DynStruct) from).get_members_as_dyn_any());
          }
        catch (InvalidValue e)
          {
            TypeMismatch t = new TypeMismatch("Invalid value");
            t.initCause(e);
            throw t;
          }
      }
    else
      throw new TypeMismatch("Not a DynStruct");
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

    abstractRecord d = newInstance(official_type, final_type, factory, orb);
    d.array = c;
    return d;
  }

  /**
   * Create a new instance when copying.
   */
  protected abstract abstractRecord newInstance(TypeCode oType, TypeCode aType,
                                                gnuDynAnyFactory aFactory,
                                                ORB anOrb
                                               );

  /**
   * Done via reflection.
   */
  public Any to_any()
  {
    try
      {
        Streamable sHolder = holderFactory.createHolder(official_type);

        Class sHolderClass = sHolder.getClass();
        Field sHolderValue = sHolderClass.getField("value");
        Class sClass = sHolderValue.getType();

        Object structure = sClass.newInstance();
        Object member;
        Any am;
        Field vread;
        Field vwrite;
        Streamable memberHolder;

        for (int i = 0; i < array.length; i++)
          {
            am = array [ i ].to_any();
            memberHolder = am.extract_Streamable();
            vwrite = structure.getClass().getField(final_type.member_name(i));
            vread = memberHolder.getClass().getField("value");
            member = vread.get(memberHolder);
            vwrite.set(structure, member);
          }

        Any g = createAny();
        sHolderValue.set(sHolder, structure);
        g.insert_Streamable(sHolder);
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
        if (s == null)
          {
            if (this instanceof DynValueCommonOperations)
              {
                ((DynValueCommonOperations) this).set_to_null();
                return;
              }
            else
              throw new InvalidValue(ISNULL);
          }

        Object structure = s.getClass().getField("value").get(s);
        if (structure == null && (this instanceof DynValueCommonOperations))
          {
            ((DynValueCommonOperations) this).set_to_null();
            return;
          }

        Any member;
        Streamable holder;
        Object field;
        TypeCode fType;
        Field fField;

        for (int i = 0; i < array.length; i++)
          {
            fField = structure.getClass().getField(fNames [ i ]);
            field = fField.get(structure);
            fType = array [ i ].type();
            holder = holderFactory.createHolder(fType);

            member = createAny();
            holder.getClass().getField("value").set(holder, field);
            member.insert_Streamable(holder);
            member.type(fType);

            array [ i ].from_any(member);
          }

        if (this instanceof DynValueCommonOperations)
          ((DynValueCommonOperations) this).set_to_value();
      }
    catch (InvalidValue v)
      {
        throw v;
      }
    catch (NoSuchFieldException ex)
      {
        TypeMismatch v =
          new TypeMismatch("holder value does not match typecode");
        v.initCause(ex);
        throw v;
      }
    catch (Exception ex)
      {
        TypeMismatch t = new TypeMismatch();
        t.initCause(ex);
        throw t;
      }
  }
}