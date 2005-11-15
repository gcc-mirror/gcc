/* DivideableAny.java --
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

import gnu.CORBA.TypeKindNamer;

import org.omg.CORBA.Any;
import org.omg.CORBA.CompletionStatus;
import org.omg.CORBA.ORB;
import org.omg.CORBA.Object;
import org.omg.CORBA.TypeCode;
import org.omg.CORBA.UNKNOWN;
import org.omg.DynamicAny.DynAny;
import org.omg.DynamicAny.DynAnyPackage.InvalidValue;
import org.omg.DynamicAny.DynAnyPackage.TypeMismatch;
import org.omg.DynamicAny.DynValueCommon;

import java.io.Serializable;

/**
 * Provides a base for DynAnys, having multiple components.
 *
 * @author Audrius Meskauskas, Lithuania (AudriusA@Bioinformatics.org)
 */
public abstract class DivideableAny
  extends AbstractAny
  implements Serializable
{
  /**
   * Use serialVersionUID for interoperability.
   */
  private static final long serialVersionUID = 1;

  /**
   * The array of the components that in general case may have different
   * final_type.
   */
  protected DynAny[] array;

  /**
   * The internal pointer.
   */
  protected int pos = 0;

  public DivideableAny(TypeCode oType, TypeCode aType,
                       gnuDynAnyFactory aFactory, ORB anOrb
                      )
  {
    super(oType, aType, aFactory, anOrb);
  }

  /**
   * Advance forward.
   */
  public boolean next()
  {
    pos++;
    return array.length > pos;
  }

  /**
   * Set zero position.
   */
  public void rewind()
  {
    pos = 0;
  }

  /**
   * Set a position.
   */
  public boolean seek(int p)
  {
    pos = p;
    return pos >= 0 && array.length > pos;
  }

  /**
   * Get the insertion point as DynAny. This method may throw exceptions if the
   * current insertion point does not support reading or insertion of the
   * primitive types.
   *
   * @return the focused component, from where the primitve value can be read or
   * where it can be inserted.
   * @throws InvalidValue if the primitive value cannot be inserted at the given
   * point.
   */
  protected DynAny focused()
                    throws InvalidValue, TypeMismatch
  {
    if (pos >= 0 && pos < array.length)
      {
        if (array [ pos ].component_count() == 0)
          return array [ pos ];
        else
          throw new TypeMismatch("Multiple coponents at " + pos);
      }
    else
      throw new InvalidValue("Out of bounds at " + pos + " valid 0.." +
                             (array.length - 1)
                            );
  }

  /** {@inheritDoc} */
  public int component_count()
  {
    return array.length;
  }

  /**
   * Return the second (enclosed any) that is stored in the wrapped Any.
   */
  public Any get_any()
              throws TypeMismatch, InvalidValue
  {
    return focused().get_any();
  }

  /** {@inheritDoc} */
  public boolean get_boolean()
                      throws TypeMismatch, InvalidValue
  {
    return focused().get_boolean();
  }

  /** {@inheritDoc} */
  public char get_char()
                throws TypeMismatch, InvalidValue
  {
    return focused().get_char();
  }

  /** {@inheritDoc} */
  public double get_double()
                    throws TypeMismatch, InvalidValue
  {
    return focused().get_double();
  }

  /** {@inheritDoc} */
  public float get_float()
                  throws TypeMismatch, InvalidValue
  {
    return focused().get_float();
  }

  /** {@inheritDoc} */
  public int get_long()
               throws TypeMismatch, InvalidValue
  {
    return focused().get_long();
  }

  /** {@inheritDoc} */
  public long get_longlong()
                    throws TypeMismatch, InvalidValue
  {
    return focused().get_longlong();
  }

  /** {@inheritDoc} */
  public byte get_octet()
                 throws TypeMismatch, InvalidValue
  {
    return focused().get_octet();
  }

  /** {@inheritDoc} */
  public Object get_reference()
                       throws TypeMismatch, InvalidValue
  {
    return focused().get_reference();
  }

  /** {@inheritDoc} */
  public short get_short()
                  throws TypeMismatch, InvalidValue
  {
    return focused().get_short();
  }

  /** {@inheritDoc} */
  public String get_string()
                    throws TypeMismatch, InvalidValue
  {
    return focused().get_string();
  }

  /** {@inheritDoc} */
  public TypeCode get_typecode()
                        throws TypeMismatch, InvalidValue
  {
    return focused().get_typecode();
  }

  /** {@inheritDoc} */
  public int get_ulong()
                throws TypeMismatch, InvalidValue
  {
    return focused().get_ulong();
  }

  /** {@inheritDoc} */
  public long get_ulonglong()
                     throws TypeMismatch, InvalidValue
  {
    return focused().get_ulonglong();
  }

  /** {@inheritDoc} */
  public short get_ushort()
                   throws TypeMismatch, InvalidValue
  {
    return focused().get_ushort();
  }

  /** {@inheritDoc} */
  public Serializable get_val()
                       throws TypeMismatch, InvalidValue
  {
    if (pos >= 0 && pos < array.length)
      {
        if (array [ pos ] instanceof DynValueCommon)
          return array [ pos ].get_val();
        else
          throw new TypeMismatch();
      }
    else
      throw new InvalidValue("Out of bounds at " + pos + " valid 0.." +
                             (array.length - 1)
                            );
  }

  /** {@inheritDoc} */
  public char get_wchar()
                 throws TypeMismatch, InvalidValue
  {
    return focused().get_wchar();
  }

  /** {@inheritDoc} */
  public String get_wstring()
                     throws TypeMismatch, InvalidValue
  {
    return focused().get_wstring();
  }

  /** {@inheritDoc} */
  public void insert_any(Any a_x)
                  throws TypeMismatch, InvalidValue
  {
    focused().insert_any(a_x);
    valueChanged();
  }

  /** {@inheritDoc} */
  public void insert_boolean(boolean a_x)
                      throws InvalidValue, TypeMismatch
  {
    focused().insert_boolean(a_x);
    valueChanged();
  }

  /** {@inheritDoc} */
  public void insert_char(char a_x)
                   throws InvalidValue, TypeMismatch
  {
    focused().insert_char(a_x);
    valueChanged();
  }

  /** {@inheritDoc} */
  public void insert_double(double a_x)
                     throws InvalidValue, TypeMismatch
  {
    focused().insert_double(a_x);
    valueChanged();
  }

  /** {@inheritDoc} */
  public void insert_float(float a_x)
                    throws InvalidValue, TypeMismatch
  {
    focused().insert_float(a_x);
    valueChanged();
  }

  /** {@inheritDoc} */
  public void insert_long(int a_x)
                   throws InvalidValue, TypeMismatch
  {
    focused().insert_long(a_x);
    valueChanged();
  }

  /** {@inheritDoc} */
  public void insert_longlong(long a_x)
                       throws InvalidValue, TypeMismatch
  {
    focused().insert_longlong(a_x);
    valueChanged();
  }

  /** {@inheritDoc} */
  public void insert_octet(byte a_x)
                    throws InvalidValue, TypeMismatch
  {
    focused().insert_octet(a_x);
    valueChanged();
  }

  /** {@inheritDoc} */
  public void insert_reference(Object a_x)
                        throws InvalidValue, TypeMismatch
  {
    focused().insert_reference(a_x);
    valueChanged();
  }

  /** {@inheritDoc} */
  public void insert_short(short a_x)
                    throws InvalidValue, TypeMismatch
  {
    focused().insert_short(a_x);
    valueChanged();
  }

  /** {@inheritDoc} */
  public void insert_string(String a_x)
                     throws InvalidValue, TypeMismatch
  {
    focused().insert_string(a_x);
    valueChanged();
  }

  /** {@inheritDoc} */
  public void insert_typecode(TypeCode a_x)
                       throws InvalidValue, TypeMismatch
  {
    focused().insert_typecode(a_x);
    valueChanged();
  }

  /** {@inheritDoc} */
  public void insert_ulong(int a_x)
                    throws InvalidValue, TypeMismatch
  {
    focused().insert_ulong(a_x);
    valueChanged();
  }

  /** {@inheritDoc} */
  public void insert_ulonglong(long a_x)
                        throws InvalidValue, TypeMismatch
  {
    focused().insert_ulonglong(a_x);
    valueChanged();
  }

  /** {@inheritDoc} */
  public void insert_ushort(short a_x)
                     throws InvalidValue, TypeMismatch
  {
    focused().insert_ushort(a_x);
    valueChanged();
  }

  /** {@inheritDoc} */
  public void insert_val(Serializable a_x)
                  throws InvalidValue, TypeMismatch
  {
    if (pos >= 0 && pos < array.length)
      {
        if (array [ pos ] instanceof DynValueCommon)
          array [ pos ].insert_val(a_x);
        else
          throw new TypeMismatch();
      }
    else
      throw new InvalidValue("Out of bounds at " + pos + " valid 0.." +
                             (array.length - 1)
                            );
    valueChanged();
  }

  /** {@inheritDoc} */
  public void insert_wchar(char a_x)
                    throws InvalidValue, TypeMismatch
  {
    focused().insert_wchar(a_x);
    valueChanged();
  }

  /** {@inheritDoc} */
  public void insert_wstring(String a_x)
                      throws InvalidValue, TypeMismatch
  {
    focused().insert_wstring(a_x);
    valueChanged();
  }

  /** {@inheritDoc} */
  public DynAny get_dyn_any()
                     throws TypeMismatch, InvalidValue
  {
    return focused().get_dyn_any();
  }

  /** {@inheritDoc} */
  public void insert_dyn_any(DynAny insert_it)
                      throws TypeMismatch, InvalidValue
  {
    focused().insert_dyn_any(insert_it);
  }

  /**
   * Get current component.
   *
   * @return current component or <code>null</code> if the pointer is out of
   * bounds.
   */
  public DynAny current_component()
                           throws TypeMismatch
  {
    if (array.length == 0)
      throw new TypeMismatch("empty");
    return (pos >= 0 && pos < array.length) ? array [ pos ] : null;
  }

  /**
   * No action, cleanup is done by garbage collector in java.
   */
  public void destroy()
  {
  }

  /**
   * Involved in equal(DynAny).
   */
  public abstract Any to_any()
                      throws TypeMismatch;

  /**
   * Compares with other DynAny for equality. The final_type, array size and
   * array members must match.
   */
  public boolean equal(DynAny other)
  {
    try
      {
        if (!official_type.equal(other.type()))
          return false;
        else if (other instanceof DivideableAny)
          {
            DivideableAny x = (DivideableAny) other;
            if (x.array.length != array.length)
              return false;

            for (int i = 0; i < array.length; i++)
              {
                if (!array [ i ].equal(x.array [ i ]))
                  return false;
              }
            return true;
          }
        else if (other == null || other instanceof AbstractAny)
          return false;
        else
          return other.to_any().equal(to_any());
      }
    catch (TypeMismatch e)
      {
        UNKNOWN u = new UNKNOWN(MINOR, CompletionStatus.COMPLETED_NO);
        u.initCause(e);
        throw u;
      }
  }
}