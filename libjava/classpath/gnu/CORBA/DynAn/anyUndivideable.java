/* Undivideable.java --
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

import java.io.Serializable;

import org.omg.CORBA.Any;
import org.omg.CORBA.ORB;
import org.omg.CORBA.Object;
import org.omg.CORBA.TypeCode;
import org.omg.DynamicAny.DynAny;
import org.omg.DynamicAny.DynAnyPackage.InvalidValue;
import org.omg.DynamicAny.DynAnyPackage.TypeMismatch;

/**
 * Represent DynAny that has no internal components (DynEnum and so on). The
 * methods, related to internal components, throw exceptions or return agreed
 * values like null.
 *
 * @author Audrius Meskauskas, Lithuania (AudriusA@Bioinformatics.org)
 */
public abstract class anyUndivideable
  extends abstractDynAny
  implements Serializable
{
  /**
   * Use serialVersionUID for interoperability.
   */
  private static final long serialVersionUID = 1;

  /**
   * Create a new instance with the given typecode.
   */
  public anyUndivideable(TypeCode oType, TypeCode aType,
                         gnuDynAnyFactory aFactory, ORB anOrb)
  {
    super(oType, aType, aFactory, anOrb);
  }

  /**
   * There are no components.
   *
   * @return 0, always.
   */
  public int component_count()
  {
    return 0;
  }

  /**
   * There is no current component.
   *
   * @throws TypeMismatch, always.
   */
  public DynAny current_component()
    throws TypeMismatch
  {
    throw new TypeMismatch("Not applicable");
  }

  /**
   * Returns without action.
   */
  public void destroy()
  {
  }

  /**
   * Not in use.
   */
  public Any get_any()
    throws TypeMismatch, InvalidValue
  {
    throw new TypeMismatch();
  }

  /**
   * Not in use.
   */
  public boolean get_boolean()
    throws TypeMismatch, InvalidValue
  {
    throw new TypeMismatch();
  }

  /**
   * Not in use.
   */
  public char get_char()
    throws TypeMismatch, InvalidValue
  {
    throw new TypeMismatch();
  }

  /**
   * Not in use.
   */
  public double get_double()
    throws TypeMismatch, InvalidValue
  {
    throw new TypeMismatch();
  }

  /**
   * Not in use.
   */
  public DynAny get_dyn_any()
    throws TypeMismatch, InvalidValue
  {
    throw new TypeMismatch();
  }

  /**
   * Not in use.
   */
  public float get_float()
    throws TypeMismatch, InvalidValue
  {
    throw new TypeMismatch();
  }

  /**
   * Not in use.
   */
  public int get_long()
    throws TypeMismatch, InvalidValue
  {
    throw new TypeMismatch();
  }

  /**
   * Not in use.
   */
  public long get_longlong()
    throws TypeMismatch, InvalidValue
  {
    throw new TypeMismatch();
  }

  /**
   * Not in use.
   */
  public byte get_octet()
    throws TypeMismatch, InvalidValue
  {
    throw new TypeMismatch();
  }

  /**
   * Not in use.
   */
  public Object get_reference()
    throws TypeMismatch, InvalidValue
  {
    throw new TypeMismatch();
  }

  /**
   * Not in use.
   */
  public short get_short()
    throws TypeMismatch, InvalidValue
  {
    throw new TypeMismatch();
  }

  /**
   * Not in use.
   */
  public String get_string()
    throws TypeMismatch, InvalidValue
  {
    throw new TypeMismatch();
  }

  /**
   * Not in use.
   */
  public TypeCode get_typecode()
    throws TypeMismatch, InvalidValue
  {
    throw new TypeMismatch();
  }

  /**
   * Not in use.
   */
  public int get_ulong()
    throws TypeMismatch, InvalidValue
  {
    throw new TypeMismatch();
  }

  /**
   * Not in use.
   */
  public long get_ulonglong()
    throws TypeMismatch, InvalidValue
  {
    throw new TypeMismatch();
  }

  /**
   * Not in use.
   */
  public short get_ushort()
    throws TypeMismatch, InvalidValue
  {
    throw new TypeMismatch();
  }

  /**
   * Not in use.
   */
  public Serializable get_val()
    throws TypeMismatch, InvalidValue
  {
    throw new TypeMismatch();
  }

  /**
   * Not in use.
   */
  public char get_wchar()
    throws TypeMismatch, InvalidValue
  {
    throw new TypeMismatch();
  }

  /**
   * Not in use.
   */
  public String get_wstring()
    throws TypeMismatch, InvalidValue
  {
    throw new TypeMismatch();
  }

  /**
   * Not in use.
   */
  public void insert_any(Any an_any)
    throws TypeMismatch, InvalidValue
  {
    throw new TypeMismatch();
  }

  /**
   * Not in use.
   */
  public void insert_boolean(boolean a_x)
    throws InvalidValue, TypeMismatch
  {
    throw new TypeMismatch();
  }

  /**
   * Not in use.
   */
  public void insert_char(char a_x)
    throws InvalidValue, TypeMismatch
  {
    throw new TypeMismatch();
  }

  /**
   * Not in use.
   */
  public void insert_double(double a_x)
    throws InvalidValue, TypeMismatch
  {
    throw new TypeMismatch();
  }

  /**
   * Not in use.
   */
  public void insert_dyn_any(DynAny insert_it)
    throws TypeMismatch, InvalidValue
  {
    throw new TypeMismatch();
  }

  /**
   * Not in use.
   */
  public void insert_float(float a_x)
    throws InvalidValue, TypeMismatch
  {
    throw new TypeMismatch();
  }

  /**
   * Not in use.
   */
  public void insert_long(int a_x)
    throws InvalidValue, TypeMismatch
  {
    throw new TypeMismatch();
  }

  /**
   * Not in use.
   */
  public void insert_longlong(long a_x)
    throws InvalidValue, TypeMismatch
  {
    throw new TypeMismatch();
  }

  /**
   * Not in use.
   */
  public void insert_octet(byte a_x)
    throws InvalidValue, TypeMismatch
  {
    throw new TypeMismatch();
  }

  /**
   * Not in use.
   */
  public void insert_reference(Object a_x)
    throws InvalidValue, TypeMismatch
  {
    throw new TypeMismatch();
  }

  /**
   * Not in use.
   */
  public void insert_short(short a_x)
    throws InvalidValue, TypeMismatch
  {
    throw new TypeMismatch();
  }

  /**
   * Not in use.
   */
  public void insert_string(String a_x)
    throws InvalidValue, TypeMismatch
  {
    throw new TypeMismatch();
  }

  /**
   * Not in use.
   */
  public void insert_typecode(TypeCode a_x)
    throws InvalidValue, TypeMismatch
  {
    throw new TypeMismatch();
  }

  /**
   * Not in use.
   */
  public void insert_ulong(int a_x)
    throws InvalidValue, TypeMismatch
  {
    throw new TypeMismatch();
  }

  /**
   * Not in use.
   */
  public void insert_ulonglong(long a_x)
    throws InvalidValue, TypeMismatch
  {
    throw new TypeMismatch();
  }

  /**
   * Not in use.
   */
  public void insert_ushort(short a_x)
    throws InvalidValue, TypeMismatch
  {
    throw new TypeMismatch();
  }

  /**
   * Not in use.
   */
  public void insert_val(Serializable a_x)
    throws InvalidValue, TypeMismatch
  {
    throw new TypeMismatch();
  }

  /**
   * Not in use.
   */
  public void insert_wchar(char a_x)
    throws InvalidValue, TypeMismatch
  {
    throw new TypeMismatch();
  }

  /**
   * Not in use.
   */
  public void insert_wstring(String a_x)
    throws InvalidValue, TypeMismatch
  {
    throw new TypeMismatch();
  }

  /**
   * Not in use.
   */
  public boolean next()
  {
    return false;
  }

  /**
   * Not in use.
   */
  public void rewind()
  {
  }

  /**
   * Not in use.
   */
  public boolean seek(int p)
  {
    return false;
  }

  /**
   * Get the typecode of this enumeration.
   */
  public TypeCode type()
  {
    return official_type;
  }

  /**
   * Compares with other DynAny for equality.
   */
  public boolean equals(java.lang.Object other)
  {
    if (other instanceof DynAny)
      return equal((DynAny) other);
    else
      return false;
  }

  /**
   * This depends on an object.
   */
  public abstract boolean equal(DynAny other);

}
