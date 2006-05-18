/* _DynValueStub.java --
   Copyright (C) 2005, 2006 Free Software Foundation, Inc.

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


package org.omg.DynamicAny;

import org.omg.CORBA.Any;
import org.omg.CORBA.MARSHAL;
import org.omg.CORBA.TCKind;
import org.omg.CORBA.TypeCode;
import org.omg.CORBA.portable.Delegate;
import org.omg.CORBA.portable.ObjectImpl;
import org.omg.DynamicAny.DynAnyPackage.InvalidValue;
import org.omg.DynamicAny.DynAnyPackage.TypeMismatch;

import java.io.Serializable;

/**
 * Should provide support for remote invocation of methods on DynValue. As
 * DynValue can never be remote at least till 1.5 inclusive, this class is
 * not in use.
 *
 * @author Audrius Meskauskas, Lithuania (AudriusA@Bioinformatics.org)
 */
public class _DynValueStub
  extends ObjectImpl
  implements DynValue, Serializable
{
  /**
   * Use serialVersionUID (v1.4) for interoperability.
   */
  private static final long serialVersionUID = 5815313794012360824L;

  /**
   * The purpose and value of this field are not documented.
   */
  public static final Class _opsClass = DynValueOperations.class;

  /**
   * Create the DynValue stub. To get the stub working,
   * you must later set the delegate with
   * {@link org.omg.CORBA.portable.ObjectImpl#_set_delegate(Delegate)}.
   */
  public _DynValueStub()
  {
  }

  /**
   * Return the array of repository ids for this object.
   */
  public String[] _ids()
  {
    return new String[] { DynValueHelper.id() };
  }

  /**
   * The remote call of DynAny methods is not possible.
   *
   * @throws MARSHAL, always.
   */
  public TCKind current_member_kind()
                             throws TypeMismatch, InvalidValue
  {
    throw new MARSHAL(_DynAnyStub.NOT_APPLICABLE);
  }

  /**
   * The remote call of DynAny methods is not possible.
   *
   * @throws MARSHAL, always.
   */
  public String current_member_name()
                             throws TypeMismatch, InvalidValue
  {
    throw new MARSHAL(_DynAnyStub.NOT_APPLICABLE);
  }

  /**
   * The remote call of DynAny methods is not possible.
   *
   * @throws MARSHAL, always.
   */
  public NameValuePair[] get_members()
                              throws InvalidValue
  {
    throw new MARSHAL(_DynAnyStub.NOT_APPLICABLE);
  }

  /**
   * The remote call of DynAny methods is not possible.
   *
   * @throws MARSHAL, always.
   */
  public NameDynAnyPair[] get_members_as_dyn_any()
                                          throws InvalidValue
  {
    throw new MARSHAL(_DynAnyStub.NOT_APPLICABLE);
  }

  /**
   * The remote call of DynAny methods is not possible.
   *
   * @throws MARSHAL, always.
   */
  public void set_members(NameValuePair[] a_members)
                   throws TypeMismatch, InvalidValue
  {
    throw new MARSHAL(_DynAnyStub.NOT_APPLICABLE);
  }

  /**
   * The remote call of DynAny methods is not possible.
   *
   * @throws MARSHAL, always.
   */
  public void set_members_as_dyn_any(NameDynAnyPair[] a_members)
                              throws TypeMismatch, InvalidValue
  {
    throw new MARSHAL(_DynAnyStub.NOT_APPLICABLE);
  }

  /**
   * The remote call of DynAny methods is not possible.
   *
   * @throws MARSHAL, always.
   */
  public boolean is_null()
  {
    throw new MARSHAL(_DynAnyStub.NOT_APPLICABLE);
  }

  /**
   * The remote call of DynAny methods is not possible.
   *
   * @throws MARSHAL, always.
   */
  public void set_to_null()
  {
    throw new MARSHAL(_DynAnyStub.NOT_APPLICABLE);
  }

  /**
   * The remote call of DynAny methods is not possible.
   *
   * @throws MARSHAL, always.
   */
  public void set_to_value()
  {
    throw new MARSHAL(_DynAnyStub.NOT_APPLICABLE);
  }
  
  /**
   * The remote call of DynAny methods is not possible.
   *
   * @throws MARSHAL, always.
   */
  public TypeCode type()
  {
    throw new MARSHAL(_DynAnyStub.NOT_APPLICABLE);
  }

  /**
   * The remote call of DynAny methods is not possible.
   *
   * @throws MARSHAL, always.
   */
  public boolean next()
  {
    throw new MARSHAL(_DynAnyStub.NOT_APPLICABLE);
  }

  /**
   * The remote call of DynAny methods is not possible.
   *
   * @throws MARSHAL, always.
   */
  public void destroy()
  {
  }

  /**
   * The remote call of DynAny methods is not possible.
   *
   * @throws MARSHAL, always.
   */
  public DynAny copy()
  {
    return this;
  }

  /**
   * The remote call of DynAny methods is not possible.
   *
   * @throws MARSHAL, always.
   */
  public void rewind()
  {
  }

  /**
   * The remote call of DynAny methods is not possible.
   *
   * @throws MARSHAL, always.
   */
  public void assign(DynAny _0)
              throws TypeMismatch
  {
  }

  /**
   * The remote call of DynAny methods is not possible.
   *
   * @throws MARSHAL, always.
   */
  public int component_count()
  {
    throw new MARSHAL(_DynAnyStub.NOT_APPLICABLE);
  }

  /**
   * The remote call of DynAny methods is not possible.
   *
   * @throws MARSHAL, always.
   */
  public DynAny current_component()
                           throws TypeMismatch
  {
    throw new MARSHAL(_DynAnyStub.NOT_APPLICABLE);
  }

  /**
   * The remote call of DynAny methods is not possible.
   *
   * @throws MARSHAL, always.
   */
  public boolean equal(DynAny _0)
  {
    throw new MARSHAL(_DynAnyStub.NOT_APPLICABLE);
  }

  /**
   * The remote call of DynAny methods is not possible.
   *
   * @throws MARSHAL, always.
   */
  public void from_any(Any _0)
                throws TypeMismatch, InvalidValue
  {
    throw new MARSHAL(_DynAnyStub.NOT_APPLICABLE);
  }

  /**
   * The remote call of DynAny methods is not possible.
   *
   * @throws MARSHAL, always.
   */
  public Any get_any()
              throws TypeMismatch, InvalidValue
  {
    throw new MARSHAL(_DynAnyStub.NOT_APPLICABLE);
  }

  /**
   * The remote call of DynAny methods is not possible.
   *
   * @throws MARSHAL, always.
   */
  public boolean get_boolean()
                      throws TypeMismatch, InvalidValue
  {
    throw new MARSHAL(_DynAnyStub.NOT_APPLICABLE);
  }

  /**
   * The remote call of DynAny methods is not possible.
   *
   * @throws MARSHAL, always.
   */
  public char get_char()
                throws TypeMismatch, InvalidValue
  {
    throw new MARSHAL(_DynAnyStub.NOT_APPLICABLE);
  }

  /**
   * The remote call of DynAny methods is not possible.
   *
   * @throws MARSHAL, always.
   */
  public double get_double()
                    throws TypeMismatch, InvalidValue
  {
    throw new MARSHAL(_DynAnyStub.NOT_APPLICABLE);
  }

  /**
   * The remote call of DynAny methods is not possible.
   *
   * @throws MARSHAL, always.
   */
  public DynAny get_dyn_any()
                     throws TypeMismatch, InvalidValue
  {
    throw new MARSHAL(_DynAnyStub.NOT_APPLICABLE);
  }

  /**
   * The remote call of DynAny methods is not possible.
   *
   * @throws MARSHAL, always.
   */
  public float get_float()
                  throws TypeMismatch, InvalidValue
  {
    throw new MARSHAL(_DynAnyStub.NOT_APPLICABLE);
  }

  /**
   * The remote call of DynAny methods is not possible.
   *
   * @throws MARSHAL, always.
   */
  public int get_long()
               throws TypeMismatch, InvalidValue
  {
    throw new MARSHAL(_DynAnyStub.NOT_APPLICABLE);
  }

  /**
   * The remote call of DynAny methods is not possible.
   *
   * @throws MARSHAL, always.
   */
  public long get_longlong()
                    throws TypeMismatch, InvalidValue
  {
    throw new MARSHAL(_DynAnyStub.NOT_APPLICABLE);
  }

  /**
   * The remote call of DynAny methods is not possible.
   *
   * @throws MARSHAL, always.
   */
  public byte get_octet()
                 throws TypeMismatch, InvalidValue
  {
    throw new MARSHAL(_DynAnyStub.NOT_APPLICABLE);
  }

  /**
   * The remote call of DynAny methods is not possible.
   *
   * @throws MARSHAL, always.
   */
  public org.omg.CORBA.Object get_reference()
                                     throws TypeMismatch, InvalidValue
  {
    throw new MARSHAL(_DynAnyStub.NOT_APPLICABLE);
  }

  /**
   * The remote call of DynAny methods is not possible.
   *
   * @throws MARSHAL, always.
   */
  public short get_short()
                  throws TypeMismatch, InvalidValue
  {
    throw new MARSHAL(_DynAnyStub.NOT_APPLICABLE);
  }

  /**
   * The remote call of DynAny methods is not possible.
   *
   * @throws MARSHAL, always.
   */
  public String get_string()
                    throws TypeMismatch, InvalidValue
  {
    throw new MARSHAL(_DynAnyStub.NOT_APPLICABLE);
  }

  /**
   * The remote call of DynAny methods is not possible.
   *
   * @throws MARSHAL, always.
   */
  public TypeCode get_typecode()
                        throws TypeMismatch, InvalidValue
  {
    throw new MARSHAL(_DynAnyStub.NOT_APPLICABLE);
  }

  /**
   * The remote call of DynAny methods is not possible.
   *
   * @throws MARSHAL, always.
   */
  public int get_ulong()
                throws TypeMismatch, InvalidValue
  {
    throw new MARSHAL(_DynAnyStub.NOT_APPLICABLE);
  }

  /**
   * The remote call of DynAny methods is not possible.
   *
   * @throws MARSHAL, always.
   */
  public long get_ulonglong()
                     throws TypeMismatch, InvalidValue
  {
    throw new MARSHAL(_DynAnyStub.NOT_APPLICABLE);
  }

  /**
   * The remote call of DynAny methods is not possible.
   *
   * @throws MARSHAL, always.
   */
  public short get_ushort()
                   throws TypeMismatch, InvalidValue
  {
    throw new MARSHAL(_DynAnyStub.NOT_APPLICABLE);
  }

  /**
   * The remote call of DynAny methods is not possible.
   *
   * @throws MARSHAL, always.
   */
  public Serializable get_val()
                       throws TypeMismatch, InvalidValue
  {
    throw new MARSHAL(_DynAnyStub.NOT_APPLICABLE);
  }

  /**
   * The remote call of DynAny methods is not possible.
   *
   * @throws MARSHAL, always.
   */
  public char get_wchar()
                 throws TypeMismatch, InvalidValue
  {
    throw new MARSHAL(_DynAnyStub.NOT_APPLICABLE);
  }

  /**
   * The remote call of DynAny methods is not possible.
   *
   * @throws MARSHAL, always.
   */
  public String get_wstring()
                     throws TypeMismatch, InvalidValue
  {
    throw new MARSHAL(_DynAnyStub.NOT_APPLICABLE);
  }

  /**
   * The remote call of DynAny methods is not possible.
   *
   * @throws MARSHAL, always.
   */
  public void insert_any(Any _0)
                  throws TypeMismatch, InvalidValue
  {
    throw new MARSHAL(_DynAnyStub.NOT_APPLICABLE);
  }

  /**
   * The remote call of DynAny methods is not possible.
   *
   * @throws MARSHAL, always.
   */
  public void insert_boolean(boolean _0)
                      throws TypeMismatch, InvalidValue
  {
    throw new MARSHAL(_DynAnyStub.NOT_APPLICABLE);
  }

  /**
   * The remote call of DynAny methods is not possible.
   *
   * @throws MARSHAL, always.
   */
  public void insert_char(char _0)
                   throws TypeMismatch, InvalidValue
  {
    throw new MARSHAL(_DynAnyStub.NOT_APPLICABLE);
  }

  /**
   * The remote call of DynAny methods is not possible.
   *
   * @throws MARSHAL, always.
   */
  public void insert_double(double _0)
                     throws TypeMismatch, InvalidValue
  {
    throw new MARSHAL(_DynAnyStub.NOT_APPLICABLE);
  }

  /**
   * The remote call of DynAny methods is not possible.
   *
   * @throws MARSHAL, always.
   */
  public void insert_dyn_any(DynAny _0)
                      throws TypeMismatch, InvalidValue
  {
    throw new MARSHAL(_DynAnyStub.NOT_APPLICABLE);
  }

  /**
   * The remote call of DynAny methods is not possible.
   *
   * @throws MARSHAL, always.
   */
  public void insert_float(float _0)
                    throws TypeMismatch, InvalidValue
  {
    throw new MARSHAL(_DynAnyStub.NOT_APPLICABLE);
  }

  /**
   * The remote call of DynAny methods is not possible.
   *
   * @throws MARSHAL, always.
   */
  public void insert_long(int _0)
                   throws TypeMismatch, InvalidValue
  {
    throw new MARSHAL(_DynAnyStub.NOT_APPLICABLE);
  }

  /**
   * The remote call of DynAny methods is not possible.
   *
   * @throws MARSHAL, always.
   */
  public void insert_longlong(long _0)
                       throws TypeMismatch, InvalidValue
  {
    throw new MARSHAL(_DynAnyStub.NOT_APPLICABLE);
  }

  /**
   * The remote call of DynAny methods is not possible.
   *
   * @throws MARSHAL, always.
   */
  public void insert_octet(byte _0)
                    throws TypeMismatch, InvalidValue
  {
    throw new MARSHAL(_DynAnyStub.NOT_APPLICABLE);
  }

  /**
   * The remote call of DynAny methods is not possible.
   *
   * @throws MARSHAL, always.
   */
  public void insert_reference(org.omg.CORBA.Object _0)
                        throws TypeMismatch, InvalidValue
  {
    throw new MARSHAL(_DynAnyStub.NOT_APPLICABLE);
  }

  /**
   * The remote call of DynAny methods is not possible.
   *
   * @throws MARSHAL, always.
   */
  public void insert_short(short _0)
                    throws TypeMismatch, InvalidValue
  {
    throw new MARSHAL(_DynAnyStub.NOT_APPLICABLE);
  }

  /**
   * The remote call of DynAny methods is not possible.
   *
   * @throws MARSHAL, always.
   */
  public void insert_string(String _0)
                     throws TypeMismatch, InvalidValue
  {
    throw new MARSHAL(_DynAnyStub.NOT_APPLICABLE);
  }

  /**
   * The remote call of DynAny methods is not possible.
   *
   * @throws MARSHAL, always.
   */
  public void insert_typecode(TypeCode _0)
                       throws TypeMismatch, InvalidValue
  {
    throw new MARSHAL(_DynAnyStub.NOT_APPLICABLE);
  }

  /**
   * The remote call of DynAny methods is not possible.
   *
   * @throws MARSHAL, always.
   */
  public void insert_ulong(int _0)
                    throws TypeMismatch, InvalidValue
  {
    throw new MARSHAL(_DynAnyStub.NOT_APPLICABLE);
  }

  /**
   * The remote call of DynAny methods is not possible.
   *
   * @throws MARSHAL, always.
   */
  public void insert_ulonglong(long _0)
                        throws TypeMismatch, InvalidValue
  {
    throw new MARSHAL(_DynAnyStub.NOT_APPLICABLE);
  }

  /**
   * The remote call of DynAny methods is not possible.
   *
   * @throws MARSHAL, always.
   */
  public void insert_ushort(short _0)
                     throws TypeMismatch, InvalidValue
  {
    throw new MARSHAL(_DynAnyStub.NOT_APPLICABLE);
  }

  /**
   * The remote call of DynAny methods is not possible.
   *
   * @throws MARSHAL, always.
   */
  public void insert_val(Serializable _0)
                  throws TypeMismatch, InvalidValue
  {
    throw new MARSHAL(_DynAnyStub.NOT_APPLICABLE);
  }

  /**
   * The remote call of DynAny methods is not possible.
   *
   * @throws MARSHAL, always.
   */
  public void insert_wchar(char _0)
                    throws TypeMismatch, InvalidValue
  {
    throw new MARSHAL(_DynAnyStub.NOT_APPLICABLE);
  }

  /**
   * The remote call of DynAny methods is not possible.
   *
   * @throws MARSHAL, always.
   */
  public void insert_wstring(String _0)
                      throws TypeMismatch, InvalidValue
  {
    throw new MARSHAL(_DynAnyStub.NOT_APPLICABLE);
  }

  /**
   * The remote call of DynAny methods is not possible.
   *
   * @throws MARSHAL, always.
   */
  public boolean seek(int _0)
  {
    throw new MARSHAL(_DynAnyStub.NOT_APPLICABLE);
  }

  /**
   * The remote call of DynAny methods is not possible.
   *
   * @throws MARSHAL, always.
   */
  public Any to_any()
  {
    throw new MARSHAL(_DynAnyStub.NOT_APPLICABLE);
  }

}