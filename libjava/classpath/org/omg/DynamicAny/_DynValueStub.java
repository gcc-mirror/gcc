/* _DynValueStub.java --
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


package org.omg.DynamicAny;

import org.omg.CORBA.MARSHAL;
import org.omg.CORBA.TCKind;
import org.omg.CORBA.portable.Delegate;
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
  extends _DynAnyStub
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
   * {@link ObjectImpl#_set_delegate(Delegate)}.
   */
  public _DynValueStub()
  {
  }

  /**
   * Create the naming context stub with the given delegate.
   */
  public _DynValueStub(Delegate delegate)
  {
    _set_delegate(delegate);
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
}