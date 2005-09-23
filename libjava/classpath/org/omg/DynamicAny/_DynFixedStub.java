/* _DynFixedStub.java --
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
import org.omg.CORBA.portable.Delegate;
import org.omg.DynamicAny.DynAnyPackage.InvalidValue;
import org.omg.DynamicAny.DynAnyPackage.TypeMismatch;

import java.io.Serializable;

/**
 * Should provide support for remote invocation of methods on DynFixed. As
 * DynFixed can never be remote at least till 1.5 inclusive, this class is
 * not in use.
 *
 * @author Audrius Meskauskas, Lithuania (AudriusA@Bioinformatics.org)
 */
public class _DynFixedStub
  extends _DynAnyStub
  implements DynFixed, Serializable
{
  /**
   * Use serialVersionUID (v1.4) for interoperability.
   */
  private static final long serialVersionUID = -1932029532964417188L;

  /**
   * The purpose and value of this field are not documented.
   */
  public static final Class _opsClass = DynFixedOperations.class;

  /**
   * Create the DynFixed stub.
   */
  public _DynFixedStub()
  {
  }

  /**
   * Create the naming context stub with the given delegate.
   */
  public _DynFixedStub(Delegate delegate)
  {
    _set_delegate(delegate);
  }

  /**
   * Return the array of repository ids for this object.
   */
  public String[] _ids()
  {
    return new String[] { DynFixedHelper.id() };
  }

  /**
   * The remote call of DynAny methods is not possible.
   *
   * @throws MARSHAL, always.
   */
  public String get_value()
  {
    throw new MARSHAL(_DynAnyStub.NOT_APPLICABLE);
  }

  /**
   * The remote call of DynAny methods is not possible.
   *
   * @throws MARSHAL, always.
   */
  public boolean set_value(String _0)
                    throws TypeMismatch, InvalidValue
  {
    throw new MARSHAL(_DynAnyStub.NOT_APPLICABLE);
  }
}