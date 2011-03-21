/* _DynAnyFactoryStub.java --
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

import gnu.CORBA.Minor;

import java.io.Serializable;

import org.omg.CORBA.Any;
import org.omg.CORBA.MARSHAL;
import org.omg.CORBA.TypeCode;
import org.omg.CORBA.portable.ObjectImpl;
import org.omg.DynamicAny.DynAnyFactoryPackage.InconsistentTypeCode;

/**
 * Should provide support for remote invocation of methods on
 * DynAnyFactory. As DynAny can never be remote at least till 1.5 inclusive,
 * this class is not in use. DynAnyFactory should be obtained from the
 * {@link org.omg.CORBA.ORB#resolve_initial_references}.
 *
 * @author Audrius Meskauskas, Lithuania (AudriusA@Bioinformatics.org)
 */
public class _DynAnyFactoryStub
  extends ObjectImpl
  implements DynAnyFactory, Serializable
{
  /**
   * Use serialVersionUID (v1.4) for interoperability.
   */
  private static final long serialVersionUID = -6575269659020082310L;

  /**
   * The purpose and value of this field are not documented.
   */
  @SuppressWarnings("unchecked") // Needed for API compatibility
  public static final Class _opsClass = DynAnyFactoryOperations.class;

  /**
   * Create the DynAnyFactory stub.
   */
  public _DynAnyFactoryStub()
  {
  }

  /**
   * Return the array of repository ids for this object.
   */
  public String[] _ids()
  {
    return new String[] { DynAnyFactoryHelper.id() };
  }

  /**
   * The remote call of this DynAnyFactory method is not possible
   * (the created DynAny would not be transferred to client).
   *
   * @throws MARSHAL, always.
   */
  public DynAny create_dyn_any(Any _0)
                        throws InconsistentTypeCode
  {
    MARSHAL m = new MARSHAL(NOT_APPLICABLE);
    m.minor = Minor.Inappropriate;
    throw m;
  }

  /**
   * The remote call of this DynAnyFactory method is not possible
   * (the created DynAny would not be transferred to client).
   *
   * @throws MARSHAL, always.
   */
  public DynAny create_dyn_any_from_type_code(TypeCode _0)
                                       throws InconsistentTypeCode
  {
    MARSHAL m = new MARSHAL(NOT_APPLICABLE);
    m.minor = Minor.Inappropriate;
    throw m;
  }

  static String NOT_APPLICABLE =
    "DynAnyFactory is always local objects. " +
    "It is never accessed on remote side via stub.";
}
