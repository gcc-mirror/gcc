/* PolicyHelper.java --
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


package org.omg.CORBA;

import gnu.CORBA.Minor;

import org.omg.CORBA.Any;
import org.omg.CORBA.BAD_OPERATION;
import org.omg.CORBA.ORB;
import org.omg.CORBA.StructMember;
import org.omg.CORBA.TCKind;
import org.omg.CORBA.TypeCode;
import org.omg.CORBA.portable.Delegate;
import org.omg.CORBA.portable.InputStream;
import org.omg.CORBA.portable.ObjectImpl;
import org.omg.CORBA.portable.OutputStream;

/**
* The helper operations for the
* CORBA object {@link Policy}.
*
* @author Audrius Meskauskas, Lithuania (AudriusA@Bioinformatics.org)
*/
public abstract class PolicyHelper
{
  /**
   * The cached {@link Policy} typecode, computed once.
   */
  private static TypeCode typeCode;

  /**
   * Get the type code of the {@link Policy}.
   *
   * @return interface typecode, named "Policy".
   */
  public static TypeCode type()
  {
    if (typeCode == null)
      typeCode = ORB.init().create_interface_tc(id(), "Policy");
    return typeCode;
  }

  /**
   * Insert the Policy into the given Any.
   *
   * @param any the Any to insert into.
   * @param that the Policy to insert.
   */
  public static void insert(Any any, Policy that)
  {
    any.insert_Streamable(new PolicyHolder(that));
  }

  /**
   * Extract the Policy from given Any.
   *
   * @throws BAD_OPERATION if the passed Any does not contain Policy.
   */
  public static Policy extract(Any any)
  {
    try
      {
        PolicyHolder h = (PolicyHolder) any.extract_Streamable();
        return h.value;
      }
    catch (ClassCastException ex)
      {
        BAD_OPERATION bad = new BAD_OPERATION("Policy expected");
        bad.minor = Minor.Any;
        throw bad;
      }
  }

  /**
   * Get the Policy repository id.
   *
   * @return "IDL:omg.org/CORBA/Policy:1.0", always.
   */
  public static String id()
  {
    return "IDL:omg.org/CORBA/Policy:1.0";
  }

  /**
   * Cast the passed object into the Policy. If the
   * object has a different java type, create an instance
   * of the _PolicyStub, using the same delegate, as for
   * the passed parameter. Hence, unlike java type cast,
   * this method may return a different object, than has been passed.
   *
   * @param obj the object to narrow.
   * @return narrowed instance.
   * @throws BAD_PARAM if the passed object is not a Policy.
   */
  public static Policy narrow(org.omg.CORBA.Object obj)
  {
    if (obj == null)
      return null;
    else if (obj instanceof Policy)
      return (Policy) obj;
    else
      {
        // Check for the policy id cannot be performed because
        // this helper must read various subclasses of the Policy,
        // and the IOR profile currently supports only one id.

        Delegate delegate = ((ObjectImpl) obj)._get_delegate();
        return new _PolicyStub(delegate);
      }
  }

  /**
   * Read the object from the CDR intput stream (IOR profile expected).
   *
   * @param input a org.omg.CORBA.portable stream to read from.
   */
  public static Policy read(InputStream input)
  {
    return narrow(input.read_Object());
  }

  /**
   * Write the object to the CDR output stream (as IOR profile).
   *
   * @param output a org.omg.CORBA.portable stream stream to write into.
   * @param value a value to write.
   */
  public static void write(OutputStream output, Policy value)
  {
    output.write_Object(value);
  }
}