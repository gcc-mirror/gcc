/* NamingContextHelper.java --
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


package org.omg.CosNaming;

import org.omg.CORBA.Any;
import org.omg.CORBA.BAD_OPERATION;
import org.omg.CORBA.BAD_PARAM;
import org.omg.CORBA.ORB;
import org.omg.CORBA.TypeCode;
import org.omg.CORBA.portable.Delegate;
import org.omg.CORBA.portable.InputStream;
import org.omg.CORBA.portable.ObjectImpl;
import org.omg.CORBA.portable.OutputStream;

/**
 * The helper operations for the naming context.
 *
 * @author Audrius Meskauskas, Lithuania (AudriusA@Bioinformatics.org)
 */
public abstract class NamingContextHelper
{
  /**
   * The naming context repository id.
   */
  private static String _id = "IDL:omg.org/CosNaming/NamingContext:1.0";

  /**
   * The cached {@link NamingContext} typecode.
   */
  private static TypeCode typeCode;

  /**
   * Extract the naming context from the given {@link Any}.
   */
  public static NamingContext extract(Any a)
  {
    try
      {
        return ((NamingContextHolder) a.extract_Streamable()).value;
      }
    catch (ClassCastException ex)
      {
        BAD_OPERATION bad = new BAD_OPERATION("Naming context expected");
        bad.initCause(ex);
        throw bad;
      }
  }

  /**
   * Get the {@link NamingContext} repository id.
   */
  public static String id()
  {
    return _id;
  }

  /**
   * Insert the naming context into the given {@link Any}
   */
  public static void insert(Any a, NamingContext that)
  {
    a.insert_Streamable(new NamingContextHolder(that));
  }

  /**
   * Cast the passed object into the NamingContext. If the
   * object has a different java type, create an instance
   * of the NamingContext, using the same delegate, as for
   * the passed parameter.
   *
   * If the object repository Id indicates that it is an instance of
   *  {@link NamingContextExt} that is a subclass of the NamingContext,
   * the functionality is  delegated to {@link NamingContextHelper#narrow}.
   *
   * @param obj the object to cast.
   * @return casted instance.
   *
   * @throws BAD_PARAM if the passed object is not an instance of
   * {@link NamingContext} or {@link NamingContextExt}.
   */
  public static NamingContext narrow(org.omg.CORBA.Object obj)
  {
    if (obj == null)
      return null;
    else if (obj instanceof NamingContext)
      return (NamingContext) obj;
    else if (obj._is_a(id()))
      {
        Delegate delegate = ((ObjectImpl) obj)._get_delegate();
        return new _NamingContextStub(delegate);
      }
    else if (obj._is_a(NamingContextExtHelper.id()))
      return NamingContextExtHelper.narrow(obj);
    else
      throw new BAD_PARAM();
  }

  /**
   * Read the naming context from the given CDR input stream.
   */
  public static NamingContext read(InputStream istream)
  {
    return narrow(istream.read_Object(_NamingContextStub.class));
  }

  /**
   * Get the type code of the {@link NamingContext}.
   */
  public static TypeCode type()
  {
    if (typeCode == null)
      typeCode = ORB.init().create_interface_tc(id(), "NamingContext");
    return typeCode;
  }

  /**
   * Write the given naming context into the given CDR output stream.
   */
  public static void write(OutputStream ostream, NamingContext value)
  {
    ostream.write_Object(value);
  }
}
