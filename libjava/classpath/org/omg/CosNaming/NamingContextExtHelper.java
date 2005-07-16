/* NamingContextExtHelper.java --
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
 * The helper operations for the extended naming context.
 *
 * @author Audrius Meskauskas, Lithuania (AudriusA@Bioinformatics.org)
 */
public abstract class NamingContextExtHelper
{
  /**
   * The naming context repository id.
   */
  private static String _id = "IDL:omg.org/CosNaming/NamingContextExt:1.0";

  /**
   * The cached {@link NamingContextExt} typecode.
   */
  private static TypeCode typeCode = null;

  /**
   * Extract the naming context from the given {@link Any}.
   */
  public static NamingContextExt extract(Any a)
  {
    try
      {
        return ((NamingContextExtHolder) a.extract_Streamable()).value;
      }
    catch (ClassCastException ex)
      {
        BAD_OPERATION bad = new BAD_OPERATION("NamingContextExt expected");
        bad.initCause(ex);
        throw bad;
      }
  }

  /**
   * Get the {@link NamingContextExt} repository id.
   */
  public static String id()
  {
    return _id;
  }

  /**
   * Insert the naming context into the given {@link Any}
   */
  public static void insert(Any a, NamingContextExt that)
  {
    a.insert_Streamable(new NamingContextExtHolder(that));
  }

  /**
   * Cast the passed object into the NamingContextExt. If the
   * object has a different java type, create an instance
   * of the NamingContextExt, using the same delegate, as for
   * the passed parameter. Hence this method may return
   * a different object, than has been passed.
   *
   * @param obj the object to cast.
   * @return casted instance.
   */
  public static NamingContextExt narrow(org.omg.CORBA.Object obj)
  {
    if (obj == null)
      return null;
    else if (obj instanceof NamingContextExt)
      return (NamingContextExt) obj;
    else if (!obj._is_a(id()))
      throw new BAD_PARAM();
    else
      {
        Delegate delegate = ((ObjectImpl) obj)._get_delegate();
        return new _NamingContextExtStub(delegate);
      }
  }

  /**
   * Read the extended naming context from the given CDR input stream.
   */
  public static NamingContextExt read(InputStream istream)
  {
    return narrow(istream.read_Object(_NamingContextExtStub.class));
  }

  /**
   * Get the type code of the {@link NamingContextExt}.
   */
  public static TypeCode type()
  {
    if (typeCode == null)
      typeCode =
        ORB.init().create_interface_tc(NamingContextExtHelper.id(),
                                       "NamingContextExt"
                                      );
    return typeCode;
  }

  /**
   * Write the given extended naming context into the given CDR output stream.
   */
  public static void write(OutputStream ostream, NamingContextExt value)
  {
    ostream.write_Object(value);
  }
}
