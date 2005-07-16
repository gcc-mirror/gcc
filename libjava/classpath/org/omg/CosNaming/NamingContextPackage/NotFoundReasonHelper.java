/* NotFoundReasonHelper.java --
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


package org.omg.CosNaming.NamingContextPackage;

import org.omg.CORBA.Any;
import org.omg.CORBA.BAD_OPERATION;
import org.omg.CORBA.ORB;
import org.omg.CORBA.TypeCode;
import org.omg.CORBA.portable.InputStream;
import org.omg.CORBA.portable.OutputStream;

/**
 * The helper operations for {@link NotFoundReason}
 *
 * @author Audrius Meskauskas, Lithuania (AudriusA@Bioinformatics.org)
 */
public abstract class NotFoundReasonHelper
{
  /**
   * The {@link AlreadyBound} repository id.
   */
  private static String _id =
    "IDL:omg.org/CosNaming/NamingContext/NotFoundReason:1.0";

  /**
   * The cached type code value.
   */
  private static TypeCode typeCode;

  /**
   * Extract the exception from the given {@link Any}.
   */
  public static NotFoundReason extract(Any a)
  {
    try
      {
        return ((NotFoundReasonHolder) a.extract_Streamable()).value;
      }
    catch (ClassCastException ex)
      {
        throw new BAD_OPERATION();
      }
  }

  /**
   * Return the exception repository id.
   */
  public static String id()
  {
    return _id;
  }

  /**
   * Insert the exception into the given {@link Any}.
   */
  public static void insert(Any a, NotFoundReason that)
  {
    a.insert_Streamable(new NotFoundReasonHolder(that));
  }

  /**
   * Read the exception from the given CDR stream.
   */
  public static NotFoundReason read(InputStream istream)
  {
    return NotFoundReason.from_int(istream.read_long());
  }

  /**
   * Create the type code for this exception.
   */
  public static TypeCode type()
  {
    if (typeCode == null)
      typeCode =
        ORB.init().create_enum_tc(id(), "NotFoundReason",
                                  new String[]
                                  {
                                    "missing_node", "not_context", "not_object"
                                  }
                                 );
    return typeCode;
  }

  /**
   * Write the exception to the CDR output stream.
   */
  public static void write(OutputStream ostream, NotFoundReason value)
  {
    ostream.write_long(value.value());
  }
}
