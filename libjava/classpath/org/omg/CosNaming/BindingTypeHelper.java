/* BindingTypeHelper.java --
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

import gnu.CORBA.Minor;

import org.omg.CORBA.Any;
import org.omg.CORBA.BAD_OPERATION;
import org.omg.CORBA.ORB;
import org.omg.CORBA.TypeCode;
import org.omg.CORBA.portable.InputStream;
import org.omg.CORBA.portable.OutputStream;

/**
 * A helper operations of the {@link BindingType}
 *
 * @author Audrius Meskauskas, Lithuania (AudriusA@Bioinformatics.org)
 */
public abstract class BindingTypeHelper
{
  /**
   * The binding type repository id.
   */
  private static String id = "IDL:omg.org/CosNaming/BindingType:1.0";
  private static TypeCode typeCode = null;

  /**
   * Extract the binding type from the given {@link Any}.
   */
  public static BindingType extract(Any any)
  {
    try
      {
        return ((BindingTypeHolder) any.extract_Streamable()).value;
      }
    catch (ClassCastException ex)
      {
        BAD_OPERATION bad = new BAD_OPERATION("Binding type expected");
        bad.initCause(ex);
        bad.minor = Minor.Any;        
        throw bad;
      }
  }

  /**
   * Return the binding type repository id.
   */
  public static String id()
  {
    return id;
  }

  /**
   * Insert the binding type into the given {@link Any}.
   */
  public static void insert(Any any, BindingType that)
  {
    any.insert_Streamable(new BindingTypeHolder(that));
  }

  /**
   * Read the binding type from the CDR input stream.
   */
  public static BindingType read(InputStream istream)
  {
    return BindingType.from_int(istream.read_long());
  }

  /**
   * Get the type code of this enumeration.
   */
  public static synchronized TypeCode type()
  {
    if (typeCode == null)
      {
        typeCode =
          ORB.init().create_enum_tc(id(), "BindingType",
                                    new String[] { "nobject", "ncontext" }
                                   );
      }
    return typeCode;
  }

  /**
   * Write the binding type to the CDR output stream.
   */
  public static void write(OutputStream ostream, BindingType value)
  {
    ostream.write_long(value.value());
  }
}
