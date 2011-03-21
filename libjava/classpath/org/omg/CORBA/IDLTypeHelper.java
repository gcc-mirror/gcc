/* IDLTypeHelper.java --
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

import gnu.CORBA.OrbRestricted;

import org.omg.CORBA.portable.Delegate;
import org.omg.CORBA.portable.InputStream;
import org.omg.CORBA.portable.ObjectImpl;
import org.omg.CORBA.portable.OutputStream;

/**
 * A helper operations for the IDL type.
 *
 * @author Audrius Meskauskas, Lithuania (AudriusA@Bioinformatics.org)
 */
public abstract class IDLTypeHelper
{
  /**
   * Insert the IDL type into the given Any.
   */
  public static void insert(Any a, IDLType that)
  {
    OutputStream out = a.create_output_stream();
    a.type(type());
    write(out, that);
    a.read_value(out.create_input_stream(), type());
  }

  /**
   * Extract the IDL type from the given Any.
   */
  public static IDLType extract(Any a)
  {
    return read(a.create_input_stream());
  }

  /**
   * Get the typecode of the IDL type (the interface typecode, name "IDLType").
   */
  public static TypeCode type()
  {
    return OrbRestricted.Singleton.create_interface_tc(IDLTypeHelper.id(),
                                                       "IDLType");
  }

  /**
   * Return the IDLType repository id.
   *
   * @return "IDL:omg.org/CORBA/IDLType:1.0", always.
   */
  public static String id()
  {
    return "IDL:omg.org/CORBA/IDLType:1.0";
  }

  /**
   * Read the IDL type from the given input stream.
   * The method reads an object and narrows into IDL type using this
   * helper.
   */
  public static IDLType read(InputStream istream)
  {
    return narrow(istream.read_Object());
  }

  /**
   * Write the IDL type to the output stream (as CORBA object).
   */
  public static void write(OutputStream ostream, IDLType value)
  {
    ostream.write_Object((org.omg.CORBA.Object) value);
  }

  /**
   * Narrows the CORBA object into the IDL type.
   */
  public static IDLType narrow(org.omg.CORBA.Object obj)
  {
    if (obj == null)
      return null;
    else if (obj instanceof IDLType)
      return (IDLType) obj;
    else if (!obj._is_a(id()))
      throw new org.omg.CORBA.BAD_PARAM();
    else
      {
        Delegate delegate = ((ObjectImpl) obj)._get_delegate();
        return new _IDLTypeStub(delegate);
      }
  }
}
