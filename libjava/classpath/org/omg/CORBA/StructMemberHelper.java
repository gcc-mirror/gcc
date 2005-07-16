/* StructMemberHelper.java --
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

import gnu.CORBA.Restricted_ORB;
import gnu.CORBA.TypeCodeHelper;

import org.omg.CORBA.portable.InputStream;
import org.omg.CORBA.portable.OutputStream;

/**
 * Defines a helper operations for StructMember.
 *
 * @author Audrius Meskauskas, Lithuania (AudriusA@Bioinformatics.org)
 */
public abstract class StructMemberHelper
{
  /**
   * The cached typecode value, computed once.
   */
  private static TypeCode typeCode;

  /**
   * This flag is used when creating typecodes for the recursive structures.
   */
  private static boolean active;

  /**
   * Insert the structure member into the given Any.
   */
  public static void insert(Any a, StructMember that)
  {
    OutputStream out = a.create_output_stream();
    a.type(type());
    write(out, that);
    a.read_value(out.create_input_stream(), type());
  }

  /**
   * Extract the structure member from the given Any.
   */
  public static StructMember extract(Any a)
  {
    return read(a.create_input_stream());
  }

  /**
   * Get a typecode for the StructMember.
   */
  public static synchronized TypeCode type()
  {
    if (typeCode == null)
      {
        ORB orb = Restricted_ORB.Singleton;

        synchronized (TypeCode.class)
          {
            if (typeCode == null)
              {
                if (active)
                  {
                    return orb.create_recursive_tc(id());
                  }
                active = true;

                StructMember[] members = new StructMember[ 3 ];
                TypeCode member = null;

                member = orb.create_string_tc(0);
                member =
                  orb.create_alias_tc(IdentifierHelper.id(), "Identifier",
                                      member
                                     );
                members [ 0 ] = new StructMember("name", member, null);

                member = orb.get_primitive_tc(TCKind.tk_TypeCode);

                members [ 1 ] = new StructMember("type", member, null);
                members [ 2 ] =
                  new StructMember("type_def", IDLTypeHelper.type(), null);
                typeCode =
                  orb.create_struct_tc(StructMemberHelper.id(), "StructMember",
                                       members
                                      );
                active = false;
              }
          }
      }
    return typeCode;
  }

  /**
   * Return the StructMember repository id.
   *
   * @return "IDL:omg.org/CORBA/StructMember:1.0", always.
   */
  public static String id()
  {
    return "IDL:omg.org/CORBA/StructMember:1.0";
  }

  /**
   * Read the StructMember from the input stream.
   */
  public static StructMember read(InputStream istream)
  {
    try
      {
        StructMember value = new StructMember();
        value.name = istream.read_string();
        value.type = TypeCodeHelper.read(istream);
        value.type_def = IDLTypeHelper.read(istream);
        return value;
      }
    catch (UserException ex)
      {
        MARSHAL m = new MARSHAL();
        m.initCause(ex);
        throw m;
      }
  }

  /**
   * Write the StructMember to the input stream.
   */
  public static void write(OutputStream ostream, StructMember value)
  {
    try
      {
        ostream.write_string(value.name);
        TypeCodeHelper.write(ostream, value.type);
        IDLTypeHelper.write(ostream, value.type_def);
      }
    catch (UserException ex)
      {
        MARSHAL m = new MARSHAL();
        m.initCause(ex);
        throw m;
      }
  }
}