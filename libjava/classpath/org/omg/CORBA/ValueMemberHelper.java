/* ValueMemberHelper.java --
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
import gnu.CORBA.OrbRestricted;
import gnu.CORBA.TypeCodeHelper;

import org.omg.CORBA.portable.InputStream;
import org.omg.CORBA.portable.OutputStream;

/**
 * The helper operations for the value member.
 *
 * @author Audrius Meskauskas, Lithuania (AudriusA@Bioinformatics.org)
 */
public abstract class ValueMemberHelper
{
  /**
   * The cached typecode value, computed once.
   */
  private static TypeCode typeCode = null;

  /**
   * Used when creating typecodes with recursive value members.
   */
  private static boolean active = false;

  /**
   * Insert the value member into the given Any.
   */
  public static void insert(Any a, ValueMember that)
  {
    OutputStream out = a.create_output_stream();
    a.type(type());
    write(out, that);
    a.read_value(out.create_input_stream(), type());
  }

  /**
   * Extract the value member from the given Any.
   */
  public static ValueMember extract(Any a)
  {
    return read(a.create_input_stream());
  }

  /**
   * Create a typecode for this value member.
   */
  public static synchronized TypeCode type()
  {
    if (typeCode == null)
      {
        synchronized (TypeCode.class)
          {
            if (typeCode == null)
              {
                ORB orb = OrbRestricted.Singleton;

                if (active)
                  {
                    return orb.create_recursive_tc(id());
                  }
                active = true;

                StructMember[] members = new StructMember[ 7 ];
                TypeCode member;
                member = orb.create_string_tc(0);
                member =
                  orb.create_alias_tc(IdentifierHelper.id(), "Identifier",
                                      member
                                     );
                members [ 0 ] = new StructMember("name", member, null);
                member = orb.create_string_tc(0);
                member =
                  orb.create_alias_tc(RepositoryIdHelper.id(), "RepositoryId",
                                      member
                                     );
                members [ 1 ] = new StructMember("id", member, null);
                member = orb.create_string_tc(0);
                member =
                  orb.create_alias_tc(RepositoryIdHelper.id(), "RepositoryId",
                                      member
                                     );
                members [ 2 ] = new StructMember("defined_in", member, null);
                member = orb.create_string_tc(0);
                member =
                  orb.create_alias_tc(VersionSpecHelper.id(), "VersionSpec",
                                      member
                                     );
                members [ 3 ] = new StructMember("version", member, null);
                member = orb.create_string_tc(0);
                member = orb.get_primitive_tc(TCKind.tk_TypeCode);
                members [ 4 ] = new StructMember("type", member, null);
                member = IDLTypeHelper.type();
                members [ 5 ] = new StructMember("type_def", member, null);
                member = orb.get_primitive_tc(TCKind.tk_short);
                member =
                  orb.create_alias_tc(VisibilityHelper.id(), "Visibility",
                                      member
                                     );
                members [ 6 ] = new StructMember("access", member, null);
                typeCode =
                  orb.create_struct_tc(ValueMemberHelper.id(), "ValueMember",
                                       members
                                      );
                active = false;
              }
          }
      }
    return typeCode;
  }

  /**
   * Return the ValueMember repository id.
   *
   * @return "IDL:omg.org/CORBA/ValueMember:1.0", always.
   */
  public static String id()
  {
    return "IDL:omg.org/CORBA/ValueMember:1.0";
  }

  /**
   * Reads the value member from the given stream.
   */
  public static ValueMember read(InputStream istream)
  {
    try
      {
        ValueMember value = new ValueMember();
        value.name = istream.read_string();
        value.id = istream.read_string();
        value.defined_in = istream.read_string();
        value.version = istream.read_string();
        value.type = TypeCodeHelper.read(istream);
        value.type_def = IDLTypeHelper.read(istream);
        value.access = istream.read_short();
        return value;
      }
    catch (UserException ex)
      {
        MARSHAL m = new MARSHAL();
        m.minor = Minor.UserException;        
        m.initCause(ex);
        throw m;
      }
  }

  /**
   * Writes the value member to the given stream.
   */
  public static void write(OutputStream ostream, ValueMember value)
  {
    try
      {
        ostream.write_string(value.name);
        ostream.write_string(value.id);
        ostream.write_string(value.defined_in);
        ostream.write_string(value.version);
        TypeCodeHelper.write(ostream, value.type);
        IDLTypeHelper.write(ostream, value.type_def);
        ostream.write_short(value.access);
      }
    catch (UserException ex)
      {
        MARSHAL m = new MARSHAL();
        m.minor = Minor.UserException;        
        m.initCause(ex);
        throw m;
      }
  }
}