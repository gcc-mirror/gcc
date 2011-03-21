/* IORHelper.java --
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


package org.omg.IOP;

import gnu.CORBA.Minor;
import gnu.CORBA.OrbRestricted;

import org.omg.CORBA.Any;
import org.omg.CORBA.BAD_OPERATION;
import org.omg.CORBA.ORB;
import org.omg.CORBA.StructMember;
import org.omg.CORBA.TCKind;
import org.omg.CORBA.TypeCode;
import org.omg.CORBA.portable.InputStream;
import org.omg.CORBA.portable.OutputStream;

/**
* A helper operations for the structure {@link IOR}.
*
* @author Audrius Meskauskas, Lithuania (AudriusA@Bioinformatics.org)
*/
public abstract class IORHelper
{
  /**
   * Create the IOR typecode (structure, named "IOR"). The typecode states that
   * the structure contains the following fields: type_id, profiles.
   */
  public static TypeCode type()
  {
    ORB orb = OrbRestricted.Singleton;
    StructMember[] members = new StructMember[2];

    TypeCode field;

    field = orb.get_primitive_tc(TCKind.tk_string);
    members[0] = new StructMember("type_id", field, null);

    field = orb.create_sequence_tc(0, TaggedProfileHelper.type());
    members[1] = new StructMember("profiles", field, null);
    return orb.create_struct_tc(id(), "IOR", members);
  }

  /**
   * Insert the IOR into the given Any. This method uses the IORHolder.
   *
   * @param any the Any to insert into.
   * @param that the IOR to insert.
   */
  public static void insert(Any any, IOR that)
  {
    any.insert_Streamable(new IORHolder(that));
  }

  /**
   * Extract the IOR from given Any.
   * This method uses the IORHolder.
   *
   * @throws BAD_OPERATION if the passed Any does not contain IOR.
   */
  public static IOR extract(Any any)
  {
    try
      {
        return ((IORHolder) any.extract_Streamable()).value;
      }
    catch (ClassCastException cex)
      {
        BAD_OPERATION bad = new BAD_OPERATION("IOR expected");
        bad.minor = Minor.Any;
        bad.initCause(cex);
        throw bad;
      }
  }

  /**
   * Get the IOR repository id.
   *
   * @return "IDL:omg.org/IOP/IOR:1.0", always.
   */
  public static String id()
  {
    return "IDL:omg.org/IOP/IOR:1.0";
  }

  /**
   * Read the structure from the CDR intput stream. Expects repository
   * it, then number of the tagged profiles and then the tagged profiles.
   * Does not expect the endian indicator, present in the beginning of the
   * stringified IOR references.
   *
   * @param input a org.omg.CORBA.portable stream to read from.
   */
  public static IOR read(InputStream input)
  {
    IOR value = new IOR();
    value.type_id = input.read_string();
    value.profiles = new TaggedProfile[ input.read_long() ];
    for (int i0 = 0; i0 < value.profiles.length; i0++)
      value.profiles [ i0 ] = TaggedProfileHelper.read(input);
    return value;
  }

  /**
   * Write the structure to the CDR output stream. Writes
   * Expects repository it, then number of the tagged profiles and then
   * the tagged profiles. Will not write the endian indicator, present
   * in the beginning of the stringified IOR references.
   *
   * @param output a org.omg.CORBA.portable stream stream to write into.
   * @param value a value to write.
   */
  public static void write(OutputStream output, IOR value)
  {
    output.write_string(value.type_id);
    output.write_long(value.profiles.length);
    for (int i0 = 0; i0 < value.profiles.length; i0++)
      TaggedProfileHelper.write(output, value.profiles [ i0 ]);
  }
}
