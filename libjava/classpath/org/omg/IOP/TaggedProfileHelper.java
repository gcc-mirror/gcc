/* TaggedProfileHelper.java --
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

import gnu.CORBA.CDR.cdrBufInput;
import gnu.CORBA.CDR.cdrBufOutput;

import org.omg.CORBA.Any;
import org.omg.CORBA.BAD_OPERATION;
import org.omg.CORBA.MARSHAL;
import org.omg.CORBA.ORB;
import org.omg.CORBA.StructMember;
import org.omg.CORBA.TCKind;
import org.omg.CORBA.TypeCode;
import org.omg.CORBA.portable.InputStream;
import org.omg.CORBA.portable.OutputStream;

import java.io.IOException;

/**
 * A helper operations for the structure {@link TaggedProfile}.
 *
 * @author Audrius Meskauskas, Lithuania (AudriusA@Bioinformatics.org)
 */
public abstract class TaggedProfileHelper
{
  /**
   * The cached typecode value, computed only once.
   */
  private static TypeCode typeCode;

  /**
   * Create the TaggedProfile typecode (structure, named "TaggedProfile"). The
   * typecode states that the structure contains the following fields: tag,
   * profile_data.
   */
  public static TypeCode type()
  {
    if (typeCode == null)
      {
        ORB orb = ORB.init();
        StructMember[] members = new StructMember[2];

        TypeCode field;

        field = orb.create_alias_tc("IDL:omg.org/IOP/ProfileId:1.0",
                                    "ProfileId",
                                    orb.get_primitive_tc(TCKind.tk_ulong));
        members[0] = new StructMember("tag", field, null);

        field = orb.create_sequence_tc(0, orb.get_primitive_tc(TCKind.tk_octet));
        members[1] = new StructMember("profile_data", field, null);
        typeCode = orb.create_struct_tc(id(), "TaggedProfile", members);
      }
    return typeCode;
  }

  /**
   * Insert the TaggedProfile into the given Any. This method uses the
   * TaggedProfileHolder.
   *
   * @param any the Any to insert into.
   * @param that the TaggedProfile to insert.
   */
  public static void insert(Any any, TaggedProfile that)
  {
    any.insert_Streamable(new TaggedProfileHolder(that));
  }

  /**
   * Extract the TaggedProfile from given Any. This method uses the
   * TaggedProfileHolder.
   *
   * @throws BAD_OPERATION if the passed Any does not contain TaggedProfile.
   */
  public static TaggedProfile extract(Any any)
  {
    try
      {
        return ((TaggedProfileHolder) any.extract_Streamable()).value;
      }
    catch (ClassCastException cex)
      {
        BAD_OPERATION bad = new BAD_OPERATION("TaggedProfile expected");
        bad.initCause(cex);
        throw bad;
      }
  }

  /**
   * Get the TaggedProfile repository id.
   *
   * @return "IDL:omg.org/IOP/TaggedProfile:1.0", always.
   */
  public static String id()
  {
    return "IDL:omg.org/IOP/TaggedProfile:1.0";
  }

  /**
   * Read the structure from the CDR intput stream.
   *
   * @param input a org.omg.CORBA.portable stream to read from.
   */
  public static TaggedProfile read(InputStream input)
  {
    TaggedProfile value = new TaggedProfile();
    value.tag = input.read_long();

    if (input instanceof cdrBufInput)
      {
        // Highly probable.
        value.profile_data = ((cdrBufInput) input).read_sequence();
      }
    else
      {
        value.profile_data = new byte[input.read_long()];
        for (int i0 = 0; i0 < value.profile_data.length; i0++)
          value.profile_data[i0] = input.read_octet();
      }
    return value;
  }

  /**
   * Write the structure to the CDR output stream.
   *
   * @param output a org.omg.CORBA.portable stream stream to write into.
   * @param value a value to write.
   */
  public static void write(OutputStream output, TaggedProfile value)
  {
    output.write_long(value.tag);

    if (output instanceof cdrBufOutput)
      {
        // Highly probable.
        output.write_long(value.profile_data.length);
        try
          {
            output.write(value.profile_data);
          }
        catch (IOException e)
          {
            MARSHAL m = new MARSHAL();
            m.initCause(e);
            throw m;
          }
      }
    else
      {
        output.write_long(value.profile_data.length);
        for (int i0 = 0; i0 < value.profile_data.length; i0++)
          output.write_octet(value.profile_data[i0]);
      }
  }
}