/* TaggedComponentHelper.java --
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
import org.omg.CORBA.MARSHAL;
import org.omg.CORBA.ORB;
import org.omg.CORBA.StructMember;
import org.omg.CORBA.TCKind;
import org.omg.CORBA.TypeCode;
import org.omg.CORBA.portable.InputStream;
import org.omg.CORBA.portable.OutputStream;

import java.io.IOException;

/**
 * A helper operations for the {@link TaggedComponent}.
 *
 * @author Audrius Meskauskas, Lithuania (AudriusA@Bioinformatics.org)
 */
public abstract class TaggedComponentHelper
{
  /**
   * Create the TaggedComponent typecode (structure, named "TaggedComponent").
   * The typecode states that the structure contains the following fields: tag,
   * component_data.
   */
  public static TypeCode type()
  {
    ORB orb = OrbRestricted.Singleton;
    StructMember[] members = new StructMember[2];
    
    TypeCode field;
    
    field = orb.create_alias_tc("IDL:omg.org/IOP/ComponentId:1.0",
                                "ComponentId",
                                orb.get_primitive_tc(TCKind.tk_ulong));
    members[0] = new StructMember("tag", field, null);
    
    field = orb.create_sequence_tc(0, orb.get_primitive_tc(TCKind.tk_octet));
    members[1] = new StructMember("component_data", field, null);
    return orb.create_struct_tc(id(), "TaggedComponent", members);
  }

  /**
   * Insert the TaggedComponent into the given Any. This method uses the
   * TaggedComponentHolder.
   *
   * @param any the Any to insert into.
   * @param that the TaggedComponent to insert.
   */
  public static void insert(Any any, TaggedComponent that)
  {
    any.insert_Streamable(new TaggedComponentHolder(that));
  }

  /**
   * Extract the TaggedComponent from given Any. This method uses the
   * TaggedComponentHolder.
   *
   * @throws BAD_OPERATION if the passed Any does not contain TaggedComponent.
   */
  public static TaggedComponent extract(Any any)
  {
    try
      {
        return ((TaggedComponentHolder) any.extract_Streamable()).value;
      }
    catch (ClassCastException cex)
      {
        BAD_OPERATION bad = new BAD_OPERATION("TaggedComponent expected");
        bad.minor = Minor.Any;        
        bad.initCause(cex);
        throw bad;
      }
  }

  /**
   * Get the TaggedComponent repository id.
   *
   * @return "IDL:omg.org/IOP/TaggedComponent:1.0", always.
   */
  public static String id()
  {
    return "IDL:omg.org/IOP/TaggedComponent:1.0";
  }

  /**
   * Read the structure from the CDR intput stream. Expects the integer
   * identifier of the tag, then the size of the tag data and then the specified
   * number of bytes, representing the data of the tag.
   *
   * @param input a org.omg.CORBA.portable stream to read from.
   */
  public static TaggedComponent read(InputStream input)
  {
    TaggedComponent value = new TaggedComponent();
    value.tag = input.read_long();
    int length = input.read_long();
    value.component_data = new byte[length];
    input.read_octet_array(value.component_data, 0, length);
    return value;
  }

  /**
   * Write the structure to the CDR output stream. Writes the integer identifier
   * of the tag, then the size of the tag data and then the specified number of
   * bytes, representing the data of the tag.
   *
   * @param output a org.omg.CORBA.portable stream stream to write into.
   * @param value a value to write.
   */
  public static void write(OutputStream output, TaggedComponent value)
  {
    output.write_long(value.tag);
    output.write_long(value.component_data.length);
    output.write_octet_array(value.component_data, 0, value.component_data.length);
  }
}