/* MultipleComponentProfileHelper.java --
   Copyright (C) 2005, 2006 Free Software Foundation, Inc.

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

import org.omg.CORBA.ORB;
import org.omg.CORBA.Any;
import org.omg.CORBA.TypeCode;
import org.omg.CORBA.BAD_OPERATION;

import org.omg.CORBA.portable.OutputStream;
import org.omg.CORBA.portable.InputStream;

 /**
 * A helper operations for the array of {@link TaggedComponent}
 * (MultipleComponentProfile).
 *
 * @author Audrius Meskauskas, Lithuania (AudriusA@Bioinformatics.org)
 */
public abstract class MultipleComponentProfileHelper
{
  public static TypeCode type()
  {
    ORB orb = OrbRestricted.Singleton;
    return orb.create_sequence_tc(0, TaggedComponentHelper.type());
  }
  /**
   * Insert the MultipleComponentProfile into the given Any. This method uses
   * the MultipleComponentProfileHolder.
   * 
   * @param any the Any to insert into.
   * @param those the TaggedComponent[] to insert.
   */
  public static void insert(Any any, TaggedComponent[] those)
  {
    any.insert_Streamable(new MultipleComponentProfileHolder(those));
  }

  /**
   * Extract the MultipleComponentProfile from given Any.
   * This method uses the MultipleComponentProfileHolder.
   *
   * @throws BAD_OPERATION if the passed Any does not contain MultipleComponentProfile.
   */
  public static TaggedComponent[] extract(Any any)
  {
    try
      {
        return ((MultipleComponentProfileHolder) any.extract_Streamable()).value;
      }
    catch (ClassCastException cex)
      {
        BAD_OPERATION bad = new BAD_OPERATION("MultipleComponentProfile expected");
        bad.minor = Minor.Any;        
        bad.initCause(cex);
        throw bad;
      }
  }

  /**
   * Get the MultipleComponentProfile repository id.
   *
   * @return "IDL:omg.org/IOP/MultipleComponentProfile:1.0", always.
   */
  public static String id()
  {
    return "IDL:omg.org/IOP/MultipleComponentProfile:1.0";
  }

  /**
   * Read the sequence from the CDR intput stream.
   * Expects the array size (as CORBA long), followed by
   * the array members (if any).
   *
   * @param input a org.omg.CORBA.portable stream to read from.
   */
  public static TaggedComponent[] read(InputStream input)
  {
    TaggedComponent[] value;
    value = new TaggedComponent[ input.read_long() ];
    for (int i0 = 0; i0 < value.length; i0++)
      value[i0] = TaggedComponentHelper.read(input);
    return value;
  }

  /**
   * Write the structure to the CDR output stream.
   * Writes the array size (as CORBA long), followed by
   * the array members (if any).
   *
   * @param output a org.omg.CORBA.portable stream stream to write into.
   * @param value an array to write.
   */
  public static void write(OutputStream output, TaggedComponent[] value)
  {
    output.write_long(value.length);
    for (int i0 = 0; i0 < value.length; i0++)
      TaggedComponentHelper.write(output, value[i0]);
  }
}