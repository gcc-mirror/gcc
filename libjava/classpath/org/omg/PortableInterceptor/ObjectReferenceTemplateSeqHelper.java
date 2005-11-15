/*  ObjectReferenceTemplateSeqHelper.java --
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

package org.omg.PortableInterceptor;

import gnu.CORBA.Minor;
import gnu.CORBA.typecodes.GeneralTypeCode;

import org.omg.CORBA.Any;
import org.omg.CORBA.BAD_OPERATION;
import org.omg.CORBA.TCKind;
import org.omg.CORBA.TypeCode;
import org.omg.CORBA.portable.InputStream;
import org.omg.CORBA.portable.OutputStream;
import org.omg.CORBA.portable.Streamable;

/**
 * Provides static helper methods for working with the array of object reference
 * templates.
 * 
 * @author Audrius Meskauskas (AudriusA@Bioinformatics.org)
 */
public abstract class ObjectReferenceTemplateSeqHelper
{
  /**
   * The type code, computed once.
   */
  static TypeCode typecode;

  /**
   * Extract the <code>ObjectReferenceTemplate[]</code> from the given
   * {@link Any}. This implementation expects the {@link Any} to hold the
   * instance of {@link ObjectReferenceTemplateSeqHolder} that is returned by
   * {@link Any#extract_Streamable() }.
   * 
   * @param a an Any to extract the array from.
   * 
   * @return the extracted array.
   * 
   * @throws BAD_OPERATION if the Any contains something other than the the
   * {@link ObjectReferenceTemplateSeqHolder}.
   */
  public static ObjectReferenceTemplate[] extract(Any a)
  {
    try
      {
        ObjectReferenceTemplateSeqHolder h = (ObjectReferenceTemplateSeqHolder) 
          a.extract_Streamable();
        return h.value;
      }
    catch (ClassCastException cex)
      {
        BAD_OPERATION bad = new BAD_OPERATION(
          "ObjectReferenceTemplate[] expected");
        bad.initCause(cex);
        bad.minor = Minor.Any;
        throw bad;
      }
  }

  /**
   * Returns the object reference template sequence repository Id.
   * 
   * @return "IDL:omg.org/PortableInterceptor/ObjectReferenceTemplateSeq:1.0",
   * always.
   */
  public static String id()
  {
    return "IDL:omg.org/PortableInterceptor/ObjectReferenceTemplateSeq:1.0";
  }

  /**
   * Insert into the given <code>ObjectReferenceTemplate[]</code> into the
   * given {@link Any}. This implementation first creates a
   * {@link ObjectReferenceTemplateSeqHolder} and then calls
   * {@link Any#insert_Streamable(Streamable)}.
   * 
   * @param into the target Any.
   * @param that the array to insert.
   */
  public static void insert(Any into, ObjectReferenceTemplate[] that)
  {
    ObjectReferenceTemplateSeqHolder holder = 
      new ObjectReferenceTemplateSeqHolder(that);
    into.insert_Streamable(holder);
  }

  /**
   * Reads the <code>ObjectReferenceTemplate[]</code> from the CORBA input
   * stream.
   * 
   * @param input the CORBA (not java.io) stream to read from.
   * @return the value from the stream.
   */
  public static ObjectReferenceTemplate[] read(InputStream input)
  {
    ObjectReferenceTemplate[] value = 
      new ObjectReferenceTemplate[input.read_long()];
    for (int i = 0; i < value.length; i++)
      value[i] = ObjectReferenceTemplateHelper.read(input);
    return value;
  }

  /**
   * Creates and returns a new instance of the TypeCode, corresponding the CORBA
   * <code>ObjectReferenceTemplate[]</code>. The length of the sequence is
   * left with the initial value 0.
   */
  public static TypeCode type()
  {
    if (typecode == null)
      {
        GeneralTypeCode t = new GeneralTypeCode(TCKind.tk_sequence);
        t.setId(id());
        t.setLength(0);
        t.setContentType(ObjectReferenceTemplateHelper.type());
        typecode = t;
      }
    return typecode;
  }

  /**
   * Writes the <code>ObjectReferenceTemplate[]</code> into the given stream.
   * 
   * @param output the CORBA (not java.io) output stream to write.
   * @param value the value that must be written.
   */
  public static void write(OutputStream output, ObjectReferenceTemplate[] value)
  {
    output.write_long(value.length);

    for (int i = 0; i < value.length; i++)
      ObjectReferenceTemplateHelper.write(output, value[i]);
  }
}
