/* ObjectReferenceTemplateHelper.java --
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


package org.omg.PortableInterceptor;

import gnu.CORBA.Minor;
import gnu.CORBA.OrbRestricted;

import org.omg.CORBA.Any;
import org.omg.CORBA.BAD_OPERATION;
import org.omg.CORBA.TypeCode;
import org.omg.CORBA.VM_ABSTRACT;
import org.omg.CORBA.ValueMember;
import org.omg.CORBA.portable.InputStream;
import org.omg.CORBA.portable.OutputStream;

/**
 * The helper operations for the  CORBA object
 * {@link ObjectReferenceTemplate}.
 *
 * @author Audrius Meskauskas, Lithuania (AudriusA@Bioinformatics.org)
 */
public abstract class ObjectReferenceTemplateHelper
{
  /**
   * Get the type code of the {@link ObjectReferenceTemplate}.
   * 
   * @return value type type code, named ObjectReferenceTemplate, no members,
   *         abstract.
   */
  public static TypeCode type()
  {
    return OrbRestricted.Singleton.create_value_tc(id(), "ObjectReferenceTemplate",
                                      VM_ABSTRACT.value, null,
                                      new ValueMember[0]);
  }

  /**
   * Insert the ObjectReferenceTemplate into the given Any.
   *
   * @param any the Any to insert into.
   * @param that the ObjectReferenceTemplate to insert.
   */
  public static void insert(Any any, ObjectReferenceTemplate that)
  {
    any.insert_Streamable(new ObjectReferenceTemplateHolder(that));
  }

  /**
   * Extract the ObjectReferenceTemplate from given Any.
   *
   * @throws BAD_OPERATION if the passed Any does not
   * contain ObjectReferenceTemplate.
   */
  public static ObjectReferenceTemplate extract(Any any)
  {
    try
      {
        ObjectReferenceTemplateHolder h =
          (ObjectReferenceTemplateHolder) any.extract_Streamable();
        return h.value;
      }
    catch (ClassCastException ex)
      {
        BAD_OPERATION bad =
          new BAD_OPERATION("ObjectReferenceTemplate expected");
        bad.minor = Minor.Any;
        bad.initCause(ex);
        throw bad;
      }
  }

  /**
   * Get the ObjectReferenceTemplate repository id.
   *
   * @return "IDL:omg.org/PortableInterceptor/ObjectReferenceTemplate:1.0",
   * always.
   */
  public static String id()
  {
    return "IDL:omg.org/PortableInterceptor/ObjectReferenceTemplate:1.0";
  }

  /**
   * Read the object reference template (as the value type).
   */
  public static ObjectReferenceTemplate read(InputStream input)
  {
    return (ObjectReferenceTemplate)
      ((org.omg.CORBA_2_3.portable.InputStream) input).read_value();
  }

  /**
   * Write the object reference template (as the value type).
   */
  public static void write(OutputStream output, ObjectReferenceTemplate value)
  {
    ((org.omg.CORBA_2_3.portable.OutputStream) output).
      write_value(value);
  }
}