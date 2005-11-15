/* ValueBaseHelper.java --
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

import gnu.CORBA.CDR.Vio;
import gnu.CORBA.typecodes.RecordTypeCode;

import org.omg.CORBA.portable.InputStream;
import org.omg.CORBA.portable.OutputStream;

import java.io.Serializable;

/**
 * A helper operations for the value base type ({@link ValueBase}).
 *
 * @since 1.3
 *
 * @author Audrius Meskauskas, Lithuania (AudriusA@Bioinformatics.org)
 */
public abstract class ValueBaseHelper
{
  /**
   * Extract the value type from the given Any.
   *
   * @param a the Any to extract from.
   *
   * @return the extracted value type.
   */
  public static Serializable extract(Any a)
  {
    return a.extract_Value();
  }

  /**
   * Return the value base helper repository id.
   *
   * @return IDL:omg.org/CORBA/ValueBase:1.0, always.
   */
  public static String id()
  {
    return "IDL:omg.org/CORBA/ValueBase:1.0";
  }

  /**
   * Insert the value base that is Serializable into
   * the given Any.
   *
   * @param a the Any to insert into.
   * @param that the value base to insert.
   */
  public static void insert(Any a, Serializable that)
  {
    a.insert_Value(that);
  }

  /**
   * Read the value base from the given input stream.
   *
   * @param input a stream to read from.
   *
   * @return the loaded value.
   *
   * @throws MARSHAL if the reading has failed due any reason.
   */
  public static Serializable read(InputStream input)
  {
    return Vio.read(input);
  }

  /**
   * Get the typecode of the value type.
   * @return
   */
  public static TypeCode type()
  {
    RecordTypeCode r = new RecordTypeCode(TCKind.tk_value);
    return r;
  }

  /**
   * Write the value base into the given stream.
   *
   * If the passed value implements the {@link CustomMarshal},
   * the helper uses {@link CustomMarshal#marshal}
   * to write the content in a user defined way. Otherwise,
   * this implementation initialises the {@link ObjectOutputStream}
   * and writes through it.
   *
   * @param output a stream to write to.
   *
   * @param value a value to write.
   *
   * @throws MARSHAL if the writing failed due any reason.
   */
  public static void write(OutputStream output, Serializable value)
  {
    Vio.write(output, value);
  }
}