/* DynAnySeq.java --
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


package org.omg.DynamicAny;

import gnu.CORBA.DynAnySeqHolder;

import org.omg.CORBA.Any;
import org.omg.CORBA.BAD_OPERATION;
import org.omg.CORBA.MARSHAL;
import org.omg.CORBA.ORB;
import org.omg.CORBA.TypeCode;
import org.omg.CORBA.portable.InputStream;
import org.omg.CORBA.portable.OutputStream;

/**
 * A helper operations for the array of {@link DynAny} ({@link DynAnySeq}).
 * Following the 1.5 JDK specifications, DynAny (and hence an sequence of
 * DynAny's) is always a local object, so the two methods of this helper
 * ({@link #read} and {@link #write} are not in use, always throwing
 * {@link MARSHAL}.
 *
 * @specnote always throwing MARSHAL in read and write ensures compatibility
 * with other popular implementations like Sun's.
 *
 * @author Audrius Meskauskas, Lithuania (AudriusA@Bioinformatics.org)
 */
public abstract class DynAnySeqHelper
{
  /**
   * The cached typecode value, computed only once.
   */
  private static TypeCode typeCode;

  public static TypeCode type()
  {
    if (typeCode == null)
      {
        ORB orb = ORB.init();
        TypeCode t = orb.create_sequence_tc(0, DynAnyHelper.type());
        typeCode = orb.create_alias_tc(id(), "DynAnySeq", t);
      }
    return typeCode;
  }

  /**
   * Insert the DynAnySeq into the given Any.
   * This method uses the DynAnySeqHolder.
   *
   * @param any the Any to insert into.
   * @param those the DynAny[] to insert.
   */
  public static void insert(Any any, DynAny[] those)
  {
    any.insert_Streamable(new DynAnySeqHolder(those));
  }

  /**
   * Extract the DynAnySeq from given Any.
   * This method uses the DynAnySeqHolder.
   *
   * @throws BAD_OPERATION if the passed Any does not contain DynAnySeq.
   */
  public static DynAny[] extract(Any any)
  {
    try
      {
        return ((DynAnySeqHolder) any.extract_Streamable()).value;
      }
    catch (ClassCastException cex)
      {
        BAD_OPERATION bad = new BAD_OPERATION("DynAnySeq expected");
        bad.initCause(cex);
        throw bad;
      }
  }

  /**
   * Get the DynAnySeq repository id.
   *
   * @return "IDL:omg.org/DynamicAny/DynAnySeq:1.0", always.
   */
  public static String id()
  {
    return "IDL:omg.org/DynamicAny/DynAnySeq:1.0";
  }

  /**
   * The method should read this object from the CDR input stream, but
   * (following the JDK 1.5 API) it does not.
   *
   * @param input a org.omg.CORBA.portable stream to read from.
   *
   * @specenote Sun throws the same exception.
   *
   * @throws MARSHAL always.
   */
  public static DynAny[] read(InputStream input)
  {
    throw new MARSHAL(DynAnyFactoryHelper.not_applicable(id()));
  }

  /**
   * The method should write this object to the CDR input stream, but
   * (following the JDK 1.5 API) it does not.
   *
   * @param input a org.omg.CORBA.portable stream to read from.
   *
   * @specenote Sun throws the same exception.
   *
   * @throws MARSHAL always.
   */
  public static void write(OutputStream output, DynAny[] value)
  {
    throw new MARSHAL(DynAnyFactoryHelper.not_applicable(id()));
  }
}