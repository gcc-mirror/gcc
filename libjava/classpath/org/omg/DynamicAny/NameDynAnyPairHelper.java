/* NameDynAnyPairHelper.java --
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


package org.omg.DynamicAny;

import gnu.CORBA.Minor;
import gnu.CORBA.NameDynAnyPairHolder;
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

/**
 * A helper operations for the structure {@link NameDynAnyPair}.
 *
 * Following the 1.5 JDK specifications, DynAny (and hence any structure,
 * containing DynAny) is always a local object, so the two methods of this
 * helper ({@link #read} and {@link #write} are not in use, always throwing
 * {@link MARSHAL}.
 *
 * @author Audrius Meskauskas, Lithuania (AudriusA@Bioinformatics.org)
 */
public abstract class NameDynAnyPairHelper
{
  /**
   * Extract the NameDynAnyPair from given Any.
   * This method uses the NameDynAnyPairHolder.
   *
   * @throws BAD_OPERATION if the passed Any does not contain NameDynAnyPair.
   */
  public static NameDynAnyPair extract(Any any)
  {
    try
      {
        return ((NameDynAnyPairHolder) any.extract_Streamable()).value;
      }
    catch (ClassCastException cex)
      {
        BAD_OPERATION bad = new BAD_OPERATION("NameDynAnyPair expected");
        bad.initCause(cex);
        bad.minor = Minor.Any;
        throw bad;
      }
  }

  /**
   * Get the NameDynAnyPair repository id.
   *
   * @return "IDL:omg.org/DynamicAny/NameDynAnyPair:1.0", always.
   */
  public static String id()
  {
    return "IDL:omg.org/DynamicAny/NameDynAnyPair:1.0";
  }

  /**
   * Create the NameDynAnyPair typecode (structure,
   * named "NameDynAnyPair").
   * The typecode states that the structure contains the
   * following fields: id, value.
   */
  public static TypeCode type()
  {
    ORB orb = OrbRestricted.Singleton;
    StructMember[] members = new StructMember[ 2 ];

    TypeCode field;

    field =
      orb.create_alias_tc("IDL:omg.org/DynamicAny/FieldName:1.0",
                          "FieldName",
                          orb.get_primitive_tc(TCKind.tk_string)
      );
    members [ 0 ] = new StructMember("id", field, null);

    field = DynAnyHelper.type();
    members [ 1 ] = new StructMember("value", field, null);
    return orb.create_struct_tc(id(), "NameDynAnyPair", members);
  }

  /**
   * Insert the NameDynAnyPair into the given Any.
   * This method uses the NameDynAnyPairHolder.
   *
   * @param any the Any to insert into.
   * @param that the NameDynAnyPair to insert.
   */
  public static void insert(Any any, NameDynAnyPair that)
  {
    any.insert_Streamable(new NameDynAnyPairHolder(that));
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
  public static NameDynAnyPair read(InputStream input)
  {
    throw new MARSHAL(DynAnyFactoryHelper.not_applicable(id()));
  }

  /**
   * The method should write this object to the CDR input stream, but
   * (following the JDK 1.5 API) it does not.
   *
   * @param output a org.omg.CORBA.portable stream to write into.
   *
   * @specenote Sun throws the same exception.
   *
   * @throws MARSHAL always.
   */
  public static void write(OutputStream output, NameDynAnyPair value)
  {
    throw new MARSHAL(DynAnyFactoryHelper.not_applicable(id()));
  }
}
