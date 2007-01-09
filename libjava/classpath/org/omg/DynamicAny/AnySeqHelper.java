/* AnySeqHelper.java --
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

import gnu.CORBA.OrbRestricted;

import org.omg.CORBA.Any;
import org.omg.CORBA.AnySeqHolder;
import org.omg.CORBA.ORB;
import org.omg.CORBA.TCKind;
import org.omg.CORBA.TypeCode;
import org.omg.CORBA.portable.InputStream;
import org.omg.CORBA.portable.OutputStream;

/**
 * A helper operations for the array of {@link Any}. This class shares
 * the same {@link AnySeqHolder} as the {@link org.omg.CORBA.AnySeqHelper}.
 *
 * @author Audrius Meskauskas, Lithuania (AudriusA@Bioinformatics.org)
 */
public abstract class AnySeqHelper
{
  /**
   * Delegates call to {@link org.omg.CORBA.AnySeqHelper#extract}.
   */
  public static Any[] extract(Any any)
  {
    return org.omg.CORBA.AnySeqHelper.extract(any);
  }

  /**
   * Get the AnySeq repository id.
   *
   * @return "IDL:omg.org/DynamicAny/AnySeq:1.0", always.
   */
  public static String id()
  {
    return "IDL:omg.org/DynamicAny/AnySeq:1.0";
  }

  /**
   * Delegates call to {@link org.omg.CORBA.AnySeqHelper#insert}.
   */
  public static void insert(Any any, Any[] those)
  {
    org.omg.CORBA.AnySeqHelper.insert(any, those);
  }

  /**
   * Delegates call to {@link org.omg.CORBA.AnySeqHelper#read}.
   *
   * @param input a org.omg.CORBA.portable stream to read from.
   */
  public static Any[] read(InputStream input)
  {
    return org.omg.CORBA.AnySeqHelper.read(input);
  }

  /**
   * Get the typecode as officially defined for this helper.
   *
   * @return alias of unbounded sequence of Any's, named AnySeq,
   * with the id, returned by {@link #id()}.
   */
  public static TypeCode type()
  {
    ORB orb = OrbRestricted.Singleton;
    TypeCode t =
      orb.create_sequence_tc(0, orb.get_primitive_tc(TCKind.tk_any));
    return orb.create_alias_tc(id(), "AnySeq", t);
  }

  /**
   * Delegates call to {@link org.omg.CORBA.AnySeqHelper#write}.
   */
  public static void write(OutputStream output, Any[] value)
  {
    org.omg.CORBA.AnySeqHelper.write(output, value);
  }
}