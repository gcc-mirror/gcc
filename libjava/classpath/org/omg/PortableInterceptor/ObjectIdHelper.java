/* ObjectIdHelper.java --
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

import gnu.CORBA.OrbRestricted;

import org.omg.CORBA.Any;
import org.omg.CORBA.ORB;
import org.omg.CORBA.OctetSeqHelper;
import org.omg.CORBA.OctetSeqHolder;
import org.omg.CORBA.TypeCode;
import org.omg.CORBA.portable.InputStream;
import org.omg.CORBA.portable.OutputStream;

/**
 * The Object Id of this package is defined in OMG specification as a byte array.
 * As such, the Object Id needs no helper, but one is included in the API anyway.
 *
 * @since 1.5
 *
 * @author Audrius Meskauskas, Lithuania (AudriusA@Bioinformatics.org)
 */
public abstract class ObjectIdHelper
{
  /**
   * Insert the Object Id into Any.
   *
   * @param a the Any to insert into.
   * @param that the string to insert.
   */
  public static void insert(Any a, byte[] that)
  {
    a.insert_Streamable(new OctetSeqHolder(that));
    a.type(type());
  }

  /**
   * Extract the Object Id from Any.
   *
   * @param a the Any to extract from.
   */
  public static byte[] extract(Any a)
  {
    return ((OctetSeqHolder) a.extract_Streamable()).value;
  }

  /**
   * Return an alias typecode (an alias of the octet sequence).
   */
  public static TypeCode type()
  {
    ORB orb = OrbRestricted.Singleton;
    return orb.create_alias_tc(id(), "ObjectId", OctetSeqHelper.type());
  }

  /**
   * Return the Object Id repository id.
   * @return "IDL:omg.org/PortableInterceptor/ObjectId:1.0", always.
   */
  public static String id()
  {
    return "IDL:omg.org/PortableInterceptor/ObjectId:1.0";
  }

  /**
   * Read the Object Id as a byte array.
   *
   * @param input the stream to read from.
   */
  public static byte[] read(InputStream input)
  {
    return OctetSeqHelper.read(input);
  }

  /**
   * Write the Object Id as a byte array.
   *
   * @param output the stream to write into.
   * @param value the Object Id value to write.
   */
  public static void write(OutputStream output, byte[] value)
  {
    OctetSeqHelper.write(output, value);
  }
}
