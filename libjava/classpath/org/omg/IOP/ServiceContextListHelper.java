/* ServiceContextListHelper.java --
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

import org.omg.CORBA.Any;
import org.omg.CORBA.BAD_OPERATION;
import org.omg.CORBA.ORB;
import org.omg.CORBA.TypeCode;
import org.omg.CORBA.portable.InputStream;
import org.omg.CORBA.portable.OutputStream;

/**
* The helper operations for the
* CORBA object {@link ServiceContext[]}.
*
* @author Audrius Meskauskas, Lithuania (AudriusA@Bioinformatics.org)
*/
public abstract class ServiceContextListHelper
{
  /**
   * The cached {@link ServiceContext[]} typecode, computed once.
   */
  private static TypeCode typeCode;

  /**
   * Get the type code of the {@link ServiceContext[]}.
   */
  public static TypeCode type()
  {
    if (typeCode == null)
      typeCode = ORB.init().create_interface_tc(id(), "ServiceContextList");
    return typeCode;
  }

  /**
   * Insert the ServiceContext[] into the given Any.
   *
   * @param any the Any to insert into.
   * @param that the ServiceContext[] to insert.
   */
  public static void insert(Any any, ServiceContext[] that)
  {
    any.insert_Streamable(new ServiceContextListHolder(that));
  }

  /**
   * Extract the ServiceContext[] from given Any.
   *
   * @throws BAD_OPERATION if the passed Any does not contain ServiceContext[].
   */
  public static ServiceContext[] extract(Any any)
  {
    try
      {
        ServiceContextListHolder holder =
          (ServiceContextListHolder) any.extract_Streamable();
        return holder.value;
      }
    catch (ClassCastException ex)
      {
        BAD_OPERATION bad = new BAD_OPERATION("ServiceContext[] expected");
        bad.minor = Minor.Any;        
        bad.initCause(ex);
        throw bad;
      }
  }

  /**
   * Get the ServiceContext[] repository id.
   *
   * @return "IDL:omg.org/IOP/ServiceContextList:1.0", always.
   */
  public static String id()
  {
    return "IDL:omg.org/IOP/ServiceContextList:1.0";
  }

  /**
   * Read the ServiceContext[] from the CDR intput stream as a flexible lenth
   * sequence.
   *
   * @param input a org.omg.CORBA.portable stream to read from.
   */
  public static ServiceContext[] read(InputStream input)
  {
    ServiceContext[] value = new ServiceContext[ input.read_long() ];
    for (int i = 0; i < value.length; i++)
      {
        value [ i ] = ServiceContextHelper.read(input);
      }
    return value;
  }

  /**
   * Write the ServiceContext[] to the CDR output stream as a flexible length
   * sequence.
   *
   * @param output a org.omg.CORBA.portable stream stream to write into.
   * @param value a value to write.
   */
  public static void write(OutputStream output, ServiceContext[] value)
  {
    output.write_long(value.length);
    for (int i = 0; i < value.length; i++)
      {
        ServiceContextHelper.write(output, value [ i ]);
      }
  }
}