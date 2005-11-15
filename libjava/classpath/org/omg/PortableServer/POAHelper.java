/* POAHelper.java --
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


package org.omg.PortableServer;

import gnu.CORBA.Minor;

import org.omg.CORBA.BAD_PARAM;
import org.omg.CORBA.TypeCode;
import org.omg.CORBA.ORB;
import org.omg.CORBA.Any;
import org.omg.CORBA.portable.InputStream;
import org.omg.CORBA.MARSHAL;
import org.omg.CORBA.portable.OutputStream;

/**
 * The helper operations for the CORBA object {@link POA}.
 *
 * @author Audrius Meskauskas, Lithuania (AudriusA@Bioinformatics.org)
 */
public abstract class POAHelper
{
  /**
   * Cast the passed object into the POA. As POA is a local object, the method
   * just uses java type cast.
   *
   * @param obj the object to narrow.
   * @return narrowed instance.
   * @throws BAD_PARAM if the passed object is not a POA.
   */
  public static POA narrow(org.omg.CORBA.Object obj)
  {
    try
      {
        return (POA) obj;
      }
    catch (ClassCastException cex)
      {
        throw new BAD_PARAM(obj.getClass().getName() + " is not a POA");
      }
  }

  /**
   * Get the type code of the {@link POA}.
   */
  public static TypeCode type()
  {
    return ORB.init().create_interface_tc(id(), "POA");
  }

  /**
   * Insert the POA into the given Any.
   *
   * @param any the Any to insert into.
   *
   * @param that the POA to insert.
   */
  public static void insert(Any any, POA that)
  {
    any.insert_Object(that);
  }

  /**
   * Extract the POA from given Any.
   *
   * @throws BAD_OPERATION if the passed Any does not contain POA.
   */
  public static POA extract(Any any)
  {
    return narrow(any.extract_Object());
  }

  /**
   * Get the POA repository id.
   *
   * @return "IDL:omg.org/PortableServer/POA:2.3", always.
   */
  public static String id()
  {
    return "IDL:omg.org/PortableServer/POA:2.3";
  }

  /**
   * This should read POA from the CDR input stream, but, following the specs,
   * it doesnot. The jdk 1.5 API specification defines that POA cannot be
   * exported.
   *
   * @param input a org.omg.CORBA.portable stream to read from.
   *
   * @specenote Sun throws the same exception.
   *
   * @throws MARSHAL, always.
   */
  public static POA read(InputStream input)
  {
    MARSHAL m = new MARSHAL("Inappropriate");
    m.minor = Minor.Inappropriate;
    throw m;
  }

  /**
   * This should read POA from the CDR input stream, but, following the specs,
   * it doesnot. The jdk 1.5 API specification defines that POA cannot be
   * exported.
   *
   * @param input a org.omg.CORBA.portable stream to read from.
   *
   * @specenote Sun throws the same exception.
   *
   * @throws MARSHAL, always.
   */
  public static void write(OutputStream output, POA value)
  {
    MARSHAL m = new MARSHAL("Inappropriate");
    m.minor = Minor.Inappropriate;
    throw m;
  }
}