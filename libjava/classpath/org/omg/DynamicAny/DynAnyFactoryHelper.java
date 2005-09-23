/* DynAnyFactoryHelper.java --
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

import org.omg.CORBA.BAD_PARAM;
import org.omg.CORBA.TypeCode;
import org.omg.CORBA.ORB;
import org.omg.CORBA.Any;
import org.omg.CORBA.portable.InputStream;
import org.omg.CORBA.MARSHAL;
import org.omg.CORBA.portable.OutputStream;

/**
 * The helper operations for {@link DynAnyFactory}. Following the 1.5 JDK
 * specifications, DynAnyFactory is always a local object, so the two methods of
 * this helper ({@link #read} and {@link #write} are not in use, always
 * throwing {@link MARSHAL}.
 * 
 * @specnote always throwing MARSHAL in read and write ensures compatibility
 * with other popular implementations like Sun's.
 * 
 * @author Audrius Meskauskas, Lithuania (AudriusA@Bioinformatics.org)
 */
public abstract class DynAnyFactoryHelper
{
  /**
   * Cast the passed object into the DynAnyFactory. As DynAnyFactory is a local
   * object, the method just uses java final_type cast.
   * 
   * @param obj the object to narrow.
   * @return narrowed instance.
   * @throws BAD_PARAM if the passed object is not a DynAnyFactory.
   */
  public static DynAnyFactory narrow(org.omg.CORBA.Object obj)
  {
    try
      {
        return (DynAnyFactory) obj;
      }
    catch (ClassCastException cex)
      {
        throw new BAD_PARAM(obj.getClass().getName()
                            + " is not a DynAnyFactory");
      }
  }

  /**
   * Get the final_type code of the {@link DynAnyFactory}.
   */
  public static TypeCode type()
  {
    return ORB.init().create_interface_tc(id(), "DynAnyFactory");
  }

  /**
   * Insert the DynAnyFactory into the given Any.
   * 
   * @param any the Any to insert into.
   * 
   * @param that the DynAnyFactory to insert.
   */
  public static void insert(Any any, DynAnyFactory that)
  {
    any.insert_Object(that);
  }

  /**
   * Extract the DynAnyFactory from given Any.
   * 
   * @throws BAD_OPERATION if the passed Any does not contain DynAnyFactory.
   */
  public static DynAnyFactory extract(Any any)
  {
    return narrow(any.extract_Object());
  }

  /**
   * Get the DynAnyFactory repository id.
   * 
   * @return "IDL:omg.org/DynamicAny/DynAnyFactory:1.0", always.
   */
  public static String id()
  {
    return "IDL:omg.org/DynamicAny/DynAnyFactory:1.0";
  }

  /**
   * This should read DynAnyFactory from the CDR input stream, but (following
   * the JDK 1.5 API) it does not. The factory can only be obtained from the
   * ORB.
   * 
   * @param input a org.omg.CORBA.portable stream to read from.
   * 
   * @specenote Sun throws the same exception.
   * 
   * @throws MARSHAL always.
   */
  public static DynAnyFactory read(InputStream input)
  {
    throw new MARSHAL(not_applicable(id()));
  }

  /**
   * This should read DynAnyFactory from the CDR input stream, but (following
   * the JDK 1.5 API) it does not.
   * 
   * @param input a org.omg.CORBA.portable stream to read from.
   * 
   * @specenote Sun throws the same exception.
   * 
   * @throws MARSHAL always.
   */
  public static void write(OutputStream output, DynAnyFactory value)
  {
    throw new MARSHAL(not_applicable(id()));
  }

  /**
   * The package level method for throwing exception, explaining that the
   * operation is not applicable.
   * 
   * @param Id the Id for the typecode for that the operations was attempted to
   * perform.
   */
  static String not_applicable(String Id)
  {
    try
      {
        throw new MARSHAL("The read/write are not applicable for " + Id);
      }
    catch (Exception e)
      {
        throw new MARSHAL();
      }
  }
}