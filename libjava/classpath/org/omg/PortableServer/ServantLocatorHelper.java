/* ServantLocatorHelper.java --
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

import org.omg.CORBA.Any;
import org.omg.CORBA.BAD_OPERATION;
import org.omg.CORBA.MARSHAL;
import org.omg.CORBA.ORB;
import org.omg.CORBA.TypeCode;
import org.omg.CORBA.portable.InputStream;
import org.omg.CORBA.portable.OutputStream;

/**
* The helper operations for the CORBA object {@link ServantLocator}.
*
* @author Audrius Meskauskas, Lithuania (AudriusA@Bioinformatics.org)
*/
public abstract class ServantLocatorHelper
{
  /**
   * Get the type code of the {@link ServantLocator}.
   */
  public static TypeCode type()
  {
    return ORB.init().create_interface_tc(id(), "ServantLocator");
  }

  /**
   * Insert the ServantLocator into the given Any.
   *
   * @param any the Any to insert into.
   * @param that the ServantLocator to insert.
   */
  public static void insert(Any any, ServantLocator that)
  {
    any.insert_Object(that);
  }

  /**
   * Extract the ServantLocator from given Any.
   *
   * @throws BAD_OPERATION if the passed Any does not contain ServantLocator.
   */
  public static ServantLocator extract(Any any)
  {
    return narrow(any.extract_Object());
  }

  /**
   * Get the ServantLocator repository id.
   *
   * @return "org.omg.PortableServer.ServantLocatorOperations", always.
   */
  public static String id()
  {
    return "org.omg.PortableServer.ServantLocatorOperations";
  }

  /**
   * Cast the passed object into the ServantLocator.
   *
   * @param obj the object to narrow.
   * @return narrowed instance.
   * @throws BAD_PARAM if the passed object is not a ServantLocator.
   */
  public static ServantLocator narrow(org.omg.CORBA.Object obj)
  {
    try
      {
        return (ServantLocator) obj;
      }
    catch (ClassCastException ex)
      {
        BAD_OPERATION bad = new BAD_OPERATION();
        bad.initCause(ex);
        throw bad;
      }
  }

  /**
   * This should read the servant locator, but it cannot be transferred
   * this way as its operations cannot be remote. The operations cannot
   * be remote because one of the method parameters, POA, is required to be
   * always a local object (both by 1.5 API and 3.0.3 OMG).
   *
   * @throws MARSHAL, always.
   *
   * @specnote Same as Sun.
   */
  public static ServantLocator read(InputStream input)
  {
    throw new MARSHAL();
  }

  /**
   * This should write the servant activator, but it cannot be transferred
   * this way as its operations cannot be remote. The operations cannot
   * be remote because one of the method parameters, POA, is required to be
   * always a local object (both by 1.5 API and 3.0.3 OMG).
   *
   * @throws MARSHAL, always.
   *
   * @specnote Same as Sun.
   */
  public static void write(OutputStream output, ServantLocator value)
  {
    throw new MARSHAL();
  }
}