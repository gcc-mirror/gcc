/* ServantActivatorHelper.java --
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


package org.omg.PortableServer;

import gnu.CORBA.Minor;
import gnu.CORBA.OrbRestricted;

import org.omg.CORBA.Any;
import org.omg.CORBA.BAD_OPERATION;
import org.omg.CORBA.BAD_PARAM;
import org.omg.CORBA.MARSHAL;
import org.omg.CORBA.TypeCode;
import org.omg.CORBA.portable.InputStream;
import org.omg.CORBA.portable.OutputStream;

/**
* The helper operations for the CORBA object {@link ServantActivator}.
*
* @author Audrius Meskauskas, Lithuania (AudriusA@Bioinformatics.org)
*/
public abstract class ServantActivatorHelper
{
  /**
   * Get the type code of the {@link ServantActivator}.
   */
  public static TypeCode type()
  {
    return OrbRestricted.Singleton.create_interface_tc(id(), "ServantActivator");
  }

  /**
   * Insert the ServantActivator into the given Any.
   *
   * @param any the Any to insert into.
   * @param that the ServantActivator to insert.
   */
  public static void insert(Any any, ServantActivator that)
  {
    any.insert_Object(that);
  }

  /**
   * Extract the ServantActivator from given Any.
   *
   * @throws BAD_OPERATION if the passed Any does not contain ServantActivator.
   */
  public static ServantActivator extract(Any any)
  {
    return narrow(any.extract_Object());
  }

  /**
   * Get the ServantActivator repository id.
   *
   * @return "IDL:omg.org/PortableServer/ServantActivator:2.3", always.
   */
  public static String id()
  {
    return "IDL:omg.org/PortableServer/ServantActivator:2.3";
  }

  /**
   * Casts the passed object into the ServantActivator.
   *
   * @param obj the object to cast.
   * @return casted instance.
   * @throws BAD_PARAM if the passed object is not a ServantActivator.
   */
  public static ServantActivator narrow(org.omg.CORBA.Object obj)
  {
    try
      {
        return (ServantActivator) obj;
      }
    catch (ClassCastException ex)
      {
        BAD_PARAM bad = new BAD_PARAM();
        bad.initCause(ex);
        throw bad;
      }
  }
  
  /**
   * Narrow the given object to the ServantActivator. For the objects that are
   * always local, this operation does not differ from the ordinary
   * {@link #narrow} (ClassCastException will be thrown if narrowing something
   * different). See OMG issue 4158.
   * 
   * @param obj the object to cast.
   * 
   * @return the casted ServantActivator.
   * 
   * @since 1.5 
   */
  public static ServantActivator unchecked_narrow(org.omg.CORBA.Object obj)
  {
    return narrow(obj);
  }  
  

  /**
   * This should read the servant activator, but it cannot be transferred
   * this way as its operations cannot be remote. The operations cannot
   * be remote because one of the method parameters, POA, is required to be
   * always a local object (both by 1.5 API and 3.0.3 OMG).
   *
   * @throws MARSHAL, always.
   *
   * @specnote Same as Sun.
   */
  public static ServantActivator read(InputStream input)
  {
    MARSHAL m = new MARSHAL("Inappropriate");
    m.minor = Minor.Inappropriate;
    throw m;
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
  public static void write(OutputStream output, ServantActivator value)
  {
    MARSHAL m = new MARSHAL("Inappropriate");
    m.minor = Minor.Inappropriate;
    throw m;
  }
}