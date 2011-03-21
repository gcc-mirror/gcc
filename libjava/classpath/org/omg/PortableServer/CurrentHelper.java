/* CurrentHelper.java --
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

import gnu.CORBA.OrbRestricted;

import org.omg.CORBA.Any;
import org.omg.CORBA.BAD_OPERATION;
import org.omg.CORBA.BAD_PARAM;
import org.omg.CORBA.NO_IMPLEMENT;
import org.omg.CORBA.TypeCode;
import org.omg.CORBA.portable.InputStream;
import org.omg.CORBA.portable.OutputStream;

/**
* The helper operations for the
* CORBA object {@link Current}.
*
* @author Audrius Meskauskas, Lithuania (AudriusA@Bioinformatics.org)
*/
public abstract class CurrentHelper
{
  /**
   * Get the type code of the POA Current.
   *
   * @return a type code of the object with POA Current id, named "Current".
   */
  public static TypeCode type()
  {
    return OrbRestricted.Singleton.create_interface_tc(id(), "Current");
  }

  /**
   * Insert the POA Current into the given Any.
   *
   * @param any the Any to insert into.
   * @param that the POA Current to insert.
   */
  public static void insert(Any any, Current that)
  {
    any.insert_Object(that);
  }

  /**
   * Extract the POA Current from given Any.
   *
   * @throws BAD_OPERATION if the passed Any does not contain a POA Current.
   */
  public static Current extract(Any any)
  {
    return narrow(any.extract_Object());
  }

  /**
   * Get the POA Current repository id.
   *
   * @return "IDL:omg.org/PortableServer/Current:2.3", always.
   */
  public static String id()
  {
    return "IDL:omg.org/PortableServer/Current:2.3";
  }

  /**
   * Cast the passed object into the POA Current.
   *
   * @param obj the object to narrow.
   * @return narrowed instance.
   * @throws BAD_PARAM if the passed object is not a Current.
   */
  public static Current narrow(org.omg.CORBA.Object obj)
  {
    try
      {
        return (Current) obj;
      }
    catch (ClassCastException ex)
      {
        BAD_PARAM bad = new BAD_PARAM("Not a POA Current");
        bad.initCause(ex);
        throw bad;
      }
  }

  /**
   * Not supported for compatibility reasons.
   *
   * @specnote Not supported by Sun at least till jdk 1.4 inclusive.
   *
   * @throws NO_IMPLEMENT always.
   */
  public static Current read(InputStream input)
  {
    throw new NO_IMPLEMENT();
  }

  /**
   * Not supported for compatibility reasons.
   *
   * @specnote Not supported by Sun at least till jdk 1.4 inclusive.
   *
   * @throws NO_IMPLEMENT always.
   */
  public static void write(OutputStream output, Current value)
  {
    throw new NO_IMPLEMENT();
  }
}
