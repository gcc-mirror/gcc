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


package org.omg.PortableInterceptor;

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
 * The helper operations for the CORBA object {@link Current}.
 *
 * @author Audrius Meskauskas, Lithuania (AudriusA@Bioinformatics.org)
 */
public abstract class CurrentHelper
{
  /**
   * Get the type code of the {@link Current}.
   */
  public static TypeCode type()
  {
    return OrbRestricted.Singleton.create_interface_tc(id(), "Current");
  }

  /**
   * Insert the Current into the given Any.
   *
   * @param any the Any to insert into.
   * @param that the Current to insert.
   */
  public static void insert(Any any, Current that)
  {
    any.insert_Object(that);
  }

  /**
   * Extract the Current from given Any.
   *
   * @throws BAD_OPERATION if the passed Any does not contain Current.
   */
  public static Current extract(Any any)
  {
    return narrow(any.extract_Object());
  }

  /**
   * Get the Current repository id.
   *
   * @return "org.omg.PortableInterceptor.CurrentOperations", always.
   */
  public static String id()
  {
    return "IDL:omg.org/PortableInterceptor/Current:1.0";
  }

  /**
   * Cast the passed object into the Current.
   *
   * @param obj the object to narrow.
   * @return narrowed instance.
   * @throws BAD_PARAM if the passed object is not a Current.
   */
  public static Current narrow(org.omg.CORBA.Object obj)
  {
    if (obj == null)
      {
        return null;
      }
    else if (obj instanceof Current)
      {
        return (Current) obj;
      }
    else
      {
        throw new BAD_PARAM("Not a Current");
      }
  }
  
  /**
   * Narrow the given object to the Current. For the objects that are
   * always local, this operation does not differ from the ordinary
   * {@link #narrow} (ClassCastException will be thrown if narrowing something
   * different). See also OMG issue 4158.
   * 
   * @param obj the object to cast.
   * 
   * @return the casted Current.
   * 
   * @since 1.5 
   */
  public static Current unchecked_narrow(org.omg.CORBA.Object obj)
  {
    return narrow(obj);
  }    

  /**
   * Not supported for compatibility reasons.
   *
   * @specnote Not supported by Sun at least till jdk 1.5 inclusive.
   *
   * @throws MARSHAL always.
   */
  public static Current read(InputStream input)
  {
    MARSHAL m = new MARSHAL("Inappropriate");
    m.minor = Minor.Inappropriate;
    throw m;
  }

  /**
   * Not supported for compatibility reasons.
   *
   * @specnote Not supported by Sun at least till jdk 1.5 inclusive.
   *
   * @throws MARSHAL always.
   */
  public static void write(OutputStream output, Current value)
  {
    MARSHAL m = new MARSHAL("Inappropriate");
    m.minor = Minor.Inappropriate;
    throw m;
  }
}