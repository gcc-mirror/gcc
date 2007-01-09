/* IORInterceptor_3_0Helper.java --
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
import org.omg.CORBA.TypeCode;
import org.omg.CORBA.portable.Delegate;
import org.omg.CORBA.portable.InputStream;
import org.omg.CORBA.portable.ObjectImpl;
import org.omg.CORBA.portable.OutputStream;

/**
 * The helper operations for the CORBA object {@link IORInterceptor_3_0}.
 * 
 * @since 1.5 
 * 
 * @author Audrius Meskauskas, Lithuania (AudriusA@Bioinformatics.org)
 */
public abstract class IORInterceptor_3_0Helper
{
  /**
   * Get the type code of the {@link IORInterceptor_3_0}.
   */
  public static TypeCode type()
  {
    return OrbRestricted.Singleton.create_interface_tc(id(), 
      "IORInterceptor_3_0");
  }

  /**
   * Insert the IORInterceptor_3_0 into the given Any.
   * 
   * @param any the Any to insert into.
   * @param that the IORInterceptor_3_0 to insert.
   */
  public static void insert(Any any, IORInterceptor_3_0 that)
  {
    any.insert_Streamable(new IORInterceptor_3_0Holder(that));
  }

  /**
   * Extract the IORInterceptor_3_0 from given Any.
   * 
   * @throws BAD_OPERATION if the passed Any does not contain
   * IORInterceptor_3_0.
   */
  public static IORInterceptor_3_0 extract(Any any)
  {
    try
      {
        IORInterceptor_3_0Holder holder = (IORInterceptor_3_0Holder) 
          any.extract_Streamable();
        return holder.value;
      }
    catch (ClassCastException cex)
      {
        BAD_OPERATION bad = new BAD_OPERATION("IORInterceptor_3_0 expected");
        bad.minor = Minor.Any;
        bad.initCause(cex);
        throw bad;
      }
  }

  /**
   * Get the IORInterceptor_3_0 repository id.
   * 
   * @return "IDL:omg.org/PortableInterceptor/IORInterceptor_3_0:1.0", always.
   */
  public static String id()
  {
    return "IDL:omg.org/PortableInterceptor/IORInterceptor_3_0:1.0";
  }

  /**
   * Narrow the passed object into the IORInterceptor_3_0. If the object has a
   * different java type, create an instance of the _IORInterceptor_3_0Stub,
   * using the same delegate, as for the passed parameter. Hence, unlike java
   * type cast, this method may return a different object, than has been passed.
   * 
   * @param obj the object to narrow.
   * @return narrowed instance.
   * @throws BAD_PARAM if the passed object is not a IORInterceptor_3_0.
   */
  public static IORInterceptor_3_0 narrow(org.omg.CORBA.Object obj)
  {
    if (obj == null)
      return null;
    else if (obj instanceof IORInterceptor_3_0)
      return (IORInterceptor_3_0) obj;
    else if (!obj._is_a(id()))
      throw new BAD_PARAM("Not a IORInterceptor_3_0");
    else
      {
        Delegate delegate = ((ObjectImpl) obj)._get_delegate();
        return new _IORInterceptor_3_0Stub(delegate);
      }
  }
  
  /**
   * Narrow the passed object into the IORInterceptor_3_0. No type-checking is
   * performed to verify that the object actually supports the requested type.
   * The {@link BAD_OPERATION} will be thrown if unsupported operations are
   * invoked on the new returned reference, but no failure is expected at the
   * time of the unchecked_narrow. For instance, the narrowing of the 
   * remote instance of the {@link IORInterceptor} will work as long as only the
   * methods, inherited from this parent, are invoked.
   * 
   * 
   * @param obj the object to narrow.
   * @return narrowed instance.
   * @throws BAD_PARAM if the passed object is not a IORInterceptor_3_0.
   */
  public static IORInterceptor_3_0 unchecked_narrow(org.omg.CORBA.Object obj)
  {
    if (obj == null)
      return null;
    else if (obj instanceof IORInterceptor_3_0)
      return (IORInterceptor_3_0) obj;
    else
      {
        Delegate delegate = ((ObjectImpl) obj)._get_delegate();
        return new _IORInterceptor_3_0Stub(delegate);
      }
  }
  

  /**
   * Read the IORInterceptor_3_0 from the CDR intput stream (IOR profile
   * expected).
   * 
   * @param input a org.omg.CORBA.portable stream to read from.
   */
  public static IORInterceptor_3_0 read(InputStream input)
  {
    return unchecked_narrow(input.read_Object());
  }

  /**
   * Write the IORInterceptor_3_0 to the CDR output stream (as IOR profile).
   * 
   * @param output a org.omg.CORBA.portable stream stream to write into.
   * @param value a value to write.
   */
  public static void write(OutputStream output, IORInterceptor_3_0 value)
  {
    output.write_Object(value);
  }
}