/* IDLTypeHelper.java --
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


package gnu.CORBA.CDR;

import gnu.CORBA.Minor;

import org.omg.CORBA.MARSHAL;
import org.omg.CORBA.portable.BoxedValueHelper;
import org.omg.CORBA.portable.InputStream;
import org.omg.CORBA.portable.OutputStream;

import java.io.Serializable;
import java.lang.reflect.Method;
import java.lang.reflect.Modifier;

/**
 * Handles case when the CORBA IDL type with the known helper is wrapped into
 * Value type.
 *
 * @author Audrius Meskauskas, Lithuania (AudriusA@Bioinformatics.org)
 */
public class IDLTypeHelper
  implements BoxedValueHelper
{
  /**
   * A helper class.
   */
  protected Class helper;

  /**
   * Argument values for Helper.id().
   */
  static final Object[] ARGS_ID_V = new Object[0];

  /**
   * Argument types for Helper.id()).
   */
  static final Class[] ARGS_ID = new Class[0];

  /**
   * Argument types for Helper.read.
   */
  static final Class[] ARGS_READ = new Class[] { org.omg.CORBA.portable.InputStream.class };

  /**
   * Create an IDLTypeHelper that works via given helper class.
   */
  public IDLTypeHelper(Class a_helperClass)
  {
    helper = a_helperClass;
  }

  /**
   * Get the Id, returned by this helper (use reflection).
   */
  public String get_id()
  {
    try
      {
        Method m = helper.getMethod("id", ARGS_ID);
        return (String) m.invoke(null, ARGS_ID_V);
      }
    catch (Exception ex)
      {
        MARSHAL m = new MARSHAL(msg() + " id()");
        m.minor = Minor.Boxed;
        m.initCause(ex);
        throw m;
      }
  }

  /**
   * Read an instance from the stream.
   */
  public Serializable read_value(InputStream input)
  {
    try
      {
        Method m = helper.getMethod("read", ARGS_READ);
        return (Serializable) m.invoke(null, new Object[] { input });
      }
    catch (Exception ex)
      {
        MARSHAL m = new MARSHAL(msg() + " read(..)");
        m.minor = Minor.Boxed;
        m.initCause(ex);
        throw m;
      }
  }

  /**
   * Write the instance to the stream.
   */
  public void write_value(OutputStream output, Serializable value)
  {
    try
      {
        Method[] m = helper.getMethods();

        for (int i = 0; i < m.length; i++)
          {
            if (m[i].getName().equals("write")
              && ((m[i].getModifiers() & Modifier.STATIC) != 0))
              {
                Class[] p = m[i].getParameterTypes();

                if (p.length == 2 && OutputStream.class.isAssignableFrom(p[0])
                  && p[1].isAssignableFrom(value.getClass()))
                  {
                    m[i].invoke(null, new Object[] { output, value });
                    return;
                  }
              }
          }
      }
    catch (Exception ex)
      {
        MARSHAL m = new MARSHAL(msg() + " write(..)");
        m.minor = Minor.Boxed;
        m.initCause(ex);
        throw m;
      }
  }

  /**
   * Create the start of message for exceptions.
   */
  String msg()
  {
    return "Failed calling " + helper.getName() + " method: ";
  }

}
