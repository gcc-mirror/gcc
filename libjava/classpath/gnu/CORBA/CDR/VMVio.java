/* VMVio.java -- Native operations, required by value IO.
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

/**
 * This is a temporary replacement for the native call that would allocate
 * objects without public constructors. The replacement only allocates
 * objects with public parameterless constructor and objects with public
 * constructor taking string (like some Throwables).
 *
 * @author Audrius Meskauskas (AudriusA@Bioinformatics.org)
 *
 * TODO FIXME replace by native call like in VMObjectInputStream.
 * Required modification of Classpath the build system.
 */


package gnu.CORBA.CDR;

import java.lang.reflect.Constructor;

public class VMVio
{
  /**
   * Allocates a new Object of type clazz but without running the default
   * constructor on it. It then calls the given constructor on it. The given
   * constructor method comes from the constr_clazz which is a super class of
   * the given clazz.
   */
  public static Object allocateObject(Class clazz, Class constr_clazz,
    Constructor constructor)
    throws InstantiationException
  {
    try
      {
        Constructor c = clazz.getConstructor(new Class[0]);
        c.setAccessible(true);
        return c.newInstance(new Object[0]);
      }
    catch (Exception ex)
      {
        try
          {
            Constructor c = clazz.getConstructor(new Class[] { String.class });
            return c.newInstance(new Object[] { "" });
          }
        catch (Exception ex2)
          {
            Constructor c[] = clazz.getConstructors();

            for (int i = 0; i < c.length; i++)
              {
                try
                  {
                    c[i].setAccessible(true);
                    Class[] args = c[i].getParameterTypes();
                    return c[i].newInstance(new Object[args.length]);
                  }
                catch (Exception ex3)
                  {
                    // Try another one.
                  }
              }
          }
        throw new InstantiationException(clazz.getName());
      }
  }
}
