/* ClassHelper.java -- Utility methods to augment java.lang.Class
   Copyright (C) 1998, 2002 Free Software Foundation, Inc.

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
Free Software Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA
02111-1307 USA.

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


package gnu.java.lang;

import java.lang.reflect.Field;
import java.lang.reflect.Method;
import java.util.Arrays;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;
import java.util.Map;
import java.util.Set;

/**
 * ClassHelper has various methods that ought to have been in Class.
 *
 * @author John Keiser
 * @author Eric Blake <ebb9@email.byu.edu>
 */
public class ClassHelper
{
  /**
   * Strip the package part from the class name.
   *
   * @param clazz the class to get the truncated name from
   * @return the truncated class name
   */
  public static String getTruncatedClassName(Class clazz)
  {
    return getTruncatedName(clazz.getName());
  }

  /**
   * Strip the package part from the class name, or the class part from
   * the method or field name.
   *
   * @param name the name to truncate
   * @return the truncated name
   */
  public static String getTruncatedName(String name)
  {
    int lastInd = name.lastIndexOf('.');
    if (lastInd == -1)
      return name;
    return name.substring(lastInd + 1);
  }

  /** Cache of methods found in getAllMethods(). */
  private static Map allMethods = new HashMap();

  /**
   * Get all the methods, public, private and otherwise, from the class,
   * getting them from the most recent class to find them. This may not
   * be quite the correct approach, as this includes methods that are not
   * inherited or accessible from clazz, so beware.
   *
   * @param clazz the class to start at
   * @return all methods declared or inherited in clazz
   */
  public static Method[] getAllMethods(Class clazz)
  {
    Method[] retval = (Method[]) allMethods.get(clazz);
    if (retval == null)
      {
        Set methods = new HashSet();
        Class c = clazz;
        while (c != null)
          {
            Method[] currentMethods = c.getDeclaredMethods();
          loop:
            for (int i = 0; i < currentMethods.length; i++)
              {
                Method current = currentMethods[i];
                int size = methods.size();
                Iterator iter = methods.iterator();
                while (--size >= 0)
                  {
                    Method override = (Method) iter.next();
                    if (current.getName().equals(override.getName())
                        && Arrays.equals(current.getParameterTypes(),
                                         override.getParameterTypes())
                        && current.getReturnType() == override.getReturnType())
                      continue loop;
                  }
                methods.add(current);
              }
            c = c.getSuperclass();
          }
        retval = new Method[methods.size()];
        methods.toArray(retval);
        allMethods.put(clazz, retval);
      }
    return retval;
  }

  /** Cache of fields found in getAllFields(). */
  private static Map allFields = new HashMap();

  /**
   * Get all the fields, public, private and otherwise, from the class,
   * getting them from the most recent class to find them. This may not
   * be quite the correct approach, as this includes fields that are not
   * inherited or accessible from clazz, so beware.
   *
   * @param clazz the class to start at
   * @return all fields declared or inherited in clazz
   */
  public static Field[] getAllFields(Class clazz)
  {
    Field[] retval = (Field[]) allFields.get(clazz);
    if (retval == null)
      {
        Set fields = new HashSet();
        Class c = clazz;
        while (c != null)
          {
            Field[] currentFields = c.getDeclaredFields();
          loop:
            for (int i = 0; i < currentFields.length; i++)
              {
                Field current = currentFields[i];
                int size = fields.size();
                Iterator iter = fields.iterator();
                while (--size >= 0)
                  {
                    Field override = (Field) iter.next();
                    if (current.getName().equals(override.getName())
                        && current.getType() == override.getType())
                      continue loop;
                  }
                fields.add(current);
              }
            c = c.getSuperclass();
          }
        retval = new Field[fields.size()];
        fields.toArray(retval);
        allFields.put(clazz, retval);
      }
    return retval;
  }
}
