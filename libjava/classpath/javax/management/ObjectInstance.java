/* ObjectInstance.java -- Represent the bean instance used by a server.
   Copyright (C) 2006 Free Software Foundation, Inc.

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

package javax.management;

import java.io.Serializable;

/**
 * A simple class used to link a bean instance to its class name.
 * If the bean is a {@link DynamicMBean}, the class name may be
 * obtained using {@link MBeanInfo#getClassName()}.
 *
 * @author Andrew John Hughes (gnu_andrew@member.fsf.org)
 * @since 1.5
 */
public class ObjectInstance
  implements Serializable
{

  /**
   * Compatible with JDK 1.5
   */
  private static final long serialVersionUID = -4099952623687795850L;

  /**
   * The name of the bean.
   */
  private ObjectName name;

  /**
   * The class name of the bean.
   */
  private String className;

  /**
   * Constructs a new {@link ObjectInstance} for the specified bean
   * with the supplied class name.  The class name should include
   * the full package name.
   *
   * @param name the name of the bean.
   * @param className the class name of the bean.
   */
  public ObjectInstance(ObjectName name, String className)
  {
    this.name = name;
    this.className = className;
  }

  /**
   * Constructs a new {@link ObjectInstance} for the specified bean
   * with the supplied class name.  The class name should include
   * the full package name.
   *
   * @param name the name of the bean.
   * @param className the class name of the bean.
   * @throws MalformedObjectNameException if the name of the bean
   *                                      does not match the syntax
   *                                      of an {@link ObjectName}.
   */
  public ObjectInstance(String name, String className)
    throws MalformedObjectNameException
  {
    this(new ObjectName(name), className);
  }

  /**
   * Returns true if the supplied object is also an {@link ObjectInstance}
   * with the same bean and class name.
   *
   * @param obj the object to compare.
   * @return true if the the supplied object is equal to <code>this</code>.
   */
  public boolean equals(Object obj)
  {
    if (!(obj instanceof ObjectInstance))
      return false;
    ObjectInstance i = (ObjectInstance) obj;
    return (i.getClassName().equals(className) &&
            i.getObjectName().equals(name));
  }

  /**
   * Returns the class name of the bean.
   *
   * @return the class name.
   */
  public String getClassName()
  {
    return className;
  }

  /**
   * Returns the name of the bean.
   *
   * @return the name of the bean.
   */
  public ObjectName getObjectName()
  {
    return name;
  }

  /**
   * Returns a hash code for this instance.  This is calculated as
   * the sum of the hashcodes for the bean's name and the class name.
   *
   * @return the hash code of this instance.
   */
  public int hashCode()
  {
    return name.hashCode() + className.hashCode();
  }

}
