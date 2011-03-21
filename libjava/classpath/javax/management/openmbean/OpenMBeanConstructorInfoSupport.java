/* OpenMBeanConstructorInfoSupport.java -- Open typed info about an constructor.
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

package javax.management.openmbean;

import java.util.Arrays;

import javax.management.MBeanConstructorInfo;
import javax.management.MBeanParameterInfo;

/**
 * Describes a constructor for an open management bean.
 *
 * @author Andrew John Hughes (gnu_andrew@member.fsf.org)
 * @since 1.5
 */
public class OpenMBeanConstructorInfoSupport
  extends MBeanConstructorInfo
  implements OpenMBeanConstructorInfo
{

  /**
   * Compatible with JDK 1.5
   */
  private static final long serialVersionUID = -4400441579007477003L;

  /**
   * The hash code of this instance.
   */
  private transient Integer hashCode;

  /**
   * The <code>toString()</code> result of this instance.
   */
  private transient String string;

  /**
   * Constructs a @link{OpenMBeanConstructorInfo} with the specified
   * name, description and parameter information. A <code>null</code>
   * value for the parameter information is the same as passing in
   * an empty array.  Neither the name nor the description may be
   * null or equal to the empty string.  A copy of the parameter array
   * is taken, so later changes have no effect.
   *
   * @param name the name of the constructor.
   * @param desc a description of the constructor.
   * @param sig the signature of the constructor, as a series
   *            of {@link MBeanParameterInfo} objects, one for
   *            each parameter.
   * @throws IllegalArgumentException if the name or description is
   *                                  either <code>null</code>
   *                                  or the empty string.
   * @throws ArrayStoreException if the members of the signature array
   *                             are not assignable to
   *                             {@link javax.management.MBeanParameterInfo}
   */
  public OpenMBeanConstructorInfoSupport(String name, String desc,
                                         OpenMBeanParameterInfo[] sig)
  {
    super(name, desc, (MBeanParameterInfo[]) sig);
    if (name == null)
      throw new IllegalArgumentException("The name may not be null.");
    if (desc == null)
      throw new IllegalArgumentException("The description may not be null.");
    if (name.length() == 0)
      throw new IllegalArgumentException("The name may not be the empty string.");
    if (desc.length() == 0)
      throw new IllegalArgumentException("The description may not be the " +
                                         "empty string.");
  }

  /**
   * Compares this attribute with the supplied object.  This returns
   * true iff the object is an instance of {@link OpenMBeanConstructorInfo}
   * with an equal name and signature.
   *
   * @param obj the object to compare.
   * @return true if the object is a {@link OpenMBeanParameterInfo}
   *         instance,
   *         <code>name.equals(object.getName())</code>,
   *         and <code>signature.equals(object.getSignature())</code>.
   */
  public boolean equals(Object obj)
  {
    if (!(obj instanceof OpenMBeanConstructorInfo))
      return false;
    OpenMBeanConstructorInfo o = (OpenMBeanConstructorInfo) obj;
    return getName().equals(o.getName()) &&
      getSignature().equals(o.getSignature());
  }

  /**
   * <p>
   * Returns the hashcode of the constructor information as the sum of
   * the hashcodes of the name and signature (calculated by
   * <code>java.util.Arrays.asList(signature).hashCode()</code>).
   * </p>
   * <p>
   * As instances of this class are immutable, the return value
   * is computed just once for each instance and reused
   * throughout its life.
   * </p>
   *
   * @return the hashcode of the constructor information.
   */
  public int hashCode()
  {
    if (hashCode == null)
      hashCode = Integer.valueOf(getName().hashCode() +
                                 Arrays.asList(getSignature()).hashCode());
    return hashCode.intValue();
  }

  /**
   * <p>
   * Returns a textual representation of this instance.  This
   * is constructed using the class name
   * (<code>javax.management.openmbean.OpenMBeanConstructorInfo</code>)
   * along with the name and signature.
   * </p>
   * <p>
   * As instances of this class are immutable, the return value
   * is computed just once for each instance and reused
   * throughout its life.
   * </p>
   *
   * @return a @link{java.lang.String} instance representing
   *         the instance in textual form.
   */
  public String toString()
  {
    if (string == null)
      string = getClass().getName()
        + "[name=" + getName()
        + ",signature=" + Arrays.toString(getSignature())
        + "]";
    return string;
  }

}
