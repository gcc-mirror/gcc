/* OpenMBeanOperationInfoSupport.java -- Open typed info about an operation.
   Copyright (C) 2006, 2007 Free Software Foundation, Inc.

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

import javax.management.MBeanOperationInfo;
import javax.management.MBeanParameterInfo;

/**
 * Describes a operation for an open management bean.
 *
 * @author Andrew John Hughes (gnu_andrew@member.fsf.org)
 * @since 1.5
 */
public class OpenMBeanOperationInfoSupport
  extends MBeanOperationInfo
  implements OpenMBeanOperationInfo
{

  /**
   * Compatible with JDK 1.5
   */
  private static final long serialVersionUID = 4996859732565369366L;

  /**
   * The open type representing the return value.
   */
  private OpenType<?> returnOpenType;

  /**
   * The hash code of this instance.
   */
  private transient Integer hashCode;

  /**
   * The <code>toString()</code> result of this instance.
   */
  private transient String string;

  /**
   * Constructs a @link{OpenMBeanOperationInfo} with the specified name,
   * description, parameter information, open return type and impact. A
   * <code>null</code> value for the parameter information is the same
   * as passing in an empty array.  A copy of the parameter array is
   * taken, so later changes have no effect.  The name and the
   * description may not be equal to the empty string, and neither
   * the name, description nor the open return type may be
   * <code>null</code>.  The value of <code>impact</code> must be
   * one of the four valid values
   * ({@link javax.management.MBeanOperationInfo#INFO},
   * {@link javax.management.MBeanOperationInfo#ACTION},
   * {@link javax.management.MBeanOperationInfo#ACTION_INFO} and
   * {@link javax.management.MBeanOperationInfo#UNKNOWN}).
   *
   *
   * @param name the name of the constructor.
   * @param desc a description of the attribute.
   * @param sig the signature of the method, as a series
   *            of {@link MBeanParameterInfo} objects, one for
   *            each parameter.
   * @param type the open return type of the method.
   * @param impact the impact of performing the operation.
   * @throws IllegalArgumentException if the name, description or
   *                                  open return type is <code>null</code>,
   *                                  the name or description are equal to
   *                                  the empty string, or the impact factor
   *                                  is not one of the values enumerated
   *                                  above.
   * @throws ArrayStoreException if the members of the signature array
   *                             are not assignable to
   *                             {@link javax.management.MBeanParameterInfo}
   */
  public OpenMBeanOperationInfoSupport(String name, String desc,
                                       OpenMBeanParameterInfo[] sig,
                                       OpenType<?> type, int impact)
  {
    super(name, desc, (MBeanParameterInfo[]) sig,
          type == null ? null : type.getClassName(), impact);
    if (name == null)
      throw new IllegalArgumentException("The name may not be null.");
    if (desc == null)
      throw new IllegalArgumentException("The description may not be null.");
    if (type == null)
      throw new IllegalArgumentException("The type may not be null.");
    if (name.length() == 0)
      throw new IllegalArgumentException("The name may not be the empty string.");
    if (desc.length() == 0)
      throw new IllegalArgumentException("The description may not be the " +
                                         "empty string.");
    if (impact != ACTION && impact != INFO &&
        impact != ACTION_INFO && impact != UNKNOWN)
      throw new IllegalArgumentException("The impact factor is an invalid value.");
    returnOpenType = type;
  }

  /**
   * Compares this attribute with the supplied object.  This returns
   * true iff the object is an instance of {@link OpenMBeanOperationInfo}
   * with an equal name, signature, open return type and impact.
   *
   * @param obj the object to compare.
   * @return true if the object is a {@link OpenMBeanParameterInfo}
   *         instance,
   *         <code>name.equals(object.getName())</code>,
   *         <code>signature.equals(object.getSignature())</code>,
   *         <code>returnOpenType.equals(object.getReturnOpenType())</code>,
   *         and <code>impact == object.getImpact()</code>.
   */
  public boolean equals(Object obj)
  {
    if (!(obj instanceof OpenMBeanOperationInfo))
      return false;
    OpenMBeanOperationInfo o = (OpenMBeanOperationInfo) obj;
    return getName().equals(o.getName()) &&
      getSignature().equals(o.getSignature()) &&
      returnOpenType.equals(o.getReturnOpenType()) &&
      getImpact() == o.getImpact();
  }

  /**
   * Returns the open type instance which represents the type of the
   * return value.
   *
   * @return the open type of the return value.
   */
  public OpenType<?> getReturnOpenType()
  {
    return returnOpenType;
  }

  /**
   * <p>
   * Returns the hashcode of the operation information as the sum of
   * the hashcodes of the name, open return type, impact and signature
   * (calculated by
   * <code>java.util.Arrays.asList(signature).hashCode()</code>).
   * </p>
   * <p>
   * As instances of this class are immutable, the return value
   * is computed just once for each instance and reused
   * throughout its life.
   * </p>
   *
   * @return the hashcode of the operation information.
   */
  public int hashCode()
  {
    if (hashCode == null)
      hashCode = Integer.valueOf(getName().hashCode() +
                                 returnOpenType.hashCode() +
                                 Integer.valueOf(getImpact()).hashCode() +
                                 Arrays.asList(getSignature()).hashCode());
    return hashCode.intValue();
  }

  /**
   * <p>
   * Returns a textual representation of this instance.  This
   * is constructed using the class name
   * (<code>javax.management.openmbean.OpenMBeanOperationInfo</code>)
   * along with the name, signature, open return type and impact.
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
      {
        String impactString;
        switch (getImpact())
          {
          case INFO:
            impactString = "INFO";
            break;
          case ACTION:
            impactString = "ACTION";
            break;
          case ACTION_INFO:
            impactString = "ACTION_INFO";
            break;
          case UNKNOWN:
            impactString = "UNKNOWN";
            break;
          default:
            impactString = "ERRONEOUS VALUE";
          }
        string = getClass().getName()
          + "[name=" + getName()
          + ",signature=" + Arrays.toString(getSignature())
          + ",returnOpenType=" + returnOpenType
          + ",impact=" + impactString
          + "]";
      }
    return string;
  }

}
