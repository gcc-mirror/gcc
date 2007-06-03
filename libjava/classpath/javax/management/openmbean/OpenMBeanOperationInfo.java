/* OpenMBeanOperationInfo.java -- Open typed info about a operation.
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

import javax.management.MBeanParameterInfo;

/**
 * Describes a operation for an open management bean.
 * This interface includes those methods specified by {@link
 * javax.management.MBeanOperationInfo}, so implementations should
 * extend this class.  The {@link #getSignature()} method should
 * return an array containing instances of {@link OpenMBeanParameterInfo}.
 *
 * @author Andrew John Hughes (gnu_andrew@member.fsf.org)
 * @since 1.5
 */
public interface OpenMBeanOperationInfo
{

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
  boolean equals(Object obj);

  /**
   * Returns a description of this operation.
   *
   * @return a human-readable description.
   */
  String getDescription();

  /**
   * <p>
   * Returns the impact of performing this operation.
   * The value is equal to one of the following:
   * </p>
   * <ol>
   * <li>{@link javax.management.MBeanOperationInfo#INFO}
   * &mdash; the method just returns
   * information (akin to an accessor).</li>
   * <li>{@link javax.management.MBeanOperationInfo#ACTION}
   * the method just alters the state of the bean, without
   * returning a value (akin to a mutator).</li>
   * <li>{@link javax.management.MBeanOperationInfo#ACTION_INFO}
   * the method both makes state changes and returns a value.</li>
   * <li>{@link javax.management.MBeanOperationInfo#UNKNOWN}
   * the behaviour of the operation is unknown.</li>
   * </ol>
   *
   * @return the impact of performing the operation.
   */
  int getImpact();

  /**
   * Returns the name of this operation.
   *
   * @return the name of the operation.
   */
  String getName();

  /**
   * Returns the open type instance which represents the type of the
   * return value.
   *
   * @return the open type of the return value.
   */
  OpenType<?> getReturnOpenType();

  /**
   * Returns the return type of the operation, as the class
   * name.  This should be identical to
   * <code>getReturnOpenType.getClassName()</code>.
   *
   * @return the return type.
   */
  String getReturnType();

  /**
   * Returns the operation's signature, in the form of
   * information on each parameter.  Each parameter is
   * described by an instance of {@link OpenMBeanParameterInfo}.
   *
   * @return an array of {@link OpenMBeanParameterInfo} objects,
   *         describing the operation parameters.
   */
  MBeanParameterInfo[] getSignature();

  /**
   * Returns the hashcode of the operation information as the sum of
   * the hashcodes of the name, open return type, impact and signature
   * (calculated by
   * <code>java.util.Arrays.asList(signature).hashCode()</code>).
   *
   * @return the hashcode of the operation information.
   */
  int hashCode();

  /**
   * Returns a textual representation of this instance.  This
   * is constructed using the class name
   * (<code>javax.management.openmbean.OpenMBeanOperationInfo</code>)
   * along with the name, signature, open return type and impact.
   *
   * @return a @link{java.lang.String} instance representing
   *         the instance in textual form.
   */
  String toString();

}
