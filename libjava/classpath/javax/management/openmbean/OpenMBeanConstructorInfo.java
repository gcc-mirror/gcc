/* OpenMBeanConstructorInfo.java -- Open typed info about a constructor.
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
 * Describes a constructor for an open management bean.
 * This interface includes those methods specified by {@link
 * javax.management.MBeanConstructorInfo}, so implementations should
 * extend this class.  The {@link #getSignature()} method should
 * return an array containing instances of {@link OpenMBeanParameterInfo}.
 *
 * @author Andrew John Hughes (gnu_andrew@member.fsf.org)
 * @since 1.5
 */
public interface OpenMBeanConstructorInfo
{

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
  boolean equals(Object obj);

  /**
   * Returns a description of this constructor.
   *
   * @return a human-readable description.
   */
  String getDescription();

  /**
   * Returns the name of this constructor.
   *
   * @return the name of the constructor.
   */
  String getName();

  /**
   * Returns the constructor's signature, in the form of
   * information on each parameter.  Each parameter is
   * described by an instance of {@link OpenMBeanParameterInfo}.
   *
   * @return an array of {@link OpenMBeanParameterInfo} objects,
   *         describing the constructor parameters.
   */
  MBeanParameterInfo[] getSignature();

  /**
   * Returns the hashcode of the constructor information as the sum of
   * the hashcodes of the name and signature (calculated by
   * <code>java.util.Arrays.asList(signature).hashCode()</code>).
   *
   * @return the hashcode of the constructor information.
   */
  int hashCode();

  /**
   * Returns a textual representation of this instance.  This
   * is constructed using the class name
   * (<code>javax.management.openmbean.OpenMBeanConstructorInfo</code>)
   * along with the name and signature.
   *
   * @return a @link{java.lang.String} instance representing
   *         the instance in textual form.
   */
  String toString();

}
