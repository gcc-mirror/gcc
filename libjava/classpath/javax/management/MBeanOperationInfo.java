/* MBeanOperationInfo.java -- Information about a bean's operations.
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

import java.lang.reflect.Method;
import java.lang.reflect.Type;

import java.util.Arrays;

/**
 * Describes the operations of a management bean.
 * The information in this class is immutable as standard.
 * Of course, subclasses may change this, but this
 * behaviour is not recommended.
 *
 * @author Andrew John Hughes (gnu_andrew@member.fsf.org)
 * @since 1.5
 */
public class MBeanOperationInfo
  extends MBeanFeatureInfo
  implements Cloneable
{

  /**
   * Compatible with JDK 1.5
   */
  private static final long serialVersionUID = -6178860474881375330L;

  /**
   * Used to signify that the operation merely provides information
   * (akin to an accessor).
   */
  public static final int INFO = 0;

  /**
   * Used to signify that the operation makes some change to the
   * state of the bean (akin to a mutator).
   */
  public static final int ACTION = 1;

  /**
   * Used to signify that the operation makes some state change
   * to the bean and also returns information.
   */
  public static final int ACTION_INFO = 2;

  /**
   * Used to signify that the behaviour of the operation is
   * unknown.
   */
  public static final int UNKNOWN = 3;

  /**
   * The return type of the method, in the form of its class name.
   */
  private String type;

  /**
   * The signature of the constructor i.e. the argument types.
   */
  private MBeanParameterInfo[] signature;

  /**
   * The impact of the method, as one of {@link #INFO}, {@link #ACTION},
   * {@link #ACTION_INFO} and {@link #UNKNOWN}.
   */
  private int impact;

  /**
   * Constructs a @link{MBeanOperationInfo} with the specified
   * description using the given method.  Each parameter is
   * described merely by its type; the name and description are
   * <code>null</code>.  The return type and impact of the
   * method are determined from the {@link Method} instance.
   *
   * @param desc a description of the attribute.
   * @param method the method.
   */
  public MBeanOperationInfo(String desc, Method method)
  {
    super(method.getName(), desc);
    Type[] paramTypes = method.getGenericParameterTypes();
    signature = new MBeanParameterInfo[paramTypes.length];
    for (int a = 0; a < paramTypes.length; ++a)
      {
        Type t = paramTypes[a];
        if (t instanceof Class)
          signature[a] = new MBeanParameterInfo(null,
                                                ((Class<?>) t).getName(),
                                                 null);
        else
          signature[a] = new MBeanParameterInfo(null, t.toString(), null);
      }
    Type retType = method.getGenericReturnType();
    if (retType instanceof Class)
      type = ((Class<?>) retType).getName();
    else
      type = retType.toString();
    if (method.getReturnType() == Void.TYPE)
      {
        if (paramTypes.length == 0)
          impact = UNKNOWN;
        else
          impact = ACTION;
      }
    else
      {
        if (paramTypes.length == 0)
          impact = INFO;
        else
          impact = ACTION_INFO;
      }
  }

  /**
   * Constructs a @link{MBeanOperationInfo} with the specified name,
   * description, parameter information, return type and impact. A
   * <code>null</code> value for the parameter information is the same
   * as passing in an empty array.  A copy of the parameter array is
   * taken, so later changes have no effect.
   *
   * @param name the name of the constructor.
   * @param desc a description of the attribute.
   * @param sig the signature of the method, as a series
   *            of {@link MBeanParameterInfo} objects, one for
   *            each parameter.
   * @param type the return type of the method, as the class name.
   * @param impact the impact of performing the operation.
   */
  public MBeanOperationInfo(String name, String desc,
                            MBeanParameterInfo[] sig, String type,
                            int impact)
  {
    super(name, desc);
    if (sig == null)
      signature = new MBeanParameterInfo[0];
    else
      {
        signature = new MBeanParameterInfo[sig.length];
        System.arraycopy(sig, 0, signature, 0, sig.length);
      }
    this.type = type;
    this.impact = impact;
  }

  /**
   * Returns a clone of this instance.  The clone is created
   * using just the method provided by {@link java.lang.Object}.
   * Thus, the clone is just a shallow clone as returned by
   * that method, and does not contain any deeper cloning based
   * on the subject of this class.
   *
   * @return a clone of this instance.
   * @see java.lang.Cloneable
   */
  public Object clone()
  {
    try
      {
        return super.clone();
      }
    catch (CloneNotSupportedException e)
      {
        /* This shouldn't happen; we implement Cloneable */
        throw new IllegalStateException("clone() called on " +
                                        "non-cloneable object.");
      }
  }

  /**
   * Compares this feature with the supplied object.  This returns
   * true iff the object is an instance of {@link
   * MBeanConstructorInfo}, {@link Object#equals()} returns true for a
   * comparison of both the name and description of this notification
   * with that of the specified object (performed by the superclass),
   * the return type and impact are equal and the two signature arrays
   * contain the same elements in the same order (but one may be
   * longer than the other).
   *
   * @param obj the object to compare.
   * @return true if the object is a {@link MBeanOperationInfo}
   *         instance,
   *         <code>name.equals(object.getName())</code>,
   *         <code>description.equals(object.getDescription())</code>,
   *         <code>type.equals(object.getReturnType())</code>,
   *         <code>impact == object.getImpact()</code>,
   *         and the corresponding elements of the signature arrays are
   *         equal.
   */
  public boolean equals(Object obj)
  {
    if (!(obj instanceof MBeanOperationInfo))
      return false;
    if (!(super.equals(obj)))
      return false;
    MBeanOperationInfo o = (MBeanOperationInfo) obj;
    MBeanParameterInfo[] sig = o.getSignature();
    for (int a = 0; a < signature.length; ++a)
      {
        if (a == sig.length)
          return true;
        if (!(signature[a].equals(sig[a])))
          return false;
      }
    return (type.equals(o.getReturnType()) &&
            impact == o.getImpact());
  }

  /**
   * <p>
   * Returns the impact of performing this operation.
   * The value is equal to one of the following:
   * </p>
   * <ol>
   * <li>{@link #INFO} &mdash; the method just returns
   * information (akin to an accessor).</li>
   * <li>{@link #ACTION} &mdash; the method just alters
   * the state of the bean, without returning a value
   * (akin to a mutator).</li>
   * <li>{@link #ACTION_INFO} &mdash; the method both makes
   * state changes and returns a value.</li>
   * <li>{@link #UNKNOWN} &mdash; the behaviour of the operation
   * is unknown.</li>
   * </ol>
   *
   * @return the impact of performing the operation.
   */
  public int getImpact()
  {
    return impact;
  }

  /**
   * Returns the return type of the operation, as the class
   * name.
   *
   * @return the return type.
   */
  public String getReturnType()
  {
    return type;
  }

  /**
   * Returns the operation's signature, in the form of
   * information on each parameter.  Each parameter is
   * described by an instance of {@link MBeanParameterInfo}.
   * The returned array is a shallow copy of the array used
   * by this instance, so changing which elements are stored
   * in the array won't affect the array used by this, but
   * changing the actual elements will affect the ones used
   * here.
   *
   * @return an array of {@link MBeanParameterInfo} objects,
   *         describing the operation parameters.
   */
  public MBeanParameterInfo[] getSignature()
  {
    return (MBeanParameterInfo[]) signature.clone();
  }

  /**
   * Returns the hashcode of the operation information as the sum of
   * the hashcode of the superclass, the parameter array, the return
   * type and the impact factor.
   *
   * @return the hashcode of the operation information.
   */
  public int hashCode()
  {
    return super.hashCode() + Arrays.hashCode(signature)
      + type.hashCode() + Integer.valueOf(impact).hashCode();
  }

  /**
   * <p>
   * Returns a textual representation of this instance.  This
   * is constructed using the class name
   * (<code>javax.management.MBeanOperationInfo</code>),
   * the name, description, return type and impact of the
   * operation and the contents of the array of parameters.
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
        switch (impact)
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
        super.toString();
        string = string.substring(0, string.length() - 1)
          + ",returnType=" + type
          + ",impact=" + impactString
          + ",signature=" + Arrays.toString(signature)
          + "]";
      }
    return string;
  }

}
