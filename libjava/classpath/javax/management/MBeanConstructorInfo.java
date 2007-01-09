/* MBeanConstructorInfo.java -- Information about a bean's constructor.
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

import java.lang.reflect.Constructor;
import java.lang.reflect.Type;

import java.util.Arrays;

/**
 * Describes the constructors of a management bean.
 * The information in this class is immutable as standard.
 * Of course, subclasses may change this, but this
 * behaviour is not recommended.
 *
 * @author Andrew John Hughes (gnu_andrew@member.fsf.org)
 * @since 1.5
 */
public class MBeanConstructorInfo
  extends MBeanFeatureInfo
  implements Cloneable
{

  /**
   * Compatible with JDK 1.5
   */
  private static final long serialVersionUID = 4433990064191844427L;

  /**
   * The signature of the constructor i.e. the argument types.
   */
  private MBeanParameterInfo[] signature;

  /**
   * Constructs a @link{MBeanConstructorInfo} with the specified
   * description using the given constructor.  Each parameter is
   * described merely by its type; the name and description are
   * <code>null</code>.
   *
   * @param desc a description of the attribute.
   * @param cons the constructor.
   */
  public MBeanConstructorInfo(String desc, Constructor cons)
  {
    super(cons.getName(), desc);
    Type[] paramTypes = cons.getGenericParameterTypes();
    signature = new MBeanParameterInfo[paramTypes.length];
    for (int a = 0; a < paramTypes.length; ++a)
      {
	Type t = paramTypes[a];
	if (t instanceof Class)
	  signature[a] = new MBeanParameterInfo(null,
						((Class) t).getName(),
						null);
	else
	  signature[a] = new MBeanParameterInfo(null, t.toString(), null);
      }
  }

  /**
   * Constructs a @link{MBeanConstructorInfo} with the specified
   * name, description and parameter information. A <code>null</code>
   * value for the parameter information is the same as passing in
   * an empty array.  A copy of the parameter array is taken, so
   * later changes have no effect.
   *
   * @param name the name of the constructor.
   * @param desc a description of the constructor.
   * @param sig the signature of the constructor, as a series
   *            of {@link MBeanParameterInfo} objects, one for
   *            each parameter.
   */
  public MBeanConstructorInfo(String name, String desc,
			      MBeanParameterInfo[] sig)
  {
    super(name, desc);
    if (sig == null)
      signature = new MBeanParameterInfo[0];
    else
      {
	signature = new MBeanParameterInfo[sig.length];
	System.arraycopy(sig, 0, signature, 0, sig.length);
      }
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
   * and the two signature arrays contain the same elements in the
   * same order (but one may be longer than the other).
   *
   * @param obj the object to compare.
   * @return true if the object is a {@link MBeanConstructorInfo}
   *         instance, 
   *         <code>name.equals(object.getName())</code>,
   *         <code>description.equals(object.getDescription())</code>
   *         and the corresponding elements of the signature arrays are
   *         equal.
   */
  public boolean equals(Object obj)
  {
    if (!(obj instanceof MBeanConstructorInfo))
      return false;
    if (!(super.equals(obj)))
      return false;
    MBeanConstructorInfo o = (MBeanConstructorInfo) obj;
    MBeanParameterInfo[] sig = o.getSignature();
    for (int a = 0; a < signature.length; ++a)
      {
	if (a == sig.length)
	  return true;
	if (!(signature[a].equals(sig[a])))
	  return false;
      }
    return true;
  }
  
  /**
   * Returns the constructor's signature, in the form of
   * information on each parameter.  Each parameter is
   * described by an instance of {@link MBeanParameterInfo}.
   * The returned array is a shallow copy of the array used
   * by this instance, so changing which elements are stored
   * in the array won't affect the array used by this, but
   * changing the actual elements will affect the ones used
   * here.
   *
   * @return an array of {@link MBeanParameterInfo} objects,
   *         describing the constructor parameters.
   */
  public MBeanParameterInfo[] getSignature()
  {
    return (MBeanParameterInfo[]) signature.clone();
  }

  /**
   * Returns the hashcode of the constructor information as the sum
   * of the hashcode of the superclass and the hashcode of the parameter
   * array.
   *
   * @return the hashcode of the constructor information.
   */
  public int hashCode()
  {
    return super.hashCode() + Arrays.hashCode(signature);
  }

  /**
   * <p>
   * Returns a textual representation of this instance.  This
   * is constructed using the class name
   * (<code>javax.management.MBeanConstructorInfo</code>),
   * the name and description of the constructor and the 
   * contents of the array of parameters.
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
	super.toString();
	string = string.substring(0, string.length() - 1) 
	  + ",signature=" + Arrays.toString(signature)
	  + "]";
      }
    return string;
  }

}
