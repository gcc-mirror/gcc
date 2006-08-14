/* MBeanParameterInfo.java -- Information about an operation's parameters.
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

/**
 * Describes the parameters of a constructor or operation associated
 * with a management bean.  The information in this class is immutable
 * as standard.  Of course, subclasses may change this, but this
 * behaviour is not recommended.
 *
 * @author Andrew John Hughes (gnu_andrew@member.fsf.org)
 * @since 1.5
 */
public class MBeanParameterInfo
  extends MBeanFeatureInfo
  implements Cloneable
{

  /**
   * Compatible with JDK 1.5
   */
  private static final long serialVersionUID = 7432616882776782338L;

  /**
   * The type of the parameter, represented by the class name.
   */
  private String type;

  /**
   * Constructs a new {@link MBeanParameterInfo} using the specified
   * name, description and type.  
   *
   * @param name the name of the attribute.
   * @param type the type of the attribute, in the form of its class name.
   * @param desc a description of the attribute.
   */
  public MBeanParameterInfo(String name, String type, String desc)
  {
    super(name, desc);
    this.type = type;
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
   * true iff the object is an instance of {@link MBeanParameterInfo},
   * {@link Object#equals()} returns true for a comparison of both the
   * name and description of this parameter with that of the specified
   * object (performed by the superclass), and the type of the two
   * instances is equal.
   *
   * @param obj the object to compare.
   * @return true if the object is a {@link MBeanParameterInfo}
   *         instance, 
   *         <code>name.equals(object.getName())</code>,
   *         <code>description.equals(object.getDescription())</code>,
   *         and <code>type.equals(object.getType())</code>.
   */
  public boolean equals(Object obj)
  {
    if (!(obj instanceof MBeanParameterInfo))
      return false;
    if (!(super.equals(obj)))
      return false;
    MBeanParameterInfo o = (MBeanParameterInfo) obj;
    return type.equals(o.getType());
  }

  /**
   * Returns the type of this attribute, in the form of its class name.
   *
   * @return the type of this attribute.
   */
  public String getType()
  {
    return type;
  }

  /**
   * Returns the hashcode of the parameter information as the sum of
   * the hashcode of the superclass and the hashcode of the type.
   *
   * @return the hashcode of the parameter information.
   */
  public int hashCode()
  {
    return super.hashCode() + type.hashCode();
  }

  /**
   * <p>
   * Returns a textual representation of this instance.  This
   * is constructed using the class name
   * (<code>javax.management.MBeanParameterInfo</code>) along
   * with the name, description and type of the parameter.
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
	  + ",type=" + type
	  + "]";
      }
    return string;
  }

}
