/* MBeanFeatureInfo.java -- Information about a bean feature.
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

import java.io.IOException;
import java.io.ObjectOutputStream;
import java.io.Serializable;

/**
 * A general superclass for the description of features
 * of management beans.  This allows the user to access
 * the feature dynamically, without knowing the details
 * beforehand.  The information is immutable as standard.
 * Of course, subclasses may change this, but this
 * behaviour is not recommended.
 * @author Andrew John Hughes (gnu_andrew@member.fsf.org)
 * @since 1.5
 */
public class MBeanFeatureInfo
  implements Serializable
{

  /**
   * Compatible with JDK 1.5
   */
  private static final long serialVersionUID = 3952882688968447265L;

  /**
   * A description of the feature in human-readable form.
   * Subclasses should access this via the {@link #getDescription()}
   * function rather than using the value directly.
   *
   * @serial a description of the feature.
   */
  protected String description;

  /**
   * The name of the feature.  Subclasses should access this
   * via the {@link #getName()} function rather than using the
   * value directly.
   *
   * @serial the name of the feature.
   */
  protected String name;

  /**
   * The <code>toString()</code> result of this instance.
   */
  transient String string;

  /**
   * Constructs a new {@link MBeanFeatureInfo} with the specified
   * name and description.
   *
   * @param name the name of the management bean feature.
   * @param description the description of the feature.
   */
  public MBeanFeatureInfo(String name, String description)
  {
    this.name = name;
    this.description = description;
  }

  /**
   * Compares this feature with the supplied object.  This
   * returns true iff the object is an instance of
   * {@link MBeanFeatureInfo} and {@link Object#equals()}
   * returns true for a comparison of both the name and
   * description of this feature with that of the specified
   * object.
   *
   * @param obj the object to compare.
   * @return true if the object is a {@link MBeanFeatureInfo}
   *         instance,
   *         <code>name.equals(object.getName())</code> and
   *         <code>description.equals(object.getDescription</code>.
   */
  public boolean equals(Object obj)
  {
    if (obj instanceof MBeanFeatureInfo)
      {
        MBeanFeatureInfo o = (MBeanFeatureInfo) obj;
        return ((name == null ?
                 o.getName() == null :
                 name.equals(o.getName())) &&
                (description == null ?
                 o.getDescription() == null :
                 description.equals(o.getDescription())));
      }
    else
      return false;
  }

  /**
   * Returns a description of this feature.
   *
   * @return a human-readable description.
   */
  public String getDescription()
  {
    return description;
  }

  /**
   * Returns the name of this feature.
   *
   * @return the name of the feature.
   */
  public String getName()
  {
    return name;
  }

  /**
   * Returns the hashcode of the feature as
   * the sum of the hashcodes of its name
   * and description.
   *
   * @return the hashcode of this feature.
   */
  public int hashCode()
  {
    return (name == null ? -1 : name.hashCode())
      + (description == null ? -1 : description.hashCode());
  }

  /**
   * <p>
   * Returns a textual representation of this instance.  This
   * is constructed using the class name
   * (<code>javax.management.MBeanFeatureInfo</code>) and
   * the name and description of the feature.
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
        + "[name=" + name
        + ",desc=" + description
        + "]";
    return string;
  }

  /**
   * Serialize the {@link MBeanFeatureInfo}.
   *
   * @param out the output stream to write to.
   * @throws IOException if an I/O error occurs.
   */
  private void writeObject(ObjectOutputStream out)
    throws IOException
  {
    out.defaultWriteObject();
    /* FIXME: Handle extra 1.6 descriptor stuff */
  }

}
