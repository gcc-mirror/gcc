/* ActivationDesc.java -- record with info to activate an object
   Copyright (c) 1996, 1997, 1998, 1999 Free Software Foundation, Inc.

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

package java.rmi.activation;

import java.io.Serializable;
import java.rmi.MarshalledObject;

/**
 * Contains the information, necessary to activate the object. This information
 * includes:
 * <ul>
 * <li>the object class name</li>
 * <li>the object group identifier</li>
 * <li>the code location (codebase URL) that can be used to load the class
 * remotely</li>
 * <li>the object restart mode</li>
 * <li>the object specific intialization information</li>
 * </ul>
 *
 * @author Audrius Meskauskas (audriusa@bioinformatics.org) (from stub)
 */
public final class ActivationDesc
    implements Serializable
{
  /**
   * Use SVUID for interoperability.
   */
  static final long serialVersionUID = 7455834104417690957L;

  /**
   * The group id.
   */
  private ActivationGroupID groupid;

  /**
   * The class name.
   */
  private String classname;

  /**
   * The code location URL.
   */
  private String location;

  /**
   * The object specific intitalization data.
   */
  private MarshalledObject<?> data;

  /**
   * The start mode.
   */
  private boolean restart;

  /**
   * Create the new activation description, assuming the object group is the
   * {@link ActivationGroup#currentGroupID()}.
   *
   * @param className the object fully qualified class name
   * @param location the code base URL
   * @param data the object initialization data, contained in a marshalled form
   */
  public ActivationDesc(String className, String location, MarshalledObject<?> data)
      throws ActivationException
  {
    this(ActivationGroup.currentGroupID(), className, location, data, false);
  }

  /**
   * Create the new activation description, assuming the object group is the
   * {@link ActivationGroup#currentGroupID()}.
   *
   * @param className the object fully qualified class name
   * @param location the code base URL
   * @param data the object initialization data, contained in a marshalled form
   * @param restart specifies reactivation mode after crash. If true, the object
   *          is activated when activator is restarted or the activation group
   *          is restarted. If false, the object is only activated on demand.
   *          This flag does has no effect during the normal operation (the
   *          object is normally activated on demand).
   */
  public ActivationDesc(String className, String location,
                        MarshalledObject<?> data, boolean restart)
      throws ActivationException
  {
    this(ActivationGroup.currentGroupID(), className, location, data, restart);
  }

  /**
   * Create the new activation description. Under crash, the object will only
   * be reactivated on demand.
   *
   * @param groupID the object group id.
   * @param className the object fully qualified class name
   * @param location the code base URL
   * @param data the object initialization data, contained in a marshalled form
   */
  public ActivationDesc(ActivationGroupID groupID, String className,
                        String location, MarshalledObject<?> data)
  {
    this(groupID, className, location, data, false);
  }

  /**
   * Create the new activation description, providing full information.
   *
   * @param groupID the object group id.
   * @param className the object fully qualified class name
   * @param location the code base URL
   * @param data the object initialization data, contained in a marshalled form
   * @param restart specifies reactivation mode after crash. If true, the object
   *          is activated when activator is restarted or the activation group
   *          is restarted. If false, the object is only activated on demand.
   *          This flag does has no effect during the normal operation (the
   *          object is normally activated on demand).
   */
  public ActivationDesc(ActivationGroupID groupID, String className,
                        String location, MarshalledObject<?> data, boolean restart)
  {
    this.groupid = groupID;
    this.classname = className;
    this.location = location;
    this.data = data;
    this.restart = restart;
  }

  public ActivationGroupID getGroupID()
  {
    return groupid;
  }

  /**
   * Get the class name of the object being activated
   *
   * @return the fully qualified class name of the object being activated
   */
  public String getClassName()
  {
    return classname;
  }

  /**
   * Get the code location URL ("codebase") of the object being activated.
   *
   * @return the codebase of the object being activated.
   */
  public String getLocation()
  {
    return location;
  }

  public MarshalledObject<?> getData()
  {
    return data;
  }

  /**
   * Get the object reactivation strategy after crash.
   *
   * @return true ir the object is activated when activator is restarted or the
   *         activation group is restarted. False if the object is only
   *         activated on demand. This flag does has no effect during the normal
   *         operation (the object is normally activated on demand).
   */
  public boolean getRestartMode()
  {
    return restart;
  }

  /**
   * Compare this object with another activation description for equality.
   *
   * @return true if all fields have the equal values, false otherwise.
   */
  public boolean equals(Object obj)
  {
    if (obj instanceof ActivationDesc)
      {
        ActivationDesc that = (ActivationDesc) obj;
        return eq(groupid, that.groupid) &&
               eq(classname, that.classname) &&
               eq(location, that.location) &&
               eq(data, that.data)
               && restart == that.restart;
      }
    else
      return false;
  }

  /**
   * Get the hash code of this object (overridden to make the returned value
   * consistent with .equals(..).
   */
  public int hashCode()
  {
    return hash(groupid) ^ hash(classname) ^
      hash(location) ^ hash(data);
  }

  /**
   * Get the hashcode of x or 0 if x == null.
   */
  static final int hash(Object x)
  {
    return x == null ? 0 : x.hashCode();
  }

  /**
   * Compare by .equals if both a and b are not null, compare directly if at
   * least one of them is null.
   */
  static final boolean eq(Object a, Object b)
  {
    if (a == null || b == null)
      return a == b;
    else
      return a.equals(b);
  }
}
