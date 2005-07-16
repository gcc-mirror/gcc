/* AccessibleRelation.java -- the relation between accessible objects
   Copyright (C) 2002, 2005 Free Software Foundation

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

package javax.accessibility;

import java.util.Locale;

/**
 * The relation between one accessible object and one or more other objects.
 * For example, a button may control an action. An AccessibleRelationSet
 * summarizes all relations of the object. This strongly typed "enumeration"
 * supports localized strings. If the constants of this class are not
 * adequate, new ones may be added in a similar matter.
 * 
 * @author Eric Blake (ebb9@email.byu.edu)
 * @since 1.2
 * @status updated to 1.4
 */
public class AccessibleRelation extends AccessibleBundle
{
  /**
   * Indicates the object labels other objects.
   *
   * @see #getTarget()
   * @see #CONTROLLER_FOR
   * @see #CONTROLLED_BY
   * @see #LABELED_BY
   * @see #MEMBER_OF
   */
  public static final String LABEL_FOR = "labelFor";

  /**
   * Indicates the object is labeled by other objects.
   *
   * @see #getTarget()
   * @see #CONTROLLER_FOR
   * @see #CONTROLLED_BY
   * @see #LABEL_FOR
   * @see #MEMBER_OF
   */
  public static final String LABELED_BY = "labeledBy";

  /**
   * Indicates an object is a member of a group of target objects.
   *
   * @see #getTarget()
   * @see #CONTROLLER_FOR
   * @see #CONTROLLED_BY
   * @see #LABEL_FOR
   * @see #LABELED_BY
   */
  public static final String MEMBER_OF = "memberOf";

  /**
   * Indicates an object is a controller for other objects.
   *
   * @see #getTarget()
   * @see #CONTROLLED_BY
   * @see #LABEL_FOR
   * @see #LABELED_BY
   * @see #MEMBER_OF
   */
  public static final String CONTROLLER_FOR = "controllerFor";

  /**
   * Indicates an object is controlled by other objects.
   *
   * @see #getTarget()
   * @see #CONTROLLER_FOR
   * @see #LABEL_FOR
   * @see #LABELED_BY
   * @see #MEMBER_OF
   */
  public static final String CONTROLLED_BY = "controlledBy";

  /** Indicates that the label target group has changed. */
  public static final String LABEL_FOR_PROPERTY = "labelForProperty";

  /** Indicates that the labelling objects have changed. */
  public static final String LABELED_BY_PROPERTY = "labeledByProperty";

  /** Indicates that group membership has changed. */
  public static final String MEMBER_OF_PROPERTY = "memberOfProperty";

  /** Indicates that the controller target group has changed. */
  public static final String CONTROLLER_FOR_PROPERTY = "controllerForProperty";

  /** Indicates that the controlling objects have changed. */
  public static final String CONTROLLED_BY_PROPERTY = "controlledByProperty";

  /** An empty set of targets. */
  private static final Object[] EMPTY_TARGETS = { };

  /**
   * The related objects.
   *
   * @see #getTarget()
   * @see #setTarget(Object)
   * @see #setTarget(Object[])
   */
  Object[] targets;

  /**
   * Create a new relation with a locale independent key, and no related
   * objects.
   *
   * @param key the name of the role
   * @see #toDisplayString(String, Locale)
   */
  public AccessibleRelation(String key)
  {
    this.key = key;
    targets = EMPTY_TARGETS;
  }

  /**
   * Create a new relation with a locale independent key, and a single related
   * object.
   *
   * @param key the name of the role
   * @param target the related object
   * @see #toDisplayString(String, Locale)
   */
  public AccessibleRelation(String key, Object target)
  {
    this.key = key;
    targets = new Object[] { target };
  }

  /**
   * Create a new relation with a locale independent key, and the given
   * related objects.
   *
   * @param key the name of the role
   * @param targets the related objects
   * @see #toDisplayString(String, Locale)
   */
  public AccessibleRelation(String key, Object[] targets)
  {
    this.key = key;
    this.targets = targets == null ? EMPTY_TARGETS : targets;
  }

  /**
   * Return the key for this relation.
   *
   * @return the key
   * @see #CONTROLLER_FOR
   * @see #CONTROLLED_BY
   * @see #LABEL_FOR
   * @see #LABELED_BY
   * @see #MEMBER_OF
   */
  public String getKey()
  {
    return key;
  }

  /**
   * Return the targets of this relation.
   *
   * @return the targets, may be empty, but never null
   */
  public Object[] getTarget()
  {
    return targets;
  }

  /**
   * Set the target to a single object.
   *
   * @param target the new target
   */
  public void setTarget(Object target)
  {
    targets = new Object[] { target };
  }

  /**
   * Set the target to an array of objects.
   *
   * @param targets the new targets
   */
  public void setTarget(Object[] targets)
  {
    this.targets = targets == null ? EMPTY_TARGETS : targets;
  }
} // class AccessibleRelation
