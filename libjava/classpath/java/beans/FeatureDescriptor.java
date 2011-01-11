/* java.beans.FeatureDescriptor
   Copyright (C) 1998, 2005 Free Software Foundation, Inc.

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


package java.beans;

import java.util.Enumeration;
import java.util.Hashtable;

/**
 * FeatureDescriptor is the common superclass for all JavaBeans Descriptor
 * classes. JavaBeans descriptors are abstract descriptors of properties,
 * events, methods, beans, etc.<P>
 *
 * <STRONG>Documentation Convention:</STRONG> for proper
 * Internalization of Beans inside an RAD tool, sometimes there
 * are two names for a property or method: a programmatic, or
 * locale-independent name, which can be used anywhere, and a
 * localized, display name, for ease of use.  In the
 * documentation I will specify different String values as
 * either <EM>programmatic</EM> or <EM>localized</EM> to
 * make this distinction clear.
 *
 * @author John Keiser
 * @since 1.1
 */

public class FeatureDescriptor
{
  String name;
  String displayName;
  String shortDescription;
  boolean expert;
  boolean hidden;
  boolean preferred;

  Hashtable<String,Object> valueHash;

  /**
   * Instantiate this FeatureDescriptor with appropriate default values.
   */
  public FeatureDescriptor()
  {
    valueHash = new Hashtable<String,Object>();
  }

  /**
   * Get the programmatic name of this feature.
   */
  public String getName()
  {
    return name;
  }

  /**
   * Set the programmatic name of this feature.
   *
   * @param name the new name for this feature.
   */
  public void setName(String name)
  {
    this.name = name;
  }

  /**
   * Get the localized (display) name of this feature.
   *
   * @returns The localized display name of this feature or falls
   * back to the programmatic name.
   */
  public String getDisplayName()
  {
    return (displayName == null) ? name : displayName;
  }

  /**
   * Set the localized (display) name of this feature.
   *
   * @param displayName the new display name for this feature.
   */
  public void setDisplayName(String displayName)
  {
    this.displayName = displayName;
  }

  /**
   * Get the localized short description for this feature.
   *
   * @returns A short localized description of this feature or
   * what <code>getDisplayName</code> returns in case, that no short description
   * is available.
   */
  public String getShortDescription()
  {
    return (shortDescription == null) ? getDisplayName() : shortDescription;
  }

  /**
   * Set the localized short description for this feature.
   *
   * @param shortDescription the new short description for this feature.
   */
  public void setShortDescription(String shortDescription)
  {
    this.shortDescription = shortDescription;
  }

  /**
   * Indicates whether this feature is for expert use only.
   *
   * @return true if for use by experts only,
   * or false if anyone can use it.
   */
  public boolean isExpert()
  {
    return expert;
  }

  /**
   * Set whether this feature is for expert use only.
   *
   * @param expert true if for use by experts only,
   * or false if anyone can use it.
   */
  public void setExpert(boolean expert)
  {
    this.expert = expert;
  }

  /**
   * Indicates whether this feature is for use by tools only.
   * If it is for use by tools only, then it should not be displayed.
   *
   * @return true if tools only should use it,
   * or false if anyone can see it.
   */
  public boolean isHidden()
  {
    return hidden;
  }

  /**
   * Set whether this feature is for use by tools only.
   * If it is for use by tools only, then it should not be displayed.
   *
   * @param hidden true if tools only should use it,
   * or false if anyone can see it.
   */
  public void setHidden(boolean hidden)
  {
    this.hidden = hidden;
  }

  public boolean isPreferred ()
  {
    return preferred;
  }

  public void setPreferred (boolean preferred)
  {
    this.preferred = preferred;
  }

  /**
   * Get an arbitrary value set with setValue().
   *
   * @param name the programmatic name of the key.
   *
   * @return the value associated with this name,
   * or null if there is none.
   */
  public Object getValue(String name)
  {
    return valueHash.get(name);
  }

  /**
   * Set an arbitrary string-value pair with this feature.
   *
   * @param name the programmatic name of the key.
   * @param value the value to associate with the name.
   */
  public void setValue(String name, Object value)
  {
    valueHash.put(name, value);
  }

  /**
   * Get a list of the programmatic key names set with setValue().
   *
   * @return an Enumerator over all the programmatic key names associated
   * with this feature.
   */
  public Enumeration<String> attributeNames()
  {
    return valueHash.keys();
  }
}
