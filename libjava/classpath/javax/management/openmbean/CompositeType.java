/* CompositeType.java -- Type descriptor for CompositeData instances.
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

import java.util.Collections;
import java.util.Iterator;
import java.util.Map;
import java.util.Set;
import java.util.TreeMap;

/**
 * The open type descriptor for instances of the
 * {@link CompositeData} class.
 * 
 * @author Andrew John Hughes (gnu_andrew@member.fsf.org)
 * @since 1.5
 */
public class CompositeType
  extends OpenType<CompositeData>
{

  /**
   * Compatible with JDK 1.5
   */
  private static final long serialVersionUID = -5366242454346948798L;

  /**
   * A map of item names to their descriptions.
   */
  private TreeMap<String,String> nameToDescription;

  /**
   * A map of item names to their types.
   */
  private TreeMap<String,OpenType<?>> nameToType;

  /**
   * The hash code of this instance.
   */
  private transient Integer hashCode;

  /**
   * The <code>toString()</code> result of this instance.
   */
  private transient String string;

  /**
   * <p>
   * Constructs a new {@link CompositeType} instance for the given
   * type name with the specified field names, descriptions and types.
   * All parameters, and the elements of the array parameters, must be
   * non-null and {@link java.lang.String} values must be something other
   * than the empty string.  The arrays must be non-empty, and be of
   * equal size.
   * </p>
   * <p>
   * The result of <code>CompositeData.class.getName()</code> is adopted
   * as the class name (see {@link OpenType}) and changes to the array
   * elements following construction of the {@link CompositeType} instance
   * will <strong>not</strong> affect the values used by the instance.
   * The field names are sorted in to ascending alphanumeric order internally,
   * and so ordering can not be used to differentiate between two instances.
   * </p>
   *
   * @param name the name of this composite type.
   * @param desc a description of this composite type.
   * @param names the names of each field within the composite type.
   * @param descs the descriptions of each field within the composite type.
   * @param types the types of each field within the composite type.
   * @throws IllegalArgumentException if any validity constraint listed above
   *                                  is broken.
   * @throws OpenDataException if duplicate item names are provided.  Item names
   *                           are case-sensitive, but whitespace is removed
   *                           before comparison.
   */
  public CompositeType(String name, String desc, String[] names,
		       String[] descs, OpenType<?>[] types)
    throws OpenDataException
  {
    super(CompositeData.class.getName(), name, desc);
    if (names.length == 0 
	|| names.length != descs.length
	|| names.length != types.length)
      throw new IllegalArgumentException("Arrays must be non-empty " +
					 "and of equal size.");
    nameToDescription = new TreeMap();
    for (int a = 0; a < names.length; ++a)
      {
	if (names[a] == null)
	  throw new IllegalArgumentException("Name " + a + " is null.");
	if (descs[a] == null)
	  throw new IllegalArgumentException("Description " + a + 
					     " is null.");
	String fieldName = names[a].trim();
	if (fieldName.length() == 0)
	  throw new IllegalArgumentException("Name " + a + " is " +
					     "the empty string.");
	if (descs[a].length() == 0)
	  throw new IllegalArgumentException("Description " + a + " is " +
					     "the empty string.");
	if (nameToDescription.containsKey(fieldName))
	  throw new OpenDataException(fieldName + " appears more " +
				      "than once.");
	nameToDescription.put(fieldName, descs[a]);
      }
    nameToType = new TreeMap<String,OpenType<?>>();
    for (int a = 0; a < names.length; ++a)
      nameToType.put(names[a].trim(), types[a]);
  }

  /**
   * Returns true if this composite data type has a field
   * with the given name.
   *
   * @param name the name of the field to check for.
   * @return true if a field of that name exists.
   */
  public boolean containsKey(String name)
  {
    return nameToDescription.containsKey(name);
  }

  /**
   * <p>
   * Compares this composite data type with another object
   * for equality.  The objects are judged to be equal if:
   * </p>
   * <ul>
   * <li><code>obj</code> is not null.</li>
   * <li><code>obj</code> is an instance of
   * {@link CompositeType}.</li>
   * <li>The type names are equal.</li>
   * <li>The fields and their types match.</li>
   * </ul>
   * 
   * @param obj the object to compare with.
   * @return true if the conditions above hold.
   */
  public boolean equals(Object obj)
  {
    if (!(obj instanceof CompositeType))
      return false;
    CompositeType ctype = (CompositeType) obj;
    if (!(ctype.getTypeName().equals(getTypeName())))
      return false;
    Set<String> keys = keySet();
    if (!(ctype.keySet().equals(keys)))
      return false;
    for (String key : keys)
    {
      if (!(ctype.getType(key).equals(getType(key))))
	return false;
    }
    return true;
  }

  /**
   * Returns the description for the given field name,
   * or <code>null</code> if the field name does not
   * exist within this composite data type.
   *
   * @param name the name of the field whose description
   *             should be returned.
   * @return the description, or <code>null</code> if the
   *         field doesn't exist.
   */
  public String getDescription(String name)
  {
    return nameToDescription.get(name);
  }

  /**
   * Returns the type for the given field name,
   * or <code>null</code> if the field name does not
   * exist within this composite data type.
   *
   * @param name the name of the field whose type
   *             should be returned.
   * @return the type, or <code>null</code> if the
   *         field doesn't exist.
   */
  public OpenType<?> getType(String name)
  {
    return nameToType.get(name);
  }

  /**
   * <p>
   * Returns the hash code of the composite data type.
   * This is computed as the sum of the hash codes of 
   * each field name and its type, together with the hash
   * code of the type name.  These are the same elements
   * of the type that are compared as part of the
   * {@link #equals(java.lang.Object)} method, thus ensuring
   * that the hashcode is compatible with the equality
   * test.
   * </p>
   * <p>
   * As instances of this class are immutable, the hash code
   * is computed just once for each instance and reused
   * throughout its life.
   * </p>
   *
   * @return the hash code of this instance.
   */
  public int hashCode()
  {
    if (hashCode == null)
      {
	int elementTotal = 0;
	Iterator it = nameToType.entrySet().iterator();
	while (it.hasNext())
	  {
	    Map.Entry entry = (Map.Entry) it.next();
	    elementTotal += (entry.getKey().hashCode() +
			     entry.getValue().hashCode());
	  }
	hashCode = Integer.valueOf(elementTotal 
				   + getTypeName().hashCode());
      }
    return hashCode.intValue();
  }
			       
  /**
   * Returns true if the specified object is a member of this
   * composite type.  The object is judged to be so if it is
   * an instance of {@link CompositeData} with an equivalent
   * type, according to the definition of
   * {@link #equals(java.lang.Object)} for {@link CompositeType}.
   *
   * @param obj the object to test for membership.
   * @return true if the object is a member of this type.
   */
  public boolean isValue(Object obj)
  {
    if (obj instanceof CompositeData)
      {
	CompositeData data = (CompositeData) obj;
	return equals(data.getCompositeType());
      }
    return false;
  }

  /**
   * Returns an unmodifiable {@link java.util.Set}-based
   * view of the field names that form part of this 
   * {@link CompositeType} instance.  The names are stored
   * in ascending alphanumeric order.
   *
   * @return a unmodifiable set containing the field
   *         name {@link java.lang.String}s.
   */
  public Set<String> keySet()
  {
    return Collections.unmodifiableSet(nameToDescription.keySet());
  }

  /**
   * <p>
   * Returns a textual representation of this instance.  This
   * is constructed using the class name
   * (<code>javax.management.openmbean.CompositeType</code>)
   * and each element of the instance which is relevant to
   * the definition of {@link equals(java.lang.Object)} and
   * {@link hashCode()} (i.e. the type name, and the name
   * and type of each field).
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
	+ "[name=" + getTypeName()
	+ ", fields=" + nameToType
	+ "]";
    return string;
  }

}
