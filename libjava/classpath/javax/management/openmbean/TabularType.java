/* TabularType.java -- Type descriptor for TabularData instances.
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

import java.util.Arrays;
import java.util.Collections;
import java.util.Iterator;
import java.util.List;

/**
 * The open type descriptor for instances of the
 * {@link TabularData} class.
 * 
 * @author Andrew John Hughes (gnu_andrew@member.fsf.org)
 * @since 1.5
 */
public class TabularType
  extends OpenType<TabularData>
{

  /**
   * Compatible with JDK 1.5
   */
  private static final long serialVersionUID = 6554071860220659261L;

  /**
   * The composite type used by the rows of the table.
   */
  private CompositeType rowType;

  /**
   * The list of index names, which form the columns of the table.
   * They are retained in the order given by the user, and is
   * unmodifiable.
   */
  private List<String> indexNames;

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
   * Constructs a new {@link TabularType} instance for the given
   * type name, description, row type and index names.  All parameters
   * (including the individual elements of the array of index names)
   * must be non-null and those that are of type {@link java.lang.String}
   * must be non-empty.  The array of index names must also be non-empty.
   * </p>
   * <p>
   * The result of <code>TabularData.class.getName()</code> is adopted
   * as the class name (see {@link OpenType}).  The ordering of the array
   * elements is relevant in determining the indicies of the values in the
   * table, and thus in the use of the 
   * {@link TabularData#get(java.lang.Object[])} and
   * {@link TabularData#remove(java.lang.Object[])} methods of the
   * {@link TabularData} class.
   * </p>
   *
   * @param name the name of this tabular type.
   * @param desc a description of this tabular type.
   * @param rowType the type of the rows of the table.
   * @param indexNames the names used to index the rows within the table.
   * @throws IllegalArgumentException if any validity constraint listed above
   *                                  is broken.
   * @throws OpenDataException if an index name does not match a corresponding
   *                           name in the given row type.
   */
  public TabularType(String name, String desc, CompositeType rowType,
		     String[] indexNames)
    throws OpenDataException
  {
    super(TabularData.class.getName(), name, desc);
    if (rowType == null)
      throw new IllegalArgumentException("A null row type was given.");
    for (int a = 0; a < indexNames.length; ++a)
      {
	if (indexNames[a] == null)
	  throw new IllegalArgumentException("Name " + a +
					     " is null.");
	if (indexNames[a].length() == 0)
	  throw new IllegalArgumentException("Name " + a + 
					     " is the empty string.");
	if (!(rowType.containsKey(indexNames[a])))
	  throw new OpenDataException("No matching key for " +
				      indexNames[a] + " was found in " +
				      "the supplied row type.");
      }
    this.rowType = rowType;
    this.indexNames = Collections.unmodifiableList(Arrays.asList(indexNames));
  }

  /**
   * <p>
   * Compares this tabular data type with another object
   * for equality.  The objects are judged to be equal if:
   * </p>
   * <ul>
   * <li><code>obj</code> is not null.</li>
   * <li><code>obj</code> is an instance of
   * {@link TabularType}.</li>
   * <li>The type names are equal.</li>
   * <li>The row types are equal.</li>
   * <li>The index names are equal and in the same order.</li>
   * </ul>
   * 
   * @param obj the object to compare with.
   * @return true if the conditions above hold.
   */
  public boolean equals(Object obj)
  {
    if (!(obj instanceof TabularType))
      return false;
    TabularType ttype = (TabularType) obj;
    return  (ttype.getTypeName().equals(getTypeName())
	     && (ttype.getRowType().equals(getRowType()))
	     && (ttype.getIndexNames().equals(getIndexNames())));
  }

  /**
   * Returns an unmodifiable list containing the index names.
   * The ordering of these names is used to determine the indicies
   * of the {@link CompositeData} values, and is retained from that
   * used in the call to this object's constructor.
   *
   * @return an unmodifiable list of the index names used by this
   *         tabular data structure.
   */
  public List<String> getIndexNames()
  {
    return indexNames;
  }

  /**
   * Returns the type of the rows used by this tabular data structure.
   *
   * @return the row type.
   */
  public CompositeType getRowType()
  {
    return rowType;
  }

  /**
   * <p>
   * Returns the hash code of the tabular data type.
   * This is computed as the sum of the hash codes of the
   * index names together with the hash code of the type
   * name and row type.  These are the same elements
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
	for (String s : indexNames)
	  elementTotal += s.hashCode();
	hashCode = Integer.valueOf(elementTotal 
				   + getTypeName().hashCode()
				   + rowType.hashCode());
      }
    return hashCode.intValue();
  }
			       
  /**
   * Returns true if the specified object is a member of this
   * tabular type.  The object is judged to be so if it is
   * an instance of {@link TabularData} with an equivalent
   * type, according to the definition of
   * {@link #equals(java.lang.Object)} for {@link TabularType}.
   *
   * @param obj the object to test for membership.
   * @return true if the object is a member of this type.
   */
  public boolean isValue(Object obj)
  {
    if (obj instanceof TabularData)
      {
	TabularData data = (TabularData) obj;
	return equals(data.getTabularType());
      }
    return false;
  }

  /**
   * <p>
   * Returns a textual representation of this instance.  This
   * is constructed using the class name
   * (<code>javax.management.openmbean.TabularType</code>)
   * and each element of the instance which is relevant to
   * the definition of {@link equals(java.lang.Object)} and
   * {@link hashCode()} (i.e. the type name, the row type
   * and the index names).
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
	+ ", rowType=" + rowType
	+ ", indexNames=" + indexNames
	+ "]";
    return string;
  }

}
