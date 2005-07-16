/* FieldPosition.java -- Keeps track of field positions while formatting
   Copyright (C) 1998, 1999, 2001, 2005  Free Software Foundation, Inc.

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


package java.text;

/**
 * This class is used by the java.text formatting classes to track
 * field positions.  A field position is defined by an identifier value
 * and begin and end index positions.  The formatting classes in java.text
 * typically define constant values for the field identifiers.
 *
 * @author Aaron M. Renn (arenn@urbanophile.com)
 * @author Per Bothner (bothner@cygnus.com)
 */
public class FieldPosition
{
  /**
   * This is the field identifier value.
   */
  private int field_id;

  /**
   * This is the beginning index of the field.
   */
  private int begin;

  /**
   * This is the ending index of the field.
   */
  private int end;

  /**
   * This is the field attribute value.
   */
  private Format.Field field_attribute;

  /**
   * This method initializes a new instance of <code>FieldPosition</code>
   * to have the specified field attribute. The attribute will be used as
   * an id. It is formally equivalent to calling FieldPosition(field, -1).
   *
   * @param field The field format attribute.
   */
  public FieldPosition (Format.Field field)
  {
    this(field, -1);
  }

  /**
   * This method initializes a new instance of <code>FieldPosition</code>
   * to have the specified field attribute. The attribute will be used as
   * an id is non null. The integer field id is only used if the Format.Field
   * attribute is not used by the formatter.
   *
   * @param field The field format attribute.
   * @param field_id The field identifier value.
   */
  public FieldPosition (Format.Field field, int field_id)
  {
    this.field_attribute = field;
    this.field_id = field_id;
  }

  /**
   * This method initializes a new instance of <code>FieldPosition</code> to
   * have the specified field id.
   *
   * @param field_id The field identifier value.
   */
  public FieldPosition (int field_id)
  {
    this.field_id = field_id;
  }

  /**
   * This method returns the field identifier value for this object.
   *
   * @return The field identifier.
   */
  public int getField ()
  {
    return field_id;
  }

  public Format.Field getFieldAttribute ()
  {
    return field_attribute;
  }

  /**
   * This method returns the beginning index for this field.
   *
   * @return The beginning index.
   */
  public int getBeginIndex ()
  {
    return begin;
  }

  /**
   * This method sets the beginning index of this field to the specified value.
   *
   * @param begin The new beginning index.
   */
  public void setBeginIndex (int begin)
  {
    this.begin = begin;
  }

  /**
   * This method returns the ending index for the field.
   *
   * @return The ending index.
   */
  public int getEndIndex ()
  {
    return end;
  }

  /**
   * This method sets the ending index of this field to the specified value.
   *
   * @param end The new ending index.
   */
  public void setEndIndex (int end)
  {
    this.end = end;
  }

  /**
   * This method tests this object for equality against the specified object.
   * The objects will be considered equal if and only if:
   * <p>
   * <ul>
   * <li>The specified object is not <code>null</code>.
   * <li>The specified object has the same class as this object.
   * <li>The specified object has the same field identifier, field attribute 
   * and beginning and ending index as this object.
   * </ul>
   *
   * @param obj The object to test for equality to this object.
   *
   * @return <code>true</code> if the specified object is equal to
   * this object, <code>false</code> otherwise. 
   */
  public boolean equals (Object obj)
  {
    if (this == obj)
      return true;

    if (obj == null || obj.getClass() != this.getClass())
      return false;

    FieldPosition fp = (FieldPosition) obj;
    return (field_id == fp.field_id
	    && (field_attribute == fp.field_attribute 
		|| (field_attribute != null 
		    && field_attribute.equals(fp.field_attribute)))
	    && begin == fp.begin
	    && end == fp.end);
  }


  /**
   * This method returns a hash value for this object
   * 
   * @return A hash value for this object.
   */
  public int hashCode ()
  {
    int hash = 5;

    hash = 31 * hash + field_id;
    hash = 31 * hash + begin;
    hash = 31 * hash + end;
    hash = 31 * hash + 
      (null == field_attribute ? 0 : field_attribute.hashCode());

    return hash;
  }

  /**
   * This method returns a <code>String</code> representation of this
   * object.
   *
   * @return A <code>String</code> representation of this object.
   */
  public String toString ()
  {
    return (getClass ().getName ()
	    + "[field=" + getField ()
	    + ",attribute=" + getFieldAttribute ()
	    + ",beginIndex=" + getBeginIndex () 
	    + ",endIndex=" + getEndIndex () 
	    + "]");
  }
}
