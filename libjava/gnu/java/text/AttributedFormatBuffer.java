/* AttributedFormatBuffer.java -- Implements an attributed FormatBuffer.
   Copyright (C) 2004 Free Software Foundation, Inc.

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
Free Software Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA
02111-1307 USA.

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
package gnu.java.text;

import java.text.AttributedCharacterIterator;
import java.util.ArrayList;
import java.util.HashMap;

/**
 * This class is an implementation of a FormatBuffer with attributes.
 * 
 * @author Guilhem Lavaux <guilhem@kaffe.org>
 * @date April 10, 2004
 */
public class AttributedFormatBuffer implements FormatBuffer
{
  private StringBuffer buffer;
  private ArrayList ranges;
  private ArrayList attributes;
  private int[] a_ranges;
  private HashMap[] a_attributes; 
  private int startingRange;
  AttributedCharacterIterator.Attribute defaultAttr;

  /**
   * This constructor accepts a StringBuffer. If the buffer contains
   * already some characters they will not be attributed. 
   */
  public AttributedFormatBuffer(StringBuffer buffer)
  {
    this.buffer = buffer;
    this.ranges = new ArrayList();
    this.attributes = new ArrayList();
    this.defaultAttr = null;
    if (buffer.length() != 0)
      {
	this.startingRange = buffer.length();
	addAttribute(buffer.length(), null);
      }
    else
      this.startingRange = -1;
  }

  public AttributedFormatBuffer(int prebuffer)
  {
    this(new StringBuffer(prebuffer));
  }

  public AttributedFormatBuffer()
  {
    this(10);
  }

  /**
   * This method is a helper function for formatters. Given a set of ranges
   * and attributes it adds exactly one attribute for the range of characters
   * comprised between the last entry in 'ranges' and the specified new range.
   *
   * @param new_range A new range to insert in the list.
   * @param new_attribute A new attribute to insert in the list.
   */  
  private final void addAttribute(int new_range, AttributedCharacterIterator.Attribute attr)
  {
    HashMap map;

    if (attr != null)
      {
	map = new HashMap();
	map.put(attr, attr);
	attributes.add(map);
      }
    else
      attributes.add(null);

    ranges.add(new Integer(new_range));
  }

  public void append(String s)
  {
    if (startingRange < 0)
      startingRange = 0;
    buffer.append(s);
  }
  
  public void append(String s, AttributedCharacterIterator.Attribute attr)
  {
    setDefaultAttribute(attr);
    startingRange = buffer.length();
    append(s);
    setDefaultAttribute(null);
  }

  public void append(String s, int[] ranges, HashMap[] attrs)
  {
    int curPos = buffer.length();

    setDefaultAttribute(null);
    if (ranges != null)
      {
	for (int i = 0; i < ranges.length; i++)
	  {	    
	    this.ranges.add(new Integer(ranges[i] + curPos));
	    this.attributes.add(attrs[i]);
	  }
      }
    startingRange = buffer.length();
    buffer.append(s);
  }

  public void append(char c)
  {
    if (startingRange < 0)
      startingRange = buffer.length();
    buffer.append(c);
  }

  public void append(char c, AttributedCharacterIterator.Attribute attr)
  {
    setDefaultAttribute(attr);
    buffer.append(c);
    setDefaultAttribute(null);
  }

  public void setDefaultAttribute(AttributedCharacterIterator.Attribute attr)
  {
    if (attr == defaultAttr)
      return;

    int currentPos = buffer.length();

    if (startingRange != currentPos && startingRange >= 0)
      {
	addAttribute(currentPos, defaultAttr);
      }
    defaultAttr = attr;
    startingRange = currentPos;
  }

  public AttributedCharacterIterator.Attribute getDefaultAttribute()
  {
    return defaultAttr;
  }

  public void cutTail(int length)
  {
    buffer.setLength(buffer.length()-length);
  }

  public int length()
  {
    return buffer.length();
  }

  public void clear()
  {
    buffer.setLength(0);
    ranges.clear();
    attributes.clear();
    defaultAttr = null;
    startingRange = -1;
  }

  /**
   * This method synchronizes the state of the attribute array.
   * After calling it you may call {@link #getDefaultAttribute()}.
   */
  public void sync()
  {
    if (startingRange < 0 || startingRange == buffer.length())
      return;

    addAttribute(buffer.length(), defaultAttr);

    a_ranges = new int[ranges.size()];
    for (int i = 0; i < a_ranges.length; i++)
      a_ranges[i] = ((Integer)(ranges.get (i))).intValue();
    
    a_attributes = new HashMap[attributes.size()];
    System.arraycopy(attributes.toArray(), 0, a_attributes, 0, a_attributes.length);
  }

  /**
   * This method returns the internal StringBuffer describing
   * the attributed string.
   *
   * @return An instance of StringBuffer which contains the string.
   */
  public StringBuffer getBuffer()
  {
    return buffer;
  }

  /**
   * This method returns the ranges for the attributes.
   *
   * @return An array of int describing the ranges.
   */
  public int[] getRanges()
  {
    return a_ranges;
  }

  /**
   * This method returns the array containing the map on the 
   * attributes.
   *
   * @return An array of {@link java.util.Map} containing the attributes.
   */
  public HashMap[] getAttributes()
  {
    return a_attributes;
  }
}
