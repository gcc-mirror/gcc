/* AttributedFormatBuffer.java -- Implements an attributed FormatBuffer.
   Copyright (C) 2004, 2012 Free Software Foundation, Inc.

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
package gnu.java.text;

import gnu.java.lang.CPStringBuilder;

import java.text.AttributedCharacterIterator;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import static java.text.AttributedCharacterIterator.Attribute;

/**
 * This class is an implementation of a FormatBuffer with attributes.
 * Note that this class is not thread-safe; external synchronisation
 * should be used if an instance is to be accessed from multiple threads.
 *
 * @author Guilhem Lavaux <guilhem@kaffe.org>
 * @date April 10, 2004
 */
public class AttributedFormatBuffer implements FormatBuffer
{
  private final CPStringBuilder buffer;
  private final ArrayList<Integer> ranges;
  private final ArrayList<Map<Attribute,Object>> attributes;
  private int[] aRanges;
  private List<Map<Attribute,Object>> aAttributes;
  private int startingRange;
  Attribute defaultAttr;

  /**
   * This constructor accepts a StringBuffer. If the buffer contains
   * already some characters they will not be attributed.
   */
  public AttributedFormatBuffer(CPStringBuilder buffer)
  {
    this.buffer = new CPStringBuilder(buffer);
    this.ranges = new ArrayList<Integer>();
    this.attributes = new ArrayList<Map<Attribute,Object>>();
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
    this(new CPStringBuilder(prebuffer));
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
   * @param newRange A new range to insert in the list.
   * @param attr A new attribute to insert in the list.
   */
  private final void addAttribute(int newRange, Attribute attr)
  {
    Map<Attribute,Object> map;

    if (attr != null)
      {
        map = new HashMap<Attribute,Object>();
        map.put(attr, attr);
        attributes.add(map);
      }
    else
      attributes.add(null);

    ranges.add(Integer.valueOf(newRange));
  }

  public void append(String s)
  {
    if (startingRange < 0)
      startingRange = 0;
    buffer.append(s);
  }

  public void append(String s, Attribute attr)
  {
    setDefaultAttribute(attr);
    startingRange = buffer.length();
    append(s);
    setDefaultAttribute(null);
  }

  public void append(String s, int[] ranges, List<Map<Attribute,Object>> attrs)
  {
    int curPos = buffer.length();

    setDefaultAttribute(null);
    if (ranges != null)
      {
        for (int i = 0; i < ranges.length; i++)
          {
            this.ranges.add(Integer.valueOf(ranges[i] + curPos));
            this.attributes.add(attrs.get(i));
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

  public void append(char c, Attribute attr)
  {
    setDefaultAttribute(attr);
    buffer.append(c);
    setDefaultAttribute(null);
  }

  public void setDefaultAttribute(Attribute attr)
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

  public Attribute getDefaultAttribute()
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

    aRanges = new int[ranges.size()];
    for (int i = 0; i < aRanges.length; i++)
      aRanges[i] = ranges.get (i).intValue();

    aAttributes = new ArrayList<Map<Attribute,Object>>(attributes);
  }

  /**
   * This method returns the internal CPStringBuilder describing
   * the attributed string.
   *
   * @return An instance of CPStringBuilder which contains the string.
   */
  public CPStringBuilder getBuffer()
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
    return aRanges;
  }

  /**
   * This method returns the array containing the map on the
   * attributes.
   *
   * @return A {@link java.util.List} of {@link java.util.Map}s containing the attributes.
   */
  public List<Map<Attribute,Object>> getAttributes()
  {
    return aAttributes;
  }
}
