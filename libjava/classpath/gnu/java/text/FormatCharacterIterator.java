/* FormatCharacter.java -- Implementation of AttributedCharacterIterator for
   formatters.
   Copyright (C) 1998, 1999, 2000, 2001, 2003, 2004, 2005, 2012 Free Software Foundation, Inc.

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

import java.text.AttributedCharacterIterator;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Set;

import static java.text.AttributedCharacterIterator.Attribute;

/**
 * This class should not be put public and it is only intended to the
 * classes of the java.text package. Its aim is to build a segmented
 * character iterator by appending strings and adding attributes to
 * portions of strings. The code intends to do some optimization
 * concerning memory consumption and attribute access but at the
 * end it is only an AttributedCharacterIterator.
 *
 * @author Guilhem Lavaux <guilhem@kaffe.org>
 * @date November 22, 2003
 */
public class FormatCharacterIterator implements AttributedCharacterIterator
{
  private String formattedString;
  private int charIndex;
  private int attributeIndex;
  private int[] ranges;
  private List<Map<Attribute,Object>> attributes;
  private static final boolean DEBUG = false;

  /**
   * This constructor builds an empty iterated strings. The attributes
   * are empty and so is the string. However you may append strings
   * and attributes to this iterator.
   */
  public FormatCharacterIterator()
  {
    formattedString = "";
    ranges = new int[0];
    attributes = new ArrayList<Map<Attribute,Object>>(0);
  }

  /**
   * This constructor take a string <code>s</code>, a set of ranges
   * and the corresponding attributes. This is used to build an iterator.
   * The array <code>ranges</code> should be formatted as follow:
   * each element of <code>ranges</code> specifies the index in the string
   * until which the corresponding map of attributes at the same position
   * is applied. For example, if you have:
   * <pre>
   *   s = "hello";
   *   ranges = new int[] { 2, 6 };
   *   attributes = new ArrayList<Map<Attribute,Object>>(2);
   * </pre>
   * <code>"he"</code> will have the attributes <code>attributes.get(0)</code>,
   * <code>"llo"</code> the <code>attributes.get(1)</code>.
   */
  public FormatCharacterIterator (String s, int[] ranges,
                                  List<Map<Attribute,Object>> attributes)
  {
    formattedString = s;
    this.ranges = ranges;
    this.attributes = attributes;
  }

  /*
   * The following methods are inherited from AttributedCharacterIterator,
   * and thus are already documented.
   */

  public Set<Attribute> getAllAttributeKeys()
  {
    if (attributes != null && attributes.get(attributeIndex) != null)
      return attributes.get(attributeIndex).keySet();
    else
      return new HashSet<Attribute>();
  }

  public Map<Attribute,Object> getAttributes()
  {
    if (attributes != null && attributes.get(attributeIndex) != null)
      return attributes.get(attributeIndex);
    else
      return new HashMap<Attribute,Object>();
  }

  public Object getAttribute (Attribute attrib)
  {
    if (attributes != null && attributes.get(attributeIndex) != null)
      return attributes.get(attributeIndex).get (attrib);
    else
      return null;
  }

  public int getRunLimit(Set<? extends Attribute> reqAttrs)
  {
    if (attributes == null)
      return formattedString.length();

    int currentAttrIndex = attributeIndex;
    Set<Attribute> newKeys;

    do
      {
        currentAttrIndex++;
        if (currentAttrIndex == attributes.size())
          return formattedString.length();
        Map<Attribute,Object> currentAttr =
          attributes.get(currentAttrIndex);
        if (currentAttr == null)
          break;
        newKeys = currentAttr.keySet();
      }
    while (newKeys.containsAll (reqAttrs));

    return ranges[currentAttrIndex-1];
  }

  public int getRunLimit (Attribute attribute)
  {
    Set<Attribute> s = new HashSet<Attribute>();

    s.add (attribute);
    return getRunLimit (s);
  }

  public int getRunLimit()
  {
    if (attributes == null)
      return formattedString.length();
    if (attributes.get(attributeIndex) == null)
      {
        for (int i=attributeIndex+1;i<attributes.size();i++)
          if (attributes.get(i) != null)
            return ranges[i-1];
        return formattedString.length();
      }

    return getRunLimit (attributes.get(attributeIndex).keySet());
  }

  public int getRunStart (Set<? extends Attribute> reqAttrs)
  {
    if (attributes == null)
      return formattedString.length();

    int currentAttrIndex = attributeIndex;
    Set<Attribute> newKeys = null;

    do
      {
        if (currentAttrIndex == 0)
          return 0;

        currentAttrIndex--;
        Map<Attribute,Object> currentAttr =
          attributes.get(currentAttrIndex);
        if (currentAttr == null)
          break;
        newKeys = currentAttr.keySet();
      }
    while (newKeys.containsAll (reqAttrs));

    return (currentAttrIndex > 0) ? ranges[currentAttrIndex-1] : 0;
  }

  public int getRunStart()
  {
    if (attributes == null)
      return 0;

    Map<Attribute,Object> attrib = attributes.get(attributeIndex);
    if (attrib == null)
      {
        for (int i=attributeIndex;i>0;i--)
          if (attributes.get(i) != null)
            return ranges[attributeIndex-1];
        return 0;
      }

    return getRunStart (attrib.keySet());
  }

  public int getRunStart (Attribute attribute)
  {
    Set<Attribute> s = new HashSet<Attribute>();

    s.add (attribute);
    return getRunStart (s);
  }

  public Object clone()
  {
    return new FormatCharacterIterator (formattedString, ranges, attributes);
  }

  /*
   * The following methods are inherited from CharacterIterator and thus
   * are already documented.
   */

  public char current()
  {
    return formattedString.charAt (charIndex);
  }

  public char first()
  {
    charIndex = 0;
    attributeIndex = 0;
    return formattedString.charAt (0);
  }

  public int getBeginIndex()
  {
    return 0;
  }

  public int getEndIndex()
  {
    return formattedString.length();
  }

  public int getIndex()
  {
    return charIndex;
  }

  public char last()
  {
    charIndex = formattedString.length()-1;
    if (attributes != null)
      attributeIndex = attributes.size()-1;
    return formattedString.charAt (charIndex);
  }

  public char next()
  {
    charIndex++;
    if (charIndex >= formattedString.length())
      {
        charIndex = getEndIndex();
        return DONE;
      }
    if (attributes != null)
      {
        if (charIndex >= ranges[attributeIndex])
          attributeIndex++;
      }
    return formattedString.charAt (charIndex);
  }

  public char previous()
  {
    charIndex--;
    if (charIndex < 0)
      {
        charIndex = 0;
        return DONE;
      }

    if (attributes != null)
      {
        if (charIndex < ranges[attributeIndex])
          attributeIndex--;
      }
    return formattedString.charAt (charIndex);
  }

  public char setIndex (int position)
  {
    if (position < 0 || position > formattedString.length())
      throw new IllegalArgumentException ("position is out of range");

    charIndex = position;
    if (attributes != null)
      {
        for (attributeIndex=0;attributeIndex<attributes.size();
             attributeIndex++)
          if (ranges[attributeIndex] > charIndex)
            break;
        attributeIndex--;
      }
    if (charIndex == formattedString.length())
      return DONE;
    else
      return formattedString.charAt (charIndex);
  }

  /**
   * This method merge the specified attributes and ranges with the
   * internal tables. This method is in charge of the optimization
   * of tables. Two following sets of attributes are never the same.
   *
   * @see #FormatCharacterIterator()
   *
   * @param attributes the new array attributes to apply to the string.
   */
  public void mergeAttributes (List<Map<Attribute,Object>> attributes,
                               int[] ranges)
  {
    List<Integer> newRanges = new ArrayList<Integer>();
    List<Map<Attribute,Object>> newAttributes =
      new ArrayList<Map<Attribute,Object>>();
    int i = 0, j = 0;

    debug("merging " + attributes.size() + " attrs");

    while (i < this.ranges.length && j < ranges.length)
      {
        if (this.attributes.get(i) != null)
          {
            newAttributes.add (this.attributes.get(i));
            if (attributes.get(j) != null)
              this.attributes.get(i).putAll (attributes.get(j));
          }
        else
          {
            newAttributes.add (attributes.get(j));
          }
        if (this.ranges[i] == ranges[j])
          {
            newRanges.add (Integer.valueOf (ranges[j]));
            i++;
            j++;
          }
        else if (this.ranges[i] < ranges[j])
          {
            newRanges.add (Integer.valueOf (this.ranges[i]));
            i++;
          }
        else
          {
            newRanges.add (Integer.valueOf (ranges[j]));
            j++;
          }
     }

    if (i != this.ranges.length)
      {
        for (;i<this.ranges.length;i++)
          {
            newAttributes.add (this.attributes.get(i));
            newRanges.add (Integer.valueOf (this.ranges[i]));
          }
      }
    if (j != ranges.length)
      {
        for (;j<ranges.length;j++)
          {
            newAttributes.add (attributes.get(j));
            newRanges.add (Integer.valueOf (ranges[j]));
          }
      }

    this.attributes = newAttributes;
    this.ranges = new int[newRanges.size()];

    for (i=0;i<newRanges.size();i++)
      {
        this.ranges[i] = newRanges.get (i).intValue();
      }

    dumpTable();
  }

  /**
   * This method appends to the internal attributed string the attributed
   * string contained in the specified iterator.
   *
   * @param iterator the iterator which contains the attributed string to
   * append to this iterator.
   */
  public void append (AttributedCharacterIterator iterator)
  {
    char c = iterator.first();
    List<Integer> moreRanges = new ArrayList<Integer>();
    List<Map<Attribute,Object>> moreAttributes =
      new ArrayList<Map<Attribute,Object>>();

    do
      {
        formattedString = formattedString + String.valueOf (c);
        // TODO: Reduce the size of the output array.
        moreAttributes.add (iterator.getAttributes());
        moreRanges.add (Integer.valueOf (formattedString.length()));
        // END TOOD
        c = iterator.next();
      }
    while (c != DONE);

    List<Map<Attribute,Object>> newAttributes =
      new ArrayList<Map<Attribute,Object>>(attributes.size() + moreAttributes.size());
    int[] newRanges = new int[ranges.length + moreRanges.size()];

    newAttributes.addAll(attributes);
    newAttributes.addAll(moreAttributes);

    System.arraycopy (ranges, 0, newRanges, 0, ranges.length);
    Integer[] newRangesArray = moreRanges.toArray(new Integer[moreRanges.size()]);
    for (int i = 0; i < moreRanges.size();i++)
      newRanges[i+ranges.length] = newRangesArray[i].intValue();

    attributes = newAttributes;
    ranges = newRanges;
  }

  /**
   * This method appends an attributed string which attributes are specified
   * directly in the calling parameters.
   *
   * @param text The string to append.
   * @param localAttributes The attributes to put on this string in the
   * iterator. If it is <code>null</code> the string will simply have no
   * attributes.
   */
  public void append (String text, HashMap<? extends Attribute,? extends Object> localAttributes)
  {
    int[] newRanges = new int[ranges.length+1];
    List<Map<Attribute,Object>> newAttributes =
      new ArrayList<Map<Attribute,Object>>(attributes.size()+1);

    formattedString += text;
    newAttributes.addAll(attributes);
    System.arraycopy (ranges, 0, newRanges, 0, ranges.length);
    newRanges[ranges.length] = formattedString.length();
    newAttributes.add(new HashMap<Attribute,Object>(localAttributes));

    ranges = newRanges;
    attributes = newAttributes;
  }

  /**
   * This method appends a string without attributes. It is completely
   * equivalent to call {@link #append(String,HashMap)} with localAttributes
   * equal to <code>null</code>.
   *
   * @param text The string to append to the iterator.
   */
  public void append (String text)
  {
    append (text, null);
  }

  /**
   * This method adds a set of attributes to a range of character. The
   * bounds are always inclusive. In the case many attributes have to
   * be added it is advised to directly use {@link #mergeAttributes(java.util.List;[I}
   *
   * @param attributes Attributes to merge into the iterator.
   * @param rangeStart Lower bound of the range of characters which will receive the
   * attribute.
   * @param rangeEnd Upper bound of the range of characters which will receive the
   * attribute.
   *
   * @throws IllegalArgumentException if ranges are out of bounds.
   */
  public void addAttributes(Map<? extends Attribute,? extends Object> attributes,
                            int rangeStart, int rangeEnd)
  {
    List<Map<Attribute,Object>> mergeAttribs = new ArrayList<Map<Attribute,Object>>();
    int[] mergeRanges;

    if (rangeStart == 0)
        mergeRanges = new int[] { rangeEnd };
    else
      {
        mergeRanges = new int[] { rangeStart, rangeEnd };
        mergeAttribs.add(null);
      }
    mergeAttribs.add(new HashMap<Attribute,Object>(attributes));
    mergeAttributes(mergeAttribs, mergeRanges);
  }

  private void debug(String s)
  {
    if (DEBUG)
      System.out.println(s);
  }

  private void dumpTable()
  {
    int startRange = 0;

    if (!DEBUG)
      return;

    System.out.println("Dumping internal table:");
    for (int i = 0; i < ranges.length; i++)
      {
        System.out.print("\t" + startRange + " => " + ranges[i] + ":");
        if (attributes.get(i) == null)
          System.out.println("null");
        else
          {
            Set<Attribute> keyset = attributes.get(i).keySet();
            if (keyset != null)
              {
                Iterator<Attribute> keys = keyset.iterator();

                while (keys.hasNext())
                  System.out.print(" " + keys.next());
              }
            else
              System.out.println("keySet null");
            System.out.println();
          }
      }
    System.out.println();
    System.out.flush();
  }
}
