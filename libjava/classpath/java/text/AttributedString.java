/* AttributedString.java -- Models text with attributes
   Copyright (C) 1998, 1999, 2004, 2005, 2006, Free Software Foundation, Inc.

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

import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.Hashtable;
import java.util.Iterator;
import java.util.Map;
import java.util.Set;

/**
 * This class models a <code>String</code> with attributes over various
 * subranges of the string.  It allows applications to access this 
 * information via the <code>AttributedCharacterIterator</code> interface.
 * 
 * @since 1.2
 *
 * @author Aaron M. Renn (arenn@urbanophile.com)
 * @since 1.2
 */
public class AttributedString
{

  /** 
   * The attributes and ranges of text over which those attributes apply. 
   */
  final class AttributeRange
  {

    /** A Map of the attributes */
    Map attribs;

    /** The beginning index of the attributes */
    int beginIndex;

    /** The ending index of the attributes */
    int endIndex;

    /**
     * Creates a new attribute range.
     * 
     * @param attribs  the attributes.
     * @param beginIndex  the start index.
     * @param endIndex  the end index.
     */
    AttributeRange(Map attribs, int beginIndex, int endIndex) 
    {
      this.attribs = attribs;
      this.beginIndex = beginIndex;
      this.endIndex = endIndex;
    }

  } // Inner class AttributeRange

  /** The string we are representing. */
  private StringCharacterIterator sci;

  /** The attribute information */
  private AttributeRange[] attribs;

  /**
   * Creates a new instance of <code>AttributedString</code>
   * that represents the specified <code>String</code> with no attributes.
   *
   * @param str The <code>String</code> to be attributed (<code>null</code> not
   *            permitted).
   * 
   * @throws NullPointerException if <code>str</code> is <code>null</code>.
   */
  public AttributedString(String str)
  {
    sci = new StringCharacterIterator(str);
    attribs = new AttributeRange[0];
  }

  /**
   * Creates a new instance of <code>AttributedString</code>
   * that represents that specified <code>String</code> with the specified
   * attributes over the entire length of the <code>String</code>.
   *
   * @param str The <code>String</code> to be attributed.
   * @param attributes The attribute list.
   */
  public AttributedString(String str,
                          Map<? extends AttributedCharacterIterator.Attribute, ?> attributes)
  {
    this(str);

    attribs = new AttributeRange[1];
    attribs[0] = new AttributeRange(attributes, 0, str.length());
  }

  /**
   * Initializes a new instance of <code>AttributedString</code>
   * that will use the text and attribute information from the specified
   * <code>AttributedCharacterIterator</code>.
   *
   * @param aci The <code>AttributedCharacterIterator</code> containing the 
   *            text and attribute information (<code>null</code> not 
   *            permitted).
   * 
   * @throws NullPointerException if <code>aci</code> is <code>null</code>.
   */
  public AttributedString(AttributedCharacterIterator aci)
  {
    this(aci, aci.getBeginIndex(), aci.getEndIndex(), null);
  }

  /**
   * Initializes a new instance of <code>AttributedString</code>
   * that will use the text and attribute information from the specified
   * subrange of the specified <code>AttributedCharacterIterator</code>.
   *
   * @param aci The <code>AttributedCharacterIterator</code> containing the 
   *            text and attribute information.
   * @param beginIndex The beginning index of the text subrange.
   * @param endIndex The ending index of the text subrange.
   */
  public AttributedString(AttributedCharacterIterator aci, int beginIndex,
                          int endIndex)
  {
    this(aci, beginIndex, endIndex, null);
  }

  /**
   * Initializes a new instance of <code>AttributedString</code>
   * that will use the text and attribute information from the specified
   * subrange of the specified <code>AttributedCharacterIterator</code>.
   * Only attributes from the source iterator that are present in the
   * specified array of attributes will be included in the attribute list
   * for this object.
   *
   * @param aci The <code>AttributedCharacterIterator</code> containing the 
   *            text and attribute information.
   * @param begin The beginning index of the text subrange.
   * @param end The ending index of the text subrange.
   * @param attributes A list of attributes to include from the iterator, or 
   *                   <code>null</code> to include all attributes.
   */
  public AttributedString(AttributedCharacterIterator aci, int begin, int end, 
                          AttributedCharacterIterator.Attribute[] attributes)
  {
    // Validate some arguments
    if ((begin < 0) || (end < begin) || end > aci.getEndIndex())
      throw new IllegalArgumentException("Bad index values");

    StringBuffer sb = new StringBuffer("");

    // Get the valid attribute list
    Set allAttribs = aci.getAllAttributeKeys();
    if (attributes != null)
      allAttribs.retainAll(Arrays.asList(attributes));

    // Loop through and extract the attributes
    char c = aci.setIndex(begin);

    ArrayList accum = new ArrayList();
    do
      { 
        sb.append(c);

        Iterator iter = allAttribs.iterator();
        while(iter.hasNext())
          {
            Object obj = iter.next();

            // What should we do if this is not true?
            if (!(obj instanceof AttributedCharacterIterator.Attribute))
              continue;

            AttributedCharacterIterator.Attribute attrib = 
              (AttributedCharacterIterator.Attribute)obj;

            // Make sure the attribute is defined.
            Object attribObj = aci.getAttribute(attrib);
            if (attribObj == null)
              continue;
            int rl = aci.getRunLimit(attrib);
            if (rl > end)
              rl = end;
            rl -= begin;

            // Check to see if we already processed this one
            int rs = aci.getRunStart(attrib);
            if ((rs < aci.getIndex()) && (aci.getIndex() != begin))
              continue;

            // If the attribute run starts before the beginning index, we
            // need to junk it if it is an Annotation.
            rs -= begin;
            if (rs < 0)
              {
                if (attribObj instanceof Annotation)
                   continue;

                rs = 0;
              }

            // Create a map object.  Yes this will only contain one attribute
            Map newMap = new Hashtable();
            newMap.put(attrib, attribObj);

            // Add it to the attribute list.
            accum.add(new AttributeRange(newMap, rs, rl));
          }

        c = aci.next();
      }
    while( aci.getIndex() < end );

    attribs = new AttributeRange[accum.size()];
    attribs = (AttributeRange[]) accum.toArray(attribs);

    sci = new StringCharacterIterator(sb.toString());
  }

  /**
   * Adds a new attribute that will cover the entire string.
   *
   * @param attrib The attribute to add.
   * @param value The value of the attribute.
   */
  public void addAttribute(AttributedCharacterIterator.Attribute attrib, 
          Object value)
  {
    addAttribute(attrib, value, 0, sci.getEndIndex());
  }

  /**
   * Adds a new attribute that will cover the specified subrange
   * of the string.
   *
   * @param attrib The attribute to add.
   * @param value The value of the attribute, which may be <code>null</code>.
   * @param begin The beginning index of the subrange.
   * @param end The ending index of the subrange.
   *
   * @exception IllegalArgumentException If attribute is <code>null</code> or 
   *            the subrange is not valid.
   */
  public void addAttribute(AttributedCharacterIterator.Attribute attrib, 
          Object value, int begin, int end)
  {
    if (attrib == null)
      throw new IllegalArgumentException("null attribute");
    if (end <= begin)
      throw new IllegalArgumentException("Requires end > begin");
    HashMap hm = new HashMap();
    hm.put(attrib, value);

    addAttributes(hm, begin, end);
  }

  /**
   * Adds all of the attributes in the specified list to the
   * specified subrange of the string.
   *
   * @param attributes The list of attributes.
   * @param beginIndex The beginning index.
   * @param endIndex The ending index
   *
   * @throws NullPointerException if <code>attributes</code> is 
   *         <code>null</code>.
   * @throws IllegalArgumentException if the subrange is not valid.
   */
  public void addAttributes(Map<? extends AttributedCharacterIterator.Attribute, ?> attributes,
			    int beginIndex, int endIndex)
  {
    if (attributes == null)
      throw new NullPointerException("null attribute");

    if ((beginIndex < 0) || (endIndex > sci.getEndIndex()) ||
        (endIndex <= beginIndex))
      throw new IllegalArgumentException("bad range");

    AttributeRange[] new_list = new AttributeRange[attribs.length + 1];
    System.arraycopy(attribs, 0, new_list, 0, attribs.length);
    attribs = new_list;
    attribs[attribs.length - 1] = new AttributeRange(attributes, beginIndex, 
                                                     endIndex);
  } 

  /**
   * Returns an <code>AttributedCharacterIterator</code> that 
   * will iterate over the entire string.
   *
   * @return An <code>AttributedCharacterIterator</code> for the entire string.
   */
  public AttributedCharacterIterator getIterator()
  {
    return(new AttributedStringIterator(sci, attribs, 0, sci.getEndIndex(), 
            null));
  }

  /**
   * Returns an <code>AttributedCharacterIterator</code> that
   * will iterate over the entire string.  This iterator will return information
   * about the list of attributes in the specified array.  Attributes not in
   * the array may or may not be returned by the iterator.  If the specified
   * array is <code>null</code>, all attributes will be returned.
   *
   * @param attributes A list of attributes to include in the returned iterator.
   *
   * @return An <code>AttributedCharacterIterator</code> for this string.
   */
  public AttributedCharacterIterator getIterator(
          AttributedCharacterIterator.Attribute[] attributes)
  {
    return(getIterator(attributes, 0, sci.getEndIndex()));
  }

  /**
   * Returns an <code>AttributedCharacterIterator</code> that
   * will iterate over the specified subrange.  This iterator will return 
   * information about the list of attributes in the specified array.  
   * Attributes not in the array may or may not be returned by the iterator.  
   * If the specified array is <code>null</code>, all attributes will be 
   * returned.  
   *
   * @param attributes A list of attributes to include in the returned iterator.
   * @param beginIndex The beginning index of the subrange.
   * @param endIndex The ending index of the subrange.
   *
   * @return An <code>AttributedCharacterIterator</code> for this string.
   */
  public AttributedCharacterIterator getIterator(
          AttributedCharacterIterator.Attribute[] attributes, 
          int beginIndex, int endIndex)
  {
    if ((beginIndex < 0) || (endIndex > sci.getEndIndex()) ||
        (endIndex < beginIndex))
      throw new IllegalArgumentException("bad range");

    return(new AttributedStringIterator(sci, attribs, beginIndex, endIndex,
                                        attributes));
  }

} // class AttributedString
