/* FormatCharacter.java -- Implementation of AttributedCharacterIterator for 
   formatters.
   Copyright (C) 1998, 1999, 2000, 2001, 2003, 2004, 2005 Free Software Foundation, Inc.

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

import gnu.classpath.Configuration;

import java.text.AttributedCharacterIterator;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;
import java.util.Map;
import java.util.Set;
import java.util.Vector;

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
  private HashMap[] attributes;
  
  /**
   * This constructor builds an empty iterated strings. The attributes
   * are empty and so is the string. However you may append strings
   * and attributes to this iterator.
   */
  public FormatCharacterIterator()
  {
    formattedString = "";
    ranges = new int[0];
    attributes = new HashMap[0];
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
   *   attributes = new HashMap[2];
   * </pre>
   * <code>"he"</code> will have the attributes <code>attributes[0]</code>,
   * <code>"llo"</code> the <code>attributes[1]</code>.
   */
  public FormatCharacterIterator (String s, int[] ranges, HashMap[] attributes)
  {
    formattedString = s;
    this.ranges = ranges;
    this.attributes = attributes;
  }
  
  /* 
   * The following methods are inherited from AttributedCharacterIterator,
   * and thus are already documented. 
   */

  public Set getAllAttributeKeys()
  {
    if (attributes != null && attributes[attributeIndex] != null)
      return attributes[attributeIndex].keySet();
    else
      return new HashSet();
  }
  
  public Map getAttributes()
  {
    if (attributes != null && attributes[attributeIndex] != null)
      return attributes[attributeIndex];
    else
      return new HashMap();
  }
  
  public Object getAttribute (AttributedCharacterIterator.Attribute attrib)
  {
    if (attributes != null && attributes[attributeIndex] != null)
      return attributes[attributeIndex].get (attrib);
    else
      return null;
  }
  
  public int getRunLimit(Set reqAttrs)
  {
    if (attributes == null)
      return formattedString.length();

    int currentAttrIndex = attributeIndex;
    Set newKeys;

    do
      {
	currentAttrIndex++;
	if (currentAttrIndex == attributes.length)
	  return formattedString.length();
	if (attributes[currentAttrIndex] == null)
	  break;
	newKeys = attributes[currentAttrIndex].keySet();
      }
    while (newKeys.containsAll (reqAttrs));

    return ranges[currentAttrIndex-1];
  }
  
  public int getRunLimit (AttributedCharacterIterator.Attribute attribute) 
  {
    Set s = new HashSet();

    s.add (attribute);
    return getRunLimit (s);
  }

  public int getRunLimit()
  {
    if (attributes == null)
      return formattedString.length();
    if (attributes[attributeIndex] == null)
      {
	for (int i=attributeIndex+1;i<attributes.length;i++)
	  if (attributes[i] != null)
	    return ranges[i-1];
	return formattedString.length();
      }

    return getRunLimit (attributes[attributeIndex].keySet());
  }
  
  public int getRunStart (Set reqAttrs)
  {
    if (attributes == null)
      return formattedString.length();
  
    int currentAttrIndex = attributeIndex;
    Set newKeys = null;

    do
      {
	if (currentAttrIndex == 0)
	  return 0;

	currentAttrIndex--;
	if (attributes[currentAttrIndex] == null)
	  break;
	newKeys = attributes[currentAttrIndex].keySet();
      }
    while (newKeys.containsAll (reqAttrs));
   
    return (currentAttrIndex > 0) ? ranges[currentAttrIndex-1] : 0;
  } 
    
  public int getRunStart()
  {
    if (attributes == null)
      return 0;

    if (attributes[attributeIndex] == null)
      {
	for (int i=attributeIndex;i>0;i--)
	  if (attributes[i] != null)
	    return ranges[attributeIndex-1];
	return 0;
      }

    return getRunStart (attributes[attributeIndex].keySet());
  }
  
  public int getRunStart (AttributedCharacterIterator.Attribute attribute) 
  {
    Set s = new HashSet();
    
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
      attributeIndex = attributes.length-1;
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
	for (attributeIndex=0;attributeIndex<attributes.length;
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
  public void mergeAttributes (HashMap[] attributes, int[] ranges)
  {
    Vector new_ranges = new Vector();
    Vector new_attributes = new Vector();
    int i = 0, j = 0;

    debug("merging " + attributes.length + " attrs");
    
    while (i < this.ranges.length && j < ranges.length)
      {
	if (this.attributes[i] != null)
	  {
	    new_attributes.add (this.attributes[i]);
	    if (attributes[j] != null)
	      this.attributes[i].putAll (attributes[j]);
	  }
	else
	  {
	    new_attributes.add (attributes[j]);
	  }
	if (this.ranges[i] == ranges[j])
	  {
	    new_ranges.add (new Integer (ranges[j]));
	    i++;
	    j++;
	  }
	else if (this.ranges[i] < ranges[j])
	  {
	    new_ranges.add (new Integer (this.ranges[i]));
	    i++;
	  }
	else
	  {
	    new_ranges.add (new Integer (ranges[j]));
	    j++;
	  }
     }
    
    if (i != this.ranges.length)
      {
	for (;i<this.ranges.length;i++)
	  {
	    new_attributes.add (this.attributes[i]);
	    new_ranges.add (new Integer (this.ranges[i]));
	  }
      }
    if (j != ranges.length)
      {
	for (;j<ranges.length;j++)
	  {
	    new_attributes.add (attributes[j]);
	    new_ranges.add (new Integer (ranges[j]));
	  }
      }

    this.attributes = new HashMap[new_attributes.size()];
    this.ranges = new int[new_ranges.size()];
    System.arraycopy (new_attributes.toArray(), 0, this.attributes,
		      0, this.attributes.length);

    for (i=0;i<new_ranges.size();i++)
      {
	this.ranges[i] = ((Integer)new_ranges.elementAt (i)).intValue();
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
    Vector more_ranges = new Vector();
    Vector more_attributes = new Vector();

    do
      {
	formattedString = formattedString + String.valueOf (c);
	// TODO: Reduce the size of the output array.
	more_attributes.add (iterator.getAttributes());
	more_ranges.add (new Integer (formattedString.length()));
	// END TOOD
	c = iterator.next();
      } 
    while (c != DONE);

    HashMap[] new_attributes = new HashMap[attributes.length
					   + more_attributes.size()];
    int[] new_ranges = new int[ranges.length + more_ranges.size()];
    
    System.arraycopy (attributes, 0, new_attributes, 0, attributes.length);
    System.arraycopy (more_attributes.toArray(), 0, new_attributes,
		      attributes.length, more_attributes.size());

    System.arraycopy (ranges, 0, new_ranges, 0, ranges.length);
    Object[] new_ranges_array = more_ranges.toArray();
    for (int i = 0; i < more_ranges.size();i++)
      new_ranges[i+ranges.length] = ((Integer) new_ranges_array[i]).intValue();

    attributes = new_attributes;
    ranges = new_ranges;
  }

  /**
   * This method appends an attributed string which attributes are specified
   * directly in the calling parameters.
   *
   * @param text The string to append.
   * @param local_attributes The attributes to put on this string in the
   * iterator. If it is <code>null</code> the string will simply have no
   * attributes.
   */
  public void append (String text, HashMap local_attributes)
  {
    int[] new_ranges = new int[ranges.length+1];
    HashMap[] new_attributes = new HashMap[attributes.length+1];

    formattedString += text;
    System.arraycopy (attributes, 0, new_attributes, 0, attributes.length);
    System.arraycopy (ranges, 0, new_ranges, 0, ranges.length);
    new_ranges[ranges.length] = formattedString.length();
    new_attributes[attributes.length] = local_attributes;

    ranges = new_ranges;
    attributes = new_attributes;
  }  

  /**
   * This method appends a string without attributes. It is completely
   * equivalent to call {@link #append(String,HashMap)} with local_attributes
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
   * be added it is advised to directly use {@link #mergeAttributes([Ljava.util.HashMap;[I}
   *
   * @param attributes Attributes to merge into the iterator.
   * @param range_start Lower bound of the range of characters which will receive the
   * attribute.
   * @param range_end Upper bound of the range of characters which will receive the
   * attribute. 
   *
   * @throws IllegalArgumentException if ranges are out of bounds.
   */
  public void addAttributes(HashMap attributes, int range_start, int range_end)
  {
    if (range_start == 0)
      mergeAttributes(new HashMap[] { attributes }, new int[] { range_end });
    else
      mergeAttributes(new HashMap[] { null, attributes }, new int[] { range_start, range_end });
  }

  private void debug(String s)
  {
    if (Configuration.DEBUG)
      System.out.println(s);
  }

  private void dumpTable()
  {
    int start_range = 0;
    
    if (!Configuration.DEBUG)
      return;

    System.out.println("Dumping internal table:");
    for (int i = 0; i < ranges.length; i++)
      {
	System.out.print("\t" + start_range + " => " + ranges[i] + ":");
	if (attributes[i] == null)
	  System.out.println("null");
	else
	  {
	    Set keyset = attributes[i].keySet();
	    if (keyset != null)
	      {
		Iterator keys = keyset.iterator();
		
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
