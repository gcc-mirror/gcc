/* AttributedStringIterator.java -- Class to iterate over AttributedString
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

import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;
import java.util.Map;
import java.util.Set;

/**
  * This class implements the AttributedCharacterIterator interface.  It
  * is used by AttributedString.getIterator().
  *
  * @version 0.0
  *
  * @author Aaron M. Renn (arenn@urbanophile.com)
  */
class AttributedStringIterator implements AttributedCharacterIterator
{

  /*************************************************************************/

  /** The character iterator containing the text */
  private CharacterIterator ci;

  /** The list of attributes and ranges */
  private AttributedString.AttributeRange[] attribs;

  /**
   * The list of attributes that the user is interested in.  We may,
   * at our option, not return any other attributes.
   */
  private AttributedCharacterIterator.Attribute[] restricts;

  /*************************************************************************/

  /**
   * Creates a new instance.
   * 
   * @param sci  an iterator for the string content.
   * @param attribs  the attribute ranges.
   * @param beginIndex  the start index.
   * @param endIndex  the end index.
   * @param restricts  the attributes that the user is interested in.
   */
  AttributedStringIterator(StringCharacterIterator sci, 
                           AttributedString.AttributeRange[] attribs,
                           int beginIndex, int endIndex,
                           AttributedCharacterIterator.Attribute[] restricts)
  {
    this.ci = new StringCharacterIterator(sci, beginIndex, endIndex);
    this.attribs = attribs;
    this.restricts = restricts;
  }

  /*************************************************************************/

  // First we have a bunch of stupid redirects.  If StringCharacterIterator
  // weren't final, I just would have extended that for this class.  Alas, no.

  public Object clone()
  {
    return(ci.clone());
  }

  public char current()
  {
    return(ci.current());
  }

  public char next()
  {
    return(ci.next());
  }

  public char previous()
  {
    return(ci.previous());
  }

  public char first()
  {
    return(ci.first());
  }

  public char last()
  {
    return(ci.last());
  }

  public int getIndex()
  {
    return(ci.getIndex());
  }

  public char setIndex(int index)
  {
    return(ci.setIndex(index));  
  }

  public int getBeginIndex()
  {
    return(ci.getBeginIndex());
  }

  public int getEndIndex()
  {
    return(ci.getEndIndex());
  }

  /*
   * Here is where the AttributedCharacterIterator methods start.
   */ 

  /*************************************************************************/

  /**
   * Returns a list of all the attribute keys that are defined anywhere
   * on this string.
   */
  public Set getAllAttributeKeys()
  {
    HashSet s = new HashSet();
    if (attribs == null)
      return(s);

    for (int i = 0; i < attribs.length; i++)
    {
      if (attribs[i].beginIndex > getEndIndex()
	  || attribs[i].endIndex <= getBeginIndex())
	continue;

      Set key_set = attribs[i].attribs.keySet();
      Iterator iter = key_set.iterator();
      while (iter.hasNext())
        {
          s.add(iter.next());
        }
    }

    return(s);
  }

  /*************************************************************************/

  /**
   * Various methods that determine how far the run extends for various
   * attribute combinations.
   */

  public int getRunLimit()
  {
    return getRunLimit(getAllAttributeKeys());
  }

  public int getRunLimit(AttributedCharacterIterator.Attribute attrib)
  {
    HashSet s = new HashSet();
    s.add(attrib);
    return(getRunLimit(s));
  }

  public synchronized int getRunLimit(Set attributeSet)
  {
    if (attributeSet == null)
      return ci.getEndIndex();
    
    int current = ci.getIndex();
    int end = ci.getEndIndex();
    int limit = current;
    if (current == end) 
      return end;
    Map runValues = getAttributes();
    while (limit < end) 
    {
      Iterator iterator = attributeSet.iterator();
      while (iterator.hasNext()) 
      {
        Attribute attributeKey = (Attribute) iterator.next();
        Object v1 = runValues.get(attributeKey);
        Object v2 = getAttribute(attributeKey, limit + 1);
        boolean changed = false;
        // check for equal or both null, if NO return start
        if (v1 != null) 
          {
            changed = !v1.equals(v2);
          }
        else 
          {
            changed = (v2 != null);  
          }
        if (changed)
          return limit + 1;
      }
      // no differences, so increment limit and next and loop again
      limit++;
    }
    return end;
  }

  /*************************************************************************/

  /**
   * Various methods that determine where the run begins for various
   * attribute combinations.
   */

  /**
   * Returns the index of the first character in the run containing the current
   * character and defined by all the attributes defined for that character
   * position.
   * 
   * @return The run start index.
   */
  public int getRunStart()
  {
    return(getRunStart(getAttributes().keySet()));
  }

  /**
   * Returns the index of the first character in the run, defined by the 
   * specified attribute, that contains the current character.
   * 
   * @param attrib  the attribute (<code>null</code> permitted).
   * 
   * return The index of the first character in the run.
   */
  public int getRunStart(AttributedCharacterIterator.Attribute attrib)
  {
    if (attrib == null)
      return ci.getBeginIndex();
    HashSet s = new HashSet();
    s.add(attrib);
    return(getRunStart(s));
  }

  /**
   * Returns the index of the first character in the run, defined by the 
   * specified attribute set, that contains the current character.
   * 
   * @param attributeSet  the attribute set (<code>null</code> permitted).
   * 
   * return The index of the first character in the run.
   */
  public int getRunStart(Set attributeSet)
  {
    if (attributeSet == null)
      return ci.getBeginIndex();
    
    int current = ci.getIndex();
    int begin = ci.getBeginIndex();
    int start = current;
    if (start == begin) 
      return begin;
    Map runValues = getAttributes();
    int prev = start - 1;
    while (start > begin) 
    {
      Iterator iterator = attributeSet.iterator();
      while (iterator.hasNext()) 
      {
        Attribute attributeKey = (Attribute) iterator.next();
        Object v1 = runValues.get(attributeKey);
        Object v2 = getAttribute(attributeKey, prev);
        boolean changed = false;
        // check for equal or both null, if NO return start
        if (v1 != null) 
          {
            changed = !v1.equals(v2);
          }
        else 
          {
            changed = (v2 != null);  
          }
        if (changed)
          return start;
      }
      // no differences, so decrement start and prev and loop again
      start--;
      prev--;
    }
    return start;
  }

  /*************************************************************************/

  /**
   * Returns the value for an attribute at the specified position.  If the
   * attribute key (<code>key</code>) is <code>null</code>, the method returns
   * <code>null</code>.
   * 
   * @param key  the key (<code>null</code> permitted).
   * @param pos  the character position.
   * 
   * @return The attribute value (possibly <code>null</code>).
   */
  private Object getAttribute(AttributedCharacterIterator.Attribute key, 
          int pos)
  {
    if (attribs == null)
      return null;
    for (int i = attribs.length - 1; i >= 0; i--)
      {
        if (pos >= attribs[i].beginIndex && pos < attribs[i].endIndex)
          {
            Set keys = attribs[i].attribs.keySet();
            if (keys.contains(key)) 
              {
                return attribs[i].attribs.get(key);
              }
          }
      }
    return null;   
  }
  
  /**
   * Returns the value for an attribute at the current position.  If the
   * attribute key (<code>key</code>) is <code>null</code>, the method returns
   * <code>null</code>.
   * 
   * @param key  the key (<code>null</code> permitted).
   * 
   * @return The attribute value (possibly <code>null</code>).
   */
  public Object getAttribute(AttributedCharacterIterator.Attribute key)
  {
    return getAttribute(key, ci.getIndex());
  }

  /*************************************************************************/

  /**
   * Return a list of all the attributes and values defined for this
   * character
   */
  public Map getAttributes()
  {
    HashMap m = new HashMap();
    if (attribs == null)
      return(m);
  
    for (int i = 0; i < attribs.length; i++)
      {
         if ((ci.getIndex() >= attribs[i].beginIndex) &&
             (ci.getIndex() < attribs[i].endIndex))
           m.putAll(attribs[i].attribs);
      }

    return(m);
  }

} // class AttributedStringIterator
