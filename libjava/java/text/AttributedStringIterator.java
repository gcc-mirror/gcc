/* AttributedStringIterator.java -- Class to iterate over AttributedString
   Copyright (C) 1998, 1999 Free Software Foundation, Inc.

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


package java.text;

import java.util.Set;
import java.util.HashSet;
import java.util.Map;
import java.util.HashMap;
import java.util.Iterator;

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

/**
  * Instance Variables
  */

/**
  * The character iterator containing the text
  */
private CharacterIterator ci;

/**
  * The list of attributes and ranges
  */
private AttributedString.AttributeRange[] attribs;

/**
  * The list of attributes that the user is interested in.  We may,
  * at our option, not return any other attributes.
  */
private AttributedCharacterIterator.Attribute[] restricts;

/*************************************************************************/

/*
 * Constructors
 */

AttributedStringIterator(StringCharacterIterator sci, 
                         AttributedString.AttributeRange[] attribs,
                         int begin_index, int end_index,
                         AttributedCharacterIterator.Attribute[] restricts)
{
  this.ci = new StringCharacterIterator(sci, begin_index, end_index);
  this.attribs = attribs;
  this.restricts = restricts;
}

/*************************************************************************/

/*
 * Instance Methods
 */

// First we have a bunch of stupid redirects.  If StringCharacterIterator
// weren't final, I just would have extended that for this class.  Alas, no.

public Object
clone()
{
  return(ci.clone());
}

public char
current()
{
  return(ci.current());
}

public char
next()
{
  return(ci.next());
}

public char
previous()
{
  return(ci.previous());
}

public char
first()
{
  return(ci.first());
}

public char
last()
{
  return(ci.last());
}

public int
getIndex()
{
  return(ci.getIndex());
}

public char
setIndex(int index)
{
  return(ci.setIndex(index));
}

public int
getBeginIndex()
{
  return(ci.getBeginIndex());
}

public int
getEndIndex()
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
public Set
getAllAttributeKeys()
{
  HashSet s = new HashSet();
  if (attribs == null)
    return(s);

  for (int i = 0; i < attribs.length; i++)
    {
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

public int
getRunLimit()
{
  return(getRunLimit(getAttributes().keySet()));
}

public int
getRunLimit(AttributedCharacterIterator.Attribute attrib)
{
  HashSet s = new HashSet();
  s.add(attrib);

  return(getRunLimit(s));
}

public synchronized int
getRunLimit(Set attribute_set)
{
  int orig_index = ci.getIndex();
  int run_limit;

  do  
    {
      run_limit = ci.getIndex();

      Map attribute_map = getAttributes();

      boolean found = false;
      Iterator iter = attribute_set.iterator();
      while(iter.hasNext()) 
        if (!attribute_map.containsKey(iter.next()))
          {
            found = true;
            break;
          }

      if (found)
        break;
    }
  while (ci.next() != CharacterIterator.DONE);

  boolean hit_end = (ci.previous() == CharacterIterator.DONE);

  ci.setIndex(orig_index);

  if (run_limit == orig_index)
    return(-1); // No characters match the given attributes
//  else if (!hit_end)
//    --run_limit;

  return(run_limit); 
}

/*************************************************************************/

/**
  * Various methods that determine where the run begins for various
  * attribute combinations.
  */

public int
getRunStart()
{
  return(getRunStart(getAttributes().keySet()));
}

public int
getRunStart(AttributedCharacterIterator.Attribute attrib)
{
  HashSet s = new HashSet();
  s.add(attrib);

  return(getRunStart(s));
}

public int
getRunStart(Set attribute_set)
{
  int orig_index = ci.getIndex();
  int run_start;

  do  
    {
      run_start = ci.getIndex();

      Map attribute_map = getAttributes();

      Iterator iter = attribute_set.iterator();
      while(iter.hasNext())
        if (!attribute_map.containsKey(iter.next()))
          break;

      if (iter.hasNext())
        break;
    }
  while (ci.previous() != CharacterIterator.DONE);

  boolean hit_beginning = (ci.previous() == CharacterIterator.DONE);

  ci.setIndex(orig_index);

  if (run_start == orig_index)
    return(-1); // No characters match the given attributes
  else if (!hit_beginning)
    ++run_start;

  return(run_start); 
}

/*************************************************************************/

public Object
getAttribute(AttributedCharacterIterator.Attribute attrib)
{
  if (attribs == null)
    return(null);

  for (int i = 0; i < attribs.length; i++)
    {
      Set key_set = attribs[i].attribs.keySet();
      Iterator iter = key_set.iterator();
      while (iter.hasNext())
        {
          Object obj = iter.next();

          // Check for attribute match and range match
          if (obj.equals(attrib))
            if ((ci.getIndex() >= attribs[i].begin_index) &&
                (ci.getIndex() <= attribs[i].end_index))
              return(attribs[i].attribs.get(obj));
        }
    }

  return(null);
}

/*************************************************************************/

/**
  * Return a list of all the attributes and values defined for this
  * character
  */
public Map
getAttributes()
{
  HashMap m = new HashMap();
  if (attribs == null)
    return(m);
  
  for (int i = 0; i < attribs.length; i++)
    {
       if ((ci.getIndex() >= attribs[i].begin_index) &&
           (ci.getIndex() <= attribs[i].end_index))
         m.putAll(attribs[i].attribs);
    }

  return(m);
}

} // class AttributedStringIterator

