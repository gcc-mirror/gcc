/* CompositeName.java --
   Copyright (C) 2001, 2005, 2006  Free Software Foundation, Inc.

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


package javax.naming;

import gnu.java.lang.CPStringBuilder;

import java.io.IOException;
import java.io.ObjectInputStream;
import java.io.ObjectOutputStream;
import java.io.Serializable;
import java.util.Enumeration;
import java.util.NoSuchElementException;
import java.util.Vector;

/**
 * Represents names that may span over several namespaces. For instance,
 * the composite name http://www.gnu.org/software/classpath/index.html spans
 * over three namespaces (the protocol http, the web server location
 * (www.gnu.org) and the index.html location on the server).
 * 
 * @author Tom Tromey (tromey@redhat.com)
 */
public class CompositeName implements Name, Cloneable, Serializable
{
  private static final long serialVersionUID = 1667768148915813118L;
  
  private transient Vector<String> elts;  

  public CompositeName ()
  {
    elts = new Vector<String> ();
  }

  protected CompositeName (Enumeration<String> comps)
  {
    elts = new Vector<String> ();
    try
      {
	while (comps.hasMoreElements ())
	  elts.add (comps.nextElement ());
      }
    catch (NoSuchElementException ignore)
      {
      }
  }

  public CompositeName (String n) throws InvalidNameException
  {
    elts = new Vector<String> ();
    // Parse the string into its components.
    final char no_quote = 'x';	// Use 'x' to mean no quoting.
    char quote = no_quote;
    boolean escaped = false;
    StringBuilder new_element = new StringBuilder ();
    for (int i = 0; i < n.length (); ++i)
      {
	char c = n.charAt (i);
	if (escaped)
	  escaped = false;
	else if (c == '\\')
	  {
	    escaped = true;
	    continue;
	  }
	else if (quote != no_quote)
	  {
	    if (quote == c)
	      {
		// The quotes must surround a complete component.
		if (i + 1 < n.length () && n.charAt (i + 1) != '/')
		  throw new InvalidNameException ("close quote before end of component");
		elts.add (new_element.toString ());
		new_element.setLength (0);
		quote = no_quote;
		continue;
	      }
	    // Otherwise, fall through.
	  }
	// Quotes are only special at the start of a component.
	else if (new_element.length () == 0
		 && (c == '\'' || c == '"'))
	  {
	    quote = c;
	    continue;
	  }
	else if (c == '/')
	  {
	    elts.add (new_element.toString ());
	    new_element.setLength (0);
	    continue;
	  }

	new_element.append (c);
      }

    if (new_element.length () != 0)
      elts.add (new_element.toString ());

    // Error checking.
    if (quote != no_quote)
      throw new InvalidNameException ("unterminated quote");
    if (escaped)
      throw new InvalidNameException ("trailing escape character");
  }

  public Name add (int posn, String comp) throws InvalidNameException
  {
    elts.add (posn, comp);
    return this;
  }

  public Name add (String comp) throws InvalidNameException
  {
    elts.add (comp);
    return this;
  }

  public Name addAll (int posn, Name n) throws InvalidNameException
  {
    Enumeration<String> e = n.getAll ();
    try
      {
	while (e.hasMoreElements ())
	  {
	    elts.add (posn, e.nextElement ());
	    ++posn;
	  }
      }
    catch (NoSuchElementException ignore)
      {
      }
    return this;
  }

  public Name addAll (Name suffix) throws InvalidNameException
  {
    Enumeration<String> e = suffix.getAll ();
    try
      {
	while (e.hasMoreElements ())
	  elts.add (e.nextElement ());
      }
    catch (NoSuchElementException ignore)
      {
      }
    return this;
  }

  public Object clone ()
  {
    return new CompositeName (elts.elements ());
  }

  public int compareTo (Object obj)
  {
    if (obj == null || ! (obj instanceof CompositeName))
      throw new ClassCastException ("CompositeName.compareTo() expected CompositeName");
    CompositeName cn = (CompositeName) obj;
    int last = Math.min (cn.elts.size (), elts.size ());
    for (int i = 0; i < last; ++i)
      {
	String f = elts.get (i);
	int comp = f.compareTo (cn.elts.get (i));
	if (comp != 0)
	  return comp;
      }
    return elts.size () - cn.elts.size ();
  }

  public boolean endsWith (Name n)
  {
    if (! (n instanceof CompositeName))
      return false;
    CompositeName cn = (CompositeName) n;
    if (cn.elts.size () > elts.size ())
      return false;
    int delta = elts.size () - cn.elts.size ();
    for (int i = 0; i < cn.elts.size (); ++i)
      {
	if (! cn.elts.get (i).equals (elts.get (delta + i)))
	  return false;
      }
    return true;
  }

  public boolean equals (Object obj)
  {
    if (! (obj instanceof CompositeName))
      return false;
    CompositeName cn = (CompositeName) obj;
    return elts.equals (cn.elts);
  }

  public String get (int posn)
  {
    return elts.get (posn);
  }

  public Enumeration<String> getAll ()
  {
    return elts.elements ();
  }

  public Name getPrefix (int posn)
  {
    CompositeName cn = new CompositeName ();
    for (int i = 0; i < posn; ++i)
      cn.elts.add (elts.get (i));
    return cn;
  }

  public Name getSuffix (int posn)
  {
    if (posn > elts.size ())
      throw new ArrayIndexOutOfBoundsException (posn);
    CompositeName cn = new CompositeName ();
    for (int i = posn; i < elts.size (); ++i)
      cn.elts.add (elts.get (i));
    return cn;
  }

  public int hashCode ()
  {
    // Specified in documentation.
    int h = 0;
    for (int i = 0; i < elts.size (); ++i)
      h += elts.get (i).hashCode ();
    return h;
  }

  public boolean isEmpty ()
  {
    return elts.isEmpty ();
  }

  public Object remove (int posn) throws InvalidNameException
  {
    return elts.remove (posn);
  }

  public int size ()
  {
    return elts.size ();
  }

  public boolean startsWith (Name n)
  {
    if (! (n instanceof CompositeName))
      return false;
    CompositeName cn = (CompositeName) n;
    if (cn.elts.size () > elts.size ())
      return false;
    for (int i = 0; i < cn.elts.size (); ++i)
      {
	if (! cn.elts.get (i).equals (elts.get (i)))
	  return false;
      }
    return true;
  }

  public String toString ()
  {
    CPStringBuilder result = new CPStringBuilder ();
    for (int i = 0; i < elts.size (); ++i)
      {
	// For simplicity we choose to always quote using escapes and
	// never quotes.
	String elt = elts.get (i);
	if (i > 0
	    || (i == elts.size () - 1 && elt.equals ("")))
	  result.append ('/');
	for (int k = 0; k < elt.length (); ++k)
	  {
	    char c = elt.charAt (k);
	    // We must quote
	    //     ... a leading quote,
	    if ((k == 0 && (c == '"' || c == '\''))
		// ... an escape preceding a meta character,
		//     or at the end of a component,
		|| (c == '\\'
		    && (k == elt.length () - 1
			|| "\\'\"/".indexOf (elt.charAt (k + 1)) != -1))
		// ... or a component separator.
		|| c == '/')
	      result.append ('\\');
	    result.append (c);
	  }
      }
    return result.toString ();
  }
  
  private void readObject(ObjectInputStream s) 
    throws IOException, ClassNotFoundException
  {
    int size = s.readInt();
    elts = new Vector<String>(size);
    for (int i = 0; i < size; i++)
      elts.add((String) s.readObject());
  }

  private void writeObject(ObjectOutputStream s) throws IOException
  {
    s.writeInt(elts.size());
    for (int i = 0; i < elts.size(); i++)
      s.writeObject(elts.get(i));
  }
}
