/* Copyright (C) 2001 Free Software Foundation

   This file is part of libgcj.

This software is copyrighted work licensed under the terms of the
Libgcj License.  Please consult the file "LIBGCJ_LICENSE" for
details.  */

package javax.naming;

import java.io.Serializable;
import java.util.Enumeration;
import java.util.NoSuchElementException;
import java.util.Vector;

/**
 * @author Tom Tromey <tromey@redhat.com>
 * @date May 16, 2001
 *
 * FIXME: must write readObject and writeObject to conform to
 * serialization spec.
 */
public class CompositeName implements Name, Cloneable, Serializable
{
  public CompositeName ()
  {
    elts = new Vector ();
  }

  protected CompositeName (Enumeration comps)
  {
    elts = new Vector ();
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
    elts = new Vector ();
    // Parse the string into its components.
    final char no_quote = 'x';	// Use 'x' to mean no quoting.
    char quote = no_quote;
    boolean escaped = false;
    StringBuffer new_element = new StringBuffer ();
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
    Enumeration e = n.getAll ();
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
    Enumeration e = suffix.getAll ();
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
	String f = (String) elts.get (i);
	int comp = f.compareTo ((String) cn.elts.get (i));
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
    return (String) elts.get (posn);
  }

  public Enumeration getAll ()
  {
    return elts.elements ();
  }

  public Name getPrefix (int posn)
  {
    CompositeName cn = new CompositeName ();
    for (int i = 0; i < posn; ++i)
      cn.elts.add ((String) elts.get (i));
    return cn;
  }

  public Name getSuffix (int posn)
  {
    if (posn > elts.size ())
      throw new ArrayIndexOutOfBoundsException (posn);
    CompositeName cn = new CompositeName ();
    for (int i = posn; i < elts.size (); ++i)
      cn.elts.add ((String) elts.get (i));
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
    StringBuffer result = new StringBuffer ();
    for (int i = 0; i < elts.size (); ++i)
      {
	// For simplicity we choose to always quote using escapes and
	// never quotes.
	String elt = (String) elts.get (i);
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

  private transient Vector elts;
}
