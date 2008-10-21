/* CompoundName.java --
   Copyright (C) 2001, 2004, 2005  Free Software Foundation, Inc.

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
import java.util.Properties;
import java.util.Vector;

/**
 * Represents hierarchical names from the single namespace. For instance,
 * the path /home/audriusa/classpath/file.txt is the compound name, using
 * the filesystem namespace. 
 * 
 * @author Tom Tromey (tromey@redhat.com)
 * @date May 16, 2001
 *
 * FIXME: this class is underspecified.  For instance, the `flat'
 * direction is never described.  If it means that the CompoundName
 * can only have a single element, then the Enumeration-based
 * constructor ought to throw InvalidNameException.
 *
 * @since 1.3
 */
public class CompoundName implements Name, Cloneable, Serializable
{
  private static final long serialVersionUID = 3513100557083972036L;

  private CompoundName (Properties syntax)
  {
    elts = new Vector<String> ();
    mySyntax = syntax;
    initializeSyntax ();
  }

  protected CompoundName (Enumeration<String> comps, Properties syntax)
  {
    elts = new Vector<String> ();
    mySyntax = syntax;
    initializeSyntax ();
    try
      {
	while (comps.hasMoreElements ())
	  elts.add (comps.nextElement ());
      }
    catch (NoSuchElementException ignore)
      {
      }
  }

  public CompoundName (String n, Properties syntax)
    throws InvalidNameException
  {
    elts = new Vector<String> ();
    mySyntax = syntax;
    initializeSyntax ();

    StringBuilder new_element = new StringBuilder ();
    int i = 0;
    // QUOTE==null means no quoting right now.  When it is set it is
    // the value of the closing quote.
    String quote = null;
    while (i < n.length ())
      {
	String special = isSpecial (n, i);

	if (special == escape && escape != null)
	  {
	    if (n.length () == i + special.length ())
	      {
		// A trailing escape is treated as itself.
		new_element.append (special);
		i += special.length ();
	      }
	    else
	      {
		String eSpecial = isSpecial (n, i + special.length ());
		if (eSpecial != null)
		  {
		    // Treat the escape as an escape.
		    new_element.append (eSpecial);
		    i += special.length () + eSpecial.length ();
		  }
		else
		  {
		    // Treat the escape as itself.
		    new_element.append (special);
		    i += special.length ();
		  }
		continue;
	      }
	  }
	else if (quote != null)
	  {
	    // It is safe to use == here.
	    if (quote == special)
	      {
		// Quotes must surround a complete component.
		if (i + quote.length () < n.length ()
		    && ! n.startsWith (separator, i + quote.length ()))
		  throw new InvalidNameException ("close quote before end of component");
		elts.add (new_element.toString ());
		new_element.setLength (0);
		i += quote.length ();
		quote = null;
		continue;
	      }
	    // Otherwise, fall through.
	  }
	// Quotes are only special at the start of a component.
	else if (new_element.length () == 0
		 && special == beginQuote
		 && beginQuote != null)
	  {
	    quote = endQuote;
	    i += special.length ();
	    continue;
	  }
	else if (new_element.length () == 0
		 && special == beginQuote2
		 && beginQuote2 != null)
	  {
	    quote = endQuote2;
	    i += special.length ();
	    continue;
	  }
	else if (direction != FLAT && special == separator)
	  {
	    elts.add (new_element.toString ());
	    new_element.setLength (0);
	    i += special.length ();
	    continue;
	  }

	// Nothing in particular, so try the next character.
	new_element.append (n.charAt (i));
	++i;
      }

    if (new_element.length () != 0)
      elts.add (new_element.toString ());

    if (direction == RIGHT_TO_LEFT)
      {
	// Reverse the order of the elements.
	int len = elts.size ();
	for (i = 0; i < len / 2; ++i)
	  {
	    String t = elts.set (i, elts.get (len - i - 1));
	    elts.set (len - i - 1, t);
	  }
      }

    // Error checking.
    if (quote != null)
      throw new InvalidNameException ("unterminated quote");
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
    return new CompoundName (elts.elements (), mySyntax);
  }

  public int compareTo (Object obj)
  {
    if (! (obj instanceof CompoundName))
      throw new ClassCastException ("CompoundName.compareTo() expected CompoundName");
    CompoundName cn = (CompoundName) obj;
    int last = Math.min (cn.elts.size (), elts.size ());
    for (int i = 0; i < last; ++i)
      {
	String f = canonicalize (elts.get (i));
	int comp = f.compareTo (canonicalize (cn.elts.get (i)));
	if (comp != 0)
	  return comp;
      }
    return elts.size () - cn.elts.size ();
  }

  public boolean endsWith (Name n)
  {
    if (! (n instanceof CompoundName))
      return false;
    CompoundName cn = (CompoundName) n;
    if (cn.elts.size () > elts.size ())
      return false;
    int delta = elts.size () - cn.elts.size ();
    for (int i = 0; i < cn.elts.size (); ++i)
      {
	String f = canonicalize (elts.get (delta + i));
	if (! f.equals (canonicalize (cn.elts.get (i))))
	  return false;
      }
    return true;
  }

  public boolean equals (Object obj)
  {
    if (! (obj instanceof CompoundName))
      return false;
    return compareTo (obj) == 0;
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
    CompoundName cn = new CompoundName (mySyntax);
    for (int i = 0; i < posn; ++i)
      cn.elts.add (elts.get (i));
    return cn;
  }

  public Name getSuffix (int posn)
  {
    if (posn > elts.size ())
      throw new ArrayIndexOutOfBoundsException (posn);
    CompoundName cn = new CompoundName (mySyntax);
    for (int i = posn; i < elts.size (); ++i)
      cn.elts.add (elts.get (i));
    return cn;
  }

  public int hashCode ()
  {
    int h = 0;
    for (int i = 0; i < elts.size (); ++i)
      h += canonicalize (elts.get (i)).hashCode ();
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
    if (! (n instanceof CompoundName))
      return false;
    CompoundName cn = (CompoundName) n;
    if (cn.elts.size () > elts.size ())
      return false;
    for (int i = 0; i < cn.elts.size (); ++i)
      {
	String f = canonicalize (elts.get (i));
	if (! f.equals (canonicalize (cn.elts.get (i))))
	  return false;
      }
    return true;
  }

  // If ELEMENT starts with some meta-sequence at OFFSET, then return
  // the string representing the meta-sequence.  Otherwise return
  // null.
  private String isSpecial (String element, int offset)
  {
    String special = null;
    if (separator != null && element.startsWith (separator, offset))
      special = separator;
    else if (escape != null && element.startsWith (escape, offset))
      special = escape;
    else if (beginQuote != null && element.startsWith (beginQuote, offset))
      special = beginQuote;
    else if (endQuote != null && element.startsWith (endQuote, offset))
      special = endQuote;
    else if (beginQuote2 != null
	     && element.startsWith (beginQuote2, offset))
      special = beginQuote2;
    else if (endQuote2 != null && element.startsWith (endQuote2, offset))
      special = endQuote2;

    return special;
  }

  public String toString ()
  {
    CPStringBuilder result = new CPStringBuilder ();
    int size = elts.size ();
    for (int i = 0; i < size; ++i)
      {
	// Find the appropriate element.  FIXME: not clear what FLAT
	// means.
	int offset = (direction == RIGHT_TO_LEFT) ? (size - i - 1) : i;
	String element = elts.get (offset);
	if (i > 0
	    || (i == size - 1 && element.equals ("")))
	  result.append (separator);

	int k = 0;
	while (k < element.length ())
	  {
	    String special = isSpecial (element, k);
	    if (special != null)
	      {
		result.append (escape);
		result.append (special);
		k += special.length ();
	      }
	    else
	      {
		result.append (element.charAt (k));
		++k;
	      }
	  }
      }

    return result.toString ();
  }

  // This canonicalizes a String, based on the syntax, for comparison
  // or other similar purposes.
  private String canonicalize (String element)
  {
    String ret = element;

    if (ignoreCase)
      ret = ret.toLowerCase ();

    if (trimBlanks)
      {
	int first = 0;
	while (first < ret.length ()
	       && Character.isWhitespace (ret.charAt (first)))
	  ++first;

	int last = ret.length () - 1;
	while (last >= first
	       && Character.isWhitespace (ret.charAt (last)))
	  --last;

	ret = ret.substring (first, last);
      }

    return ret;
  }

  // This initializes all the syntax variables.  This seems easier
  // than re-querying the properties every time.  We're allowed to do
  // this because the spec says that subclasses should consider the
  // syntax as being read-only.
  private void initializeSyntax ()
  {
    String t = mySyntax.getProperty ("jndi.syntax.direction", "flat");
    if (t.equals ("right_to_left"))
      this.direction = RIGHT_TO_LEFT;
    else if (t.equals ("left_to_right"))
      this.direction = LEFT_TO_RIGHT;
    else
      {
	// If we don't recognize it, default to flat.
	this.direction = FLAT;
      }

    // This is required unless the direction is FLAT.  Unfortunately
    // there is no way to report this error.
    this.separator = mySyntax.getProperty ("jndi.syntax.separator", "");

    this.ignoreCase
      = Boolean.valueOf (mySyntax.getProperty ("jndi.syntax.ignorecase",
					       "false")).booleanValue ();
    this.escape = mySyntax.getProperty ("jndi.syntax.escape", null);
    this.beginQuote = mySyntax.getProperty ("jndi.syntax.beginquote", null);
    this.endQuote = mySyntax.getProperty ("jndi.syntax.endquote",
					  this.beginQuote);
    this.beginQuote2 = mySyntax.getProperty ("jndi.syntax.beginquote2",
					     null);
    this.endQuote2 = mySyntax.getProperty ("jndi.syntax.endquote2",
					   this.beginQuote2);
    this.trimBlanks
      = Boolean.valueOf (mySyntax.getProperty ("jndi.syntax.trimblanks",
					       "false")).booleanValue ();
  }

  private void readObject(ObjectInputStream s)
    throws IOException, ClassNotFoundException
  {
    mySyntax = (Properties) s.readObject();
    int count = s.readInt();
    elts = new Vector<String>(count);
    for (int i = 0; i < count; i++)
      elts.addElement((String) s.readObject());
  }

  private void writeObject(ObjectOutputStream s)
    throws IOException
  {
    s.writeObject(mySyntax);
    s.writeInt(elts.size());
    for (int i = 0; i < elts.size(); i++)
        s.writeObject(elts.elementAt(i));
  }

  // The spec specifies this but does not document it in any way (it
  // is a package-private class).  It is useless as far as I can tell.
  // So we ignore it.
  // protected transient NameImpl impl;
  protected transient Properties mySyntax;

  // The actual elements.
  private transient Vector<String> elts;

  // The following are all used for syntax.
  private transient int direction;
  private transient String separator;
  private transient boolean ignoreCase;
  private transient String escape;
  private transient String beginQuote;
  private transient String endQuote;
  private transient String beginQuote2;
  private transient String endQuote2;
  private transient boolean trimBlanks;
  // We didn't need these for parsing, so they are gone.
  // private transient String avaSeparator;
  // private transient String typevalSeparator;

  private static final int RIGHT_TO_LEFT = -1;
  private static final int LEFT_TO_RIGHT = 1;
  private static final int FLAT = 0;
}
