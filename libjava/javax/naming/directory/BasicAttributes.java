/* Copyright (C) 2000, 2001 Free Software Foundation

   This file is part of libgcj.

This software is copyrighted work licensed under the terms of the
Libgcj License.  Please consult the file "LIBGCJ_LICENSE" for
details.  */

package javax.naming.directory;

import javax.naming.*;
import java.util.*;

/**
 * @author Tom Tromey <tromey@redhat.com>
 * @date June 22, 2001
 */
public class BasicAttributes implements Attributes
{
  public BasicAttributes ()
  {
    this (false);
  }

  public BasicAttributes (boolean ignoreCase)
  {
    this.ignoreCase = ignoreCase;
    this.attributes = new Vector ();
  }

  public BasicAttributes (String attrID, Object val)
  {
    this (attrID, val, false);
  }

  public BasicAttributes (String attrID, Object val, boolean ignoreCase)
  {
    this.ignoreCase = ignoreCase;
    attributes = new Vector ();
    attributes.add (new BasicAttribute (attrID, val));
  }

  public Object clone ()
  {
    // Slightly inefficient as we make a garbage Vector here.
    BasicAttributes ba = new BasicAttributes (ignoreCase);
    ba.attributes = (Vector) attributes.clone ();
    return ba;
  }

  public boolean equals (Object obj)
  {
    if (! (obj instanceof BasicAttributes))
      return false;
    BasicAttributes b = (BasicAttributes) obj;
    if (ignoreCase != b.ignoreCase
	|| attributes.size () != b.attributes.size ())
      return false;

    // Does order matter?
    for (int i = 0; i < attributes.size (); ++i)
      {
	if (! attributes.get (i).equals (b.attributes.get (i)))
	  return false;
      }

    return true;
  }

  public Attribute get (String attrID)
  {
    for (int i = 0; i < attributes.size (); ++i)
      {
	Attribute at = (Attribute) attributes.get (i);
	if ((ignoreCase && attrID.equalsIgnoreCase (at.getID ()))
	    || (! ignoreCase && attrID.equals (at.getID ())))
	  return at;
      }

    return null;
  }

  public NamingEnumeration getAll ()
  {
    return new BasicAttributesEnumeration (false);
  }

  public NamingEnumeration getIDs ()
  {
    return new BasicAttributesEnumeration (true);
  }

  public int hashCode ()
  {
    int val = 0;
    for (int i = 0; i < attributes.size (); ++i)
      val += attributes.get (i).hashCode ();
    return val;
  }

  public boolean isCaseIgnored ()
  {
    return ignoreCase;
  }

  public Attribute put (Attribute attr)
  {
    Attribute r = remove (attr.getID ());
    attributes.add (attr);
    return r;
  }

  public Attribute put (String attrID, Object val)
  {
    return put (new BasicAttribute (attrID, val));
  }

  public Attribute remove (String attrID)
  {
    for (int i = 0; i < attributes.size (); ++i)
      {
	Attribute at = (Attribute) attributes.get (i);
	if ((ignoreCase && attrID.equalsIgnoreCase (at.getID ()))
	    || (! ignoreCase && attrID.equals (at.getID ())))
	  {
	    attributes.remove (i);
	    return at;
	  }
      }

    return null;
  }

  public int size ()
  {
    return attributes.size ();
  }

  public String toString ()
  {
    String r = "";
    for (int i = 0; i < attributes.size (); ++i)
      {
	if (i > 0)
	  r += "; ";
	r += attributes.get (i).toString ();
      }
    return r;
  }

  // This is set by the serialization spec.
  private boolean ignoreCase;
  private transient Vector attributes;

  // Used when enumerating.
  private class BasicAttributesEnumeration implements NamingEnumeration
  {
    int where = -1;
    boolean id;

    public BasicAttributesEnumeration (boolean id)
    {
      this.id = id;
    }

    public void close () throws NamingException
    {
    }

    public boolean hasMore () throws NamingException
    {
      return hasMoreElements ();
    }

    public Object next () throws NamingException
    {
      return nextElement ();
    }

    public boolean hasMoreElements ()
    {
      return where < attributes.size ();
    }

    public Object nextElement () throws NoSuchElementException
    {
      if (where + 1 >= attributes.size ())
	throw new NoSuchElementException ("no more elements");
      ++where;
      Attribute at = (Attribute) attributes.get (where);
      return id ? (Object) at.getID () : (Object) at;
    }
  }
}
