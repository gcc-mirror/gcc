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
 * @date June 20, 2001
 */
public class BasicAttribute implements Attribute
{
  /** The ID of this attribute.  */
  protected String attrID;
  /** True if this attribute's values are ordered.  */
  protected boolean ordered;
  /** Values for this attribute.  */
  protected transient Vector values;

  // Used by cloning.
  private BasicAttribute ()
  {
  }

  public BasicAttribute (String id)
  {
    this (id, false);
  }

  public BasicAttribute (String id, boolean ordered)
  {
    attrID = id;
    this.ordered = ordered;
    values = new Vector ();
  }

  public BasicAttribute (String id, Object value)
  {
    this (id, value, false);
  }

  public BasicAttribute (String id, Object value, boolean ordered)
  {
    attrID = id;
    this.ordered = ordered;
    values = new Vector ();
    values.add (value);
  }

  public void add (int index, Object val)
  {
    if (! ordered && contains (val))
      throw new IllegalStateException ("value already in attribute");
    values.add (index, val);
  }

  public boolean add (Object val)
  {
    if (! ordered && contains (val))
      throw new IllegalStateException ("value already in attribute");
    return values.add (val);
  }

  public void clear ()
  {
    values.clear ();
  }

  public Object clone ()
  {
    BasicAttribute c = new BasicAttribute ();
    c.attrID = attrID;
    c.ordered = ordered;
    c.values = (Vector) values.clone ();
    return c;
  }

  public boolean contains (Object val)
  {
    for (int i = 0; i < values.size (); ++i)
      {
	if (equals (val, values.get (i)))
	  return true;
      }

    return false;
  }

  public boolean equals (Object obj)
  {
    if (! (obj instanceof BasicAttribute))
      return false;
    BasicAttribute b = (BasicAttribute) obj;

    if (ordered != b.ordered
	|| ! attrID.equals (b.attrID)
	|| values.size () != b.values.size ())
      return false;

    for (int i = 0; i < values.size (); ++i)
      {
	boolean ok = false;
	if (ordered)
	  ok = equals (values.get (i), b.values.get (i));
	else
	  {
	    for (int j = 0; j < b.values.size (); ++j)
	      {
		if (equals (values.get (i), b.values.get (j)))
		  {
		    ok = true;
		    break;
		  }
	      }
	  }

	if (! ok)
	  return false;
      }

    return true;
  }

  public Object get ()
  {
    if (values.size () == 0)
      throw new NoSuchElementException ("no values");
    return get (0);
  }

  public Object get (int index)
  {
    return values.get (index);
  }

  public NamingEnumeration getAll ()
  {
    return new BasicAttributeEnumeration ();
  }

  public DirContext getAttributeDefinition ()
    throws OperationNotSupportedException, NamingException
  {
    throw new OperationNotSupportedException ();
  }

  public DirContext getAttributeSyntaxDefinition ()
    throws OperationNotSupportedException, NamingException
  {
    throw new OperationNotSupportedException ();
  }

  public String getID ()
  {
    return attrID;
  }

  public int hashCode ()
  {
    int val = attrID.hashCode ();
    for (int i = 0; i < values.size (); ++i)
      {
	Object o = values.get (i);
	if (o == null)
	  {
	    // Nothing.
	  }
	else if (o instanceof Object[])
	  {
	    Object[] a = (Object[]) o;
	    for (int j = 0; j < a.length; ++j)
	      val += a[j].hashCode ();
	  }
	else
	  val += o.hashCode ();
      }

    return val;
  }

  public boolean isOrdered ()
  {
    return ordered;
  }

  public Object remove (int index)
  {
    return values.remove (index);
  }

  public boolean remove (Object val)
  {
    for (int i = 0; i < values.size (); ++i)
      {
	if (equals (val, values.get (i)))
	  {
	    values.remove (i);
	    return true;
	  }
      }

    return false;
  }

  public Object set (int index, Object val)
  {
    if (! ordered && contains (val))
      throw new IllegalStateException ("value already in attribute");
    return values.set (index, val);
  }

  public int size ()
  {
    return values.size ();
  }

  public String toString ()
  {
    String r = attrID;
    for (int i = 0; i < values.size (); ++i)
      r += ";" + values.get (i).toString ();
    return r;
  }

  // This is used for testing equality of two Objects according to our
  // local rules.
  private boolean equals (Object one, Object two)
  {
    if (one == null)
      return two == null;

    if (one instanceof Object[])
      {
	if (! (two instanceof Object[]))
	  return false;

	Object[] aone = (Object[]) one;
	Object[] atwo = (Object[]) two;

	if (aone.length != atwo.length)
	  return false;

	for (int i = 0; i < aone.length; ++i)
	  {
	    if (! aone[i].equals (atwo[i]))
	      return false;
	  }

	return true;
      }

    return one.equals (two);
  }

  // Used when enumerating this attribute.
  private class BasicAttributeEnumeration implements NamingEnumeration
  {
    int where = -1;

    public BasicAttributeEnumeration ()
    {
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
      return where < values.size ();
    }

    public Object nextElement () throws NoSuchElementException
    {
      if (where + 1 >= values.size ())
	throw new NoSuchElementException ("no more elements");
      ++where;
      return values.get (where);
    }
  }
}
