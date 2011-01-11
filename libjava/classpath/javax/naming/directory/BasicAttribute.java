/* BasicAttribute.java --
   Copyright (C) 2000, 2001, 2004, 2006  Free Software Foundation, Inc.

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


package javax.naming.directory;

import java.io.IOException;
import java.io.ObjectInputStream;
import java.io.ObjectOutputStream;
import java.util.NoSuchElementException;
import java.util.Vector;

import javax.naming.NamingEnumeration;
import javax.naming.NamingException;
import javax.naming.OperationNotSupportedException;

/**
 * @author Tom Tromey (tromey@redhat.com)
 * @date June 20, 2001
 * @since 1.3
 */
public class BasicAttribute implements Attribute
{
  private static final long serialVersionUID = 6743528196119291326L;

  /** The ID of this attribute.  */
  protected String attrID;
  /** True if this attribute's values are ordered.  */
  protected boolean ordered;
  /** Values for this attribute.  */
  protected transient Vector<Object> values;

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
    values = new Vector<Object> ();
  }

  public BasicAttribute (String id, Object value)
  {
    this (id, value, false);
  }

  public BasicAttribute (String id, Object value, boolean ordered)
  {
    attrID = id;
    this.ordered = ordered;
    values = new Vector<Object> ();
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
    c.values = (Vector<Object>) values.clone ();
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
    throws NamingException
  {
    if (values.size () == 0)
      throw new NoSuchElementException ("no values");
    return get (0);
  }

  public Object get (int index)
    throws NamingException
  {
    return values.get (index);
  }

  public NamingEnumeration<?> getAll ()
    throws NamingException
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

  private void readObject(ObjectInputStream s)
    throws IOException, ClassNotFoundException
  {
    s.defaultReadObject();
    int size = s.readInt();
    values = new Vector<Object>(size);
    for (int i=0; i < size; i++)
      values.add(s.readObject());
  }

  private void writeObject(ObjectOutputStream s) throws IOException
  {
    s.defaultWriteObject();
    s.writeInt(values.size());
    for (int i=0; i < values.size(); i++)
      s.writeObject(values.get(i));
  }

  // Used when enumerating this attribute.
  private class BasicAttributeEnumeration implements NamingEnumeration
  {
    int where = 0;

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
      if (where == values.size ())
        throw new NoSuchElementException ("no more elements");
      return values.get (where++);
    }
  }
}
