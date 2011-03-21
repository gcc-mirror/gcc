/* CipherSuiteList.java -- A list of cipher suites.
   Copyright (C) 2006  Free Software Foundation, Inc.

This file is a part of GNU Classpath.

GNU Classpath is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2 of the License, or (at
your option) any later version.

GNU Classpath is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
General Public License for more details.

You should have received a copy of the GNU General Public License
along with GNU Classpath; if not, write to the Free Software
Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA 02110-1301
USA

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
exception statement from your version.  */


package gnu.javax.net.ssl.provider;

import java.io.PrintWriter;
import java.io.StringWriter;

import java.nio.ByteBuffer;
import java.util.ConcurrentModificationException;
import java.util.ListIterator;
import java.util.NoSuchElementException;

public final class CipherSuiteList implements Iterable<CipherSuite>
{
  private final ByteBuffer buffer;
  private final ProtocolVersion version;
  private int modCount;

  public CipherSuiteList (final ByteBuffer buffer)
  {
    this (buffer, ProtocolVersion.SSL_3);
  }

  public CipherSuiteList (final ByteBuffer buffer, final ProtocolVersion version)
  {
    this.version = version;
    this.buffer = buffer;
    modCount = 0;
  }

  /**
   * Return the number of elements in this list.
   *
   * @return The size of this list.
   */
  public int size ()
  {
    return (buffer.getShort (0) & 0xFFFF) >>> 1;
  }

  /**
   * Get the cipher suite at the specified index.
   *
   * @param index The index of the suite to get.
   * @return The cipher suite at that index.
   * @throws IndexOutOfBoundsException If the index is negative or is
   * not less than {@link size()}.
   */
  public CipherSuite get (final int index)
  {
    int size = size ();
    if (index < 0 || index >= size)
      throw new IndexOutOfBoundsException ("limit: " + size
                                           + "; requested: " + index);
    return CipherSuite.forValue(buffer.getShort(2 + (index << 1))).resolve();
  }

  /**
   * Set the CipherSuite at the specified index. The list must have
   * sufficient size to hold the element (that is, <code>index &lt;=
   * size ()</code>).
   *
   * @param index The index to put the suite.
   * @param suite The CipherSuite object.
   * @throws IndexOutOfBoundsException If <code>index</code> is not
   * less than @{link #size()}, or if it is negative.
   * @throws NullPointerException If <code>suite</code> is
   * <code>null</code>.
   * @throws java.nio.ReadOnlyBufferException If the underlying buffer
   * is not writable.
   */
  public void put (final int index, final CipherSuite suite)
  {
    int size = size ();
    if (index < 0 || index >= size)
      throw new IndexOutOfBoundsException ("limit: " + size
                                           + "; requested: " + index);
    buffer.position (2 + (index << 1));
    buffer.put (suite.id ());
    modCount++;
  }

  /**
   * Sets the size of this list. You must call this if you are adding
   * elements to the list; calling {@link
   * #put(int,gnu.jessie.provider.CipherSuite)} does not expand the
   * list size (the same goes for removing elements, as there is no
   * <code>remove</code> method).
   *
   * @param newSize The new size of this list.
   * @throws IllegalArgumentException If the new size is negative or
   * greater than 32767, or if there is insufficient space for that
   * many elements in the underlying buffer.
   * @throws java.nio.ReadOnlyBufferException If the underlying buffer
   * is not writable.
   */
  public void setSize (final int newSize)
  {
    if (newSize < 0 || newSize > 32767)
      throw new IllegalArgumentException ("size must be between 0 and 32767");
    if ((newSize << 1) + 2 > buffer.capacity ())
      throw new IllegalArgumentException ("limit: " + buffer.capacity ()
                                          + "; requested: " + newSize);
    buffer.putShort (0, (short) (newSize << 1));
    modCount++;
  }

  public String toString ()
  {
    return toString (null);
  }

  public String toString (final String prefix)
  {
    StringWriter str = new StringWriter ();
    PrintWriter out = new PrintWriter (str);
    if (prefix != null)
      out.print (prefix);
    out.print ("[");
    out.print (size ());
    out.println ("] {");
    for (Iterator it = new Iterator (); it.hasNext (); )
      {
        CipherSuite suite = (CipherSuite) it.next ();
        if (prefix != null)
          out.print (prefix);
        out.print ("  ");
        out.print (suite);
        if (it.hasNext ())
          out.print (",");
        out.println ();
      }
    if (prefix != null)
      out.print (prefix);
    out.print ("};");
    return str.toString ();
  }

  public boolean equals (Object o)
  {
    if (!(o instanceof CipherSuiteList))
      return false;
    CipherSuiteList that = (CipherSuiteList) o;

    if (size () != that.size ())
      return false;

    for (Iterator it1 = new Iterator (), it2 = that.new Iterator ();
         it1.hasNext () && it2.hasNext (); )
      {
        if (!it1.next ().equals (it2.next ()))
          return false;
      }
    return true;
  }

  public java.util.Iterator<CipherSuite> iterator ()
  {
    return new Iterator ();
  }

  /**
   * An iterator for the elements in this list. The iterator supports
   * only the <code>set</code> method out of the optional methods,
   * because elements in a CipherSuiteList may not be removed or
   * added; only the size of the list can be changed, and elements at
   * a specific index changed.
   */
  public class Iterator implements ListIterator<CipherSuite>
  {
    private final int modCount;
    private int index;

    Iterator ()
    {
      this.modCount = CipherSuiteList.this.modCount;
      index = 0;
    }

    public void add (CipherSuite cs)
    {
      throw new UnsupportedOperationException ();
    }

    public boolean hasNext ()
    {
      return (index < size ());
    }

    public boolean hasPrevious ()
    {
      return (index > 0);
    }

    public CipherSuite next () throws NoSuchElementException
    {
      if (modCount != CipherSuiteList.this.modCount)
        throw new ConcurrentModificationException ();
      try
        {
          return get (index++);
        }
      catch (IndexOutOfBoundsException ioobe)
        {
          throw new NoSuchElementException ();
        }
    }

    public int nextIndex ()
    {
      if (hasNext ())
        return (index + 1);
      return -1;
    }

    public CipherSuite previous () throws NoSuchElementException
    {
      if (index == 0)
        throw new NoSuchElementException ();
      if (modCount != CipherSuiteList.this.modCount)
        throw new ConcurrentModificationException ();
      try
        {
          return get (--index);
        }
      catch (IndexOutOfBoundsException ioobe) // on empty list
        {
          throw new NoSuchElementException ();
        }
    }

    public int previousIndex ()
    {
      return (index - 1);
    }

    public void remove ()
    {
      throw new UnsupportedOperationException ();
    }

    public void set (final CipherSuite cs)
    {
      put (index, cs);
    }
  }
}
