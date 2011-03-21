/* ClientCertificateTypeList.java -- A list of certificate types.
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

import gnu.javax.net.ssl.provider.CertificateRequest.ClientCertificateType;

import java.io.PrintWriter;
import java.io.StringWriter;

import java.nio.ByteBuffer;

import java.util.ConcurrentModificationException;
import java.util.ListIterator;
import java.util.NoSuchElementException;

public class ClientCertificateTypeList implements Iterable<ClientCertificateType>
{
  private final ByteBuffer buffer;
  private int modCount;

  public ClientCertificateTypeList (final ByteBuffer buffer)
  {
    this.buffer = buffer;
    modCount = 0;
  }

  public int size ()
  {
    return (buffer.get (0) & 0xFF);
  }

  public CertificateRequest.ClientCertificateType get (final int index)
  {
    int size = size ();
    if (index < 0 || index >= size)
      throw new IndexOutOfBoundsException ("limit: " + size
                                           + "; requested: " + index);
    return CertificateRequest.ClientCertificateType.forValue
      (buffer.get (index + 1) & 0xFF);
  }

  public java.util.Iterator<ClientCertificateType> iterator()
  {
    return new Iterator();
  }

  public void put (final int index, final CertificateRequest.ClientCertificateType type)
  {
    int size = size ();
    if (index < 0 || index >= size)
      throw new IndexOutOfBoundsException ("limit: " + size
                                           + "; requested: " + index);
    buffer.put (index + 1, (byte) type.getValue ());
    modCount++;
  }

  public void setSize (final int newSize)
  {
    if (newSize < 0 || newSize > 255)
      throw new IllegalArgumentException ("size must be between 0 and 255");
    if (newSize + 1 > buffer.capacity ())
      throw new IllegalArgumentException ("limit: " + (buffer.capacity () - 1)
                                          + "; requested: " + newSize);
    buffer.put (0, (byte) newSize);
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
    if (prefix != null) out.print (prefix);
    out.print ("[");
    out.print (size ());
    out.println ("] {");
    for (Iterator it = new Iterator (); it.hasNext (); )
      {
        if (prefix != null) out.print (prefix);
        out.print ("  ");
        out.print (it.next ());
        if (it.hasNext ())
          out.print (",");
        out.println ();
      }
    if (prefix != null) out.print (prefix);
    out.println ("};");
    return str.toString ();
  }

  public boolean equals (Object o)
  {
    if (!(o instanceof ClientCertificateTypeList))
      return false;
    ClientCertificateTypeList that = (ClientCertificateTypeList) o;

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

  public class Iterator implements ListIterator<CertificateRequest.ClientCertificateType>
  {
    private int index;
    private final int modCount;

    Iterator ()
    {
      index = 0;
      modCount = ClientCertificateTypeList.this.modCount;
    }

    public void add (CertificateRequest.ClientCertificateType type)
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

    public CertificateRequest.ClientCertificateType next () throws NoSuchElementException
    {
      if (modCount != ClientCertificateTypeList.this.modCount)
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

    public CertificateRequest.ClientCertificateType previous () throws NoSuchElementException
    {
      if (index == 0)
        throw new NoSuchElementException ();
      if (modCount != ClientCertificateTypeList.this.modCount)
        throw new ConcurrentModificationException ();
      try
        {
          return get (--index);
        }
      catch (IndexOutOfBoundsException ioobe)
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

    public void set (final CertificateRequest.ClientCertificateType type)
    {
      put (index, type);
    }
  }
}
