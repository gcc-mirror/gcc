/* X500Principal.java -- X.500 principal.
   Copyright (C) 2003 Free Software Foundation, Inc.

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


package javax.security.auth.x500;

import java.io.ByteArrayInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.NotActiveException;
import java.io.ObjectInputStream;
import java.io.ObjectOutputStream;
import java.io.Serializable;

import java.security.Principal;

import java.util.HashSet;
import java.util.LinkedList;

import gnu.java.security.x509.X500DistinguishedName;

public final class X500Principal implements Principal, Serializable
{
  private static final long serialVersionUID = -500463348111345721L;

  // Constants and fields.
  // ------------------------------------------------------------------------

  public static final String CANONICAL = "CANONICAL";

  public static final String RFC1779 = "RFC1779";

  public static final String RFC2253 = "RFC2253";

  private transient X500DistinguishedName name;

  // Constructors.
  // ------------------------------------------------------------------------

  public X500Principal(String name)
  {
    if (name == null)
      throw new NullPointerException();
    this.name = new X500DistinguishedName(name);
  }

  public X500Principal(byte[] encoded)
  {
    try
      {
        name = new X500DistinguishedName(encoded);
      }
    catch (IOException ioe)
      {
        throw new IllegalArgumentException(ioe.toString());
      }
  }

  public X500Principal(InputStream encoded)
  {
    try
      {
        name = new X500DistinguishedName(encoded);
      }
    catch (IOException ioe)
      {
        throw new IllegalArgumentException(ioe.toString());
      }
  }

  // Instance methods.
  // ------------------------------------------------------------------------

  public boolean equals(Object o)
  {
    return ((X500Principal) o).name.equals(name);
  }

  public byte[] getEncoded()
  {
    return name.getEncoded();
  }

  public String getName()
  {
    return getName(RFC2253);
  }

  public String getName(String format)
  {
    if (format.equalsIgnoreCase(RFC2253))
      return name.toRFC2253();
    else if (format.equalsIgnoreCase(RFC1779))
      return name.toRFC1779();
    else if (format.equalsIgnoreCase(CANONICAL))
      return name.toCanonical();
    throw new IllegalArgumentException("unsupported format " + format);
  }

  // Serialization methods.
  // ------------------------------------------------------------------------

  private void writeObject(ObjectOutputStream out) throws IOException
  {
    out.writeObject(name.getEncoded());
  }

  private void readObject(ObjectInputStream in)
    throws IOException, NotActiveException, ClassNotFoundException
  {
    byte[] buf = (byte[]) in.readObject();
    name = new X500DistinguishedName(buf);
  }
}
