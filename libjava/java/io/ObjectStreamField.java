/* ObjectStreamField.java -- Class used to store name and class of fields
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

As a special exception, if you link this library with other files to
produce an executable, this library does not by itself cause the
resulting executable to be covered by the GNU General Public License.
This exception does not however invalidate any other reasons why the
executable file might be covered by the GNU General Public License. */


package java.io;

import gnu.java.lang.reflect.TypeSignature;

// XXX doc
public class ObjectStreamField implements java.lang.Comparable
{
  public ObjectStreamField (String name, Class type)
  {
    this.name = name;
    this.type = type;
  }

  public String getName ()
  {
    return name;
  }

  public Class getType ()
  {
    return type;
  }

  public char getTypeCode ()
  {
    return TypeSignature.getEncodingOfClass (type).charAt (0);
  }

  public String getTypeString ()
  {
    return TypeSignature.getEncodingOfClass (type);
  }

  public int getOffset ()
  {
    return offset;
  }

  protected void setOffset (int off)
  {
    offset = off;
  }

  public boolean isPrimitive ()
  {
    return type.isPrimitive ();
  }

  public int compareTo (Object o)
  {
    ObjectStreamField f = (ObjectStreamField)o;
    boolean this_is_primitive = isPrimitive ();
    boolean f_is_primitive = f.isPrimitive ();

    if (this_is_primitive && !f_is_primitive)
      return -1;

    if (!this_is_primitive && f_is_primitive)
      return 1;

    return getName ().compareTo (f.getName ());
  }

  public String toString ()
  {
    return "ObjectStreamField< " + type + " " + name + " >";
  }

  private String name;
  private Class type;
  private int offset = -1; // XXX make sure this is correct
}
