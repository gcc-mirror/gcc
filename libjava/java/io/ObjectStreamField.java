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


package java.io;

import gnu.java.lang.reflect.TypeSignature;

// XXX doc
public class ObjectStreamField implements java.lang.Comparable
{
  public ObjectStreamField (String name, Class type)
  {
    this.name = name;
    this.type = type;
    this.typename = TypeSignature.getEncodingOfClass(type);
  }
 
  /**
   * There're many cases you can't get java.lang.Class from typename if your context
   * class loader can't load it, then use typename to construct the field
   */
  ObjectStreamField (String name, String typename){
    this.name = name;
    this.typename = typename;
    try{
      type = TypeSignature.getClassForEncoding(typename);
    }catch(ClassNotFoundException e){
      type = Object.class; //??
    }
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
    return typename.charAt (0);
  }

  public String getTypeString ()
  {
    // use intern()
    return typename.intern();
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
  private String typename;
  private int offset = -1; // XXX make sure this is correct
}
