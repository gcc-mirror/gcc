/*
  Copyright (c) 1996, 1997, 1998, 1999 Free Software Foundation, Inc.

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

package java.rmi;

import java.io.Serializable;
import java.io.ByteArrayOutputStream;
import java.io.IOException;
import gnu.java.rmi.RMIMarshalledObjectInputStream;
import gnu.java.rmi.RMIMarshalledObjectOutputStream;

/**
 * FIXME - doc missing
 */
public final class MarshalledObject
  extends Object implements Serializable 
{
  
  //The following fields are from Java API Documentation "Serialized form"
  private static final long serialVersionUID = 8988374069173025854L;
  byte[] objBytes;
  byte[] locBytes;
  int hash;
  
  public MarshalledObject(Object obj) throws java.io.IOException
  {
    ByteArrayOutputStream objStream = new ByteArrayOutputStream();
    RMIMarshalledObjectOutputStream stream = new RMIMarshalledObjectOutputStream(objStream);
    stream.writeObject(obj);
    stream.flush();
    objBytes = objStream.toByteArray();
    locBytes = stream.getLocBytes();
    
    //The following algorithm of calculating hashCode is similar to String
    hash = 0;
    for (int i = 0; i < objBytes.length; i++)
      hash = hash * 31 + objBytes[i];
    if(locBytes != null)
      for (int i = 0; i < locBytes.length; i++)
	hash = hash * 31 + locBytes[i];
  }
  
  public boolean equals(Object obj) 
  {
    if(obj == null || !(obj instanceof MarshalledObject) )
      return false;

    // hashCode even differs, don't do the time-consuming comparisons
    if (obj.hashCode() != hash)
      return false;
    
    MarshalledObject aobj = (MarshalledObject)obj;
    if (objBytes == null || aobj.objBytes == null)
      return objBytes == aobj.objBytes;
    if (objBytes.length != aobj.objBytes.length)
      return false;
    for (int i = 0; i < objBytes.length; i++) 
      {
	if (objBytes[i] != aobj.objBytes[i])
	  return false;
      }
    // Ignore comparison of locBytes(annotation)
    return true;
  }
  
public Object get() 
  throws java.io.IOException, java.lang.ClassNotFoundException
{
  if(objBytes == null)
    return null;
  RMIMarshalledObjectInputStream stream = 
    new RMIMarshalledObjectInputStream(objBytes, locBytes);
  return stream.readObject();
}
  
  public int hashCode() {
    return hash;
  }
  
}
