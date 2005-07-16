/* StubDelegateImpl.java -- 
   Copyright (C) 2002, 2004 Free Software Foundation, Inc.

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


package gnu.javax.rmi.CORBA;

import java.io.IOException;
import java.io.ObjectInputStream;
import java.io.ObjectOutputStream;
import java.rmi.RemoteException;
import javax.rmi.CORBA.Stub;
import javax.rmi.CORBA.StubDelegate;

public class StubDelegateImpl
  implements StubDelegate
{

  private int hashCode;
    
  public StubDelegateImpl(){
    hashCode = 0;   
  }
  // XXX javax.rmi.ORB -> org.omg.CORBA.ORB
  public void connect(Stub self, javax.rmi.ORB orb)
    throws RemoteException
  {
    throw new Error("Not implemented for StubDelegate");
  }

  public boolean equals(Stub self, Object obj)
  {
    if(self == null || obj == null)
      return self == obj;
    if(!(obj instanceof Stub))
      return false;
    return self.hashCode() == ((Stub)obj).hashCode();
  }

  public int hashCode(Stub self)
  {
    //FIX ME
    return hashCode;
  }

  public String toString(Stub self)
  {
    try
      {
	return self._orb().object_to_string(self);
      }
    // XXX javax.rmi.BAD_OPERATION -> org.omg.CORBA.BAD_OPERATION
    catch(javax.rmi.BAD_OPERATION bad_operation)
      {
	return null;
      }
  }

  public void readObject(Stub self, ObjectInputStream s)
    throws IOException, ClassNotFoundException
  {
    throw new Error("Not implemented for StubDelegate");
  }

  public void writeObject(Stub self, ObjectOutputStream s)
    throws IOException
  {
    throw new Error("Not implemented for StubDelegate");
  }
    
}
