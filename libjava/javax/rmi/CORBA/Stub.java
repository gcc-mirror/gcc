/* Stub.java -- 
   Copyright (C) 2002 Free Software Foundation, Inc.

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


package javax.rmi.CORBA;

import java.io.IOException;
import java.io.ObjectInputStream;
import java.io.ObjectOutputStream;
import java.io.Serializable;
import java.rmi.RemoteException;
//import org.omg.CORBA.ORB;
//import org.omg.CORBA_2_3.portable.ObjectImpl;
//import org.omg.CORBA.portable.ObjectImpl;
import gnu.javax.rmi.CORBA.DelegateFactory;
import gnu.javax.rmi.CORBA.GetDelegateInstanceException;

public abstract class Stub extends ObjectImpl
  implements Serializable
{
  private transient StubDelegate delegate;
                
  protected Stub()
  {
    try
      {
	delegate = (StubDelegate)DelegateFactory.getInstance("Stub");
      }
    catch(GetDelegateInstanceException e)
      {
	delegate = null;
      }
  }

  public int hashCode()
  {
    if(delegate != null)
      return delegate.hashCode(this);
    else
      return 0;
  }

  public boolean equals(Object obj)
  {
    if(delegate != null)
      return delegate.equals(this, obj);
    else
      return false;
  }

  public String toString()
  {
    String s = null;
    if(delegate != null)
      s = delegate.toString(this);
    if(s == null)
      s = super.toString();
    return s;
  }

  // XXX javax.rmi.ORB -> org.omg.CORBA.ORB
  public void connect(javax.rmi.ORB orb)
    throws RemoteException
  {
    if(delegate != null)
      delegate.connect(this, orb);
  }

  /**
   * The following two routines are required by serialized form of Java API doc.
   */
  private void readObject(ObjectInputStream stream)
    throws IOException, ClassNotFoundException
  {
    if(delegate != null)
      delegate.readObject(this, stream);
  }

  private void writeObject(ObjectOutputStream stream)
    throws IOException
  {
    if(delegate != null)
      delegate.writeObject(this, stream);
  }

}
