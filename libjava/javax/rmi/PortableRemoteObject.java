/* PortableRemoteObject.java -- 
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


package javax.rmi;

import java.rmi.Remote;
import java.rmi.RemoteException;
import java.rmi.NoSuchObjectException;
import gnu.javax.rmi.CORBA.DelegateFactory;
import gnu.javax.rmi.CORBA.GetDelegateInstanceException;
import javax.rmi.CORBA.PortableRemoteObjectDelegate;
import javax.rmi.CORBA.Util;

public class PortableRemoteObject
  implements Remote /* why doc doesn't say should implement Remote */
{

  private static PortableRemoteObjectDelegate delegate;
  static
  {
    try
      {
	delegate = (PortableRemoteObjectDelegate)DelegateFactory.getInstance
	  ("PortableRemoteObject");
      }
    catch(GetDelegateInstanceException e)
      {
	e.printStackTrace();
	delegate = null;
      }
  }

  protected PortableRemoteObject()
    throws RemoteException
  {
    if(delegate != null)
      exportObject((Remote)this);
  }

  public static void connect(Remote target, Remote source)
    throws RemoteException
  {
    if(delegate != null)
      delegate.connect(target, source);
  }
    
  public static void exportObject(Remote obj)
    throws RemoteException
  {
    if(delegate != null)
      delegate.exportObject(obj);
  }

  public static Object narrow(Object narrowFrom, Class narrowTo)
    throws ClassCastException
  {
    if(delegate != null)
      return delegate.narrow(narrowFrom, narrowTo);
    else
      return null;
  }

  public static Remote toStub(Remote obj)
    throws NoSuchObjectException
  {
    if(delegate != null)
      return delegate.toStub(obj);
    else
      return null;
  }

  public static void unexportObject(Remote obj)
    throws NoSuchObjectException
  {
    if(delegate != null)
      delegate.unexportObject(obj);
  }
  
}
