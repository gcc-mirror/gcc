/* PortableRemoteObjectDelegateImpl.java -- 
   Copyright (C) 2002, 2004  Free Software Foundation, Inc.

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

import gnu.javax.rmi.PortableServer;

import java.rmi.NoSuchObjectException;
import java.rmi.Remote;
import java.rmi.RemoteException;

import javax.rmi.CORBA.PortableRemoteObjectDelegate;

public class PortableRemoteObjectDelegateImpl
  implements PortableRemoteObjectDelegate
{
    
  public PortableRemoteObjectDelegateImpl()
  {
  }

  public void connect(Remote remote, Remote remote1)
    throws RemoteException
  {
    throw new Error("Not implemented for PortableRemoteObjectDelegateImpl");
  }
        
  public void exportObject(Remote obj)
    throws RemoteException
  {
    PortableServer.exportObject(obj);
  }

  public Object narrow(Object narrowFrom, Class narrowTo)
    throws ClassCastException
  {
    if (narrowTo == null)
      throw new ClassCastException("Can't narrow to null class");
    if (narrowFrom == null)
      return null;

    Class fromClass = narrowFrom.getClass();
    Object result = null;
        
    try
      {
	if (narrowTo.isAssignableFrom(fromClass))
	  result = narrowFrom;
	else
	  {
	    System.out.println("We still haven't implement this case: narrow "
			       + narrowFrom + " of type " + fromClass + " to "
			       + narrowTo);
	    Class[] cs = fromClass.getInterfaces();
	    for (int i = 0; i < cs.length; i++)
	      System.out.println(cs[i]);
	    Exception e1 = new Exception();
	    try
	      {
		throw e1;
	      }
	    catch(Exception ee)
	      {
		ee.printStackTrace();
	      }
	    System.exit(2);
	    //throw new Error("We still haven't implement this case: narrow "
	    //                + narrowFrom + " of type " + fromClass + " to "
	    //                + narrowTo);
	    /*
	      ObjectImpl objimpl = (ObjectImpl)narrowFrom;
	      if(objimpl._is_a(PortableServer.getTypeName(narrowTo)))
	      result = PortableServer.getStubFromObjectImpl(objimpl, narrowTo);
	    */
	  }
      }
    catch(Exception e)
      {
	result = null;
      }
        
    if (result == null)
      throw new ClassCastException("Can't narrow from "
				   + fromClass + " to " + narrowTo);
            
    return result;
  }
        
  public Remote toStub(Remote obj)
    throws NoSuchObjectException
  {
    return PortableServer.toStub(obj);
  }

  public void unexportObject(Remote obj)
    throws NoSuchObjectException
  {
    PortableServer.unexportObject(obj);
  }

}
