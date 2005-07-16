/* UtilDelegateImpl.java -- 
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

import java.rmi.Remote;
import java.rmi.RemoteException;
import java.rmi.server.RMIClassLoader;
import java.net.MalformedURLException;
import java.io.*;
//import org.omg.CORBA.ORB;
//import org.omg.CORBA.SystemException;
//import org.omg.CORBA.portable.InputStream;
//import org.omg.CORBA.portable.OutputStream;
import javax.rmi.CORBA.*;

public class UtilDelegateImpl
  implements UtilDelegate
{
  // XXX javax.rmi.ORB -> org.omg.CORBA.ORB
  public Object copyObject(Object obj, javax.rmi.ORB orb)
    throws RemoteException
  {
    throw new Error("Not implemented for UtilDelegate");
  }

  // XXX javax.rmi.ORB -> org.omg.CORBA.ORB
  public Object[] copyObjects(Object obj[], javax.rmi.ORB orb)
    throws RemoteException
  {
    throw new Error("Not implemented for UtilDelegate");
  }

  public ValueHandler createValueHandler()
  {
    throw new Error("Not implemented for UtilDelegate");
  }
    
  public String getCodebase(Class clz)
  {
    throw new Error("Not implemented for UtilDelegate");
  }
    
  public Tie getTie(Remote target)
  {
    throw new Error("Not implemented for UtilDelegate");
  }
    
  public boolean isLocal(Stub stub)
    throws RemoteException
  {
    throw new Error("Not implemented for UtilDelegate");
  }

  public Class loadClass(String className, String remoteCodebase,
		  	 ClassLoader loader)
    throws ClassNotFoundException
  {
    try{
      if (remoteCodebase == null)
	return RMIClassLoader.loadClass(className);
      else
	return RMIClassLoader.loadClass(remoteCodebase, className);
    }
    catch (MalformedURLException e1)
      {
	throw new ClassNotFoundException(className, e1);
      }
    catch(ClassNotFoundException e2)
      {
	if(loader != null)
	  return loader.loadClass(className);
	else
	  return null;
      }
  }

  public RemoteException mapSystemException(SystemException ex)
  {
    throw new Error("Not implemented for UtilDelegate");
  }

  public Object readAny(InputStream in)
  {
    throw new Error("Not implemented for UtilDelegate");
  }

  public void registerTarget(Tie tie, Remote target)
  {
    throw new Error("Not implemented for UtilDelegate");
  }
    
  public void unexportObject(Remote target)
  {
    throw new Error("Not implemented for UtilDelegate");
  }
    
  public RemoteException wrapException(Throwable orig)
  {
    throw new Error("Not implemented for UtilDelegate");
  }
    
  public void writeAbstractObject(OutputStream out, Object obj)
  {
    throw new Error("Not implemented for UtilDelegate");
  }
    
  public void writeAny(OutputStream out, Object obj)
  {
    throw new Error("Not implemented for UtilDelegate");
  }

  public void writeRemoteObject(OutputStream out, Object obj)
  {
    throw new Error("Not implemented for UtilDelegate");
  }
}
