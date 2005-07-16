/* PortableServer.java -- 
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


package gnu.javax.rmi;

import java.rmi.Remote;
import java.rmi.RemoteException;
import java.rmi.NoSuchObjectException;
import java.rmi.server.UnicastRemoteObject;
import java.rmi.server.RemoteStub;
import java.util.Hashtable;

import javax.rmi.CORBA.*;

/**
 * The relationship of PortableRemoteObjectImpl with PortableServer
 * is like that of UnicastRemoteObject with UnicastServer
 */
public class PortableServer
{
  static private Hashtable tieCache = new Hashtable();
  static private Object NO_TIE = new Object();
    
  public static final synchronized void exportObject(Remote obj)
    throws RemoteException
  {
    if(Util.getTie(obj) != null)
      return;
    
    Tie tie = getTieFromRemote(obj);
    if (tie != null)
      Util.registerTarget(tie, obj);
    else
      UnicastRemoteObject.exportObject(obj);
  }
    
  public static final void unexportObject(Remote obj)
  {
    if (Util.getTie(obj) != null)
      Util.unexportObject(obj);
    if (tieCache.get(obj) != null) //??
      tieCache.remove(obj);
  }
  
  public static final Remote toStub(Remote obj)
    throws NoSuchObjectException
  {
    if (obj instanceof Stub || obj instanceof RemoteStub) 
      return obj;
    
    Tie tie = Util.getTie(obj);
    Remote stub;
    if (tie != null)
      stub = getStubFromTie(tie);
    else
      throw new NoSuchObjectException("Can't toStub an unexported object");
    return stub;
  }
    
  static synchronized Tie getTieFromRemote(Remote obj)
  {
    Object tie = tieCache.get(obj);
    if (tie == null)
      {
	tie = getTieFromClass(obj.getClass());
	if(tie == null)
	  tieCache.put(obj, NO_TIE);
	else
	  tieCache.put(obj, tie);
      }
    else
      if(tie != NO_TIE)
	{
	  try
	    {
	      tie = obj.getClass().newInstance();
	    }
	  catch(Exception _)
	    {
	      tie = null;
	    }
	}
      else //NO_TIE
	tie = null;
                
    return (Tie)tie;
  }
  
  static synchronized Tie getTieFromClass(Class clz)
  {
    //FIX ME
    return null;
  }
    
  public static Remote getStubFromTie(Tie tie)
  {
    //FIX ME
    return null;
  }
  
  public static Remote getStubFromObjectImpl(ObjectImpl objimpl, Class toClass)
  {
    //FIX ME
    return null;
  }
}
