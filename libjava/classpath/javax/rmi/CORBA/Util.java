/* Util.java -- 
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


package javax.rmi.CORBA;

import gnu.javax.rmi.CORBA.DelegateFactory;
import gnu.javax.rmi.CORBA.GetDelegateInstanceException;

import java.io.InputStream;
import java.io.OutputStream;
import java.rmi.Remote;
import java.rmi.RemoteException;

public class Util
{

  private static UtilDelegate delegate;
  static
  {
    try
      {
	delegate = (UtilDelegate)DelegateFactory.getInstance("Util");
      }
    catch(GetDelegateInstanceException e)
      {
	delegate = null;
      }
  }

  private Util()
  {
  }

  // XXX - javax.rmi.ORB -> org.omg.CORBA.ORB
  public static Object copyObject(Object obj, javax.rmi.ORB orb)
    throws RemoteException
  {
    if(delegate != null)
      return delegate.copyObject(obj, orb);
    else
      return null;
  }

  // XXX - javax.rmi.ORB -> org.omg.CORBA.ORB
  public static Object[] copyObjects(Object obj[], javax.rmi.ORB orb)
    throws RemoteException
  {
    if(delegate != null)
      return delegate.copyObjects(obj, orb);
    else
      return null;
  }
    
  public static ValueHandler createValueHandler()
  {
    if(delegate != null)
      return delegate.createValueHandler();
    else
      return null;
  }
    
  public static String getCodebase(Class clz)
  {
    if(delegate != null)
      return delegate.getCodebase(clz);
    else
      return null;
  }
    
  public static Tie getTie(Remote target)
  {
    if(delegate != null)
      return delegate.getTie(target);
    else
      return null;
  }

  public static boolean isLocal(Stub stub)
    throws RemoteException
  {
    if(delegate != null)
      return delegate.isLocal(stub);
    else
      return false;
  }

  public static Class loadClass(String className, String remoteCodebase, ClassLoader loader)
    throws ClassNotFoundException
  {
    if(delegate != null)
      return delegate.loadClass(className, remoteCodebase, loader);
    else
      throw new ClassNotFoundException(className + ": delegate == null");
  }
    
  public static RemoteException mapSystemException(SystemException ex)
  {
    if(delegate != null)
      return delegate.mapSystemException(ex);
    else
      return null;
  }

  public static Object readAny(InputStream in)
  {
    if(delegate != null)
      return delegate.readAny(in);
    else
      return null;
  }

  public static void registerTarget(Tie tie, Remote target)
  {
    if(delegate != null)
      delegate.registerTarget(tie, target);
  }
    
  public static void unexportObject(Remote target)
  {
    if(delegate != null)
      delegate.unexportObject(target);
  }
    
  public static RemoteException wrapException(Throwable orig)
  {
    if(delegate != null)
      return delegate.wrapException(orig);
    else
      return null;
  }
    
  public static void writeAbstractObject(OutputStream out, Object obj)
  {
    if(delegate != null)
      delegate.writeAbstractObject(out, obj);
  }
    
  public static void writeAny(OutputStream out, Object obj)
  {
    if(delegate != null)
      delegate.writeAny(out, obj);
  }
    
  public static void writeRemoteObject(OutputStream out, Object obj)
  {
    if(delegate != null)
      delegate.writeRemoteObject(out, obj);
  }

}
