/* UnicastServerRef.java --
   Copyright (c) 1996, 1997, 1998, 1999, 2002, 2003, 2004
   Free Software Foundation, Inc.

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


package gnu.java.rmi.server;

import java.io.ObjectInputStream;
import java.lang.reflect.Constructor;
import java.lang.reflect.Method;
import java.lang.reflect.InvocationTargetException;
import java.rmi.Remote;
import java.rmi.RemoteException;
import java.rmi.server.RemoteStub;
import java.rmi.server.ObjID;
import java.rmi.server.ServerRef;
import java.rmi.server.RemoteServer;
import java.rmi.server.RemoteRef;
import java.rmi.server.ServerNotActiveException;
import java.rmi.server.RMIServerSocketFactory;
import java.rmi.server.Skeleton;
import java.util.Hashtable;

public class UnicastServerRef
	extends UnicastRef 
	implements ServerRef{ //SHOULD implement ServerRef

final static private Class[] stubprototype = new Class[] { RemoteRef.class };

Remote myself; //save the remote object itself
private Skeleton skel;
private RemoteStub stub;
private Hashtable methods = new Hashtable();

/**
 * Used by serialization.
 */
UnicastServerRef()
{
}

public UnicastServerRef(ObjID id, int port, RMIServerSocketFactory ssf) throws RemoteException {
	super(id);
	manager = UnicastConnectionManager.getInstance(port, ssf);
}

public RemoteStub exportObject(Remote obj) throws RemoteException {
	if (myself == null) {
		myself = obj;
		// Save it to server manager, to let client calls in the same VM to issue
		//  local call
		manager.serverobj = obj;

		// Find and install the stub
		Class cls = obj.getClass();
		Class expCls;
		try {
			// where ist the _Stub? (check superclasses also)
			expCls = findStubSkelClass(cls); 
		} catch (Exception ex) {
			throw new RemoteException("can not find stubs for class: " + cls, ex);
		}

		stub = (RemoteStub)getHelperClass(expCls, "_Stub");
		if (stub == null) {
			throw new RemoteException("failed to export: " + cls);
		}

		// Find and install the skeleton (if there is one)
		skel = (Skeleton)getHelperClass(expCls, "_Skel");

		// Build hash of methods which may be called.
		buildMethodHash(obj.getClass(), true);

		// Export it.
		UnicastServer.exportObject(this);
	}

	return (stub);
}

public RemoteStub exportObject(Remote remote, Object obj)
        throws RemoteException
{
    //FIX ME
	return exportObject(remote);
}

public RemoteStub getStub(){
    return stub;
}


public boolean unexportObject(Remote obj, boolean force) {
    // Remove all hashes of methods which may be called.
    buildMethodHash(obj.getClass(), false);
    return UnicastServer.unexportObject(this, force);
}

/**
*
*  The Subs/Skels might not there for the actual class, but maybe 
*  for one of the superclasses.
*
*/
private Class findStubSkelClass(Class startCls) throws Exception {
	Class cls = startCls;

	while (true) {
		try {
			String stubClassname = cls.getName() + "_Stub";
			ClassLoader cl = cls.getClassLoader();
			Class scls = cl == null ? Class.forName(stubClassname)
						: cl.loadClass(stubClassname);
			return cls; // found it
		} catch (ClassNotFoundException e) {
			Class superCls = cls.getSuperclass();
			if (superCls == null 
				|| superCls == java.rmi.server.UnicastRemoteObject.class) 
			{
				throw new Exception("Neither " + startCls 
					+ " nor one of their superclasses (like" + cls + ")" 
					+ " has a _Stub");
			}
			cls = superCls;
		}
	}
}



private Object getHelperClass(Class cls, String type) {
	try {   
	    String classname = cls.getName();
		ClassLoader cl = cls.getClassLoader();
		Class scls = cl == null ? Class.forName(classname + type)
					: cl.loadClass(classname + type);
		if (type.equals("_Stub")) {
			try {
				// JDK 1.2 stubs
				Constructor con = scls.getConstructor(stubprototype);
				return (con.newInstance(new Object[]{this}));
			}
			catch (NoSuchMethodException e) {
			}
			catch (InstantiationException e) {
			}
			catch (IllegalAccessException e) {
			}
			catch (IllegalArgumentException e) {
			}
			catch (InvocationTargetException e) {
			}
			// JDK 1.1 stubs
			RemoteStub stub = (RemoteStub)scls.newInstance();
			UnicastRemoteStub.setStubRef(stub, this);
			return (stub);
		}
		else {
			// JDK 1.1 skel
			return (scls.newInstance());
		}
	}
	catch (ClassNotFoundException e) {
	}
	catch (InstantiationException e) {
	}
	catch (IllegalAccessException e) {
	}
	return (null);
}



public String getClientHost() throws ServerNotActiveException {
	return RemoteServer.getClientHost();
}

private void buildMethodHash(Class cls, boolean build) {
	Method[] meths = cls.getMethods();
	for (int i = 0; i < meths.length; i++) {
		/* Don't need to include any java.xxx related stuff */
		if (meths[i].getDeclaringClass().getName().startsWith("java.")) {
			continue;
		}
		long hash = RMIHashes.getMethodHash(meths[i]);
		if(build)
		    methods.put(new Long (hash), meths[i]);
		else
		    methods.remove(new Long (hash));
//System.out.println("meth = " + meths[i] + ", hash = " + hash);
	}
}

Class getMethodReturnType(int method, long hash) throws Exception
{
    if (method == -1) {
        Method meth = (Method)methods.get(new Long (hash));
        return meth.getReturnType();
    }else
        return null;
}

public Object incomingMessageCall(UnicastConnection conn, int method, long hash) throws Exception {
//System.out.println("method = " + method + ", hash = " + hash);
	// If method is -1 then this is JDK 1.2 RMI - so use the hash
	// to locate the method
	if (method == -1) {
		Method meth = (Method)methods.get(new Long (hash));
//System.out.println("class = " + myself.getClass() + ", meth = " + meth);
		if (meth == null) {
			throw new NoSuchMethodException();
		}

		ObjectInputStream in = conn.getObjectInputStream();
		int nrargs = meth.getParameterTypes().length;
		Object[] args = new Object[nrargs];
		for (int i = 0; i < nrargs; i++) {
			/** 
			 * For debugging purposes - we don't handle CodeBases
			 * quite right so we don't always find the stubs.  This
			 * lets us know that.
			 */
			try {
				// need to handle primitive types
				args[i] = ((RMIObjectInputStream)in).readValue(meth.getParameterTypes()[i]);
				
			}
			catch (Exception t) {
				t.printStackTrace();
				throw t;
			}
		}
		//We must reinterpret the exception thrown by meth.invoke()
		//return (meth.invoke(myself, args));
		Object ret = null;
		try{
		    ret = meth.invoke(myself, args);
		}catch(InvocationTargetException e){
                    Throwable cause = e.getTargetException();
                    if (cause instanceof Exception) {
                        throw (Exception)cause;
                    }
                    else if (cause instanceof Error) {
                        throw (Error)cause;
                    }
                    else {
                        throw new Error("The remote method threw a java.lang.Throwable that is neither java.lang.Exception nor java.lang.Error.", e);
                    }
		}
		return ret;
	}
	// Otherwise this is JDK 1.1 style RMI - we find the skeleton
	// and invoke it using the method number.  We wrap up our
	// connection system in a UnicastRemoteCall so it appears in a
	// way the Skeleton can handle.
	else {
		if (skel == null) {
			throw new NoSuchMethodException();
		}
		UnicastRemoteCall call = new UnicastRemoteCall(conn);
		skel.dispatch(myself, call, method, hash);		  
		if (!call.isReturnValue())
		  return RMIVoidValue.INSTANCE;
		else
		  return (call.returnValue());
	}
}

}


