/* UnicastServerRef.java --
   Copyright (c) 1996, 1997, 1998, 1999, 2002, 2003, 2004, 2006
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


package gnu.java.rmi.server;

import java.io.ObjectInputStream;
import java.lang.reflect.Constructor;
import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;
import java.lang.reflect.Proxy;
import java.rmi.Remote;
import java.rmi.RemoteException;
import java.rmi.server.ObjID;
import java.rmi.server.RMIServerSocketFactory;
import java.rmi.server.RemoteObjectInvocationHandler;
import java.rmi.server.RemoteRef;
import java.rmi.server.RemoteServer;
import java.rmi.server.RemoteStub;
import java.rmi.server.ServerNotActiveException;
import java.rmi.server.Skeleton;
import java.util.HashSet;
import java.util.Hashtable;
import java.util.Iterator;

/**
 * This class connects the local, remotely available (exported) object to
 * the local RMI server that accepts the remote calls. 
 */
public class UnicastServerRef
    extends UnicastRef
{ 

  /**
   * Use GNU Classpath v 0.20 SVUID for interoperability
   */
  private static final long serialVersionUID = - 5585608108300801246L;
  
  /**
   * The class array, defining parameters of the jdk 1.2 RMI stub constructor. 
   */
  private static final Class[] stubprototype = new Class[] { RemoteRef.class };
  
  /**
   * The exported remote object itself.
   */
  Remote myself; // save the remote object itself
  
  /**
   * The skeleton (if any), associated with the exported remote object.
   */
  protected Skeleton skel;
  
  /**
   * The stub, associated with the exported remote object (may be proxy class).
   */
  protected Remote stub;
  
  /**
   * The method table (RMI hash code to method) of the methods of the 
   * exported object.
   */
  protected Hashtable methods = new Hashtable();

  /**
   * Used by serialization.
   */
  UnicastServerRef()
  {
  }

  public UnicastServerRef(ObjID id, int port, RMIServerSocketFactory ssf)
      throws RemoteException
  {
    super(id);
    manager = UnicastConnectionManager.getInstance(port, ssf);
  }
  
  /**
   * Export the object and return its remote stub. The method tries to locate
   * existing stubs and skeletons. If this fails, the method instantiates the
   * proxy stub class.
   * 
   * Stubs and skeletons are always ignored (even if present) if the 
   * java.rmi.server.ignoreStubClasses property is set to true.
   * 
   * @param obj the object being exported.
   * @return the stub (existing class or proxy) of the exported object.
   * @throws RemoteException if the export failed due any reason
   */
  public Remote exportObject(Remote obj) throws RemoteException
  {
    if (myself == null)
      {
        myself = obj;
        // Save it to server manager, to let client calls in the same VM to
        // issue local call
        manager.serverobj = obj;

        String ignoreStubs;
        
        ClassLoader loader =obj.getClass().getClassLoader(); 
        
        // Stubs are always searched for the bootstrap classes that may have
        // obsolete pattern and may still need also skeletons.
        if (loader==null)
          ignoreStubs = "false";
        else
          ignoreStubs = System.getProperty("java.rmi.server.ignoreStubClasses", 
                                           "false");
        
        if (! ignoreStubs.equals("true"))
          {
            // Find and install the stub
            Class cls = obj.getClass();

            // where ist the _Stub? (check superclasses also)
            Class expCls = findStubSkelClass(cls);

            if (expCls != null)
              {
                stub = (RemoteStub) getHelperClass(expCls, "_Stub");
                // Find and install the skeleton (if there is one)
                skel = (Skeleton) getHelperClass(expCls, "_Skel");
              }
          }

        if (stub == null)
          stub = createProxyStub(obj.getClass(), this);

        // Build hash of methods which may be called.
        buildMethodHash(obj.getClass(), true);

        // Export it.
        UnicastServer.exportObject(this);
      }

    return stub;
  }
  
  /**
   * Get the stub (actual class or proxy) of the exported remote object.
   * 
   * @return the remote stub (null if exportObject has not been called).
   */
  public Remote getStub()
  {
    return stub;
  }
  
  /**
   * Unexport the object (remove methods from the method hashcode table 
   * and call UnicastServer.unexportObject.
   * 
   * @param obj the object being unexported
   * @param force passed to the UnicastServer.unexportObject.
   * @return value, returned by the UnicastServer.unexportObject.
   */
  public boolean unexportObject(Remote obj, boolean force)
  {
    // Remove all hashes of methods which may be called.
    buildMethodHash(obj.getClass(), false);
    return UnicastServer.unexportObject(this, force);
  }

  /**
   * Return the class in the hierarchy for that the stub class is defined.
   * The Subs/Skels might not there for the actual class, but maybe for one of
   * the superclasses.
   * 
   * @return the class having stub defined, null if none.
   */
  protected Class findStubSkelClass(Class startCls)
  {
    Class cls = startCls;

    while (true)
      {
        try
          {
            String stubClassname = cls.getName() + "_Stub";
            ClassLoader cl = cls.getClassLoader();
            Class scls = cl == null ? Class.forName(stubClassname)
                                   : cl.loadClass(stubClassname);
            return cls; // found it
          }
        catch (ClassNotFoundException e)
          {
            Class superCls = cls.getSuperclass();
            if (superCls == null
                || superCls == java.rmi.server.UnicastRemoteObject.class)
              {
                return null;
              }
            cls = superCls;
          }
      }
  }
  
  /**
   * Get the helper (assisting) class with the given type. 
   * 
   * @param cls the class, for that the helper class is requested. This class
   * and the requested helper class must share the same class loader.
   * 
   * @param type the type of the assisting helper. The only currently supported
   * non deprecated value is "_Stub" (load jdk 1.1 or 1.2 RMI stub). Another
   * (deprecated) value is "_Skel" (load skeleton).
   * 
   * @return the instantiated instance of the helper class or null if the
   * helper class cannot be found or instantiated.
   */
  protected Object getHelperClass(Class cls, String type)
  {
    try
      {
        String classname = cls.getName();
        ClassLoader cl = cls.getClassLoader();
        Class scls = cl == null ? Class.forName(classname + type)
                               : cl.loadClass(classname + type);
        if (type.equals("_Stub"))
          {
            try
              {
                // JDK 1.2 stubs
                Constructor con = scls.getConstructor(stubprototype);
                return (con.newInstance(new Object[] { this }));
              }
            catch (NoSuchMethodException e)
              {
              }
            catch (InstantiationException e)
              {
              }
            catch (IllegalAccessException e)
              {
              }
            catch (IllegalArgumentException e)
              {
              }
            catch (InvocationTargetException e)
              {
              }
            // JDK 1.1 stubs
            RemoteStub stub = (RemoteStub) scls.newInstance();
            UnicastRemoteStub.setStubRef(stub, this);
            return (stub);
          }
        else
          {
            // JDK 1.1 skel
            return (scls.newInstance());
          }
      }
    catch (ClassNotFoundException e)
      {
      }
    catch (InstantiationException e)
      {
      }
    catch (IllegalAccessException e)
      {
      }
    return (null);
  }

  public String getClientHost() throws ServerNotActiveException
  {
    return RemoteServer.getClientHost();
  }
  
  /**
   * Build the method has code table and put it into {@link #methods}
   * (mapping RMI hashcode tos method). The same method is used to remove
   * the table.
   * 
   * @param cls the class for that the method table is built. 
   * @param build if true, the class methods are added to the table. If 
   * false, they are removed from the table.
   */
  protected void buildMethodHash(Class cls, boolean build)
  {
    Method[] meths = cls.getMethods();
    for (int i = 0; i < meths.length; i++)
      {
        /* Don't need to include any java.xxx related stuff */
        if (meths[i].getDeclaringClass().getName().startsWith("java."))
          {
            continue;
          }
        long hash = RMIHashes.getMethodHash(meths[i]);
        if (build)
          methods.put(new Long(hash), meths[i]);
        else
          methods.remove(new Long(hash));
        // System.out.println("meth = " + meths[i] + ", hash = " + hash);
      }
  }

  Class getMethodReturnType(int method, long hash) throws Exception
  {
    if (method == - 1)
      {
        Method meth = (Method) methods.get(new Long(hash));
        return meth.getReturnType();
      }
    else
      return null;
  }
  
  /**
   * This method is called from the {@link UnicastServer#incomingMessageCall}
   * to deliver the remote call to this object.
   */
  public Object incomingMessageCall(UnicastConnection conn, int method,
                                    long hash) throws Exception
  {
    // System.out.println("method = " + method + ", hash = " + hash);
    // If method is -1 then this is JDK 1.2 RMI - so use the hash
    // to locate the method
    if (method == - 1)
      {
        Method meth = (Method) methods.get(new Long(hash));
        // System.out.println("class = " + myself.getClass() + ", meth = " +
        // meth);
        if (meth == null)
          {
            throw new NoSuchMethodException(
              myself.getClass().getName()+" hash "+hash);
          }

        ObjectInputStream in = conn.getObjectInputStream();
        int nrargs = meth.getParameterTypes().length;
        Object[] args = new Object[nrargs];
        for (int i = 0; i < nrargs; i++)
          {
            /**
             * For debugging purposes - we don't handle CodeBases quite right so
             * we don't always find the stubs. This lets us know that.
             */
            try
              {
                // need to handle primitive types
                args[i] = ((RMIObjectInputStream) in)
                  .readValue(meth.getParameterTypes()[i]);

              }
            catch (Exception t)
              {
                t.printStackTrace();
                throw t;
              }
          }
        //We must reinterpret the exception thrown by meth.invoke()
        //return (meth.invoke(myself, args));
        Object ret = null;
        try
          {
            ret = meth.invoke(myself, args);
          }
        catch (InvocationTargetException e)
          {
            Throwable cause = e.getTargetException();
            if (cause instanceof Exception)
              {
                throw (Exception) cause;
              }
            else if (cause instanceof Error)
              {
                throw (Error) cause;
              }
            else
              {
                throw new Error(
                  "The remote method threw a java.lang.Throwable that"+
                  " is neither java.lang.Exception nor java.lang.Error.",
                  e);
              }
          }
        return ret;
      }
    // Otherwise this is JDK 1.1 style RMI - we find the skeleton
    // and invoke it using the method number.  We wrap up our
    // connection system in a UnicastRemoteCall so it appears in a
    // way the Skeleton can handle.
    else
      {
        if (skel == null)
          throw new NoSuchMethodException("JDK 1.1 call - Skeleton required");
        
        UnicastRemoteCall call = new UnicastRemoteCall(conn);
        skel.dispatch(myself, call, method, hash);
        if (! call.isReturnValue())
          return RMIVoidValue.INSTANCE;
        else
          return (call.returnValue());
      }
  }
  
  /**
   * Create the 1.2 proxy stub in the case when the pre-generated stub is not
   * available of the system is explicitly instructed to use proxy stubs.
   * 
   * @param stubFor the class for that the proxy class must be constructed.
   * @param reference the remote reference, used to find the given object
   * 
   * @return the applicable proxy stub.
   * 
   * @author Audrius Meskauskas (AudriusA@Bioinformatics.org)
   */
  Remote createProxyStub(Class stubFor, RemoteRef reference)
  {
    // Collect all interfaces, implemented by stubFor and derived from
    // Remote (also Remote itself):
    HashSet interfaces = new HashSet();
    Class c = stubFor;
    Class[] intfs;

    while (c != null)
      {
        intfs = c.getInterfaces();
        for (int i = 0; i < intfs.length; i++)
          {
            if (Remote.class.isAssignableFrom(intfs[i]))
              interfaces.add(intfs[i]);
          }
        c = c.getSuperclass();
      }

    intfs = new Class[interfaces.size()];
    Iterator it = interfaces.iterator();

    for (int i = 0; i < intfs.length; i++)
      intfs[i] = (Class) it.next();
    
    RemoteObjectInvocationHandler handler = 
      new RemoteObjectInvocationHandler(reference);
    
    Object proxy = 
      Proxy.newProxyInstance(stubFor.getClassLoader(), intfs, handler);

    return (Remote) proxy;
  }
  

}


