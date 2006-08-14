/* ActivatableServerRef.java -- The activatable server reference
   Copyright (C) 2006 Free Software Foundation, Inc.

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

import java.io.IOException;
import java.io.ObjectInput;
import java.io.ObjectOutput;
import java.rmi.Remote;
import java.rmi.RemoteException;
import java.rmi.activation.ActivationID;
import java.rmi.server.ObjID;
import java.rmi.server.RMIServerSocketFactory;
import java.rmi.server.RemoteStub;
import java.rmi.server.Skeleton;

/**
 * The activatable server reference works like UnicastServerReference, but it
 * additionally activates the associated object on demand, during the first
 * incoming call. When UnicastServerReference takes the working reference,
 * the ActivatableServerRef takes the activation id instead.
 * 
 * @author Audrius Meskauskas (Audriusa@Bioinformatics.org)
 */
public class ActivatableServerRef extends UnicastServerRef
{
  /**
   * Use SVUID for interoperability
   */
  private static final long serialVersionUID = 1;
  
  /**
   * The object activation id.
   */
  public ActivationID actId;

  /**
   * Used by serialization only
   */
  public ActivatableServerRef()
  {
    super();
  }
  
  /**
   * Create the new activatable server reference that will activate object on
   * the first call using the given activation id.
   */
  public ActivatableServerRef(ObjID id, ActivationID anId, int aPort,
                              RMIServerSocketFactory ssFactory)
      throws RemoteException
  {
    super(id, aPort, ssFactory);
    actId = anId;
    
    // The object ID will be placed in the object map and should deliver
    // incoming call to {@link #incommingMessageCall}. The object itself
    // is currently null.
    UnicastServer.exportActivatableObject(this);
  }
  
  /**
   * Inactivate the object (stop the server).
   */
  public void inactivate()
  {
    manager.stopServer();
  }
  
  /**
   * Activate the object (normally during the first call).
   */
  protected void activate() throws RemoteException
  {
    try
      {
        Remote self = actId.activate(false);
        
        // This will call UnicastServer.exportObject, replacing null by
        // the activated object (self) in the object map.
        exportObject(self);
      }
    catch (RemoteException rex)
      {
        throw rex;
      }
    catch (Exception exc)
      {
        RemoteException rx = new RemoteException("Activation failed.");
        rx.detail = exc;
        throw rx;
      }
  }

  /**
   * If the object is not active, activate it first.
   */
  public Object incomingMessageCall(UnicastConnection conn, int method,
                                    long hash) throws Exception
  {
    if (myself == null)
      activate();
    return super.incomingMessageCall(conn, method, hash);
  }

  /**
   * Export object and ensure it is present in the server activation table 
   * as well.
   */
  public Remote exportObject(Remote obj) throws RemoteException
  {
    Remote r = super.exportObject(obj);
    UnicastServer.registerActivatable(this);
    return r;
  }
  
  /**
   * Export object and ensure it is present in the server activation table as
   * well.
   * 
   * @param aClass the class being exported, must implement Remote.
   */
  public Remote exportClass(Class aClass) throws RemoteException
  {
    if (!Remote.class.isAssignableFrom(aClass))
      throw new InternalError(aClass.getName()+" must implement Remote");

        String ignoreStubs;
        
        ClassLoader loader =aClass.getClassLoader(); 
        
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
            Class cls = aClass;

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
          stub = createProxyStub(aClass, this);

        // Build hash of methods which may be called.
        buildMethodHash(aClass, true);

    UnicastServer.registerActivatable(this);
    return stub;
  }

  /**
   * Get the referencing class.
   */
  public String getRefClass(ObjectOutput out)
  {
    return "ActivatableRef";
  }

  /**
   * Read the content from the input stream.
   */
  public void readExternal(ObjectInput in) throws IOException, ClassNotFoundException
  {
    super.readExternal(in);
    actId = (ActivationID) in.readObject();
  }

  /**
   * Write the content to the output stream.
   */
  public void writeExternal(ObjectOutput out) throws IOException
  {
    super.writeExternal(out);
    out.writeObject(actId);
  }
  
}
