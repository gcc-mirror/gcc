/* DefaultActivationGroup.java -- Default activation group.
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


package gnu.java.rmi.activation;

import gnu.java.rmi.server.ActivatableServerRef;
import gnu.java.rmi.server.UnicastServer;

import java.lang.reflect.Constructor;
import java.rmi.MarshalledObject;
import java.rmi.Remote;
import java.rmi.RemoteException;
import java.rmi.activation.ActivationDesc;
import java.rmi.activation.ActivationException;
import java.rmi.activation.ActivationGroup;
import java.rmi.activation.ActivationGroupID;
import java.rmi.activation.ActivationID;
import java.rmi.activation.UnknownObjectException;

/**
 * The default activation group class. This activation group assumes that
 * all classes are accessible via current thread context class loader.
 * The remote class loading is not supported for security reasons. The 
 * activation always occurs in the current jre.
 * 
 * @author Audrius Meskauskas (audriusa@Bioinformatics.org)
 */
public class DefaultActivationGroup
    extends ActivationGroup
{
  /**
   * Use the serialVersionUID for interoperability.
   */
  private static final long serialVersionUID = 1;
  
  /**
   * Used during the group creation (required constructor).
   */
  static final Class[] cConstructorTypes = new Class[]
                                                   {
                                                    ActivationID.class,
                                                    MarshalledObject.class
                                                   };
  
  
  /**
   * Create the new default activation group.
   * 
   * @param id the group activation id.
   * @param data may contain the group initialization data (unused and can be
   *          null)
   * @throws RemoteException if the super constructor does
   */   
  public DefaultActivationGroup(ActivationGroupID id, MarshalledObject data)
  throws RemoteException
  {
    super(id);
  }
  
  
  /**
   * May be overridden and used as a hook. This method is called each time
   * the new object is instantiated.
   */
  public void activeObject(ActivationID id, Remote obj)
      throws ActivationException, UnknownObjectException, RemoteException
  {
    // Nothing to do (the monitor is already notified in newInstance)
  }

  /**
   * Create the new instance of the object, using the class name and location
   * information, stored in the passed descriptor. The method expects the object
   * class to have the two parameter constructor, the first parameter being the
   * {@link ActivationID} and the second the {@link MarshalledObject}.
   * 
   * @param id the object activation id
   * @param desc the activation descriptor, providing the information, necessary
   *          to create and activate the object
   * @return the marshalled object, containing the exported stub of the created
   *         object
   * @throws ActivationException if the activation fails due any reason
   */  
  public MarshalledObject newInstance(ActivationID id, ActivationDesc desc)
      throws ActivationException, RemoteException
  {
    try
      {
        if (ActivationSystemTransient.debug)
          System.out.println("Instantiating "+desc.getClassName());
        
        Remote object;
        Class objectClass;

        ClassLoader loader = Thread.currentThread().getContextClassLoader();
        objectClass = loader.loadClass(desc.getClassName());
        Constructor constructor = objectClass.getConstructor(cConstructorTypes);
        object = (Remote) constructor.newInstance(
          new Object[] { id, desc.getData() });
        
        // Make the object accessible and create the stub.
        ActivatableServerRef ref = UnicastServer.getActivatableRef(id);
        Remote stub = ref.exportObject(object);
        
        MarshalledObject marsh = new MarshalledObject(stub);
        
        // Notify the activation monitor.
        activeObject(id, marsh);
        
        // Make call to the hook that may be overridden.
        activeObject(id, stub);
        
        return marsh;
      }
    catch (Exception e)
      {
        ActivationException acex = new ActivationException(
          "Unable to activate "+ desc.getClassName()
            + " from "+ desc.getLocation(), e);
        throw acex;
      }
  }

}
