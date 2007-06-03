/* ActivationSystemImpl.java -- implementation of the activation system.
   Copyright (c) 2006 Free Software Foundation, Inc.

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

package gnu.classpath.tools.rmid;

import gnu.classpath.tools.common.Persistent;
import gnu.java.rmi.activation.ActivationSystemTransient;

import java.io.File;
import java.io.IOException;
import java.io.ObjectInputStream;
import java.io.ObjectOutputStream;
import java.io.Serializable;
import java.rmi.MarshalledObject;
import java.rmi.RemoteException;
import java.rmi.activation.ActivationDesc;
import java.rmi.activation.ActivationException;
import java.rmi.activation.ActivationGroupDesc;
import java.rmi.activation.ActivationGroupID;
import java.rmi.activation.ActivationID;
import java.rmi.activation.ActivationInstantiator;
import java.rmi.activation.ActivationMonitor;
import java.rmi.activation.ActivationSystem;
import java.rmi.activation.Activator;
import java.rmi.activation.UnknownGroupException;
import java.rmi.activation.UnknownObjectException;

/**
 * Implements the rmid activation system.
 * 
 * @author Audrius Meskauskas (audriusa@bioinformatics.org)
 */
public class ActivationSystemImpl extends ActivationSystemTransient implements
    ActivationSystem, Activator, ActivationMonitor, Serializable
{
  /**
   * Use for interoperability.
   */
  private static final long serialVersionUID = 1;
  
  /**
   * The singleton instance of this class.
   */
  public static ActivationSystemImpl singleton2;
  
  /**
   * Obtain the singleton instance of this class.
   * 
   * @param folder the folder, where the activation system will keep its files.
   * @param cold do the cold start if true, hot (usual) if false.
   */
  public static ActivationSystem getInstance(File folder, boolean cold)
  {
    if (singleton2 == null)
      singleton2 = new ActivationSystemImpl(folder, cold);
    return singleton2;
  }  
  
  /**
   * Creates the group with transient maps.
   * 
   * @param folder
   *          the folder, where the activation system will keep its files.
   * @param cold
   *          do the cold start if true, hot (usual) if false.
   */
  protected ActivationSystemImpl(File folder, boolean cold)
  {
    super(new PersistentBidiHashTable(), new PersistentBidiHashTable());
    singleton2 = this;
    ((PersistentBidiHashTable) groupDescs).init(
        new File(folder, "asi_objects.data"), cold);
    ((PersistentBidiHashTable) descriptions).init(
        new File(folder, "asi_groups.data"),  cold);
  }  
  
  /** @inheritDoc */
  public MarshalledObject activate(ActivationID id, boolean force)
      throws ActivationException, UnknownObjectException, RemoteException
  {
    return super.activate(id, force);
  }

  /** @inheritDoc */
  public ActivationMonitor activeGroup(ActivationGroupID id,
                                       ActivationInstantiator group,
                                       long incarnation)
      throws UnknownGroupException, ActivationException, RemoteException
  {
    return super.activeGroup(id, group, incarnation);
  }

  /** @inheritDoc */
  public void activeObject(ActivationID id, MarshalledObject obj)
      throws UnknownObjectException, RemoteException
  {
    super.activeObject(id, obj);
  }

  /** @inheritDoc */
  public ActivationDesc getActivationDesc(ActivationID id)
      throws ActivationException, UnknownObjectException, RemoteException
  {
    return super.getActivationDesc(id);
  }

  public ActivationGroupDesc getActivationGroupDesc(ActivationGroupID groupId)
      throws ActivationException, UnknownGroupException, RemoteException
  {
    return super.getActivationGroupDesc(groupId);
  }

  /** @inheritDoc */
  public void inactiveGroup(ActivationGroupID groupId, long incarnation)
      throws UnknownGroupException, RemoteException
  {
    super.inactiveGroup(groupId, incarnation);
  }

  /** @inheritDoc */
  public void inactiveObject(ActivationID id) throws UnknownObjectException,
      RemoteException
  {
    super.inactiveObject(id);
  }

  /** @inheritDoc */
  public ActivationGroupID registerGroup(ActivationGroupDesc groupDesc)
      throws ActivationException, RemoteException
  {
    return super.registerGroup(groupDesc);
  }

  /** @inheritDoc */
  public ActivationID registerObject(ActivationDesc desc)
      throws ActivationException, UnknownGroupException, RemoteException
  {
    return super.registerObject(desc);
  }

  /** @inheritDoc */
  public ActivationDesc setActivationDesc(ActivationID id, ActivationDesc desc)
      throws ActivationException, UnknownObjectException,
      UnknownGroupException, RemoteException
  {
    return super.setActivationDesc(id, desc);
  }

  /** @inheritDoc */
  public ActivationGroupDesc setActivationGroupDesc(
    ActivationGroupID groupId, ActivationGroupDesc groupDesc)
      throws ActivationException, UnknownGroupException, RemoteException
  {
    return super.setActivationGroupDesc(groupId, groupDesc);
  }

  /**
   * This method saves the state of the activation system and then
   * terminates in 10 seconds. 
   */
  public void shutdown() throws RemoteException
  {
    super.shutdown();
    System.out.println("Shutdown command received. Will terminate in 10 s");    
    Persistent.timer.schedule(new Persistent.ExitTask(), 10000);
  }

  /** @inheritDoc */
  public void unregisterGroup(ActivationGroupID groupId)
      throws ActivationException, UnknownGroupException, RemoteException
  {
    super.unregisterGroup(groupId);
  }

  /** @inheritDoc */
  public void unregisterObject(ActivationID id) throws ActivationException,
      UnknownObjectException, RemoteException
  {
    super.unregisterObject(id);
  }

  /**
   * Read the object from the input stream.
   * 
   * @param in the stream to read from
   * 
   * @throws IOException if thrown by the stream
   * @throws ClassNotFoundException
   */
  private void readObject(ObjectInputStream in) throws IOException,
      ClassNotFoundException
  {
    // Read no fields.
  }
  
  /**
   * Write the object to the output stream.
   * 
   * @param out the stream to write int
   * @throws IOException if thrown by the stream
   * @throws ClassNotFoundException
   */
  private void writeObject(ObjectOutputStream out) throws IOException,
      ClassNotFoundException
  {
    // Write no fields.
  }  
  
}
