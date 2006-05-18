/* ActivationSystemTransient.java -- The transient RMI object activation system.
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

import java.rmi.MarshalledObject;
import java.rmi.RemoteException;
import java.rmi.activation.ActivationDesc;
import java.rmi.activation.ActivationException;
import java.rmi.activation.ActivationGroup;
import java.rmi.activation.ActivationGroupDesc;
import java.rmi.activation.ActivationGroupID;
import java.rmi.activation.ActivationID;
import java.rmi.activation.ActivationInstantiator;
import java.rmi.activation.ActivationMonitor;
import java.rmi.activation.ActivationSystem;
import java.rmi.activation.Activator;
import java.rmi.activation.UnknownGroupException;
import java.rmi.activation.UnknownObjectException;
import java.util.HashMap;
import java.util.Hashtable;
import java.util.Iterator;
import java.util.Map;

/**
 * Provides the default transient activation system.
 * 
 * @author Audrius Meskauskas (audriusa@bioinformatics.org)
 */
public class ActivationSystemTransient
    extends DefaultActivationSystem
    implements ActivationSystem, ActivationMonitor, Activator
{
  /**
   * Maps group identifiers into group descriptions.
   */
  protected final BidiTable groupDescs;  
  
  /**
   * Maps object identifiers into object activation descriptions
   */
  protected final BidiTable descriptions;

  /**
   * Maps group identifiers into already activated groups.
   */
  protected transient final Map groupInstantiators = new Hashtable();
  
  /**
   * The cache of the activated objects, maps activation ids to remote
   * object stubs.
   */
  protected transient final Map activatedObjects = new HashMap();
 
  /**
   * The object incarnation counter.
   */
  static long groupIncarnations = 0;
  
  /**
   * The singleton of this activation system
   */
  static ActivationSystem singleton;
  
  /**
   * Set to true to print the event messages to console.
   */
  public static boolean debug = false;
  
  
  /**
   * Creates the group which uses the given maps to store the data.
   */
  protected ActivationSystemTransient(BidiTable objectDescriptions,
                                      BidiTable groupDescriptiopns)
  {
    descriptions = objectDescriptions;
    groupDescs = groupDescriptiopns;
  }
  
  /**
   * Creates the group with transient maps.
   */
  protected ActivationSystemTransient()
  {
    this (new BidiTable(), new BidiTable());
  }
  
  public static ActivationSystem getInstance()
  {
    if (singleton == null)
      singleton = new ActivationSystemTransient();
    return singleton;
  }
  
  /**
   * Activate the given object (try cache first if force = false)
   */
  public MarshalledObject activate(ActivationID id, boolean force)
      throws ActivationException, UnknownObjectException, RemoteException
  {
    if (! force)
      {
        synchronized (activatedObjects)
          {
            MarshalledObject object = (MarshalledObject) activatedObjects.get(id);
            if (object != null)
              return object;
          }
      }

    ActivationDesc desc = (ActivationDesc) descriptions.get(id);
    if (desc == null)
      throw new UnknownObjectException("Activating unknown object  "+
                                       id == null ? "null" : id.toString());

    ActivationInstantiator group = 
      (ActivationInstantiator) groupInstantiators.get(desc.getGroupID());

    if (group == null)
      {
        // The group is not active - must be activated.
        ActivationGroupID gid = desc.getGroupID();
        ActivationGroupDesc adesc = (ActivationGroupDesc) groupDescs.get(gid);

        if (adesc == null)
          throw new UnknownGroupException("Activating unknown group " 
                                          + gid + " for "+ id+" this "+this);

        synchronized (ActivationSystemTransient.class)
          {
            groupIncarnations++;
          }

        group = ActivationGroup.createGroup(gid, adesc, groupIncarnations);
        activeGroup(gid, group, groupIncarnations);
      }

    MarshalledObject object = group.newInstance(id, desc);

    synchronized (activatedObjects)
      {
        activatedObjects.put(id, object);
      }
    return object;
  }
  
  /**
   * Returns the activation monitor (THIS) and remebers the instantiator, used
   * by that group.
   */
  public ActivationMonitor activeGroup(ActivationGroupID id,
                                       ActivationInstantiator group,
                                       long incarnation)
      throws UnknownGroupException, ActivationException, RemoteException
  {
    groupInstantiators.put(id, group);
    return this;
  }
  
  /**
   * Get the activation descriptor for the given activation id.
   * 
   * @return the activation descriptor, never null.
   * @throws UnknownObjectException if such object is unknown.
   */
  public ActivationDesc getActivationDesc(ActivationID id)
      throws ActivationException, UnknownObjectException, RemoteException
  {
    ActivationDesc desc = (ActivationDesc) descriptions.get(id);
    if (desc == null)
      throw new UnknownObjectException("No desc for "+
                                       id == null ? "null" : id.toString());
    return desc;
  }
  
  /**
   * Get the descriptor of the given activation group.
   * 
   * @return the activation group descriptor, never null.
   * @throws UnknownGroupException if such group is unknown
   */
  public ActivationGroupDesc getActivationGroupDesc(ActivationGroupID groupId)
      throws ActivationException, UnknownGroupException, RemoteException
  {
    ActivationGroupDesc desc = (ActivationGroupDesc) groupDescs.get(groupId);
    if (desc == null)
      throw new UnknownGroupException(groupId == null ? "null"
                                                     : groupId.toString());
    return desc;
  }

  /**
   * Create the activation group id and put this id-descriptor combination into
   * the group map. The new ID will only be created if this description has not
   * already been registered, otherwise the id of the registered description
   * will be returned.
   */
  public ActivationGroupID registerGroup(ActivationGroupDesc groupDesc)
      throws ActivationException, RemoteException
  {
    ActivationGroupID id = (ActivationGroupID) groupDescs.getKey(groupDesc);
    if (id == null)
      {
        id = new ActivationGroupID(this);
        groupDescs.put(id, groupDesc);
      }
    if (debug)
      System.out.println("Register group " + id +":"+groupDesc+" this "+this);

    return id;
  }
  
  /**
   * Create the object activation id and put this id-descriptor combination into
   * the group map. The new ID will only be created if this description has not
   * already been registered, otherwise the id of the registered description
   * will be returned.
   */
  public ActivationID registerObject(ActivationDesc desc)
      throws ActivationException, UnknownGroupException, RemoteException
  {
    ActivationID id = (ActivationID) descriptions.getKey(desc);
    if (id == null)
      {
        id = new ActivationID(this);
        descriptions.put(id, desc);
      }
    
    if (debug)
      System.out.println("Register object " + id +":"+desc+" this "+this);
    
    return id;
  }
  
  /**
   * Replace the activation descriptor, return the previous descriptor.
   */
  public ActivationDesc setActivationDesc(ActivationID id, ActivationDesc desc)
      throws ActivationException, UnknownObjectException,
      UnknownGroupException, RemoteException
  {
    ActivationDesc prev = getActivationDesc(id);
    descriptions.put(id, desc);
    return prev;
  }
   
  /**
   * Replace the activation group descriptor, return the previous descriptor.
   */
  public ActivationGroupDesc setActivationGroupDesc(
                                                    ActivationGroupID groupId,
                                                    ActivationGroupDesc groupDesc)
      throws ActivationException, UnknownGroupException, RemoteException
  {
    ActivationGroupDesc prev = getActivationGroupDesc(groupId);
    groupDescs.put(groupId, groupDesc);
    return prev;
  }
  
  /**
   * Calls .shutdown on all bidirectional tables (has no effect if these
   * table are not persistent).
   */
  public void shutdown() throws RemoteException
  {
    descriptions.shutdown();
    groupDescs.shutdown();
  }
  
  /**
   * Remove the group from the group map
   */
  public void unregisterGroup(ActivationGroupID groupId) throws ActivationException,
      UnknownGroupException, RemoteException
  {
    if (! groupDescs.containsKey(groupId))
      throw new UnknownGroupException("Unknown group "+groupId);
    
    groupDescs.removeKey(groupId);
    groupInstantiators.remove(groupId);
  }
  
  /**
   * Remove the object id from the active object and description maps.
   */
  public void unregisterObject(ActivationID id) throws ActivationException,
      UnknownObjectException, RemoteException
  {
    if (! descriptions.containsKey(id))
      throw new UnknownObjectException("Unregistering unknown object");
    descriptions.removeKey(id);

    synchronized (activatedObjects)
      {
        activatedObjects.remove(id);
      }
  }
  
  /**
   * Put the object into active object map.
   */
  public void activeObject(ActivationID id, MarshalledObject obj)
      throws UnknownObjectException, RemoteException
  {
    if (! descriptions.containsKey(id))
      throw new UnknownObjectException("Activating unknown object "+
                                        id+" this "+this);
    try
      {
        synchronized (activatedObjects)
          {
            activatedObjects.put(id, obj.get());
          }
      }
    catch (RemoteException e)
      {
        throw e;
      }
    catch (Exception e)
      {
        UnknownObjectException un = new UnknownObjectException(
          "Cannot get Remote for MarshalledObject of "+id);
        un.detail = e;
        throw un;
      }
  }
  
  /**
   * Check if the group is known. Remove all active objects, belonging to
   * that group, from the active object cache.
   */
  public void inactiveGroup(ActivationGroupID groupId, long incarnation)
      throws UnknownGroupException, RemoteException
  {
    if (! groupInstantiators.containsKey(groupId))
      throw new UnknownGroupException("Inactivating unkwnon group");
    
    groupInstantiators.remove(groupId);
    
    // Remove all members of this group from the cache.
    synchronized (activatedObjects)
    {
      Iterator iter = activatedObjects.keySet().iterator();
      ActivationID id;
      ActivationDesc desc;
      while (iter.hasNext())
        {
          id = (ActivationID) iter.next();
          desc = (ActivationDesc) descriptions.get(id);
          if (desc.getGroupID().equals(groupId))
            activatedObjects.remove(id);
        }
    }
  }

  /**
   * Removes this id from the active object cache.
   */
  public void inactiveObject(ActivationID id) throws UnknownObjectException,
      RemoteException
  {
    if (! descriptions.containsKey(id))
      throw new UnknownObjectException("Inactivating unknown object");

    synchronized (activatedObjects)
      {
        activatedObjects.remove(id);
      }
  }
}
