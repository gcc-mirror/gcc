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

import java.lang.reflect.Method;
import java.rmi.server.RemoteRef;
import java.rmi.server.RemoteStub;
import java.rmi.UnexpectedException;

/**
 * This class delegates its method calls to the remote RMI object, referenced
 * by {@link RemoteRef}. 
 *
 * It is normally generated with rmic.
 */
public final class ActivationSystemImpl_Stub 
    extends RemoteStub
    implements ActivationMonitor, Activator, ActivationSystem
{
    /**
     * Use serialVersionUID for interoperability 
     */
    private static final long serialVersionUID = 2;
    
    /**
     * The explaining message for {@ling UnexpectedException}.
     */
    private static final String exception_message = 
      "undeclared checked exception";

     /* All remote methods, invoked by this stub: */
    private static final Method met_setActivationGroupDesc;
    private static final Method met_inactiveGroup;
    private static final Method met_unregisterObject;
    private static final Method met_getActivationDesc;
    private static final Method met_setActivationDesc;
    private static final Method met_shutdown;
    private static final Method met_activate;
    private static final Method met_activeGroup;
    private static final Method met_registerGroup;
    private static final Method met_getActivationGroupDesc;
    private static final Method met_activeObject;
    private static final Method met_registerObject;
    private static final Method met_inactiveObject;
    private static final Method met_unregisterGroup;
    private static final Object[] NO_ARGS = new Object[0];
    static
      {
        final Class[]  NO_ARGSc = new Class[0];      
        try 
          {
             met_setActivationGroupDesc =
               ActivationSystem.class.getMethod("setActivationGroupDesc", new Class[]
                 {
                   ActivationGroupID.class, ActivationGroupDesc.class
                 });
             met_inactiveGroup =
               ActivationMonitor.class.getMethod("inactiveGroup", new Class[]
                 {
                   ActivationGroupID.class, long.class
                 });
             met_unregisterObject =
               ActivationSystem.class.getMethod("unregisterObject", new Class[]
                 {
                   ActivationID.class
                 });
             met_getActivationDesc =
               ActivationSystem.class.getMethod("getActivationDesc", new Class[]
                 {
                   ActivationID.class
                 });
             met_setActivationDesc =
               ActivationSystem.class.getMethod("setActivationDesc", new Class[]
                 {
                   ActivationID.class, ActivationDesc.class
                 });
             met_shutdown =
               ActivationSystem.class.getMethod("shutdown", NO_ARGSc);
             met_activate =
               Activator.class.getMethod("activate", new Class[]
                 {
                   ActivationID.class, boolean.class
                 });
             met_activeGroup =
               ActivationSystem.class.getMethod("activeGroup", new Class[]
                 {
                   ActivationGroupID.class, ActivationInstantiator.class, long.class
                 });
             met_registerGroup =
               ActivationSystem.class.getMethod("registerGroup", new Class[]
                 {
                   ActivationGroupDesc.class
                 });
             met_getActivationGroupDesc =
               ActivationSystem.class.getMethod("getActivationGroupDesc", new Class[]
                 {
                   ActivationGroupID.class
                 });
             met_activeObject =
               ActivationMonitor.class.getMethod("activeObject", new Class[]
                 {
                   ActivationID.class, MarshalledObject.class
                 });
             met_registerObject =
               ActivationSystem.class.getMethod("registerObject", new Class[]
                 {
                   ActivationDesc.class
                 });
             met_inactiveObject =
               ActivationMonitor.class.getMethod("inactiveObject", new Class[]
                 {
                   ActivationID.class
                 });
             met_unregisterGroup =
               ActivationSystem.class.getMethod("unregisterGroup", new Class[]
                 {
                   ActivationGroupID.class
                 });

          }
        catch (NoSuchMethodException nex)
          {
             NoSuchMethodError err = new NoSuchMethodError(
               "ActivationSystemImpl_Stub class initialization failed");
             err.initCause(nex);
             throw err;
          }  
      }
    
    /**
     * Create the instance for _ActivationSystemImpl_Stub that forwards method calls to the
     * remote object.
     *
     * @para the reference to the remote object.
     */
    public ActivationSystemImpl_Stub(RemoteRef reference) 
    {
       super(reference);
    }    
    
    /* Methods */
  /** @inheritDoc */
  public ActivationGroupDesc setActivationGroupDesc(ActivationGroupID p0,
                                                    ActivationGroupDesc p1)
      throws ActivationException, UnknownGroupException, RemoteException
  {
    try
      {
        Object result = ref.invoke(this, met_setActivationGroupDesc,
                                   new Object[] { p0, p1 },
                                   1213918527826541191L);
        return (ActivationGroupDesc) result;
      }
    catch (RuntimeException e)
      {
        throw e;
      }
    catch (RemoteException e)
      {
        throw e;
      }
    catch (Exception e)
      {
        UnexpectedException uex = new UnexpectedException(exception_message);
        uex.detail = e;
        throw uex;
      }
  }
   
  /** @inheritDoc */
  public void inactiveGroup(ActivationGroupID p0, long p1)
      throws UnknownGroupException, RemoteException
  {
    try
      {
        ref.invoke(this, met_inactiveGroup, new Object[] { p0, new Long(p1) },
                   -399287892768650944L);
      }
    catch (RuntimeException e)
      {
        throw e;
      }
    catch (RemoteException e)
      {
        throw e;
      }
    catch (Exception e)
      {
        UnexpectedException uex = new UnexpectedException(exception_message);
        uex.detail = e;
        throw uex;
      }
  }
   
  /** @inheritDoc */
  public void unregisterObject(ActivationID p0) throws ActivationException,
      UnknownObjectException, RemoteException
  {
    try
      {
        ref.invoke(this, met_unregisterObject, new Object[] { p0 },
                   -6843850585331411084L);
      }
    catch (RuntimeException e)
      {
        throw e;
      }
    catch (RemoteException e)
      {
        throw e;
      }
    catch (Exception e)
      {
        UnexpectedException uex = new UnexpectedException(exception_message);
        uex.detail = e;
        throw uex;
      }
  }
   
  /** @inheritDoc */
  public ActivationDesc getActivationDesc(ActivationID p0)
      throws ActivationException, UnknownObjectException, RemoteException
  {
    try
      {
        Object result = ref.invoke(this, met_getActivationDesc,
                                   new Object[] { p0 }, 4830055440982622087L);
        return (ActivationDesc) result;
      }
    catch (RuntimeException e)
      {
        throw e;
      }
    catch (RemoteException e)
      {
        throw e;
      }
    catch (Exception e)
      {
        UnexpectedException uex = new UnexpectedException(exception_message);
        uex.detail = e;
        throw uex;
      }
  }
   
  /** @inheritDoc */
  public ActivationDesc setActivationDesc(ActivationID p0, ActivationDesc p1)
      throws ActivationException, UnknownObjectException,
      UnknownGroupException, RemoteException
  {
    try
      {
        Object result = ref.invoke(this, met_setActivationDesc,
                                   new Object[] { p0, p1 },
                                   7128043237057180796L);
        return (ActivationDesc) result;
      }
    catch (RuntimeException e)
      {
        throw e;
      }
    catch (RemoteException e)
      {
        throw e;
      }
    catch (Exception e)
      {
        UnexpectedException uex = new UnexpectedException(exception_message);
        uex.detail = e;
        throw uex;
      }
  }
   
  /** @inheritDoc */
  public void shutdown() throws RemoteException
  {
    try
      {
        ref.invoke(this, met_shutdown, NO_ARGS, -7207851917985848402L);
      }
    catch (RuntimeException e)
      {
        throw e;
      }
    catch (RemoteException e)
      {
        throw e;
      }
    catch (Exception e)
      {
        UnexpectedException uex = new UnexpectedException(exception_message);
        uex.detail = e;
        throw uex;
      }
  }
   
  /** @inheritDoc */
  public MarshalledObject activate(ActivationID p0, boolean p1)
      throws ActivationException, UnknownObjectException, RemoteException
  {
    try
      {
        Object result = ref.invoke(this, met_activate,
                                   new Object[] { p0, new Boolean(p1) },
                                   -8767355154875805558L);
        return (MarshalledObject) result;
      }
    catch (RuntimeException e)
      {
        throw e;
      }
    catch (RemoteException e)
      {
        throw e;
      }
    catch (Exception e)
      {
        UnexpectedException uex = new UnexpectedException(exception_message);
        uex.detail = e;
        throw uex;
      }
  }
   
  /** @inheritDoc */
  public ActivationMonitor activeGroup(ActivationGroupID p0,
                                       ActivationInstantiator p1, long p2)
      throws UnknownGroupException, ActivationException, RemoteException
  {
    try
      {
        Object result = ref.invoke(this, met_activeGroup,
                                   new Object[] { p0, p1, new Long(p2) },
                                   -4575843150759415294L);
        return (ActivationMonitor) result;
      }
    catch (RuntimeException e)
      {
        throw e;
      }
    catch (RemoteException e)
      {
        throw e;
      }
    catch (Exception e)
      {
        UnexpectedException uex = new UnexpectedException(exception_message);
        uex.detail = e;
        throw uex;
      }
  }
   
  /** @inheritDoc */
  public ActivationGroupID registerGroup(ActivationGroupDesc p0)
      throws ActivationException, RemoteException
  {
    try
      {
        Object result = ref.invoke(this, met_registerGroup,
                                   new Object[] { p0 }, 6921515268192657754L);
        return (ActivationGroupID) result;
      }
    catch (RuntimeException e)
      {
        throw e;
      }
    catch (RemoteException e)
      {
        throw e;
      }
    catch (Exception e)
      {
        UnexpectedException uex = new UnexpectedException(exception_message);
        uex.detail = e;
        throw uex;
      }
  }
   
  /** @inheritDoc */
  public ActivationGroupDesc getActivationGroupDesc(ActivationGroupID p0)
      throws ActivationException, UnknownGroupException, RemoteException
  {
    try
      {
        Object result = ref.invoke(this, met_getActivationGroupDesc,
                                   new Object[] { p0 }, -8701843806548736528L);
        return (ActivationGroupDesc) result;
      }
    catch (RuntimeException e)
      {
        throw e;
      }
    catch (RemoteException e)
      {
        throw e;
      }
    catch (Exception e)
      {
        UnexpectedException uex = new UnexpectedException(exception_message);
        uex.detail = e;
        throw uex;
      }
  }
   
  /** @inheritDoc */
  public void activeObject(ActivationID p0, MarshalledObject p1)
      throws UnknownObjectException, RemoteException
  {
    try
      {
        ref.invoke(this, met_activeObject, new Object[] { p0, p1 },
                   2543984342209939736L);
      }
    catch (RuntimeException e)
      {
        throw e;
      }
    catch (RemoteException e)
      {
        throw e;
      }
    catch (Exception e)
      {
        UnexpectedException uex = new UnexpectedException(exception_message);
        uex.detail = e;
        throw uex;
      }
  }
   
  /** @inheritDoc */
  public ActivationID registerObject(ActivationDesc p0)
      throws ActivationException, UnknownGroupException, RemoteException
  {
    try
      {
        Object result = ref.invoke(this, met_registerObject,
                                   new Object[] { p0 }, -3006759798994351347L);
        return (ActivationID) result;
      }
    catch (RuntimeException e)
      {
        throw e;
      }
    catch (RemoteException e)
      {
        throw e;
      }
    catch (Exception e)
      {
        UnexpectedException uex = new UnexpectedException(exception_message);
        uex.detail = e;
        throw uex;
      }
  }
   
  /** @inheritDoc */
  public void inactiveObject(ActivationID p0) throws UnknownObjectException,
      RemoteException
  {
    try
      {
        ref.invoke(this, met_inactiveObject, new Object[] { p0 },
                   -4165404120701281807L);
      }
    catch (RuntimeException e)
      {
        throw e;
      }
    catch (RemoteException e)
      {
        throw e;
      }
    catch (Exception e)
      {
        UnexpectedException uex = new UnexpectedException(exception_message);
        uex.detail = e;
        throw uex;
      }
  }
   
  /** @inheritDoc */
  public void unregisterGroup(ActivationGroupID p0) throws ActivationException,
      UnknownGroupException, RemoteException
  {
    try
      {
        ref.invoke(this, met_unregisterGroup, new Object[] { p0 },
                   3768097077835970701L);
      }
    catch (RuntimeException e)
      {
        throw e;
      }
    catch (RemoteException e)
      {
        throw e;
      }
    catch (Exception e)
      {
        UnexpectedException uex = new UnexpectedException(exception_message);
        uex.detail = e;
        throw uex;
      }
  }
   
    
}
