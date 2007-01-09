/* RegistryImpl_Stub.java -- Registry stub.
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

package gnu.classpath.tools.rmiregistry;

import java.rmi.AccessException;
import java.rmi.AlreadyBoundException;
import java.rmi.NotBoundException;
import java.rmi.Remote;
import java.rmi.RemoteException;
import java.rmi.registry.Registry;

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
public final class RegistryImpl_Stub 
    extends RemoteStub
    implements Registry
{
    /**
     * Use serialVersionUID for interoperability 
     */
    private static final long serialVersionUID = 3;
    
    /**
     * The explaining message for {@ling UnexpectedException}.
     */
    private static final String exception_message = 
      "undeclared checked exception";

     /* All remote methods, invoked by this stub: */
    private static final Method met_list;
    private static final Method met_rebind;
    private static final Method met_unbind;
    private static final Method met_lookup;
    private static final Method met_bind;
    private static final Object[] NO_ARGS = new Object[0];
    static
      {
        final Class[]  NO_ARGSc = new Class[0];      
        try 
          {
             met_list =
               Registry.class.getMethod("list", NO_ARGSc);
             met_rebind =
               Registry.class.getMethod("rebind", new Class[]
                 {
                   String.class, Remote.class
                 });
             met_unbind =
               Registry.class.getMethod("unbind", new Class[]
                 {
                   String.class
                 });
             met_lookup =
               Registry.class.getMethod("lookup", new Class[]
                 {
                   String.class
                 });
             met_bind =
               Registry.class.getMethod("bind", new Class[]
                 {
                   String.class, Remote.class
                 });

          }
        catch (NoSuchMethodException nex)
          {
             NoSuchMethodError err = new NoSuchMethodError(
               "RegistryImpl_Stub class initialization failed");
             err.initCause(nex);
             throw err;
          }  
      }
    
    /**
     * Create the instance for _RegistryImpl_Stub that forwards method calls to the
     * remote object.
     *
     * @para the reference to the remote object.
     */
    public RegistryImpl_Stub(RemoteRef reference) 
    {
       super(reference);
    }    
    
    /* Methods */    
  /** @inheritDoc */
  public String [] list() 
    throws RemoteException, AccessException
  {
    try
      {
        Object result =  ref.invoke(this, met_list,
                   NO_ARGS,
                   2571371476350237748L);
        return (String []) result;           
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
  public void rebind(String p0, Remote p1) 
    throws RemoteException, AccessException
  {
    try
      {
        ref.invoke(this, met_rebind,
          new Object[] {p0, p1},
          -8381844669958460146L);
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
  public void unbind(String p0) 
    throws RemoteException, NotBoundException, AccessException
  {
    try
      {
        ref.invoke(this, met_unbind,
          new Object[] {p0},
          7305022919901907578L);
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
  public Remote lookup(String p0) 
    throws RemoteException, NotBoundException, AccessException
  {
    try
      {
        Object result =  ref.invoke(this, met_lookup,
                   new Object[] {p0},
                   -7538657168040752697L);
        return (Remote) result;           
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
  public void bind(String p0, Remote p1) 
    throws RemoteException, AlreadyBoundException, AccessException
  {
    try
      {
        ref.invoke(this, met_bind,
          new Object[] {p0, p1},
          7583982177005850366L);
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
