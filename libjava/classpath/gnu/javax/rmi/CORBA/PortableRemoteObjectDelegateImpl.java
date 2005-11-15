/* PortableRemoteObjectDelegateImpl.java -- 
   Copyright (C) 2002, 2004, 2005 Free Software Foundation, Inc.

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


package gnu.javax.rmi.CORBA;

import gnu.CORBA.SimpleDelegate;
import gnu.CORBA.Unexpected;
import gnu.CORBA.Poa.LocalDelegate;
import gnu.CORBA.Poa.ORB_1_4;
import gnu.CORBA.Poa.AOM;

import java.rmi.NoSuchObjectException;
import java.rmi.Remote;
import java.rmi.RemoteException;
import java.rmi.server.RMIClassLoader;

import javax.rmi.CORBA.PortableRemoteObjectDelegate;
import javax.rmi.CORBA.Stub;
import javax.rmi.CORBA.Tie;
import javax.rmi.CORBA.Util;

import org.omg.CORBA.BAD_PARAM;
import org.omg.CORBA.ORB;
import org.omg.CORBA.portable.Delegate;
import org.omg.CORBA.portable.ObjectImpl;
import org.omg.PortableServer.POA;
import org.omg.PortableServer.POAHelper;
import org.omg.PortableServer.Servant;
import org.omg.PortableServer.POAManagerPackage.State;

/**
 * Implements PortableRemoteObjectDelegate.
 * 
 * @author Wu Gansha (gansha.wu@intel.com) (stub)
 * @author Audrius Meskauskas (AudriusA@Bioinformatics.org) (implementation)
 */
public class PortableRemoteObjectDelegateImpl
  implements PortableRemoteObjectDelegate
{
  /**
   * <p>
   * Makes the remote object <code>a_target</code> ready for remote
   * communication using the same communications runtime as for the passed
   * <code>a_source</code> parameter. The a_target is connected to the same
   * ORB (and, if applicable, to the same POA) as the a_source.
   * 
   * @param a_target the target to connect to ORB, must be an instance of either
   * {@link ObjectImpl} (Stubs and old-style ties) or {@link Servant} (POA-bases
   * ties).
   * 
   * @param a_source the object, providing the connection information, must be
   * an instance of either {@link ObjectImpl} (Stubs and old-style ties) or
   * {@link Servant} (POA-bases ties).
   * 
   * @throws RemoteException if the target is already connected to another ORB.
   */
  public void connect(Remote a_target, Remote a_source)
    throws RemoteException
  {
    ORB orb = null;
    POA poa = null;
    boolean ok = false;

    try
      {
        if (a_source instanceof Servant)
          {
            Servant s = (Servant) a_source;
            orb = s._orb();
            poa = s._poa();
            ok = true;
          }

        if (!ok && a_source instanceof ObjectImpl)
          {
            ObjectImpl o = (ObjectImpl) a_source;
            orb = o._orb();
            ok = true;
            try
              {
                if (orb instanceof ORB_1_4)
                  {
                    // POA information available.
                    ORB_1_4 xorb = (ORB_1_4) orb;
                    Delegate d = o._get_delegate();

                    if (d instanceof LocalDelegate)
                      {
                        LocalDelegate l = (LocalDelegate) d;
                        poa = l.poa;
                      }
                    else if (d instanceof SimpleDelegate)
                      {
                        byte[] ior_key = ((SimpleDelegate) d).getIor().key;
                        AOM.Obj ref = xorb.rootPOA.findIorKey(ior_key);
                        if (ref != null)
                          poa = ref.poa;
                      }
                  }
              }
            catch (Exception ex)
              {
                // OK, POA info is not available, but as ORB is available, we
                // will connect in a default way.
              }
          }
      }
    catch (Exception ex)
      {
        RuntimeException rex = new RuntimeException("Unable to get info from "
          + a_source);
        rex.initCause(ex);
        throw rex;
      }

    if (!ok && a_source instanceof Tie)
      {
        Tie t = (Tie) a_source;
        orb = t.orb();
        poa = null;
        ok = true;
      }

    if (orb == null)
      throw new RemoteException("Unable to determine ORB from " + a_source);

    if (a_target instanceof Stub)
      {
        StubDelegateImpl.connect((Stub) a_target, orb, poa);
      }
    else if (a_target instanceof Servant)
      {
        try
          {
            if (poa == null)
              {
                poa = POAHelper.narrow(orb.resolve_initial_references("RootPOA"));
                // Activate if not active.
                if (poa.the_POAManager().get_state().value() == State._HOLDING)
                  poa.the_POAManager().activate();
              }
            poa.servant_to_reference((Servant) a_target);
          }
        catch (Exception ex)
          {
            throw new Unexpected(ex);
          }
      }
    else if (a_target instanceof org.omg.CORBA.Object)
      {
        // Connect as object.
        orb.connect((org.omg.CORBA.Object) a_target);
      }
    else if (a_target instanceof Tie)
      {
        // We avoid calling this because it will aways connect to the root poa.
        ((Tie) a_target).orb(orb);
      }
  }

  /**
   * Narrow the given object to the instance of the given class. The currently
   * supported narrowing types are:
   * 
   * 1. Simple widening conversion.<br>
   * 2. ObjectImpl -> RMI interface.<br>
   * 3. ObjectImpl -> ObjectImpl.<br>
   * 4. Tie -> Remote (implementation)<br>
   * 5. Remote (implementation) -> Tie.<br>
   * 
   * The narrowing has sense only for derived classes.
   */
  public Object narrow(Object narrowFrom, Class narrowTo)
    throws ClassCastException
  {
    if (narrowTo == null)
      throw new ClassCastException("Can't narrow to null class");
    else if (narrowFrom == null)
      return null;
    else
    // Simple narrowing case.
    if (narrowTo.isAssignableFrom(narrowFrom.getClass()))
      return narrowFrom;
    else if (narrowTo.isInterface() || narrowFrom instanceof ObjectImpl)
      {
        // Narrow CORBA object to passed interface.

        String interf = narrowTo.getName();
        String stubClassName;

        stubClassName = getStubClassName(interf);

        try
          {
            // Replace the interface class by the stub class.
            narrowTo = Util.loadClass(stubClassName, null,
              narrowTo.getClassLoader());
          }
        catch (ClassNotFoundException e)
          {
            ClassCastException cex = new ClassCastException("Class not found: "
              + stubClassName);
            cex.initCause(e);
            throw cex;
          }
      }
    else if (narrowFrom instanceof Tie)
      {
        // Try to substitute the return tie target as a return value.
        Remote target = ((Tie) narrowFrom).getTarget();
        if (target != null && narrowTo.isAssignableFrom(target.getClass()))
          return target;
      }

    Object narrowed;
    try
      {
        narrowed = narrowTo.newInstance();
      }
    catch (Exception e)
      {
        ClassCastException cex = new ClassCastException("Cannot instantiate "
          + narrowTo.getName());
        cex.initCause(e);
        throw cex;
      }

    if (narrowed instanceof ObjectImpl)
      {
        // This also works for the instances of the Stub.
        ObjectImpl target = (ObjectImpl) narrowed;
        // Set the delegate, as is done in *Helper.narrow(..).
        target._set_delegate(((ObjectImpl) narrowFrom)._get_delegate());
      }
    else if (narrowed instanceof Tie && narrowFrom instanceof Remote)
      {
        // Try to set the narrowing object as a target for the Tie.
        ((Tie) narrowed).setTarget((Remote) narrowFrom);
      }
    else
      throw new ClassCastException("Narrowing of " + narrowFrom.getClass()
        + " to " + narrowTo + " is either not possible or not implemented.");

    return narrowed;
  }
  
  /**
   * Get the Stub class name for the name, representing the given interface.
   */
  static String getStubClassName(String interf)
  {
    String stubClassName;
    int p = interf.lastIndexOf('.');

    if (p < 0)
      // The interface is defined in the default package.
      stubClassName = "_" + interf + "_Stub";
    else
      stubClassName = interf.substring(0, p + 1) + "_"
        + interf.substring(p + 1) + "_Stub";
    return stubClassName;
  }
  
  /**
   * Get stub for the given implementation, searching by class name pattern. The
   * found stub must implement Remote for this method to succeed.
   */
  public Remote toStub(Remote ObjImpl)
    throws NoSuchObjectException
  {
    String icn = ObjImpl.getClass().getName();
    if (!icn.endsWith("Impl"))
      throw new BAD_PARAM("Invalid class name '" + icn
        + "', must end with 'Impl'");

    String sn = "_" + icn.substring(0, icn.length() - "Impl".length())
      + "_Stub";

    Class stubClass;
    Object o_stub;

    try
      {
        stubClass = RMIClassLoader.loadClass(sn);
        o_stub = stubClass.newInstance();
      }
    catch (Exception e)
      {
        NoSuchObjectException n = new NoSuchObjectException(sn);
        n.initCause(e);
        throw n;
      }

    if (!Remote.class.isAssignableFrom(stubClass))
      throw new ClassCastException(stubClass.getName()
        + " exists but cannot be returned as it does not inherit from "
        + Remote.class.getName());

    return (Remote) o_stub;
  }

  /**
   * If the object tie is no longer in use, disconnet it from the orb.
   */
  public void unexportObject(Remote obj)
    throws NoSuchObjectException
  {
    Util.unexportObject(obj);
  }

  /**
   * Find or create a tie for this target and mark it as being used by the given
   * object.
   */
  public void exportObject(Remote obj)
    throws RemoteException
  {
    if (obj instanceof Stub)
      Util.registerTarget(StubDelegateImpl.getTieFromStub((Stub) obj), obj);
    else if (obj instanceof Tie)
      {
        Tie t = (Tie) obj;
        Util.registerTarget(t, null);
      }
  }

}
