/* StubDelegateImpl.java --
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

import gnu.CORBA.ObjectCreator;
import gnu.CORBA.Unexpected;
import gnu.CORBA.CDR.BufferredCdrInput;
import gnu.CORBA.CDR.BufferedCdrOutput;

import java.io.IOException;
import java.io.ObjectInputStream;
import java.io.ObjectOutputStream;
import java.rmi.Remote;
import java.rmi.RemoteException;

import javax.rmi.PortableRemoteObject;
import javax.rmi.CORBA.Stub;
import javax.rmi.CORBA.StubDelegate;
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
 * The default stub delegate.
 * 
 * @author Wu Gansha (gansha.wu@intel.com) (stub)
 * @author Audrius Meskauskas (AudriusA@Bioinformatics.org) (implementation)
 */
public class StubDelegateImpl
  implements StubDelegate
{
  /**
   * <p>
   * Finds the suitable {@link Tie} for this Stub and connects it to the given
   * ORB. The tie is found by the name pattern. If the found tie is derived from
   * {@link org.omg.CORBA.PortableServer.Servant}, it is connected to the root
   * POA, also activating it (if not already active).
   * </p>
   * <p>
   * This method does not allow to specify, to which POA the found Tie must be
   * connected and requires to use the deprecated method {@link ORB#connect}.
   * Many useful POA features remain unaccessible. A better alternative it might
   * be to generate a {@link org.omg.CORBA.PortableServer.Servant} - derived Tie
   * (-poa key in rmic) and connect it to POA in one of the many ways, listed in
   * the description of the {@link orb.omg.PortableServer} package). The
   * obtained CORBA object can be narrowed into stub using
   * {@link PortableRemoteObject#narrow}.
   * </p>
   * 
   * @param orb the ORB where the Stub must be connected.
   * 
   * @throws RemoteException if the stub is already connected to some other ORB.
   * If the stub is already connected to the ORB that was passed as parameter,
   * the method returns without action.
   * 
   * @throws BAD_PARAM if the name of this stub does not match the stub name
   * pattern, "_*_Stub" or if the Tie class, "_*Impl_Tie", does not exists or an
   * instance of this class cannot be instantiated.
   */
  public void connect(Stub self, ORB orb)
    throws RemoteException
  {
    connect(self, orb, null);
  }

  /**
   * Connect when the POA is specified.
   */
  public static void connect(Stub self, ORB orb, POA poa)
    throws RemoteException
  {
    ORB oorb = null;
    try
      {
        Delegate d = self._get_delegate();
        if (d != null)
          oorb = d.orb(self);
      }
    catch (Exception e)
      {
        // Failed to get Delegate or ORB.
        // (possible ony for user-written Stubs).
      }

    if (oorb != null)
      {
        if (!oorb.equals(orb))
          throw new RemoteException("Stub " + self
            + " is connected to another ORB, " + orb);
        else
          return;
      }

    Tie t = null;
    if (self instanceof Remote)
      t = Util.getTie((Remote) self);

    // Find by name pattern.
    if (t == null)
      t = getTieFromStub(self);

    Delegate delegate;

    if (t instanceof Servant)
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

            ObjectImpl obj = (ObjectImpl) poa.servant_to_reference((Servant) t);
            delegate = obj._get_delegate();
          }
        catch (Exception ex)
          {
            throw new Unexpected(ex);
          }
      }
    else if (t instanceof ObjectImpl)
      {
        ObjectImpl o = (ObjectImpl) t;
        orb.connect(o);
        delegate = o._get_delegate();
      }
    else
      throw new BAD_PARAM("The Tie must be either Servant or ObjectImpl");

    self._set_delegate(delegate);
  }

  /**
   * Locate a tie class, appropriate to the given stub class, by the name
   * pattern.
   */
  public static Tie getTieFromStub(java.lang.Object self)
  {
    Tie t;
    String sn = self.getClass().getName();
    if (!sn.endsWith("_Stub"))
      throw new BAD_PARAM("The stub name, " + sn
        + ", does not match _*_Stub pattern");

    String tn = sn.substring(0, sn.length() - "_Stub".length()) + "Impl_Tie";
    Class tieClass = null;

    try
      {
        tieClass = ObjectCreator.forName(tn);
        t = (Tie) tieClass.newInstance();
        if (self instanceof Remote)
          Util.registerTarget(t, (Remote) self);
      }
    catch (Exception e)
      {
        BAD_PARAM bad = new BAD_PARAM("Unable to instantiate '" + tn + "'");
        bad.initCause(e);
        throw bad;
      }
    return t;
  }

  /**
   * Compare two stubs for equality.
   */
  public boolean equals(Stub self, java.lang.Object obj)
  {
    if (obj instanceof ObjectImpl)
      {
        ObjectImpl other = (ObjectImpl) obj;
        Delegate d1 = other._get_delegate();
        Delegate d2 = self._get_delegate();
        if (d1 == null || d2 == null)
          return d1 == d2;
        else
          return d1.equals(d2);
      }
    else return false;
  }

  /**
   * Get the hash code (from IOR reference).
   */
  public int hashCode(Stub self)
  {
    Delegate d = self._get_delegate();
    return d==null?0:d.hashCode();
  }

  /**
   * Returns the IOR reference of the connected ORB.
   * 
   * @see ORB#object_to_string(org.omg.CORBA.Object);
   */
  public String toString(Stub self)
  {
    try
      {
        return self._orb().object_to_string(self);
      }
    catch (Exception ex)
      {
        return null;
      }
  }

  /**
   * This should never be called. The ORB must be supplied.
   * 
   * @see #connect
   */
  public void readObject(Stub self, ObjectInputStream input)
    throws IOException, ClassNotFoundException
  {
    readObject(self, input, null);
  }

  /**
   * Read as CORBA object when the ORB is known. The ORB must be set under the
   * previous call of Stub.connect. The Stub is automatically registered with
   * this ORB.
   */
  public void readObject(Stub self, ObjectInputStream input, ORB orb)
    throws IOException, ClassNotFoundException
  {
    byte[] b = (byte[]) input.readObject();
    BufferredCdrInput in = new BufferredCdrInput(b);

    if (orb != null)
      in.setOrb(orb);

    ObjectImpl r = (ObjectImpl) in.read_Object();

    self._set_delegate(r._get_delegate());
  }

  /**
   * Write as CORBA object. The ORB is taken from the
   * org.omg.CORBA.portable.Delegate. The Stub is automatically registered with
   * this ORB (if not already done).
   */
  public void writeObject(Stub self, ObjectOutputStream output)
    throws IOException
  {
    writeObject(self, output, null);
  }

  /**
   * Write as CORBA object. The ORB must be either set under the previous call
   * of Stub.connect or it is taken from the org.omg.CORBA.portable.Delegate.
   * The Stub is automatically registered with this ORB (if not already done).
   */
  public void writeObject(Stub self, ObjectOutputStream output, ORB orb)
    throws IOException
  {
    BufferedCdrOutput out = new BufferedCdrOutput();
    out.setOrb(orb == null ? self._orb() : orb);
    out.write_Object(self);

    output.writeObject(out.buffer.toByteArray());
  }
}