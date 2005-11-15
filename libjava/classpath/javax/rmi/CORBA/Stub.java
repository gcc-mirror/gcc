/* Stub.java --
   Copyright (C) 2004, 2004, 2005 Free Software Foundation, Inc.

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


package javax.rmi.CORBA;

import gnu.javax.rmi.CORBA.DelegateFactory;
import gnu.javax.rmi.CORBA.StubDelegateImpl;

import java.io.IOException;
import java.io.ObjectInputStream;
import java.io.ObjectOutputStream;
import java.io.Serializable;
import java.rmi.RemoteException;

import javax.rmi.PortableRemoteObject;

import org.omg.CORBA.ORB;
import org.omg.CORBA_2_3.portable.ObjectImpl;

/**
 * A Stub descendants provide access to the object on the client side. This base
 * class implements methods, required for remote or local invocation using CORBA
 * mechanisms. The most of the functionality is forwarded to the stub delegate.
 * This delegate can be altered by setting the system property
 * "javax.rmi.CORBA.StubClass" to the name of the alternative class that must
 * implement {@link StubDelegate}. Hence Stub contains two delegates, one for
 * Stub-related operations and another inherited from the ObjectImpl.
 * 
 * @specnote GNU Classpath uses separate delegate per each Stub. The delegate
 * holds information about the ORB and other data, specific for the each Stub.
 * 
 * @author Audrius Meskauskas (AudriusA@Bioinformatics.org)
 */
public abstract class Stub
  extends ObjectImpl
  implements Serializable
{
  /**
   * For compatability with Sun's JDK 1.4.2 rev. 5
   */
  private static final long serialVersionUID = 1087775603798577179L;

  /**
   * The hashcode, computed once (expensive operation).
   */
  transient int m_hash = Integer.MIN_VALUE;

  /**
   * The stringified reference, computed once (expensive operation).
   */
  transient String m_ior;

  /**
   * The ORB, where the stub is connected on the client side.
   */
  transient ORB m_orb;

  /**
   * The associated delegate, responsible for the major of the functionality of
   * this stub.
   */
  static StubDelegate delegate = (StubDelegate) DelegateFactory.getInstance(DelegateFactory.STUB);

  /**
   * Returns the same hashcode for all stubs that point to the same remote
   * object.
   */
  public int hashCode()
  {
    if (m_hash == Integer.MIN_VALUE)
      m_hash = delegate.hashCode(this);
    // This should finally result to the IOR comparison.
    return m_hash;
  }

  /**
   * The stubs are equal if they point to the same remote object.
   */
  public boolean equals(java.lang.Object obj)
  {
    return delegate.equals(this, obj);
  }

  /**
   * Get the string representation of this Stub.
   * 
   * @return the CORBA IOR reference.
   */
  public String toString()
  {
    if (m_ior == null)
      m_ior = delegate.toString(this);
    return m_ior;
  }

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
   * <p>
   * It is frequently easier to call {@link PortableRemoteObject#connect} rather
   * than this method.
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
  public void connect(ORB orb)
    throws RemoteException
  {
    if (m_orb != null && orb != null)
      {
        if (m_orb.equals(orb))
          throw new RemoteException("Stub " + this
            + " is connected to another ORB, " + orb);
        else
          return;
      }
    m_orb = orb;
    delegate.connect(this, orb);
  }

  /**
   * Required by serialized form of Java API doc.
   */
  private void readObject(ObjectInputStream input)
    throws IOException, ClassNotFoundException
  {
    if (delegate instanceof StubDelegateImpl)
      ((StubDelegateImpl) delegate).readObject(this, input, m_orb);
    else
      delegate.readObject(this, input);
  }

  /**
   * Required by serialized form of Java API doc.
   */
  private void writeObject(ObjectOutputStream output)
    throws IOException
  {
    // The m_orb in this case may be either known or not.
    if (delegate instanceof StubDelegateImpl)
      ((StubDelegateImpl) delegate).writeObject(this, output, m_orb);
    else

      delegate.writeObject(this, output);
  }
}