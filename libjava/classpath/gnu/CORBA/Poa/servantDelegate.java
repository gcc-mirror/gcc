/* servantDelegate.java --
   Copyright (C) 2005 Free Software Foundation, Inc.

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


package gnu.CORBA.Poa;

import gnu.CORBA.Unexpected;

import org.omg.CORBA.NO_IMPLEMENT;
import org.omg.CORBA.ORB;
import org.omg.CORBA.ORBPackage.InvalidName;
import org.omg.CORBA.Object;
import org.omg.PortableServer.CurrentPackage.NoContext;
import org.omg.PortableServer.POA;
import org.omg.PortableServer.POAHelper;
import org.omg.PortableServer.Servant;
import org.omg.PortableServer.portable.Delegate;

/**
 * The implementation of the servant delegate for the locally existing
 * servant.The associated servant that must also implement the
 * {@link InvokeHandler} interface. Each servant requires a separate
 * instance of this delegate and can serve a single object only.
 * Hence the fields are final, but the delegate is typically reused
 * unless the same servant is connected to different objects.
 *
 * @author Audrius Meskauskas, Lithuania (AudriusA@Bioinformatics.org)
 */
public class servantDelegate
  implements Delegate
{
  /**
   * The servant, associated with this object.
   */
  final Servant servant;

  /**
   * The servant (not object) id.
   */
  final byte[] servant_id;

  /**
   * The POA, where the servant is connected.
   */
  final gnuPOA poa;

  /**
   * The object, exposed as an object, served by this servant.
   */
  final gnuServantObject object;

  /**
   * Create the delegat for the servant that will be connected to the
   * given poa. The method is normally called from inside of gnuPOA.
   * The constructor sets the newly created delegate as the delegate to this
   * servant by calling Servant._set_delegate.
   *
   * @param a_poa the poa.
   * @param a_servant the servant.
   * @param a_servant_id the servant id.
   */
  public servantDelegate(Servant a_servant, gnuPOA a_poa, byte[] a_servant_id)
  {
    poa = a_poa;
    servant = a_servant;
    servant_id = a_servant_id;
    servant._set_delegate(this);
    object =
      new gnuServantObject(servant, servant_id, (ORB_1_4) servant._orb(), a_poa);
    object._set_delegate(new LocalDelegate(object, poa, a_servant_id));
  }

  /**
   * Check if this object could be named by the given repository id.
   * @param idl_id the repository id to check.
   *
   * @return true if it is one of the possible repository ids of this
   * object.
   */
  public boolean is_a(Servant a_servant, String idl_id)
  {
    same(a_servant);

    String[] maybe = object.repository_ids;
    if (maybe == null)
      maybe = servant._all_interfaces(poa, object.Id);
    for (int i = 0; i < maybe.length; i++)
      {
        if (maybe [ i ].equals(idl_id))
          return true;
      }
    return false;
  }

  /**
   * Return the ORB's default POA.
   */
  public POA default_POA(Servant a_servant)
  {
    same(a_servant);
    try
      {
        return POAHelper.narrow(orb(a_servant).resolve_initial_references("RootPOA"));
      }
    catch (InvalidName ex)
      {
        throw new Unexpected(ex);
      }
  }

  /**
   * Get ORB.
   */
  public ORB orb(Servant a_servant)
  {
    same(a_servant);
    return poa.orb();
  }

  /**
   * Get the object, exposing the servant.
   */
  public Object this_object(Servant a_servant)
  {
    same(a_servant);
    try
      {
        return poa.aom.get(poa.m_orb.currents.get_object_id()).object;
      }
    catch (NoContext ex)
      {
        return object;
      }
  }

  /**
   * Not supported.
   *
   * @specnote Same as for Sun up till 1.5 inclusive.
   */
  public Object get_interface_def(Servant a_servant)
  {
    same(a_servant);
    throw new NO_IMPLEMENT();
  }

  /**
   * Get the Id of the object being currently served.
   */
  public byte[] object_id(Servant a_servant)
  {
    same(a_servant);
    try
      {
        byte[] id = poa.m_orb.currents.get_object_id();
        return id;
      }
    catch (NoContext ex)
      {
        return object.Id;
      }
  }

  /**
   * Always returns false;
   */
  public boolean non_existent(Servant a_servant)
  {
    same(a_servant);
    return false;
  }

  /**
   * Return the associated POA.
   */
  public POA poa(Servant a_servant)
  {
    same(a_servant);
    try
      {
        return poa.m_orb.currents.get_POA();
      }
    catch (NoContext ex)
      {
        return poa;
      }
  }

  /**
   * Checks if the passed servant is the same as the servant, associated with
   * this delegate. This class requires a single servant per delegate.
   */
  void same(Servant some_servant)
  {
    if (servant != some_servant)
      throw new InternalError("Only one servant per delegate is supported.");
  }
}