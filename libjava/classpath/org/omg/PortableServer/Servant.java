/* Servant.java --
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


package org.omg.PortableServer;

import org.omg.CORBA.BAD_OPERATION;
import org.omg.CORBA.NO_IMPLEMENT;
import org.omg.CORBA.OBJECT_NOT_EXIST;
import org.omg.CORBA.ORB;
import org.omg.PortableServer.POAPackage.ServantNotActive;
import org.omg.PortableServer.POAPackage.WrongPolicy;
import org.omg.PortableServer.portable.Delegate;
import gnu.CORBA.Poa.ORB_1_4;
import gnu.CORBA.Poa.gnuPOA;

/**
 * <p>
 * The servant is responsible for handling the method invocation on the
 * target object. It can be one servant per object, or the same servant can
 * support several (possibly all) objects, associated with the given POA.
 * </p> <p>
 * Till JDK 1.3 inclusive, a typical IDL to java compiler generates an
 * implementation base (name pattern _*ImplBase.java) that is derived from the
 * {@link org.omg.CORBA.portable.ObjectImpl}. Since JDK 1.4 the implementation
 * base is derived from the Servant, also having a different name pattern
 * (*POA.java). This suffix may be confusing, as the servant itself is
 * <i>not</i> POA nor it is derived from it.
 * </p><p>
 * In both cases, the implementation base also inherits an interface, containing
 * definitions of the application specific methods. The application programmer
 * writes a child of the implementation base, implementing these methods
 * for the application-specific functionality. The ObjectImpl is connected
 * directly to the ORB. The Servant is connected to POA that can be obtained
 * from the ORB.
 * </p><p>
 * If the servant is connected to more than one object, the exact object
 * being currently served can be identified with {@link #_object_id}.
 * </p><p>
 * The derivativ of Servant, being directly connected to serve requests,
 * must inherit either from {@link org.omg.CORBA.portable.InvokeHandler}
 * or from {@link org.omg.PortableServer.DynamicImplementation}).
 * </p><p>
 * The Servant type is a CORBA <code>native</code> type.
 * </p>
 *
 * @see POA.servant_to_reference(Servant)
 *
 * @author Audrius Meskauskas, Lithuania (AudriusA@Bioinformatics.org)
 */
public abstract class Servant
{
  /**
   * The delegate, where calls to some Servant methods are forwarded.
   */
  private Delegate delegate;

  /**
   * Get the repository ids of all interfaces, supported by the
   * CORBA object, identified by the passed Id. In the typical code the
   * passed parameters are ignored, returning an array of repository ids,
   * supported by the servant implementation.
   *
   * @param poa a POA of the given object.
   * @param object_ID the object Id of the given object.
   *
   * @return an array, containing the repository ids.
   */
  public abstract String[] _all_interfaces(POA poa, byte[] object_ID);

  /**
   * Get the delegate, where calls to some Servant methods are forwarded.
   */
  public final Delegate _get_delegate()
  {
    return delegate;
  }

  /**
  * Get the interface repository definition <code>InterfaceDef</code> for this
  * Object. By default, forwards request to the delegate.
  *
  * @specnote The interface repository is officially not implemented up till
  * JDK 1.5 inclusive. The delegate throws NO_IMPLEMENT, always.
  */
  public org.omg.CORBA.Object _get_interface_def()
  {
    throw new NO_IMPLEMENT();
  }

  /**
  * Checks if the passed servant is an instance of the given CORBA IDL type.
  * By default, forwards the requet to the delegate.
  *
  * @param a_servant a servant to check.
  * @param an_id a repository ID, representing an IDL type for that the
  * servant must be checked.
  *
  * @return true if the servant is an instance of the given type, false
  * otherwise.
  */
  public boolean _is_a(String repository_id)
  {
    return delegate.is_a(this, repository_id);
  }

  /**
   * Determines if the server object for this reference has already
   * been destroyed. By default, forwards request to the delegate.
   *
   * @return true if the object has been destroyed, false otherwise.
   */
  public boolean _non_existent()
  {
    return delegate.non_existent(this);
  }

  /**
  * Returns the ORB that is directly associated with the given servant.
  * In this implementation, the method is overridden to return
  */
  public final ORB _orb()
  {
    return delegate.orb(this);
  }

  /**
   * Returns the root POA of the ORB instance, associated with this servant.
   * It is the same POA that would be returned by resolving the initial
   * reference "RootPOA" for that orb. By default, forwards request to the
   * delegate.
   *
   * @see ORB.resolve_initial_references
   */
  public POA _default_POA()
  {
    return delegate == null ? null : delegate.default_POA(this);
  }

  /**
  * Return the invocation target object identifier as a byte array.
  * This is typically used when the same servant serves multiple objects,
  * and the object id can encapsulated the whole description of the
  * object.
  *
  * This method returns correct values even when the same
  * servant serves several objects in parallel threads. The ORB maintains the
  * thread to invocation data map for all calls that are currently being
  * processed.
  */
  public final byte[] _object_id()
  {
    if (delegate != null)
      return delegate.object_id(this);
    else
      throw new OBJECT_NOT_EXIST();
  }

  /**
  * Get POA that is directly associated with the given servant.
  * By default, forwards request to the delegate.
  */
  public final POA _poa()
  {
    return delegate.poa(this);
  }

  /**
  * Set the delegate for this servant.
  */
  public final void _set_delegate(Delegate a_delegate)
  {
    delegate = a_delegate;
  }

  /**
  * Obtains the CORBA object reference that is a current invocation target for
  * the given servant. This is important when the same servant serves
  * multiple objects. If the servant is not yet connected to the passed
  * orb, the method will try to connect it to that orb on POA, returned
  * by the method {@link _default_POA}. That method can be overridden to
  * get poa where the object must be automatically connected when
  * calling this method.
  *
  * @param an_orb the ORB with relate to that the object is requested.
  */
  public final org.omg.CORBA.Object _this_object(ORB an_orb)
  {
    if (delegate != null)
      return delegate.this_object(this);
    else
      {
        if (an_orb instanceof ORB_1_4)
          {
            ORB_1_4 m_orb = (ORB_1_4) an_orb;

            gnuPOA dp = (gnuPOA) _default_POA();
            if (dp == null)
              dp = m_orb.rootPOA;

            try
              {
                return dp.servant_to_reference(this);
              }
            catch (WrongPolicy unexp)
              {
                BAD_OPERATION bad = new BAD_OPERATION();
                bad.initCause(unexp);
                throw bad;
              }
            catch (ServantNotActive ex)
              {
                try
                  {
                    return dp.id_to_reference(dp.activate_object(this));
                  }
                catch (Exception unexp)
                  {
                    unexp.initCause(ex);

                    BAD_OPERATION bad = new BAD_OPERATION();
                    bad.initCause(unexp);
                    throw bad;
                  }
              }
          }
      }
    throw new OBJECT_NOT_EXIST();
  }

  /**
  * Obtains the CORBA object reference that is a current invocation target for
  * the given servant. This is important when the same servant serves
  * multiple objects. This method required the servant to be connected
  * to a single orb, and a delegate set.
  *
  * This method returns correct values even when the same
  * servant serves several objects in parallel threads. The ORB maintains the
  * thread to invocation data map for all calls that are currently being
  * processed.
  */
  public final org.omg.CORBA.Object _this_object()
  {
    if (delegate != null)
      return _this_object(_orb());
    else
      {
        POA def = _default_POA();
        if (def instanceof gnuPOA)
          return _this_object(((gnuPOA) def).orb());
      }
    throw new OBJECT_NOT_EXIST();
  }
}