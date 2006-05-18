/* ServantLocatorPOA.java --
   Copyright (C) 2005, 2006 Free Software Foundation, Inc.

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

import gnu.CORBA.Poa.gnuServantObject;

import org.omg.CORBA.NO_IMPLEMENT;
import org.omg.CORBA.ORB;
import org.omg.CORBA.SystemException;
import org.omg.CORBA.portable.InputStream;
import org.omg.CORBA.portable.InvokeHandler;
import org.omg.CORBA.portable.OutputStream;
import org.omg.CORBA.portable.ResponseHandler;
import org.omg.PortableServer.ServantLocatorPackage.CookieHolder;

/**
 * <p>The ServantLocator stub is an optional base for the
 * servant locators. It cannot serve remote invocations, as
 * methods in {@link ServantLocatorOperations} take POA as one of parameters.
 * Both JDK 1.5 API and OMG specifies that POA is a local object that must not
 * be transferred to the remote invocation target.
 * </p><p>
 * You do not need to derive your servant locator from this stub,
 * it is enough to implement the {@link ServantLocator} interface.
 * But you may choose to do this if you need its functional
 * {@link org.omg.PortableServer.ServantActivatorPOA.delegator#_ids()} 
 * method or want to keep default behaviour during pre- or post- invokcations.
 * </p>
 * @author Audrius Meskauskas, Lithuania (AudriusA@Bioinformatics.org)
 */
public abstract class ServantLocatorPOA
  extends Servant
  implements ServantLocatorOperations, InvokeHandler
{
  /**
   * Used to access the outer class in the nested classes.
   */
  final ServantLocatorPOA THIS = this;

  /**
   * Our implementation will not call this method. After setting your
   * manager to POA, it will call incarnate and etherialize directly.
   */
  public OutputStream _invoke(String method, InputStream input,
                              ResponseHandler handler
                             )
                       throws SystemException
  {
    throw new NO_IMPLEMENT();
  }

  /**
   * Returns an array of interfaces, supported by the servant locator.
   */
  public String[] _all_interfaces(POA poa, byte[] Object_Id)
  {
    return new _ServantLocatorStub()._ids();
  }

  /**
   * Return the complete instance of the servant activator, based on
   * the current class (ServantActivatorPOA or derived).
   */
  public ServantLocator _this()
  {
    return new delegator(this);
  }

  /**
   * Return the complete instance of the servant activator, based on
   * the current class (ServantActivatorPOA or derived).
   */
  public ServantLocator _this(ORB orb)
  {
    return new delegator(this);
  }

  /**
   * This class is used to support _this.
   */
  class delegator
    extends gnuServantObject
    implements ServantLocator
  {
    delegator(Servant s)
    {
      super(s, new byte[ 0 ], null, null);
    }

    public Servant preinvoke(byte[] Object_Id, POA poa, String method,
                             CookieHolder cookie_holder
                            )
                      throws org.omg.PortableServer.ForwardRequest
    {
      return THIS.preinvoke(Object_Id, poa, method, cookie_holder);
    }

    public void postinvoke(byte[] Object_Id, POA poa, String method,
                           java.lang.Object cookie, Servant servant
                          )
    {
      THIS.postinvoke(Object_Id, poa, method, cookie, servant);
    }

    public String[] _ids()
    {
      return THIS._all_interfaces(null, null);
    }
  }
}