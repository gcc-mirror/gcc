/* ServantActivatorPOA.java --
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

/**
 * <p>This ServantActivator stub is an optional base for the
 * servant activators. This stub cannot serve remote invocations, as
 * methods in {@link ServantActivatorOperations} take POA as one of parameters.
 * Both JDK 1.5 API and OMG specifies that POA is a local object that must not
 * be transferred to the remote invocation target.
 * </p><p>
 * You do not need to derive your servant activator from this stub,
 * it is enough to implement the {@link ServantActivator} interface.
 * But you may choose to do this if you need the functional
 * {@link #_all_interfaces(POA, byte[])} method or want to keep default 
 * behavior during the incarnation or etherialization.
 * </p>
 */
public abstract class ServantActivatorPOA
  extends Servant
  implements InvokeHandler, ServantActivatorOperations
{
  /**
   * Used to access the outer class in the nested delegator class.
   */
  final ServantActivatorPOA THIS = this;

  /**
   * This class is used to support _this.
   */
  class delegator
    extends gnuServantObject
    implements ServantActivator
  {
    delegator(Servant s)
    {
      super(s, new byte[ 0 ], null, null);
    }

    public Servant incarnate(byte[] key, POA poa)
                      throws org.omg.PortableServer.ForwardRequest
    {
      return THIS.incarnate(key, poa);
    }

    public void etherealize(byte[] key, POA poa, Servant servant,
                            boolean cleanup, boolean remains
                           )
    {
      THIS.etherealize(key, poa, servant, cleanup, remains);
    }
  }

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
   * Returns an array of interfaces, supported by the servant activator.
   */
  public String[] _all_interfaces(POA poa, byte[] Object_Id)
  {
    return new _ServantActivatorStub()._ids();
  }

  /**
   * Return the complete instance of the servant activator, based on
   * the current class (ServantActivatorPOA or derived).
   */
  public ServantActivator _this()
  {
    return new delegator(this);
  }

  /**
   * Return the complete instance of the servant activator, based on
   * the current class (ServantActivatorPOA or derived).
   */
  public ServantActivator _this(ORB orb)
  {
    return new delegator(this);
  }
}