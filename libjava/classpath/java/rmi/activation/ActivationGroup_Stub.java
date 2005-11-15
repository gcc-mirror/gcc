/* ActivationGroup_Stub.java -- Stub class for ActivationGroup impls.
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


package java.rmi.activation;

import java.lang.reflect.Method;
import java.rmi.MarshalledObject;
import java.rmi.Remote;
import java.rmi.RemoteException;
import java.rmi.UnexpectedException;
import java.rmi.server.RemoteRef;
import java.rmi.server.RemoteStub;

/**
 * A stub class for {@link ActivationGroup} implementations.
 *
 * @author Roman Kennke (kennke@aicas.com)
 */
public class ActivationGroup_Stub extends RemoteStub
  implements ActivationInstantiator, Remote
{
  private static final long serialVersionUID = 2L;

  /**
   * Creates a new instance of ActivationGroup_Stub.
   *
   * @param ref the remote reference
   */
  public ActivationGroup_Stub(RemoteRef ref)
  {
    super(ref);
  }

  /**
   * Stub method for <code>ActivationGroup.newInstance()</code>.
   *
   * @param id the activation ID
   * @param desc the activation description
   *
   * @return the return value of the invocation
   *
   * @throws RemoteException if the invocation throws a RemoteException
   * @throws ActivationException if the invocation throws an
   *         ActivationException
   */
  public MarshalledObject newInstance(ActivationID id, ActivationDesc desc)
    throws RemoteException, ActivationException
  {
    try
      {
        Method method = ActivationGroup_Stub.class.getDeclaredMethod
                          ("newInstance", new Class[]{ ActivationID.class,
                                                       ActivationDesc.class });
        return (MarshalledObject) ref.invoke(this, method,
                                             new Object[]{id, desc},
                                            -5274445189091581345L);
      }
    catch (RuntimeException ex)
      {
        throw ex;
      }
    catch (RemoteException ex)
      {
        throw ex;
      }
    catch (ActivationException ex)
      {
        throw ex;
      }
    catch (Exception ex)
      {
        throw new UnexpectedException("Unexpected exception", ex);
      }
  }
}
