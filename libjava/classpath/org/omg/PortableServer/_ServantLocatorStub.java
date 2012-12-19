/* _ServantLocatorStub.java --
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

import org.omg.CORBA.portable.ObjectImpl;
import org.omg.PortableServer.ServantLocatorPackage.CookieHolder;

import java.io.Serializable;

/**
 * <p>The ServantLocator stub is an optional base for the
 * servant locators. This stub cannot accept remote invocations, as
 * methods in {@link ServantLocatorOperations} take POA as one of parameters.
 * Both JDK 1.5 API and OMG specifies that POA is a local object that must not
 * be transferred to the remote invocation target.
 * </p><p>
 * You do not need to derive your servant locator from this stub,
 * it is enough to implement the {@link ServantLocator} interface.
 * But you may choose to do this if you need its functional
 * {@link #_ids()} method or want to keep default behavior during per-
 * or post- invokcations.
 * </p>
 * @author Audrius Meskauskas, Lithuania (AudriusA@Bioinformatics.org)
 */
public class _ServantLocatorStub
  extends ObjectImpl
  implements ServantLocator, Serializable
{
  /**
   * Use serialVersionUID (v1.4) for interoperability.
   */
  private static final long serialVersionUID = -2374963516905770111L;

  /**
   * This the purpose of this field is undocumented up till 1.5 java API
   * inclusive.
   */
  @SuppressWarnings("rawtypes") // Needed for API compatibility
  public static final Class _opsClass = ServantLocatorOperations.class;

  /**
   * The package private string, used as a parameter for
   * the throws NullPointerExceptions in both servant locator and activator
   * stubs.
   */
  static final String OVERRIDE = "Override this method to get functionality.";

  /**
   * Return the array of repository ids for this object, stating that it is
   * both Servant locator and Servant manager.
   *
   * @return { "IDL:omg.org/PortableServer/ServantLocator:1.0",
   *           "IDL:omg.org/PortableServer/ServantManager:1.0" }, always.
   */
  public String[] _ids()
  {
    return new String[]
           {
             "IDL:omg.org/PortableServer/ServantLocator:1.0",
             "IDL:omg.org/PortableServer/ServantManager:1.0"
           };
  }

  /**
   * It is your responsibility to take the preinvoke actions, if any,
   * and also supply an appropriate servant for the current invocation.
   *
   * The default method instructs POA that the servant cannot be
   * provided by locator. The OBJ_ADAPTER exception will be
   * thrown by POA, unless it uses the available default servant for all
   * invocations.
   *
   * @specnote in GNU Classpath, returning null means that the
   * locator does not supply the servant.
   *
   * @see ServantLocatorOperations#preinvoke
   */
  public Servant preinvoke(byte[] Object_id, POA poa, String method,
                           CookieHolder cookie
                          )
                    throws ForwardRequest
  {
    return null;
  }

  /**
   * It is your responsibility to take the postinvoke actions, if any,
   * by overriding this method. The default method does nothing.
   *
   * @see ServantLocatorOperations#postinvoke
   */
  public void postinvoke(byte[] Object_id, POA poa, String method,
                         Object cookie, Servant servant
                        )
  {
  }
}
