/* ForwardRequest.java --
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

import org.omg.CORBA.UserException;
import org.omg.CORBA.portable.IDLEntity;

import java.io.Serializable;

/**
 * <p>
 * This exception is raised by {@link ServantManager} to indicate that the
 * invocation target has moved to another known location. In this case,
 * the client will receive a redirection (LOCATION_FORWARD) message and should
 * resend the request to the new target. The exception contains the object
 * reference, indicating the new location.
 * </p><p>
 * The exception can be thrown both by servant locators and servant activators.
 * If the exception is raised anywhere else than in the ServantManager
 * methods, it is handled as an ordinary user excepton.
 * </p>
 *
 * @author Audrius Meskauskas, Lithuania (AudriusA@Bioinformatics.org)
*/
public final class ForwardRequest
  extends UserException
  implements IDLEntity, Serializable
{
  /**
   * Use serialVersionUID (v1.4) for interoperability.
   */
  private static final long serialVersionUID = -4159318367582473975L;

  /**
   * The object reference, indicating the new location of the invocation target.
   */
  public org.omg.CORBA.Object forward_reference;

  /**
   * Create ForwardRequest with no explaining message and stating the
   * new location is <code>null</code>.
   */
  public ForwardRequest()
  {
  }

  /**
   * Create the ForwardRequest with explaining message and
   * initialising the object reference to the given value.
   *
   * @param why a string, explaining, why this exception has been thrown.
   * @param a_forward_reference a value for forward_reference.
   */
  public ForwardRequest(String why, org.omg.CORBA.Object a_forward_reference)
  {
    super(why);
    this.forward_reference = a_forward_reference;
  }

  /**
   * Create the ForwardRequest without explaining
   * message and initialising the object reference to the given value.
   *
   * @param a_forward_reference a value for forward_reference.
   */
  public ForwardRequest(org.omg.CORBA.Object a_forward_reference)
  {
    this.forward_reference = a_forward_reference;
  }
}
