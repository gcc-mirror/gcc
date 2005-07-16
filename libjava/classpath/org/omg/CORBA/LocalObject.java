/* LocalObject.java --
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


package org.omg.CORBA;

import org.omg.CORBA.BAD_PARAM;
import org.omg.CORBA.Context;
import org.omg.CORBA.ContextList;
import org.omg.CORBA.DomainManager;
import org.omg.CORBA.ExceptionList;
import org.omg.CORBA.NO_IMPLEMENT;
import org.omg.CORBA.NVList;
import org.omg.CORBA.NamedValue;
import org.omg.CORBA.Policy;
import org.omg.CORBA.Request;
import org.omg.CORBA.SetOverrideType;

/**
 * An object, formally implementing the CORBA {@link Object}, but actually
 * handling all invocations locally.
 * Various calls to the remote object specific methods throw the
 * {@link NO_IMPLEMENT}. The explaining message for this exception is
 * specified in java API as <code>"This is a locally constrained object."</code>.
 * It is not possible to get a stringified reference of the local object, or
 * invoke a method using DII (by name via {@link Request} class).
 *
 * However narrowing and widening methods will work with local objects.
 *
 * @since 1.4
 *
 * @author Audrius Meskauskas, Lithuania (AudriusA@Bioinformatics.org)
 */
public class LocalObject
  implements org.omg.CORBA.Object
{
  /**
   * The explaining message for the exception, thrown in response to call
   * the method, not appropriate for the local object.
   */
  private static final String INAPPROPRIATE =
    "This is a locally constrained object.";

  /**
   * This method is not appropriate for the local objects and just
   * throws an exception.
   *
   * @throws NO_IMPLEMENT, always.
   */
  public Request _create_request(Context context, String operation,
                                 NVList parameters, NamedValue returns
                                )
  {
    throw new NO_IMPLEMENT(INAPPROPRIATE);
  }

  /**
   * This method is not appropriate for the local objects and just
   * throws an exception.
   *
   * @throws NO_IMPLEMENT, always.
   */
  public Request _create_request(Context context, String operation,
                                 NVList parameters, NamedValue returns,
                                 ExceptionList exceptions, ContextList ctx_list
                                )
  {
    throw new NO_IMPLEMENT(INAPPROPRIATE);
  }

  /**
   * This method is not appropriate for the local objects and just
   * throws an exception.
   *
   * @throws NO_IMPLEMENT, always.
   */
  public org.omg.CORBA.Object _duplicate()
  {
    throw new NO_IMPLEMENT(INAPPROPRIATE);
  }

  /**
   * This method is not appropriate for the local objects and just
   * throws an exception.
   *
   * @throws NO_IMPLEMENT, always.
   */
  public DomainManager[] _get_domain_managers()
  {
    throw new NO_IMPLEMENT(INAPPROPRIATE);
  }

  /**
   * This method is not appropriate for the local objects and just
   * throws an exception.
   *
   * @throws NO_IMPLEMENT, always.
   */
  public org.omg.CORBA.Object _get_interface_def()
  {
    throw new NO_IMPLEMENT(INAPPROPRIATE);
  }

  /**
   * This method is not appropriate for the local objects and just
   * throws an exception.
   *
   * @throws NO_IMPLEMENT, always.
   */
  public Policy _get_policy(int a_policy_type)
                     throws BAD_PARAM
  {
    throw new NO_IMPLEMENT(INAPPROPRIATE);
  }

  /**
   * {@inheritDoc}
   */
  public int _hash(int maximum)
  {
    return Math.abs(hashCode()) % maximum;
  }

  /**
   * This method is not appropriate for the local objects and just
   * throws an exception.
   *
   * @throws NO_IMPLEMENT, always.
   */
  public boolean _is_a(String repositoryIdentifer)
  {
    throw new NO_IMPLEMENT(INAPPROPRIATE);
  }

  /**
   * Determines if the object is equal to another object, so far as it is
   * possible to determine easily.
   *
   * @param other the other object.
   *
   * @return true if the pased object is not null and
   * java.lang.Object.equals(other) returns true. False otherwise.
   */
  public boolean _is_equivalent(org.omg.CORBA.Object other)
  {
    if (other != null)
      if (other.equals(this))
        return true;

    return false;
  }

  /**
   * Always returs false.
   *
   * @return false, always.
   */
  public boolean _non_existent()
  {
    return false;
  }

  /**
   * This method is not appropriate for the local objects and just
   * throws an exception.
   *
   * @throws NO_IMPLEMENT, always.
   */
  public void _release()
  {
    throw new NO_IMPLEMENT(INAPPROPRIATE);
  }

  /**
   * This method is not appropriate for the local objects and just
   * throws an exception.
   *
   * @specnote it is possible to implement a local handling of the _request(),
   * but the API clearly states that NO_IMPLEMENT
   * must be thrown instead.
   *
   * @throws NO_IMPLEMENT, always.
   */
  public Request _request(String operation)
  {
    throw new NO_IMPLEMENT(INAPPROPRIATE);
  }

  /**
   * This method is not appropriate for the local objects and just
   * throws an exception.
   *
   * @throws NO_IMPLEMENT, always.
   */
  public org.omg.CORBA.Object _set_policy_override(Policy[] policies,
                                                   SetOverrideType how
                                                  )
  {
    throw new NO_IMPLEMENT(INAPPROPRIATE);
  }
}