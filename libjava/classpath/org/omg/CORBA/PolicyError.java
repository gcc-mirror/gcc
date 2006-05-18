/* PolicyError.java --
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


package org.omg.CORBA;

import org.omg.CORBA.portable.IDLEntity;

import java.io.Serializable;

/**
 * The exception is thrown when a policy error occurs.
 * A PolicyError exception may include one of the following error reason codes:
 * {@link BAD_POLICY}, {@link BAD_POLICY_TYPE},
 * {@link BAD_POLICY_VALUE}, {@link UNSUPPORTED_POLICY},
 * {@link UNSUPPORTED_POLICY_VALUE}.
 *
 * @author Audrius Meskauskas, Lithuania (AudriusA@Bioinformatics.org)
 */
public final class PolicyError
  extends UserException
  implements IDLEntity, Serializable
{
  /**
   * Holds the exception reason code, one of
   * {@link BAD_POLICY}, {@link BAD_POLICY_TYPE},
   * {@link BAD_POLICY_VALUE}, {@link UNSUPPORTED_POLICY},
   * {@link UNSUPPORTED_POLICY_VALUE}.
  */
  public short reason;

  /**
   * Use serialVersionUID (v1.4) for interoperability.
   */
  private static final long serialVersionUID = -9196809779974700103L;

  /**
   * Constructs an uninitialised instance of the policy error.
   */
  public PolicyError()
  {
  }

  /**
   * Constructs the policy error with the given reason code.
   * @param a_reason a reason code, one of
   * {@link BAD_POLICY}, {@link BAD_POLICY_TYPE},
   * {@link BAD_POLICY_VALUE}, {@link UNSUPPORTED_POLICY},
   * {@link UNSUPPORTED_POLICY_VALUE}.
   */
  public PolicyError(short a_reason)
  {
    reason = a_reason;
  }

  /**
   * Constructs the policy error with the given reason code and the
   * detailed message.
   * @param a_details the detailed message.
   * @param a_reason a reason code, one of
   * {@link BAD_POLICY}, {@link BAD_POLICY_TYPE},
   * {@link BAD_POLICY_VALUE}, {@link UNSUPPORTED_POLICY},
   * {@link UNSUPPORTED_POLICY_VALUE}.
   */
  public PolicyError(String a_details, short a_reason)
  {
    super(a_details);
    reason = a_reason;
  }
}
