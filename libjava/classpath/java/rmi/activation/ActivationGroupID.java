/* ActivationGroupID.java --
   Copyright (c) 1996, 1997, 1998, 1999, 2006 Free Software Foundation, Inc.

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

import java.io.Serializable;
import java.rmi.server.UID;

/**
 * This identifier identifies the activation group inside the scope of its
 * activation system. It also contains (and can provide) the reference to the
 * groups activation system.
 *
 * @see ActivationSystem#registerGroup(ActivationGroupDesc)
 */
public class ActivationGroupID
    implements Serializable
{
  /**
   * Use SVUID for interoperability.
   */
  static final long serialVersionUID = - 1648432278909740833L;

  /**
   * The associated activation system.
   */
  final ActivationSystem system;

  /**
   * The object identifier, making the ID unique.
   */
  final UID uid;

  /**
   * Create the new activation group id in the scope of the given activation
   * system
   *
   * @param aSystem the activation system
   */
  public ActivationGroupID(ActivationSystem aSystem)
  {
    system = aSystem;
    uid = new UID();
  }

  /**
   * Get the associated activation system
   *
   * @return the associated activation system
   */
  public ActivationSystem getSystem()
  {
    return system;
  }

  /**
   * Get the hash code of the associated activation system.
   */
  public int hashCode()
  {
    return uid.hashCode();
  }

  /**
   * Copmare for equality, returns true if the passed object is also the
   * activation group id and its activation system is the same.
   */
  public boolean equals(Object obj)
  {
    if (obj instanceof ActivationGroupID)
      {
        ActivationGroupID that = (ActivationGroupID) obj;
        return uid.equals(that.uid);
      }
    else
      return false;
  }

  /**
   * Get the string representation
   */
  public String toString()
  {
    return uid.toString();
  }

}
