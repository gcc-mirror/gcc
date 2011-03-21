/* DelegateOperations.java --
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


package org.omg.PortableServer.portable;

import org.omg.CORBA.ORB;
import org.omg.PortableServer.POA;
import org.omg.PortableServer.Servant;

/**
 * Each {@link Servant} has an associated delegate, where the most of the calls
 * are forwarded. The delegate is responsible for providing the actual
 * functionality. This class is required to supports a conceptions of
 * the CORBA 2.3.1 Servant.
 *
 * @author Audrius Meskauskas, Lithuania (AudriusA@Bioinformatics.org)
 */
public interface Delegate
{
  /**
  * Returns the root POA of the ORB instance, associated with this servant.
  * It is the same POA that would be returned by resolving the initial
  * reference "RootPOA" for that orb. The default {@link Servant#_default_POA()}
  * method forwards call to the delegate can be overridden to
  * obtain the alternative default POA.
  *
  * @see ORB#resolve_initial_references
  */
  POA default_POA(Servant a_servant);

  /**
  * Get the interface repository defintion
  * <code>InterfaceDef</code> for this Object.
  */
  org.omg.CORBA.Object get_interface_def(Servant a_servant);

  /**
  * Checks if the passed servant is an instance of the given CORBA IDL type.
  *
  * @param a_servant a servant to check.
  * @param an_id a repository ID, representing an IDL type for that the
  * servant must be checked.
  *
  * @return true if the servant is an instance of the given type, false
  * otherwise.
  */
  boolean is_a(Servant a_servant, String an_id);

  /**
   * Determines if the server object for this reference has already
   * been destroyed.
   *
   * @return true if the object has been destroyed, false otherwise.
   */
  boolean non_existent(Servant a_servant);

  /**
  * Return the invocation target object identifier as a byte array.
  */
  byte[] object_id(Servant a_servant);

  /**
  * Returns the ORB that is directly associated with the given servant.
  */
  ORB orb(Servant a_servant);

  /**
  * Get POA that is directly associated with the given servant.
  */
  POA poa(Servant a_servant);

  /**
  * Obtains the CORBA object reference that is an invocation target for the
  * given servant.
  */
  org.omg.CORBA.Object this_object(Servant a_servant);
}
