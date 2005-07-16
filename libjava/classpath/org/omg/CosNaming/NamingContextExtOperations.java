/* NamingContextExtOperations.java --
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


package org.omg.CosNaming;

import org.omg.CosNaming.NamingContextExtPackage.InvalidAddress;
import org.omg.CosNaming.NamingContextPackage.CannotProceed;
import org.omg.CosNaming.NamingContextPackage.InvalidName;
import org.omg.CosNaming.NamingContextPackage.NotFound;

/**
 * The extended naming context operations, defined since 1.4.
 * The extensions are focused on providing the simplier way
 * to use naming service with the string-based names and
 * addresses.
 *
 * @author Audrius Meskauskas, Lithuania (AudriusA@Bioinformatics.org)
 */
public interface NamingContextExtOperations
  extends NamingContextOperations
{
  /**
   * Resolve the name, represented in the form of the string.
   * The components of the composite name are separated by
   * slash ('/').
   *
   * @param a_name_string the name to resolve.
   * @return the object, referenced by the name.
   */
  org.omg.CORBA.Object resolve_str(String a_name_string)
                            throws NotFound, CannotProceed, InvalidName;

  /**
   * Converts the name, represented in the form of the string,
   * into the older name representation (array of the name
   * components).
   *
   * @param a_name_string the stringified form of the name.
   *
   * @return the component array form of the name.
   *
   * @throws InvalidName if the name is invalid.
   */
  NameComponent[] to_name(String a_name_string)
                   throws InvalidName;

  /**
   * Converts the older representation for the name (array
   * of the name components) into the string form of the name.
   *
   * @param a_name the name, as an array of components.
   *
   * @return the same name as a string.
   *
   * @throws InvalidName if the name is invalid.
   */
  String to_string(NameComponent[] a_name)
            throws InvalidName;

  String to_url(String an_address, String a_name_string)
         throws InvalidAddress, InvalidName;
}
