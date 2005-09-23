/* IORInfoOperations.java --
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


package org.omg.PortableInterceptor;

import org.omg.CORBA.Policy;
import org.omg.IOP.TaggedComponent;

/**
 * The ORB service uses this interface to add the service specific components to
 * the new IOR being constructed. The interface provides also possibility to get
 * the POA policies the apply to the IOR being constructed.
 *
 * @author Audrius Meskauskas, Lithuania (AudriusA@Bioinformatics.org)
 */
public interface IORInfoOperations
{
  /**
   * Adds a service-specific component to the IOR profile being constructed.
   *
   * @param tagged_component a tagged component being added.
   *
   * @param profile_id the IOR profile to that the component must be added.
   * The 0 value ({@link org.omg.IOP.TAG_INTERNET_IOP#value}) adds to the
   * Internet profile where host and port are stored by default.
   */
  void add_ior_component_to_profile(TaggedComponent tagged_component,
    int profile_id
  );

  /**
   * Adds a service-specific component to the IOR profile being constructed.
   * The specified component will be included in all profiles, present in the
   * IOR being constructed.
   *
   * @param tagged_component a tagged component being added.
   */
  void add_ior_component(TaggedComponent tagged_component);

  /**
       * Get the server side policy for an IOR being constructed. The method returns
   * policies applying for POA where the object, represented by this IOR, is
   * connected.
   *
   * @param policy_type the type of the policy.
   *
   * @return the policy of the given type that applies to the IOR being
   * constructed.
   *
   * @see org.omg.PortableServer.POAOperations#create_POA
   */
  Policy get_effective_policy(int policy_type);
}