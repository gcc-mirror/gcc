/* gnuIorInfo.java --
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


package gnu.CORBA.Interceptor;

import gnu.CORBA.IOR;
import gnu.CORBA.Poa.ORB_1_4;

import org.omg.CORBA.LocalObject;
import org.omg.CORBA.Policy;
import org.omg.IOP.TaggedComponent;
import org.omg.PortableInterceptor.IORInfo;
import org.omg.PortableServer.POA;

/**
 * Implements IORInfo.
 *
 * @author Audrius Meskauskas, Lithuania (AudriusA@Bioinformatics.org)
 */
public class gnuIorInfo extends LocalObject implements IORInfo
{
  /**
   * Use serialVersionUID for interoperability.
   */
  private static final long serialVersionUID = 1;

  /**
   * The ORB, to that the IOR is related.
   */
  public final ORB_1_4 orb;

  /**
   * The POA, to that IOR is related.
   */
  public final POA poa;

  /**
   * The IOR itself.
   */
  private final IOR ior;

  /**
   * Create an instance.
   */
  public gnuIorInfo(ORB_1_4 an_orb, POA a_poa, IOR an_ior)
  {
    orb = an_orb;
    poa = a_poa;
    ior = an_ior;
  }

  /**
   * Add component to tje specified profile of this IOR.
   */
  public void add_ior_component_to_profile(TaggedComponent tagged_component,
    int profile_id
  )
  {
    ior.add_ior_component_to_profile(tagged_component, profile_id);
  }

  /**
   * Add component to all found profiles in this IOR.
   */
  public void add_ior_component(TaggedComponent tagged_component)
  {
    ior.add_ior_component(tagged_component);
  }

  /**
   * Get the POA policy.
   */
  public Policy get_effective_policy(int policy_type)
  {
    return poa._get_policy(policy_type);
  }

  /**
   * Return the state of the object POA.
   */
  short state()
  {
    return (short) poa.the_POAManager().get_state().value();
  }
}