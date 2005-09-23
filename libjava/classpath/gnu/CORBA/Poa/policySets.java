/* policySets.java --
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


package gnu.CORBA.Poa;

import org.omg.CORBA.Policy;
import org.omg.PortableServer.IdAssignmentPolicyValue;
import org.omg.PortableServer.IdUniquenessPolicyValue;
import org.omg.PortableServer.ImplicitActivationPolicyValue;
import org.omg.PortableServer.LifespanPolicyValue;
import org.omg.PortableServer.RequestProcessingPolicyValue;
import org.omg.PortableServer.ServantRetentionPolicyValue;
import org.omg.PortableServer.ThreadPolicyValue;

import java.util.ArrayList;

/**
 * Contains the frequently uset POA policy sets. The policy
 * arrays are package private for security reasons, the cloned
 * copies are returned.
 *
 * @author Audrius Meskauskas, Lithuania (AudriusA@Bioinformatics.org)
 */
public class policySets
{
  /**
   * The default policy set, as defined in OMG specs. This is also
   * the policy set for the root POA.
   */
  private static final vPolicy[] rootPOASet =
    new vPolicy[]
    {
      new gnuThreadPolicy(ThreadPolicyValue.ORB_CTRL_MODEL),
      new gnuLifespanPolicy(LifespanPolicyValue.TRANSIENT),
      new gnuIdUniquenessPolicy(IdUniquenessPolicyValue.UNIQUE_ID),
      new gnuIdAssignmentPolicy(IdAssignmentPolicyValue.SYSTEM_ID),
      new gnuServantRetentionPolicy(ServantRetentionPolicyValue.RETAIN),
      new gnuRequestProcessingPolicy(RequestProcessingPolicyValue.USE_ACTIVE_OBJECT_MAP_ONLY),
      new gnuImplicitActivationPolicy(ImplicitActivationPolicyValue.IMPLICIT_ACTIVATION)
    };

  /**
   * Return the policy set, applicable for the root POA, as defined
   * in OMG specs.
   */
  public static Policy[] rootPoa()
  {
    Policy[] p = new Policy[ rootPOASet.length ];
    System.arraycopy(rootPOASet, 0, p, 0, p.length);
    return p;
  }

  /**
   * Convert the potentially incomplete policy array into array, containing
   * the complete policy set.
   *
   * @param policies the policy list, may be incomplete (even zero size).
   *
   * @return the complete policy array. The missing, but needed policies
   * are added with they default values.
   */
  public static Policy[] withDefault(Policy[] policies)
  {
    ArrayList current = new ArrayList(rootPOASet.length);
    Policy p_default;
    boolean specified;

    for (int i = 0; i < rootPOASet.length; i++)
      {
        p_default = rootPOASet [ i ];
        specified = false;
        ForThis:
        for (int j = 0; j < policies.length; j++)
          {
            if (policies [ j ].policy_type() == p_default.policy_type())
              {
                specified = true;
                current.add(policies [ j ]);
                break ForThis;
              }
          }
        if (!specified)
          current.add(p_default.copy());
      }

    Policy[] complete = new Policy[ current.size() ];
    for (int i = 0; i < complete.length; i++)
      {
        complete [ i ] = (Policy) current.get(i);
      }
    return complete;
  }
}