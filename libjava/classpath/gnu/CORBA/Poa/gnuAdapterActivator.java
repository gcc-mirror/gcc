/* gnuAdapterActivator.java --
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

import org.omg.CORBA.LocalObject;
import org.omg.PortableServer.AdapterActivator;
import org.omg.PortableServer.POA;

/**
 * Defines a simple adapter activator.
 *
 * @author Audrius Meskauskas, Lithuania (AudriusA@Bioinformatics.org)
 */
public class gnuAdapterActivator
  extends LocalObject
  implements AdapterActivator
{
  /**
   * Create a new POA on the parent, using the parent policy set
   * from the suitable parent of grandparend and with independent
   * POA manager (passing null to the createPOA).
   *
   * @param parent a parent. Either this parent or one of its
   * grandparents must be gnuAbstractPOA, able to provide a
   * policy set.
   *
   * @param child_name the name of the child being created.
   *
   * @return true on success or false if no gnuAbstractPOA
   * found till the root poa.
   */
  public boolean unknown_adapter(POA parent, String child_name)
  {
    try
      {
        POA n = parent.create_POA(child_name, null, StandardPolicies.rootPoa());
        n.the_POAManager().activate();
      }
    catch (Exception ex)
      {
        return false;
      }
    return true;
  }
}