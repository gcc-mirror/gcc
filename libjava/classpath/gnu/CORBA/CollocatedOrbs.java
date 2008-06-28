/* CollocatedOrbs.java -- Handles collocations
   Copyright (C) 2006 Free Software Foundation, Inc.

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


package gnu.CORBA;

import gnu.CORBA.Poa.gnuServantObject;

import java.net.InetAddress;
import java.net.UnknownHostException;
import java.util.ArrayList;

/**
 * This class provides support for the direct method invocations without
 * involving the network in the case when both ORBs run on the same java
 * virtual machine. Special attention is only needed when call is made
 * between two independent ORBs, instantiated via ORB.init. The call to the
 * object, obtained via IOR reference from the ORB where it was locally
 * connected is always local anyway.
 * 
 * For security reasons it may be sensible to keep this class and all support
 * package private.
 * 
 * @author Audrius Meskauskas
 */
class CollocatedOrbs
{
  /**
   * This field is used in automated Classpath specific testing to disable
   * the direct calls. 
   */
  static boolean DIRECT_CALLS_ALLOWED = true;
  
   /**
    * Containts the references of the all running GNU Classpath ORBs in the
    * local virtual machine. GNU Classpath ORBs register themselves here when
    * created and unregister when either ORB.destroy is called or in the 
    * finalizer.
    */
   private static ArrayList orbs = new ArrayList();
   
   /**
    * The address of the local host.
    */
   static String localHost;
   
   static
    {
      try
        {
          localHost = InetAddress.getLocalHost().getHostAddress();
        }
      catch (UnknownHostException ex)
        {
          throw new InternalError("Local host is not accessible:" + ex);
        }
    }
   
   /**
     * Register the new ORB
     * 
     * @param orb the orb to register
     */
  static void registerOrb(OrbFunctional orb)
  {
    if (DIRECT_CALLS_ALLOWED)
      synchronized (orbs)
        {
          assert ! orbs.contains(orb);
          orbs.add(orb);
        }
  }
  
   /**
     * Unregister the ORB. The ORB will no longer be reacheable locally but may
     * be reacheable via network as if it would be remote.
     * 
     * @param orb the orb to unregister
     */
  static void unregisterOrb(OrbFunctional orb)
  {
    if (DIRECT_CALLS_ALLOWED)
      synchronized (orbs)
        {
          assert orbs.contains(orb);
          orbs.remove(orb);
        }
  }
  
  /**
   * Search the possibly local object. If the IOR is not local or none of the
   * found ORBs of this virtual machine knows about it, null is returned.
   * 
   * @param ior the IOR to search
   * @return the found local CORBA object or null in not found.
   */
  static org.omg.CORBA.Object searchLocalObject(IOR ior)
  {
    if (! DIRECT_CALLS_ALLOWED && ! ior.Internet.host.equals(localHost))
      return null;

    synchronized (orbs)
      {
        OrbFunctional orb;
        org.omg.CORBA.Object object;
        for (int i = 0; i < orbs.size(); i++)
          {
            orb = (OrbFunctional) orbs.get(i);
            object = orb.find_connected_object(ior.key, ior.Internet.port);
            if (object != null)
              {
                if (object instanceof SafeForDirectCalls)
                  {
                    return object;
                  }
                else if (object instanceof gnuServantObject)
                  {
                    return object;
                  }
              }
          }
      }
    return null;
  }
  
}
