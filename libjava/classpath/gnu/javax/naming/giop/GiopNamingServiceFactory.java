/* GiopNamingServiceFactory.java -- handles corbaname: urls
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


package gnu.javax.naming.giop;

import gnu.CORBA.OrbFunctional;

import java.util.Enumeration;
import java.util.Hashtable;
import java.util.Iterator;
import java.util.Map;
import java.util.Properties;
import java.util.TreeMap;

import javax.naming.Context;
import javax.naming.Name;

import org.omg.CORBA.ORB;

/**
 * The context factory to represent the corbaname: style urls. Such URL states
 * that the CORBA naming service exists on the given host. This service can
 * return the required object, finding it by the given name. The names are
 * parsed using the specification of the corbaname urls. Being the naming
 * service, the returned context supports creating the subcontexts, forwarding
 * this task to the existing naming service.
 * 
 * @author Audrius Meskauskas (audriusa@Bioinformatics.org)
 */
public class GiopNamingServiceFactory
{
  /**
   * The default naming service provider. It is assumed, that the naming service
   * is running on the port 900 of the local host, using the GIOP version 1.2
   */
  public static final String DEFAULT_PROVIDER = 
    "corbaloc:iiop:1.2@127.0.0.1:900/NameService";
  
  /**
   * The table of all instantiated ORB's that are found by they ORB 
   * properties signatures. If all ORB related properties are the same,
   * the ORB's are shared.
   */
  public static Hashtable orbs = new Hashtable();
  
  
  /**
   * Create a new instance of the corbaname URL context.
   */
  public Object getObjectInstance(Object refObj, Name name, Context nameCtx,
                                  Hashtable environment)
  {
    String provider = (String) environment.get(Context.PROVIDER_URL);
    if (provider == null)
      provider = DEFAULT_PROVIDER;

    String orbSignature = getOrbSignature(environment);

    ORB orb;
    synchronized (orbs)
      {
        orb = (ORB) orbs.get(orbSignature);
        if (orb == null)
          {
            Properties props = new Properties();
            props.putAll(environment);
            orb = ORB.init(new String[0], props);
            orbs.put(orbSignature, orb);
            final ORB runIt = orb;
            new Thread()
            {
              public void run()
              {
                runIt.run();
              }
            }.start();
          }
      }

    return new GiopNamingServiceURLContext(environment, this, orb);
  }
  
  /**
   * Check if this ORB is still in use (maybe it is time to shutdown it). This
   * method only works when the Classpath CORBA implementation is used
   * (otherwise it return without action). The method is called from the close()
   * method of the created context.
   * 
   * @param orb
   *          the ORB that maybe is no longer referenced.
   */
  public void checkIfReferenced(ORB orb)
  {
    synchronized (orbs)
      {
        // We can only do this with the Classpath implementation.
        if (orb instanceof OrbFunctional)
          {
            OrbFunctional cOrb = (OrbFunctional) orb;
            // If there are no connected objects, we can destroy the orb.
            if (cOrb.countConnectedObjects() == 0)
              {
                cOrb.shutdown(false);
                cOrb.destroy();
                
                Enumeration keys = orbs.keys();
                Object key;
                Remove: while (keys.hasMoreElements())
                  {
                    key = keys.nextElement();
                    if (orbs.get(key) == orb)
                      {
                        orbs.remove(key);
                        break Remove;
                      }
                  }
              }
          }
      }
  }
  
  /**
   * Get all properties.
   */
  public String getOrbSignature(Map props)
  {
     TreeMap map = new TreeMap();
     map.putAll(props);
     StringBuffer b = new StringBuffer(50*props.size());
     
     Iterator iter = map.entrySet().iterator();
     Map.Entry m;
     while (iter.hasNext())
       {
         m = (Map.Entry) iter.next();
         b.append(m.getKey());
         b.append('=');
         b.append(m.getValue());
       }
     return b.toString();
  }
}
