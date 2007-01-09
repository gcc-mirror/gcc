/* RegistryImpl.java -- the RMI registry implementation
   Copyright (c) 1996, 1997, 1998, 1999, 2002, 2005, 2006
   Free Software Foundation, Inc.

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

package gnu.classpath.tools.rmiregistry;

import gnu.classpath.tools.common.Persistent;

import java.rmi.AccessException;
import java.rmi.AlreadyBoundException;
import java.rmi.NotBoundException;
import java.rmi.Remote;
import java.rmi.RemoteException;
import java.rmi.registry.Registry;
import java.util.ArrayList;
import java.util.Map;

/**
 * The optionally persistent registry implementation.
 * 
 * @author Audrius Meskauskas (audriusa@bioinformatics.org)
 */
public class RegistryImpl implements Registry
{
  /**
   * The binding table.
   */
  Map bindings;
  
  /**
   * Create the registry implementation that uses the given bidirectinal
   * table to keep the data.
   */
  public RegistryImpl(Map aTable)
  {
    bindings = aTable;    
  }
  
  /** @inheritDoc */
  public Remote lookup(String name) throws RemoteException, NotBoundException,
      AccessException
  {
    Object obj = bindings.get(name);
    if (obj == null)
      throw new NotBoundException(name);
    return ((Remote) obj);
  }

   /** @inheritDoc */
  public void bind(String name, Remote obj) throws RemoteException,
      AlreadyBoundException, AccessException
  {
    if (Main.verbose)
      System.out.println("Bind "+name);
    if (bindings.containsKey(name))
      throw new AlreadyBoundException(name);
    bindings.put(name, obj);
  }

  /** @inheritDoc */
  public void unbind(String name) throws RemoteException, NotBoundException,
      AccessException
  {
    if (name.equals(Main.STOP))
      {
        if (bindings instanceof Persistent)
          ((Persistent) bindings).writeContent();
         // Terminate in 10 seconds.
         System.out.println("Shutdown command received. Will terminate in 10 s");
         Persistent.timer.schedule(new Persistent.ExitTask(), 10000);
      }
    else
      {
        if (Main.verbose)
          System.out.println("Unbind "+name);
        
        if (!bindings.containsKey(name))
          throw new NotBoundException(name);
        else
          bindings.remove(name);
      }
  }

  /** @inheritDoc */
  public void rebind(String name, Remote obj) throws RemoteException,
      AccessException
  {
    if (Main.verbose)
      System.out.println("Rebind "+name);
    bindings.put(name, obj);
  }

  /** @inheritDoc */  
  public String[] list() throws RemoteException, AccessException
  {
    // Create a separated array to prevent race conditions.
    ArrayList keys = new ArrayList(bindings.keySet());
    int n = keys.size();
    String[] rt = new String[n];
    for (int i = 0; i < n; i++)
      rt[i] = (String) keys.get(i);
    return rt;
  }
}
