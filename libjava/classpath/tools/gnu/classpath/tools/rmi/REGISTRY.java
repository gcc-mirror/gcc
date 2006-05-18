/* REGISTY.java -- RMI registry starter.
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


package gnu.classpath.tools.rmi;

import gnu.classpath.tools.HelpPrinter;
import gnu.classpath.tools.rmi.registry.RegistryImpl;
import gnu.java.rmi.server.UnicastServerRef;

import java.io.File;
import java.rmi.NotBoundException;
import java.rmi.RemoteException;
import java.rmi.registry.LocateRegistry;
import java.rmi.registry.Registry;
import java.rmi.server.ObjID;
import java.rmi.server.RMIServerSocketFactory;
import java.util.Hashtable;
import java.util.Map;

/**
 * The optionally persistent RMI registry implementation.
 * 
 * @author Audrius Meskauskas (audriusa@bioinformatics.org)
 */
public class REGISTRY
{
  /**
   * The stop command.
   */
  public static String STOP = "gnu.classpath.tools.rmi.registry.command.STOP";
  
  /**
   * If true, the registry prints registration events to console.
   */
  public static boolean verbose = false;
  
  /**
   * The RMI registry implementation entry point.
   */
  public static void main(String[] args)
  {
    String HelpPath = "rmi/REGISTRY.txt";
    HelpPrinter.checkHelpKey(args, HelpPath);
    
    // Parse parameters:
    String folder = ".";
    boolean cold = false;
    boolean trans = false;
    boolean stop = false;

    int port = Registry.REGISTRY_PORT;
    RMIServerSocketFactory ssf = null;

    for (int i = 0; i < args.length; i++)
      {
        String a = args[i];
        if (a.equals("-restart"))
          cold = true;
        else if (a.equals("-transient"))
          trans = true;
        else if (a.equals("-verbose"))
          verbose = true;
        else if (a.equals("-stop"))          
          stop = true;
        else if (i < args.length - 1)
          {
            // The additional key parameter is possible.
            if (a.equals("-port"))
              port = Integer.parseInt(args[++i]);
            else if (a.equals("-folder"))
              folder = args[++i];
          }
      }

    if (!stop)
      {
        Map table;
        if (trans)
          table = new Hashtable();
        else
          {
            // Start the system.
            File dataFolder = new File(folder);
            if (!dataFolder.exists())
              dataFolder.mkdirs();
            table = PersistentHashTable.createInstance(
              new File(dataFolder, "rmiregistry.data"), cold);
          }

        RegistryImpl system = new RegistryImpl(table);

        // We must export with the specific activation id that is only
        // possible when going into the gnu.java.rmi
        try
          {
            UnicastServerRef sref = new UnicastServerRef(
              new ObjID(ObjID.REGISTRY_ID), port, ssf);

            sref.exportObject(system);
            System.out.println("The RMI naming service is listening at " + port);
          }
        catch (Exception ex)
          {
            System.out.println("Failed to start RMI naming service at " + port);
          }
      }
    else
      {
        // Stop the naming service.
        try
          {
            Registry r = LocateRegistry.getRegistry(port);
            // Search for this specific line will command to stop the registry.
            
            // Our service returns null, but any other service will thrown
            // NotBoundException.
            r.unbind(STOP);
          }
        catch (RemoteException e)
          {
            System.out.println("Failed to stop RMI naming service at " + port);
          }
        catch (NotBoundException e)
        {
            System.out.println("The naming service at port "+port+" is not a "+
                               REGISTRY.class.getName());
        }
      }
  }
}
