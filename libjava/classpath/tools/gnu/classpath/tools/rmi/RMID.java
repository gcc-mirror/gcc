/* RMID.java -- the RMI activation daemon.
   Copyright (c) 2006 Free Software Foundation, Inc.

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
import gnu.classpath.tools.rmi.rmid.ActivationSystemImpl;
import gnu.java.rmi.activation.ActivationSystemTransient;
import gnu.java.rmi.server.UnicastServerRef;

import java.io.File;
import java.net.InetAddress;
import java.rmi.Remote;
import java.rmi.activation.ActivationSystem;
import java.rmi.registry.LocateRegistry;
import java.rmi.registry.Registry;
import java.rmi.server.ObjID;
import java.rmi.server.RMIServerSocketFactory;


/**
 * The persistent RMI activation daemon.
 * 
 * @author Audrius Meskauskas (audriusa@bioinformatics.org)
 */
public class RMID
{
  /**
   * The RMI server socket factory.
   */
  static RMIServerSocketFactory ACTIVATION_REGISTY_SOCKET_FACTORY = null;
  
  /**
   * The activation registry port.
   */
  static int ACTIVATION_REGISTRY_PORT = ActivationSystem.SYSTEM_PORT;
  
  /**
   * The activation system name.
   */
  static String ACTIVATION_SYSTEM_NAME = "java.rmi.activation.ActivationSystem";
  
  /**
   * The RMI activation daemon entry point.
   */
  public static void main(String[] args)
  {
    String HelpPath = "rmi/RMID.txt";
    HelpPrinter.checkHelpKey(args, HelpPath);
    
    // Parse parameters:
    boolean stop = false;
    String folder = ".";
    boolean cold = false;
    boolean trans = false;
    
    for (int i = 0; i < args.length; i++)
      {
        String a = args[i];
        if (a.equals("-verbose"))
          ActivationSystemTransient.debug = true;
        else if (a.equals("-stop"))
          stop = true;
        else if (a.equals("-restart"))
          cold = true;
        else if (a.equals("-transient"))
          trans = true;
        else if (i < args.length - 1)
          {
            // The additional key parameter is possible.
            if (a.equals("-port"))
              ACTIVATION_REGISTRY_PORT = Integer.parseInt(args[++i]);
            else if (a.equals("-folder"))
              folder = args[++i];
          }
      }

    try
      {
        if (!stop)
          {
            // Start the system.
            File dataFolder = new File(folder);
            if (!dataFolder.exists())
              dataFolder.mkdirs();
            ActivationSystem system;
            
            if (trans)
              system = ActivationSystemTransient.getInstance();
            else
              system = ActivationSystemImpl.getInstance(dataFolder, cold);
            
            // We must export with the specific activation id that is only
            // possible when going into the gnu.java.rmi.activation.
            UnicastServerRef sref = new UnicastServerRef(
               new ObjID(ObjID.ACTIVATOR_ID), ACTIVATION_REGISTRY_PORT, 
               ACTIVATION_REGISTY_SOCKET_FACTORY);
            Remote systemStub = sref.exportObject(system);

            // Start the naming system on the activation system port
            // (if not already running).

            Registry r;
            try
              {
                // Expect the naming service running first.
                // The local host may want to use the shared registry
                r = LocateRegistry.getRegistry(ACTIVATION_REGISTRY_PORT);
                r.rebind(ACTIVATION_SYSTEM_NAME, systemStub);
              }
            catch (Exception ex)
              {
                // The naming service is not running. Start it.
                r = LocateRegistry.createRegistry(ACTIVATION_REGISTRY_PORT);
                r.rebind(ACTIVATION_SYSTEM_NAME, systemStub);
              }
            String host = InetAddress.getLocalHost().getCanonicalHostName();
            System.out.println("The RMI daemon is listening on " + host + 
                               " (port "
                               + ACTIVATION_REGISTRY_PORT + ")");

          }
        else
          {
            // Stop the activation system.
            Registry r;
            try
              {
                System.out.print("Stopping RMI daemon at "
                                   + ACTIVATION_REGISTRY_PORT+" ... ");
                // Expect the naming service running first.
                // The local host may want to use the shared registry
                r = LocateRegistry.getRegistry(ACTIVATION_REGISTRY_PORT);
                ActivationSystem asys = 
                  (ActivationSystem) r.lookup(ACTIVATION_SYSTEM_NAME);
                asys.shutdown();
                System.out.println("OK.");
              }
            catch (Exception ex)
              {
                System.out.println("The RMI daemon seems not running at "
                                   + ACTIVATION_REGISTRY_PORT);
                if (ActivationSystemTransient.debug)
                  ex.printStackTrace();
              }
          }
      }
    catch (Exception e)
      {
        System.out.println("Failed to start the RMI daemon.");
        if (ActivationSystemTransient.debug)
          e.printStackTrace();
      }
  } 
}
