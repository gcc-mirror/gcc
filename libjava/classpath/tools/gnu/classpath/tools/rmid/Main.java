/* Main.java -- the RMI activation daemon.
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

package gnu.classpath.tools.rmid;

import gnu.classpath.tools.rmid.ActivationSystemImpl;
import gnu.classpath.tools.common.ClasspathToolParser;
import gnu.classpath.tools.getopt.FileArgumentCallback;
import gnu.classpath.tools.getopt.Option;
import gnu.classpath.tools.getopt.OptionException;
import gnu.classpath.tools.getopt.OptionGroup;
import gnu.classpath.tools.getopt.Parser;
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
public class Main
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
  
  // Parse parameters:
  private boolean stop = false;
  private String directory = ".";
  private boolean cold = false;
  private boolean persistent = false;

  private Parser initializeParser()
  {
    Parser parser = new ClasspathToolParser("rmiregistry", true); //$NON-NLS-1$
    parser.setHeader(Messages.getString("Main.Usage")); //$NON-NLS-1$


    OptionGroup controlGroup
      = new OptionGroup(Messages.getString("Main.ControlGroup")); //$NON-NLS-1$
    controlGroup.add(new Option("port", //$NON-NLS-1$
                          Messages.getString("Main.PortOption"), //$NON-NLS-1$
                          Messages.getString("Main.Port")) //$NON-NLS-1$
      {
        public void parsed(String portArgument) throws OptionException
        {
          ACTIVATION_REGISTRY_PORT = Integer.parseInt(portArgument);
        }
      });
    controlGroup.add(new Option("restart", //$NON-NLS-1$
                                Messages.getString("Main.Restart")) //$NON-NLS-1$
      {
        public void parsed(String argument) throws OptionException
        {
          cold = true;
        }
      });
    controlGroup.add(new Option("stop", //$NON-NLS-1$
                                Messages.getString("Main.Stop")) //$NON-NLS-1$
      {
        public void parsed(String argument) throws OptionException
        {
          stop = true;
        }
      });
    parser.add(controlGroup);

    OptionGroup persistenceGroup
      = new OptionGroup(Messages.getString("Main.PersistenceGroup")); //$NON-NLS-1$
    persistenceGroup.add(new Option("persistent", //$NON-NLS-1$
                                    Messages.getString("Main.Persistent")) //$NON-NLS-1$
      {
        public void parsed(String argument) throws OptionException
        {
          persistent = true;
        }
      });
    persistenceGroup.add(new Option("directory", //$NON-NLS-1$
                                    Messages.getString("Main.Directory"), //$NON-NLS-1$
                                    Messages.getString("Main.DirectoryArgument")) //$NON-NLS-1$
      {
        public void parsed(String argument) throws OptionException
        {
          directory = argument;
        }
      });
    parser.add(persistenceGroup);

    OptionGroup debuggingGroup
      = new OptionGroup(Messages.getString("Main.DebugGroup")); //$NON-NLS-1$
    debuggingGroup.add(new Option("verbose", //$NON-NLS-1$
                                  Messages.getString ("Main.Verbose")) //$NON-NLS-1$
      {
        public void parsed(String argument) throws OptionException
        {
          ActivationSystemTransient.debug = true;
        }
      });
    parser.add(debuggingGroup);

    return parser;
  }

  private void run(String[] args)
  {
    Parser p = initializeParser();
    p.parse(args);

    try
      {
        if (!stop)
          {
            // Start the system.
            File dataDirectory = new File(directory);
            if (!dataDirectory.exists())
              dataDirectory.mkdirs();
            ActivationSystem system;
            
            if (!persistent)
              system = ActivationSystemTransient.getInstance();
            else
              system = ActivationSystemImpl.getInstance(dataDirectory, cold);
            
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

  /**
   * The activation system entry point.
   */
  public static void main(String[] args)
  {
    Main rmidprogram = new Main();
    try
      {
        rmidprogram.run(args);
      }
    catch (Exception e)
      {
        System.err.println(Messages.getString("Main.InternalError")); //$NON-NLS-1$
        e.printStackTrace(System.err);
        System.exit(1);
      }
  }
}
