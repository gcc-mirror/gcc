/* Main.java -- RMI registry starter.
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


package gnu.classpath.tools.rmiregistry;

import gnu.classpath.tools.common.ClasspathToolParser;
import gnu.classpath.tools.getopt.FileArgumentCallback;
import gnu.classpath.tools.getopt.Option;
import gnu.classpath.tools.getopt.OptionException;
import gnu.classpath.tools.getopt.OptionGroup;
import gnu.classpath.tools.getopt.Parser;
import gnu.classpath.tools.rmiregistry.RegistryImpl;
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
public class Main
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
   * Parsed parameters.
   */
  private String directory = ".";
  private boolean cold = false;
  private boolean persistent = false;
  private boolean stop = false;
  private int port = Registry.REGISTRY_PORT;
  private RMIServerSocketFactory ssf = null;

  private Parser initializeParser()
  {
    Parser parser = new ClasspathToolParser("rmiregistry", true); //$NON-NLS-1$
    parser.setHeader(Messages.getString("Main.Usage")); //$NON-NLS-1$

    OptionGroup controlGroup
      = new OptionGroup(Messages.getString("Main.ControlGroup")); //$NON-NLS-1$
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
          verbose = true;
        }
      });
    parser.add(debuggingGroup);

    return parser;
  }

  private void run(String[] args)
  {
    Parser p = initializeParser();
    p.parse(args, new FileArgumentCallback()
      {
        public void notifyFile(String portArgument)
        {
          port = Integer.parseInt(portArgument);
        }
      });

    if (!stop)
      {
        Map table;
        if (!persistent)
          table = new Hashtable();
        else
          {
            // Start the system.
            File dataDirectory = new File(directory);
            if (!dataDirectory.exists())
              dataDirectory.mkdirs();
            table = PersistentHashTable.createInstance(
              new File(dataDirectory, "rmiregistry.data"), cold);
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
            System.out.println("The naming service at port " + port + " is not a "
                               + Main.class.getName());
        }
      }
  }

  /**
   * The RMI registry implementation entry point.
   */
  public static void main(String[] args)
  {
    Main rmiregistryprogram = new Main();
    try
      {
        rmiregistryprogram.run(args);
      }
    catch (Exception e)
      {
        System.err.println(Messages.getString("Main.InternalError")); //$NON-NLS-1$
        e.printStackTrace(System.err);
        System.exit(1);
      }
  }
}
