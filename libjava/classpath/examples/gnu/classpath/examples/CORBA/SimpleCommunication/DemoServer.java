/* DemoServer.java --
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


package gnu.classpath.examples.CORBA.SimpleCommunication;

import gnu.classpath.examples.CORBA.SimpleCommunication.communication.DemoServant;

import org.omg.CORBA.ORB;

import java.io.FileOutputStream;
import java.io.PrintStream;

/**
 * This is the server class that handles the client requests,
 * delegating the functionality to the {@link DemoServant}.
 *
 * When starting, the server writes the IOR.txt file into the current
 * folder. With the information, stored in this file, the server
 * should be reachable over Internet, unless blocked by security tools.
 *
 * This code is tested for interoperability with Sun Microsystems
 * java implementation 1.4.2 (08.b03). Server, client of both can
 * be started either on Sun's or on Classpath CORBA implementation,
 * in any combinations.
 *
 * BE SURE TO START THIS SERVER BEFORE STARTING THE CLIENT.
 *
 * @author Audrius Meskauskas, Lithuania (AudriusA@Bioinformatics.org)
 */
public class DemoServer
{

  public static void main(String[] args)
  {
    start_server(args);
  }

  public static ORB start_server(String[] args)
  {
    try
      {
        // Create and initialize the ORB.
        final ORB orb = org.omg.CORBA.ORB.init(args, null);

        // Create the servant and register it with the ORB.
        DemoServant tester = new DemoServant();
        orb.connect(tester);

        // Storing the IOR reference.
        String ior = orb.object_to_string(tester);
        System.out.println("IOR: " + ior);

        gnu.CORBA.IOR ii = gnu.CORBA.IOR.parse(ior);
        System.out.println(ii);

        // The file IOR.txt in the current folder will be used
        // to find the object by clients.
        FileOutputStream f = new FileOutputStream("IOR.txt");
        PrintStream p = new PrintStream(f);
        p.print(ior);
        p.close();

        System.out.println("The test server ready and waiting ...");

        new Thread()
          {
            public void run()
            {
              // Start the thread, serving the invocations from clients.
              orb.run();
            }
          }.start();

        return orb;
      }
    catch (Exception e)
      {
        System.err.println("ERROR: " + e);
        e.printStackTrace(System.out);
        return null;
      }
  }
}
