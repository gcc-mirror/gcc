/* GameManagerAddressServer.java --
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


package gnu.classpath.examples.CORBA.swing.x5;

import java.io.BufferedWriter;
import java.io.File;
import java.io.FileWriter;
import java.io.IOException;
import java.io.OutputStream;
import java.net.InetAddress;
import java.net.ServerSocket;
import java.net.Socket;

/**
 * The main executable class of the game manager server.
 * 
 * The manager address server returns the IOR address string of the game
 * manager. Hence the user does not need to enter the rather long IOR address
 * string and only needs to specify the host and port of the machine where the
 * game manager is running.
 * 
 * The manager address server starts the main game manager as well.
 * 
 * This server acts as a HTTP server that always returns the same response. This
 * primitive functionality is sufficient for its task.
 * 
 * The more complex CORBA applications should use the name service instead. We
 * do not use the name service as this would require to start additional
 * external application, specific for the different java platforms.
 * 
 * @author Audrius Meskauskas (AudriusA@Bioinformatics.org) 
 */
public class X5Server
{
  /**
   * Start the game manager.
   */
  public static void main(String[] args)
  {
    // Start the game manager, write the IOR to the agreed location.
    OrbStarter.startManager(args);

    if (!GameManagerImpl.ok)
      {
        System.out.println("Unable to start the game manager:");
        System.exit(1);
      }

    // Print the IOR.
    System.out.println(GameManagerImpl.ior);

    String manager_address = null;

    // Start the game manager server.
    ServerSocket nameServer = null;
    try
      {
        nameServer = new ServerSocket(OrbStarter.MANAGER_NAMER_PORT);

        System.out.println("The game manager is listening at:");
        manager_address = "http://"
          + InetAddress.getLocalHost().getHostAddress() + ":"
          + nameServer.getLocalPort();

        System.out.println(manager_address);

        System.out.println("Enter this address to the "
          + "input field of the game client.");

        System.out.println("Use ^C to stop the manager.");
      }
    catch (Exception ex)
      {
        System.out.println("The port " + OrbStarter.MANAGER_NAMER_PORT
          + " is not available. The game manager namer will not start.");
        System.exit(1);
      }

    // Write the IOR to the local file system.
    if (OrbStarter.WRITE_URL_TO_FILE != null)
      {
        try
          {
            File gmf = new File(OrbStarter.WRITE_URL_TO_FILE);
            FileWriter f = new FileWriter(gmf);
            BufferedWriter b = new BufferedWriter(f);

            b.write(manager_address);
            b.close();
          }
        catch (IOException e)
          {
            System.out.println("Local filesystem not accessible."
              + "Read IOR from console.");
          }
      }

    // Do forever.
    while (true)
      {
        try
          {
            Socket socket = nameServer.accept();

            System.out.println("Connected.");

            // Set the two minutes timeout.
            socket.setSoTimeout(1000 * 120);

            OutputStream out = socket.getOutputStream();

            int length = GameManagerImpl.ior.length();

            StringBuilder b = new StringBuilder();
            b.append("HTTP/1.0 200 OK\r\n");
            b.append("Content-Length: " + length + "\r\n");
            b.append("Connection: close\r\n");
            b.append("Content-Type: text/plain; charset=UTF-8\r\n");
            b.append("\r\n");

            b.append(GameManagerImpl.ior);

            out.write(b.toString().getBytes("UTF-8"));

            socket.shutdownOutput();

            if (!socket.isClosed())
              socket.close();

            System.out.println("Completed.");
          }
        catch (Exception exc)
          {
            exc.printStackTrace();
            System.out.println("Network problem.");
          }
      }
  }
}
