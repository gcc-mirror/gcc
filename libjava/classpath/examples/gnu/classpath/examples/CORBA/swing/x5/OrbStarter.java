/* OrbStarter.java --
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

import java.rmi.RemoteException;
import java.util.Properties;

import javax.rmi.PortableRemoteObject;
import javax.rmi.CORBA.Tie;

import org.omg.CORBA.ORB;
import org.omg.PortableServer.POA;
import org.omg.PortableServer.POAHelper;
import org.omg.PortableServer.Servant;

/**
 * Starts the ORBs, involved into this application.
 *
 * @author Audrius Meskauskas (AudriusA@Bioinformatics.org)
 */
public class OrbStarter
{
  /**
   * The game manager name server port. This server allows to access the game
   * manager by host (IP) and port rather than by the rather long IOR string.
   */
  static int MANAGER_NAMER_PORT = 1500;

  /**
   * The used port range (understood and used by GNU Classpath only).
   */
  static String USED_PORT_RANGE = "1501-1503";

  /**
   * Specify the file where under start the game manager writes its IOR.
   * You may specify the path if the game manager and player clients have
   * access to some share file system or if you prefer to write IOR to
   * floppy and then read from the floppy on the client side. Both clients
   * and server will use this constant. Set to null not to write the IOR.
   */
  static String WRITE_URL_TO_FILE = "game_manager_ior.txt";

  /**
   * Start the manager ORB.
   * @return the manager URL if it starts.
   */
  public static String startManager(final String[] args)
  {
    GameManagerImpl.ior = null;
    GameManagerImpl.ok = false;

    final Properties p = new Properties();
    p.put("gnu.CORBA.ListenerPort", USED_PORT_RANGE);

    try
      {
        new Thread()
        {
          public void run()
          {
            try
              {
                GameManagerImpl.orb = ORB.init(args, p);

                // Obtain the root poa:
                POA rootPOA = POAHelper.narrow(GameManagerImpl.orb.resolve_initial_references("RootPOA"));

                GameManagerImpl impl = new GameManagerImpl();

                PortableRemoteObject.exportObject(impl);

                // Construct the tie that is also the servant.
                Tie tie = new _GameManagerImpl_Tie();

                // Set the invocation target for this tie.
                tie.setTarget(impl);

                // Obtain the reference to the corresponding CORBA object:
                org.omg.CORBA.Object object = rootPOA.servant_to_reference((Servant) tie);

                GameManagerImpl.ok = true;

                // Activate the root POA.
                rootPOA.the_POAManager().activate();

                // Get the IOR URL that must be passed to clients.
                GameManagerImpl.ior = GameManagerImpl.orb.object_to_string(object);

                GameManagerImpl.orb.run();
              }
            catch (Exception exc)
              {
                exc.printStackTrace();
                GameManagerImpl.ior = "Unable to start the ORB: " + exc;
              }
          }
        }.start();

        // Wait the thread to enter orb.run.
        long t = System.currentTimeMillis();
        while (GameManagerImpl.ior == null
          && System.currentTimeMillis() - t < 20000)
          {
            Thread.sleep(100);
          }

        return GameManagerImpl.ior;
      }
    catch (Exception e)
      {
        e.printStackTrace();
        return "Exception: " + e;
      }
  }

  /**
   * Start the client ORB.
   */
  public static String startPlayer(final Player player, final PlayingDesk desk)
  {
    desk.ior = null;
    desk.ok = false;

    final Properties p = new Properties();
    p.put("gnu.CORBA.ListenerPort", USED_PORT_RANGE);

    try
      {
        new Thread()
        {
          public void run()
          {
            try
              {
                desk.orb = ORB.init(new String[0], p);

                POA rootPOA = POAHelper.narrow(desk.orb.resolve_initial_references("RootPOA"));
                rootPOA.the_POAManager().activate();

                // Construct the tie.
                Tie tie = new _PlayerImpl_Tie();

                // Set the implementing class (invocation target).
                tie.setTarget(new PlayerImpl());

                // Connect the tie as POA servant.
                org.omg.CORBA.Object object = rootPOA.servant_to_reference((Servant) tie);

                // Get the stringified reference.
                desk.ior = desk.orb.object_to_string(object);

                // Mark that the object was created OK.
                desk.ok = true;
                desk.orb.run();
              }
            catch (Exception exc)
              {
                exc.printStackTrace();
                desk.ior = "Unable to start the ORB: " + exc;
              }
          }
        }.start();

        long t = System.currentTimeMillis();
        while (desk.ior == null && System.currentTimeMillis() - t < 20000)
          {
            Thread.sleep(100);
          }
      }
    catch (Exception e)
      {
        e.printStackTrace();
        return "Exception: " + e;
      }

    // Add shutdown hook to unregister from the manager.
    Runtime.getRuntime().addShutdownHook(new Thread()
    {
      public void run()
      {
        if (desk.manager != null && player != null)
          {
            try
              {
                desk.manager.unregister(player);
              }
            catch (RemoteException ex)
              {
                // We will print the exception because this is a demo
                // application that
                // may be modified for learning purposes.
                ex.printStackTrace();
              }
            desk.manager = null;
          }
      }
    });
    return desk.ior;
  }
}
