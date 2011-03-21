/* GameManagerImpl.java --
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

import org.omg.CORBA.ORB;
import org.omg.CORBA.Object;

/**
 * The manager connects two players into the game.
 *
 * @author Audrius Meskauskas (AudriusA@Bioinformatics.org)
 */
public class GameManagerImpl
  implements GameManager
{
  /**
   * The game manager IOR.
   */
  static String ior;

  /**
   * The game manager ORB.
   */
  static ORB orb;

  /**
   * True if the manager started ok.
   */
  static boolean ok;

  /**
   * Another player that is already waiting for the game.
   */
  Player queuedPlayer = null;

  public synchronized void requestTheGame(Player newPlayer)
    throws RemoteException
  {
    System.out.println("Game requested");

    if (queuedPlayer == null)
      {
        // No other player so far.
        newPlayer.receive_chat(ChatConstants.GAME_SERVER,
          "Request registered, waiting for the other player to come...");
        System.out.println("Player queued.");
        queuedPlayer = newPlayer;
      }
    else if (queuedPlayer.equals(newPlayer))
      {
        // The same player applies again.
        newPlayer.receive_chat(ChatConstants.GAME_SERVER,
          "No other player so far... Please wait.");
      }
    else
      {
        // As the queued player waited for the game, we allow him/her
        // to start the game. This is a reward for waiting.
        newPlayer.receive_chat(ChatConstants.GAME_SERVER,
          "The other player is waiting. The game started, your "
            + "partner begins...");
        queuedPlayer.receive_chat(ChatConstants.GAME_SERVER,
          "The other player arrived. Lets play, you begin the game now...");

        newPlayer.start_game(queuedPlayer, false);
        queuedPlayer.start_game(newPlayer, true);

        queuedPlayer = null;
        System.out.println("Players connected.");
      }
  }

  /**
   * Unregister the player who left and is no longer waiting for another side.
   */
  public void unregister(Player player)
    throws RemoteException
  {
    if (queuedPlayer != null)
      {
        // We need to verify the identity of the player being unregistered.
        // The stubs, being derived from the org.omg.CORBA.Object, have the
        // method for this. This method compares the player host address,
        // used port and the object key.
        if (player instanceof Object && queuedPlayer instanceof Object)
          {
            Object a = (Object) player;
            Object b = (Object) queuedPlayer;

            if (a._is_equivalent(b))
              queuedPlayer = null;
          }
        else
          queuedPlayer = null;
      }
    System.out.println("Unregistering player");
  }
}
