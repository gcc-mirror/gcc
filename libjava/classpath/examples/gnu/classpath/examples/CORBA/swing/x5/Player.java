/* Player.java --
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

import java.awt.Point;

import java.rmi.Remote;
import java.rmi.RemoteException;

/**
 * Defines remote methods that are invoked by another player or by the
 * challenge server.
 * 
 * @author Audrius Meskauskas (AudriusA@Bioinformatics.org) 
 */
public interface Player extends Remote
{
  /**
   * Receive the invitation to play from the patner or the game manager.
   *
   * @param address the address (host and port) of the remote partner.
   * @param youStart if true, the game manager instructs to start
   * the game first (another side is instructed to start the game second).
   *
   * @return true on success.
   */
  boolean start_game(Player otherPlayer, boolean youStart)
    throws RemoteException;

  /**
   * Get the state of the local player (one of the constants, defined
   * in this interface).
   */
  int get_current_state() throws RemoteException;

  /**
   * Receive the chat message from the friend or challenge server (remote).
   * Possible at any state, always remote.
   *
   * @param color the color code, used to highlight the message.
   * @param text the message text.
   */
  void receive_chat(byte color, String test) throws RemoteException;

  /**
   * Indicated that the remote side leaves the game (capitulating).
   */
  void disconnect() throws RemoteException;

  /**
   * Receive friends move (possible at I_WAIT_FOR_YOUR_MOVE).
   *
   * @param x grid position.
   * @param y grid position.
   *
   * @param sessionId the session id, must match (otherwise the call is ignored).
   * @param victory if not a null, the friend thinks that it has won, the parameter
   * containing the ends of the builded line.
   */
  void receive_move(int x, int y, Point[] victory) throws RemoteException;
}