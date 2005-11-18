/* PlayerImpl.java --
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

import java.awt.Color;
import java.awt.Point;

import java.rmi.RemoteException;

/**
 * The implementation of the PlayerCommunicator, providing the local
 * functionality. Apart remote methods, the class also defines some local
 * methods, needed for the co-ordinated work with the game user interface.
 * 
 * @author Audrius Meskauskas (AudriusA@Bioinformatics.org) 
 */
public class PlayerImpl
  implements Player, State
{
  /**
   * The playing table.
   */
  PlayingDesk desk;

  /**
   * The state of this player (one of the constants, defined in the player
   * interface.
   */
  private int state = DISCONNECTED;

  /**
   * The other player.
   */
  Player partner;

  /**
   * Called when the local player refuses to continue the game.
   */
  public void leave()
  {
    try
      {
        if (state == I_THINK || state == I_WAIT_FOR_YOUR_MOVE)
          {
            partner.receive_chat(ChatConstants.REMOTE_PLAYER,
              "Your partner has left the game.");
            partner.disconnect();
          }
        else if (state == State.QUEUED)
          {
            if (desk.manager != null)
              desk.manager.unregister(desk.player);
            receive_chat(ChatConstants.SYSTEM,
              "Do not be so pessimistic, try to play first!");
          }
        set_current_state(State.DISCONNECTED);

        desk.frame.bChat.setEnabled(false);
        desk.frame.bLeave.setEnabled(false);
        desk.frame.bConnect.setEnabled(true);
        desk.frame.taUrl.setText(desk.frame.mior);
      }
    catch (RemoteException ex)
      {
        // We will print the exception because this is a demo application that
        // may be modified for learning purposes.
        ex.printStackTrace();
      }
  }

  /**
   * Called when we make the move. The PlayingTable is responsible for checking
   * the correctness of the move and detecting the victory.
   * 
   * @param x x position of the new dot.
   * @param y y position of the new dot.
   * 
   * @param victory array of two memebers, representing the endpoints of the
   * drawn line (victory detected) or null if no such yet exists.
   */
  public void we_move(int x, int y, Point[] victory)
  {
    try
      {
        set_current_state(I_WAIT_FOR_YOUR_MOVE);
        partner.receive_move(x, y, victory);
      }
    catch (RemoteException ex)
      {
        // We will print the exception because this is a demo application that
        // may be modified for learning purposes.
        ex.printStackTrace();

        state = ERROR;
      }
  }

  /**
   * Set the current state.
   */
  public void set_current_state(int new_state)
  {
    state = new_state;

    if (state == DISCONNECTED)
      {
        setStatus("Disconnected");
      }
    else if (state == I_THINK)
      {
        setStatus("Our move");
      }
    else if (state == I_WAIT_FOR_YOUR_MOVE)
      {
        setStatus("Partner's move");
      }
    else if (state == ERROR)
      {
        setStatus("Error.");
      }
    else if (state == I_HAVE_LOST)
      {
        setStatus("We lost");
      }
    else if (state == I_HAVE_WON)
      {
        setStatus("Victory");
      }
    else if (state == QUEUED)
      {
        setStatus("Queued");
      }
    else
      {
        setStatus("State " + state);
      }

    boolean connected = state != State.DISCONNECTED;

    desk.frame.bConnect.setEnabled(!connected && state != State.QUEUED);
    desk.frame.bReset.setEnabled(connected);
    desk.frame.bLeave.setEnabled(connected);
    desk.frame.bChat.setEnabled(connected);
  }

  /**
   * Show the state in the status line.
   */
  public void setStatus(String status)
  {
    desk.frame.lbState.setText(status);
  }

  /**
   * Receive the invitation to play from the patner or the game manager.
   * 
   * @param address the address (host and port) of the remote partner.
   * @param youStart if true, the game manager instructs to start the game first
   * (another side is instructed to start the game second).
   * 
   * Game server may also chat a little bit with both players, saying that the
   * game has started.
   * 
   * @return true on success.
   */
  public boolean start_game(Player otherPlayer, boolean youStart)
    throws RemoteException
  {
    partner = otherPlayer;
    desk.reset();

    if (youStart)
      {
        set_current_state(I_THINK);
      }
    else
      {
        set_current_state(I_WAIT_FOR_YOUR_MOVE);
      }

    desk.frame.taUrl.setText("");

    return true;
  }

  /**
   * Get the state of the local player (one of the constants, defined in this
   * interface).
   */
  public int get_current_state()
    throws RemoteException
  {
    return state;
  }

  /**
   * Receive the chat message from the friend or challenge server (remote).
   * Possible at any state, always remote.
   * 
   * @param color the color code, used to highlight the message.
   * @param text the message text.
   */
  public void receive_chat(byte color, String text)
    throws RemoteException
  {
    if (color >= ChatConstants.colors.length)
      color = ChatConstants.REMOTE_PLAYER;

    desk.frame.talk(ChatConstants.colors[color], text);
  }

  /**
   * Indicated that the remote side leaves the game (capitulating).
   */
  public void disconnect()
    throws RemoteException
  {
    desk.frame.talk(Color.red, "The partner leaves the game.");
    partner = null;
    set_current_state(DISCONNECTED);

    desk.frame.taUrl.setText(desk.frame.mior);
  }

  /**
   * Receive friends move (possible at I_WAIT_FOR_YOUR_MOVE).
   * 
   * @param x grid position.
   * @param y grid position.
   * @param victory if not a null, the friend thinks that it has won, the
   * parameter containing the ends of the builded line.
   */
  public void receive_move(int x, int y, Point[] victory)
    throws RemoteException
  {
    // The state changes are handled by the PlayingTable
    desk.friendsMove(x, y, victory);
  }
}