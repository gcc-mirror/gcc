/* PlayingDesk.java --
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
import java.awt.Graphics;
import java.awt.Point;
import java.awt.event.MouseEvent;
import java.awt.event.MouseListener;
import java.rmi.RemoteException;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Iterator;

import javax.swing.JComponent;

import org.omg.CORBA.ORB;

/**
 * Manages actions, related to the game rules and also does all painting.
 *
 * @author Audrius Meskauskas, Lithuania (AudriusA@Bioinformatics.org)
 */
public class PlayingDesk
  extends JComponent
  implements MouseListener, State
{
  /**
   * Use serialVersionUID for interoperability.
   */
  private static final long serialVersionUID = 1;

  /**
   * Indicates that the field point state is the red oval.
   */
  public static final int RED = 0;

  /**
   * Indicates that the field point state is the black cross.
   */
  public static final int BLACK = 1;

  /**
   * Indicates that the field point state is the hint, suggested by the fan.
   */
  public static final int HINT = 2;

  /**
   * The access to the main frame methods.
   */
  ClientFrame frame;

  /**
   * The access to the player communicator.
   */
  PlayerImpl player;

  /**
   * The game manager.
   */
  GameManager manager;

  /**
   * The player ORB.
   */
  ORB orb;

  /**
   * The player IOR.
   */
  String ior;

  /**
   * True if the player ORB started ok.
   */
  boolean ok;

  /**
   * The grid spacing.
   */
  static int W = 16;

  /**
   * The radius of the dots being painted.
   */
  static int R = W / 3;

  /**
   * The collection of the red dots.
   */
  ArrayList reds = new ArrayList();

  /**
   * The collection of the black dots.
   */
  ArrayList blacks = new ArrayList();

  /**
   * The array of hints.
   */
  ArrayList hints = new ArrayList();

  /**
   * When the game is completed, obtains the value of the two end points of the
   * created line.
   */
  Point[] endOfGame;

  public PlayingDesk()
  {
    try
      {
        player = new PlayerImpl();
        player.desk = this;

        OrbStarter.startPlayer(player, this);

        jbInit();
      }
    catch (Exception e)
      {
        e.printStackTrace();
      }
  }

  /**
   * Paint this component.
   */
  public void paintComponent(Graphics g)
  {
    int w = getWidth();
    int h = getHeight();

    g.setColor(Color.white);
    g.fillRect(0, 0, w, h);

    drawGrid(w, h, g);
    drawFigures(g);
  }

  /**
   * Check maybe a game is finished after setting the point N
   */
  public Point[] checkFinished(Collection x, Point N)
  {
    Iterator iter = x.iterator();
    Point p;

    // The victory, if happens, must occur inside these boundaries:
    int ax = N.x - 5;
    int bx = N.x + 5;

    int ay = N.y - 5;
    int by = N.y + 5;

    while (iter.hasNext())
      {
        p = (Point) iter.next();

        if (p.x > ax && p.x < bx && p.y > ay && p.y < by)
          {
            // Check the vertical line down
            if (pointPresent(p.x, p.y + 1, x))
              if (pointPresent(p.x, p.y + 2, x))
                if (pointPresent(p.x, p.y + 3, x))
                  if (pointPresent(p.x, p.y + 4, x))
                    return new Point[] { p, new Point(p.x, p.y + 4) };

            // Check the horizontal line left
            if (pointPresent(p.x + 1, p.y, x))
              if (pointPresent(p.x + 2, p.y, x))
                if (pointPresent(p.x + 3, p.y, x))
                  if (pointPresent(p.x + 4, p.y, x))
                    return new Point[] { p, new Point(p.x + 4, p.y) };

            // Check the diagonal line right down.
            if (pointPresent(p.x + 1, p.y + 1, x))
              if (pointPresent(p.x + 2, p.y + 2, x))
                if (pointPresent(p.x + 3, p.y + 3, x))
                  if (pointPresent(p.x + 4, p.y + 4, x))
                    return new Point[] { p, new Point(p.x + 4, p.y + 4) };

            // Check the diagonal line left down.
            if (pointPresent(p.x - 1, p.y + 1, x))
              if (pointPresent(p.x - 2, p.y + 2, x))
                if (pointPresent(p.x - 3, p.y + 3, x))
                  if (pointPresent(p.x - 4, p.y + 4, x))
                    return new Point[] { p, new Point(p.x - 4, p.y + 4) };
          }
      }
    return null;
  }

  /**
   * Called when the "end of the game" situation is detected.
   */
  public void drawFinishLine(int xa, int ya, int xb, int yb, Graphics g)
  {
    g.setColor(Color.blue);

    int hW = W / 2;
    g.drawLine(xa * W + hW, ya * W + hW, xb * W + hW, yb * W + hW);
  }

  /**
   * Check for the presence of the given point in the collection.
   */
  public final boolean pointPresent(int x, int y, Collection in)
  {
    Iterator iter = in.iterator();
    Point p;
    while (iter.hasNext())
      {
        p = (Point) iter.next();
        if (p.x == x && p.y == y)
          return true;
      }
    return false;
  }

  public void drawGrid(int w, int h, Graphics g)
  {
    g.setColor(Color.lightGray);

    // Draw vertical lines:
    for (int x = 0; x < w; x += W)
      {
        g.drawLine(x, 0, x, h);
      }

    // Draw horizontal lines:
    for (int y = 0; y < h; y += W)
      {
        g.drawLine(0, y, w, y);
      }
      
    g.setColor(Color.gray);
    g.drawRect(0,0, frame.DESK_SIZE.width, frame.DESK_SIZE.height);
    g.drawRect(0,0, frame.DESK_SIZE.width+3, frame.DESK_SIZE.height+3);    
  }

  public void drawFigures(Graphics g)
  {
    g.setColor(Color.red);
    drawDots(reds, g, RED);

    g.setColor(Color.black);
    drawDots(blacks, g, BLACK);

    g.setColor(Color.lightGray);
    drawDots(hints, g, HINT);

    if (endOfGame != null)
      drawFinishLine(endOfGame[0].x, endOfGame[0].y, endOfGame[1].x,
        endOfGame[1].y, g);
  }

  public Point makePoint(int x, int y)
  {
    return new Point(x / W, y / W);
  }

  /**
   * Draw a collection of dots (the collor must be set before calling the
   * method).
   */
  public void drawDots(Collection dots, Graphics g, int mode)
  {
    Iterator iter = dots.iterator();
    int x;
    int y;

    int hW = W / 2;
    int RR = R * 2;
    int hR = R / 2;
    Point p;
    while (iter.hasNext())
      {
        p = (Point) iter.next();
        x = p.x * W + hW;
        y = p.y * W + hW;

        if (mode == RED)
          g.drawOval(x - R, y - R, RR, RR);
        else if (mode == BLACK)
          {
            g.drawLine(x - R, y - R, x + R, y + R);
            g.drawLine(x - R, y + R, x + R, y - R);
          }
        else
          {
            // Hint.
            g.drawOval(x - hR, y - hR, R, R);
          }
      }
  }

  private void jbInit()
    throws Exception
  {
    addMouseListener(this);
  }

  public void mouseClicked(MouseEvent e)
  {
    try
      {
        int state = player.get_current_state();

        // Check if the state is correct.
        if (state == I_WAIT_FOR_YOUR_MOVE)
          {
            frame.talk(Color.black,
              "It is now time for our partner's move, not ours. Please wait.");
          }
        else if (state == DISCONNECTED)
          {
            frame.talk(Color.black,
              "We are not connected to the playing partner yet.");
          }
        else if (state == I_HAVE_LOST)
          {
            frame.talk(Color.black,
              "We have already lost this battle, but why not to try again?");
          }
        else if (state == I_HAVE_WON)
          {
            frame.talk(Color.black,
              "The victory is ours, nothing more to do here.");
          }
        else if (player.partner == null)
          frame.talk(Color.black, "No other player so far.");
        else
          {
            int x = e.getX();
            int y = e.getY();

            if (x>frame.DESK_SIZE.width ||
                y>frame.DESK_SIZE.height)
            {
              frame.talk(Color.black,"Outside the game area.");
              return;
            }

            Point p = makePoint(x, y);

            // Ignore clicks on the occupied cells.
            if (pointPresent(p.x, p.y, reds)
              || (pointPresent(p.x, p.y, blacks)))
              {
                frame.talk(Color.black,
                  "This is against the rules, select the unoccupied cell.");
                return;
              }

            reds.add(p);

            endOfGame = checkFinished(reds, p);
            repaint();

            if (endOfGame != null)
              {
                frame.talk(Color.red, "Our move " + p.x + "-" + p.y
                  + " and we win!");
                player.set_current_state(I_HAVE_WON);
              }
            else
              {
                frame.talk(Color.black, "Our move " + p.x + "-" + p.y
                  + ". Waiting for the other side move...");
                player.set_current_state(I_WAIT_FOR_YOUR_MOVE);
              }

            player.partner.receive_move(p.x, p.y, endOfGame);
          }
      }
    catch (RemoteException ex)
      {
        // We will print the exception because this is a demo application
        // that may be modified for learning purposes.
        ex.printStackTrace();
      }
  }

  /**
   * Handle the move of the other playing side.
   */
  public void friendsMove(int x, int y, Point[] victory)
  {
    try
      {
        int state = player.get_current_state();
        if (state != I_WAIT_FOR_YOUR_MOVE || pointPresent(x, y, blacks))
          {
            stateFailed("Move " + x + "-" + y);
          }
        else
          {
            blacks.add(new Point(x, y));
            repaint();

            if (victory != null)
              {
                frame.talk(Color.red,
                  " We have lost this time, unfortunately..");
                player.set_current_state(I_HAVE_LOST);
                endOfGame = victory;
              }
            else
              {
                frame.talk(Color.black, "Partner goes " + x + "-" + y
                  + ". Your move?");
                player.set_current_state(I_THINK);
              }
          }
      }
    catch (RemoteException rex)
      {
        rex.printStackTrace();
      }
  }

  /**
   * Prepare for the new game.
   */
  public void reset()
  {
    blacks.clear();
    reds.clear();
    hints.clear();
    endOfGame = null;
    repaint();
  }

  public void mouseEntered(MouseEvent m)
  {
    // Nothing to do.
  }

  public void mousePressed(MouseEvent m)
  {
    // Nothing to do.
  }

  public void mouseReleased(MouseEvent m)
  {
    // Nothing to do.
  }

  public void mouseExited(MouseEvent m)
  {
    // Nothing to do.
  }

  /**
   * The systems detected the error conditions. The game cannot continue (the
   * chat is still possible).
   */
  public void stateFailed(String reason)
  {
    try
      {
        player.receive_chat(ChatConstants.REMOTE_PLAYER,
          "Wrong move, game cannot continue (our state was "
            + player.get_current_state() + ")");
        frame.talk(Color.red, "The remote side violates communicating rules.");
        player.set_current_state(State.ERROR);
      }
    catch (RemoteException ex)
      {
        // We will print the exception because this is a demo application
        // that may be modified for learning purposes.
        ex.printStackTrace();
      }
  }
}