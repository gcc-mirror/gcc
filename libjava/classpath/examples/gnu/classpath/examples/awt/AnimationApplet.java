/* AnimationApplet.java -- An example of an old-style AWT applet
   Copyright (C) 2006 Free Software Foundation, Inc.

This file is part of GNU Classpath examples.

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
02110-1301 USA. */

package gnu.classpath.examples.awt;

import java.awt.*;
import java.awt.event.*;
import java.applet.*;


/**
 * AnimationApplet demonstrates the need for Xflush calls in
 * GdkGraphics.c.  To see how this demo can fail in their absence,
 * remove the contents of schedule_flush in GdkGraphics.c.  The
 * animation will be so choppy that it is effectively stopped.
 */
public class AnimationApplet
  extends Applet
  implements Runnable
{
  boolean going = false;
  Thread animThread = null;
  int SPEED = 5;
  int circleX = 0;
  int circleY = 0;
  int circleXold = 0;
  int circleYold = 0;
  int circleXdelta = 0;
  int circleYdelta = 0;
  int circleDiameter = 0;
  int autoCircleX = 0;
  int autoCircleY = 0;
  int autoCircleXold = 0;
  int autoCircleYold = 0;
  int autoCircleXdelta = (int) (0.66 * SPEED);
  int autoCircleYdelta = (int) (1.33 * SPEED);
  int boardWidth = 0;
  int boardHeight = 0;
  int CIRCLE_SIZE = 5;

  private Graphics appletGraphics;

  // Update the circles' location values.
  private void moveCircles()
  {
    circleX += circleXdelta;
    if (circleX < 0)
      circleX = 0;
    if (circleX > boardWidth - circleDiameter)
      circleX = boardWidth - circleDiameter;

    circleY += circleYdelta;
    if (circleY < 0)
      circleY = 0;
    if (circleY > boardHeight - circleDiameter)
      circleY = boardHeight - circleDiameter;

    autoCircleX += autoCircleXdelta;
    if (autoCircleX < 0)
      {
        autoCircleX = 0;
        autoCircleXdelta = -autoCircleXdelta;
      }
    if (autoCircleX > boardWidth - circleDiameter)
      {
        autoCircleX = boardWidth - circleDiameter;
        autoCircleXdelta = -autoCircleXdelta;
      }

    autoCircleY += autoCircleYdelta;
    if (autoCircleY < 0)
      {
        autoCircleY = 0;
        autoCircleYdelta = -autoCircleYdelta;
      }
    if (autoCircleY > boardHeight - circleDiameter)
      {
        autoCircleY = boardHeight - circleDiameter;
        autoCircleYdelta = -autoCircleYdelta;
      }
  }

  // Clear the circle in the old location and paint a new circle
  // in the new location.
  private void paintCircles()
  {
    appletGraphics.setColor(Color.BLUE);
    appletGraphics.fillOval(circleXold, circleYold, circleDiameter,
                            circleDiameter);
    appletGraphics.setColor(Color.YELLOW);
    appletGraphics.fillOval(circleX, circleY, circleDiameter,
                            circleDiameter);

    appletGraphics.setColor(Color.BLUE);
    appletGraphics.fillOval(autoCircleXold, autoCircleYold, circleDiameter,
                            circleDiameter);
    appletGraphics.setColor(Color.WHITE);
    appletGraphics.fillOval(autoCircleX, autoCircleY, circleDiameter,
                            circleDiameter);
  }

  // Override Applet.run.
  public void run()
  {
    while (animThread != null)
      {
        circleXold = circleX;
        circleYold = circleY;
        autoCircleXold = autoCircleX;
        autoCircleYold = autoCircleY;

        moveCircles();
        paintCircles();

        if (animThread != null)
          {
            try
              {
                Thread.sleep(20);
              }
            catch (InterruptedException e)
              {
              }
          }
      }
  }

  // Override Applet.paint.
  public void paint(Graphics g)
  {
    boardWidth = this.getSize().width;
    boardHeight = this.getSize().height;
    g.setColor(Color.BLUE);
    g.fillRect(0, 0, boardWidth, boardHeight);
    if (!going)
      {
        FontMetrics fm = appletGraphics.getFontMetrics();
        appletGraphics.setColor(Color.WHITE);
        String msg = "Click to Start";
        appletGraphics.drawString(msg,
                                  (boardWidth >> 1) - (fm.stringWidth(msg) >> 1),
                                  (boardHeight >> 1) - (fm.getHeight() >> 1));
      }
  }

  // Override Applet.destroy.
  public void destroy()
  {
    // animThread.stop();
    animThread = null;
  }

  // Override Applet.init.
  public void init()
  {
    boardWidth = this.getSize().width;
    boardHeight = this.getSize().height;
    going = false;
    appletGraphics = getGraphics();
    appletGraphics.setFont(new Font(appletGraphics.getFont().getName(),
                                    Font.BOLD, 15));
  }

  // Override Component.preferredSize for when we're run standalone.
  public Dimension preferredSize ()
  {
    return new Dimension (400, 400);
  }

  // Override Applet.handleEvent, the old-style AWT-event handler.
  public boolean handleEvent(Event event)
  {
    switch (event.id)
      {
      case Event.MOUSE_DOWN:
        if (!going)
          {
            going = true;
            circleDiameter = boardWidth / CIRCLE_SIZE;
            circleX = (boardWidth - circleDiameter) >> 1;
            circleY = (boardHeight - circleDiameter) >> 1;
            circleXdelta = 0;
            circleYdelta = 0;
            repaint();
            animThread = new Thread(this);
            animThread.start();
          }
        break;
      case Event.KEY_ACTION:
      case Event.KEY_PRESS:
        if (event.key == Event.LEFT)
          circleXdelta = -SPEED;
        else if (event.key == Event.RIGHT)
          circleXdelta = SPEED;
        else if (event.key == Event.UP)
          circleYdelta = -SPEED;
        else if (event.key == Event.DOWN)
          circleYdelta = SPEED;
        break;
      case Event.KEY_ACTION_RELEASE:
      case Event.KEY_RELEASE:
        if (event.key == Event.LEFT && circleXdelta < 0)
          circleXdelta = 0;
        else if (event.key == Event.RIGHT && circleXdelta > 0)
          circleXdelta = 0;
        else if (event.key == Event.UP && circleYdelta < 0)
          circleYdelta = 0;
        else if (event.key == Event.DOWN && circleYdelta > 0)
          circleYdelta = 0;
        break;
      default:
        break;
      }
    return false;
  }
}
