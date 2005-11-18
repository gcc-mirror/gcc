/* CanvasWorld.java --
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
import java.awt.Dimension;
import java.awt.Graphics;
import java.awt.Point;
import java.awt.event.MouseEvent;
import java.awt.event.MouseListener;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Iterator;

import javax.swing.JComponent;
import javax.swing.JFrame;
import javax.swing.JScrollPane;

/**
 * The purpose of this simple example is to check if the mouse events are
 * correctly received in a scrollable canvas and also if the canvas are
 * correctly repainted. The similar canvas are used in various games and
 * interactive demonstrations. 
 * 
 * The user can set one of the three possible figures with the different
 * mouse buttons. The figure must be set where the user have clicked the
 * mouse.
 *
 * @author Audrius Meskauskas (AudriusA@Bioinformatics.org)
 */
public class CanvasWorld
  extends JComponent
  implements MouseListener, State
{
  /**
   * Use serialVersionUID for interoperability.
   */
  private static final long serialVersionUID = 1;

  /**
   * Red oval, set by the left mouse button.
   */
  public static final int RED = 0;

  /**
   * Black cross, set by the right mouse button.
   */
  public static final int BLACK = 1;
  
  /**
   * Blue and smaller oval, set by the middle mouse button.
   */
  public static final int HINT = 2;
  
  /**
   * The message string is displayed at the top of the window.
   */
  String message = "Click left, right or middle button in to set the figure";
  
  /**
   * The additinal message, related to the mouse events.
   */
  String mouse = "No mouse event so far";

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
   * The collection of the black crosses.
   */
  ArrayList blacks = new ArrayList();
  
  /**
   * The collection of the smaller blue crosses.
   */
  ArrayList hints = new ArrayList();

  public CanvasWorld()
  {
    try
      {
        addMouseListener(this);
      }
    catch (Exception e)
      {
        throw new AssertionError(e);
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
    
    g.setColor(Color.black);
    
    g.drawString(message, W, W);
    g.drawString(mouse, W, 2*W);
    
    drawFigures(g);
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

    int xs = 2*W+W/2;
    
    // Draw vertical lines:
    for (int x = 0; x < w; x += W)
      {
        g.drawLine(x, xs, x, h);
      }

    // Draw horizontal lines:
    for (int y = 3*W; y < h; y += W)
      {
        g.drawLine(0, y, w, y);
      }
      
    g.setColor(Color.gray);
  }

  public void drawFigures(Graphics g)
  {
    g.setColor(Color.red);
    drawDots(reds, g, RED);

    g.setColor(Color.black);
    drawDots(blacks, g, BLACK);
    
    g.setColor(Color.blue);
    drawDots(hints, g, HINT);
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

  public void mouseClicked(MouseEvent e)
  {
    int x = e.getX();
    int y = e.getY();

    Point p = makePoint(x, y);

    // Ignore clicks on the occupied cells.
    if (pointPresent(p.x, p.y, reds) || (pointPresent(p.x, p.y, blacks)))
      {
        message = "Clicked on the occupied cell.";
        return;
      }
    else
      message = "Figure set at ["+p.x+","+p.y+"]";

    if (e.getButton() == MouseEvent.BUTTON1)
      reds.add(p);
    else if (e.getButton() == MouseEvent.BUTTON3)
      blacks.add(p);
    else if (e.getButton() == MouseEvent.BUTTON2)
      hints.add(p);
    repaint();
  }

  public void mouseEntered(MouseEvent m)
  {
    mouse = "Mouse entered.";
    repaint();  
  }

  public void mousePressed(MouseEvent m)
  {
    mouse = "Mouse pressed at "+m.getX()+","+m.getY();
    repaint();
  }

  public void mouseReleased(MouseEvent m)
  {
    mouse = "Mouse released at "+m.getX()+","+m.getY();    
    repaint();
  }

  public void mouseExited(MouseEvent m)
  {
    mouse = "Mouse exited";
    repaint();
  }
  
  public static void main(String[] args)
  {
    JFrame frame = new JFrame();
    CanvasWorld world = new CanvasWorld();
    world.setPreferredSize(new Dimension(1000,1000));
    frame.add(new JScrollPane(world));
    frame.setSize(400, 200);
    frame.setVisible(true);
  }

}