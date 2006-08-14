/* JNIOverhead.java - demonstrator for classpath/gcj fillrect performance issue
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

package gnu.classpath.examples.java2d;

import gnu.classpath.examples.swing.DemoFactory;

import java.awt.BorderLayout;
import java.awt.Color;
import java.awt.Dimension;
import java.awt.Graphics;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;

import javax.swing.JButton;
import javax.swing.JCheckBox;
import javax.swing.JComponent;
import javax.swing.JFrame;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.SwingUtilities;

/** 
 * @author Norman Hendrich
 */
public class JNIOverhead
  extends JPanel
  implements ActionListener 
{

  static JNIOverhead fillRectDemo;

  LCDCanvas lcd;
  Worker worker;
  JLabel label;
  JCheckBox translate;
  JCheckBox lines;

  int     nx = 128;
  int     ny = 64;
  int     matrix[][], future[][];
  int     generation = 0;

  // 20 msec, or 50 repaints per sec (theoretically)
  int     sleepMillis = 20;
  long    lastMillis = System.currentTimeMillis();

  boolean enableRepaints = true;
  
  /**
   * If true, test translation.
   */
  boolean testTranslation = false;
  
  /**
   * If true, paint lines rather than rectangles
   */
  boolean paintLines;

  public void actionPerformed(ActionEvent e) 
  {
    if (e.getActionCommand().equals("CLOSE"))
    {
      System.exit(0);
    }
  }

  public JNIOverhead()
  {
    setSize(nx, ny);
    createContent();
  }

  public void createContent()
  {
    setLayout(new BorderLayout());

    JPanel p = new JPanel(new BorderLayout());
    lcd   = new LCDCanvas();
    label = new JLabel();
    label.setText("not running");
    
    translate = new JCheckBox("translate");
    translate.addActionListener(new ActionListener()
    {
      public void actionPerformed(ActionEvent event)
      {
        testTranslation = translate.isSelected();
      }
    });
    
    lines = new JCheckBox("lines");
    lines.addActionListener(new ActionListener()
    {
      public void actionPerformed(ActionEvent event)
      {
        paintLines = lines.isSelected();
      }
    });
    
    JPanel bottom = new JPanel();
    bottom.add(lines);
    bottom.add(translate);
    
    p.add(lcd, BorderLayout.CENTER);
    p.add(bottom, BorderLayout.SOUTH);
    p.add(label, BorderLayout.NORTH);
    add(p);
  }

  public void setSize(int _nx,int _ny )
  {
    nx = _nx;
    ny = _ny;
    matrix = new int[nx][ny];
    future = new int[nx][ny];
  }

  public void initFrameContent()
  {
    JPanel closePanel = new JPanel();
    JButton closeButton = new JButton("Close");
    closeButton.setActionCommand("CLOSE");
    closeButton.addActionListener(this);
    closePanel.add(closeButton);
    add(closePanel, BorderLayout.SOUTH);
  }

  public void setSleepMillis(int millis)
  {
    sleepMillis = millis;
  }

  public class LCDCanvas extends JPanel
  {
    private int   sx, sy;
    private Color activePixel  = new Color(30, 30, 40);
    private Color passivePixel = new Color(200, 180, 240);
    private Color gridPixel    = new Color(255, 240, 240);

    public LCDCanvas()
    {
      super();
      sx = 4 * nx;
      sy = 4 * ny;
    }

    public void paintComponent(Graphics g)
    {
      // for buffered drawing - not used atm
      // g.drawImage( buffer, 0, 0, null );
      long t1 = System.currentTimeMillis();

      g.setColor(gridPixel);
      g.fillRect(0, 0, sx, sy);

      Color pixelColor = null;

      int dx, dy;

      if (paintLines)
        {
          for (int ix = 0; ix < nx; ix++)
            for (int iy = 0; iy < ny; iy++)
              {
                if (matrix[ix][iy] != 0)
                  pixelColor = activePixel;
                else
                  pixelColor = passivePixel;

                dx = 4 * ix;
                dy = 4 * iy;
                g.setColor(pixelColor);

                if (testTranslation)
                  {
                    g.translate(dx, dy);
                    g.drawLine(0, 0, 5, 5);
                    g.translate(- dx, - dy);
                  }
                else
                  g.drawLine(dx, dy, dx + 5, dy + 5);
              }
        }
      else
        for (int ix = 0; ix < nx; ix++)
          {
            for (int iy = 0; iy < ny; iy++)
              {
                if (matrix[ix][iy] != 0)
                  pixelColor = activePixel;
                else
                  pixelColor = passivePixel;

                dx = 4 * ix;
                dy = 4 * iy;
                g.setColor(pixelColor);

                if (testTranslation)
                  {
                    g.translate(dx, dy);
                    g.fillRect(0, 0, 3, 3);
                    g.translate(- dx, - dy);
                  }
                else
                  g.fillRect(dx, dy, 3, 3);
              }
          }

      long t2 = System.currentTimeMillis();

      label.setText("paintComponent took " + (t2 - t1) + " msec. " + "("
                    + (nx * ny + 1) + " "
                    + (paintLines ? "drawLine" : "fillRect") + " calls)");

    }

    public Dimension getPreferredSize()
    {
      return new Dimension(sx,sy);
    }

    public Dimension getMinimumSize()
    {
      return new Dimension(sx,sy);
    }
  }

  public class Worker extends Thread
  {
    public void run()
    {
      boolean running = true;
      while(running)
        {
          iteration();

          if (enableRepaints)
            display();

          if (sleepMillis > 0)
            {
              try
                {
                  Thread.sleep( sleepMillis );
                }
              catch(InterruptedException ie)
                {
                  running = false;
                }
            }
        }
    }
  }

  /** 
   * stupid animation algorithm: show binary representation of current
   * iteration.
   */
  public void iteration()
  {
    generation++;

    for (int i = 0; i < nx; i++)
      {
        long tmp1 = 1L << i;
        for (int j = 0; j < ny; j++)
          {
            // count neighbors
            long tmp2 = (1L << j);

        
            long tmp3 = generation & tmp1 & tmp2;
            if (tmp3 != 0) 
              matrix[i][j] = 1;
            else
              matrix[i][j] = 0;
          }
    }

    if ((generation % 100) == 0)
      {
        long t = System.currentTimeMillis();
        //        System.out.println(
        //           " generation= " + generation +
        //           " iterations/sec= " + 100.0*1000/(t-lastMillis) );
        lastMillis = t;
      }
  }

  public void display()
  {
    lcd.repaint();
  }

  public static void usage()
  {
    System.out.println( 
      "Usage: <java> FillRect2 [-sleep <millis>] [-size <int>] [-nopaint]\n"
    + "Example: jamvm FillRect2 -sleep 10 -size 100\n"
    );
    System.exit(0);
  }

  public static void main(String args[])
    throws Exception
  {
    fillRectDemo = new JNIOverhead();
    for (int i = 0; i < args.length; i++)
      {
        if ("-help".equals(args[i]))
          {
            usage();
          }
        if ("-sleep".equals(args[i]))
          {
            fillRectDemo.setSleepMillis( Integer.parseInt(args[i + 1]));
            i++;
          }
        if ("-size".equals(args[i]))
          {
            int size = Integer.parseInt(args[i + 1]);
            fillRectDemo.setSize(size, size);
            i++;
          }
        if ("-nopaint".equals(args[i]))
          {
            fillRectDemo.enableRepaints = false; 
          }
      }

    SwingUtilities.invokeLater (new Runnable()
     {
       public void run()
       {

         fillRectDemo.initFrameContent();
         JFrame frame = new JFrame("FillRect performance test");
         frame.getContentPane().add(fillRectDemo);
         frame.pack();
         frame.show();
         fillRectDemo.worker = fillRectDemo.new Worker();
         fillRectDemo.worker.start();
       }
      });
  }

  /**
   * Returns a DemoFactory that creates a SliderDemo.
   *
   * @return a DemoFactory that creates a SliderDemo
   */
  public static DemoFactory createDemoFactory()
  {    
    return new DemoFactory()
    {
      public JComponent createDemo()
      {
        fillRectDemo = new JNIOverhead();
        SwingUtilities.invokeLater
        (new Runnable()
         {
           public void run()
           {
             fillRectDemo.worker = fillRectDemo.new Worker();
             fillRectDemo.worker.start();
           }
         });
        return fillRectDemo;
      }
    };
  }
}
