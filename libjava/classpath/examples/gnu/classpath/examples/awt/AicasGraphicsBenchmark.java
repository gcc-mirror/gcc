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

import java.awt.BorderLayout;
import java.awt.Canvas;
import java.awt.Color;
import java.awt.Dimension;
import java.awt.Frame;
import java.awt.Graphics;
import java.awt.Image;
import java.awt.Insets;
import java.awt.Label;
import java.awt.Panel;
import java.awt.Toolkit;
import java.awt.event.WindowAdapter;
import java.awt.event.WindowEvent;
import java.net.URL;
import java.util.Iterator;
import java.util.Map;
import java.util.StringTokenizer;
import java.util.TreeMap;
import java.util.logging.Level;
import java.util.logging.Logger;

public class AicasGraphicsBenchmark extends Panel
{
  /**
   * Default number of test-iterations.
   */
  private static final int DEFAULT_TEST_SIZE = 1000;

  /**
   * Default screen size.
   */
  private static final int DEFAULT_SCREEN_WIDTH  = 320;
  private static final int DEFAULT_SCREEN_HEIGHT = 240;

  /**
   * AWT tests.
   */
  private static final int AWTTEST_LINES = 1 << 0;
  private static final int AWTTEST_RECT = 1 << 1;
  private static final int AWTTEST_POLYLINE = 1 << 2;
  private static final int AWTTEST_POLYGON = 1 << 3;
  private static final int AWTTEST_ARC = 1 << 4;
  private static final int AWTTEST_OVAL = 1 << 5;
  private static final int AWTTEST_ROUNDRECT = 1 << 6;
  private static final int AWTTEST_STRING = 1 << 7;
  private static final int AWTTEST_TRANSPARENTIMAGE = 1 << 8;
  private static final int AWTTEST_IMAGE = 1 << 9;

  private static final int AWTTEST_NONE = 0;
  private static final int AWTTEST_ALL  =   AWTTEST_LINES
                                          | AWTTEST_RECT
                                          | AWTTEST_POLYLINE
                                          | AWTTEST_POLYGON
                                          | AWTTEST_ARC
                                          | AWTTEST_OVAL
                                          | AWTTEST_ROUNDRECT
                                          | AWTTEST_STRING
                                          | AWTTEST_TRANSPARENTIMAGE
                                          | AWTTEST_IMAGE
                                          ;

  int iterations = 1;
  private int screenWidth = DEFAULT_SCREEN_WIDTH;  
  private int screenHeight = DEFAULT_SCREEN_HEIGHT;  
  boolean doubleBufferFlag = true;
  private int awtTests = AWTTEST_ALL;

  private Label testLabel;

  private String testContext = "";

  Logger logger = Logger.getLogger("AicasGraphicsBenchmark");

  private Image pngTestImage;
  private Image gifTestImage;

  private TestSet testSetMap = new TestSet();

  public AicasGraphicsBenchmark()
  {
    pngTestImage = loadImage("../icons/aicas.png");
    gifTestImage = loadImage("../icons/palme.gif");

    setLayout(new BorderLayout());
    testLabel = new Label();
    add(testLabel,BorderLayout.NORTH);
    add(new GraphicsTest(),BorderLayout.CENTER);
  }

  void setTestContext(String testName)
  {
    logger.logp(Level.INFO, "AicasGraphicsBenchmark", "recordTest",
                "--- Starting new test context: " + testName);
    testContext = testName;
    testLabel.setText(testName);
  }

  private void recordTest(String testName, long time)
  {
    logger.logp(Level.INFO, "AicasGraphicsBenchmark", "recordTest",
                testContext + ": " + testName + " duration (ms): " + time);
    TestRecorder recorder = testSetMap.getTest(testName);
    if (recorder == null)
      {
        recorder = new TestRecorder(testName);
        testSetMap.putTest(testName,recorder);
      }
    recorder.addRun(time);
  }

  void printReport()
  {
    for (Iterator i = testSetMap.testIterator(); i.hasNext(); )
    {
      TestRecorder recorder = testSetMap.getTest((String)i.next());
      System.out.println("TEST " + recorder.getTestName() + ": average "
                         + recorder.getAverage() + "ms ["
                         + recorder.getMinTime() + "-" + recorder.getMaxTime()
                         + "]");
    }
  }

  public static void main(String[] args)
  {
    int awtTests;
    int i;
    boolean endOfOptionsFlag;
    AicasGraphicsBenchmark speed= new AicasGraphicsBenchmark();

    // Parse arguments.
    i = 0;
    endOfOptionsFlag = false;
    awtTests = AWTTEST_NONE;
    while (i < args.length)
      {
        if (!endOfOptionsFlag)
          {
            if (args[i].equals("--help") || args[i].equals("-help")
                || args[i].equals("-h"))
              {
                System.out.println("Usage: AicasGraphicsBenchmark [<options>] [<test>  ...]");
                System.out.println("");
                System.out.println("Options: -i|--iterations=<n|-1> - number of iterations (-1 is infinite)");
                System.out.println("         -w|--width=<n>         - screen width; default "+DEFAULT_SCREEN_WIDTH);
                System.out.println("         -h|--height=<n>        - screen height; default "+DEFAULT_SCREEN_HEIGHT);
                System.out.println("         -n|--noDoubleBuffer    - disable double-buffering test");
                System.out.println("");
                System.out.println("Tests: line");
                System.out.println("       rect");
                System.out.println("       polyline");
                System.out.println("       polygon");
                System.out.println("       arc");
                System.out.println("       oval");
                System.out.println("       roundrect");
                System.out.println("       string");
                System.out.println("       transparentimage");
                System.out.println("       image");
                System.exit(1);
              }
            else if ((args[i].startsWith("-i=")
                || args[i].startsWith("--iterations=")))
              {
                speed.iterations =
                  Integer.parseInt(args[i].substring(args[i].indexOf('=') + 1));
                i += 1;
                continue;
              }
            else if ((args[i].equals("-i") || args[i].equals("--iterations")))
              {
                if ((i + 1) >= args.length)
                  {
                    System.err.println("ERROR: No argument given for option '"
                                       + args[i] + "'!");
                    System.exit(2);
                  }
                speed.iterations = Integer.parseInt(args[i + 1]);
                i += 2;
                continue;
              }
            else if ((args[i].startsWith("-w=")
                || args[i].startsWith("--width=")))
              {
                speed.screenWidth =
                  Integer.parseInt(args[i].substring(args[i].indexOf('=') + 1));
                i += 1;
                continue;
              }
            else if ((args[i].equals("-w") || args[i].equals("--width")))
              {
                if ((i + 1) >= args.length)
                  {
                    System.err.println("ERROR: No argument given for option '"
                                       + args[i] + "'!");
                    System.exit(2);
                  }
                speed.screenWidth = Integer.parseInt(args[i + 1]);
                i += 2;
                continue;
              }
            else if ((args[i].startsWith("-h=")
                || args[i].startsWith("--height=")))
              {
                speed.screenHeight =
                  Integer.parseInt(args[i].substring(args[i].indexOf('=') + 1));
                i+=1;
                continue;
              }
            else if ((args[i].equals("-h") || args[i].equals("--height")))
              {
                if ((i+1) >= args.length)
                  {
                    System.err.println("ERROR: No argument given for option '"
                                       + args[i] + "'!");
                    System.exit(2);
                  }
                speed.screenHeight = Integer.parseInt(args[i + 1]);
                i += 2;
                continue;
              }
            else if ((args[i].equals("-n")
                || args[i].equals("--noDoubleBuffer")))
              {
                speed.doubleBufferFlag = false;
                i += 1;
                continue;
              }
            else if (args[i].equals("--"))
              {
                endOfOptionsFlag = true;
                i += 1;
                continue;
              }
            else if (args[i].startsWith("-"))
              {
                System.err.println("ERROR: Unknown option '" + args[i] + "'!");
                System.exit(2);
              }
          }
        StringTokenizer tokenizer = new StringTokenizer(args[i], " +,");
        while (tokenizer.hasMoreTokens())
          {
            String s = tokenizer.nextToken().toLowerCase();
            if (s.equals("line"))
              awtTests |= AWTTEST_LINES;
            else if (s.equals("rect"))
              awtTests |= AWTTEST_RECT;
            else if (s.equals("polyline"))
              awtTests |= AWTTEST_POLYLINE;
            else if (s.equals("polygon"))
              awtTests |= AWTTEST_POLYGON;
            else if (s.equals("arc"))
              awtTests |= AWTTEST_ARC;
            else if (s.equals("oval"))
              awtTests |= AWTTEST_OVAL;
            else if (s.equals("roundrect"))
              awtTests |= AWTTEST_ROUNDRECT;
            else if (s.equals("string"))
              awtTests |= AWTTEST_STRING;
            else if (s.equals("transparentimage"))
              awtTests |= AWTTEST_TRANSPARENTIMAGE;
            else if (s.equals("image"))
              awtTests |= AWTTEST_IMAGE;
            else
              {
                System.err.println("Unknown AWT test '" + s + "'!");
                System.exit(2);
              }
          }
        i += 1;
      }
    if (awtTests != AWTTEST_NONE)
      speed.awtTests = awtTests;

    // Create graphics.
    final Frame frame = new Frame("AicasGraphicsBenchmark");

    frame.addWindowListener(new WindowAdapter()
    {
      public void windowClosing(WindowEvent e)
      {
        frame.setVisible(false);
        System.exit(0);
      }
    });

    frame.add(speed,BorderLayout.CENTER);
    frame.setSize(speed.screenWidth,speed.screenHeight);
    frame.setVisible(true);

    // Insets are correctly set only after the native peer was created.
    Insets insets = frame.getInsets();
    // The internal size of the frame should be 320x240.
    frame.setSize(320 + insets.right + insets.left,
                  240 + insets.top + insets.bottom);
  }

  private Image loadImage(String imageName)
  {
    Image result = null;
    logger.logp(Level.INFO, "AicasGraphicsBenchmark", "loadImage",
                "Loading image: " + imageName);
    URL url = getClass().getResource(imageName);
    if (url != null)
      {
        result = Toolkit.getDefaultToolkit().getImage(url);
        prepareImage(result, this);
      }
    else
      {
        logger.logp(Level.WARNING, "AicasGraphicsBenchmark", "loadImage",
                    "Could not locate image resource in class path: "
                    + imageName);
      }
    return result;
  }

  /**
   * Executes the test methods.
   *
   * @param g The Graphics object that is used to paint.
   * @param size The size of the canvas.
   */
  void runTestSet(Graphics g, Dimension size)
  {
    if ((awtTests & AWTTEST_LINES) != 0)
      test_drawLine(g, size);
    if ((awtTests & AWTTEST_RECT) != 0)
      test_drawRect(g, size);
    if ((awtTests & AWTTEST_RECT) != 0)
      test_fillRect(g, size);
    if ((awtTests & AWTTEST_POLYLINE) != 0)
      test_drawPolyline(g, size);
    if ((awtTests & AWTTEST_POLYGON) != 0)
      test_drawPolygon(g, size);
    if ((awtTests & AWTTEST_POLYGON) != 0)
      test_fillPolygon(g,size);
    if ((awtTests & AWTTEST_ARC) != 0)
      test_drawArc(g,size);
    if ((awtTests & AWTTEST_ARC) != 0)
      test_fillArc(g,size);
    if ((awtTests & AWTTEST_OVAL) != 0)
      test_drawOval(g, size);
    if ((awtTests & AWTTEST_OVAL) != 0)
      test_fillOval(g, size);
    if ((awtTests & AWTTEST_ROUNDRECT) != 0)
      test_fillRoundRect(g, size);
    if ((awtTests & AWTTEST_STRING) != 0)
      test_drawString(g, size);
    if ((awtTests & AWTTEST_TRANSPARENTIMAGE) != 0)
      test_drawTransparentImage(g,size);
    if ((awtTests & AWTTEST_IMAGE) != 0)
      test_drawImage(g,size);
  }

  /**
   * Gets a new random Color.
   *
   * @returna new random Color
   */
  private Color getNextColor()
  {
    return new Color((int) (Math.random() * 254) + 1,
                     (int) (Math.random() * 254) + 1,
                     (int) (Math.random() * 254) + 1);
  }

  /**
   * Draws random lines within the given dimensions.
   *
   * @param g The Graphics object that is used to paint.
   * @param size The size of the canvas.
   */
  private void test_drawLine(Graphics g, Dimension size)
  {
    int maxTests = DEFAULT_TEST_SIZE;
    int minSize = 10;
    long startTime = System.currentTimeMillis();
    for (int i=0; i < maxTests; i += 1)
      {
        g.setColor(getNextColor());
        int x1 = (int) (Math.random() * (size.width-minSize));
        int y1 = (int) (Math.random() * (size.height-minSize));
        int x2 = (int) (Math.random() * (size.width-minSize));
        int y2 = (int) (Math.random() * (size.height-minSize));
        g.drawLine(x1, y1, x2, y2);
      }
    long endTime = System.currentTimeMillis();
    recordTest("drawLine " + maxTests + " times", (endTime-startTime));
  }

  /**
   * Draws random rectangles within the given dimensions.
   *
   * @param g The Graphics object that is used to paint.
   * @param size The size of the canvas.
   */
  private void test_drawRect(Graphics g, Dimension size)
  {
    int maxTests = DEFAULT_TEST_SIZE;
    int minSize = 10;
    long startTime = System.currentTimeMillis();
    for (int i=0; i < maxTests; i += 1)
      {
        g.setColor(getNextColor());
        int x1 = (int) (Math.random() * (size.width-minSize));
        int y1 = (int) (Math.random() * (size.height-minSize));
        int x2 = (int) (Math.random() * (size.width-minSize));
        int y2 = (int) (Math.random() * (size.height-minSize));
        g.drawRect(x1, y1, x2, y2);
      }
    long endTime = System.currentTimeMillis();
    recordTest("drawRect " + maxTests + " times", (endTime-startTime));
  }

  /**
   * Draws random rectangles within the given dimensions.
   *
   * @param g The Graphics object that is used to paint.
   * @param size The size of the canvas.
   */
  private void test_fillRect(Graphics g, Dimension size)
  {
    int maxTests = DEFAULT_TEST_SIZE;
    int minSize = 10;
    long startTime = System.currentTimeMillis();
    for (int i = 0; i < maxTests; i += 1)
      {
        g.setColor(getNextColor());
        int x1 = (int) (Math.random() * (size.width-minSize));
        int y1 = (int) (Math.random() * (size.height-minSize));
        int x2 = (int) (Math.random() * (size.width-minSize));
        int y2 = (int) (Math.random() * (size.height-minSize));
        g.fillRect(x1, y1, x2, y2);
      }
    long endTime = System.currentTimeMillis();
    recordTest("fillRect " + maxTests + " times", (endTime-startTime));
  }

  /**
   * Draws random polylines within the given dimensions.
   *
   * @param g The Graphics object that is used to paint.
   * @param size The size of the canvas.
   */
  private void test_drawPolyline(Graphics g, Dimension size)
  {
    int maxTests = DEFAULT_TEST_SIZE;
    long startTime = System.currentTimeMillis();
    for (int i=0; i < maxTests; i += 1)
      {
        g.setColor(getNextColor());
        int points = (int)(Math.random() * 6) + 3;
        int[] x_coords = new int[points];
        int[] y_coords = new int[points];
        for (int j = 0; j < points; j+=1)
          {
            x_coords[j] = (int)(Math.random() * (size.width));
            y_coords[j] = (int)(Math.random() * (size.height));
          }
        g.drawPolyline(x_coords,y_coords, points);
      }
    long endTime = System.currentTimeMillis();
    recordTest("drawPolyline " + maxTests + " times", (endTime-startTime));
  }

  /**
   * Draws random polygons within the given dimensions.
   * @param g The Graphics object that is used to paint.
   * @param size The size of the canvas.
   */
  private void test_drawPolygon(Graphics g, Dimension size)
  {
    int maxTests = DEFAULT_TEST_SIZE;
    long startTime = System.currentTimeMillis();
    for (int i=0; i < maxTests; i += 1)
      {
        g.setColor(getNextColor());
        int points = (int) (Math.random() * 6) + 3;
        int[] xcoords = new int[points];
        int[] ycoords = new int[points];
        for(int j = 0; j < points; j+=1)
          {
            xcoords[j] = (int) (Math.random() * (size.width));
            ycoords[j] = (int) (Math.random() * (size.height));
          }
        g.drawPolygon(xcoords, ycoords, points);
      }
    long endTime = System.currentTimeMillis();
    recordTest("drawPolygon " + maxTests + " times", (endTime-startTime));
  }

  /**
   * Draws random filled polygons within the given dimensions.
   *
   * @param g The Graphics object that is used to paint.
   * @param size The size of the canvas.
   */
  private void test_fillPolygon(Graphics g, Dimension size)
  {
    int maxTests = DEFAULT_TEST_SIZE;
    long startTime = System.currentTimeMillis();
    for (int i=0; i < maxTests; i += 1)
      {
        g.setColor(getNextColor());
        int points = (int) (Math.random() * 6) + 3;
        int[] xcoords = new int[points];
        int[] ycoords = new int[points];
        for (int j = 0; j < points; j+=1)
          {
            xcoords[j] = (int) (Math.random() * (size.width));
            ycoords[j] = (int) (Math.random() * (size.height));
          }
        g.fillPolygon(xcoords, ycoords, points);
      }
    long endTime = System.currentTimeMillis();
    recordTest("fillPolygon " + maxTests + " times", (endTime-startTime));
  }

  /**
   * Draws random arcs within the given dimensions.
   *
   * @param g The Graphics object that is used to paint.
   * @param size The size of the canvas.
   */
  private void test_drawArc(Graphics g, Dimension size)
  {
    int maxTests = DEFAULT_TEST_SIZE;
    int minSize;
    long startTime;
    long endTime;
    minSize = 10;
    startTime = System.currentTimeMillis();
    for (int i=0; i < maxTests; i += 1)
      {
        g.setColor(getNextColor());
        int x = (int) (Math.random() * (size.width - minSize + 1));
        int y = (int) (Math.random() * (size.height - minSize + 1));
        int width = (int) (Math.random() * (size.width - x - minSize) + minSize);
        int height = (int) (Math.random() * (size.height - y - minSize) + minSize);
        int startAngle = (int) (Math.random() * 360);
        int arcAngle = (int) (Math.random() * 360 - startAngle);
        g.drawArc(x, y, width, height, startAngle, arcAngle);
      }
    endTime = System.currentTimeMillis();
    recordTest("drawArc " + maxTests + " times", (endTime-startTime));
  }

  /**
   * Draws random filled arcs within the given dimensions.
   * @param g The Graphics object that is used to paint.
   * @param size The size of the canvas.
   */
  private void test_fillArc(Graphics g, Dimension size)
  {
    int maxTests = DEFAULT_TEST_SIZE;
    int minSize;
    long startTime;
    long endTime;
    minSize = 10;
    startTime = System.currentTimeMillis();
    for (int i = 0; i < maxTests; i += 1)
    {
      g.setColor(getNextColor());
      int x = (int) (Math.random() * (size.width - minSize + 1));
      int y = (int) (Math.random() * (size.height - minSize + 1));
      int width = (int)(Math.random() * (size.width - x - minSize) + minSize);
      int height = (int)(Math.random() * (size.height - y - minSize) + minSize);
      int startAngle = (int)(Math.random() * 360);
      int arcAngle = (int)(Math.random() * 360);
      g.fillArc(x, y, width, height, startAngle, arcAngle);

    }
    endTime = System.currentTimeMillis();
    recordTest("fillArc " + maxTests + " times", (endTime - startTime));
  }

  /**
   * Draws random ovals within the given dimensions.
   *
   * @param g The Graphics object that is used to paint.
   * @param size The size of the canvas.
   */
  private void test_drawOval(Graphics g, Dimension size)
  {
    int maxTests = DEFAULT_TEST_SIZE;
    int minSize;
    long startTime;
    long endTime;
    minSize = 10;
    startTime = System.currentTimeMillis();
    for (int i = 0; i < maxTests; i += 1)
      {
        g.setColor(getNextColor());
        int x = (int)(Math.random() * (size.width - minSize + 1));
        int y = (int)(Math.random() * (size.height - minSize + 1));
        int width = (int)(Math.random() * (size.width - x - minSize) + minSize);
        int height = (int)(Math.random() * (size.height - y - minSize) + minSize);
        g.drawOval(x, y, Math.min(width, height), Math.min(width, height));
      }
    endTime = System.currentTimeMillis();
    recordTest("drawOval " + maxTests + " times", (endTime-startTime));
  }

  /**
   * Draws random filled ovals within the given dimensions.
   *
   * @param g The Graphics object that is used to paint.
   * @param size The size of the canvas.
   */
  private void test_fillOval(Graphics g, Dimension size)
  {
    int maxTests = DEFAULT_TEST_SIZE;
    int minSize;
    long startTime;
    long endTime;
    minSize = 10;
    startTime = System.currentTimeMillis();
    for (int i = 0; i < maxTests; i += 1)
      {
        g.setColor(getNextColor());
        int x = (int) (Math.random() * (size.width - minSize + 1));
        int y = (int) (Math.random() * (size.height - minSize + 1));
        int width = (int) (Math.random() * (size.width - x - minSize) + minSize);
        int height = (int) (Math.random() * (size.height - y - minSize) + minSize);
        g.fillOval(x, y, width,height);
      }
    endTime = System.currentTimeMillis();
    recordTest("fillOval " + maxTests + " times", (endTime-startTime));
  }

  /**
   * Draws random filled rounded rectangles within the given dimensions.
   *
   * @param g The Graphics object that is used to paint.
   * @param size The size of the canvas.
   */
  private void test_fillRoundRect(Graphics g, Dimension size)
  {
    int maxTests = DEFAULT_TEST_SIZE;
    int minSize;
    long startTime;
    long endTime;
    minSize = 10;
    startTime = System.currentTimeMillis();
    for (int i=0; i < maxTests; i+=1)
      {
        g.setColor(getNextColor());
        int x = (int) (Math.random() * (size.width - minSize + 1));
        int y = (int) (Math.random() * (size.height - minSize + 1));
        int width = (int) (Math.random() * (size.width - x - minSize) + minSize);
        int height = (int) (Math.random() * (size.height - y - minSize) + minSize);
        int arcWidth = (int) (Math.random() * (width - 1) + 1);
        int arcHeight = (int) (Math.random() * (height - 1) + 5);
        g.fillRoundRect(x, y, width, height, arcWidth, arcHeight);
      }
    endTime = System.currentTimeMillis();
    recordTest("fillRoundRect " + maxTests + " times", (endTime-startTime));
  }

  /**
   * Draws random images within the given dimensions.
   *
   * @param g The Graphics object that is used to paint.
   * @param size The size of the canvas.
   */
  private void test_drawImage(Graphics g, Dimension size)
  {
    if (gifTestImage == null)
      {
        logger.logp(Level.WARNING, "AicasGraphicsBenchmark", "runTestSet",
                    "Skipping 'test_drawImage' due to missing resource.");
        return;
      }
    
    int maxTests = DEFAULT_TEST_SIZE / 2;
    if(maxTests == 0)
      maxTests = 1;
    int imageWidth = gifTestImage.getWidth(this);
    int imageHeight = gifTestImage.getHeight(this);
    long  startTime = System.currentTimeMillis();
    for (int i = 0; i < maxTests; i += 1)
      {
        g.setColor(getNextColor());
        int x = (int) (Math.random() * (size.width - imageWidth + 1));
        int y = (int) (Math.random() * (size.height - imageHeight + 1));
        g.drawImage(gifTestImage, x, y, this);
      }
    long endTime = System.currentTimeMillis();
    recordTest("drawImage " + maxTests + " times", (endTime-startTime));
  }

  /**
   * Draws random transparent images within the given dimensions.
   *
   * @param g The Graphics object that is used to paint.
   * @param size The size of the canvas.
   */
  private void test_drawTransparentImage(Graphics g, Dimension size)
  {
    if (pngTestImage == null)
      {
        logger.logp(Level.WARNING, "AicasGraphicsBenchmark", "runTestSet",
                    "Skipping 'test_drawTransparentImage' due to missing resource.");
        return;
      }

    
    int maxTests = DEFAULT_TEST_SIZE / 5;
    if(maxTests == 0)
      maxTests = 1;
    int imageWidth = pngTestImage.getWidth(this);
    int imageHeight = pngTestImage.getHeight(this);
    long  startTime = System.currentTimeMillis();
    for (int i = 0; i < maxTests; i += 1)
      {
        g.setColor(getNextColor());
        int x = (int) (Math.random() * (size.width - imageWidth + 1));
        int y = (int) (Math.random() * (size.height - imageHeight + 1));
        g.drawImage(pngTestImage, x, y, this);
      }
    long endTime = System.currentTimeMillis();
    recordTest("draw transparent image " + maxTests + " times",
               (endTime-startTime));
  }

  /**
   * Draws random strings within the given dimensions.
   *
   * @param g The Graphics object that is used to paint.
   * @param size The size of the canvas.
   */
  private void test_drawString(Graphics g, Dimension size)
  {
      int maxTests = DEFAULT_TEST_SIZE;
      String testString = "HelloWorld";
      int stringWidth = g.getFontMetrics().stringWidth(testString);
      int stringHeight = g.getFontMetrics().getHeight();
      
      long startTime = System.currentTimeMillis();
      for(int i = 0; i < maxTests; i += 1)
        {
          g.setColor(getNextColor());
          g.drawString(testString, (int) (Math.random() * (size.width - stringWidth + 1)),(int)(Math.random() * (size.height - stringHeight + 1)) + stringHeight);
        }
      long endTime = System.currentTimeMillis();
      recordTest("drawString " + maxTests + " times", (endTime-startTime));
  }

  private class GraphicsTest extends Canvas implements Runnable
  {
    Thread paintThread;
    boolean done = false;
    boolean doPaint = false;
    boolean withClipping = false;

    public GraphicsTest()
    {
      paintThread = new Thread(this);
      paintThread.start();
    }

    public void run()
    {
      int runCount = 0;
      while (!done)
        {
          runCount++;

          try
            {
              synchronized (this)
                {
                  while (!doPaint)
                    {
                      try
                        {
                          wait(200);
                        }
                      catch (InterruptedException exception)
                        {
                          return;
                        }
                    }
                }

              if (iterations != 0)
                System.out.println("--- run...(" + runCount + "/" + iterations
                                   + ") ------------------------------------------------------");

              Graphics g = getGraphics();
              Dimension size = getSize();
              logger.logp(Level.INFO, "AicasGraphicsBenchmark.GraphicsTest", "run",
              "Start testing non-double-buffered drawing");
              runSet_noClipping(g,size);
              runSet_zeroClipping(g, size);
              runSet_withClipping(g, size);
              g.dispose();

              if (doubleBufferFlag)
                {
                  logger.logp(Level.INFO, "AicasGraphicsBenchmark.GraphicsTest",
                              "run", "Start testing double-buffered drawing");
                  Graphics canvas = getGraphics();
                  Image doublebuffer = createImage(size.width,size.height);
                  g = doublebuffer.getGraphics();
                  runSet_noClipping(g,size);
                  g.dispose();
                  canvas.drawImage(doublebuffer, 0, 0, this);

                  g = doublebuffer.getGraphics();
                  runSet_withClipping(g, size);
                  g.dispose();
                  canvas.drawImage(doublebuffer, 0, 0, this);

                  g = doublebuffer.getGraphics();
                  runSet_zeroClipping(g, size);
                  g.dispose();
                  canvas.drawImage(doublebuffer, 0, 0, this);
                  canvas.dispose();
                }

              printReport();

              if (iterations != 0)
                {
                  if (iterations != -1)
                    iterations--;
                }
              else
                {
                  System.out.println("--- done --------------------------------------------------------");
                  synchronized (this)
                    {
                      doPaint = false;
                    }
                  done = true;
                }
            }
          catch (Error error)
            {
              System.err.println("Error: " + error);
              System.exit(129);
            }
        }
      System.exit(0);
    }

    private void runSet_zeroClipping(Graphics g, Dimension size)
    {
      int clipped_width;
      int clipped_height;
      int clipped_x;
      int clipped_y;

      clipped_width = 0;
      clipped_height = 0;
      clipped_x = (size.width) / 2;
      clipped_y = (size.height) / 2;
      g.setClip(0, 0, size.width, size.height);
      g.setColor(Color.BLACK);
      g.fillRect(0, 0, size.width, size.height);
      g.setColor(Color.WHITE);
      g.drawRect(0, 0, size.width - 1, size.height - 1);
      g.fillRect(clipped_x - 1, clipped_y - 1, clipped_width + 2, clipped_height + 2);

      g.clipRect(clipped_x, clipped_y, clipped_width, clipped_height);
      g.setColor(Color.BLACK);
      g.fillRect(0, 0, size.width, size.height);

      setTestContext("clipping to zero");

      runTestSet(g, size);
    }

    private void runSet_withClipping(Graphics g, Dimension size)
    {
      int clipped_width = 2 * size.width / 3;
      int clipped_height = 2 * size.height / 3;
      int clipped_x = (size.width - clipped_width) / 2;
      int clipped_y = (size.height - clipped_height) / 2;

      g.setClip(0,0,size.width,size.height);

      g.setColor(Color.BLACK);
      g.fillRect(0, 0, size.width, size.height);
      g.setColor(Color.GREEN);
      g.drawRect(0, 0, size.width - 1, size.height - 1);
      g.setColor(Color.WHITE);
      g.fillRect(clipped_x - 1, clipped_y - 1, clipped_width + 2, clipped_height + 2);

      g.clipRect(clipped_x, clipped_y, clipped_width, clipped_height);
      g.setColor(Color.BLACK);
      g.fillRect(0, 0, size.width, size.height);

      setTestContext("with clipping");

      runTestSet(g, size);
    }

    public void runSet_noClipping(Graphics g, Dimension size)
    {
      g.setColor(Color.BLACK);
      g.fillRect(0, 0, size.width, size.height);

      setTestContext("without clipping");

      runTestSet(g, size);
    }

    public void paint(Graphics g)
    {
      synchronized(this)
        {
          doPaint=true;
          notify();
        }
    }
  }
}

class TestContext
{
}

class TestSet
{
  private Map testsMap = new TreeMap();

  public void putTest(String testName, TestRecorder recoder)
  {
    testsMap.put(testName,recoder);
  }

  public TestRecorder getTest(String testName)
  {
    return (TestRecorder)testsMap.get(testName);
  }

  public Iterator testIterator()
  {
    return testsMap.keySet().iterator();
  }
}

class TestRecorder
{
  String test;
  long   totalTime = 0;
  long   minTime   = Long.MAX_VALUE;
  long   maxTime   = Long.MIN_VALUE;
  int    runCount  = 0;

  /**
   * @return Returns the maxTime.
   */
  public final long getMaxTime()
  {
    return maxTime;
  }

  /**
   * @return Returns the minTime.
   */
  public final long getMinTime()
  {
    return minTime;
  }

  /**
   * @return Returns the test name.
   */
  public final String getTestName()
  {
    return test;
  }

  public final double getAverage()
  {
    return ((double)totalTime) / ((double)runCount);
  }

  public TestRecorder(String testName)
  {
    test = testName;
  }

  public void addRun(long time)
  {
    totalTime += time;
    if(minTime > time)
      minTime = time;
    if(maxTime < time)
      maxTime = time;
    runCount += 1;
  }
}
