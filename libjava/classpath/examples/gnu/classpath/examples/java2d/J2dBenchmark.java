/* J2dBenchmark.java -- Benchmarking utility for java2d,
   based on the Aicas AWT benchmarker
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

import java.awt.AlphaComposite;
import java.awt.BasicStroke;
import java.awt.BorderLayout;
import java.awt.Canvas;
import java.awt.Color;
import java.awt.Dimension;
import java.awt.Frame;
import java.awt.GradientPaint;
import java.awt.Graphics;
import java.awt.Graphics2D;
import java.awt.Image;
import java.awt.Insets;
import java.awt.Label;
import java.awt.MediaTracker;
import java.awt.Panel;
import java.awt.Rectangle;
import java.awt.RenderingHints;
import java.awt.TexturePaint;
import java.awt.Toolkit;
import java.awt.event.WindowAdapter;
import java.awt.event.WindowEvent;
import java.awt.geom.AffineTransform;
import java.awt.geom.Arc2D;
import java.awt.geom.CubicCurve2D;
import java.awt.geom.Ellipse2D;
import java.awt.geom.GeneralPath;
import java.awt.geom.Line2D;
import java.awt.geom.QuadCurve2D;
import java.awt.geom.Rectangle2D;
import java.awt.geom.RoundRectangle2D;
import java.awt.image.BufferedImage;
import java.net.URL;
import java.util.Iterator;
import java.util.Map;
import java.util.StringTokenizer;
import java.util.TreeMap;
import java.util.logging.Level;
import java.util.logging.Logger;

public class J2dBenchmark
    extends Panel
{
  /**
   * Default number of test-iterations.
   */
  protected static final int DEFAULT_TEST_SIZE = 1000;

  /**
   * Default screen size.
   */
  protected static final int DEFAULT_SCREEN_WIDTH = 320;

  protected static final int DEFAULT_SCREEN_HEIGHT = 240;

  /**
   * Java2D tests.
   */
  protected static final int J2DTEST_ARC = 1 << 0;

  protected static final int J2DTEST_CUBICCURVE = 1 << 1;

  protected static final int J2DTEST_ELLIPSE = 1 << 2;

  protected static final int J2DTEST_GENERALPATH = 1 << 3;

  protected static final int J2DTEST_LINE = 1 << 4;

  protected static final int J2DTEST_QUADCURVE = 1 << 5;

  protected static final int J2DTEST_RECTANGLE = 1 << 6;

  protected static final int J2DTEST_ROUNDRECTANGLE = 1 << 7;

  protected static final int J2DTEST_IMAGE = 1 << 8;

  protected static final int J2DTEST_NONE = 0;

  /*
  private static final int J2DTEST_ALL = J2DTEST_ARC | J2DTEST_CUBICCURVE
                                         | J2DTEST_ELLIPSE
                                         | J2DTEST_GENERALPATH | J2DTEST_LINE
                                         | J2DTEST_QUADCURVE
                                         | J2DTEST_RECTANGLE
                                         | J2DTEST_ROUNDRECTANGLE
                                         | J2DTEST_IMAGE;
  */
  private static final int J2DTEST_ALL = J2DTEST_ARC | J2DTEST_CUBICCURVE
                                         | J2DTEST_ELLIPSE
                                         | J2DTEST_LINE
                                         | J2DTEST_QUADCURVE
                                         | J2DTEST_RECTANGLE
                                         | J2DTEST_ROUNDRECTANGLE
                                         | J2DTEST_IMAGE;

  int iterations = 1;

  protected int screenWidth = DEFAULT_SCREEN_WIDTH;

  protected int screenHeight = DEFAULT_SCREEN_HEIGHT;

  protected boolean noClippingFlag = true;

  protected boolean withClippingFlag = true;

  protected boolean zeroClippingFlag = true;

  protected boolean singleBufferFlag = true;

  protected boolean doubleBufferFlag = true;

  protected boolean gradientFlag = false;

  protected String texture = null;

  protected boolean strokeFlag = false;

  protected float composite = 1;

  protected int xtranslate = 0;

  protected int ytranslate = 0;

  protected double xshear = 0;

  protected double yshear = 0;

  protected double rotate = 0;

  protected boolean antialiasFlag = false;

  protected AffineTransform affineTransform = null;

  protected int awtTests = J2DTEST_ALL;

  protected int testSize = DEFAULT_TEST_SIZE;

  private Label testLabel;

  private String testContext = "";

  Logger logger = Logger.getLogger("J2dGraphicsBenchmark");

  private Image pngTestImage;

  private Image gifTestImage;

  protected BufferedImage textureImage;

  protected TestSet testSetMap = new TestSet();

  public String init()
  {
    boolean loadError = false;
    pngTestImage = loadImage("../icons/aicas.png");
    gifTestImage = loadImage("../icons/palme.gif");

    if (texture != null)
      {
        textureImage = loadBufferedImage(texture);

        if (textureImage == null)
          {
            logger.logp(Level.WARNING, "J2dGraphicsBenchmark", "init",
                        "Unable to load texture - defaulting "
                            + "to solid colours");
            texture = null;
            loadError = true;
          }
      }

    setLayout(new BorderLayout());
    testLabel = new Label();
    add(testLabel, BorderLayout.NORTH);
    add(new GraphicsTest(), BorderLayout.CENTER);

    if (loadError)
      return "Unable to load image";
    else
      return null;
  }

  void setTestContext(String testName)
  {
    logger.logp(Level.INFO, "J2dGraphicsBenchmark", "recordTest",
                "--- Starting new test context: " + testName);
    testContext = testName;
    testLabel.setText(testName);
  }

  private void recordTest(String testName, long time)
  {
    logger.logp(Level.INFO, "J2dGraphicsBenchmark", "recordTest",
                testContext + ": " + testName + " duration (ms): " + time);
    TestRecorder recorder = testSetMap.getTest(testName);
    if (recorder == null)
      {
        recorder = new TestRecorder(testName);
        testSetMap.putTest(testName, recorder);
      }
    recorder.addRun(time);
  }

  void printReport()
  {
    for (Iterator i = testSetMap.testIterator(); i.hasNext();)
      {
        TestRecorder recorder = testSetMap.getTest((String) i.next());
        System.out.println("TEST " + recorder.getTestName() + ": average "
                           + recorder.getAverage() + "ms ["
                           + recorder.getMinTime() + "-"
                           + recorder.getMaxTime() + "]");
      }
  }

  void testComplete()
  {
    System.exit(0);
  }

  public static void main(String[] args)
  {
    int awtTests;
    int i;
    boolean endOfOptionsFlag;
    J2dBenchmark speed = new J2dBenchmark();

    // Parse arguments.
    i = 0;
    endOfOptionsFlag = false;
    awtTests = J2DTEST_NONE;
    while (i < args.length)
      {
        if (! endOfOptionsFlag)
          {
            if (args[i].equals("--help") || args[i].equals("-help")
                || args[i].equals("-h"))
              {
                System.out.println("Usage: J2dBenchmark [<options>] [<test>  ...]");
                System.out.println("");
                System.out.println("Options: -i|--iterations=<n|-1> - number of iterations (-1 is infinite; default "
                                   + speed.iterations + ")");
                System.out.println("         -w|--width=<n>         - screen width; default "
                                   + DEFAULT_SCREEN_WIDTH);
                System.out.println("         -h|--height=<n>        - screen height; default "
                                   + DEFAULT_SCREEN_HEIGHT);
                System.out.println("         -d|--noDoubleBuffer    - disable double-buffering test");
                System.out.println("         -s|--testsize=<n>      - size of each test; default "
                                   + DEFAULT_TEST_SIZE);
                System.out.println("         -c|--noClipping        - disable clipping test");
                System.out.println("         -z|--noZeroClipping    - disable clipping to zero test");
                System.out.println("");
                System.out.println("Additional options:");
                System.out.println("         --with-gradients       - enable gradients (not compatible with --texture)");
                System.out.println("         --with-stroking        - enable random stroking");
                System.out.println("         --texture=<file>       - enable texturing with this file (not compatible with --with-gradients)");
                System.out.println("         --composite=<n|-1>     - set alpha composite level; -1 for random; default 1.0 (no transparency)");
                System.out.println("         --anti-alias=<on|off>  - set anti-aliasing hint (not all implementations respect this); default off");
                System.out.println("         --x-translate=<n>      - set x-axis translation; default 0");
                System.out.println("         --y-translate=<n>      - set y-axis translation; default 0");
                System.out.println("         --x-shear=<n>          - set x-axis shear; default 0");
                System.out.println("         --y-shear=<n>          - set y-axis shear; default 0");
                System.out.println("         --rotate=<n|-1>        - set rotation (radians); -1 for random; default: 0 (none)");
                System.out.println("");
                System.out.println("Tests: arc");
                System.out.println("       cubiccurve");
                System.out.println("       ellipse");
                // System.out.println(" generalpath");
                System.out.println("       line");
                System.out.println("       quadcurve");
                System.out.println("       rectangle");
                System.out.println("       roundrectangle");
                System.out.println("       image");
                System.exit(1);
              }
            else if ((args[i].startsWith("-i=") || args[i].startsWith("--iterations=")))
              {
                speed.iterations = Integer.parseInt(args[i].substring(args[i].indexOf('=') + 1));
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
            else if ((args[i].startsWith("-w=") || args[i].startsWith("--width=")))
              {
                speed.screenWidth = Integer.parseInt(args[i].substring(args[i].indexOf('=') + 1));
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
            else if ((args[i].startsWith("-h=") || args[i].startsWith("--height=")))
              {
                speed.screenHeight = Integer.parseInt(args[i].substring(args[i].indexOf('=') + 1));
                i += 1;
                continue;
              }
            else if ((args[i].equals("-h") || args[i].equals("--height")))
              {
                if ((i + 1) >= args.length)
                  {
                    System.err.println("ERROR: No argument given for option '"
                                       + args[i] + "'!");
                    System.exit(2);
                  }
                speed.screenHeight = Integer.parseInt(args[i + 1]);
                i += 2;
                continue;
              }
            else if ((args[i].equals("-d") || args[i].equals("--noDoubleBuffer")))
              {
                speed.doubleBufferFlag = false;
                i += 1;
                continue;
              }
            else if ((args[i].startsWith("-s=") || args[i].startsWith("--testsize=")))
              {
                if ((i + 1) >= args.length)
                  {
                    System.err.println("ERROR: No argument given for option '"
                                       + args[i] + "'!");
                    System.exit(2);
                  }
                speed.testSize = Integer.parseInt(args[i].substring(args[i].indexOf('=') + 1));
                i += 1;
                continue;
              }
            else if ((args[i].equals("-s") || args[i].equals("--testsize")))
              {
                if ((i + 1) >= args.length)
                  {
                    System.err.println("ERROR: No argument given for option '"
                                       + args[i] + "'!");
                    System.exit(2);
                  }
                speed.testSize = Integer.parseInt(args[i + 1]);
                i += 2;
                continue;
              }
            else if ((args[i].equals("-c") || args[i].equals("--noClipping")))
              {
                speed.noClippingFlag = false;
                i += 1;
                continue;
              }
            else if ((args[i].equals("-z") || args[i].equals("--noZeroClipping")))
              {
                speed.zeroClippingFlag = false;
                i += 1;
                continue;
              }
            else if (args[i].equals("--with-gradients"))
              {
                speed.gradientFlag = true;
                i += 1;
                continue;
              }
            else if (args[i].equals("--with-stroking"))
              {
                speed.strokeFlag = true;
                i += 1;
                continue;
              }
            else if (args[i].startsWith("--texture="))
              {
                speed.texture = args[i].substring(args[i].indexOf('=') + 1);
                i += 1;
                continue;
              }
            else if (args[i].startsWith("--composite="))
              {
                speed.composite = Float.parseFloat(args[i].substring(args[i].indexOf('=') + 1));
                if (speed.composite != - 1
                    && (speed.composite < 0 || speed.composite > 1))
                  {
                    System.err.println("ERROR: Invalid value for composite (must be between 0 and 1, or -1 for random)");
                    System.exit(2);
                  }
                i += 1;
                continue;
              }
            else if (args[i].startsWith("--anti-alias="))
              {
                speed.antialiasFlag = (args[i].substring(args[i].indexOf('=') + 1).equals("on"));
                i += 1;
                continue;
              }
            else if (args[i].startsWith("--x-translate="))
              {
                speed.xtranslate = Integer.parseInt(args[i].substring(args[i].indexOf('=') + 1));
                i += 1;
                continue;
              }
            else if (args[i].startsWith("--y-translate="))
              {
                speed.ytranslate = Integer.parseInt(args[i].substring(args[i].indexOf('=') + 1));
                i += 1;
                continue;
              }
            else if (args[i].startsWith("--x-shear="))
              {
                speed.xshear = Double.parseDouble(args[i].substring(args[i].indexOf('=') + 1));
                i += 1;
                continue;
              }
            else if (args[i].startsWith("--y-shear="))
              {
                speed.yshear = Double.parseDouble(args[i].substring(args[i].indexOf('=') + 1));
                i += 1;
                continue;
              }
            else if (args[i].startsWith("--rotate="))
              {
                speed.rotate = Double.parseDouble(args[i].substring(args[i].indexOf('=') + 1));
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
            if (s.equals("arc"))
              awtTests |= J2DTEST_ARC;
            else if (s.equals("cubiccurve"))
              awtTests |= J2DTEST_CUBICCURVE;
            else if (s.equals("ellipse"))
              awtTests |= J2DTEST_ELLIPSE;
            else if (s.equals("generalpath"))
              awtTests |= J2DTEST_GENERALPATH;
            else if (s.equals("line"))
              awtTests |= J2DTEST_LINE;
            else if (s.equals("quadcurve"))
              awtTests |= J2DTEST_QUADCURVE;
            else if (s.equals("rectangle"))
              awtTests |= J2DTEST_RECTANGLE;
            else if (s.equals("roundrectangle"))
              awtTests |= J2DTEST_ROUNDRECTANGLE;
            else if (s.equals("image"))
              awtTests |= J2DTEST_IMAGE;
            else
              {
                System.err.println("Unknown AWT test '" + s + "'!");
                System.exit(2);
              }
          }
        i += 1;
      }
    if (awtTests != J2DTEST_NONE)
      speed.awtTests = awtTests;

    // Create graphics.
    speed.init();
    final Frame frame = new Frame("J2dGraphicsBenchmark");

    frame.addWindowListener(new WindowAdapter()
    {
      public void windowClosing(WindowEvent e)
      {
        frame.setVisible(false);
        System.exit(0);
      }
    });

    frame.add(speed, BorderLayout.CENTER);
    frame.setSize(speed.screenWidth, speed.screenHeight);
    frame.setVisible(true);

    // Insets are correctly set only after the native peer was created.
    Insets insets = frame.getInsets();
    // The internal size of the frame should be 320x240.
    frame.setSize(320 + insets.right + insets.left, 240 + insets.top
                                                    + insets.bottom);
  }

  private Image loadImage(String imageName)
  {
    Image result = null;
    logger.logp(Level.INFO, "J2dGraphicsBenchmark", "loadImage",
                "Loading image: " + imageName);
    URL url = getClass().getResource(imageName);
    if (url != null)
      {
        result = Toolkit.getDefaultToolkit().getImage(url);
        prepareImage(result, this);
      }
    else
      {
        logger.logp(Level.WARNING, "J2dGraphicsBenchmark", "loadImage",
                    "Could not locate image resource in class path: "
                        + imageName);
      }
    return result;
  }

  private BufferedImage loadBufferedImage(String imageName)
  {
    BufferedImage result = null;
    logger.logp(Level.INFO, "J2dGraphicsBenchmark", "loadImage",
                "Loading image: " + imageName);

    // Try to load image out of classpath before trying an absolute filename
    URL url = getClass().getResource(imageName);
    Image img;
    if (url != null)
      img = Toolkit.getDefaultToolkit().getImage(url);
    else
      img = Toolkit.getDefaultToolkit().getImage(imageName);

    if (img != null)
      {
        // Wait for image to load
        try
          {
            MediaTracker tracker = new MediaTracker(this);
            tracker.addImage(img, 1);
            tracker.waitForAll();

            prepareImage(img, this);
            result = new BufferedImage(img.getWidth(this), img.getHeight(this),
                                       BufferedImage.TYPE_INT_RGB);
            result.createGraphics().drawImage(img, 0, 0, this);
          }
        catch (InterruptedException e)
          {
          }
        catch (IllegalArgumentException e)
          {
          }
      }

    if (result == null)
      {
        logger.logp(Level.WARNING, "J2dGraphicsBenchmark", "loadBufferedImage",
                    "Could not locate image resource in class path: "
                        + imageName);
      }
    return result;
  }

  /**
   * Executes the test methods.
   * 
   * @param g The Graphics2D object that is used to paint.
   * @param size The size of the canvas.
   */
  void runTestSet(Graphics2D g, Dimension size)
  {
    // Any user-specified options (ie set transforms, rendering hints)
    prepareGraphics(g);

    if ((awtTests & J2DTEST_ARC) != 0)
      {
        test_drawArc(g, size);
        test_fillArc(g, size);
      }

    if ((awtTests & J2DTEST_CUBICCURVE) != 0)
      {
        test_drawCubicCurve(g, size);
      }

    if ((awtTests & J2DTEST_ELLIPSE) != 0)
      {
        test_drawEllipse(g, size);
        test_fillEllipse(g, size);
      }

    if ((awtTests & J2DTEST_GENERALPATH) != 0)
      {
        // Current implementation doesn't work
        test_drawGeneralPath(g, size);
        test_fillGeneralPath(g, size);
      }

    if ((awtTests & J2DTEST_LINE) != 0)
      {
        test_drawLine(g, size);
      }

    if ((awtTests & J2DTEST_QUADCURVE) != 0)
      {
        test_drawQuadCurve(g, size);
      }

    if ((awtTests & J2DTEST_RECTANGLE) != 0)
      {
        test_drawRectangle(g, size);
        test_fillRectangle(g, size);
      }

    if ((awtTests & J2DTEST_ROUNDRECTANGLE) != 0)
      {
        test_drawRoundRectangle(g, size);
        test_fillRoundRectangle(g, size);
      }

    if ((awtTests & J2DTEST_IMAGE) != 0)
      {
        test_drawImage(g, size);
        test_drawTransparentImage(g, size);
      }
  }

  /**
   * Reset all graphics settings to the standard, default values
   * 
   * @param g the object to apply settings to
   */
  private void resetGraphics(Graphics2D g)
  {
    g.setTransform(new AffineTransform());
    g.setStroke(new BasicStroke());
    g.setComposite(AlphaComposite.SrcOut);
  }

  /**
   * Sets initial user graphics options
   * 
   * @param g the object to apply settings to
   */
  private void prepareGraphics(Graphics2D g)
  {
    // Transforms
    if (affineTransform != null)
      g.setTransform(affineTransform);

    else if (xtranslate != 0 || ytranslate != 0 || xshear != 0 || yshear != 0)
      {
        g.translate(xtranslate, ytranslate);
        g.shear(xshear, yshear);
      }

    if (rotate > 0)
      g.rotate(rotate * Math.PI, screenWidth / 2, screenHeight / 2);

    // Composite (transparency)
    if (composite > 0)
      {
        g.setComposite(AlphaComposite.getInstance(AlphaComposite.SRC_OVER,
                                                  composite));
      }

    // Textures
    if (texture != null)
      g.setPaint(new TexturePaint(textureImage,
                                  new Rectangle(0, 0, textureImage.getWidth(),
                                                textureImage.getHeight())));

    // Anti-alias setting
    if (antialiasFlag)
      g.setRenderingHints(new RenderingHints(RenderingHints.KEY_ANTIALIASING,
                                             RenderingHints.VALUE_ANTIALIAS_ON));
    else
      g.setRenderingHints(new RenderingHints(RenderingHints.KEY_ANTIALIASING,
                                             RenderingHints.VALUE_ANTIALIAS_OFF));
  }

  /**
   * Gets new random settings
   * 
   * @param g the object to set parameters for
   * @param size the screen size
   */
  private void setRandom(Graphics2D g, Dimension size)
  {
    // Set colour / paint
    if (gradientFlag)
      {
        Color c1 = new Color((int) (Math.random() * 254) + 1,
                             (int) (Math.random() * 254) + 1,
                             (int) (Math.random() * 254) + 1);

        Color c2 = new Color((int) (Math.random() * 254) + 1,
                             (int) (Math.random() * 254) + 1,
                             (int) (Math.random() * 254) + 1);

        g.setPaint(new GradientPaint(0, 0, c1, screenWidth / 5,
                                     screenHeight / 5, c2, true));
      }

    else if (texture == null)
      g.setPaint(new Color((int) (Math.random() * 254) + 1,
                           (int) (Math.random() * 254) + 1,
                           (int) (Math.random() * 254) + 1));

    // Set stroke width and options
    if (strokeFlag)
      {
        int cap = (int) (Math.random() * 3 + 1);
        if (cap == 1)
          cap = BasicStroke.CAP_SQUARE;
        else if (cap == 2)
          cap = BasicStroke.CAP_BUTT;
        else
          cap = BasicStroke.CAP_ROUND;

        int join = (int) (Math.random() * 3 + 1);
        if (join == 1)
          join = BasicStroke.JOIN_MITER;
        else if (join == 2)
          join = BasicStroke.JOIN_BEVEL;
        else
          join = BasicStroke.JOIN_ROUND;

        float[] dashes = { 10, 10 };
        g.setStroke(new BasicStroke((int) (Math.random() * 10), cap, join, 10f,
                                    dashes, 0));
      }

    // Composite / transparency
    if (composite == - 1)
      {
        g.setComposite(AlphaComposite.getInstance(AlphaComposite.SRC_OVER,
                                                  (float) Math.random()));
      }

    // Transformations
    if (rotate == - 1)
      g.rotate(Math.random() * Math.PI * 2);
  }

  /**
   * Draws random arcs within the given dimensions.
   * 
   * @param g The Graphics2D object that is used to paint.
   * @param size The size of the canvas.
   */
  private void test_drawArc(Graphics2D g, Dimension size)
  {
    int maxTests = testSize;
    int minSize;
    long startTime;
    long endTime;
    minSize = 10;
    startTime = System.currentTimeMillis();
    for (int i = 0; i < maxTests; i += 1)
      {
        setRandom(g, size);
        int x = (int) (Math.random() * (size.width - minSize + 1));
        int y = (int) (Math.random() * (size.height - minSize + 1));
        int width = (int) (Math.random() * (size.width - x - minSize) + minSize);
        int height = (int) (Math.random() * (size.height - y - minSize) + minSize);
        int startAngle = (int) (Math.random() * 360);
        int arcAngle = (int) (Math.random() * 360 - startAngle);

        Arc2D arc = new Arc2D.Double(x, y, width, height, startAngle, arcAngle,
                                     Arc2D.OPEN);
        g.draw(arc);
      }
    endTime = System.currentTimeMillis();
    recordTest("draw(Arc2D.Double) " + maxTests + " times",
               (endTime - startTime));
  }

  /**
   * Draws random filled arcs within the given dimensions.
   * 
   * @param g The Graphics2D object that is used to paint.
   * @param size The size of the canvas.
   */
  private void test_fillArc(Graphics2D g, Dimension size)
  {
    int maxTests = testSize;
    int minSize;
    long startTime;
    long endTime;
    minSize = 10;
    startTime = System.currentTimeMillis();
    for (int i = 0; i < maxTests; i += 1)
      {
        setRandom(g, size);
        int x = (int) (Math.random() * (size.width - minSize + 1));
        int y = (int) (Math.random() * (size.height - minSize + 1));
        int width = (int) (Math.random() * (size.width - x - minSize) + minSize);
        int height = (int) (Math.random() * (size.height - y - minSize) + minSize);
        int startAngle = (int) (Math.random() * 360);
        int arcAngle = (int) (Math.random() * 360);

        Arc2D arc = new Arc2D.Double(x, y, width, height, startAngle, arcAngle,
                                     Arc2D.OPEN);
        g.fill(arc);
      }
    endTime = System.currentTimeMillis();
    recordTest("fill(Arc2D.Double) " + maxTests + " times",
               (endTime - startTime));
  }

  /**
   * Draws random cubic curves within the given dimensions.
   * 
   * @param g The Graphics2D object that is used to paint.
   * @param size The size of the canvas.
   */
  private void test_drawCubicCurve(Graphics2D g, Dimension size)
  {
    int maxTests = testSize;
    int minSize = 10;
    long startTime = System.currentTimeMillis();
    for (int i = 0; i < maxTests; i += 1)
      {
        setRandom(g, size);
        int x1 = (int) (Math.random() * (size.width - minSize));
        int y1 = (int) (Math.random() * (size.height - minSize));
        int xc1 = (int) (Math.random() * (size.width - minSize));
        int yc1 = (int) (Math.random() * (size.height - minSize));
        int xc2 = (int) (Math.random() * (size.width - minSize));
        int yc2 = (int) (Math.random() * (size.height - minSize));
        int x2 = (int) (Math.random() * (size.width - minSize));
        int y2 = (int) (Math.random() * (size.height - minSize));

        CubicCurve2D curve = new CubicCurve2D.Double(x1, y1, xc1, yc1, xc2,
                                                     yc2, x2, y2);
        g.draw(curve);
      }
    long endTime = System.currentTimeMillis();
    recordTest("draw(CubicCurve2D.Double) " + maxTests + " times",
               (endTime - startTime));
  }

  /**
   * Draws random ellipses within the given dimensions.
   * 
   * @param g The Graphics2D object that is used to paint.
   * @param size The size of the canvas.
   */
  private void test_drawEllipse(Graphics2D g, Dimension size)
  {
    int maxTests = testSize;
    int minSize = 10;
    long startTime = System.currentTimeMillis();
    for (int i = 0; i < maxTests; i += 1)
      {
        setRandom(g, size);
        int x1 = (int) (Math.random() * (size.width - minSize));
        int y1 = (int) (Math.random() * (size.height - minSize));
        int x2 = (int) (Math.random() * (size.width - minSize));
        int y2 = (int) (Math.random() * (size.height - minSize));
        Ellipse2D ellipse = new Ellipse2D.Double(x1, y1, x2, y2);
        g.draw(ellipse);
      }
    long endTime = System.currentTimeMillis();
    recordTest("draw(Ellipse.Double) " + maxTests + " times",
               (endTime - startTime));
  }

  /**
   * Draws random ellipses within the given dimensions.
   * 
   * @param g The Graphics2D object that is used to paint.
   * @param size The size of the canvas.
   */
  private void test_fillEllipse(Graphics2D g, Dimension size)
  {
    int maxTests = testSize;
    int minSize = 10;
    long startTime = System.currentTimeMillis();
    for (int i = 0; i < maxTests; i += 1)
      {
        setRandom(g, size);
        int x1 = (int) (Math.random() * (size.width - minSize));
        int y1 = (int) (Math.random() * (size.height - minSize));
        int x2 = (int) (Math.random() * (size.width - minSize));
        int y2 = (int) (Math.random() * (size.height - minSize));
        Ellipse2D ellipse = new Ellipse2D.Double(x1, y1, x2, y2);
        g.fill(ellipse);
      }
    long endTime = System.currentTimeMillis();
    recordTest("fill(Ellipse.Double) " + maxTests + " times",
               (endTime - startTime));
  }

  // TODO: fix the GeneralPath methods.
  /**
   * Draws random polygons within the given dimensions.
   * 
   * @param g The Graphics2D object that is used to paint.
   * @param size The size of the canvas.
   */
  private void test_drawGeneralPath(Graphics2D g, Dimension size)
  {
    int maxTests = testSize;
    long startTime = System.currentTimeMillis();

    for (int i = 0; i < maxTests; i += 1)
      {
        setRandom(g, size);
        int points = (int) (Math.random() * 6) + 2;
        GeneralPath shape = new GeneralPath();
        shape.moveTo((float) Math.random() * (size.width),
                     (float) Math.random() * (size.height));
        for (int j = 0; j < points; j += 1)
          {
            shape.lineTo((float) (Math.random() * (size.width)),
                         (float) (Math.random() * (size.height)));
          }
        g.draw(shape);
      }
    long endTime = System.currentTimeMillis();
    recordTest("draw(GeneralPath) " + maxTests + " times",
               (endTime - startTime));
  }

  /**
   * Draws random filled polygons within the given dimensions.
   * 
   * @param g The Graphics2D object that is used to paint.
   * @param size The size of the canvas.
   */
  private void test_fillGeneralPath(Graphics2D g, Dimension size)
  {
    int maxTests = testSize;
    long startTime = System.currentTimeMillis();

    GeneralPath shape = new GeneralPath();
    shape.moveTo((float) Math.random() * (size.width), (float) Math.random()
                                                       * (size.height));

    for (int i = 0; i < maxTests; i += 1)
      {
        setRandom(g, size);
        int points = (int) (Math.random() * 6) + 2;
        for (int j = 0; j < points; j += 1)
          {
            shape.lineTo((float) (Math.random() * (size.width)),
                         (float) (Math.random() * (size.height)));
          }
        g.fill(shape);
      }
    long endTime = System.currentTimeMillis();
    recordTest("fill(GeneralPath) " + maxTests + " times",
               (endTime - startTime));
  }

  /**
   * Draws random lines within the given dimensions.
   * 
   * @param g The Graphics2D object that is used to paint.
   * @param size The size of the canvas.
   */
  private void test_drawLine(Graphics2D g, Dimension size)
  {
    int maxTests = testSize;
    int minSize = 10;
    long startTime = System.currentTimeMillis();
    for (int i = 0; i < maxTests; i += 1)
      {
        setRandom(g, size);
        int x1 = (int) (Math.random() * (size.width - minSize));
        int y1 = (int) (Math.random() * (size.height - minSize));
        int x2 = (int) (Math.random() * (size.width - minSize));
        int y2 = (int) (Math.random() * (size.height - minSize));
        Line2D line = new Line2D.Double(x1, y1, x2, y2);
        g.draw(line);
      }
    long endTime = System.currentTimeMillis();
    recordTest("draw(Line2D.Double) " + maxTests + " times",
               (endTime - startTime));
  }

  /**
   * Draws random quadratic curves within the given dimensions.
   * 
   * @param g The Graphics2D object that is used to paint.
   * @param size The size of the canvas.
   */
  private void test_drawQuadCurve(Graphics2D g, Dimension size)
  {
    int maxTests = testSize;
    int minSize = 10;
    long startTime = System.currentTimeMillis();
    for (int i = 0; i < maxTests; i += 1)
      {
        setRandom(g, size);
        int x1 = (int) (Math.random() * (size.width - minSize));
        int y1 = (int) (Math.random() * (size.height - minSize));
        int xc = (int) (Math.random() * (size.width - minSize));
        int yc = (int) (Math.random() * (size.height - minSize));
        int x2 = (int) (Math.random() * (size.width - minSize));
        int y2 = (int) (Math.random() * (size.height - minSize));

        QuadCurve2D curve = new QuadCurve2D.Double(x1, y1, xc, yc, x2, y2);
        g.draw(curve);
      }
    long endTime = System.currentTimeMillis();
    recordTest("draw(QuadCurve2D.Double) " + maxTests + " times",
               (endTime - startTime));
  }

  /**
   * Draws random rectangles within the given dimensions.
   * 
   * @param g The Graphics2D object that is used to paint.
   * @param size The size of the canvas.
   */
  private void test_drawRectangle(Graphics2D g, Dimension size)
  {
    int maxTests = testSize;
    int minSize = 10;
    long startTime = System.currentTimeMillis();
    for (int i = 0; i < maxTests; i += 1)
      {
        setRandom(g, size);
        int x1 = (int) (Math.random() * (size.width - minSize));
        int y1 = (int) (Math.random() * (size.height - minSize));
        int x2 = (int) (Math.random() * (size.width - minSize));
        int y2 = (int) (Math.random() * (size.height - minSize));
        Rectangle2D rect = new Rectangle2D.Double(x1, y1, x2, y2);
        g.draw(rect);
      }
    long endTime = System.currentTimeMillis();
    recordTest("draw(Rectangle.Double) " + maxTests + " times",
               (endTime - startTime));
  }

  /**
   * Draws random rectangles within the given dimensions.
   * 
   * @param g The Graphics2D object that is used to paint.
   * @param size The size of the canvas.
   */
  private void test_fillRectangle(Graphics2D g, Dimension size)
  {
    int maxTests = testSize;
    int minSize = 10;
    long startTime = System.currentTimeMillis();
    for (int i = 0; i < maxTests; i += 1)
      {
        setRandom(g, size);
        int x1 = (int) (Math.random() * (size.width - minSize));
        int y1 = (int) (Math.random() * (size.height - minSize));
        int x2 = (int) (Math.random() * (size.width - minSize));
        int y2 = (int) (Math.random() * (size.height - minSize));
        Rectangle2D rect = new Rectangle2D.Double(x1, y1, x2, y2);
        g.fill(rect);
      }
    long endTime = System.currentTimeMillis();
    recordTest("fill(Rectangle.Double) " + maxTests + " times",
               (endTime - startTime));
  }

  /**
   * Draws random rounded rectangles within the given dimensions.
   * 
   * @param g The Graphics2D object that is used to paint.
   * @param size The size of the canvas.
   */
  private void test_drawRoundRectangle(Graphics2D g, Dimension size)
  {
    int maxTests = testSize;
    int minSize;
    long startTime;
    long endTime;
    minSize = 10;
    startTime = System.currentTimeMillis();
    for (int i = 0; i < maxTests; i += 1)
      {
        setRandom(g, size);
        int x = (int) (Math.random() * (size.width - minSize + 1));
        int y = (int) (Math.random() * (size.height - minSize + 1));
        int width = (int) (Math.random() * (size.width - x - minSize) + minSize);
        int height = (int) (Math.random() * (size.height - y - minSize) + minSize);
        int arcWidth = (int) (Math.random() * (width - 1) + 1);
        int arcHeight = (int) (Math.random() * (height - 1) + 5);
        RoundRectangle2D rect = new RoundRectangle2D.Double(x, y, width,
                                                            height, arcWidth,
                                                            arcHeight);
        g.draw(rect);
      }
    endTime = System.currentTimeMillis();
    recordTest("draw(RoundRectangle.Double) " + maxTests + " times",
               (endTime - startTime));
  }

  /**
   * Draws random filled rounded rectangles within the given dimensions.
   * 
   * @param g The Graphics2D object that is used to paint.
   * @param size The size of the canvas.
   */
  private void test_fillRoundRectangle(Graphics2D g, Dimension size)
  {
    int maxTests = testSize;
    int minSize;
    long startTime;
    long endTime;
    minSize = 10;
    startTime = System.currentTimeMillis();
    for (int i = 0; i < maxTests; i += 1)
      {
        setRandom(g, size);
        int x = (int) (Math.random() * (size.width - minSize + 1));
        int y = (int) (Math.random() * (size.height - minSize + 1));
        int width = (int) (Math.random() * (size.width - x - minSize) + minSize);
        int height = (int) (Math.random() * (size.height - y - minSize) + minSize);
        int arcWidth = (int) (Math.random() * (width - 1) + 1);
        int arcHeight = (int) (Math.random() * (height - 1) + 5);
        RoundRectangle2D rect = new RoundRectangle2D.Double(x, y, width,
                                                            height, arcWidth,
                                                            arcHeight);
        g.fill(rect);
      }
    endTime = System.currentTimeMillis();
    recordTest("fill(RoundRectangle.Double) " + maxTests + " times",
               (endTime - startTime));
  }

  /**
   * Draws random images within the given dimensions.
   * 
   * @param g The Graphics2D object that is used to paint.
   * @param size The size of the canvas.
   */
  private void test_drawImage(Graphics2D g, Dimension size)
  {
    if (gifTestImage == null)
      {
        logger.logp(Level.WARNING, "J2dGraphicsBenchmark", "runTestSet",
                    "Skipping 'test_drawImage' due to missing resource.");
        return;
      }

    int maxTests = testSize / 2;
    if (maxTests == 0)
      maxTests = 1;
    int imageWidth = gifTestImage.getWidth(this);
    int imageHeight = gifTestImage.getHeight(this);
    long startTime = System.currentTimeMillis();
    for (int i = 0; i < maxTests; i += 1)
      {
        setRandom(g, size);
        int x = (int) (Math.random() * (size.width - imageWidth + 1));
        int y = (int) (Math.random() * (size.height - imageHeight + 1));
        g.drawImage(gifTestImage, x, y, this);
      }
    long endTime = System.currentTimeMillis();
    recordTest("drawImage " + maxTests + " times", (endTime - startTime));
  }

  /**
   * Draws random transparent images within the given dimensions.
   * 
   * @param g The Graphics object that is used to paint.
   * @param size The size of the canvas.
   */
  private void test_drawTransparentImage(Graphics2D g, Dimension size)
  {
    if (pngTestImage == null)
      {
        logger.logp(Level.WARNING, "AicasGraphicsBenchmark", "runTestSet",
                    "Skipping 'drawTransparentImage' due to missing resource.");
        return;
      }

    int maxTests = testSize / 5;
    if (maxTests == 0)
      maxTests = 1;
    int imageWidth = pngTestImage.getWidth(this);
    int imageHeight = pngTestImage.getHeight(this);
    long startTime = System.currentTimeMillis();
    for (int i = 0; i < maxTests; i += 1)
      {
        setRandom(g, size);
        int x = (int) (Math.random() * (size.width - imageWidth + 1));
        int y = (int) (Math.random() * (size.height - imageHeight + 1));
        g.drawImage(pngTestImage, x, y, this);
      }
    long endTime = System.currentTimeMillis();
    recordTest("draw transparent image " + maxTests + " times",
               (endTime - startTime));
  }

  private class GraphicsTest
      extends Canvas
      implements Runnable
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
      while (! done)
        {
          runCount++;

          try
            {
              synchronized (this)
                {
                  while (! doPaint)
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

              // if (iterations != 0)
              // System.out.println("--- run...("
              // + runCount
              // + "/"
              // + iterations
              // + ") ------------------------------------------------------");

              Graphics g = getGraphics();
              Dimension size = getSize();

              if (singleBufferFlag)
                {
                  logger.logp(Level.INFO, "J2dGraphicsBenchmark.GraphicsTest",
                              "run",
                              "Start testing non-double-buffered drawing");

                  if (noClippingFlag)
                    runSet_noClipping((Graphics2D) g, size, runCount);

                  if (withClippingFlag)
                    runSet_withClipping((Graphics2D) g, size, runCount);

                  if (zeroClippingFlag)
                    runSet_zeroClipping((Graphics2D) g, size, runCount);

                  g.dispose();
                }

              if (doubleBufferFlag)
                {
                  logger.logp(Level.INFO, "J2dGraphicsBenchmark.GraphicsTest",
                              "run", "Start testing double-buffered drawing");
                  Graphics canvas = getGraphics();
                  Image doublebuffer = createImage(size.width, size.height);

                  if (noClippingFlag)
                    {
                      g = doublebuffer.getGraphics();
                      runSet_noClipping((Graphics2D) g, size,
                                        "double buffering", runCount);
                      g.dispose();
                      canvas.drawImage(doublebuffer, 0, 0, this);
                    }

                  if (withClippingFlag)
                    {
                      g = doublebuffer.getGraphics();
                      runSet_withClipping((Graphics2D) g, size,
                                          "double buffering", runCount);
                      g.dispose();
                      canvas.drawImage(doublebuffer, 0, 0, this);
                    }

                  if (zeroClippingFlag)
                    {
                      g = doublebuffer.getGraphics();
                      runSet_zeroClipping((Graphics2D) g, size,
                                          "double buffering", runCount);
                      g.dispose();
                      canvas.drawImage(doublebuffer, 0, 0, this);
                      canvas.dispose();
                    }
                }

              printReport();

              if (iterations != 1)
                {
                  if (iterations != - 1)
                    iterations--;
                }
              else
                {
                  // System.out.println("--- done
                  // --------------------------------------------------------");
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
      testComplete();
    }

    private void runSet_zeroClipping(Graphics2D g, Dimension size, int runCount)
    {
      runSet_zeroClipping(g, size, "", runCount);
    }

    private void runSet_zeroClipping(Graphics2D g, Dimension size,
                                     String context, int runCount)
    {
      int clipped_width;
      int clipped_height;
      int clipped_x;
      int clipped_y;

      clipped_width = 0;
      clipped_height = 0;
      clipped_x = (size.width) / 2;
      clipped_y = (size.height) / 2;

      // Reset any transforms from past tests
      resetGraphics(g);

      Rectangle fullWindow = new Rectangle(0, 0, size.width, size.height);
      g.setClip(fullWindow);
      g.setPaint(Color.BLACK);
      g.fill(fullWindow);

      Rectangle windowBorder = new Rectangle(0, 0, size.width - 1,
                                             size.width - 1);
      g.setPaint(Color.WHITE);
      g.draw(windowBorder);

      Rectangle innerBorder = new Rectangle(clipped_x - 1, clipped_y - 1,
                                            clipped_width + 2,
                                            clipped_height + 2);
      g.fill(innerBorder);

      Rectangle innerBox = new Rectangle(clipped_x, clipped_y, clipped_width,
                                         clipped_height);
      g.clip(innerBox);
      g.setPaint(Color.BLACK);
      g.fill(fullWindow);

      if (context.equals(""))
        setTestContext("(" + runCount + ") clipping to zero");
      else
        setTestContext("(" + runCount + ") clipping to zero (" + context + ")");

      runTestSet(g, size);
    }

    private void runSet_withClipping(Graphics2D g, Dimension size, int runCount)
    {
      runSet_withClipping(g, size, "", runCount);
    }

    private void runSet_withClipping(Graphics2D g, Dimension size,
                                     String context, int runCount)
    {
      int clipped_width = 2 * size.width / 3;
      int clipped_height = 2 * size.height / 3;
      int clipped_x = (size.width - clipped_width) / 2;
      int clipped_y = (size.height - clipped_height) / 2;

      // Reset any transforms from past tests
      resetGraphics(g);

      Rectangle fullWindow = new Rectangle(0, 0, size.width, size.height);
      g.setClip(fullWindow);

      g.setPaint(Color.BLACK);
      g.fill(fullWindow);

      Rectangle windowBorder = new Rectangle(0, 0, size.width - 1,
                                             size.height - 1);
      g.setPaint(Color.GREEN);
      g.draw(windowBorder);

      Rectangle innerBorder = new Rectangle(clipped_x - 1, clipped_y - 1,
                                            clipped_width + 2,
                                            clipped_height + 2);
      g.setPaint(Color.WHITE);
      g.fill(innerBorder);

      Rectangle innerBox = new Rectangle(clipped_x, clipped_y, clipped_width,
                                         clipped_height);
      g.clip(innerBox);

      g.setPaint(Color.BLACK);
      g.fill(fullWindow);

      if (context.equals(""))
        setTestContext("(" + runCount + ") with clipping ");
      else
        setTestContext("(" + runCount + ") with clipping (" + context + ")");

      runTestSet(g, size);
    }

    private void runSet_noClipping(Graphics2D g, Dimension size, int runCount)
    {
      runSet_noClipping(g, size, "", runCount);
    }

    private void runSet_noClipping(Graphics2D g, Dimension size,
                                   String context, int runCount)
    {
      // Reset any transforms from past tests
      resetGraphics(g);

      Rectangle fullWindow = new Rectangle(0, 0, size.width, size.height);
      g.setPaint(Color.BLACK);
      g.fill(fullWindow);

      if (context.equals(""))
        setTestContext("(" + runCount + ") without clipping");
      else
        setTestContext("(" + runCount + ") without clipping (" + context + ")");

      runTestSet(g, size);
    }

    public void paint(Graphics g)
    {
      synchronized (this)
        {
          doPaint = true;
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
    testsMap.put(testName, recoder);
  }

  public TestRecorder getTest(String testName)
  {
    return (TestRecorder) testsMap.get(testName);
  }

  public Iterator testIterator()
  {
    return testsMap.keySet().iterator();
  }
}

class TestRecorder
{
  String test;

  long totalTime = 0;

  long minTime = Long.MAX_VALUE;

  long maxTime = Long.MIN_VALUE;

  int runCount = 0;

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

  public final long getAverage()
  {
    return (totalTime / runCount);
  }

  public TestRecorder(String testName)
  {
    test = testName;
  }

  public void addRun(long time)
  {
    totalTime += time;
    if (minTime > time)
      minTime = time;
    if (maxTime < time)
      maxTime = time;
    runCount += 1;
  }
}
