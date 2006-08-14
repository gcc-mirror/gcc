/* J2dBenchmarkGUI.java -- GUI for java2d benchmarker
 Copyright (C) 2006  Free Software Foundation, Inc.

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

package gnu.classpath.examples.java2d;

import java.awt.BorderLayout;
import java.awt.Color;
import java.awt.Container;
import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.awt.GridLayout;
import java.awt.Insets;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.WindowAdapter;
import java.awt.event.WindowEvent;
import java.util.ArrayList;
import java.util.Hashtable;
import java.util.Iterator;
import java.util.logging.Handler;
import java.util.logging.Level;
import java.util.logging.LogRecord;

import javax.swing.Box;
import javax.swing.BoxLayout;
import javax.swing.ButtonGroup;
import javax.swing.JButton;
import javax.swing.JCheckBox;
import javax.swing.JComboBox;
import javax.swing.JComponent;
import javax.swing.JFrame;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JRadioButton;
import javax.swing.JScrollPane;
import javax.swing.JTextArea;
import javax.swing.JTextField;
import javax.swing.border.BevelBorder;

/**
 * Extends the J2dBenchmark to provide a GUI for selecting options and tests.
 */
public class J2dBenchmarkGUI
    implements ActionListener
{

  JLabel errorLabel;

  JCheckBox noClipping;

  JCheckBox withClipping;

  JCheckBox zeroClipping;

  JCheckBox singleBuffer;

  JCheckBox doubleBuffer;

  public J2dBenchmarkGUI()
  {
    super();
  }

  public static void main(String[] args)
  {
    new J2dBenchmarkGUI().run();
  }

  /**
   * Sets up the initial GUI
   */
  public void run()
  {
    // Store all elements in a hashtable so that they can be passed into the
    // harness easily.
    Hashtable elements = new Hashtable();

    // Set up frame
    final JFrame frame = new JFrame("Java2D benchmark");
    errorLabel = new JLabel(" ");

    JPanel panel = new JPanel();
    panel.setLayout(new BoxLayout(panel, BoxLayout.PAGE_AXIS));
    Container content = frame.getContentPane();

    // Display options for dimensions, iterations, test size, etc
    JPanel options = new JPanel(new GridLayout(0, 2));

    options.add(new JLabel("Height: "));
    JTextField heightField = new JTextField(Integer.toString(J2dBenchmark.DEFAULT_SCREEN_HEIGHT));
    heightField.setColumns(5);
    options.add(heightField);
    elements.put("height", heightField);

    options.add(new JLabel("Width: "));
    JTextField widthField = new JTextField(Integer.toString(J2dBenchmark.DEFAULT_SCREEN_WIDTH));
    widthField.setColumns(5);
    options.add(widthField);
    elements.put("width", widthField);

    options.add(new JLabel("Iterations: "));
    JTextField iterField = new JTextField("1");
    iterField.setColumns(5);
    options.add(iterField);
    elements.put("iterations", iterField);

    options.add(new JLabel("Test size: "));
    JTextField testSizeField = new JTextField(Integer.toString(J2dBenchmark.DEFAULT_TEST_SIZE));
    testSizeField.setColumns(5);
    options.add(testSizeField);
    elements.put("size", testSizeField);

    options.add(new JLabel("Test without clipping: "));
    noClipping = new JCheckBox("", true);
    noClipping.addActionListener(this);
    options.add(noClipping);
    elements.put("noclip", noClipping);

    options.add(new JLabel("Test with clipping: "));
    withClipping = new JCheckBox("", true);
    withClipping.addActionListener(this);
    options.add(withClipping);
    elements.put("withclip", withClipping);

    options.add(new JLabel("Test with clipping to zero: "));
    zeroClipping = new JCheckBox("", true);
    zeroClipping.addActionListener(this);
    options.add(zeroClipping);
    elements.put("zeroclip", zeroClipping);

    options.add(new JLabel("Run single-buffer test: "));
    singleBuffer = new JCheckBox("", true);
    singleBuffer.addActionListener(this);
    options.add(singleBuffer);
    elements.put("singlebuffer", singleBuffer);

    options.add(new JLabel("Run double-buffer test: "));
    doubleBuffer = new JCheckBox("", true);
    doubleBuffer.addActionListener(this);
    options.add(doubleBuffer);
    elements.put("doublebuffer", doubleBuffer);

    // Allow user to select tests to run
    JPanel tests = new JPanel();
    tests.setLayout(new BoxLayout(tests, BoxLayout.PAGE_AXIS));
    tests.setBorder(new BevelBorder(BevelBorder.RAISED));
    tests.add(new JLabel("Shapes to test:"));

    JCheckBox test_arcDraw = new JCheckBox("Arc", true);
    tests.add(test_arcDraw);
    elements.put("test_arcDraw", test_arcDraw);

    JCheckBox test_ccurveDraw = new JCheckBox("Cubic Curve", true);
    tests.add(test_ccurveDraw);
    elements.put("test_ccurveDraw", test_ccurveDraw);

    JCheckBox test_ellipseDraw = new JCheckBox("Ellipse", true);
    tests.add(test_ellipseDraw);
    elements.put("test_ellipseDraw", test_ellipseDraw);

    /*
     JCheckBox test_pathDraw = new JCheckBox("General Path", true);
     tests.add(test_pathDraw);
     elements.put("test_pathDraw", test_pathDraw);
     */

    JCheckBox test_lineDraw = new JCheckBox("Line", true);
    tests.add(test_lineDraw);
    elements.put("test_lineDraw", test_lineDraw);

    JCheckBox test_qcurveDraw = new JCheckBox("Quadratic Curve", true);
    tests.add(test_qcurveDraw);
    elements.put("test_qcurveDraw", test_qcurveDraw);

    JCheckBox test_rectDraw = new JCheckBox("Rectangle", true);
    tests.add(test_rectDraw);
    elements.put("test_rectDraw", test_rectDraw);

    JCheckBox test_rrectDraw = new JCheckBox("Round Rectangle", true);
    tests.add(test_rrectDraw);
    elements.put("test_rrectDraw", test_rrectDraw);

    JCheckBox test_image = new JCheckBox("Images", true);
    tests.add(test_image);
    elements.put("test_image", test_image);

    // Additional image-processing options
    JPanel extras = new JPanel();
    extras.setBorder(new BevelBorder(BevelBorder.LOWERED));
    GridBagLayout layout = new GridBagLayout();
    GridBagConstraints gbc = new GridBagConstraints();
    gbc.anchor = GridBagConstraints.NORTHWEST;
    gbc.insets = new Insets(5, 2, 15, 15);
    extras.setLayout(layout);

    // Filling (solid, gradient, or texture)
    JPanel opt_Fill = new JPanel();
    opt_Fill.setLayout(new BoxLayout(opt_Fill, BoxLayout.PAGE_AXIS));
    JLabel opt_FillLabel = new JLabel("Filling:");
    opt_FillLabel.setBorder(new BevelBorder(BevelBorder.RAISED));
    opt_Fill.add(opt_FillLabel);

    ButtonGroup opt_FillGroup = new ButtonGroup();
    JRadioButton opt_FillSolid = new JRadioButton("Solid colour", true);
    opt_FillSolid.setActionCommand("solid");
    opt_Fill.add(opt_FillSolid);
    opt_FillGroup.add(opt_FillSolid);
    JRadioButton opt_FillGradient = new JRadioButton("Gradient", false);
    opt_FillGradient.setActionCommand("gradient");
    opt_Fill.add(opt_FillGradient);
    opt_FillGroup.add(opt_FillGradient);
    JRadioButton opt_FillTexture = new JRadioButton("Texture", false);
    opt_FillTexture.setActionCommand("texture");
    opt_Fill.add(opt_FillTexture);
    opt_FillGroup.add(opt_FillTexture);
    JTextField opt_FillTextureFile = new JTextField("texture file");
    opt_FillTextureFile.setAlignmentX(JComponent.LEFT_ALIGNMENT);
    opt_Fill.add(opt_FillTextureFile);
    elements.put("opt_FillGroup", opt_FillGroup);
    elements.put("opt_FillTextureFile", opt_FillTextureFile);
    layout.setConstraints(opt_Fill, gbc);
    extras.add(opt_Fill);

    // Stroke
    JPanel opt_Stroke = new JPanel();
    opt_Stroke.setLayout(new BoxLayout(opt_Stroke, BoxLayout.PAGE_AXIS));
    JLabel opt_StrokeLabel = new JLabel("Stroke:");
    opt_StrokeLabel.setBorder(new BevelBorder(BevelBorder.RAISED));
    opt_Stroke.add(opt_StrokeLabel);
    JCheckBox opt_StrokeRandom = new JCheckBox("random", false);
    elements.put("opt_StrokeRandom", opt_StrokeRandom);
    opt_Stroke.add(opt_StrokeRandom);
    gbc.gridwidth = GridBagConstraints.REMAINDER;
    layout.setConstraints(opt_Stroke, gbc);
    extras.add(opt_Stroke);

    // Anti-Alias
    JPanel opt_Alias = new JPanel();
    opt_Alias.setLayout(new BoxLayout(opt_Alias, BoxLayout.PAGE_AXIS));
    JLabel opt_AliasLabel = new JLabel("Anti-Aliasing:");
    opt_AliasLabel.setBorder(new BevelBorder(BevelBorder.RAISED));
    opt_Alias.add(opt_AliasLabel);
    JCheckBox opt_AliasOn = new JCheckBox("on", false);
    elements.put("opt_AliasOn", opt_AliasOn);
    opt_Alias.add(opt_AliasOn);
    gbc.gridwidth = 1;
    layout.setConstraints(opt_Alias, gbc);
    extras.add(opt_Alias);

    // Alpha composite
    JPanel opt_Composite = new JPanel();
    opt_Composite.setLayout(new BoxLayout(opt_Composite, BoxLayout.PAGE_AXIS));
    JLabel opt_CompositeLabel = new JLabel("Alpha Composite:");
    opt_CompositeLabel.setBorder(new BevelBorder(BevelBorder.RAISED));
    opt_Composite.add(opt_CompositeLabel);
    JTextField opt_CompositeValue = new JTextField("1.0");
    opt_CompositeValue.setAlignmentX(JComponent.LEFT_ALIGNMENT);
    elements.put("opt_CompositeValue", opt_CompositeValue);
    opt_Composite.add(opt_CompositeValue);
    gbc.gridwidth = GridBagConstraints.REMAINDER;
    layout.setConstraints(opt_Composite, gbc);
    extras.add(opt_Composite);

    // Transformations
    // TODO: allow user-defined matrices for AffineTransform
    // (backend already has hooks for it, need to create gui)
    JLabel opt_TransformLabel = new JLabel("Transformations:");
    opt_TransformLabel.setBorder(new BevelBorder(BevelBorder.RAISED));
    gbc.insets = new Insets(5, 2, 0, 15);
    layout.setConstraints(opt_TransformLabel, gbc);
    extras.add(opt_TransformLabel);

    JPanel opt_Transform_Translate = new JPanel(new GridLayout(0, 2, 5, 5));
    opt_Transform_Translate.add(new JLabel("x-axis translation "));
    JTextField opt_TransformTranslateX = new JTextField("0");
    opt_TransformTranslateX.setAlignmentX(JComponent.LEFT_ALIGNMENT);
    opt_Transform_Translate.add(opt_TransformTranslateX);
    elements.put("opt_TransformTranslateX", opt_TransformTranslateX);
    opt_Transform_Translate.add(new JLabel("y-axis translation "));
    JTextField opt_TransformTranslateY = new JTextField("0");
    opt_TransformTranslateY.setAlignmentX(JComponent.LEFT_ALIGNMENT);
    opt_Transform_Translate.add(opt_TransformTranslateY);
    elements.put("opt_TransformTranslateY", opt_TransformTranslateY);
    gbc.gridwidth = 1;
    gbc.insets = new Insets(0, 2, 5, 15);
    layout.setConstraints(opt_Transform_Translate, gbc);
    extras.add(opt_Transform_Translate);

    JPanel opt_Transform_Shear = new JPanel(new GridLayout(0, 2, 5, 5));
    opt_Transform_Shear.add(new JLabel("x-axis shear "));
    JTextField opt_TransformShearX = new JTextField("0");
    opt_TransformShearX.setAlignmentX(JComponent.LEFT_ALIGNMENT);
    opt_Transform_Shear.add(opt_TransformShearX);
    elements.put("opt_TransformShearX", opt_TransformShearX);
    opt_Transform_Shear.add(new JLabel("y-axis shear "));
    JTextField opt_TransformShearY = new JTextField("0");
    opt_Transform_Shear.add(opt_TransformShearY);
    elements.put("opt_TransformShearY", opt_TransformShearY);
    gbc.gridwidth = GridBagConstraints.REMAINDER;
    layout.setConstraints(opt_Transform_Shear, gbc);
    extras.add(opt_Transform_Shear);

    JPanel opt_Transform_Rotate = new JPanel(new GridLayout(0, 2, 5, 5));
    opt_Transform_Rotate.add(new JLabel("rotation (radians) "));
    JTextField opt_TransformRotate = new JTextField("0");
    opt_Transform_Rotate.add(opt_TransformRotate);
    elements.put("opt_TransformRotate", opt_TransformRotate);
    layout.setConstraints(opt_Transform_Rotate, gbc);
    extras.add(opt_Transform_Rotate);

    // Final submit button
    JPanel submit = new JPanel();
    submit.setLayout(new BoxLayout(submit, BoxLayout.PAGE_AXIS));

    JButton rectButton = new JButton("Run benchmark");
    rectButton.setAlignmentX(JComponent.CENTER_ALIGNMENT);
    submit.add(rectButton, BorderLayout.CENTER);

    errorLabel.setAlignmentX(JComponent.CENTER_ALIGNMENT);
    errorLabel.setForeground(Color.RED);
    submit.add(errorLabel);

    rectButton.addActionListener(new Harness(elements, errorLabel));

    // Lay it all out
    JPanel body = new JPanel();
    body.setLayout(new BoxLayout(body, BoxLayout.LINE_AXIS));
    options.setAlignmentX(JComponent.LEFT_ALIGNMENT);
    body.add(options);
    body.add(Box.createHorizontalStrut(50));
    tests.setAlignmentX(JComponent.RIGHT_ALIGNMENT);
    body.add(tests);

    body.setAlignmentX(JComponent.CENTER_ALIGNMENT);
    panel.add(body);
    extras.setAlignmentX(JComponent.CENTER_ALIGNMENT);
    panel.add(extras);
    submit.setAlignmentX(JComponent.CENTER_ALIGNMENT);
    panel.add(submit);

    content.add(panel, BorderLayout.CENTER);

    // Leave some breathing space in the frame
    frame.pack();

    frame.addWindowListener(new WindowAdapter()
    {
      public void windowClosing(WindowEvent e)
      {
        frame.setVisible(false);
        System.exit(0);
      }
    });

    frame.show();
  }

  /**
   * Handles user events on the options GUI, ensuring that user input is valid
   */
  public void actionPerformed(ActionEvent ev)
  {
    if (! noClipping.isSelected() && ! withClipping.isSelected()
        && ! zeroClipping.isSelected())
      errorLabel.setText("You must select at least one clipping option");

    else if (! singleBuffer.isSelected() && ! doubleBuffer.isSelected())
      errorLabel.setText("You must select at least one buffering option");

    else
      errorLabel.setText(" ");
  }

  /**
   * Parses GUI input and sets options in the benchmarker 
   */
  private class Harness
      implements ActionListener
  {
    Hashtable elements;

    JLabel errorLabel;

    /**
     * Creates a new Harness object
     * 
     * @param elements Hashtable containing the swing elements from the GUI
     * @param errorLabel JLabel on which to display any error messages
     */
    public Harness(Hashtable elements, JLabel errorLabel)
    {
      super();

      this.elements = elements;
      this.errorLabel = errorLabel;
    }

    /**
     * Handles user button-clicks, parsing the form, setting options, and
     * starting the J2dBenchmark
     * 
     * @param ae event that triggered this action
     */
    public void actionPerformed(ActionEvent ae)
    {
      try
        {
          // Create benchmarker object
          final JFrame frame = new JFrame("Java2D benchmark");
          J2dBenchmarkWrapper speed = new J2dBenchmarkWrapper(frame);

          // Set options
          speed.setDimensions(Integer.parseInt(((JTextField) elements.get("width")).getText()),
                              Integer.parseInt(((JTextField) elements.get("height")).getText()));

          speed.setIterations(Integer.parseInt(((JTextField) elements.get("iterations")).getText()));
          speed.setTestSize(Integer.parseInt(((JTextField) elements.get("size")).getText()));

          speed.setClipping(((JCheckBox) elements.get("noclip")).isSelected(),
                            ((JCheckBox) elements.get("withclip")).isSelected(),
                            ((JCheckBox) elements.get("zeroclip")).isSelected());

          speed.setBuffers(((JCheckBox) elements.get("singlebuffer")).isSelected(),
                           ((JCheckBox) elements.get("doublebuffer")).isSelected());

          // Set additional processing options
          speed.setFill(((ButtonGroup) elements.get("opt_FillGroup")).getSelection().getActionCommand(),
                        ((JTextField) elements.get("opt_FillTextureFile")).getText());

          speed.setStroke(((JCheckBox) elements.get("opt_StrokeRandom")).isSelected());

          speed.setAlias(((JCheckBox) elements.get("opt_AliasOn")).isSelected());

          speed.setComposite(Float.parseFloat(((JTextField) elements.get("opt_CompositeValue")).getText()));

          speed.setTranslation(Integer.parseInt(((JTextField) elements.get("opt_TransformTranslateX")).getText()),
                               Integer.parseInt(((JTextField) elements.get("opt_TransformTranslateY")).getText()));

          speed.setRotation(Double.parseDouble(((JTextField) elements.get("opt_TransformRotate")).getText()));

          speed.setShear(Double.parseDouble(((JTextField) elements.get("opt_TransformShearX")).getText()),
                         Double.parseDouble(((JTextField) elements.get("opt_TransformShearY")).getText()));

          // Set tests
          int testSuite = 0;
          if (((JCheckBox) elements.get("test_arcDraw")).isSelected())
            testSuite |= J2dBenchmarkWrapper.J2DTEST_ARC;
          if (((JCheckBox) elements.get("test_ccurveDraw")).isSelected())
            testSuite |= J2dBenchmarkWrapper.J2DTEST_CUBICCURVE;
          if (((JCheckBox) elements.get("test_ellipseDraw")).isSelected())
            testSuite |= J2dBenchmarkWrapper.J2DTEST_ELLIPSE;
          //if (((JCheckBox)elements.get("test_pathDraw")).isSelected())
          //  testSuite |= J2dBenchmarkWrapper.J2DTEST_GENERALPATH; 
          if (((JCheckBox) elements.get("test_lineDraw")).isSelected())
            testSuite |= J2dBenchmarkWrapper.J2DTEST_LINE;
          if (((JCheckBox) elements.get("test_qcurveDraw")).isSelected())
            testSuite |= J2dBenchmarkWrapper.J2DTEST_QUADCURVE;
          if (((JCheckBox) elements.get("test_rectDraw")).isSelected())
            testSuite |= J2dBenchmarkWrapper.J2DTEST_RECTANGLE;
          if (((JCheckBox) elements.get("test_rrectDraw")).isSelected())
            testSuite |= J2dBenchmarkWrapper.J2DTEST_ROUNDRECTANGLE;
          if (((JCheckBox) elements.get("test_image")).isSelected())
            testSuite |= J2dBenchmarkWrapper.J2DTEST_IMAGE;

          if (testSuite != 0)
            {
              speed.setTests(testSuite);

              String initResult = speed.init();

              if (initResult == null)
                {
                  // Create graphics.
                  frame.add(speed, BorderLayout.CENTER);
                  frame.setSize(
                                Integer.parseInt(((JTextField) elements.get("width")).getText()),
                                Integer.parseInt(((JTextField) elements.get("height")).getText()));
                  frame.setVisible(true);

                  // Insets are correctly set only after the native peer was
                  // created.
                  Insets insets = frame.getInsets();
                  frame.setSize(frame.getWidth() + insets.right + insets.left,
                                frame.getHeight() + insets.top + insets.bottom);

                  // Clear any old error messages
                  errorLabel.setText(" ");
                }
              else
                errorLabel.setText(initResult);
            }
          else
            errorLabel.setText("Please select at least one test.");
        }
      catch (NumberFormatException e)
        {
          errorLabel.setText("Please enter valid integers");
        }
    }
  }

  /**
   * Wrapper for the J2dBenchmark, which outputs the results to a GUI
   * instead of the command-line
   */
  private class J2dBenchmarkWrapper
      extends J2dBenchmark
  {
    JFrame myFrame;

    ResultsDisplay display;

    /**
     * Create new J2dBenchmarkWrapper object
     * 
     * @param frame parent frame
     */
    public J2dBenchmarkWrapper(JFrame frame)
    {
      // Redirect log messages to the custom handler
      logger.setUseParentHandlers(false);
      display = new ResultsDisplay();
      display.setLevel(Level.INFO);
      logger.addHandler(display);

      myFrame = frame;
    }

    /**
     * Set dimensions of benchmarking canvas
     * 
     * @param width width of canvas
     * @param height height of canvas
     */
    public void setDimensions(int width, int height)
    {
      screenHeight = height;
      screenWidth = width;
      setSize(width, height);
    }

    /**
     * Set number of iterations
     * 
     * @param it number of iterations
     */
    public void setIterations(int it)
    {
      iterations = it;
    }

    /**
     * Set size of each test
     * 
     * @param size size of test
     */
    public void setTestSize(int size)
    {
      testSize = size;
    }

    /**
     * Set clipping options
     * 
     * @param no run test with no clipping
     * @param with run test with clipping
     * @param zero run test with clipping to zero
     */
    public void setClipping(boolean no, boolean with, boolean zero)
    {
      this.noClippingFlag = no;
      this.withClippingFlag = with;
      this.zeroClippingFlag = zero;
    }

    /**
     * Set buffering options
     * 
     * @param single run test without double-buffering
     * @param doubleb run test with double-buffering
     */
    public void setBuffers(boolean single, boolean doubleb)
    {
      this.singleBufferFlag = single;
      this.doubleBufferFlag = doubleb;
    }

    /**
     * Set fill options
     * 
     * @param type fill type: "solid", "gradient", or "texture"
     * @param file filename to use if texturing
     */
    public void setFill(String type, String file)
    {
      if (type.equals("gradient"))
        this.gradientFlag = true;
      else if (type.equals("texture"))
        {
          this.texture = file;
        }
    }

    /**
     * Set stroke options
     * 
     * @param stroke boolean flag to use random stroking or not
     */
    public void setStroke(boolean stroke)
    {
      this.strokeFlag = stroke;
    }

    /**
     * Set anti-aliasing options
     * 
     * @param alias boolean flag to use anti-aliasing or not
     */
    public void setAlias(boolean alias)
    {
      this.antialiasFlag = alias;
    }

    /**
     * Set alpha composite
     * 
     * @param alpha alpha composite
     */
    public void setComposite(float alpha)
    {
      this.composite = alpha;
    }

    /**
     * Set translation values
     * 
     * @param x x-axis translation
     * @param y y-axis translation
     */
    public void setTranslation(int x, int y)
    {
      this.xtranslate = x;
      this.ytranslate = y;
    }

    /**
     * Set rotation
     * 
     * @param theta angle to rotate by (radians)
     */
    public void setRotation(double theta)
    {
      this.rotate = theta;
    }

    /**
     * Set shear values
     * 
     * @param x x-axis shear value
     * @param y-axis shear value
     */
    public void setShear(double x, double y)
    {
      this.xshear = x;
      this.yshear = y;
    }

    /**
     * Set tests to run
     * 
     * @param tests bit-shifted list of tests (see J2dBenchmark constants)
     */
    public void setTests(int tests)
    {
      awtTests = tests;
    }

    /**
     * Saves test report after each iteration
     */
    void printReport()
    {
      // Report test results to the GUI display
      ArrayList results = new ArrayList();
      for (Iterator i = testSetMap.testIterator(); i.hasNext();)
        {
          TestRecorder recorder = testSetMap.getTest((String) i.next());

          results.add("TEST " + recorder.getTestName() + ": average "
                      + recorder.getAverage() + "ms [" + recorder.getMinTime()
                      + "-" + recorder.getMaxTime() + "]");
        }

      display.report(results);
    }

    /**
     * Disables current frame and displays test results
     */
    void testComplete()
    {
      // Clear benchmarking canvas and display results instead
      myFrame.setVisible(false);

      display.show();
    }
  }

  /**
   * GUI to display results of benchmarking
   */
  private class ResultsDisplay
      extends Handler
      implements ActionListener
  {
    /**
     * Allow user to select results from each iteration
     */
    JComboBox iterations;

    /**
     * Area to print results in
     */
    JTextArea results;

    /**
     * Allow user to view summary or full details of test report
     */
    JCheckBox details;

    /**
     * Store all test results
     */
    ArrayList testResults;

    /**
     * Store all test details
     */
    ArrayList testDetails;

    /**
     * Initialize variables
     */
    public ResultsDisplay()
    {
      testResults = new ArrayList();
      testDetails = new ArrayList();
      testDetails.add(new ArrayList());
    }

    /**
     * Parse all results and display on a GUI
     */
    public void show()
    {
      // Set up panel
      JFrame frame = new JFrame("Java2D benchmark results");
      Container cp = frame.getContentPane();

      // Non-editable text area for the results
      results = new JTextArea();
      results.setEditable(false);
      results.setRows(15);
      results.setColumns(60);

      // Checkbox to optionally display details (ie log messages)
      details = new JCheckBox("Details", false);
      details.addActionListener(this);

      // Combo box to allow selection of iteration number
      iterations = new JComboBox();
      iterations.addActionListener(this);
      for (int i = 0; i < testResults.size(); i++)
        iterations.addItem("Iteration #" + (i + 1));

      // Lay it all out
      JPanel topleft = new JPanel();
      topleft.add(new JLabel("View results from: "));
      topleft.add(iterations);
      topleft.setAlignmentX(JComponent.LEFT_ALIGNMENT);
      details.setAlignmentX(JComponent.RIGHT_ALIGNMENT);
      JPanel top = new JPanel();
      top.setLayout(new BoxLayout(top, BoxLayout.LINE_AXIS));
      top.add(topleft);
      top.add(details);

      cp.add(top, BorderLayout.NORTH);
      cp.add(new JScrollPane(results), BorderLayout.SOUTH);

      frame.pack();
      frame.show();
    }

    /**
     * This overrides the logger publish method, which accepts log messages and
     * saves them for later display
     * 
     * @param record information about the log event
     */
    public void publish(LogRecord record)
    {
      ((ArrayList) testDetails.get(testDetails.size() - 1)).add(record.getMessage());
    }

    /**
     * Accepts a test summary report, generated after each iteration of a test
     * 
     * @param results test results
     */
    public void report(ArrayList results)
    {
      testResults.add(results);
      testDetails.add(new ArrayList());
    }

    /**
     * Provided as part of the Handler interface; not used
     */
    public void flush()
    {
    }

    /**
     * Provided as part of the Handler interface; not used
     */
    public void close()
    {
    }

    /**
     * Handle user-generated events on the results GUI
     */
    public void actionPerformed(ActionEvent ev)
    {
      // Display information about the requested iteration
      int iteration = iterations.getSelectedIndex();
      String message = "";

      // Display summary or details, as requested
      Iterator it;
      if (details.isSelected())
        it = ((ArrayList) testDetails.get(iteration)).iterator();
      else
        it = ((ArrayList) testResults.get(iteration)).iterator();

      // Parse the ArrayList's
      while (it.hasNext())
        {
          message = message + ((String) it.next() + "\n");
        }

      // Output to screen
      results.setText(message);
    }
  }

}
