/* DefaultHSBChooserPanel.java --
   Copyright (C) 2004, 2005 Free Software Foundation, Inc.

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


package javax.swing.colorchooser;

import java.awt.BorderLayout;
import java.awt.Color;
import java.awt.Container;
import java.awt.Dimension;
import java.awt.Graphics;
import java.awt.GridLayout;
import java.awt.Image;
import java.awt.Point;
import java.awt.event.MouseAdapter;
import java.awt.event.MouseEvent;
import java.awt.event.MouseMotionListener;
import java.awt.image.MemoryImageSource;

import javax.swing.AbstractButton;
import javax.swing.ButtonGroup;
import javax.swing.Icon;
import javax.swing.JColorChooser;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JRadioButton;
import javax.swing.JSlider;
import javax.swing.JSpinner;
import javax.swing.SpinnerNumberModel;
import javax.swing.SwingConstants;
import javax.swing.event.ChangeEvent;
import javax.swing.event.ChangeListener;

/**
 * This is the Default HSB Panel displayed in the JColorChooser.
 */
class DefaultHSBChooserPanel extends AbstractColorChooserPanel
{
  /** The gradient image displayed.
   * This is package-private to avoid an accessor method.  */
  transient Image gradientImage;

  /** The Panel that holds the gradient image. */
  private transient JPanel gradientPanel;

  /** The track gradient image.
   * This is package-private to avoid an accessor method.  */
  transient Image trackImage;

  /** The panel that holds the track. */
  private transient JPanel trackPanel;

  /** The slider for the locked HSB value.
   * This is package-private to avoid an accessor method.  */
  transient JSlider slider;

  /** The RadioButton that controls the Hue.
   * This is package-private to avoid an accessor method.  */
  transient JRadioButton hRadio;

  /** The RadioButton that controls the Saturation.
   * This is package-private to avoid an accessor method.  */
  transient JRadioButton sRadio;

  /** The RadioButton that controls the Brightness.
   * This is package-private to avoid an accessor method.  */
  transient JRadioButton bRadio;

  /** The JSpinner that controls the Hue.
   * This is package-private to avoid an accessor method.  */
  transient JSpinner hSpinner;

  /** The JSpinner that controls the Saturation.
   * This is package-private to avoid an accessor method.  */
  transient JSpinner sSpinner;

  /** The JSpinner that controls the Brightness.
   * This is package-private to avoid an accessor method.  */
  transient JSpinner bSpinner;

  /** The default width of the gradient image. */
  private static final int imgWidth = 200;

  /** The default height of the gradient image. */
  private static final int imgHeight = 200;

  /** The default width of the track gradient. */
  private static final int trackWidth = 30;

  /** The JLabel for Red. */
  private static final JLabel R = new JLabel("R");

  /** The JLabel for Green. */
  private static final JLabel G = new JLabel("G");

  /** The JLabel for Blue. */
  private static final JLabel B = new JLabel("B");

  // FIXME: Should be textfields.

  /** The JLabel that displays the value of Red. */
  private transient JLabel rFull;

  /** The JLabel that displays the value of Green. */
  private transient JLabel gFull;

  /** The JLabel that displays the value of Blue. */
  private transient JLabel bFull;

  /** The point that is displayed in the gradient image.
   * Package-private to avoid an accessor method.
   */
  transient Point gradientPoint = new Point();

  /**
   * This indicates that the change to the slider or point is triggered
   * internally.
   * This is package-private to avoid an accessor method.
   */
  transient boolean internalChange = false;

  /** This indicates that the change to the spinner is triggered
   * internally.
   * This is package-private to avoid an accessor method.  */
  transient boolean spinnerTrigger = false;

  /** This int identifies which spinner is currently locked.
   * This is package-private to avoid an accessor method.  */
  transient int locked = -1;

  /** This value indicates that the Hue spinner is locked. */
  static final int HLOCKED = 0;

  /** This value indicates that the Saturation spinner is locked. */
  static final int SLOCKED = 1;

  /** This value indicates that the Brightness spinner is locked. */
  static final int BLOCKED = 2;

  /**
   * This method indicates that the mouse event is in the process of being
   * handled.
   * This is package-private to avoid an accessor method.
   */
  transient boolean handlingMouse;

  /**
   * This helper class handles mouse events on the gradient image.
   */
  class MainGradientMouseListener extends MouseAdapter
    implements MouseMotionListener
  {
    /**
     * This method is called when the mouse is pressed over the gradient
     * image. The JColorChooser is then updated with new HSB values.
     *
     * @param e The MouseEvent.
     */
    public void mousePressed(MouseEvent e)
    {
      gradientPoint = e.getPoint();
      update(e.getPoint());
    }

    /**
     * This method is called when the mouse is dragged over the gradient
     * image. The JColorChooser is then updated with the new HSB values.
     *
     * @param e The MouseEvent.
     */
    public void mouseDragged(MouseEvent e)
    {
      Point p = e.getPoint();
      if (p.x < 0 || p.y < 0 || p.y > imgHeight || p.x > imgWidth)
	return;

      gradientPoint = p;
      update(p);
    }

    /**
     * This method is called when the mouse is moved over the gradient image.
     *
     * @param e The MouseEvent.
     */
    public void mouseMoved(MouseEvent e)
    {
      // Do nothing.
    }

    /**
     * This method updates the JColorChooser with the new values.
     *
     * @param p The Point where the MouseEvent occurred.
     */
    private void update(Point p)
    {
      handlingMouse = true;
      if (hSpinner.isEnabled())
	updateH(p);
      else if (sSpinner.isEnabled())
	updateS(p);
      else
	updateB(p);
      handlingMouse = false;
    }

    /**
     * This method updates the SB values if Hue is locked.
     *
     * @param p The point where the MouseEvent occurred.
     */
    private void updateH(Point p)
    {
      float s = (imgWidth - p.x * 1f) / imgWidth;
      float b = (imgHeight - p.y * 1f) / imgHeight;

      // Avoid two changes to the model by changing internalChange to true.
      internalChange = true;
      sSpinner.setValue(new Integer((int) (s * 100)));
      internalChange = false;
      bSpinner.setValue(new Integer((int) (b * 100)));

      revalidate();
    }

    /**
     * This method updates the HB values if Saturation is locked.
     *
     * @param p The point where the MouseEvent occurred.
     */
    private void updateS(Point p)
    {
      float h = p.x * 1f / imgWidth;
      float b = (imgHeight - p.y * 1f) / imgHeight;

      internalChange = true;
      hSpinner.setValue(new Integer((int) (h * 365)));
      internalChange = false;
      bSpinner.setValue(new Integer((int) (b * 100)));

      revalidate();
    }

    /**
     * This method updates the HS values if Brightness is locked.
     *
     * @param p The point where the MouseEvent occurred.
     */
    private void updateB(Point p)
    {
      float h = p.x * 1f / imgWidth;
      float s = (imgHeight - p.y * 1f) / imgHeight;

      internalChange = true;
      hSpinner.setValue(new Integer((int) (h * 365)));
      internalChange = false;
      sSpinner.setValue(new Integer((int) (s * 100)));

      revalidate();
    }
  }

  /**
   * This method listens for slider value changes.
   */
  class SliderChangeListener implements ChangeListener
  {
    /**
     * This method is called when the slider value changes. It should change
     * the color of the JColorChooser.
     *
     * @param e The ChangeEvent.
     */
    public void stateChanged(ChangeEvent e)
    {
      if (internalChange)
	return;

      Integer value = new Integer(slider.getValue());

      switch (locked)
        {
	case HLOCKED:
	  hSpinner.setValue(value);
	  break;
	case SLOCKED:
	  sSpinner.setValue(value);
	  break;
	case BLOCKED:
	  bSpinner.setValue(value);
	  break;
        }
    }
  }

  /**
   * This helper class determines the active JSpinner.
   */
  class RadioStateListener implements ChangeListener
  {
    /**
     * This method is called when there is a new JRadioButton that was
     * selected. As a result, it should activate the associated JSpinner.
     *
     * @param e The ChangeEvent.
     */
    public void stateChanged(ChangeEvent e)
    {
      JSpinner change;
      if (e.getSource() == hRadio)
        {
	  locked = HLOCKED;
	  change = hSpinner;
        }
      else if (e.getSource() == sRadio)
        {
	  locked = SLOCKED;
	  change = sSpinner;
        }
      else
        {
	  locked = BLOCKED;
	  change = bSpinner;
        }

      change.setEnabled(((AbstractButton) e.getSource()).isSelected());
      updateSlider();
      updateTrack();
      updateImage();
      repaint();
    }
  }

  /**
   * This class listens to the JSpinners for changes.
   */
  class ImageScrollListener implements ChangeListener
  {
    /**
     * This method is called whenever one of the JSpinner values change. The
     * JColorChooser should be updated with the new HSB values.
     *
     * @param e The ChangeEvent.
     */
    public void stateChanged(ChangeEvent e)
    {
      if (internalChange)
	return;

      float h = ((Number) hSpinner.getValue()).intValue() / 360f;
      float s = ((Number) sSpinner.getValue()).intValue() / 100f;
      float b = ((Number) bSpinner.getValue()).intValue() / 100f;

      spinnerTrigger = true;
      getColorSelectionModel().setSelectedColor(new Color(Color.HSBtoRGB(h, s,
                                                                         b)));
      spinnerTrigger = false;

      if (! handlingMouse && slider != null && ! slider.getValueIsAdjusting())
        {
	  updateImage();
	  updateTrack();
        }
      repaint();
    }
  }

  /**
   * Creates a new DefaultHSBChooserPanel object.
   */
  DefaultHSBChooserPanel()
  {
    super();
  }

  /**
   * This method returns the name displayed by the JColorChooser tab that
   * holds this panel.
   *
   * @return The name displayed in the JColorChooser tab.
   */
  public String getDisplayName()
  {
    return "HSB";
  }

  /**
   * This method updates the various components inside the HSBPanel (the
   * JSpinners, the JSlider, and the gradient image point) with updated
   * values when the JColorChooser color value changes.
   */
  public void updateChooser()
  {
    Color c = getColorSelectionModel().getSelectedColor();

    float[] hsbVals = Color.RGBtoHSB(c.getRed(), c.getGreen(), c.getBlue(),
                                     null);

    internalChange = true;

    if (! spinnerTrigger)
      {
	hSpinner.setValue(new Integer((int) (hsbVals[0] * 360)));
	sSpinner.setValue(new Integer((int) (hsbVals[1] * 100)));
	bSpinner.setValue(new Integer((int) (hsbVals[2] * 100)));
      }

    switch (locked)
      {
      case HLOCKED:
	if (slider != null)
	  slider.setValue(((Number) hSpinner.getValue()).intValue());
	if (! handlingMouse)
	  {
	    gradientPoint.x = (int) ((1
	                      - ((Number) sSpinner.getValue()).intValue() / 100f) * imgWidth);
	    gradientPoint.y = (int) ((1
	                      - ((Number) bSpinner.getValue()).intValue() / 100f) * imgHeight);
	  }
	break;
      case SLOCKED:
	if (slider != null)
	  slider.setValue(((Number) sSpinner.getValue()).intValue());
	if (! handlingMouse)
	  {
	    gradientPoint.x = (int) (((Number) hSpinner.getValue()).intValue() / 360f * imgWidth);
	    gradientPoint.y = (int) ((1
	                      - ((Number) bSpinner.getValue()).intValue() / 100f) * imgHeight);
	  }
	break;
      case BLOCKED:
	if (slider != null)
	  slider.setValue(((Number) bSpinner.getValue()).intValue());
	if (! handlingMouse)
	  {
	    gradientPoint.x = (int) (((Number) hSpinner.getValue()).intValue() / 360f * imgWidth);
	    gradientPoint.y = (int) ((1
	                      - ((Number) sSpinner.getValue()).intValue() / 100f) * imgHeight);
	  }
	break;
      }
    internalChange = false;

    if (! handlingMouse && slider != null && ! slider.getValueIsAdjusting())
      updateImage();

    if (! handlingMouse || locked != HLOCKED)
      updateTrack();
    updateTextFields();
  }

  /**
   * This method builds the DefaultHSBChooserPanel.
   */
  protected void buildChooser()
  {
    setLayout(new BorderLayout());

    add(buildRightPanel(), BorderLayout.EAST);

    JPanel container = new JPanel();
    container.setLayout(new BorderLayout());

    gradientPanel = new JPanel()
        {
	  public Dimension getPreferredSize()
	  {
	    return new Dimension(imgWidth, imgHeight);
	  }

	  public void paint(Graphics g)
	  {
	    if (gradientImage != null)
	      g.drawImage(gradientImage, 0, 0, this);

	    Color saved = g.getColor();
	    g.setColor(Color.WHITE);
	    g.drawOval(gradientPoint.x - 3, gradientPoint.y - 3, 6, 6);
	    g.setColor(saved);
	  }
        };

    MouseAdapter ml = new MainGradientMouseListener();
    gradientPanel.addMouseListener(ml);
    gradientPanel.addMouseMotionListener((MouseMotionListener) ml);

    trackPanel = new JPanel()
        {
	  public Dimension getPreferredSize()
	  {
	    return new Dimension(trackWidth, imgHeight);
	  }

	  public void paint(Graphics g)
	  {
	    if (trackImage != null)
	      g.drawImage(trackImage, 0, 0, this);
	  }
        };

    slider = new JSlider();
    slider.setPaintTrack(false);
    slider.setPaintTicks(false);

    slider.setOrientation(SwingConstants.VERTICAL);

    updateSlider();

    container.add(gradientPanel, BorderLayout.WEST);
    container.add(slider, BorderLayout.CENTER);
    container.add(trackPanel, BorderLayout.EAST);

    add(container, BorderLayout.WEST);
    slider.addChangeListener(new SliderChangeListener());
    repaint();
  }

  /**
   * This method uninstalls the DefaultHSBPanel.
   *
   * @param chooser The JColorChooser to remove this panel from.
   */
  public void uninstallChooserPanel(JColorChooser chooser)
  {
    trackImage = null;
    gradientImage = null;
    gradientPanel = null;
    slider = null;

    hSpinner = null;
    sSpinner = null;
    bSpinner = null;

    hRadio = null;
    sRadio = null;
    bRadio = null;

    removeAll();
    super.uninstallChooserPanel(chooser);
  }

  /**
   * This helper method creates the right side panel (the panel with the
   * Spinners and TextFields).
   *
   * @return The right side panel.
   */
  private Container buildRightPanel()
  {
    JPanel container = new JPanel();
    container.setLayout(new GridLayout(6, 2));

    hRadio = new JRadioButton("H");
    sRadio = new JRadioButton("S");
    bRadio = new JRadioButton("B");

    ButtonGroup group = new ButtonGroup();
    group.add(hRadio);
    group.add(sRadio);
    group.add(bRadio);

    hSpinner = new JSpinner(new SpinnerNumberModel(0, 0, 359, 1));
    sSpinner = new JSpinner(new SpinnerNumberModel(0, 0, 100, 1));
    bSpinner = new JSpinner(new SpinnerNumberModel(100, 0, 100, 1));

    hSpinner.setEnabled(false);
    sSpinner.setEnabled(false);
    bSpinner.setEnabled(false);

    ChangeListener cl = new RadioStateListener();
    ChangeListener scroll = new ImageScrollListener();

    hRadio.addChangeListener(cl);
    sRadio.addChangeListener(cl);
    bRadio.addChangeListener(cl);

    hSpinner.addChangeListener(scroll);
    sSpinner.addChangeListener(scroll);
    bSpinner.addChangeListener(scroll);

    hRadio.setSelected(true);

    container.add(hRadio);
    container.add(hSpinner);

    container.add(sRadio);
    container.add(sSpinner);

    container.add(bRadio);
    container.add(bSpinner);

    rFull = new JLabel("red full");
    gFull = new JLabel("green full");
    bFull = new JLabel("blue full");

    container.add(R);
    container.add(rFull);

    container.add(G);
    container.add(gFull);

    container.add(B);
    container.add(bFull);

    return container;
  }

  /**
   * This method returns the small display icon.
   *
   * @return The small display icon.
   */
  public Icon getSmallDisplayIcon()
  {
    return null;
  }

  /**
   * This method returns the large display icon.
   *
   * @return The large display icon.
   */
  public Icon getLargeDisplayIcon()
  {
    return null;
  }

  /**
   * This method paints the chooser panel.
   *
   * @param g The graphics object to paint with.
   */
  public void paint(Graphics g)
  {
    super.paint(g);
  }

  /**
   * This method updates the gradient image with a new one taking the Hue
   * value as the constant.
   */
  private void updateHLockImage()
  {
    int index = 0;
    int[] pix = new int[imgWidth * imgHeight];
    float hValue = ((Number) hSpinner.getValue()).intValue() / 360f;

    for (int j = 0; j < imgHeight; j++)
      for (int i = 0; i < imgWidth; i++)
	pix[index++] = Color.HSBtoRGB(hValue, (imgWidth - i * 1f) / imgWidth,
	                              (imgHeight - j * 1f) / imgHeight)
	               | (255 << 24);

    gradientImage = createImage(new MemoryImageSource(imgWidth, imgHeight,
                                                      pix, 0, imgWidth));
  }

  /**
   * This method updates the gradient image with a new one taking the
   * Brightness value as the constant.
   */
  private void updateBLockImage()
  {
    int[] pix = new int[imgWidth * imgHeight];
    float bValue = ((Number) bSpinner.getValue()).intValue() / 100f;

    int index = 0;
    for (int j = 0; j < imgHeight; j++)
      for (int i = 0; i < imgWidth; i++)
	pix[index++] = Color.HSBtoRGB(i * 1f / imgWidth,
	                              (imgHeight - j * 1f) / imgHeight, bValue)
	               | (255 << 24);

    gradientImage = createImage(new MemoryImageSource(imgWidth, imgHeight,
                                                      pix, 0, imgWidth));
  }

  /**
   * This method updates the gradient image with a new one taking the
   * Saturation value as the constant.
   */
  private void updateSLockImage()
  {
    int[] pix = new int[imgWidth * imgHeight];
    float sValue = ((Number) sSpinner.getValue()).intValue() / 100f;

    int index = 0;
    for (int j = 0; j < imgHeight; j++)
      for (int i = 0; i < imgWidth; i++)
	pix[index++] = Color.HSBtoRGB(i * 1f / imgWidth, sValue,
	                              (imgHeight - j * 1f) / imgHeight)
	               | (255 << 24);
    gradientImage = createImage(new MemoryImageSource(imgWidth, imgHeight,
                                                      pix, 0, imgWidth));
  }

  /**
   * This method calls the appropriate method to update the gradient image
   * depending on which HSB value is constant.
   * This is package-private to avoid an accessor method.
   */
  void updateImage()
  {
    switch (locked)
      {
      case HLOCKED:
	updateHLockImage();
	break;
      case SLOCKED:
	updateSLockImage();
	break;
      case BLOCKED:
	updateBLockImage();
	break;
      }
  }

  /**
   * This method updates the TextFields with the correct RGB values.
   */
  private void updateTextFields()
  {
    int c = getColorSelectionModel().getSelectedColor().getRGB();

    rFull.setText("" + (c >> 16 & 0xff));
    gFull.setText("" + (c >> 8 & 0xff));
    bFull.setText("" + (c & 0xff));

    repaint();
  }

  /**
   * This method updates the slider in response to making a different HSB
   * property the constant.
   * This is package-private to avoid an accessor method.
   */
  void updateSlider()
  {
    if (slider == null)
      return;

    slider.setMinimum(0);
    if (locked == HLOCKED)
      {
	slider.setMaximum(359);
	;
	slider.setValue(((Number) hSpinner.getValue()).intValue());
	slider.setInverted(true);
      }
    else
      {
	slider.setMaximum(100);
	slider.setInverted(false);
	if (sRadio.isSelected())
	  slider.setValue(((Number) sSpinner.getValue()).intValue());
	else
	  slider.setValue(((Number) bSpinner.getValue()).intValue());
      }
    repaint();
  }

  /**
   * This method updates the track gradient image depending on which HSB
   * property is constant.
   * This is package-private to avoid an accessor method.
   */
  void updateTrack()
  {
    switch (locked)
      {
      case HLOCKED:
	updateHTrack();
	break;
      case SLOCKED:
	updateSTrack();
	break;
      case BLOCKED:
	updateBTrack();
	break;
      }
  }

  /**
   * This method updates the track gradient image if the Hue value is allowed
   * to change (according to the JRadioButtons).
   */
  private void updateHTrack()
  {
    int trackIndex = 0;
    int[] trackPix = new int[trackWidth * imgHeight];

    for (int j = 0; j < imgHeight; j++)
      for (int i = 0; i < trackWidth; i++)
	trackPix[trackIndex++] = Color.HSBtoRGB(j * 1f / imgHeight, 1f, 1f)
	                         | (255 << 24);

    trackImage = createImage(new MemoryImageSource(trackWidth, imgHeight,
                                                   trackPix, 0, trackWidth));
  }

  /**
   * This method updates the track gradient image if the Saturation value is
   * allowed to change (according to the JRadioButtons).
   */
  private void updateSTrack()
  {
    int[] trackPix = new int[trackWidth * imgHeight];

    float hValue = ((Number) hSpinner.getValue()).intValue() / 360f;
    float bValue = ((Number) bSpinner.getValue()).intValue() / 100f;

    int trackIndex = 0;
    for (int j = 0; j < imgHeight; j++)
      for (int i = 0; i < trackWidth; i++)
	trackPix[trackIndex++] = Color.HSBtoRGB(hValue,
	                                        (imgHeight - j * 1f) / imgHeight,
	                                        bValue) | (255 << 24);

    trackImage = createImage(new MemoryImageSource(trackWidth, imgHeight,
                                                   trackPix, 0, trackWidth));
  }

  /**
   * This method updates the track gradient image if the Brightness value is
   * allowed to change (according to the JRadioButtons).
   */
  private void updateBTrack()
  {
    int[] trackPix = new int[trackWidth * imgHeight];

    float hValue = ((Number) hSpinner.getValue()).intValue() / 360f;
    float sValue = ((Number) sSpinner.getValue()).intValue() / 100f;

    int trackIndex = 0;
    for (int j = 0; j < imgHeight; j++)
      for (int i = 0; i < trackWidth; i++)
	trackPix[trackIndex++] = Color.HSBtoRGB(hValue, sValue,
	                                        (imgHeight - j * 1f) / imgHeight)
	                         | (255 << 24);

    trackImage = createImage(new MemoryImageSource(trackWidth, imgHeight,
                                                   trackPix, 0, trackWidth));
  }

  /**
   * This method returns the HSB values for the currently selected color.
   *
   * @return The HSB values for the currently selected color.
   */
  private float[] getHSBValues()
  {
    Color c = getColorFromModel();
    float[] f = Color.RGBtoHSB(c.getRed(), c.getGreen(), c.getBlue(), null);
    return f;
  }
}
