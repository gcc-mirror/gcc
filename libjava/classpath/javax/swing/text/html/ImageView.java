package javax.swing.text.html;

import gnu.javax.swing.text.html.ImageViewIconFactory;
import gnu.javax.swing.text.html.css.Length;

import java.awt.Graphics;
import java.awt.Image;
import java.awt.MediaTracker;
import java.awt.Rectangle;
import java.awt.Shape;
import java.awt.Toolkit;
import java.awt.image.ImageObserver;
import java.net.MalformedURLException;
import java.net.URL;

import javax.swing.Icon;
import javax.swing.SwingUtilities;
import javax.swing.text.AbstractDocument;
import javax.swing.text.AttributeSet;
import javax.swing.text.BadLocationException;
import javax.swing.text.Document;
import javax.swing.text.Element;
import javax.swing.text.View;
import javax.swing.text.Position.Bias;
import javax.swing.text.html.HTML.Attribute;

/**
 * A view, representing a single image, represented by the HTML IMG tag.
 *
 * @author Audrius Meskauskas (AudriusA@Bioinformatics.org)
 */
public class ImageView extends View
{
  /**
   * Tracks image loading state and performs the necessary layout updates.
   */
  class Observer
    implements ImageObserver
  {

    public boolean imageUpdate(Image image, int flags, int x, int y, int width, int height)
    {
      boolean widthChanged = false;
      if ((flags & ImageObserver.WIDTH) != 0 && spans[X_AXIS] == null)
        widthChanged = true;
      boolean heightChanged = false;
      if ((flags & ImageObserver.HEIGHT) != 0 && spans[Y_AXIS] == null)
        heightChanged = true;
      if (widthChanged || heightChanged)
        safePreferenceChanged(ImageView.this, widthChanged, heightChanged);
      boolean ret = (flags & ALLBITS) != 0;
      return ret;
    }

  }

  /**
   * True if the image loads synchronuosly (on demand). By default, the image
   * loads asynchronuosly.
   */
  boolean loadOnDemand;

  /**
   * The image icon, wrapping the image,
   */
  Image image;

  /**
   * The image state.
   */
  byte imageState = MediaTracker.LOADING;

  /**
   * True when the image needs re-loading, false otherwise.
   */
  private boolean reloadImage;

  /**
   * True when the image properties need re-loading, false otherwise.
   */
  private boolean reloadProperties;

  /**
   * True when the width is set as CSS/HTML attribute.
   */
  private boolean haveWidth;

  /**
   * True when the height is set as CSS/HTML attribute.
   */
  private boolean haveHeight;

  /**
   * True when the image is currently loading.
   */
  private boolean loading;

  /**
   * The current width of the image.
   */
  private int width;

  /**
   * The current height of the image.
   */
  private int height;

  /**
   * Our ImageObserver for tracking the loading state.
   */
  private ImageObserver observer;

  /**
   * The CSS width and height.
   *
   * Package private to avoid synthetic accessor methods.
   */
  Length[] spans;

  /**
   * The cached attributes.
   */
  private AttributeSet attributes;

  /**
   * Creates the image view that represents the given element.
   *
   * @param element the element, represented by this image view.
   */
  public ImageView(Element element)
  {
    super(element);
    spans = new Length[2];
    observer = new Observer();
    reloadProperties = true;
    reloadImage = true;
    loadOnDemand = false;
  }

  /**
   * Load or reload the image. This method initiates the image reloading. After
   * the image is ready, the repaint event will be scheduled. The current image,
   * if it already exists, will be discarded.
   */
  private void reloadImage()
  {
    loading = true;
    reloadImage = false;
    haveWidth = false;
    haveHeight = false;
    image = null;
    width = 0;
    height = 0;
    try
      {
        loadImage();
        updateSize();
      }
    finally
      {
        loading = false;
      }
  }

  /**
   * Get the image alignment. This method works handling standart alignment
   * attributes in the HTML IMG tag (align = top bottom middle left right).
   * Depending from the parameter, either horizontal or vertical alingment
   * information is returned.
   *
   * @param axis -
   *          either X_AXIS or Y_AXIS
   */
  public float getAlignment(int axis)
  {
    AttributeSet attrs = getAttributes();
    Object al = attrs.getAttribute(Attribute.ALIGN);

    // Default is top left aligned.
    if (al == null)
      return 0.0f;

    String align = al.toString();

    if (axis == View.X_AXIS)
      {
        if (align.equals("middle"))
          return 0.5f;
        else if (align.equals("left"))
          return 0.0f;
        else if (align.equals("right"))
          return 1.0f;
        else
          return 0.0f;
      }
    else if (axis == View.Y_AXIS)
      {
        if (align.equals("middle"))
          return 0.5f;
        else if (align.equals("top"))
          return 0.0f;
        else if (align.equals("bottom"))
          return 1.0f;
        else
          return 0.0f;
      }
    else
      throw new IllegalArgumentException("axis " + axis);
  }

  /**
   * Get the text that should be shown as the image replacement and also as the
   * image tool tip text. The method returns the value of the attribute, having
   * the name {@link Attribute#ALT}. If there is no such attribute, the image
   * name from the url is returned. If the URL is not available, the empty
   * string is returned.
   */
  public String getAltText()
  {
    Object rt = getAttributes().getAttribute(Attribute.ALT);
    if (rt != null)
      return rt.toString();
    else
      {
        URL u = getImageURL();
        if (u == null)
          return "";
        else
          return u.getFile();
      }
  }

  /**
   * Returns the combination of the document and the style sheet attributes.
   */
  public AttributeSet getAttributes()
  {
    if (attributes == null)
      attributes = getStyleSheet().getViewAttributes(this);
    return attributes;
  }

  /**
   * Get the image to render. May return null if the image is not yet loaded.
   */
  public Image getImage()
  {
    updateState();
    return image;
  }

  /**
   * Get the URL location of the image to render. If this method returns null,
   * the "no image" icon is rendered instead. By defaul, url must be present as
   * the "src" property of the IMG tag. If it is missing, null is returned and
   * the "no image" icon is rendered.
   *
   * @return the URL location of the image to render.
   */
  public URL getImageURL()
  {
    Element el = getElement();
    String src = (String) el.getAttributes().getAttribute(Attribute.SRC);
    URL url = null;
    if (src != null)
      {
        URL base = ((HTMLDocument) getDocument()).getBase();
        try
          {
            url = new URL(base, src);
          }
        catch (MalformedURLException ex)
          {
            // Return null.
          }
      }
    return url;
  }

  /**
   * Get the icon that should be displayed while the image is loading and hence
   * not yet available.
   *
   * @return an icon, showing a non broken sheet of paper with image.
   */
  public Icon getLoadingImageIcon()
  {
    return ImageViewIconFactory.getLoadingImageIcon();
  }

  /**
   * Get the image loading strategy.
   *
   * @return false (default) if the image is loaded when the view is
   *         constructed, true if the image is only loaded on demand when
   *         rendering.
   */
  public boolean getLoadsSynchronously()
  {
    return loadOnDemand;
  }

  /**
   * Get the icon that should be displayed when the image is not available.
   *
   * @return an icon, showing a broken sheet of paper with image.
   */
  public Icon getNoImageIcon()
  {
    return ImageViewIconFactory.getNoImageIcon();
  }

  /**
   * Get the preferred span of the image along the axis. The image size is first
   * requested to the attributes {@link Attribute#WIDTH} and
   * {@link Attribute#HEIGHT}. If they are missing, and the image is already
   * loaded, the image size is returned. If there are no attributes, and the
   * image is not loaded, zero is returned.
   *
   * @param axis -
   *          either X_AXIS or Y_AXIS
   * @return either width of height of the image, depending on the axis.
   */
  public float getPreferredSpan(int axis)
  {
    Image image = getImage();

    if (axis == View.X_AXIS)
      {
        if (spans[axis] != null)
          return spans[axis].getValue();
        else if (image != null)
          return image.getWidth(getContainer());
        else
          return getNoImageIcon().getIconWidth();
      }
    else if (axis == View.Y_AXIS)
      {
        if (spans[axis] != null)
          return spans[axis].getValue();
        else if (image != null)
          return image.getHeight(getContainer());
        else
          return getNoImageIcon().getIconHeight();
      }
    else
      throw new IllegalArgumentException("axis " + axis);
  }

  /**
   * Get the associated style sheet from the document.
   *
   * @return the associated style sheet.
   */
  protected StyleSheet getStyleSheet()
  {
    HTMLDocument doc = (HTMLDocument) getDocument();
    return doc.getStyleSheet();
  }

  /**
   * Get the tool tip text. This is overridden to return the value of the
   * {@link #getAltText()}. The parameters are ignored.
   *
   * @return that is returned by getAltText().
   */
  public String getToolTipText(float x, float y, Shape shape)
  {
    return getAltText();
  }

  /**
   * Paints the image or one of the two image state icons. The image is resized
   * to the shape bounds. If there is no image available, the alternative text
   * is displayed besides the image state icon.
   *
   * @param g
   *          the Graphics, used for painting.
   * @param bounds
   *          the bounds of the region where the image or replacing icon must be
   *          painted.
   */
  public void paint(Graphics g, Shape bounds)
  {
    updateState();
    Rectangle r = bounds instanceof Rectangle ? (Rectangle) bounds
                                              : bounds.getBounds();
    Image image = getImage();
    if (image != null)
      {
        g.drawImage(image, r.x, r.y, r.width, r.height, observer);
      }
    else
      {
        Icon icon = getNoImageIcon();
        if (icon != null)
          icon.paintIcon(getContainer(), g, r.x, r.y);
      }
  }

  /**
   * Set if the image should be loaded only when needed (synchronuosly). By
   * default, the image loads asynchronuosly. If the image is not yet ready, the
   * icon, returned by the {@link #getLoadingImageIcon()}, is displayed.
   */
  public void setLoadsSynchronously(boolean load_on_demand)
  {
    loadOnDemand = load_on_demand;
  }

  /**
   * Update all cached properties from the attribute set, returned by the
   * {@link #getAttributes}.
   */
  protected void setPropertiesFromAttributes()
  {
    AttributeSet atts = getAttributes();
    StyleSheet ss = getStyleSheet();
    float emBase = ss.getEMBase(atts);
    float exBase = ss.getEXBase(atts);
    spans[X_AXIS] = (Length) atts.getAttribute(CSS.Attribute.WIDTH);
    if (spans[X_AXIS] != null)
      {
        spans[X_AXIS].setFontBases(emBase, exBase);
      }
    spans[Y_AXIS] = (Length) atts.getAttribute(CSS.Attribute.HEIGHT);
    if (spans[Y_AXIS] != null)
      {
        spans[Y_AXIS].setFontBases(emBase, exBase);
      }
  }

  /**
   * Maps the picture co-ordinates into the image position in the model. As the
   * image is not divideable, this is currently implemented always to return the
   * start offset.
   */
  public int viewToModel(float x, float y, Shape shape, Bias[] bias)
  {
    return getStartOffset();
  }

  /**
   * This is currently implemented always to return the area of the image view,
   * as the image is not divideable by character positions.
   *
   * @param pos character position
   * @param area of the image view
   * @param bias bias
   *
   * @return the shape, where the given character position should be mapped.
   */
  public Shape modelToView(int pos, Shape area, Bias bias)
      throws BadLocationException
  {
    return area;
  }

  /**
   * Starts loading the image asynchronuosly. If the image must be loaded
   * synchronuosly instead, the {@link #setLoadsSynchronously} must be
   * called before calling this method. The passed parameters are not used.
   */
  public void setSize(float width, float height)
  {
    updateState();
    // TODO: Implement this when we have an alt view for the alt=... attribute.
  }

  /**
   * This makes sure that the image and properties have been loaded.
   */
  private void updateState()
  {
    if (reloadImage)
      reloadImage();
    if (reloadProperties)
      setPropertiesFromAttributes();
  }

  /**
   * Actually loads the image.
   */
  private void loadImage()
  {
    URL src = getImageURL();
    Image newImage = null;
    if (src != null)
      {
        // Call getImage(URL) to allow the toolkit caching of that image URL.
        Toolkit tk = Toolkit.getDefaultToolkit();
        newImage = tk.getImage(src);
        tk.prepareImage(newImage, -1, -1, observer);
        if (newImage != null && getLoadsSynchronously())
          {
            // Load image synchronously.
            MediaTracker tracker = new MediaTracker(getContainer());
            tracker.addImage(newImage, 0);
            try
              {
                tracker.waitForID(0);
              }
            catch (InterruptedException ex)
              {
                Thread.interrupted();
              }

          }
      }
    image = newImage;
  }

  /**
   * Updates the size parameters of the image.
   */
  private void updateSize()
  {
    int newW = 0;
    int newH = 0;
    Image newIm = getImage();
    if (newIm != null)
      {
        // Fetch width.
        Length l = spans[X_AXIS];
        if (l != null)
          {
            newW = (int) l.getValue();
            haveWidth = true;
          }
        else
          {
            newW = newIm.getWidth(observer);
          }
        // Fetch height.
        l = spans[Y_AXIS];
        if (l != null)
          {
            newH = (int) l.getValue();
            haveHeight = true;
          }
        else
          {
            newW = newIm.getWidth(observer);
          }
        // Go and trigger loading.
        Toolkit tk = Toolkit.getDefaultToolkit();
        if (haveWidth || haveHeight)
          tk.prepareImage(newIm, width, height, observer);
        else
          tk.prepareImage(newIm, -1, -1, observer);
      }
  }

  /**
   * Calls preferenceChanged from the event dispatch thread and within
   * a read lock to protect us from threading issues.
   *
   * @param v the view
   * @param width true when the width changed
   * @param height true when the height changed
   */
  void safePreferenceChanged(final View v, final boolean width,
                             final boolean height)
  {
    if (SwingUtilities.isEventDispatchThread())
      {
        Document doc = getDocument();
        if (doc instanceof AbstractDocument)
          ((AbstractDocument) doc).readLock();
        try
          {
            preferenceChanged(v, width, height);
          }
        finally
          {
            if (doc instanceof AbstractDocument)
              ((AbstractDocument) doc).readUnlock();
          }
      }
    else
      {
        SwingUtilities.invokeLater(new Runnable()
        {
          public void run()
          {
            safePreferenceChanged(v, width, height);
          }
        });
      }
  }
}
