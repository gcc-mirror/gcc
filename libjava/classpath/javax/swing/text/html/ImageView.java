package javax.swing.text.html;

import gnu.javax.swing.text.html.CombinedAttributes;
import gnu.javax.swing.text.html.ImageViewIconFactory;

import java.awt.Graphics;
import java.awt.Image;
import java.awt.MediaTracker;
import java.awt.Rectangle;
import java.awt.Shape;
import java.net.MalformedURLException;
import java.net.URL;

import javax.swing.Icon;
import javax.swing.ImageIcon;
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
   * True if the image loads synchronuosly (on demand). By default, the image
   * loads asynchronuosly.
   */
  boolean loadOnDemand;
  
  /**
   * The image icon, wrapping the image,
   */
  ImageIcon imageIcon;
 
  /**
   * The image state.
   */
  byte imageState = MediaTracker.LOADING;

  /**
   * Creates the image view that represents the given element.
   * 
   * @param element the element, represented by this image view.
   */
  public ImageView(Element element)
  {
    super(element);
  }
 
  /**
   * Load or reload the image. This method initiates the image reloading. After
   * the image is ready, the repaint event will be scheduled. The current image,
   * if it already exists, will be discarded.
   * 
   * @param itsTime
   *          also load if the "on demand" property is set
   */
  void reloadImage(boolean itsTime)
  {
    URL url = getImageURL();
    if (url == null)
      imageState = (byte) MediaTracker.ERRORED;
    else if (!(loadOnDemand && !itsTime))
      imageIcon = new ImageIcon(url);
    else
      imageState = (byte) MediaTracker.LOADING;
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
    StyleSheet styles = getStyleSheet();
    if (styles == null)
      return super.getAttributes();
    else
      return CombinedAttributes.combine(super.getAttributes(),
                                        styles.getViewAttributes(this));
  }
  
  /**
   * Get the image to render. May return null if the image is not yet loaded.
   */
  public Image getImage()
  {
    if (imageIcon == null)
      return null;
    else
      return imageIcon.getImage();
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
    Object url = getAttributes().getAttribute(Attribute.SRC);
    if (url == null)
      return null;

    try
      {
        return new URL(url.toString());
      }
    catch (MalformedURLException e)
      {
        // The URL is malformed - no image.
        return null;
      }
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
    AttributeSet attrs = getAttributes();
    
    Image image = getImage();

    if (axis == View.X_AXIS)
      {
        Object w = attrs.getAttribute(Attribute.WIDTH);
        if (w != null)
          return Integer.parseInt(w.toString());
        else if (image != null)
          return image.getWidth(getContainer());
        else
          return getNoImageIcon().getIconWidth();
      }
    else if (axis == View.Y_AXIS)
      {
        Object w = attrs.getAttribute(Attribute.HEIGHT);
        if (w != null)
          return Integer.parseInt(w.toString());
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
    Document d = getElement().getDocument();
    if (d instanceof HTMLDocument)
      return ((HTMLDocument) d).getStyleSheet();
    else
      return null;
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
    Rectangle r = bounds.getBounds();

    if (imageIcon == null)

      {
        // Loading image on demand, rendering the loading icon so far.
        reloadImage(true);
         
        // The reloadImage sets the imageIcon, unless the URL is broken 
        // or malformed.
        if (imageIcon != null)
          {
            if (imageIcon.getImageLoadStatus() != MediaTracker.COMPLETE)
              {
                // Render "not ready" icon, unless the image is ready
                // immediately.
                renderIcon(g, r, getLoadingImageIcon());
                // Add the listener to repaint when the icon will be ready.
                imageIcon.setImageObserver(getContainer());
                return;
              }
          }
        else
          {
            renderIcon(g, r, getNoImageIcon());
            return;
          }
      }

    imageState = (byte) imageIcon.getImageLoadStatus();

    switch (imageState)
      {
      case MediaTracker.ABORTED:
      case MediaTracker.ERRORED:
        renderIcon(g, r, getNoImageIcon());
        break;
      case MediaTracker.LOADING:
      // If the image is not loaded completely, we still render it, as the
      // partial image may be available.
      case MediaTracker.COMPLETE:
      {
        // Paint the scaled image.
        Image scaled = imageIcon.getImage().getScaledInstance(
                                                              r.width,
                                                              r.height,
                                                              Image.SCALE_DEFAULT);
        ImageIcon painter = new ImageIcon(scaled);
        painter.paintIcon(getContainer(), g, r.x, r.y);
      }
        break;
      }
  }
  
  /**
   * Render "no image" icon and the alternative "no image" text. The text is
   * rendered right from the icon and is aligned to the icon bottom.
   */
  private void renderIcon(Graphics g, Rectangle bounds, Icon icon)
  {
    Shape current = g.getClip();
    try
      {
        g.setClip(bounds);
        if (icon != null)
          {
            icon.paintIcon(getContainer(), g, bounds.x, bounds.y);
            g.drawString(getAltText(), bounds.x + icon.getIconWidth(),
                         bounds.y + icon.getIconHeight());
          }
      }
    finally
      {
        g.setClip(current);
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
    // In the current implementation, nothing is cached yet, unless the image
    // itself.
    imageIcon = null;
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
    if (imageIcon == null)
      reloadImage(false);
  }  
  

}
