/* ImageIcon.java --
   Copyright (C) 2002, 2004, 2005, 2006,  Free Software Foundation, Inc.

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

package javax.swing;

import java.awt.Component;
import java.awt.Graphics;
import java.awt.IllegalComponentStateException;
import java.awt.Image;
import java.awt.MediaTracker;
import java.awt.Toolkit;
import java.awt.image.ImageObserver;
import java.io.Serializable;
import java.net.URL;
import java.util.Locale;

import javax.accessibility.Accessible;
import javax.accessibility.AccessibleContext;
import javax.accessibility.AccessibleIcon;
import javax.accessibility.AccessibleRole;
import javax.accessibility.AccessibleStateSet;

/**
 * An {@link Icon} implementation that is backed by an {@link Image}.
 */
public class ImageIcon
  implements Icon, Serializable, Accessible
{
  /**
   * Provides the accessibility features for the <code>ImageIcon</code>
   * class.
   */
  protected class AccessibleImageIcon
    extends AccessibleContext
    implements AccessibleIcon, Serializable
  {
    private static final long serialVersionUID = 2113430526551336564L;

    /**
     * Creates a new instance of <code>AccessibleImageIcon</code>.
     */
    protected AccessibleImageIcon()
    {
      // Nothing to do here.
    }

    /**
     * Returns the accessible role for the <code>ImageIcon</code>.
     *
     * @return {@link AccessibleRole#ICON}.
     */
    public AccessibleRole getAccessibleRole()
    {
      return AccessibleRole.ICON;
    }

    /**
     * Returns the accessible state for the <code>ImageIcon</code>.  To
     * match the reference implementation, this method always returns 
     * <code>null</code>.
     *
     * @return <code>null</code>.
     */
    public AccessibleStateSet getAccessibleStateSet()
    {
      // refer to Sun's bug report 4269253
      return null;
    }

    /**
     * Returns the accessible parent of this object.  To match the reference
     * implementation, this method always returns <code>null</code>.
     *
     * @return <code>null</code>.
     */
    public Accessible getAccessibleParent()
    {
      // refer to Sun's bug report 4269253
      return null;
    }

    /**
     * Returns the index of this object in its accessible parent.  To match
     * the reference implementation, this method always returns <code>-1</code>.
     *
     * @return <code>-1</code>.
     */
    public int getAccessibleIndexInParent()
    {
      // refer to Sun's bug report 4269253
      return -1;
    }

    /**
     * Returns the number of accessible children of this component,
     * which is 0, because an {@link ImageIcon} has no children.
     *
     * @return <code>0</code>.
     */
    public int getAccessibleChildrenCount()
    {
      return 0;
    }

    /**
     * Returns the accessible child at index <code>i</code>, which is
     * <code>null</code> in this case because an {@link ImageIcon} has no 
     * children.
     *
     * @param i the index of the child to be fetched
     *
     * @return <code>null</code>.
     */
    public Accessible getAccessibleChild(int i)
    {
      return null;
    }

    /**
     * Returns the locale of this object.  To match the reference 
     * implementation, this method always returns <code>null</code>.
     *
     * @return <code>null</code>.
     */
    public Locale getLocale() 
      throws IllegalComponentStateException
    {
      // refer to Sun's bug report 4269253
      return null;
    }

    /**
     * Returns the accessible icon description.  This returns the
     * <code>description</code> property of the underlying {@link ImageIcon}.
     *
     * @return The description (possibly <code>null</code>).
     * 
     * @see #setAccessibleIconDescription(String)
     */
    public String getAccessibleIconDescription()
    {
      return getDescription();
    }

    /**
     * Sets the accessible icon description.  This sets the 
     * <code>description</code> property of the underlying {@link ImageIcon}.
     *
     * @param newDescr the description (<code>null</code> permitted).
     * 
     * @see #getAccessibleIconDescription()
     */
    public void setAccessibleIconDescription(String newDescr)
    {
      setDescription(newDescr);
    }

    /**
     * Returns the icon height. This returns the <code>iconHeight</code> 
     * property of the underlying {@link ImageIcon}.
     *
     * @return The icon height.
     */
    public int getAccessibleIconHeight()
    {
      return getIconHeight();
    }
    
    /**
     * Returns the icon width. This returns the <code>iconWidth</code> property 
     * of the underlying {@link ImageIcon}.
     *
     * @return The icon width.
     */
    public int getAccessibleIconWidth()
    {
      return getIconWidth();
    }
  } // AccessibleIcon

  private static final long serialVersionUID = 532615968316031794L;

  /** A dummy Component that is used in the MediaTracker. */
  protected static final Component component = new Component()
  {
    // No need to implement this. 
  };

  /** The MediaTracker used to monitor the loading of images. */
  protected static final MediaTracker tracker = new MediaTracker(component);

  /** The ID that is used in the tracker. */
  private static int id;

  Image image;
  String description;
  ImageObserver observer;

  /** The image loading status. */
  private int loadStatus;

  /** The AccessibleContext of this ImageIcon. */
  private AccessibleContext accessibleContext;

  /**
   * Creates an ImageIcon without any properties set.
   */
  public ImageIcon()
  {
    // Nothing to do here.
  }
 
  /**
   * Constructs an ImageIcon given a filename.  The icon's description
   * is initially set to the filename itself.  A filename of "" means
   * create a blank icon.
   *
   * @param filename name of file to load or "" for a blank icon
   */
  public ImageIcon(String filename)
  {
    this(filename, filename);
  }

  /**
   * Constructs an ImageIcon from the given filename, setting its
   * description to the given description.  A filename of "" means
   * create a blank icon.
   *
   * @param filename name of file to load or "" for a blank icon
   * @param description human-readable description of this icon
   */
  public ImageIcon(String filename, String description)
  {
    this(Toolkit.getDefaultToolkit().getImage(filename), description);
  }

  /**
   * Creates an ImageIcon from the given byte array without any
   * description set.
   */
  public ImageIcon(byte[] imageData)
  {
    this(imageData, null);
  }
  
  /**
   * Creates an ImageIcon from the given byte array and sets the given
   * description.
   */
  public ImageIcon(byte[] imageData, String description)
  {
    this(Toolkit.getDefaultToolkit().createImage(imageData), description);
  }

  /**
   * Creates an ImageIcon from the given URL and sets the description
   * to the URL String representation.
   */
  public ImageIcon(URL url)
  {
    this(url, url.toString());
  }

  /**
   * Creates an ImageIcon from the given URL and sets the given
   * description.
   */
  public ImageIcon(URL url, String description)
  {
    this(Toolkit.getDefaultToolkit().getImage(url), description);
  }

  /**
   * Creates an ImageIcon from the given Image without any description
   * set.
   */
  public ImageIcon(Image image)
  {
    this(image, null);
  }

  /**
   * Creates an ImageIcon from the given Image and sets the given
   * description.
   */
  public ImageIcon(Image image, String description)
  {
    setImage(image);
    setDescription(description);
  }

  /**
   * Returns the ImageObserver that is used for all Image
   * operations. Defaults to null when not explicitly set.
   */
  public ImageObserver getImageObserver()
  {
    return observer;
  }
  
  /**
   * Sets the ImageObserver that will be used for all Image
   * operations. Can be set to null (the default) when no observer is
   * needed.
   */
  public void setImageObserver(ImageObserver newObserver)
  {
    observer = newObserver;
  }

  /**
   * Returns the backing Image for this ImageIcon. Might be set to
   * null in which case no image is shown.
   */
  public Image getImage()
  {
    return image;
  }

  /**
   * Explicitly sets the backing Image for this ImageIcon. Will call
   * loadImage() to make sure that the Image is completely loaded
   * before returning.
   */
  public void setImage(Image image)
  {
    loadImage(image);
    this.image = image;
  }

  /**
   * Returns a human readable description for this ImageIcon or null
   * when no description is set or available.
   */
  public String getDescription()
  {
    return description;
  }

  /**
   * Sets a human readable description for this ImageIcon. Can be set
   * to null when no description is available.
   */
  public void setDescription(String description)
  {
    this.description = description;
  }

  /**
   * Returns the the height of the backing Image, or -1 if the backing
   * Image is null. The getHeight() method of the Image will be called
   * with the set observer of this ImageIcon.
   */
  public int getIconHeight()
  {
    if (image == null)
      return -1;

    return image.getHeight(observer);
  }

  /**
   * Returns the the width of the backing Image, or -1 if the backing
   * Image is null. The getWidth() method of the Image will be called
   * with the set observer of this ImageIcon.
   */
  public int getIconWidth()
  {
    if (image == null)
      return -1;

    return image.getWidth(observer);
  }

  /**
   * Calls <code>g.drawImage()</code> on the backing Image using the
   * set observer of this ImageIcon. If the set observer is null, the
   * given Component is used as observer.
   */
  public void paintIcon(Component c, Graphics g, int x, int y)
  {
    g.drawImage(image, x, y, observer != null ? observer : c);
  }

  /**
   * Loads the image and blocks until the loading operation is finished.
   *
   * @param image the image to be loaded
   */
  protected void loadImage(Image image)
  {
    try
      {
        tracker.addImage(image, id);
        id++;
        tracker.waitForID(id - 1);
      }
    catch (InterruptedException ex)
      {
        // Ignore this for now.
      }
    finally
      {
        loadStatus = tracker.statusID(id - 1, false);
        tracker.removeImage(image, id - 1);
      }
  }

  /**
   * Returns the load status of the icon image.
   *
   * @return the load status of the icon image
   *
   * @see MediaTracker#COMPLETE
   * @see MediaTracker#ABORTED
   * @see MediaTracker#ERRORED
   */
  public int getImageLoadStatus()
  {
    return loadStatus;
  }

  /**
   * Returns the object that provides accessibility features for this
   * <code>ImageIcon</code> instance.
   *
   * @return The accessible context (an instance of 
   *     {@link AccessibleImageIcon}).
   */
  public AccessibleContext getAccessibleContext()
  {
    if (accessibleContext == null)
      accessibleContext = new AccessibleImageIcon();
    return accessibleContext;
  }
}
