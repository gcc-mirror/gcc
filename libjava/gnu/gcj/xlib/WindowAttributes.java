/* Copyright (C) 2000  Free Software Foundation

   This file is part of libgcj.

This software is copyrighted work licensed under the terms of the
Libgcj License.  Please consult the file "LIBGCJ_LICENSE" for
details.  */

package gnu.gcj.xlib;
import gnu.gcj.RawData;

/**
 * 
 * Collection of attributes that can be applied to or read from an
 * X11 window.
 *
 * <p>TODO: Split this class into two classes. One for the structure
 * XSetWindowAttributes and one for the XWindowAttributes.  However
 * they should still share this common base class.
 *
 * @author Rolf W. Rasmussen <rolfwr@ii.uib.no> */
public class WindowAttributes
{
  // Must match definitions in X.h:
  public final static long MASK_BUTTON_PRESS     = 1L<< 2,
                           MASK_BUTTON_RELEASE   = 1L<< 3,
                           MASK_EXPOSURE         = 1L<<15,
                           MASK_STRUCTURE_NOTIFY = 1L<<17;

  public WindowAttributes()
  {
    init(null);
  }
  
  public WindowAttributes(Window from)
  {
    initFromWindow(from);
  }

  private native void initFromWindow(Window from);
  private native void init(WindowAttributes copyFrom);
  protected native void finalize();
    
  public Object clone()
  {
    WindowAttributes attributes = (WindowAttributes) super.clone();
    // In case of an exception before the stucture is copied.
    attributes.in  = null;
    attributes.out = null;
    
    // FIXME: do anything else?
	
    attributes.init(this);
    return attributes;
  }

  public native void setBackground(long pixel);
  public native void setBackground(Pixmap pixmap);
  public native void setEventMask(long eventMask);

  public void setVisual(Visual visual)
  {
    this.visual = visual;
  }

  /**
   * Retrieve the visual. 
   *
   * @return the visual that is or should be used by a window.  null
   * means CopyFormParent. 
   */
  public native Visual getVisual();

  Display display;

  /**
   * Reference to XWindowAttribute structure containing attributes
   * read from a window.
   */
  RawData in = null;

  /**
   * Reference to XSetWindowAttribute structure containing attributes
   * to be applied to a window.
   */
  RawData out = null;

  long mask;

  /** null means CopyFromParent during window creation. */
  Visual visual = null;

  public native void apply(Window window);

  final RawData getXSetWindowAttributesStructure()
  {
    if (out == null)
      initOut();
    return out;
  }
  
  void initOut()
  {
    throw new UnsupportedOperationException("not implemented yet");
  }
}
