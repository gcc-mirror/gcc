/* Copyright (C) 2000, 2003  Free Software Foundation

   This file is part of libgcj.

This software is copyrighted work licensed under the terms of the
Libgcj License.  Please consult the file "LIBGCJ_LICENSE" for
details.  */

package gnu.gcj.xlib;

import gnu.gcj.RawData;
import java.awt.Rectangle;

/**
 * An X11 graphics context.  Unlike a traditional X11 graphics
 * context, the target drawable is part of the GC state.
 *
 * Implementation notes: There is no need to do coalescing of changes
 * since Xlib will do this for us.  The implementation relies on the
 * Xlib GC cache and will not try to be clever.
 *
 * @author Rolf W. Rasmussen <rolfwr@ii.uib.no>
 */
public class GC implements Cloneable
{
  /** Protected constructor, because GC.create(target) should be used instead.
   */
  protected GC(Drawable target)
  {
    this.target = target;
    initStructure(null);
  }

  /** Try to get a suitable GC from the drawable's cache.
   * If there isn't one, create one.
   */
  public Object clone()
  {
    try
      {
	GC gcClone = target.getGCFromCache ();
	if (gcClone==null)
	  {
	    gcClone = (GC) super.clone();
	    gcClone.structure = null;
	  }
	gcClone.initStructure(this);
	gcClone.updateClip(clipRectangles);
	return gcClone;
      } 
    catch (CloneNotSupportedException ex)
      {
	// This should never happen.
	throw new InternalError ();
      }
  }

  private native void initStructure(GC copyFrom);

  public GC create()
  {
    return (GC) clone();
  }
  
  /** Create a GC, or if one is already cached for target, return that.
   * @param target The Drawable for which a GC is needed
   * @return The new or retrieved GC
   */
  static public GC create (Drawable target)
  {
    GC returnValue = target.getGCFromCache ();
    if (returnValue == null)
      returnValue = new GC (target);
    return returnValue;
  }

  public void finalize()
  {
    disposeImpl();
  }

  /** Save this GC in the drawable's cache.
   *  The "real" dispose (disposeImpl) is called when the
   *  drawable is finialized, to free X server resources.
   */
  public void dispose()
  {
    target.putGCInCache (this);
  }

  public synchronized native void disposeImpl();

  public native void setForeground(long pixel);
  public native void setFont(gnu.gcj.xlib.Font font);

  /**
   * Set the clip region for the graphics operations performed by the
   * GC.
   *
   * This is one of the few costly operations of this class.  It is
   * suggested that the clip is only set or changed if really
   * necessary.  Higher level APIs can make such optimizations
   * transparent.
   *
   * @param rectangles the union of these rectangles describe the clip
   * region.
   */
  public void setClipRectangles(Rectangle[] rectangles)
  {
    clipRectangles = rectangles;
    updateClip(clipRectangles);
  }

  public native void drawString(String text, int x, int y);
  public native void drawLine(int x1, int y1, int x2, int y2);
  public native void drawRectangle(int x, int y, int w, int h);

  public native void fillRectangle(int x, int y, int w, int h);
  public native void fillPolygon(int[] xPoints, int[] yPoints, int nPoints,
				 int translateX, int translateY);
  
  public native void drawArc(int x, int y, int w, int h,
			     int startAngle, int arcAngle);
  public native void fillArc(int x, int y, int w, int h,
			     int startAngle, int arcAngle);

  /** 
   * 
   * Clear area using the background pixel or pixmap of the drawable.
   * Note that this operation does not adhere to the current clip.
   */
  public native void clearArea(int x, int y, int w, int h,
			       boolean exposures);


  public native void putImage(XImage image,
			      int srcX, int srcY,
			      int destX, int destY,
			      int width, int height);

  public native void copyArea (Drawable source,
                               int srcX, int srcY,
                               int destX, int destY,
                               int width, int height);
  
  public Drawable getDrawable()
  {
    return target;
  }

  private native void updateClip(Rectangle[] rectangles);

  private Drawable target;
  private RawData structure;
  private Rectangle[] clipRectangles;
}

