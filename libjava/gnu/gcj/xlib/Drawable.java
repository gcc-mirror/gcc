/* Copyright (C) 1999, 2000  Free Software Foundation

   This file is part of libgcj.

This software is copyrighted work licensed under the terms of the
Libgcj License.  Please consult the file "LIBGCJ_LICENSE" for
details.  */

package gnu.gcj.xlib;

import java.awt.Rectangle;

/** An X11 drawable.
 *
 * @author Rolf W. Rasmussen <rolfwr@ii.uib.no>
 */
public class Drawable extends XID
{
  private GC[] gcCache = new GC[10];
  private int gcCachedCount = 0;

  public Drawable(Display display, int xid)
  {
    super(display, xid);
  }

  /**
   * Gets as much as possible of the image data within the requested
   * region. Data from obscured parts of windows may not be
   * retrievable.
   *  
   * @param dest where to place the image data.
   * 
   * @return the actual region of image data that was retrieved.
   */
  public Rectangle copyIntoXImage(XImage dest, Rectangle bounds,
				  int destX, int destY)
  {
    Rectangle newBounds = null;
    int tries = 5;
    while (!bounds.isEmpty())
      {
	if (copyIntoXImageImpl(dest, bounds.x, bounds.y,
			       bounds.width, bounds.height,
			       destX, destY))
	  return bounds;
	    
	// failed, likely due to wrong bounds...
	
	newBounds = getBounds(newBounds);
	
	bounds = newBounds.intersection(bounds);
	
	tries--;
	
	if (tries < 0)
	throw new RuntimeException("copyIntoXImage is buggy");
	
      }
    
    return bounds; // always empty
  }



  /**
   * Performs an XGetSubImage. This method will fail if the X server
   * does not possess the requested image data. This might occur when
   * requesting the image date of a window that is partially obscured.
   *
   * @param desitantionImage where to place the image data
   *
   * @return false if method was unable to read the requested region.
   */
  private native boolean copyIntoXImageImpl(XImage destinationImage,
					    int x, int y,
					    int width, int height,
					    int destX, int destY);

  public native Rectangle getBounds(Rectangle rv);
  
  public native int getDepth ();
  
  private static final String MSG_XGETSUBIMAGE_FAILED =
    "XGetSubImage() failed.";

  protected void finalize() throws Throwable
  {
    // Dispose all the cached GCs, to reduce X server resource usage
    for (int i=0; i<gcCachedCount; i++)
      gcCache[i].disposeImpl ();
    gcCachedCount = 0;
    super.finalize();
  }

  /** Put a GC in the cache for this drawable, so it can be retrieved later.
   * @param gc The GC to put
   */
  void putGCInCache (GC gc)
  {
    if (gcCachedCount >= gcCache.length)
    {
      // List full - extend it to double its present size
      GC[] oldList = gcCache;
      gcCache = new GC[oldList.length*2];
      System.arraycopy (oldList,0,gcCache,0,oldList.length);
    }
    gcCache[gcCachedCount++] = gc;
  }

  /** Get a GC from the cache, if available
   * @return A GC from the cache, or null if the cache is empty
   */
  GC getGCFromCache ()
  {
    return (gcCachedCount>0) ? gcCache[--gcCachedCount] : null;
  }
}
