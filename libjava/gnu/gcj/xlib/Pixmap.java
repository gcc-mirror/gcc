/* Copyright (C) 2000  Free Software Foundation

   This file is part of libgcj.

This software is copyrighted work licensed under the terms of the
Libgcj License.  Please consult the file "LIBGCJ_LICENSE" for
details.  */

package gnu.gcj.xlib;

/**
 * An X11 Pixmap. A pixmap is an offscreen drawable that resides on
 * the X server. A pixmap is bound to the screen it was created for.
 *
 * @author Rolf W. Rasmussen <rolfwr@ii.uib.no>
 */
public class Pixmap extends Drawable
{
  public Pixmap(XImage image, Screen screen)
  {
    this(screen.getRootWindow(),
	 image.getWidth(), image.getHeight(),
	 image.getDepth());
    
    /* FIXME: don't create a new GC all the time.  This might actually
    not be as bad as initially believed.  The GC cache of Xlib makes
    this operation less costly. */
    GC gc = GC.create (this);
    
    gc.putImage(image, 0, 0, 0, 0, image.getWidth(), image.getHeight());
  }

  public Pixmap(Drawable sameScreenAs, int width, int height, int depth)
  {
    super(sameScreenAs.getDisplay(), 
	  createXID(sameScreenAs, width, height, depth));
  }

  protected static native int createXID(Drawable sameScreenAs,
					int width, int height, int depth);

  protected native void finalize();
}
