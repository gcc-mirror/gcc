/* Copyright (C) 2000  Free Software Foundation

   This file is part of libgcj.

This software is copyrighted work licensed under the terms of the
Libgcj License.  Please consult the file "LIBGCJ_LICENSE" for
details.  */

package gnu.gcj.xlib;

import gnu.gcj.RawData;

/** 
 * A visual determines how a color is encoded into a pixel/bitfield
 * value.  It does not determine how the pixel/bitfield value is
 * encoded into the image data.
 * 
 * <p>This class encapsulates all three Xlib representations of a
 * visual.
 * 
 * <ul>
 * 
 * <li>int: visual id.
 * 
 * <li>Visual: opaque data structure used by a lot of Xlib functions.
 * 
 * <li>VisualInfo: transparent data structure that binds the visual to
 * a certain screen and depth.
 * 
 * </ul>
 * 
 * <p>Implementation note: This class does not examine nor manipulate
 * the Visual structure, since the X manual says the structure is
 * opaque, and that XVisualInfo should be used instead.</p>
 *
 * @author Rolf W. Rasmussen <rolfwr@ii.uib.no>
 */
public final class Visual
{
  public static final int VC_STATIC_GRAY  = 0,
	                  VC_GRAY_SCALE   = 1,
                          VC_STATIC_COLOR = 2,
                          VC_PSEUDO_COLOR = 3,
                      	  VC_TRUE_COLOR   = 4,
                          VC_DIRECT_COLOR = 5;
  
  protected static final int MASK_ID            = 1 << 0,
                             MASK_SCREEN        = 1 << 1,
                             MASK_DEPTH         = 1 << 2,
                             MASK_CLASS         = 1 << 3,
                             MASK_RED           = 1 << 4,
                             MASK_GREEN         = 1 << 5,
                             MASK_BLUE          = 1 << 6,
                             MASK_COLORMAP_SIZE = 1 << 7,
                             MASK_BITS_PER_RGB  = 1 << 8;

  protected static final int MASK_ALL = MASK_ID
      | MASK_SCREEN
      | MASK_DEPTH
      | MASK_CLASS
      | MASK_RED
      | MASK_GREEN
      | MASK_BLUE
      | MASK_COLORMAP_SIZE
      | MASK_BITS_PER_RGB;

  private static final int MASK_VISUAL_STRUCTURE = 1 << 31;

  Display display;
  RawData xVisualInfo;
  int infoMask;
  Screen screen;

  Visual(RawData structure, Screen screen, int depth )
  {
    this.display = screen.getDisplay();
    this.screen = screen;
    init(structure, depth);
  }

  Visual(Display display, RawData structure, int depth )
  {
    this.display = display;
    init(structure, depth);
  }

  protected native void init(RawData structure, int depth);

  protected native void finalize();

  /**
   *
   * Returns the a reference to the visual structure.  This method has
   * package accessibility since the data visual structure is only
   * useful for direct Xlib calls.
   *
   * @return a pointer to the visual structure.
   */
  native RawData getVisualStructure();

    
  // These methods only make sense if the visual is decomposed:

  public native int getRedMask();
  public native int getGreenMask();
  public native int getBlueMask();

  public native int getScreenNumber();
  public native int getDepth();

  public Screen getScreen()
  {
    if (screen == null)
      screen = new Screen(display, getScreenNumber());
    return screen;
  }

  public native int getVisualClass();

  public boolean hasRGBSubfields()
  {
    switch (getVisualClass())
      {
      case VC_TRUE_COLOR:
      case VC_DIRECT_COLOR:
	return true;
      default:
	return false;
      }
  }

  protected native void ensureXVisualInfo(int requiredMask);


  public String toString()
  {
    int missingInfo = ~infoMask;
    boolean hasSubfieldInfo =
      (missingInfo & (MASK_CLASS|MASK_RED|MASK_GREEN|MASK_BLUE)) == 0;

    boolean hasDepth = (missingInfo & MASK_DEPTH) == 0;
	
    return getClass().getName() + "[" +
      (hasDepth ? "depth=" + getDepth() : "") +
      (hasRGBSubfields() ?
       (", redMask=" + Integer.toHexString(getRedMask()) +
	", greenMask=" + Integer.toHexString(getGreenMask()) +
	", blueMask=" + Integer.toHexString(getBlueMask())) :
       ", no-subfields") + ", class=" + getVisualClass() +
      "]";
  }
}
