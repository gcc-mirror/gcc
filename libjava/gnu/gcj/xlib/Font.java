/* Copyright (C) 2000  Free Software Foundation

   This file is part of libgcj.

This software is copyrighted work licensed under the terms of the
Libgcj License.  Please consult the file "LIBGCJ_LICENSE" for
details.  */

package gnu.gcj.xlib;

import gnu.gcj.RawData;

/**
 * An X11 Font, implemented as a wrapper around an X11 Font XID and
 * the associated Xlib XFontStruct structure.
 *
 * @author Rolf W. Rasmussen <rolfwr@ii.uib.no>
 */
public final class Font extends XID
{

  /**
   * @param lfdNamePattern a font name pattern following the
   * <em>X Logical Font Description Conventions</em>.
   */
  public Font(Display display, String lfdNamePattern)
  {
    this(display, loadFont(display, lfdNamePattern));
  }

  Font(Display display, RawData struct)
  {
    super(display, getXIDFromStruct(struct));
    structure = struct;
  }

  static native RawData loadFont(Display display, String lfdNamePattern);

  static native int getXIDFromStruct(RawData structure);

  public native int getAscent();
  public native int getDescent();
  public native int getMaxAscent();
  public native int getMaxDescent();

  public native int getStringWidth(String str);

  protected native void finalize();

  RawData structure;
}
