/* SystemColor.java -- Class to access system color values.
   Copyright (C) 1999 Free Software Foundation, Inc.

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
Free Software Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA
02111-1307 USA.

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


package java.awt;

/**
  * This class contains the various "system colors" in use by the
  * native windowing system.
  *
  * @author Aaron M. Renn (arenn@urbanophile.com)
  */
public final class SystemColor extends Color implements java.io.Serializable
{

/*
 * Static Variables
 */

/**
  * Array index of the desktop color.  Used by 
  * <code>Toolkit.loadSystemColors()</code>.
  */
public static final int DESKTOP = 0;

/**
  * Array index of the active caption color.  Used by 
  * <code>Toolkit.loadSystemColors()</code>.
  */
public static final int ACTIVE_CAPTION = 1;

/**
  * Array index of the active caption text color.  Used by 
  * <code>Toolkit.loadSystemColors()</code>.
  */
public static final int ACTIVE_CAPTION_TEXT = 2;

/**
  * Array index of the active caption border color.  Used by 
  * <code>Toolkit.loadSystemColors()</code>.
  */
public static final int ACTIVE_CAPTION_BORDER = 3;

/**
  * Array index of the inactive caption color.  Used by 
  * <code>Toolkit.loadSystemColors()</code>.
  */
public static final int INACTIVE_CAPTION = 4;

/**
  * Array index of the inactive caption text color.  Used by 
  * <code>Toolkit.loadSystemColors()</code>.
  */
public static final int INACTIVE_CAPTION_TEXT = 5;

/**
  * Array index of the inactive caption border color.  Used by 
  * <code>Toolkit.loadSystemColors()</code>.
  */
public static final int INACTIVE_CAPTION_BORDER = 6;

/**
  * Array index of the window background color.  Used by 
  * <code>Toolkit.loadSystemColors()</code>.
  */
public static final int WINDOW = 7;

/**
  * Array index of the window border color.  Used by 
  * <code>Toolkit.loadSystemColors()</code>.
  */
public static final int WINDOW_BORDER = 8;

/**
  * Array index of the window text color.  Used by 
  * <code>Toolkit.loadSystemColors()</code>.
  */
public static final int WINDOW_TEXT = 9;

/**
  * Array index of the menu background color.  Used by 
  * <code>Toolkit.loadSystemColors()</code>.
  */
public static final int MENU = 10;

/**
  * Array index of the menu text color.  Used by 
  * <code>Toolkit.loadSystemColors()</code>.
  */
public static final int MENU_TEXT = 11;

/**
  * Array index of the text background color.  Used by 
  * <code>Toolkit.loadSystemColors()</code>.
  */
public static final int TEXT = 12;

/**
  * Array index of the text foreground color.  Used by 
  * <code>Toolkit.loadSystemColors()</code>.
  */
public static final int TEXT_TEXT = 13;

/**
  * Array index of the highlighted text background color.  Used by 
  * <code>Toolkit.loadSystemColors()</code>.
  */
public static final int TEXT_HIGHLIGHT = 14;

/**
  * Array index of the highlighted text foreground color.  Used by 
  * <code>Toolkit.loadSystemColors()</code>.
  */
public static final int TEXT_HIGHLIGHT_TEXT = 15;

/**
  * Array index of the inactive text foreground color.  Used by 
  * <code>Toolkit.loadSystemColors()</code>.
  */
public static final int TEXT_INACTIVE_TEXT = 16;

/**
  * Array index of the control background color.  Used by 
  * <code>Toolkit.loadSystemColors()</code>.
  */
public static final int CONTROL = 17;

/**
  * Array index of the control text color.  Used by 
  * <code>Toolkit.loadSystemColors()</code>.
  */
public static final int CONTROL_TEXT = 18;

/**
  * Array index of the highlighted control background color.  Used by 
  * <code>Toolkit.loadSystemColors()</code>.
  */
public static final int CONTROL_HIGHLIGHT = 19;

/**
  * Array index of the lightly highlighted control background color.  Used by 
  * <code>Toolkit.loadSystemColors()</code>.
  */
public static final int CONTROL_LT_HIGHLIGHT = 20;

/**
  * Array index of the shadowed control background color.  Used by 
  * <code>Toolkit.loadSystemColors()</code>.
  */
public static final int CONTROL_SHADOW = 21;

/**
  * Array index of the darkly shadowed control background color.  Used by 
  * <code>Toolkit.loadSystemColors()</code>.
  */
public static final int CONTROL_DK_SHADOW = 22;

/**
  * Array index of the scrollbar background color.  Used by 
  * <code>Toolkit.loadSystemColors()</code>.
  */
public static final int SCROLLBAR = 23;

/**
  * Array index of the info background color.  Used by 
  * <code>Toolkit.loadSystemColors()</code>.
  */
public static final int INFO = 24;

/**
  * Array index of the info text color.  Used by 
  * <code>Toolkit.loadSystemColors()</code>.
  */
public static final int INFO_TEXT = 25;

/**
  * The number of system colors. Used by
  * <code>Toolkit.loadSystemColors()</code>.
  */
public static final int NUM_COLORS = 26;

/**
  * The desktop color.
  */
public static final SystemColor desktop;

/**
  * The active caption background color.
  */
public static final SystemColor activeCaption;

/**
  * The active caption text color.
  */
public static final SystemColor activeCaptionText;

/**
  * The active caption border color.
  */
public static final SystemColor activeCaptionBorder;

/**
  * The inactive caption background color.
  */
public static final SystemColor inactiveCaption;

/**
  * The inactive caption text color.
  */
public static final SystemColor inactiveCaptionText;

/**
  * The inactive caption border color.
  */
public static final SystemColor inactiveCaptionBorder;

/**
  * The window background color.
  */
public static final SystemColor window;

/**
  * The window border color.
  */
public static final SystemColor windowBorder;

/**
  * The window text color.
  */
public static final SystemColor windowText;

/**
  * The menu background color.
  */
public static final SystemColor menu;

/**
  * The menu text color.
  */
public static final SystemColor menuText;

/**
  * The text background color.
  */
public static final SystemColor text;

/**
  * The text foreground color.
  */
public static final SystemColor textText;

/**
  * The highlighted text background color.
  */
public static final SystemColor textHighlight;

/**
  * The highlighted text foreground color.
  */
public static final SystemColor textHighlightText;

/**
  * The inactive text color.
  */
public static final SystemColor textInactiveText;

/**
  * The control background color.
  */
public static final SystemColor control;

/**
  * The control text color.
  */
public static final SystemColor controlText;

/**
  * The control highlight color.
  */
public static final SystemColor controlHighlight;

/**
  * The control light highlight color.
  */
public static final SystemColor controlLtHighlight; 

/**
  * The control shadow color.
  */
public static final SystemColor controlShadow;

/**
  * The control dark shadow color.
  */
public static final SystemColor controlDkShadow;

/**
  * The scrollbar color.
  */
public static final SystemColor scrollbar;

/**
  * The info text background color.
  */
public static final SystemColor info;

/**
  * The info text foreground color.
  */
public static final SystemColor infoText;

// Serialization version constant
private static final long serialVersionUID = 4503142729533789064L;

static
{
  int[] sys_color_rgbs = new int[NUM_COLORS];
  Toolkit.getDefaultToolkit().loadSystemColors(sys_color_rgbs);

  desktop = new SystemColor(sys_color_rgbs[DESKTOP]);
  activeCaption= new SystemColor(sys_color_rgbs[ACTIVE_CAPTION]);
  activeCaptionText= new SystemColor(sys_color_rgbs[ACTIVE_CAPTION_TEXT]);
  activeCaptionBorder = new SystemColor(sys_color_rgbs[ACTIVE_CAPTION_BORDER]);
  inactiveCaption = new SystemColor(sys_color_rgbs[INACTIVE_CAPTION]);
  inactiveCaptionText = new SystemColor(sys_color_rgbs[INACTIVE_CAPTION_TEXT]);
  inactiveCaptionBorder = 
    new SystemColor(sys_color_rgbs[INACTIVE_CAPTION_BORDER]);
  window = new SystemColor(sys_color_rgbs[WINDOW]);
  windowBorder = new SystemColor(sys_color_rgbs[WINDOW_BORDER]);
  windowText = new SystemColor(sys_color_rgbs[WINDOW_TEXT]);
  menu = new SystemColor(sys_color_rgbs[MENU]);
  menuText = new SystemColor(sys_color_rgbs[MENU_TEXT]);
  text = new SystemColor(sys_color_rgbs[TEXT]);
  textText = new SystemColor(sys_color_rgbs[TEXT_TEXT]);
  textHighlight = new SystemColor(sys_color_rgbs[TEXT_HIGHLIGHT]);
  textHighlightText = new SystemColor(sys_color_rgbs[TEXT_HIGHLIGHT_TEXT]);
  textInactiveText = new SystemColor(sys_color_rgbs[TEXT_INACTIVE_TEXT]);
  control = new SystemColor(sys_color_rgbs[CONTROL]);
  controlText = new SystemColor(sys_color_rgbs[CONTROL_TEXT]);
  controlHighlight = new SystemColor(sys_color_rgbs[CONTROL_HIGHLIGHT]);
  controlLtHighlight = new SystemColor(sys_color_rgbs[CONTROL_LT_HIGHLIGHT]);
  controlShadow = new SystemColor(sys_color_rgbs[CONTROL_SHADOW]);
  controlDkShadow = new SystemColor(sys_color_rgbs[CONTROL_DK_SHADOW]);
  scrollbar = new SystemColor(sys_color_rgbs[SCROLLBAR]);
  info = new SystemColor(sys_color_rgbs[INFO]);
  infoText = new SystemColor(sys_color_rgbs[INFO_TEXT]);
}

/*************************************************************************/

/*
 * Constructors
 */

private
SystemColor(int rgb)
{
  super(rgb);
}

/*************************************************************************/

/*
 * Instance Methods
 */

/**
  * Returns the RGB value for this color as an <code>int</code>.  The first
  * byte is the blue value, the second the green value, the third the
  * red value and the fourth is set to 0xFF.
  *
  * @return The RGB value.
  */
public int
getRGB()
{
  // Override only to be spec consistent.
  return(super.getRGB());
}

/*************************************************************************/

/**
  * Returns a string describing this color.
  *
  * @return A string describing this color.
  */
public String
toString()
{
  return("SystemColor(R=" + getRed() + ",G=" + getGreen() + ",B=" +
         getBlue() + ")");
}

} // class SystemColor 

