/* Copyright (C) 2000  Free Software Foundation

   This file is part of libgcj.

This software is copyrighted work licensed under the terms of the
Libgcj License.  Please consult the file "LIBGCJ_LICENSE" for
details.  */

package java.awt;

/** This class contains most of the colors used by the native
 * windowing sytem to draw native components.
 * @author Tom Tromey <tromey@redhat.com>
 * @date December 25, 2000
 */
public final class SystemColor extends Color implements java.io.Serializable
{
  /** The background color for the title bar of an active window.  */
  public static final SystemColor activeCaption
      = new SystemColor (ACTIVE_CAPTION);
  /** The border color of an active window.  */
  public static final SystemColor activeCaptionBorder
      = new SystemColor (ACTIVE_CAPTION_BORDER);
  /** The color of text in the title bar of an active window.  */
  public static final SystemColor activeCaptionText
      = new SystemColor (ACTIVE_CAPTION_TEXT);
  /** The background color.  */
  public static final SystemColor control = new SystemColor (CONTROL);
  /** The darkest color of an outline.  */
  public static final SystemColor controlDkShadow
      = new SystemColor (CONTROL_DK_SHADOW);
  /** The second brightest color of an outline.  */
  public static final SystemColor controlHighlight
      = new SystemColor (CONTROL_HIGHLIGHT);
  /** The brightest color of an outline.  */
  public static final SystemColor controlLtHighlight
      = new SystemColor (CONTROL_LT_HIGHLIGHT);
  /** The second darkest color of an outline.  */
  public static final SystemColor controlShadow
      = new SystemColor (CONTROL_SHADOW);
  /** The color of text in a label.  */
  public static final SystemColor controlText = new SystemColor (CONTROL_TEXT);
  /** The background color of the desktop.  */
  public static final SystemColor desktop = new SystemColor (DESKTOP);
  /** The background color for the title bar of an inactive window.  */
  public static final SystemColor inactiveCaption
      = new SystemColor (INACTIVE_CAPTION);
  /** The border color of an inactive window.  */
  public static final SystemColor inactiveCaptionBorder
      = new SystemColor (INACTIVE_CAPTION_BORDER);
  /** The color of text in the title ar of an inactive window.  */
  public static final SystemColor inactiveCaptionText
      = new SystemColor (INACTIVE_CAPTION_TEXT);
  /** The background color of tooltips. */
  public static final SystemColor info = new SystemColor (INFO);
  /** The color of text in tooltips.  */
  public static final SystemColor infoText = new SystemColor (INFO_TEXT);
  /** The background color of a menu.  */
  public static final SystemColor menu = new SystemColor (MENU);
  /** The color of text in a menu.  */
  public static final SystemColor menuText = new SystemColor (MENU_TEXT);
  /** The background color of a scrollbar.  */
  public static final SystemColor scrollbar = new SystemColor (SCROLLBAR);
  /** The background color of text components.  */
  public static final SystemColor text = new SystemColor (TEXT);
  /** The background color of highlighted text.  */
  public static final SystemColor textHighlight
      = new SystemColor (TEXT_HIGHLIGHT);
  /** The color of highlighted text.  */
  public static final SystemColor textHighlightText
      = new SystemColor (TEXT_HIGHLIGHT_TEXT);
  /** The color of inactive text.  */
  public static final SystemColor textInactiveText
      = new SystemColor (TEXT_INACTIVE_TEXT);
  /** The color of text in text components.  */
  public static final SystemColor textText = new SystemColor (TEXT_TEXT);
  /** The background color of a window.  */
  public static final SystemColor window = new SystemColor (WINDOW);
  /** The border color of a window.  */
  public static final SystemColor windowBorder
      = new SystemColor (WINDOW_BORDER);
  /** The color of text in a window.  */
  public static final SystemColor windowText = new SystemColor (WINDOW_TEXT);

  public static final int DESKTOP = 0;
  public static final int ACTIVE_CAPTION = 1;
  public static final int ACTIVE_CAPTION_TEXT = 2;
  public static final int ACTIVE_CAPTION_BORDER = 3;
  public static final int INACTIVE_CAPTION = 4;
  public static final int INACTIVE_CAPTION_TEXT = 5;
  public static final int INACTIVE_CAPTION_BORDER = 6;
  public static final int WINDOW = 7;
  public static final int WINDOW_BORDER = 8;
  public static final int WINDOW_TEXT = 9;
  public static final int MENU = 10;
  public static final int MENU_TEXT = 11;
  public static final int TEXT = 12;
  public static final int TEXT_TEXT = 13;
  public static final int TEXT_HIGHLIGHT = 14;
  public static final int TEXT_HIGHLIGHT_TEXT = 15;
  public static final int TEXT_INACTIVE_TEXT = 16;
  public static final int CONTROL = 17;
  public static final int CONTROL_TEXT = 18;
  public static final int CONTROL_HIGHLIGHT = 19;
  public static final int CONTROL_LT_HIGHLIGHT = 20;
  public static final int CONTROL_SHADOW = 21;
  public static final int CONTROL_DK_SHADOW = 22;
  public static final int SCROLLBAR = 23;
  public static final int INFO = 24;
  public static final int INFO_TEXT = 25;

  public static final int NUM_COLORS = 26;

  private static final int rgbs[] =
  {
    0x005c5c,
    0x000080,
    0xffffff,
    0xc0c0c0,
    0x808080,
    0xc0c0c0,
    0xc0c0c0,
    0xffffff,
    0x000000,
    0x000000,
    0xc0c0c0,
    0x000000,
    0xc0c0c0,
    0x000000,
    0x000080,
    0xffffff,
    0x808080,
    0xc0c0c0,
    0x000000,
    0xffffff,
    0xe0e0e0,
    0x808080,
    0x000000,
    0xe0e0e0,
    0xe0e000,
    0x000000
  };

  public int getRGB ()
  {
    return rgbs[rgba];
  }

  public String toString ()
  {
    return "[" + getClass ().getName () + " " + rgba + "]";
  }

  private SystemColor (int index)
  {
    super (index, true);
  }
}
