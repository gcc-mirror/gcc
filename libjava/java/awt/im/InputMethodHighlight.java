/* InputMethodHighlight.java -- highlights the current text selection
   Copyright (C) 2002, 2005  Free Software Foundation, Inc.

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

package java.awt.im;

import java.util.Map;

/**
 * This describes the highlight attributes of text composed in an input method.
 * The description includes an abstract level (whether text has been converted
 * yet, and whether it is selected), and a concrete level (which style
 * attributes are used in rendering). If no concrete level is defined, the
 * renderer should use
 * {@link Toolkit#mapInputMethodHighlight(InputMethodHighlight)}. An example
 * of conversion state is kana -&gt; kanji.
 *
 * <p>Instances of this class are typically used in
 * AttributedCharacterIterators, and may be wrapped in Annotations to separate
 * text segments.
 *
 * @author Eric Blake (ebb9@email.byu.edu)
 * @see AttributedCharacterIterators
 * @see Annotation
 * @since 1.2
 * @status updated to 1.4
 */
public class InputMethodHighlight
{
  /** Raw text state (before conversion). */
  public static final int RAW_TEXT = 0;

  /** Converted text state (after conversion). */
  public static final int CONVERTED_TEXT = 1;

  /** Default do-nothing highlighting for unselected raw text. */
  public static final InputMethodHighlight UNSELECTED_RAW_TEXT_HIGHLIGHT
    = new InputMethodHighlight(false, RAW_TEXT);

  /** Default do-nothing highlighting for selected raw text. */
  public static final InputMethodHighlight SELECTED_RAW_TEXT_HIGHLIGHT
    = new InputMethodHighlight(true, RAW_TEXT);

  /** Default do-nothing highlighting for unselected converted text. */
  public static final InputMethodHighlight UNSELECTED_CONVERTED_TEXT_HIGHLIGHT
    = new InputMethodHighlight(false, CONVERTED_TEXT);

  /** Default do-nothing highlighting for selected converted text. */
  public static final InputMethodHighlight SELECTED_CONVERTED_TEXT_HIGHLIGHT
    = new InputMethodHighlight(true, CONVERTED_TEXT);

  /** Whether the highlighting applies to selected text. */
  private final boolean selected;

  /** The state of highlighted text. */
  private final int state;

  /** Any variation on the highlighting style. */
  private final int variation;

  /** The unmodifiable map of rendering styles. */
  private final Map style;

  /**
   * Create an input method highlight style, with variation 0 and null style
   * mapping.
   *
   * @param selected whether the text range is selected
   * @param state either {@link #RAW_TEXT} or {@link #CONVERTED_TEXT}
   * @throws IllegalArgumentException if state is invalid
   */
  public InputMethodHighlight(boolean selected, int state)
  {
    this(selected, state, 0, null);
  }

  /**
   * Create an input method highlight style, with null style mapping.
   *
   * @param selected whether the text range is selected
   * @param state either {@link #RAW_TEXT} or {@link #CONVERTED_TEXT}
   * @param variation the style variation
   * @throws IllegalArgumentException if state is invalid
   */
  public InputMethodHighlight(boolean selected, int state, int variation)
  {
    this(selected, state, variation, null);
  }

  /**
   * Create an input method highlight style.
   *
   * @param selected whether the text range is selected
   * @param state either {@link #RAW_TEXT} or {@link #CONVERTED_TEXT}
   * @param variation the style variation
   * @param style an unmodifiable map of rendering styles, or null
   * @throws IllegalArgumentException if state is invalid
   * @since 1.3
   */
  public InputMethodHighlight(boolean selected, int state, int variation,
                              Map style)
  {
    if (state != RAW_TEXT && state != CONVERTED_TEXT)
      throw new IllegalArgumentException();
    this.selected = selected;
    this.state = state;
    this.variation = variation;
    this.style = style;
  }

  /**
   * Return whether the highlighting applies to selected text.
   *
   * @return the selection status
   */
  public boolean isSelected()
  {
    return selected;
  }

  /**
   * Return the conversion state of the highlighted text.
   *
   * @return one of {@link #RAW_TEXT} or {@link #CONVERTED_TEXT}
   */
  public int getState()
  {
    return state;
  }

  /**
   * Return the highlighting style variation.
   *
   * @return the variation
   */
  public int getVariation()
  {
    return variation;
  }

  /**
   * Return the rendering style attributes map, or null if it should be the
   * default mapping.
   *
   * @return the style map
   * @since 1.3
   */
  public Map getStyle()
  {
    return style;
  }
} // class InputMethodHighlight
