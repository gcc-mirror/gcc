/* TextAttribute.java --
   Copyright (C) 2003, 2004, 2005  Free Software Foundation, Inc.

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


package java.awt.font;

import java.io.InvalidObjectException;
import java.text.AttributedCharacterIterator;

/**
 * Attributes (and associated values) that can be used to define an
 * {@link java.text.AttributedString}.
 */
public final class TextAttribute extends AttributedCharacterIterator.Attribute
{
  private static final long serialVersionUID = 7744112784117861702L;
  
  /** A key for the background paint attribute. */
  public static final TextAttribute BACKGROUND =
    new TextAttribute("background");
  
  /** A key for the BIDI_EMBEDDING attribute. */
  public static final TextAttribute BIDI_EMBEDDING =
    new TextAttribute("bidi_embedding");
  
  /** A key for the CHAR_REPLACEMENT attribute. */
  public static final TextAttribute CHAR_REPLACEMENT =
    new TextAttribute("char_replacement");
  
  /** A key for the FAMILY attribute. */
  public static final TextAttribute FAMILY = new TextAttribute("family");
  
  /** A key for the font attribute. */
  public static final TextAttribute FONT = new TextAttribute("font");
  
  /** A key for the foreground paint attribute. */
  public static final TextAttribute FOREGROUND = 
    new TextAttribute("foreground");
  
  /** A key for the INPUT_METHOD_HIGHLIGHT attribute. */
  public static final TextAttribute INPUT_METHOD_HIGHLIGHT =
    new TextAttribute("input method highlight");
  
  /** A key for the INPUT_METHOD_UNDERLINE attribute. */
  public static final TextAttribute INPUT_METHOD_UNDERLINE =
    new TextAttribute("input method underline");
  
  /** A key for the text justification attribute. */
  public static final TextAttribute JUSTIFICATION =
    new TextAttribute("justification");
  
  /** 
   * A value that can be used with the {@link #JUSTIFICATION} attribute to
   * indicate full justification of the text. 
   */
  public static final Float JUSTIFICATION_FULL = new Float(1.0);
  
  /** 
   * A value that can be used with the {@link #JUSTIFICATION} attribute to
   * indicate no justification of the text. 
   */
  public static final Float JUSTIFICATION_NONE = new Float(0.0);
  
  /** A key for the NUMERIC_SHAPING attribute. */
  public static final TextAttribute NUMERIC_SHAPING =
    new TextAttribute("numeric_shaping");
  
  /** A key for the POSTURE attribute. */
  public static final TextAttribute POSTURE = new TextAttribute("posture");
  
  /** A value that can be used with the {@link #POSTURE} attribute. */
  public static final Float POSTURE_OBLIQUE = new Float(0.2);
  
  /** A value that can be used with the {@link #POSTURE} attribute. */
  public static final Float POSTURE_REGULAR = new Float(0.0);
  
  /** A key for the RUN_DIRECTION attribute. */
  public static final TextAttribute RUN_DIRECTION =
    new TextAttribute("run_direction");
  
  /** A value that can be used with the {@link #RUN_DIRECTION} attribute. */
  public static final Boolean RUN_DIRECTION_LTR = Boolean.FALSE;
  
  /** A value that can be used with the {@link #RUN_DIRECTION} attribute. */
  public static final Boolean RUN_DIRECTION_RTL = Boolean.TRUE;
  
  /** A key for the text size attribute. */
  public static final TextAttribute SIZE = new TextAttribute("size");
  
  /** A key for the STRIKETHROUGH attribute. */
  public static final TextAttribute STRIKETHROUGH =
    new TextAttribute("strikethrough");
  
  /** A value that can be used with the {@link #STRIKETHROUGH} attribute. */
  public static final Boolean STRIKETHROUGH_ON = Boolean.TRUE;
  
  /** A key for the SUPERSCRIPT attribute. */
  public static final TextAttribute SUPERSCRIPT =
    new TextAttribute("superscript");
  
  /** A value that can be used with the {@link #SUPERSCRIPT} attribute. */
  public static final Integer SUPERSCRIPT_SUB = new Integer(-1);
  
  /** A value that can be used with the {@link #SUPERSCRIPT} attribute. */
  public static final Integer SUPERSCRIPT_SUPER = new Integer(1);
  
  /** A key for the SWAP_COLORS attribute. */
  public static final TextAttribute SWAP_COLORS =
    new TextAttribute("swap_colors");
  
  /** A value that can be used with the {@link #SWAP_COLORS} attribute. */
  public static final Boolean SWAP_COLORS_ON = Boolean.TRUE;
  
  /** A key for the TRANFORM attribute. */
  public static final TextAttribute TRANSFORM = new TextAttribute("transform");
  
  /** A key for the UNDERLINE attribute. */
  public static final TextAttribute UNDERLINE = new TextAttribute("underline");
  
  /** A value that can be used with the {@link #UNDERLINE} attribute. */
  public static final Integer UNDERLINE_LOW_DASHED = new Integer(5);
  
  /** A value that can be used with the {@link #UNDERLINE} attribute. */
  public static final Integer UNDERLINE_LOW_DOTTED = new Integer(3);
  
  /** A value that can be used with the {@link #UNDERLINE} attribute. */
  public static final Integer UNDERLINE_LOW_GRAY = new Integer(4);
  
  /** A value that can be used with the {@link #UNDERLINE} attribute. */
  public static final Integer UNDERLINE_LOW_ONE_PIXEL = new Integer(1);
  
  /** A value that can be used with the {@link #UNDERLINE} attribute. */
  public static final Integer UNDERLINE_LOW_TWO_PIXEL = new Integer(2);
  
  /** A value that can be used with the {@link #UNDERLINE} attribute. */
  public static final Integer UNDERLINE_ON = new Integer(0);
  
  /** A key for the WEIGHT attribute. */
  public static final TextAttribute WEIGHT = new TextAttribute("weight");
  
  /** A value that can be used with the {@link #WEIGHT} attribute. */
  public static final Float WEIGHT_BOLD = new Float(2.0);
  
  /** A value that can be used with the {@link #WEIGHT} attribute. */
  public static final Float WEIGHT_DEMIBOLD = new Float(1.75);
  
  /** A value that can be used with the {@link #WEIGHT} attribute. */
  public static final Float WEIGHT_DEMILIGHT = new Float(0.875);
  
  /** A value that can be used with the {@link #WEIGHT} attribute. */
  public static final Float WEIGHT_EXTRA_LIGHT = new Float(0.5);
  
  /** A value that can be used with the {@link #WEIGHT} attribute. */
  public static final Float WEIGHT_EXTRABOLD = new Float(2.5);
  
  /** A value that can be used with the {@link #WEIGHT} attribute. */
  public static final Float WEIGHT_HEAVY = new Float(2.25);
  
  /** A value that can be used with the {@link #WEIGHT} attribute. */
  public static final Float WEIGHT_LIGHT = new Float(0.75);
  
  /** A value that can be used with the {@link #WEIGHT} attribute. */
  public static final Float WEIGHT_MEDIUM = new Float(1.5);
  
  /** A value that can be used with the {@link #WEIGHT} attribute. */
  public static final Float WEIGHT_REGULAR = new Float(1.0);
  
  /** A value that can be used with the {@link #WEIGHT} attribute. */
  public static final Float WEIGHT_SEMIBOLD = new Float(1.25);
  
  /** A value that can be used with the {@link #WEIGHT} attribute. */
  public static final Float WEIGHT_ULTRABOLD = new Float(2.75);
  
  /** A key for the WIDTH attribute. */
  public static final TextAttribute WIDTH = new TextAttribute("width");
  
  /** A value that can be used with the {@link #WIDTH} attribute. */
  public static final Float WIDTH_CONDENSED = new Float(0.75);
  
  /** A value that can be used with the {@link #WIDTH} attribute. */
  public static final Float WIDTH_EXTENDED = new Float(1.5);
  
  /** A value that can be used with the {@link #WIDTH} attribute. */
  public static final Float WIDTH_REGULAR = new Float(1.0);
  
  /** A value that can be used with the {@link #WIDTH} attribute. */
  public static final Float WIDTH_SEMI_CONDENSED = new Float(0.875);
  
  /** A value that can be used with the {@link #WIDTH} attribute. */
  public static final Float WIDTH_SEMI_EXTENDED = new Float(1.25);
          
  /**
   * Creates a new attribute.
   * 
   * @param name  the name.
   */
  protected TextAttribute(String name)
  {
    super(name);
  }
  
  /**
   * After deserialization, this method ensures that only one instance of
   * each attribute is used.
   * 
   * @return The (single) attribute instance.
   * 
   * @throws InvalidObjectException if the attribute is not recognised.
   */
  protected Object readResolve()
    throws InvalidObjectException
  {
    if (this.getName().equals("background"))
      return BACKGROUND;

    if (this.getName().equals("bidi_embedding"))
      return BIDI_EMBEDDING;

    if (this.getName().equals("char_replacement"))
      return CHAR_REPLACEMENT;

    if (this.getName().equals("family"))
      return FAMILY;

    if (this.getName().equals("font"))
      return FONT;

    if (this.getName().equals("foreground"))
      return FOREGROUND;

    if (this.getName().equals("input method highlight"))
      return INPUT_METHOD_HIGHLIGHT;

    if (this.getName().equals("input method underline"))
      return INPUT_METHOD_UNDERLINE;

    if (this.getName().equals("justification"))
      return JUSTIFICATION;

    if (this.getName().equals("numeric_shaping"))
      return NUMERIC_SHAPING;

    if (this.getName().equals("posture"))
      return POSTURE;

    if (this.getName().equals("run_direction"))
      return RUN_DIRECTION;

    if (this.getName().equals("size"))
      return SIZE;

    if (this.getName().equals("strikethrough"))
      return STRIKETHROUGH;

    if (this.getName().equals("superscript"))
      return SUPERSCRIPT;

    if (this.getName().equals("swap_colors"))
      return SWAP_COLORS;

    if (this.getName().equals("transform"))
      return TRANSFORM;

    if (this.getName().equals("underline"))
      return UNDERLINE;

    if (this.getName().equals("weight"))
      return WEIGHT;

    if (this.getName().equals("width"))
      return WIDTH;

    throw new InvalidObjectException("Can't resolve Attribute: " + getName());
  }
}
