/* TextAttribute.java
   Copyright (C) 2003 Free Software Foundation, Inc.

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

public final class TextAttribute extends AttributedCharacterIterator.Attribute
{
  private static final long serialVersionUID = 7744112784117861702L;
  
  public static final TextAttribute BACKGROUND =
    new TextAttribute ("BACKGROUND");
  public static final TextAttribute BIDI_EMBEDDING =
    new TextAttribute ("BIDI_EMBEDDING");
  public static final TextAttribute CHAR_REPLACEMENT =
    new TextAttribute ("CHAR_REPLACEMENT");
  public static final TextAttribute FAMILY = new TextAttribute ("FAMILY");
  public static final TextAttribute FONT = new TextAttribute ("FONT");
  public static final TextAttribute FOREGROUND =
    new TextAttribute ("FOREGROUND");
  public static final TextAttribute INPUT_METHOD_HIGHLIGHT =
    new TextAttribute ("INPUT_METHOD_HIGHLIGHT");
  public static final TextAttribute INPUT_METHOD_UNDERLINE =
    new TextAttribute ("INPUT_METHOD_UNDERLINE");
  public static final TextAttribute JUSTIFICATION =
    new TextAttribute ("JUSTIFICATION");
  public static final Float JUSTIFICATION_FULL = new Float (1.0);
  public static final Float JUSTIFICATION_NONE = new Float (0.0);
  public static final TextAttribute NUMERIC_SHAPING =
    new TextAttribute ("NUMERIC_SHAPING");
  public static final TextAttribute POSTURE = new TextAttribute ("POSTURE");
  public static final Float POSTURE_OBLIQUE = new Float (0.2);
  public static final Float POSTURE_REGULAR = new Float (0.0);
  public static final TextAttribute RUN_DIRECTION =
    new TextAttribute ("RUN_DIRECTION");
  public static final Boolean RUN_DIRECTION_LTR = new Boolean (true);
  public static final Boolean RUN_DIRECTION_RTL = new Boolean (false);
  public static final TextAttribute SIZE = new TextAttribute ("SIZE");
  public static final TextAttribute STRIKETHROUGH =
    new TextAttribute ("STRIKETHROUGH");
  public static final Boolean STRIKETHROUGH_ON = new Boolean (true);
  public static final TextAttribute SUPERSCRIPT =
    new TextAttribute ("SUPERSCRIPT");
  public static final Integer SUPERSCRIPT_SUB = new Integer (-1);
  public static final Integer SUPERSCRIPT_SUPER = new Integer (1);
  public static final TextAttribute SWAP_COLORS =
    new TextAttribute ("SWAP_COLORS");
  public static final Boolean SWAP_COLORS_ON = new Boolean (true);
  public static final TextAttribute TRANSFORM = new TextAttribute ("TRANSFORM");
  public static final TextAttribute UNDERLINE = new TextAttribute ("UNDERLINE");
  public static final Integer UNDERLINE_LOW_DASHED = new Integer (0);
  public static final Integer UNDERLINE_LOW_DOTTED = new Integer (0);
  public static final Integer UNDERLINE_LOW_GRAY = new Integer (0);
  public static final Integer UNDERLINE_LOW_ONE_PIXEL = new Integer (0);
  public static final Integer UNDERLINE_LOW_TWO_PIXEL = new Integer (0);
  public static final Integer UNDERLINE_ON = new Integer (0);
  public static final TextAttribute WEIGHT = new TextAttribute ("WEIGHT");
  public static final Float WEIGHT_BOLD = new Float (2.0);
  public static final Float WEIGHT_DEMIBOLD = new Float (1.75);
  public static final Float WEIGHT_DEMILIGHT = new Float (0.875);
  public static final Float WEIGHT_EXTRA_LIGHT = new Float (0.5);
  public static final Float WEIGHT_EXTRABOLD = new Float (2.5);
  public static final Float WEIGHT_HEAVY = new Float (2.25);
  public static final Float WEIGHT_LIGHT = new Float (0.75);
  public static final Float WEIGHT_MEDIUM = new Float (1.5);
  public static final Float WEIGHT_REGULAR = new Float (1.0);
  public static final Float WEIGHT_SEMIBOLD = new Float (1.25);
  public static final Float WEIGHT_ULTRABOLD = new Float (2.75);
  public static final TextAttribute WIDTH = new TextAttribute ("");
  public static final Float WIDTH_CONDENSED = new Float (0.75);
  public static final Float WIDTH_EXTENDED = new Float (1.5);
  public static final Float WIDTH_REGULAR = new Float (1.0);
  public static final Float WIDTH_SEMI_CONDENSED = new Float (0.875);
  public static final Float WIDTH_SEMI_EXTENDED = new Float (1.25);
             
  protected TextAttribute (String name)
  {
    super (name);
  }
  
  protected Object readResolve ()
    throws InvalidObjectException
  {
    throw new Error ("not implemented");
  }
}
