/* StyleConstants.java --
   Copyright (C) 2004, 2005, 2006 Free Software Foundation, Inc.

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
Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
02110-1301 USA.

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


package javax.swing.text;

import java.awt.Color;
import java.awt.Component;
import java.util.ArrayList;

import javax.swing.Icon;

/**
 * Represents standard attribute keys.  This class also contains a set of 
 * useful static utility methods for querying and populating an 
 * {@link AttributeSet}.
 * 
 * @since 1.2
 */
public class StyleConstants
{
  /** 
   * A value representing left alignment for the 
   * {@link ParagraphConstants#Alignment} attribute. 
   */
  public static final int ALIGN_LEFT = 0;

  /** 
   * A value representing center alignment for the 
   * {@link ParagraphConstants#Alignment} attribute. 
   */
  public static final int ALIGN_CENTER = 1;

  /** 
   * A value representing right alignment for the 
   * {@link ParagraphConstants#Alignment} attribute. 
   */
  public static final int ALIGN_RIGHT = 2;

  /** 
   * A value representing ful justification for the 
   * {@link ParagraphConstants#Alignment} attribute. 
   */
  public static final int ALIGN_JUSTIFIED = 3;

  /** An alias for {@link CharacterConstants#Background}. */
  public static final Object Background = CharacterConstants.Background;

  /** An alias for {@link CharacterConstants#BidiLevel}. */
  public static final Object BidiLevel = CharacterConstants.BidiLevel;
  
  /** An alias for {@link CharacterConstants#Bold}. */
  public static final Object Bold = CharacterConstants.Bold;
  
  /** An alias for {@link CharacterConstants#ComponentAttribute}. */
  public static final Object ComponentAttribute 
      = CharacterConstants.ComponentAttribute;
  
  /** An alias for {@link CharacterConstants#Family}. */
  public static final Object Family = CharacterConstants.Family;
  
  /** An alias for {@link CharacterConstants#Family}. */
  public static final Object FontFamily = CharacterConstants.Family;  
  
  /** An alias for {@link CharacterConstants#Size}. */
  public static final Object FontSize = CharacterConstants.Size;
  
  /** An alias for {@link CharacterConstants#Foreground}. */
  public static final Object Foreground = CharacterConstants.Foreground;
  
  /** An alias for {@link CharacterConstants#IconAttribute}. */
  public static final Object IconAttribute = CharacterConstants.IconAttribute;
  
  /** An alias for {@link CharacterConstants#Italic}. */
  public static final Object Italic = CharacterConstants.Italic;
  
  /** An alias for {@link CharacterConstants#Size}. */
  public static final Object Size = CharacterConstants.Size;
  
  /** An alias for {@link CharacterConstants#StrikeThrough}. */
  public static final Object StrikeThrough = CharacterConstants.StrikeThrough;
  
  /** An alias for {@link CharacterConstants#Subscript}. */
  public static final Object Subscript = CharacterConstants.Subscript;
  
  /** An alias for {@link CharacterConstants#Superscript}. */
  public static final Object Superscript = CharacterConstants.Superscript;
  
  /** An alias for {@link CharacterConstants#Underline}. */
  public static final Object Underline = CharacterConstants.Underline;

  /** An alias for {@link ParagraphConstants#Alignment}. */
  public static final Object Alignment = ParagraphConstants.Alignment;
  
  /** An alias for {@link ParagraphConstants#FirstLineIndent}. */
  public static final Object FirstLineIndent 
      = ParagraphConstants.FirstLineIndent;
  
  /** An alias for {@link ParagraphConstants#LeftIndent}. */
  public static final Object LeftIndent = ParagraphConstants.LeftIndent;
  
  /** An alias for {@link ParagraphConstants#LineSpacing}. */
  public static final Object LineSpacing = ParagraphConstants.LineSpacing;
  
  /** An alias for {@link ParagraphConstants#Orientation}. */
  public static final Object Orientation = ParagraphConstants.Orientation;
  
  /** An alias for {@link ParagraphConstants#RightIndent}. */
  public static final Object RightIndent = ParagraphConstants.RightIndent;
  
  /** An alias for {@link ParagraphConstants#SpaceAbove}. */
  public static final Object SpaceAbove = ParagraphConstants.SpaceAbove;
  
  /** An alias for {@link ParagraphConstants#SpaceBelow}. */
  public static final Object SpaceBelow = ParagraphConstants.SpaceBelow;
  
  /** An alias for {@link ParagraphConstants#TabSet}. */
  public static final Object TabSet = ParagraphConstants.TabSet;

  public static final String ComponentElementName = "component";

  public static final String IconElementName = "icon";

  public static final Object ComposedTextAttribute 
      = new StyleConstants("composed text");
  
  public static final Object ModelAttribute = new StyleConstants("model");
  
  public static final Object NameAttribute = new StyleConstants("name");
  
  public static final Object ResolveAttribute = new StyleConstants("resolver");

  /**
   * All StyleConstants keys. This is used in StyleContext to register
   * all known keys as static attribute keys for serialization.
   */
  static ArrayList keys;

  String keyname;

  // Package-private to avoid accessor constructor for use by
  // subclasses.
  StyleConstants(String k) 
  {
    keyname = k;
    if (keys == null)
      keys = new ArrayList();
    keys.add(this);
  }

  /**
   * Returns a string representation of the attribute key.
   * 
   * @return A string representation of the attribute key.
   */
  public String toString()
  {
    return keyname;
  }

  /**
   * Returns the alignment specified in the given attributes, or 
   * {@link #ALIGN_LEFT} if no alignment is specified.
   * 
   * @param a  the attribute set (<code>null</code> not permitted).
   * 
   * @return The alignment (typically one of {@link #ALIGN_LEFT}, 
   *         {@link #ALIGN_RIGHT}, {@link #ALIGN_CENTER} or 
   *         {@link #ALIGN_JUSTIFIED}).
   *         
   * @see #setAlignment(MutableAttributeSet, int)
   */
  public static int getAlignment(AttributeSet a)
  {
    Integer i = (Integer) a.getAttribute(Alignment);
    if (i != null)
      return i.intValue();
    else
      return ALIGN_LEFT;      
  } 

  /**
   * Returns the background color specified in the given attributes, or
   * {@link Color#BLACK} if no background color is specified.
   * 
   * @param a  the attribute set (<code>null</code> not permitted).
   * 
   * @return The background color.
   * 
   * @see #setBackground(MutableAttributeSet, Color)
   */
  public static Color getBackground(AttributeSet a)
  {
    Color c = (Color) a.getAttribute(Background);
    if (c != null) 
      return c;
    else
      return Color.BLACK;
  } 

  /**
   * Returns the bidi level specified in the given attributes, or 
   * <code>0</code> if no bidi level is specified.
   * 
   * @param a  the attribute set (<code>null</code> not permitted).
   * 
   * @return The bidi level.
   * 
   * @see #setBidiLevel(MutableAttributeSet, int)
   */  
  public static int getBidiLevel(AttributeSet a)
  {
    Integer i = (Integer) a.getAttribute(BidiLevel);
    if (i != null)
      return i.intValue();
    else
      return 0;
  } 

  /**
   * Returns the component specified in the given attributes, or 
   * <code>null</code> if no component is specified.
   * 
   * @param a  the attribute set (<code>null</code> not permitted).
   * 
   * @return The component (possibly <code>null</code>).
   * 
   * @see #setComponent(MutableAttributeSet, Component)
   */    
  public static Component getComponent(AttributeSet a)
  {
    Component c = (Component) a.getAttribute(ComponentAttribute);
    if (c != null)
      return c;
    else
      return null;
  } 

  /**
   * Returns the indentation specified in the given attributes, or 
   * <code>0.0f</code> if no indentation is specified.
   * 
   * @param a  the attribute set (<code>null</code> not permitted).
   * 
   * @return The indentation.
   * 
   * @see #setFirstLineIndent(MutableAttributeSet, float)
   */    
  public static float getFirstLineIndent(AttributeSet a)
  {
    Float f = (Float) a.getAttribute(FirstLineIndent);
    if (f != null)
      return f.floatValue();
    else
      return 0.0f;
  } 

  /**
   * Returns the font family specified in the given attributes, or 
   * <code>Monospaced</code> if no font family is specified.
   * 
   * @param a  the attribute set (<code>null</code> not permitted).
   * 
   * @return The font family.
   * 
   * @see #setFontFamily(MutableAttributeSet, String)
   */    
  public static String getFontFamily(AttributeSet a)
  {
    String ff = (String) a.getAttribute(FontFamily);
    if (ff != null)
      return ff;
    else
      return "Monospaced";
  } 

  /**
   * Returns the font size specified in the given attributes, or 
   * <code>12</code> if no font size is specified.
   * 
   * @param a  the attribute set (<code>null</code> not permitted).
   * 
   * @return The font size.
   * 
   * @see #setFontSize(MutableAttributeSet, int)
   */  
  public static int getFontSize(AttributeSet a)
  {
    Integer i = (Integer) a.getAttribute(FontSize);
    if (i != null)
      return i.intValue();
    else
      return 12;
  } 

  /**
   * Returns the foreground color specified in the given attributes, or
   * {@link Color#BLACK} if no foreground color is specified.
   * 
   * @param a  the attribute set (<code>null</code> not permitted).
   * 
   * @return The foreground color.
   * 
   * @see #setForeground(MutableAttributeSet, Color)
   */
  public static Color getForeground(AttributeSet a)
  {
    Color c = (Color) a.getAttribute(Foreground);
    if (c != null)
      return c;
    else
      return Color.BLACK;
  } 

  /**
   * Returns the icon specified in the given attributes, or 
   * <code>null</code> if no icon is specified.
   * 
   * @param a  the attribute set (<code>null</code> not permitted).
   * 
   * @return The icon (possibly <code>null</code>).
   * 
   * @see #setIcon(MutableAttributeSet, Icon)
   */    
  public static Icon getIcon(AttributeSet a)
  {
    return (Icon) a.getAttribute(IconAttribute);
  } 

  /**
   * Returns the left indentation specified in the given attributes, or 
   * <code>0.0f</code> if no left indentation is specified.
   * 
   * @param a  the attribute set (<code>null</code> not permitted).
   * 
   * @return The left indentation.
   * 
   * @see #setLeftIndent(MutableAttributeSet, float)
   */    
  public static float getLeftIndent(AttributeSet a)
  {
    Float f = (Float) a.getAttribute(LeftIndent);
    if (f != null)
      return f.floatValue();
    else
      return 0.0f;
  } 

  /**
   * Returns the line spacing specified in the given attributes, or 
   * <code>0.0f</code> if no line spacing is specified.
   * 
   * @param a  the attribute set (<code>null</code> not permitted).
   * 
   * @return The line spacing.
   * 
   * @see #setLineSpacing(MutableAttributeSet, float)
   */    
  public static float getLineSpacing(AttributeSet a)
  {
    Float f = (Float) a.getAttribute(LineSpacing);
    if (f != null)
      return f.floatValue();
    else
      return 0.0f;
  } 

  /**
   * Returns the right indentation specified in the given attributes, or 
   * <code>0.0f</code> if no right indentation is specified.
   * 
   * @param a  the attribute set (<code>null</code> not permitted).
   * 
   * @return The right indentation.
   * 
   * @see #setRightIndent(MutableAttributeSet, float)
   */    
  public static float getRightIndent(AttributeSet a)
  {
    Float f = (Float) a.getAttribute(RightIndent);
    if (f != null)
      return f.floatValue();
    else
      return 0.0f;
  } 

  /**
   * Returns the 'space above' specified in the given attributes, or 
   * <code>0.0f</code> if no 'space above' is specified.
   * 
   * @param a  the attribute set (<code>null</code> not permitted).
   * 
   * @return The 'space above'.
   * 
   * @see #setSpaceAbove(MutableAttributeSet, float)
   */    
  public static float getSpaceAbove(AttributeSet a)
  {
    Float f = (Float) a.getAttribute(SpaceAbove);
    if (f != null)
      return f.floatValue();
    else 
      return 0.0f;
  } 

  /**
   * Returns the 'space below' specified in the given attributes, or 
   * <code>0.0f</code> if no 'space below' is specified.
   * 
   * @param a  the attribute set (<code>null</code> not permitted).
   * 
   * @return The 'space below'.
   * 
   * @see #setSpaceBelow(MutableAttributeSet, float)
   */    
  public static float getSpaceBelow(AttributeSet a)
  {
    Float f = (Float) a.getAttribute(SpaceBelow);
    if (f != null)
      return f.floatValue();
    else
      return 0.0f;
  } 

  /**
   * Returns the tab set specified in the given attributes, or 
   * <code>null</code> if no tab set is specified.
   * 
   * @param a  the attribute set (<code>null</code> not permitted).
   * 
   * @return The tab set.
   * 
   * @see #setTabSet(MutableAttributeSet, javax.swing.text.TabSet)
   */    
  public static javax.swing.text.TabSet getTabSet(AttributeSet a)
  {
    // I'm guessing that the fully qualified class name is to differentiate
    // between the TabSet class and the TabSet (attribute) instance on some
    // compiler...
    return (javax.swing.text.TabSet) a.getAttribute(StyleConstants.TabSet);
  } 

  /**
   * Returns the value of the bold flag in the given attributes, or 
   * <code>false</code> if no bold flag is specified.
   * 
   * @param a  the attribute set (<code>null</code> not permitted).
   * 
   * @return The bold flag.
   * 
   * @see #setBold(MutableAttributeSet, boolean)
   */
  public static boolean isBold(AttributeSet a)
  {
    Boolean b = (Boolean) a.getAttribute(Bold);
    if (b != null)
      return b.booleanValue();
    else
      return false;
  } 

  /**
   * Returns the value of the italic flag in the given attributes, or 
   * <code>false</code> if no italic flag is specified.
   * 
   * @param a  the attribute set (<code>null</code> not permitted).
   * 
   * @return The italic flag.
   * 
   * @see #setItalic(MutableAttributeSet, boolean)
   */
  public static boolean isItalic(AttributeSet a)
  {
    Boolean b = (Boolean) a.getAttribute(Italic);
    if (b != null)
      return b.booleanValue();
    else
      return false;
  } 

  /**
   * Returns the value of the strike-through flag in the given attributes, or 
   * <code>false</code> if no strike-through flag is specified.
   * 
   * @param a  the attribute set (<code>null</code> not permitted).
   * 
   * @return The strike-through flag.
   * 
   * @see #setStrikeThrough(MutableAttributeSet, boolean)
   */
  public static boolean isStrikeThrough(AttributeSet a)
  {
    Boolean b = (Boolean) a.getAttribute(StrikeThrough);
    if (b != null)
      return b.booleanValue();
    else
      return false;
  } 

  /**
   * Returns the value of the subscript flag in the given attributes, or 
   * <code>false</code> if no subscript flag is specified.
   * 
   * @param a  the attribute set (<code>null</code> not permitted).
   * 
   * @return The subscript flag.
   * 
   * @see #setSubscript(MutableAttributeSet, boolean)
   */
  public static boolean isSubscript(AttributeSet a)
  {
    Boolean b = (Boolean) a.getAttribute(Subscript);
    if (b != null)
      return b.booleanValue();
    else
      return false;
  } 

  /**
   * Returns the value of the superscript flag in the given attributes, or 
   * <code>false</code> if no superscript flag is specified.
   * 
   * @param a  the attribute set (<code>null</code> not permitted).
   * 
   * @return The superscript flag.
   * 
   * @see #setSuperscript(MutableAttributeSet, boolean)
   */
  public static boolean isSuperscript(AttributeSet a)
  {
    Boolean b = (Boolean) a.getAttribute(Superscript);
    if (b != null)
      return b.booleanValue();
    else 
      return false;
  } 

  /**
   * Returns the value of the underline flag in the given attributes, or 
   * <code>false</code> if no underline flag is specified.
   * 
   * @param a  the attribute set (<code>null</code> not permitted).
   * 
   * @return The underline flag.
   * 
   * @see #setUnderline(MutableAttributeSet, boolean)
   */
  public static boolean isUnderline(AttributeSet a)
  {
    Boolean b = (Boolean) a.getAttribute(Underline);
    if (b != null)
      return b.booleanValue();
    else
      return false;
  } 

  /**
   * Adds an alignment attribute to the specified set.
   * 
   * @param a  the attribute set (<code>null</code> not permitted).
   * @param align  the alignment (typically one of 
   *               {@link StyleConstants#ALIGN_LEFT}, 
   *               {@link StyleConstants#ALIGN_RIGHT}, 
   *               {@link StyleConstants#ALIGN_CENTER} or 
   *               {@link StyleConstants#ALIGN_JUSTIFIED}).
   * 
   * @throws NullPointerException if <code>a</code> is <code>null</code>.
   * 
   * @see #getAlignment(AttributeSet)
   */
  public static void setAlignment(MutableAttributeSet a, int align)
  {
    a.addAttribute(Alignment, new Integer(align));
  } 

  /**
   * Adds a background attribute to the specified set.
   * 
   * @param a  the attribute set (<code>null</code> not permitted).
   * @param bg  the background (<code>null</code> not permitted).
   * 
   * @throws NullPointerException if either argument is <code>null</code>.
   * 
   * @see #getBackground(AttributeSet)
   */
  public static void setBackground(MutableAttributeSet a, Color bg)
  {
    a.addAttribute(Background, bg);
  } 

  /**
   * Adds a bidi-level attribute to the specified set.
   * 
   * @param a  the attribute set (<code>null</code> not permitted).
   * @param lev  the level.
   * 
   * @throws NullPointerException if <code>a</code> is <code>null</code>.
   * 
   * @see #getBidiLevel(AttributeSet)
   */
  public static void setBidiLevel(MutableAttributeSet a, int lev)
  {
    a.addAttribute(BidiLevel, new Integer(lev));
  } 

  /**
   * Adds a bold attribute to the specified set.
   * 
   * @param a  the attribute set (<code>null</code> not permitted).
   * @param b  the new value of the bold attribute.
   * 
   * @throws NullPointerException if <code>a</code> is <code>null</code>.
   * 
   * @see #isBold(AttributeSet)
   */
  public static void setBold(MutableAttributeSet a, boolean b)
  {
    a.addAttribute(Bold, Boolean.valueOf(b));
  } 
  
  /**
   * Adds a component attribute to the specified set.
   * 
   * @param a  the attribute set (<code>null</code> not permitted).
   * @param c  the component (<code>null</code> not permitted).
   * 
   * @throws NullPointerException if either argument is <code>null</code>.
   * 
   * @see #getComponent(AttributeSet)
   */
  public static void setComponent(MutableAttributeSet a, Component c)
  {
    a.addAttribute(ComponentAttribute, c);
  } 

  /**
   * Adds a first line indentation attribute to the specified set.
   * 
   * @param a  the attribute set (<code>null</code> not permitted).
   * @param i  the indentation.
   * 
   * @throws NullPointerException if <code>a</code> is <code>null</code>.
   * 
   * @see #getFirstLineIndent(AttributeSet)
   */
  public static void setFirstLineIndent(MutableAttributeSet a, float i)
  {
    a.addAttribute(FirstLineIndent, new Float(i));
  } 

  /**
   * Adds a font family attribute to the specified set.
   * 
   * @param a  the attribute set (<code>null</code> not permitted).
   * @param fam  the font family name (<code>null</code> not permitted).
   * 
   * @throws NullPointerException if either argument is <code>null</code>.
   * 
   * @see #getFontFamily(AttributeSet)
   */
  public static void setFontFamily(MutableAttributeSet a, String fam)
  {
    a.addAttribute(FontFamily, fam);
  } 

  /**
   * Adds a font size attribute to the specified set.
   * 
   * @param a  the attribute set (<code>null</code> not permitted).
   * @param s  the font size (in points).
   * 
   * @throws NullPointerException if <code>a</code> is <code>null</code>.
   * 
   * @see #getFontSize(AttributeSet)
   */
  public static void setFontSize(MutableAttributeSet a, int s)
  {
    a.addAttribute(FontSize, new Integer(s));
  } 

  /**
   * Adds a foreground color attribute to the specified set.
   * 
   * @param a  the attribute set (<code>null</code> not permitted).
   * @param fg  the foreground color (<code>null</code> not permitted).
   * 
   * @throws NullPointerException if either argument is <code>null</code>.
   * 
   * @see #getForeground(AttributeSet)
   */
  public static void setForeground(MutableAttributeSet a, Color fg)
  {
    a.addAttribute(Foreground, fg);
  }

  /**
   * Adds an icon attribute to the specified set.
   * 
   * @param a  the attribute set (<code>null</code> not permitted).
   * @param c  the icon (<code>null</code> not permitted).
   * 
   * @throws NullPointerException if either argument is <code>null</code>.
   * 
   * @see #getIcon(AttributeSet)
   */
  public static void setIcon(MutableAttributeSet a, Icon c)
  {
    a.addAttribute(AbstractDocument.ElementNameAttribute, IconElementName);
    a.addAttribute(IconAttribute, c);
  }
 
  /**
   * Adds an italic attribute to the specified set.
   * 
   * @param a  the attribute set (<code>null</code> not permitted).
   * @param b  the new value of the italic attribute.
   * 
   * @throws NullPointerException if <code>a</code> is <code>null</code>.
   * 
   * @see #isItalic(AttributeSet)
   */
  public static void setItalic(MutableAttributeSet a, boolean b)
  {
    a.addAttribute(Italic, Boolean.valueOf(b));
  }
 
  /**
   * Adds a left indentation attribute to the specified set.
   * 
   * @param a  the attribute set (<code>null</code> not permitted).
   * @param i  the indentation.
   * 
   * @throws NullPointerException if <code>a</code> is <code>null</code>.
   * 
   * @see #getLeftIndent(AttributeSet)
   */
  public static void setLeftIndent(MutableAttributeSet a, float i)
  {
    a.addAttribute(LeftIndent, new Float(i));
  } 

  /**
   * Adds a line spacing attribute to the specified set.
   * 
   * @param a  the attribute set (<code>null</code> not permitted).
   * @param i  the line spacing.
   * 
   * @throws NullPointerException if <code>a</code> is <code>null</code>.
   * 
   * @see #getLineSpacing(AttributeSet)
   */
  public static void setLineSpacing(MutableAttributeSet a, float i)
  {
    a.addAttribute(LineSpacing, new Float(i));
  } 

  /**
   * Adds a right indentation attribute to the specified set.
   * 
   * @param a  the attribute set (<code>null</code> not permitted).
   * @param i  the right indentation.
   * 
   * @throws NullPointerException if <code>a</code> is <code>null</code>.
   * 
   * @see #getRightIndent(AttributeSet)
   */
  public static void setRightIndent(MutableAttributeSet a, float i)
  {
    a.addAttribute(RightIndent, new Float(i));
  } 

  /**
   * Adds a 'space above' attribute to the specified set.
   * 
   * @param a  the attribute set (<code>null</code> not permitted).
   * @param i  the space above attribute value.
   * 
   * @throws NullPointerException if <code>a</code> is <code>null</code>.
   * 
   * @see #getSpaceAbove(AttributeSet)
   */
  public static void setSpaceAbove(MutableAttributeSet a, float i)
  {
    a.addAttribute(SpaceAbove, new Float(i));
  } 

  /**
   * Adds a 'space below' attribute to the specified set.
   * 
   * @param a  the attribute set (<code>null</code> not permitted).
   * @param i  the space below attribute value.
   * 
   * @throws NullPointerException if <code>a</code> is <code>null</code>.
   * 
   * @see #getSpaceBelow(AttributeSet)
   */
  public static void setSpaceBelow(MutableAttributeSet a, float i)
  {
    a.addAttribute(SpaceBelow, new Float(i));
  } 

  /**
   * Adds a strike-through attribue to the specified set.
   * 
   * @param a  the attribute set (<code>null</code> not permitted).
   * @param b  the strike-through attribute value.
   * 
   * @throws NullPointerException if <code>a</code> is <code>null</code>.
   * 
   * @see #isStrikeThrough(AttributeSet)
   */
  public static void setStrikeThrough(MutableAttributeSet a, boolean b)
  {
    a.addAttribute(StrikeThrough, Boolean.valueOf(b));
  } 

  /**
   * Adds a subscript attribute to the specified set.
   * 
   * @param a  the attribute set (<code>null</code> not permitted).
   * @param b  the subscript attribute value.
   * 
   * @throws NullPointerException if <code>a</code> is <code>null</code>.
   * 
   * @see #isSubscript(AttributeSet)
   */
  public static void setSubscript(MutableAttributeSet a, boolean b)
  {
    a.addAttribute(Subscript, Boolean.valueOf(b));
  } 

  /**
   * Adds a superscript attribute to the specified set.
   * 
   * @param a  the attribute set (<code>null</code> not permitted).
   * @param b  the superscript attribute value.
   * 
   * @throws NullPointerException if <code>a</code> is <code>null</code>.
   * 
   * @see #isSuperscript(AttributeSet)
   */
  public static void setSuperscript(MutableAttributeSet a, boolean b)
  {
    a.addAttribute(Superscript, Boolean.valueOf(b));
  } 

  /**
   * Adds a {@link TabSet} attribute to the specified set.
   * 
   * @param a  the attribute set (<code>null</code> not permitted).
   * @param tabs  the tab set (<code>null</code> not permitted).
   * 
   * @throws NullPointerException if either argument is <code>null</code>.
   * 
   * @see #getTabSet(AttributeSet)
   */
  public static void setTabSet(MutableAttributeSet a, 
                               javax.swing.text.TabSet tabs)
  {
    a.addAttribute(StyleConstants.TabSet, tabs);
  } 

  /**
   * Adds an underline attribute to the specified set.
   * 
   * @param a  the attribute set (<code>null</code> not permitted).
   * @param b  the underline attribute value.
   * 
   * @throws NullPointerException if <code>a</code> is <code>null</code>.
   * 
   * @see #isUnderline(AttributeSet)
   */
  public static void setUnderline(MutableAttributeSet a, boolean b)
  {
    a.addAttribute(Underline, Boolean.valueOf(b));
  } 

  // The remainder are so-called "typesafe enumerations" which 
  // alias subsets of the above constants.

  /**
   * A set of keys for attributes that apply to characters.
   */
  public static class CharacterConstants
    extends StyleConstants
    implements AttributeSet.CharacterAttribute
  {
    /**
     * Private constructor prevents new instances being created.
     * 
     * @param k  the key name.
     */
    private CharacterConstants(String k) 
    {
      super(k);
    }
    
    /** An alias for {@link ColorConstants#Background}. */
    public static final Object Background = ColorConstants.Background;
    
    /** A key for the bidi level character attribute. */
    public static final Object BidiLevel = new CharacterConstants("bidiLevel");
    
    /** An alias for {@link FontConstants#Bold}. */
    public static final Object Bold = FontConstants.Bold;
    
    /** A key for the component character attribute. */
    public static final Object ComponentAttribute 
        = new CharacterConstants("component");
    
    /** An alias for {@link FontConstants#Family}. */
    public static final Object Family = FontConstants.Family;
    
    /** An alias for {@link FontConstants#Size}. */
    public static final Object Size = FontConstants.Size;
    
    /** An alias for {@link ColorConstants#Foreground}. */
    public static final Object Foreground = ColorConstants.Foreground;
    
    /** A key for the icon character attribute. */
    public static final Object IconAttribute = new CharacterConstants("icon");
    
    /** A key for the italic character attribute. */
    public static final Object Italic = FontConstants.Italic;
    
    /** A key for the strike through character attribute. */
    public static final Object StrikeThrough 
        = new CharacterConstants("strikethrough");
    
    /** A key for the subscript character attribute. */
    public static final Object Subscript = new CharacterConstants("subscript");
    
    /** A key for the superscript character attribute. */
    public static final Object Superscript 
        = new CharacterConstants("superscript");
    
    /** A key for the underline character attribute. */
    public static final Object Underline = new CharacterConstants("underline");
  
  }

  /**
   * A set of keys for attributes that relate to colors.
   */
  public static class ColorConstants
    extends StyleConstants
    implements AttributeSet.ColorAttribute, AttributeSet.CharacterAttribute
  {
    /**
     * Private constructor prevents new instances being created.
     * 
     * @param k  the key name.
     */
    private ColorConstants(String k) 
    {
      super(k);
    }
    
    /** A key for the foreground color attribute. */
    public static final Object Foreground = new ColorConstants("foreground");

    /** A key for the background color attribute. */
    public static final Object Background = new ColorConstants("background");
  }

  /**
   * A set of keys for attributes that apply to fonts.
   */
  public static class FontConstants
    extends StyleConstants
    implements AttributeSet.FontAttribute, AttributeSet.CharacterAttribute
  {
    /**
     * Private constructor prevents new instances being created.
     * 
     * @param k  the key name.
     */
    private FontConstants(String k) 
    {
      super(k);
    }
    
    /** A key for the bold font attribute. */
    public static final Object Bold = new FontConstants("bold");

    /** A key for the family font attribute. */
    public static final Object Family = new FontConstants("family");
    
    /** A key for the italic font attribute. */
    public static final Object Italic = new FontConstants("italic");
    
    /** A key for the size font attribute. */
    public static final Object Size = new FontConstants("size");
  }

  /**
   * A set of keys for attributes that apply to paragraphs.
   */
  public static class ParagraphConstants
    extends StyleConstants
    implements AttributeSet.ParagraphAttribute
  {
    /**
     * Private constructor prevents new instances being created.
     * 
     * @param k  the key name.
     */
    private ParagraphConstants(String k) 
    {
      super(k);
    }
    
    /** A key for the alignment paragraph attribute. */
    public static final Object Alignment = new ParagraphConstants("Alignment");

    /** A key for the first line indentation paragraph attribute. */
    public static final Object FirstLineIndent 
        = new ParagraphConstants("FirstLineIndent");
    
    /** A key for the left indentation paragraph attribute. */
    public static final Object LeftIndent 
        = new ParagraphConstants("LeftIndent");
    
    /** A key for the line spacing paragraph attribute. */
    public static final Object LineSpacing 
        = new ParagraphConstants("LineSpacing");
    
    /** A key for the orientation paragraph attribute. */
    public static final Object Orientation 
        = new ParagraphConstants("Orientation");
    
    /** A key for the right indentation paragraph attribute. */
    public static final Object RightIndent 
        = new ParagraphConstants("RightIndent");
    
    /** A key for the 'space above' paragraph attribute. */
    public static final Object SpaceAbove 
        = new ParagraphConstants("SpaceAbove");
    
    /** A key for the 'space below' paragraph attribute. */
    public static final Object SpaceBelow 
        = new ParagraphConstants("SpaceBelow");
    
    /** A key for the tabset paragraph attribute. */
    public static final Object TabSet = new ParagraphConstants("TabSet");
    
  }

}
