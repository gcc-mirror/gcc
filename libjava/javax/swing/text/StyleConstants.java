/* StyleConstants.java --
   Copyright (C) 2004 Free Software Foundation, Inc.

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


package javax.swing.text;

import java.awt.Color;
import java.awt.Component;

import javax.swing.Icon;

public class StyleConstants
{
  public static final int ALIGN_LEFT = 0;
  public static final int ALIGN_CENTER = 1;
  public static final int ALIGN_RIGHT = 2;
  public static final int ALIGN_JUSTIFIED = 3;

  public static final Object Background = CharacterConstants.Background;
  public static final Object BidiLevel = CharacterConstants.BidiLevel;
  public static final Object Bold = CharacterConstants.Bold;
  public static final Object ComponentAttribute = CharacterConstants.ComponentAttribute;
  public static final Object FontFamily = CharacterConstants.Family;
  public static final Object FontSize = CharacterConstants.Size;
  public static final Object Foreground = CharacterConstants.Foreground;
  public static final Object IconAttribute = CharacterConstants.IconAttribute;
  public static final Object Italic = CharacterConstants.Italic;
  public static final Object StrikeThrough = CharacterConstants.StrikeThrough;
  public static final Object Subscript = CharacterConstants.Subscript;
  public static final Object Superscript = CharacterConstants.Superscript;
  public static final Object Underline = CharacterConstants.Underline;

  public static final Object Alignment = ParagraphConstants.Alignment;
  public static final Object FirstLineIndent = ParagraphConstants.FirstLineIndent;
  public static final Object LeftIndent = ParagraphConstants.LeftIndent;
  public static final Object LineSpacing = ParagraphConstants.LineSpacing;
  public static final Object Orientation = ParagraphConstants.Orientation;
  public static final Object RightIndent = ParagraphConstants.RightIndent;
  public static final Object SpaceAbove = ParagraphConstants.SpaceAbove;
  public static final Object SpaceBelow = ParagraphConstants.SpaceBelow;
  public static final Object TabSet = ParagraphConstants.TabSet;

  public static final String ComponentElementName = "component";
  public static final String IconElementName = "icon";

  public static final Object ComposedTextAttribute = new StyleConstants("composed text");
  public static final Object ModelAttribute = new StyleConstants("model");
  public static final Object NameAttribute = new StyleConstants("name");
  public static final Object ResolveAttribute = new StyleConstants("resolver");

  String keyname;
  
  private StyleConstants(String k) 
  {
    keyname = k;
  }

  public String toString()
  {
    return keyname;
  }

  public static int getAlignment(AttributeSet a)
  {
    if (a.isDefined(Alignment))
      return ((Integer)a.getAttribute(Alignment)).intValue();
    else
      return ALIGN_LEFT;      
  } 

  public static Color getBackground(AttributeSet a)
  {
    if (a.isDefined(Background))
      return (Color) a.getAttribute(Background);
    else
      return Color.BLACK;
  } 
  
  public static int getBidiLevel(AttributeSet a)
  {
    if (a.isDefined(BidiLevel))
      return ((Integer)a.getAttribute(BidiLevel)).intValue();
    else
      return 0;
  } 

  public static Component getComponent(AttributeSet a)
  {
    if (a.isDefined(ComponentAttribute))
      return (Component) a.getAttribute(ComponentAttribute);
    else
      return (Component) null;
  } 

  public static float getFirstLineIndent(AttributeSet a)
  {
    if (a.isDefined(FirstLineIndent))
      return ((Float)a.getAttribute(FirstLineIndent)).floatValue();
    else
      return 0.f;
  } 

  public static String getFontFamily(AttributeSet a)
  {
    if (a.isDefined(FontFamily))
      return (String) a.getAttribute(FontFamily);
    else
      return "Monospaced";
  } 

  public static int getFontSize(AttributeSet a)
  {
    if (a.isDefined(FontSize))
      return ((Integer)a.getAttribute(FontSize)).intValue();
    else
      return 12;
  } 

  public static Color getForeground(AttributeSet a)
  {
    if (a.isDefined(Foreground))
      return (Color) a.getAttribute(Foreground);
    else
      return Color.BLACK;
  } 

  public static Icon getIcon(AttributeSet a)
  {
    if (a.isDefined(IconAttribute))
      return (Icon) a.getAttribute(IconAttribute);
    else
      return (Icon) null;
  } 

  public static float getLeftIndent(AttributeSet a)
  {
    if (a.isDefined(LeftIndent))
      return ((Float)a.getAttribute(LeftIndent)).floatValue();
    else
      return 0.f;  
  } 

  public static float getLineSpacing(AttributeSet a)
  {
    if (a.isDefined(LineSpacing))
      return ((Float)a.getAttribute(LineSpacing)).floatValue();
    else
      return 0.f;  
  } 

  public static float getRightIndent(AttributeSet a)
  {
    if (a.isDefined(RightIndent))
      return ((Float)a.getAttribute(RightIndent)).floatValue();
    else
      return 0.f;  
  } 

  public static float getSpaceAbove(AttributeSet a)
  {
    if (a.isDefined(SpaceAbove))
      return ((Float)a.getAttribute(SpaceAbove)).floatValue();
    else
      return 0.f;  
  } 

  public static float getSpaceBelow(AttributeSet a)
  {
    if (a.isDefined(SpaceBelow))
      return ((Float)a.getAttribute(SpaceBelow)).floatValue();
    else
      return 0.f;  
  } 

  public static javax.swing.text.TabSet getTabSet(AttributeSet a)
  {
    if (a.isDefined(StyleConstants.TabSet))
      return (javax.swing.text.TabSet) a.getAttribute(StyleConstants.TabSet);
    else
      return (javax.swing.text.TabSet) null;
  } 

  public static boolean isBold(AttributeSet a)
  {
    if (a.isDefined(Bold))
      return ((Boolean) a.getAttribute(Bold)).booleanValue();
    else
      return false;    
  } 

  public static boolean isItalic(AttributeSet a)
  {
    if (a.isDefined(Italic))
      return ((Boolean) a.getAttribute(Italic)).booleanValue();
    else
      return false;    
  } 

  public static boolean isStrikeThrough(AttributeSet a)
  {
    if (a.isDefined(StrikeThrough))
      return ((Boolean) a.getAttribute(StrikeThrough)).booleanValue();
    else
      return false;    
  } 

  public static boolean isSubscript(AttributeSet a)
  {
    if (a.isDefined(Subscript))
      return ((Boolean) a.getAttribute(Subscript)).booleanValue();
    else
      return false;    
  } 

  public static boolean isSuperscript(AttributeSet a)
  {
    if (a.isDefined(Superscript))
      return ((Boolean) a.getAttribute(Superscript)).booleanValue();
    else
      return false;    
  } 

  public static boolean isUnderline(AttributeSet a)
  {
    if (a.isDefined(Underline))
      return ((Boolean) a.getAttribute(Underline)).booleanValue();
    else
      return false;    
  } 

  public static void setAlignment(MutableAttributeSet a, int align)
  {
    a.addAttribute(Alignment, new Integer(align));
  } 

  public static void setBackground(MutableAttributeSet a, Color fg)
  {
    a.addAttribute(Background, fg);
  } 

  public static void setBidiLevel(MutableAttributeSet a, int lev)
  {
    a.addAttribute(BidiLevel, new Integer(lev));
  } 

  public static void setBold(MutableAttributeSet a, boolean b)
  {
    a.addAttribute(Bold, Boolean.valueOf(b));
  } 
  
  public static void setComponent(MutableAttributeSet a, Component c)
  {
    a.addAttribute(ComponentAttribute, c);
  } 

  public static void setFirstLineIndent(MutableAttributeSet a, float i)
  {
    a.addAttribute(FirstLineIndent, new Float(i));
  } 

  public static void setFontFamily(MutableAttributeSet a, String fam)
  {
    a.addAttribute(FontFamily, fam);
  } 

  public static void setFontSize(MutableAttributeSet a, int s)
  {
    a.addAttribute(FontSize, new Integer(s));
  } 

  public static void setForeground(MutableAttributeSet a, Color fg)
  {
    a.addAttribute(Foreground, fg);
  }

  public static void setIcon(MutableAttributeSet a, Icon c)
  {
    a.addAttribute(IconAttribute, c);
  }
 
  public static void setItalic(MutableAttributeSet a, boolean b)
  {
    a.addAttribute(Italic, Boolean.valueOf(b));
  }
 
  public static void setLeftIndent(MutableAttributeSet a, float i)
  {
    a.addAttribute(LeftIndent, new Float(i));
  } 

  public static void setLineSpacing(MutableAttributeSet a, float i)
  {
    a.addAttribute(LineSpacing, new Float(i));
  } 

  public static void setRightIndent(MutableAttributeSet a, float i)
  {
    a.addAttribute(RightIndent, new Float(i));
  } 

  public static void setSpaceAbove(MutableAttributeSet a, float i)
  {
    a.addAttribute(SpaceAbove, new Float(i));
  } 

  public static void setSpaceBelow(MutableAttributeSet a, float i)
  {
    a.addAttribute(SpaceBelow, new Float(i));
  } 

  public static void setStrikeThrough(MutableAttributeSet a, boolean b)
  {
    a.addAttribute(StrikeThrough, Boolean.valueOf(b));
  } 

  public static void setSubscript(MutableAttributeSet a, boolean b)
  {
    a.addAttribute(Subscript, Boolean.valueOf(b));
  } 

  public static void setSuperscript(MutableAttributeSet a, boolean b)
  {
    a.addAttribute(Superscript, Boolean.valueOf(b));
  } 

  public static void setTabSet(MutableAttributeSet a, javax.swing.text.TabSet tabs)
  {
    a.addAttribute(StyleConstants.TabSet, tabs);
  } 

  public static void setUnderline(MutableAttributeSet a, boolean b)
  {
    a.addAttribute(Underline, Boolean.valueOf(b));
  } 

  // The remainder are so-called "typesafe enumerations" which 
  // alias subsets of the above constants.
  public static class CharacterConstants
    extends StyleConstants
    implements AttributeSet.CharacterAttribute
  {
    private CharacterConstants(String k) 
    {
      super(k);
    }
    
    public static Object Background = ColorConstants.Background;
    public static Object BidiLevel = new CharacterConstants("bidiLevel");
    public static Object Bold = FontConstants.Bold;
    public static Object ComponentAttribute = new CharacterConstants("component");
    public static Object Family = FontConstants.Family;
    public static Object Size = FontConstants.Size;
    public static Object Foreground = ColorConstants.Foreground;
    public static Object IconAttribute = new CharacterConstants("icon");
    public static Object Italic = FontConstants.Italic;
    public static Object StrikeThrough = new CharacterConstants("strikethrough");
    public static Object Subscript = new CharacterConstants("subscript");
    public static Object Superscript = new CharacterConstants("superscript");
    public static Object Underline = new CharacterConstants("underline");
  }

  public static class ColorConstants
    extends StyleConstants
    implements AttributeSet.ColorAttribute, AttributeSet.CharacterAttribute
  {
    private ColorConstants(String k) 
    {
      super(k);
    }
    public static Object Foreground = new ColorConstants("foreground");
    public static Object Background = new ColorConstants("background");
  }

  public static class FontConstants
    extends StyleConstants
    implements AttributeSet.FontAttribute, AttributeSet.CharacterAttribute
  {
    private FontConstants(String k) 
    {
      super(k);
    }
    public static Object Bold = new FontConstants("bold");
    public static Object Family = new FontConstants("family");
    public static Object Italic = new FontConstants("italic");
    public static Object Size = new FontConstants("size");
  }

  public static class ParagraphConstants
    extends StyleConstants
    implements AttributeSet.ParagraphAttribute
  {
    private ParagraphConstants(String k) 
    {
      super(k);
    }
    public static Object Alignment = new ParagraphConstants("Alignment");
    public static Object FirstLineIndent = new ParagraphConstants("FirstLineIndent");
    public static Object LeftIndent = new ParagraphConstants("LeftIndent");
    public static Object LineSpacing = new ParagraphConstants("LineSpacing");
    public static Object Orientation = new ParagraphConstants("Orientation");
    public static Object RightIndent = new ParagraphConstants("RightIndent");
    public static Object SpaceAbove = new ParagraphConstants("SpaceAbove");
    public static Object SpaceBelow = new ParagraphConstants("SpaceBelow");
    public static Object TabSet = new ParagraphConstants("TabSet");
  }

}
