/* CharacterAttributeTranslator.java -- 
   Copyright (C) 2006  Free Software Foundation, Inc.

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

package gnu.javax.swing.text.html;

import java.awt.Color;
import java.util.HashMap;
import java.util.StringTokenizer;

import javax.swing.text.MutableAttributeSet;
import javax.swing.text.StyleConstants;
import javax.swing.text.html.HTML.Attribute;
import javax.swing.text.html.HTML.Tag;

/**
 * This is a small utility class to translate HTML character attributes to
 * Swing StyleConstants
 */
public class CharacterAttributeTranslator
{
  /**
   * Maps color name to its hex encoding.
   */
  private static final HashMap colorMap = new HashMap();
  static 
  {
    colorMap.put("aqua" , "#00FFFF");
    colorMap.put("blue" , "#0000FF");
    colorMap.put("black", "#000000");
    colorMap.put("fuchsia" , "#FF00FF");
    colorMap.put("gray" , "#808080");
    colorMap.put("green" , "#008000");
    colorMap.put("lime" , "#00FF00");
    colorMap.put("maroon" , "#800000");
    colorMap.put("navy" , "#000080");
    colorMap.put("olive" , "#808000");
    colorMap.put("purple" , "#800080");
    colorMap.put("red" , "#FF0000");
    colorMap.put("silver" , "#C0C0C0");
    colorMap.put("teal" , "#008080");
    colorMap.put("white" , "#FFFFFF");
    colorMap.put("yellow" , "#FFFF00");
  }
  
  /**
   * Convert the color string represenation into java.awt.Color. The valid
   * values are like "aqua" , "#00FFFF" or "rgb(1,6,44)".
   * 
   * @param colorName the color to convert.
   * @return the matching java.awt.color
   */
  public static Color getColor(String colorName)
  {
    colorName = colorName.toLowerCase();
    try
      {
        if (colorName.startsWith("rgb"))
          {
            // rgb(red, green, blue) notation.
            StringTokenizer st = new StringTokenizer(colorName, " ,()");
            String representation = st.nextToken();

            // Return null if the representation is not supported.
            if (! representation.equals("rgb"))
              return null;
            int red = Integer.parseInt(st.nextToken());
            int green = Integer.parseInt(st.nextToken());
            int blue = Integer.parseInt(st.nextToken());

            return new Color(red, green, blue);
          }
        else
          {
            String s2 = (String) colorMap.get(colorName);
            if (s2 == null)
              s2 = colorName;
            return Color.decode(s2);
          }
      }
    catch (Exception nex)
      {
        // Can be either number format exception or illegal argument
        // exception.
        return null;
      }
  }
  
  /**
   * Translate the HTML character attribute to the Swing style constant.
   * 
   * @param charAttr the character attributes of the html tag
   * @param t the html tag itself
   * @param a the attribute set where the translated attributes will be stored
   * 
   * @return true if some attributes were translated, false otherwise.
   */
  public static boolean translateTag(MutableAttributeSet charAttr, 
				     Tag t, MutableAttributeSet a)
  {
    if(t == Tag.FONT)
      {
        Object color = a.getAttribute(Attribute.COLOR); 
	if(color != null)
	  {
	    Color c = getColor(color.toString());
	    if( c == null )
	      return false;
	    charAttr.addAttribute(StyleConstants.Foreground, c);
	    return true;
	  }

	if(a.getAttribute(Attribute.SIZE) != null)
	  {
	    // FIXME
	    //	    charAttr.addAttribute(StyleConstants.FontSize, 
	    //				  new java.lang.Integer(72));
	    return true;
	  }
      }

    if( t == Tag.B )
      {
	charAttr.addAttribute(StyleConstants.Bold, Boolean.TRUE);
	return true;
      }

    if( t == Tag.I )
      {
	charAttr.addAttribute(StyleConstants.Italic, Boolean.TRUE);
	return true;
      }

    if( t == Tag.U )
      {
	charAttr.addAttribute(StyleConstants.Underline, Boolean.TRUE);
	return true;
      }

    if( t == Tag.STRIKE )
      {
	charAttr.addAttribute(StyleConstants.StrikeThrough, Boolean.TRUE);
	return true;
      }

    if( t == Tag.SUP )
      {
	charAttr.addAttribute(StyleConstants.Superscript, Boolean.TRUE);
	return true;
      }

    if( t == Tag.SUB )
      {
	charAttr.addAttribute(StyleConstants.Subscript, Boolean.TRUE);
	return true;
      }
    return false;
  }
}
