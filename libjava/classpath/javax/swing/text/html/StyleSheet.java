/* StyleSheet.java -- 
   Copyright (C) 2005 Free Software Foundation, Inc.

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


package javax.swing.text.html;

import java.awt.Color;
import java.awt.Font;
import java.awt.Graphics;

import java.io.IOException;
import java.io.Reader;
import java.io.Serializable;
import java.io.StringReader;

import java.net.MalformedURLException;
import java.net.URL;

import java.util.Enumeration;
import java.util.Vector;

import javax.swing.text.AttributeSet;
import javax.swing.text.Element;
import javax.swing.text.MutableAttributeSet;
import javax.swing.text.SimpleAttributeSet;
import javax.swing.text.Style;
import javax.swing.text.StyleContext;
import javax.swing.text.View;


/**
 * This class adds support for defining the visual characteristics of HTML views
 * being rendered. This enables views to be customized by a look-and-feel, mulitple
 * views over the same model can be rendered differently. Each EditorPane has its 
 * own StyleSheet, but by default one sheet will be shared by all of the HTMLEditorKit
 * instances. An HTMLDocument can also have a StyleSheet, which holds specific CSS
 * specs. 
 * 
 *  In order for Views to store less state and therefore be more lightweight, 
 *  the StyleSheet can act as a factory for painters that handle some of the 
 *  rendering tasks. Since the StyleSheet may be used by views over multiple
 *  documents the HTML attributes don't effect the selector being used.
 *  
 *  The rules are stored as named styles, and other information is stored to 
 *  translate the context of an element to a rule.
 * 
 * @author Lillian Angel (langel@redhat.com)
 */
public class StyleSheet extends StyleContext
{

  /** The base URL */
  URL base;
  
  /** Base font size (int) */
  int baseFontSize;
  
  /** The style sheets stored. */
  StyleSheet[] styleSheet;
  
  /**
   * Constructs a StyleSheet.
   */
  public StyleSheet()
  {
    super();
    baseFontSize = 4; // Default font size from CSS
  }

  /**
   * Gets the style used to render the given tag. The element represents the tag
   * and can be used to determine the nesting, where the attributes will differ
   * if there is nesting inside of elements.
   * 
   * @param t - the tag to translate to visual attributes
   * @param e - the element representing the tag
   * @return the set of CSS attributes to use to render the tag.
   */
  public Style getRule(HTML.Tag t, Element e)
  {
    // FIXME: Not implemented.
    return null;
  }
  
  /**
   * Gets the rule that best matches the selector. selector is a space
   * separated String of element names. The attributes of the returned 
   * Style will change as rules are added and removed.
   * 
   * @param selector - the element names separated by spaces
   * @return the set of CSS attributes to use to render
   */
  public Style getRule(String selector)
  {
    // FIXME: Not implemented.
    return null; 
  }
  
  /**
   * Adds a set if rules to the sheet. The rules are expected to be in valid
   * CSS format. This is called as a result of parsing a <style> tag
   * 
   * @param rule - the rule to add to the sheet
   */
  public void addRule(String rule)
  {
    CssParser cp = new CssParser();
    try
    {
      cp.parse(base, new StringReader(rule), false, false);
    }
    catch (IOException io)
    {
      // Do nothing here.
    }
  }
  
  /**
   * Translates a CSS declaration into an AttributeSet. This is called
   * as a result of encountering an HTML style attribute.
   * 
   * @param decl - the declaration to get
   * @return the AttributeSet representing the declaration
   */
  public AttributeSet getDeclaration(String decl)
  {
    if (decl == null)
      return SimpleAttributeSet.EMPTY;
    // FIXME: Not implemented.
    return null;     
  }
  
  /**
   * Loads a set of rules that have been specified in terms of CSS grammar.
   * If there are any conflicts with existing rules, the new rule is added.
   * 
   * @param in - the stream to read the CSS grammar from.
   * @param ref - the reference URL. It is the location of the stream, it may
   * be null. All relative URLs specified in the stream will be based upon this
   * parameter.
   * @throws IOException - For any IO error while reading
   */
  public void loadRules(Reader in, URL ref) throws IOException
  {
    CssParser cp = new CssParser();
    cp.parse(ref, in, false, false);
  }
  
  /**
   * Gets a set of attributes to use in the view. This is a set of
   * attributes that can be used for View.getAttributes
   * 
   * @param v - the view to get the set for
   * @return the AttributeSet to use in the view.
   */
  public AttributeSet getViewAttributes(View v)
  {
    // FIXME: Not implemented.
    return null;
  }
  
  /**
   * Removes a style previously added.
   * 
   * @param nm - the name of the style to remove
   */
  public void removeStyle(String nm)
  {
    // FIXME: Not implemented.
    super.removeStyle(nm);
  }
  
  /**
   * Adds the rules from ss to those of the receiver. ss's rules will
   * override the old rules. An added StyleSheet will never override the rules
   * of the receiving style sheet.
   * 
   * @param ss - the new StyleSheet.
   */
  public void addStyleSheet(StyleSheet ss)
  {
    if (styleSheet == null)
      styleSheet = new StyleSheet[] {ss};
    else
      System.arraycopy(new StyleSheet[] {ss}, 0, styleSheet, 
                       styleSheet.length, 1);
  }
  
  /**
   * Removes ss from those of the receiver
   * 
   * @param ss - the StyleSheet to remove.
   */
  public void removeStyleSheet(StyleSheet ss)
  {
    if (styleSheet.length == 1 && styleSheet[0].equals(ss))
      styleSheet = null;
    else
      {
        for (int i = 0; i < styleSheet.length; i++)
          {
            StyleSheet curr = styleSheet[i];
            if (curr.equals(ss))
              {
                StyleSheet[] tmp = new StyleSheet[styleSheet.length - 1];
                if (i != 0 && i != (styleSheet.length - 1))
                  {
                    System.arraycopy(styleSheet, 0, tmp, 0, i);
                    System.arraycopy(styleSheet, i + 1, tmp, i,
                                     styleSheet.length - i - 1);
                  }
                else if (i == 0)
                  System.arraycopy(styleSheet, 1, tmp, 0, styleSheet.length - 1);
                else
                  System.arraycopy(styleSheet, 0, tmp, 0, styleSheet.length - 1);
                
                styleSheet = tmp;
                break;
              }
          }
      }
  }
  
  /**
   * Returns an array of the linked StyleSheets. May return null.
   * 
   * @return - An array of the linked StyleSheets.
   */
  public StyleSheet[] getStyleSheets()
  {
    return styleSheet;
  }
  
  /**
   * Imports a style sheet from the url. The rules are directly added to the
   * receiver.
   * 
   * @param url - the URL to import the StyleSheet from.
   */
  public void importStyleSheet(URL url)
  {
    // FIXME: Not implemented
  }
  
  /**
   * Sets the base url. All import statements that are relative, will be
   * relative to base.
   * 
   * @param base -
   *          the base URL.
   */
  public void setBase(URL base)
  {
    this.base = base;
  }
  
  /**
   * Gets the base url.
   * 
   * @return - the base
   */
  public URL getBase()
  {
    return base;
  }
  
  /**
   * Adds a CSS attribute to the given set.
   * 
   * @param attr - the attribute set
   * @param key - the attribute to add
   * @param value - the value of the key
   */
  public void addCSSAttribute(MutableAttributeSet attr, CSS.Attribute key,
                              String value)
  {
    attr.addAttribute(key, value);
  }
  
  /**
   * Adds a CSS attribute to the given set.
   * This method parses the value argument from HTML based on key. 
   * Returns true if it finds a valid value for the given key, 
   * and false otherwise.
   * 
   * @param attr - the attribute set
   * @param key - the attribute to add
   * @param value - the value of the key
   * @return true if a valid value was found.
   */
  public boolean addCSSAttributeFromHTML(MutableAttributeSet attr, CSS.Attribute key,
                                         String value)
  {
    // FIXME: Need to parse value from HTML based on key.
    attr.addAttribute(key, value);
    return attr.containsAttribute(key, value);
  }
  
  /**
   * Converts a set of HTML attributes to an equivalent set of CSS attributes.
   * 
   * @param htmlAttrSet - the set containing the HTML attributes.
   * @return the set of CSS attributes
   */
  public AttributeSet translateHTMLToCSS(AttributeSet htmlAttrSet)
  {
    // FIXME: Not implemented.
    return null;    
  }

  /**
   * Adds an attribute to the given set and returns a new set. This is implemented
   * to convert StyleConstants attributes to CSS before forwarding them to the superclass.
   * The StyleConstants attribute do not have corresponding CSS entry, the attribute
   * is stored (but will likely not be used).
   * 
   * @param old - the old set
   * @param key - the non-null attribute key
   * @param value - the attribute value
   * @return the updated set 
   */
  public AttributeSet addAttribute(AttributeSet old, Object key,
                                   Object value)
  {
    // FIXME: Not implemented.
    return super.addAttribute(old, key, value);       
  }
  
  /**
   * Adds a set of attributes to the element. If any of these attributes are
   * StyleConstants, they will be converted to CSS before forwarding to the 
   * superclass.
   * 
   * @param old - the old set
   * @param attr - the attributes to add
   * @return the updated attribute set
   */
  public AttributeSet addAttributes(AttributeSet old, AttributeSet attr)
  {
    // FIXME: Not implemented.
    return super.addAttributes(old, attr);           
  }
  
  /**
   * Removes an attribute from the set. If the attribute is a
   * StyleConstants, it will be converted to CSS before forwarding to the 
   * superclass.
   * 
   * @param old - the old set
   * @param key - the non-null attribute key
   * @return the updated set 
   */
  public AttributeSet removeAttribute(AttributeSet old, Object key)
  {
    // FIXME: Not implemented.
    return super.removeAttribute(old, key);    
  }
  
  /**
   * Removes an attribute from the set. If any of the attributes are
   * StyleConstants, they will be converted to CSS before forwarding to the 
   * superclass.
   * 
   * @param old - the old set
   * @param attrs - the attributes to remove
   * @return the updated set 
   */
  public AttributeSet removeAttributes(AttributeSet old, AttributeSet attrs)
  {
    // FIXME: Not implemented.
    return super.removeAttributes(old, attrs);    
  }
  
  /**
   * Removes a set of attributes for the element. If any of the attributes is a
   * StyleConstants, they will be converted to CSS before forwarding to the 
   * superclass.
   * 
   * @param old - the old attribute set
   * @param names - the attribute names
   * @return the update attribute set
   */
  public AttributeSet removeAttributes(AttributeSet old, Enumeration names)
  {
    // FIXME: Not implemented.
    return super.removeAttributes(old, names);        
  }
  
  /**
   * Creates a compact set of attributes that might be shared. This is a hook
   * for subclasses that want to change the behaviour of SmallAttributeSet.
   * 
   * @param a - the set of attributes to be represented in the compact form.
   * @return the set of attributes created
   */
  protected StyleContext.SmallAttributeSet createSmallAttributeSet(AttributeSet a)
  {
    return super.createSmallAttributeSet(a);     
  }
  
  /**
   * Creates a large set of attributes. This set is not shared. This is a hook
   * for subclasses that want to change the behaviour of the larger attribute
   * storage format.
   * 
   * @param a - the set of attributes to be represented in the larger form.
   * @return the large set of attributes.
   */
  protected MutableAttributeSet createLargeAttributeSet(AttributeSet a)
  {
    return super.createLargeAttributeSet(a);     
  }
  
  /**
   * Gets the font to use for the given set.
   * 
   * @param a - the set to get the font for.
   * @return the font for the set
   */
  public Font getFont(AttributeSet a)
  {
    return super.getFont(a);    
  }
  
  /**
   * Takes a set of attributes and turns it into a foreground
   * color specification. This is used to specify things like, brigher, more hue
   * etc.
   * 
   * @param a - the set to get the foreground color for
   * @return the foreground color for the set
   */
  public Color getForeground(AttributeSet a)
  {
    return super.getForeground(a);     
  }
  
  /**
   * Takes a set of attributes and turns it into a background
   * color specification. This is used to specify things like, brigher, more hue
   * etc.
   * 
   * @param a - the set to get the background color for
   * @return the background color for the set
   */
  public Color getBackground(AttributeSet a)
  {
    return super.getBackground(a);     
  }
  
  /**
   * Gets the box formatter to use for the given set of CSS attributes.
   * 
   * @param a - the given set
   * @return the box formatter
   */
  public BoxPainter getBoxPainter(AttributeSet a)
  {
    return new BoxPainter(a);     
  }
  
  /**
   * Gets the list formatter to use for the given set of CSS attributes.
   * 
   * @param a - the given set
   * @return the list formatter
   */
  public ListPainter getListPainter(AttributeSet a)
  {
    return new ListPainter(a);         
  }
  
  /**
   * Sets the base font size between 1 and 7.
   * 
   * @param sz - the new font size for the base.
   */
  public void setBaseFontSize(int sz)
  {
    if (sz <= 7 && sz >= 1)
      baseFontSize = sz;
  }
  
  /**
   * Sets the base font size from the String. It can either identify
   * a specific font size (between 1 and 7) or identify a relative
   * font size such as +1 or -2.
   * 
   * @param size - the new font size as a String.
   */
  public void setBaseFontSize(String size)
  {
    size.trim();
    int temp = 0;
    try
      {
        if (size.length() == 2)
          {
            int i = new Integer(size.substring(1)).intValue();
            if (size.startsWith("+"))
              temp = baseFontSize + i;
            else if (size.startsWith("-"))
              temp = baseFontSize - i;
          }
        else if (size.length() == 1)
          temp = new Integer(size.substring(0)).intValue();

        if (temp <= 7 && temp >= 1)
          baseFontSize = temp;
      }
    catch (NumberFormatException nfe)
      {
        // Do nothing here
      }
  }
  
  /**
   * TODO
   * 
   * @param pt - TODO
   * @return TODO
   */
  public static int getIndexOfSize(float pt)
  {
    // FIXME: Not implemented.
    return 0;
  }
  
  /**
   * Gets the point size, given a size index.
   * 
   * @param index - the size index
   * @return the point size.
   */
  public float getPointSize(int index)
  {
    // FIXME: Not implemented.
    return 0;    
  }
  
  /**
   * Given the string of the size, returns the point size value.
   * 
   * @param size - the string representation of the size.
   * @return - the point size value.
   */
  public float getPointSize(String size)
  {
    // FIXME: Not implemented.
    return 0;    
  }
  
  /**
   * Converst a color string to a color. If it is not found, null is returned.
   * 
   * @param color - the color string such as "RED" or "#NNNNNN"
   * @return the Color, or null if not found.
   */
  public Color stringToColor(String color)
  {
    color = color.toLowerCase();
    if (color.equals("black") || color.equals("#000000"))
      return Color.BLACK;
    else if (color.equals("aqua") || color.equals("#00FFFF"))
      return new Color(127, 255, 212);
    else if (color.equals("gray") || color.equals("#808080"))
      return Color.GRAY;
    else if (color.equals("navy") || color.equals("#000080"))
      return new Color(0, 0, 128);
    else if (color.equals("silver") || color.equals("#C0C0C0"))
      return Color.LIGHT_GRAY;
    else if (color.equals("green") || color.equals("#008000"))
      return Color.GREEN;
    else if (color.equals("olive") || color.equals("#808000"))
      return new Color(128, 128, 0);
    else if (color.equals("teal") || color.equals("#008080"))
      return new Color(0, 128, 128);
    else if (color.equals("blue") || color.equals("#0000FF"))
      return Color.BLUE;
    else if (color.equals("lime") || color.equals("#00FF00"))
      return new Color(0, 255, 0);
    else if (color.equals("purple") || color.equals("#800080"))
      return new Color(128, 0, 128);
    else if (color.equals("white") || color.equals("#FFFFFF"))
      return Color.WHITE;
    else if (color.equals("fuchsia") || color.equals("#FF00FF"))
      return Color.MAGENTA;
    else if (color.equals("maroon") || color.equals("#800000"))
      return new Color(128, 0, 0);
    else if (color.equals("Red") || color.equals("#FF0000"))
      return Color.RED;
    else if (color.equals("Yellow") || color.equals("#FFFF00"))
      return Color.YELLOW;
    return null; 
  }
  
  /**
   * This class carries out some of the duties of CSS formatting. This enables views
   * to present the CSS formatting while not knowing how the CSS values are cached.
   * 
   * This object is reponsible for the insets of a View and making sure
   * the background is maintained according to the CSS attributes.
   * 
   * @author Lillian Angel (langel@redhat.com)
   */
  public static class BoxPainter extends Object implements Serializable
  {
    
    /**
     * Attribute set for painter
     */
    AttributeSet as;
    
    /**
     * Package-private constructor.
     * 
     * @param as - AttributeSet for painter
     */
    BoxPainter(AttributeSet as)
    {
      this.as = as;
    }
    
    /**
     * Gets the inset needed on a given side to account for the margin, border
     * and padding.
     * 
     * @param size - the size of the box to get the inset for. View.TOP, View.LEFT,
     * View.BOTTOM or View.RIGHT.
     * @param v - the view making the request. This is used to get the AttributeSet,
     * amd may be used to resolve percentage arguments.
     * @return the inset
     * @throws IllegalArgumentException - for an invalid direction.
     */
    public float getInset(int size, View v)
    {
      // FIXME: Not implemented.
      return 0;       
    }
    
    /**
     * Paints the CSS box according to the attributes given. This should
     * paint the border, padding and background.
     * 
     * @param g - the graphics configuration
     * @param x - the x coordinate
     * @param y - the y coordinate
     * @param w - the width of the allocated area
     * @param h - the height of the allocated area
     * @param v - the view making the request
     */
    public void paint(Graphics g, float x, float y, float w, float h, View v)
    {
      // FIXME: Not implemented.
    }
  }
  
  /**
   * This class carries out some of the CSS list formatting duties. Implementations
   * of this class enable views to present the CSS formatting while not knowing anything
   * about how the CSS values are being cached.
   * 
   * @author Lillian Angel (langel@redhat.com)
   */
  public static class ListPainter extends Object implements Serializable
  {
    
    /**
     * Attribute set for painter
     */
    AttributeSet as;
    
    /**
     * Package-private constructor.
     * 
     * @param as - AttributeSet for painter
     */
    ListPainter(AttributeSet as)
    {
      this.as = as;
    }
    
    /**
     * Paints the CSS list decoration according to the attributes given.
     * 
     * @param g - the graphics configuration
     * @param x - the x coordinate
     * @param y - the y coordinate
     * @param w - the width of the allocated area
     * @param h - the height of the allocated area
     * @param v - the view making the request
     * @param item - the list item to be painted >=0.
     */
    public void paint(Graphics g, float x, float y, float w, float h, View v,
                      int item)
    {
      // FIXME: Not implemented.
    }
  }
  
  /**
   * The parser callback for the CSSParser.
   */
  class CssParser implements CSSParser.CSSParserCallback
  {
    /** 
     * A vector of all the selectors. 
     * Each element is an array of all the selector tokens 
     * in a single rule. 
     */
    Vector selectors;

    /** A vector of all the selector tokens in a rule. */
    Vector selectorTokens;

    /**  Name of the current property. */
    String propertyName;

    /** The set of CSS declarations */
    MutableAttributeSet declaration;

    /** 
     * True if parsing a declaration, that is the Reader will not 
     * contain a selector. 
     */
    boolean parsingDeclaration;

    /** True if the attributes are coming from a linked/imported style. */
    boolean isLink;

    /** The base URL */
    URL base;

    /** The parser */
    CSSParser parser;

    /**
     * Constructor
     */
    CssParser()
    {
      selectors = new Vector();
      selectorTokens = new Vector();
      parser = new CSSParser();
      base = StyleSheet.this.base;
      declaration = new SimpleAttributeSet();
    }

    /**
     * Parses the passed in CSS declaration into an AttributeSet.
     * 
     * @param s - the declaration
     * @return the set of attributes containing the property and value.
     */
    public AttributeSet parseDeclaration(String s)
    {
      try
      {
        return parseDeclaration(new StringReader(s));
      }
      catch (IOException e)
      {
         // Do nothing here.
      }
      return null;
    }

    /**
     * Parses the passed in CSS declaration into an AttributeSet.
     * 
     * @param r - the reader
     * @return the attribute set
     * @throws IOException from the reader
     */
    public AttributeSet parseDeclaration(Reader r) throws IOException
    {
      parse(base, r, true, false);
      return declaration;
    }

    /**
     * Parse the given CSS stream
     * 
     * @param base - the url
     * @param r - the reader
     * @param parseDec - True if parsing a declaration
     * @param isLink - True if parsing a link
     */
   public void parse(URL base, Reader r, boolean parseDec, boolean isLink) throws IOException
   {
     parsingDeclaration = parseDec;
     this.isLink = isLink;
     this.base = base;
     
     // flush out all storage
     propertyName = null;
     selectors.clear();
     selectorTokens.clear();
     declaration.removeAttributes(declaration);
     
     parser.parse(r, this, parseDec);
   }

   /**
    * Invoked when a valid @import is encountered, 
    * will call importStyleSheet if a MalformedURLException 
    * is not thrown in creating the URL.
    *
    * @param s - the string after @import
    */ 
   public void handleImport(String s)
    {
      if (s != null)
        {
          try
            {
              if (s.startsWith("url(") && s.endsWith(")"))
                s = s.substring(4, s.length() - 1);
              if (s.indexOf("\"") >= 0)
                s = s.replaceAll("\"","");

              URL url = new URL(s);
              if (url == null && base != null)
                url = new URL(base, s);
              
              importStyleSheet(url);
            }
          catch (MalformedURLException e)
            {
              // Do nothing here.
            }
        }
    }

   /**
     * A selector has been encountered.
     * 
     * @param s - a selector (e.g. P or UL or even P,)
     */
   public void handleSelector(String s)
   {
     if (s.endsWith(","))
       s = s.substring(0, s.length() - 1);
     
     selectorTokens.addElement(s);
     addSelector();
   }

   /**
    * Invoked when the start of a rule is encountered.
    */
   public void startRule()
   {
     addSelector();
   }

   /**
    * Invoked when a property name is encountered.
    *
    * @param s - the property
    */
   public void handleProperty(String s)
   {
     propertyName = s;
   }

  /**
   * Invoked when a property value is encountered.
   *
   * @param s - the value
   */
   public void handleValue(String s)
   {
     // call addCSSAttribute
     // FIXME: Not implemented
   }
   
   /**
    * Invoked when the end of a rule is encountered.
    */
   public void endRule()
   {
     // FIXME: Not implemented
     // add rules
     propertyName = null;
   }

   /**
    * Adds the selector to the vector.
    */
   private void addSelector()
   {
     int length = selectorTokens.size();
     if (length > 0)
       {
         Object[] sel = new Object[length];
         System.arraycopy(selectorTokens.toArray(), 0, sel, 0, length);
         selectors.add(sel);
         selectorTokens.clear();
       }
   }
  }
}
