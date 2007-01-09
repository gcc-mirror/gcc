/* HTML.java -- HTML document tag constants
   Copyright (C) 2002 Free Software Foundation, Inc.

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

import java.io.Serializable;

import java.lang.reflect.Field;
import java.lang.reflect.Modifier;

import java.util.Map;
import java.util.TreeMap;

import javax.swing.text.AttributeSet;

/**
 * HTML attribute and tag definitions.
 * @author Audrius Meskauskas (AudriusA@Bioinformatics.org)
 */
public class HTML
{
  /**
   * Represents a HTML attribute.
   */
  public static final class Attribute
  {
    /**
     * The action attribute
     */
    public static final Attribute ACTION = new Attribute("action");

    /**
     * The align attribute
     */
    public static final Attribute ALIGN = new Attribute("align");

    /**
     * The alink attribute
     */
    public static final Attribute ALINK = new Attribute("alink");

    /**
     * The alt attribute
     */
    public static final Attribute ALT = new Attribute("alt");

    /**
     * The archive attribute
     */
    public static final Attribute ARCHIVE = new Attribute("archive");

    /**
     * The background attribute
     */
    public static final Attribute BACKGROUND = new Attribute("background");

    /**
     * The bgcolor attribute
     */
    public static final Attribute BGCOLOR = new Attribute("bgcolor");

    /**
     * The border attribute
     */
    public static final Attribute BORDER = new Attribute("border");

    /**
     * The cellpadding attribute
     */
    public static final Attribute CELLPADDING = new Attribute("cellpadding");

    /**
     * The cellspacing attribute
     */
    public static final Attribute CELLSPACING = new Attribute("cellspacing");

    /**
     * The checked attribute
     */
    public static final Attribute CHECKED = new Attribute("checked");

    /**
     * The class attribute
     */
    public static final Attribute CLASS = new Attribute("class");

    /**
     * The classid attribute
     */
    public static final Attribute CLASSID = new Attribute("classid");

    /**
     * The clear attribute
     */
    public static final Attribute CLEAR = new Attribute("clear");

    /**
     * The code attribute
     */
    public static final Attribute CODE = new Attribute("code");

    /**
     * The codebase attribute
     */
    public static final Attribute CODEBASE = new Attribute("codebase");

    /**
     * The codetype attribute
     */
    public static final Attribute CODETYPE = new Attribute("codetype");

    /**
     * The color attribute
     */
    public static final Attribute COLOR = new Attribute("color");

    /**
     * The cols attribute
     */
    public static final Attribute COLS = new Attribute("cols");

    /**
     * The colspan attribute
     */
    public static final Attribute COLSPAN = new Attribute("colspan");

    /**
     * The comment attribute
     */
    public static final Attribute COMMENT = new Attribute("comment");

    /**
     * The compact attribute
     */
    public static final Attribute COMPACT = new Attribute("compact");

    /**
     * The content attribute
     */
    public static final Attribute CONTENT = new Attribute("content");

    /**
     * The coords attribute
     */
    public static final Attribute COORDS = new Attribute("coords");

    /**
     * The data attribute
     */
    public static final Attribute DATA = new Attribute("data");

    /**
     * The declare attribute
     */
    public static final Attribute DECLARE = new Attribute("declare");

    /**
     * The dir attribute
     */
    public static final Attribute DIR = new Attribute("dir");

    /**
     * The dummy attribute
     */
    public static final Attribute DUMMY = new Attribute("dummy");

    /**
     * The enctype attribute
     */
    public static final Attribute ENCTYPE = new Attribute("enctype");

    /**
     * The endtag attribute
     */
    public static final Attribute ENDTAG = new Attribute("endtag");

    /**
     *  The face attribute
     */
    public static final Attribute FACE = new Attribute("face");

    /**
     *  The frameborder attribute
     */
    public static final Attribute FRAMEBORDER = new Attribute("frameborder");

    /**
     *  The halign attribute
     */
    public static final Attribute HALIGN = new Attribute("halign");

    /**
     *  The height attribute
     */
    public static final Attribute HEIGHT = new Attribute("height");

    /**
     *  The href attribute
     */
    public static final Attribute HREF = new Attribute("href");

    /**
     *  The hspace attribute
     */
    public static final Attribute HSPACE = new Attribute("hspace");

    /**
     *  The http-equiv attribute
     */
    public static final Attribute HTTPEQUIV = new Attribute("http-equiv");

    /**
     *  The id attribute
     */
    public static final Attribute ID = new Attribute("id");

    /**
     *  The ismap attribute
     */
    public static final Attribute ISMAP = new Attribute("ismap");

    /**
     *  The lang attribute
     */
    public static final Attribute LANG = new Attribute("lang");

    /**
     *  The language attribute
     */
    public static final Attribute LANGUAGE = new Attribute("language");

    /**
     *  The link attribute
     */
    public static final Attribute LINK = new Attribute("link");

    /**
     *  The lowsrc attribute
     */
    public static final Attribute LOWSRC = new Attribute("lowsrc");

    /**
     *  The marginheight attribute
     */
    public static final Attribute MARGINHEIGHT = new Attribute("marginheight");

    /**
     *  The marginwidth attribute
     */
    public static final Attribute MARGINWIDTH = new Attribute("marginwidth");

    /**
     *  The maxlength attribute
     */
    public static final Attribute MAXLENGTH = new Attribute("maxlength");

    /**
     *  The media attribute
     */
    static final Attribute MEDIA = new Attribute("media");

    /**
     *  The method attribute
     */
    public static final Attribute METHOD = new Attribute("method");

    /**
     *  The multiple attribute
     */
    public static final Attribute MULTIPLE = new Attribute("multiple");

    /**
     *  The n attribute
     */
    public static final Attribute N = new Attribute("n");

    /**
     *  The name attribute
     */
    public static final Attribute NAME = new Attribute("name");

    /**
     *  The nohref attribute
     */
    public static final Attribute NOHREF = new Attribute("nohref");

    /**
     *  The noresize attribute
     */
    public static final Attribute NORESIZE = new Attribute("noresize");

    /**
     *  The noshade attribute
     */
    public static final Attribute NOSHADE = new Attribute("noshade");

    /**
     *  The nowrap attribute
     */
    public static final Attribute NOWRAP = new Attribute("nowrap");

    /**
     *  The prompt attribute
     */
    public static final Attribute PROMPT = new Attribute("prompt");

    /**
     *  The rel attribute
     */
    public static final Attribute REL = new Attribute("rel");

    /**
     *  The rev attribute
     */
    public static final Attribute REV = new Attribute("rev");

    /**
     *  The rows attribute
     */
    public static final Attribute ROWS = new Attribute("rows");

    /**
     *  The rowspan attribute
     */
    public static final Attribute ROWSPAN = new Attribute("rowspan");

    /**
     *  The scrolling attribute
     */
    public static final Attribute SCROLLING = new Attribute("scrolling");

    /**
     *  The selected attribute
     */
    public static final Attribute SELECTED = new Attribute("selected");

    /**
     *  The shape attribute
     */
    public static final Attribute SHAPE = new Attribute("shape");

    /**
     *  The shapes attribute
     */
    public static final Attribute SHAPES = new Attribute("shapes");

    /**
     *  The size attribute
     */
    public static final Attribute SIZE = new Attribute("size");

    /**
     *  The src attribute
     */
    public static final Attribute SRC = new Attribute("src");

    /**
     *  The standby attribute
     */
    public static final Attribute STANDBY = new Attribute("standby");

    /**
     *  The start attribute
     */
    public static final Attribute START = new Attribute("start");

    /**
     *  The style attribute
     */
    public static final Attribute STYLE = new Attribute("style");

    /**
     *  The target attribute
     */
    public static final Attribute TARGET = new Attribute("target");

    /**
     *  The text attribute
     */
    public static final Attribute TEXT = new Attribute("text");

    /**
     *  The title attribute
     */
    public static final Attribute TITLE = new Attribute("title");

    /**
     *  The type attribute
     */
    public static final Attribute TYPE = new Attribute("type");

    /**
     *  The usemap attribute
     */
    public static final Attribute USEMAP = new Attribute("usemap");

    /**
     *  The valign attribute
     */
    public static final Attribute VALIGN = new Attribute("valign");

    /**
     *  The value attribute
     */
    public static final Attribute VALUE = new Attribute("value");

    /**
     *  The valuetype attribute
     */
    public static final Attribute VALUETYPE = new Attribute("valuetype");

    /**
     *  The version attribute
     */
    public static final Attribute VERSION = new Attribute("version");

    /**
     *  The vlink attribute
     */
    public static final Attribute VLINK = new Attribute("vlink");

    /**
     *  The vspace attribute
     */
    public static final Attribute VSPACE = new Attribute("vspace");

    /**
     *  The width attribute
     */
    public static final Attribute WIDTH = new Attribute("width");

    /**
     * This is used to reflect the pseudo class for the a tag.
     */
    static final Attribute PSEUDO_CLASS = new Attribute("_pseudo");

    /**
     * This is used to reflect the dynamic class for the a tag.
     */
    static final Attribute DYNAMIC_CLASS = new Attribute("_dynamic");

    /**
     * The attribute name.
     */
    private final String name;

    /**
     * Creates the attribute with the given name.
     */
    private Attribute(String a_name)
    {
      name = a_name;
    }

    /**
     * Returns the attribute name. The names of the built-in attributes
     * are always returned in lowercase.
     */
    public String toString()
    {
      return name;
    }

    /**
     *  Return an array of all attributes, declared in the HTML.Attribute
     *  class. WARNING: attributes are the only public fields,
     *  expected in this class.
     */
    static Attribute[] getAllAttributes()
    {
      Field[] f = Attribute.class.getFields();
      Attribute[] attrs = new Attribute[ f.length ];
      Field x;
      int p = 0;
      Attribute a;

      for (int i = 0; i < f.length; i++)
        {
          x = f [ i ];

          if ((x.getModifiers() & Modifier.STATIC) != 0)
            {
              if (x.getType().equals(Attribute.class))
                {
                  try
                    {
                      a = (Attribute) x.get(null);
                      attrs [ p++ ] = a;
                    }
                  catch (Exception ex)
                    {
                      ex.printStackTrace(System.err);
                      throw new Error("This should never happen, report a bug");
                    }
                }
            }
        }

      return attrs;
    }
  }

  /**
   * Represents a HTML tag.
   */
  public static class Tag
  {
    /**
     * The &lt;a&gt; tag
     */
    public static final Tag A = new Tag("a");

    /**
     * The &lt;address&gt; tag
     */
    public static final Tag ADDRESS = new Tag("address");

    /**
     * The &lt;applet&gt; tag
     */
    public static final Tag APPLET = new Tag("applet");

    /**
     * The &lt;area&gt; tag
     */
    public static final Tag AREA = new Tag("area");

    /**
     * The &lt;b&gt; tag
     */
    public static final Tag B = new Tag("b");

    /**
     * The &lt;base&gt; tag
     */
    public static final Tag BASE = new Tag("base");

    /**
     * The &lt;basefont&gt; tag
     */
    public static final Tag BASEFONT = new Tag("basefont");

    /**
     * The &lt;big&gt; tag
     */
    public static final Tag BIG = new Tag("big");

    /**
     * The &lt;blockquote&gt; tag , breaks flow, block tag.
     */
    public static final Tag BLOCKQUOTE = new Tag("blockquote", BREAKS | BLOCK);

    /**
     * The &lt;body&gt; tag , breaks flow, block tag.
     */
    public static final Tag BODY = new Tag("body", BREAKS | BLOCK);

    /**
     * The &lt;br&gt; tag , breaks flow.
     */
    public static final Tag BR = new Tag("br", BREAKS);

    /**
     * The &lt;caption&gt; tag
     */
    public static final Tag CAPTION = new Tag("caption");

    /**
     * The &lt;center&gt; tag , breaks flow.
     */
    public static final Tag CENTER = new Tag("center", BREAKS);

    /**
     * The &lt;cite&gt; tag
     */
    public static final Tag CITE = new Tag("cite");

    /**
     * The &lt;code&gt; tag
     */
    public static final Tag CODE = new Tag("code");

    /**
     * The &lt;dd&gt; tag , breaks flow, block tag.
     */
    public static final Tag DD = new Tag("dd", BREAKS | BLOCK);

    /**
     * The &lt;dfn&gt; tag
     */
    public static final Tag DFN = new Tag("dfn");

    /**
     * The &lt;dir&gt; tag , breaks flow, block tag.
     */
    public static final Tag DIR = new Tag("dir", BREAKS | BLOCK);

    /**
     * The &lt;div&gt; tag , breaks flow, block tag.
     */
    public static final Tag DIV = new Tag("div", BREAKS | BLOCK);

    /**
     * The &lt;dl&gt; tag , breaks flow, block tag.
     */
    public static final Tag DL = new Tag("dl", BREAKS | BLOCK);

    /**
     * The &lt;dt&gt; tag , breaks flow, block tag.
     */
    public static final Tag DT = new Tag("dt", BREAKS | BLOCK);

    /**
     * The &lt;em&gt; tag
     */
    public static final Tag EM = new Tag("em");

    /**
     * The &lt;font&gt; tag
     */
    public static final Tag FONT = new Tag("font");

    /**
     * The &lt;form&gt; tag , breaks flow.
     */
    public static final Tag FORM = new Tag("form", BREAKS);

    /**
     * The &lt;frame&gt; tag
     */
    public static final Tag FRAME = new Tag("frame");

    /**
     * The &lt;frameset&gt; tag
     */
    public static final Tag FRAMESET = new Tag("frameset");

    /**
     * The &lt;h1&gt; tag , breaks flow, block tag.
     */
    public static final Tag H1 = new Tag("h1", BREAKS | BLOCK);

    /**
     * The &lt;h2&gt; tag , breaks flow, block tag.
     */
    public static final Tag H2 = new Tag("h2", BREAKS | BLOCK);

    /**
     * The &lt;h3&gt; tag , breaks flow, block tag.
     */
    public static final Tag H3 = new Tag("h3", BREAKS | BLOCK);

    /**
     * The &lt;h4&gt; tag , breaks flow, block tag.
     */
    public static final Tag H4 = new Tag("h4", BREAKS | BLOCK);

    /**
     * The &lt;h5&gt; tag , breaks flow, block tag.
     */
    public static final Tag H5 = new Tag("h5", BREAKS | BLOCK);

    /**
     * The &lt;h6&gt; tag , breaks flow, block tag.
     */
    public static final Tag H6 = new Tag("h6", BREAKS | BLOCK);

    /**
     * The &lt;head&gt; tag , breaks flow, block tag.
     */
    public static final Tag HEAD = new Tag("head", BREAKS | BLOCK);

    /**
     * The &lt;hr&gt; tag , breaks flow.
     */
    public static final Tag HR = new Tag("hr", BREAKS);

    /**
     * The &lt;html&gt; tag , breaks flow.
     */
    public static final Tag HTML = new Tag("html", BREAKS);

    /**
     * The &lt;i&gt; tag
     */
    public static final Tag I = new Tag("i");

    /**
     * The &lt;img&gt; tag
     */
    public static final Tag IMG = new Tag("img");

    /**
     * The &lt;input&gt; tag
     */
    public static final Tag INPUT = new Tag("input");

    /**
     * The &lt;isindex&gt; tag , breaks flow.
     */
    public static final Tag ISINDEX = new Tag("isindex", BREAKS);

    /**
     * The &lt;kbd&gt; tag
     */
    public static final Tag KBD = new Tag("kbd");

    /**
     * The &lt;li&gt; tag , breaks flow, block tag.
     */
    public static final Tag LI = new Tag("li", BREAKS | BLOCK);

    /**
     * The &lt;link&gt; tag
     */
    public static final Tag LINK = new Tag("link");

    /**
     * The &lt;map&gt; tag
     */
    public static final Tag MAP = new Tag("map");

    /**
     * The &lt;menu&gt; tag , breaks flow, block tag.
     */
    public static final Tag MENU = new Tag("menu", BREAKS | BLOCK);

    /**
     * The &lt;meta&gt; tag
     */
    public static final Tag META = new Tag("meta");

    /**
     * The &lt;nobr&gt; tag
     */
    static final Tag NOBR = new Tag("nobr");

    /**
     * The &lt;noframes&gt; tag , breaks flow, block tag.
     */
    public static final Tag NOFRAMES = new Tag("noframes", BREAKS | BLOCK);

    /**
     * The &lt;object&gt; tag
     */
    public static final Tag OBJECT = new Tag("object");

    /**
     * The &lt;ol&gt; tag , breaks flow, block tag.
     */
    public static final Tag OL = new Tag("ol", BREAKS | BLOCK);

    /**
     * The &lt;option&gt; tag
     */
    public static final Tag OPTION = new Tag("option");

    /**
     * The &lt;p&gt; tag , breaks flow, block tag.
     */
    public static final Tag P = new Tag("p", BREAKS | BLOCK);

    /**
     * The &lt;param&gt; tag
     */
    public static final Tag PARAM = new Tag("param");

    /**
     * The &lt;pre&gt; tag , breaks flow, block tag, preformatted.
     */
    public static final Tag PRE = new Tag("pre", BREAKS | BLOCK | PREFORMATTED);

    /**
     * The &lt;s&gt; tag
     */
    public static final Tag S = new Tag("s");

    /**
     * The &lt;samp&gt; tag
     */
    public static final Tag SAMP = new Tag("samp");

    /**
     * The &lt;script&gt; tag
     */
    public static final Tag SCRIPT = new Tag("script");

    /**
     * The &lt;select&gt; tag
     */
    public static final Tag SELECT = new Tag("select");

    /**
     * The &lt;small&gt; tag
     */
    public static final Tag SMALL = new Tag("small");

    /**
     * The &lt;span&gt; tag
     */
    public static final Tag SPAN = new Tag("span");

    /**
     * The &lt;strike&gt; tag
     */
    public static final Tag STRIKE = new Tag("strike");

    /**
     * The &lt;strong&gt; tag
     */
    public static final Tag STRONG = new Tag("strong");

    /**
     * The &lt;style&gt; tag
     */
    public static final Tag STYLE = new Tag("style");

    /**
     * The &lt;sub&gt; tag
     */
    public static final Tag SUB = new Tag("sub");

    /**
     * The &lt;sup&gt; tag
     */
    public static final Tag SUP = new Tag("sup");

    /**
     * The &lt;table&gt; tag , block tag.
     */
    public static final Tag TABLE = new Tag("table", BLOCK);

    /**
     * The &lt;td&gt; tag , breaks flow, block tag.
     */
    public static final Tag TD = new Tag("td", BREAKS | BLOCK);

    /**
     * The &lt;textarea&gt; tag , preformatted.
     */
    public static final Tag TEXTAREA = new Tag("textarea", PREFORMATTED);

    /**
     * The &lt;th&gt; tag , breaks flow, block tag.
     */
    public static final Tag TH = new Tag("th", BREAKS | BLOCK);

    /**
     * The &lt;title&gt; tag , breaks flow, block tag.
     */
    public static final Tag TITLE = new Tag("title", BREAKS | BLOCK);

    /**
     * The &lt;tr&gt; tag , block tag.
     */
    public static final Tag TR = new Tag("tr", BLOCK);

    /**
     * The &lt;tt&gt; tag
     */
    public static final Tag TT = new Tag("tt");

    /**
     * The &lt;u&gt; tag
     */
    public static final Tag U = new Tag("u");

    /**
     * The &lt;ul&gt; tag , breaks flow, block tag.
     */
    public static final Tag UL = new Tag("ul", BREAKS | BLOCK);

    /**
     * The &lt;var&gt; tag
     */
    public static final Tag VAR = new Tag("var");

    /* Special tags */

    /**
     * Total number of syntetic tags, delared in the Tag class.
     * This must be adjusted if the new synthetic tags are declared.
     * Otherwise the HTML.getAllTags() will not work as expected.
     */
    private static final int TOTAL_SYNTHETIC_TAGS = 3;

    /**
     * All comments are labeled with this tag.
     * This tag is not included into the array, returned by getAllTags().
     * toString() returns 'comment'. HTML reader synthesizes this tag.
     */
    public static final Tag COMMENT = new Tag("comment", SYNTHETIC);

    /**
     *  All text content is labeled with this tag.
     *  This tag is not included into the array, returned by getAllTags().
     *  toString() returns 'content'. HTML reader synthesizes this tag.
     */
    public static final Tag CONTENT = new Tag("content", SYNTHETIC);

    /**
     * All text content must be in a paragraph element.
     * If a paragraph didn't exist when content was encountered,
     * a paragraph is manufactured.
     * toString() returns 'p-implied'. HTML reader synthesizes this tag.
     */
    public static final Tag IMPLIED = new Tag("p-implied", SYNTHETIC);
    final String name;
    final int flags;

    /**
     * Create the unitialised instance of HTML.Tag.
     *
     * The {@link #breaksFlow()}, {@link #isBlock()}
     * and {@link #isPreformatted()} will always return false.
     * The {@link #toString()} will return <code>null</code>.
     *
     * @since 1.3
     */
    public Tag()
    {
      name = null;
      flags = 0;
    }

    /**
     * Creates a new Tag with the specified id, and with causesBreak
     * and isBlock set to false.
     */
    protected Tag(String id)
    {
      name = id;
      flags = 0;
    }

    /**
     * Creates a new Tag with the specified tag name and
     * causesBreak and isBlock properties.
     */
    protected Tag(String id, boolean causesBreak, boolean isBlock)
    {
      int f = 0;

      if (causesBreak)
        {
          f |= BREAKS;
        }

      if (isBlock)
        {
          f |= BLOCK;
        }

      flags = f;
      name = id;
    }

    /**
     * Create a tag taking flags.
     */
    Tag(String id, int a_flags)
    {
      name = id;
      flags = a_flags;
    }

    /**
     * Returns true if this tag is a block tag, which is a tag used to
     * add structure to a document.
     */
    public boolean isBlock()
    {
      return (flags & BLOCK) != 0;
    }

    /**
     * Returns true if this tag is pre-formatted, which is true if
     * the tag is either PRE or TEXTAREA
     */
    public boolean isPreformatted()
    {
      return (flags & PREFORMATTED) != 0;
    }

    /**
     * Returns true if this tag causes a line break to the flow of text
     */
    public boolean breaksFlow()
    {
      return (flags & BREAKS) != 0;
    }

    /**
     * Returns the tag name. The names of the built-in tags are always
     * returned in lowercase.
     */
    public String toString()
    {
      return name;
    }

    /**
     * Return an array of HTML tags, declared in HTML.Tag class.
     * WARNING: This method expects that the Tags are the only
     * public fields declared in the Tag class.
     */
    static Tag[] getAllTags()
    {
      Field[] f = Tag.class.getFields();
      Field x;

      // The syntetic tags are not included.
      Tag[] tags = new Tag[ f.length - TOTAL_SYNTHETIC_TAGS ];
      int p = 0;
      Tag t;

      for (int i = 0; i < f.length; i++)
        {
          x = f [ i ];

          if ((x.getModifiers() & Modifier.STATIC) != 0)
            {
              if (x.getType().equals(Tag.class))
                {
                  try
                    {
                      t = (Tag) x.get(null);

                      if (!t.isSyntetic())
                        {
                          tags [ p++ ] = t;
                        }
                    }
                  catch (IllegalAccessException ex)
                    {
                      unexpected(ex);
                    }
                  catch (IllegalArgumentException ex)
                    {
                      unexpected(ex);
                    }
                }
            }
        }

      return tags;
    }

    /**
     * Returns true for tags, generated by the html reader
     * (COMMENT, CONTENT and IMPLIED).
     */
    boolean isSyntetic()
    {
      return (flags & SYNTHETIC) != 0;
    }

    private static void unexpected(Exception ex)
                            throws Error
    {
      throw new Error("This should never happen, report a bug", ex);
    }
  }

  /**
   * Represents an unknown HTML tag.
   * @author Mark Wielaard (mark@klomp.org)
   */
  public static class UnknownTag
    extends Tag
    implements Serializable
  {
    private static final long serialVersionUID = -1534369342247250625L;

    /**
     * Creates a new UnknownTag with the specified name
     * @param name The tag name.
     *
     */
    public UnknownTag(String name)
    {
      super(name);
    }
  }

  /**
   * This value is returned for attributes without value that have no
   * default value defined in the DTD.
   */
  public static final String NULL_ATTRIBUTE_VALUE = "#DEFAULT";

  /* Package level html tag flags */
  static final int BREAKS = 1;
  static final int BLOCK = 2;
  static final int PREFORMATTED = 4;
  static final int SYNTHETIC = 8;
  private static Map<String,Tag> tagMap;
  private static Map<String,Attribute> attrMap;

  /**
   * The public constructor (does nothing). It it seldom required to have
   * an instance of this class, because all public fields and methods
   * are static.
   */
  public HTML()
  {
    // Nothing to do here.
  }

  /**
   * Returns the set of the recognized HTML attributes.
   */
  public static HTML.Attribute[] getAllAttributeKeys()
  {
    return Attribute.getAllAttributes();
  }

  /**
   * Returns the set of actual HTML tags that are recognized by
   * the default HTML reader. The returned array does not include the
   * COMMENT, CONTENT and IMPLIED tags.
   */
  public static HTML.Tag[] getAllTags()
  {
    return Tag.getAllTags();
  }

  /**
   * Returns an htl attribute constant for the given attribute name.
   * @param attName the attribute name, case insensitive
   */
  public static Attribute getAttributeKey(String attName)
  {
    if (attrMap == null)
      {
        // Create the map on demand.
        attrMap = new TreeMap<String,Attribute>();

        Attribute[] attrs = getAllAttributeKeys();

        for (int i = 0; i < attrs.length; i++)
          {
            attrMap.put(attrs [ i ].toString(), attrs [ i ]);
          }
      }

    return attrMap.get(attName.toLowerCase());
  }

  /**
   * Searches the value of given attribute in the provided set.
   * If the value is found (String type expected), tries to parse it as
   * an integer value. If succeded, returns the obtained integer value.
   *
   * For example:<p><code>
   * SimpleAttributeSet ase = new SimpleAttributeSet();
   * ase.addAttribute(HTML.getAttributeKey("size"),"222");
   * System.out.println(
   *  HTML.getIntegerAttributeValue
   *     (ase, HTML.getAttributeKey("size"), 333)); // prints "222"
   * System.out.println(
   *  HTML.getIntegerAttributeValue
   *     (ase, HTML.getAttributeKey("width"), 333)); // prints "333".
   * </code></p>
   *
   *
   * @param set The attribute set to search in. If the set contains the
   * given attribute, it must by a type of String.
   * @param attribute The html attribute to search in
   * @param defaultValue The value that is returned if the attribute is not
   * found in the given set or if the NumberFormatException was thrown
   * during the parsing.
   */
  public static int getIntegerAttributeValue(AttributeSet set,
                                             HTML.Attribute attribute,
                                             int defaultValue
                                            )
  {
    Object v = set.getAttribute(attribute);

    if (v == null)
      {
        return defaultValue;
      }

    try
      {
        return Integer.parseInt(v.toString().trim());
      }
    catch (Exception ex)
      {
        return defaultValue;
      }
  }

  /**
   * Returns a HTML tag constant for the given HTML attribute name.
   * If the tag is unknown, the null is returned.
   * @param tagName the tag name, case insensitive
   */
  public static Tag getTag(String tagName)
  {
    if (tagMap == null)
      {
        // Create the mao on demand.
        tagMap = new TreeMap<String,Tag>();

        Tag[] tags = getAllTags();

        for (int i = 0; i < tags.length; i++)
          {
            tagMap.put(tags [ i ].toString(), tags [ i ]);
          }
      }

    return tagMap.get(tagName.toLowerCase());
  }
}
