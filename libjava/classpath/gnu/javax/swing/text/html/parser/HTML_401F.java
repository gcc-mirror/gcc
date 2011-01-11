/* HTML_401F.java -- HTML 4.01 FRAMESET DTD java conception.
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


package gnu.javax.swing.text.html.parser;

import gnu.javax.swing.text.html.parser.models.PCDATAonly_model;
import gnu.javax.swing.text.html.parser.models.TableRowContentModel;
import gnu.javax.swing.text.html.parser.models.noTagModel;

import java.io.IOException;
import java.io.Serializable;

import javax.swing.text.html.parser.*;
import javax.swing.text.html.parser.ContentModel;
import javax.swing.text.html.parser.DTDConstants;

/**
 * This class represents the java implementation of the HTML 4.01
 * ( -//W3C//DTD HTML 4.01 Frameset//EN ) Frameset version. The
 * Frameset version includes as recommended, as obsoleted features and
 * also the frameset support. This the default DTD to parse HTML
 * documents in this implementation, containing 315 pre-defined general
 * entities and 92 elements.
 *
 * @author Audrius Meskauskas, Lithuania (AudriusA@Bioinformatics.org)
 */
public class HTML_401F
  extends gnuDTD
  implements DTDConstants, Serializable
{
   private static final long serialVersionUID = 1;

   /**
    * The standard name of this DTD,
    * '-//W3C//DTD HTML 4.01 Frameset//EN'
    */
   public static final String DTD_NAME = "-//W3C//DTD HTML 4.01 Frameset//EN";

   /**
    * The integer representing length in pixels.
    */
    static final int PIXELS = NUMBER;

    static final String[] NONE = new String[0];

  /* Define the HTML tags. */
    static final String PCDATA = "#pcdata";
    static final String A = "a";
    static final String ABBR = "abbr";
    static final String ACRONYM = "acronym";
    static final String ADDRESS = "address";
    static final String APPLET = "applet";
    static final String AREA = "area";
    static final String B = "b";
    static final String BASE = "base";
    static final String BASEFONT = "basefont";
    static final String BDO = "bdo";
    static final String BIG = "big";
    static final String BLOCKQUOTE = "blockquote";
    static final String BODY = "body";
    static final String BR = "br";
    static final String BUTTON = "button";
    static final String CAPTION = "caption";
    static final String CENTER = "center";
    static final String CITE = "cite";
    static final String CODE = "code";
    static final String COL = "col";
    static final String COLGROUP = "colgroup";
    static final String DEFAULTS = "default";
    static final String DD = "dd";
    static final String DEL = "del";
    static final String DFN = "dfn";
    static final String DIR = "dir";
    static final String DIV = "div";
    static final String DL = "dl";
    static final String DT = "dt";
    static final String EM = "em";
    static final String FIELDSET = "fieldset";
    static final String FONT = "font";
    static final String FORM = "form";
    static final String FRAME = "frame";
    static final String FRAMESET = "frameset";
    static final String H1 = "h1";
    static final String H2 = "h2";
    static final String H3 = "h3";
    static final String H4 = "h4";
    static final String H5 = "h5";
    static final String H6 = "h6";
    static final String HEAD = "head";
    static final String HR = "hr";
    static final String HTML = "html";
    static final String I = "i";
    static final String IFRAME = "iframe";
    static final String IMG = "img";
    static final String INPUT = "input";
    static final String INS = "ins";
    static final String ISINDEX = "isindex";
    static final String KBD = "kbd";
    static final String LABEL = "label";
    static final String LEGEND = "legend";
    static final String LI = "li";
    static final String LINK = "link";
    static final String MAP = "map";
    static final String MENU = "menu";
    static final String META = "meta";
    static final String NOFRAMES = "noframes";
    static final String NOSCRIPT = "noscript";
    static final String NONES    = "none";
    static final String sNAME    = "name";
    static final String OBJECT = "object";
    static final String OL = "ol";
    static final String OPTGROUP = "optgroup";
    static final String OPTION = "option";
    static final String P = "p";
    static final String PARAM = "param";
    static final String PRE = "pre";
    static final String Q = "q";
    static final String S = "s";
    static final String SAMP = "samp";
    static final String SCRIPT = "script";
    static final String SELECT = "select";
    static final String SMALL = "small";
    static final String SPAN = "span";
    static final String STRIKE = "strike";
    static final String STRONG = "strong";
    static final String STYLE = "style";
    static final String SUB = "sub";
    static final String SUP = "sup";
    static final String TABLE = "table";
    static final String TBODY = "tbody";
    static final String TD = "td";
    static final String TEXTAREA = "textarea";
    static final String TFOOT = "tfoot";
    static final String TH = "th";
    static final String THEAD = "thead";
    static final String TITLE = "title";
    static final String TR = "tr";
    static final String TT = "tt";
    static final String U = "u";
    static final String UL = "ul";
    static final String VAR = "var";

  /* Define the attribute constants. */
    static final String C_0 = "0";
    static final String C_1 = "1";
    static final String CHECKBOX = "checkbox";
    static final String DATA = "data";
    static final String FILE = "file";
    static final String GET = "get";
    static final String HIDDEN = "hidden";
    static final String IMAGE = "image";
    static final String PASSWORD = "password";
    static final String POST = "post";
    static final String RADIO = "radio";
    static final String REF = "ref";
    static final String RESET = "reset";
    static final String SUBMIT = "submit";
    static final String TEXT = "text";
    static final String ABOVE = "above";
    static final String ACCEPT = "accept";
    static final String ACCEPTCHARSET = "accept-charset";
    static final String ACCESSKEY = "accesskey";
    static final String ACTION = "action";
    static final String ALIGN = "align";
    static final String ALINK = "alink";
    static final String ALL = "all";
    static final String ALT = "alt";
    static final String APPLICATION_X_WWW_FORM_URLENCODED
     = "application/x-www-form-urlencoded";
    static final String ARCHIVE = "archive";
    static final String AUTO = "auto";
    static final String AXIS = "axis";
    static final String BACKGROUND = "background";
    static final String BASELINE = "baseline";
    static final String BELOW = "below";
    static final String BGCOLOR = "bgcolor";
    static final String BORDER = "border";
    static final String BOTTOM = "bottom";
    static final String BOX = "box";
    static final String CELLPADDING = "cellpadding";
    static final String CELLSPACING = "cellspacing";
    static final String CHAR = "char";
    static final String CHAROFF = "charoff";
    static final String CHARSET = "charset";
    static final String CHECKED = "checked";
    static final String CIRCLE = "circle";
    static final String CLASS = "class";
    static final String CLASSID = "classid";
    static final String CLEAR = "clear";
    static final String CODEBASE = "codebase";
    static final String CODETYPE = "codetype";
    static final String COLOR = "color";
    static final String COLS = "cols";
    static final String COLSPAN = "colspan";
    static final String COMPACT = "compact";
    static final String CONTENT = "content";
    static final String COORDS = "coords";
    static final String DATAPAGESIZE = "datapagesize";
    static final String DATETIME = "datetime";
    static final String DECLARE = "declare";
    static final String DEFER = "defer";
    static final String DISABLED = "disabled";
    static final String DISC = "disc";
    static final String ENCTYPE = "enctype";
    static final String EVENT = "event";
    static final String FACE = "face";
    static final String FOR = "for";
    static final String FRAMEBORDER = "frameborder";
    static final String GROUPS = "groups";
    static final String HEADERS = "headers";
    static final String HEIGHT = "height";
    static final String HREF = "href";
    static final String HREFLANG = "hreflang";
    static final String HSIDES = "hsides";
    static final String HSPACE = "hspace";
    static final String HTTPEQUIV = "http-equiv";
    static final String sID = "id";
    static final String ISMAP = "ismap";
    static final String JUSTIFY = "justify";
    static final String LANG = "lang";
    static final String LANGUAGE = "language";
    static final String LEFT = "left";
    static final String LHS = "lhs";
    static final String LONGDESC = "longdesc";
    static final String LTR = "ltr";
    static final String MARGINHEIGHT = "marginheight";
    static final String MARGINWIDTH = "marginwidth";
    static final String MAXLENGTH = "maxlength";
    static final String MEDIA = "media";
    static final String METHOD = "method";
    static final String MIDDLE = "middle";
    static final String MULTIPLE = "multiple";
    static final String NO = "no";
    static final String NOHREF = "nohref";
    static final String NORESIZE = "noresize";
    static final String NOSHADE = "noshade";
    static final String NOWRAP = "nowrap";
    static final String ONBLUR = "onblur";
    static final String ONCHANGE = "onchange";
    static final String ONCLICK = "onclick";
    static final String ONDBLCLICK = "ondblclick";
    static final String ONFOCUS = "onfocus";
    static final String ONKEYDOWN = "onkeydown";
    static final String ONKEYPRESS = "onkeypress";
    static final String ONKEYUP = "onkeyup";
    static final String ONLOAD = "onload";
    static final String ONMOUSEDOWN = "onmousedown";
    static final String ONMOUSEMOVE = "onmousemove";
    static final String ONMOUSEOUT = "onmouseout";
    static final String ONMOUSEOVER = "onmouseover";
    static final String ONMOUSEUP = "onmouseup";
    static final String ONRESET = "onreset";
    static final String ONSELECT = "onselect";
    static final String ONSUBMIT = "onsubmit";
    static final String ONUNLOAD = "onunload";
    static final String POLY = "poly";
    static final String PROFILE = "profile";
    static final String PROMPT = "prompt";
    static final String READONLY = "readonly";
    static final String RECT = "rect";
    static final String REL = "rel";
    static final String REV = "rev";
    static final String RHS = "rhs";
    static final String RIGHT = "right";
    static final String ROW = "row";
    static final String ROWGROUP = "rowgroup";
    static final String ROWS = "rows";
    static final String ROWSPAN = "rowspan";
    static final String RTL = "rtl";
    static final String RULES = "rules";
    static final String SCHEME = "scheme";
    static final String SCOPE = "scope";
    static final String SCROLLING = "scrolling";
    static final String SELECTED = "selected";
    static final String SHAPE = "shape";
    static final String SIZE = "size";
    static final String SQUARE = "square";
    static final String SRC = "src";
    static final String STANDBY = "standby";
    static final String START = "start";
    static final String SUMMARY = "summary";
    static final String TABINDEX = "tabindex";
    static final String TARGET = "target";
    static final String TOP = "top";
    static final String TYPE = "type";
    static final String USEMAP = "usemap";
    static final String VALIGN = "valign";
    static final String VALUE = "value";
    static final String VALUETYPE = "valuetype";
    static final String VERSION = "version";
    static final String VLINK = "vlink";
    static final String VOID = "void";
    static final String VSIDES = "vsides";
    static final String VSPACE = "vspace";
    static final String WIDTH = "width";
    static final String YES = "yes";

    static final String[] BLOCK =
    new String[] {
      ADDRESS, BLOCKQUOTE, CENTER, DIR,
      DIV, DL, FIELDSET, FORM,
      H1, H2, H3, H4, H5, H6,
      HR, ISINDEX, MENU, NOFRAMES, NOSCRIPT,
      OL, P, PRE, TABLE, UL
    };

   /**
   * Creates this DTD, filling in the entities and attributes data
   * as defined in -//W3C//DTD HTML 4.01 Frameset//EN.
   */
  protected HTML_401F()
  {
    super(DTD_NAME);
    defineEntities();
    defineElements();
  }

  /**
   * Either takes the document (by name) from DTD table, or
   * creates a new instance and registers it in the tabe.
   * The document is registerd under name "-//W3C//DTD HTML 4.01 Frameset//EN".
   * @return The new or existing DTD for parsing HTML 4.01 Frameset.
   */
  public static DTD getInstance()
  {
    try
      {
        DTD dtd = getDTD(DTD_NAME);
        if (dtd == null || dtd.getClass().equals(DTD.class))
          {
            dtd = new HTML_401F();
            putDTDHash(DTD_NAME, dtd);
          }
        return dtd;
      }
    catch (IOException ex)
      {
        throw new Error("This should never happen. Report the bug.", ex);
      }
  }

  /**
   * Define all elements of this DTD.
   */
  protected void defineElements()
  {
    /* Define the elements.  This used to be one huge method, which
       unfortunately took too long to compile and consumed
       too much memory while compiling it.  While it can serve as
       a good stress test for gcj, it is better to split it up
       to save time and memory used during GCC bootstrap.  */
    defineElements1();
    defineElements2();
    defineElements3();
    defineElements4();
    defineElements5();
    defineElements6();
  }

  /**
   * Define first sixth of elements of this DTD.
   */
  private void defineElements1()
  {
    /* Define the elements. */
      defElement(PCDATA, 0, false, false, null, NONE, NONE,
        new AttributeList[ 0 ]);

      defElement(A, 0, false, false, null,
      new String[] {
        A
      }
      ,
      new String[] {
        PCDATA, ABBR, ACRONYM, APPLET,
        B, BASEFONT, BDO, BIG, BR,
        BUTTON, CITE, CODE, DFN, EM,
        FONT, I, IFRAME, IMG, INPUT,
        KBD, LABEL, MAP, OBJECT, Q,
        S, SAMP, SCRIPT, SELECT, SMALL,
        SPAN, STRIKE, STRONG, SUB, SUP,
        TEXTAREA, TT, U, VAR
      }
    ,
      new AttributeList[] {
        attr(sID, null, null, ID, IMPLIED),
        attr(CLASS, null, null, 0, IMPLIED),
        attr(STYLE, null, null, 0, IMPLIED),
        attr(TITLE, null, null, 0, IMPLIED),
        attr(LANG, null, null, 0, IMPLIED),
        attr(DIR, null,  new String[] { LTR, RTL }, 0, IMPLIED),
        attr(ONCLICK, null, null, 0, IMPLIED),
        attr(ONDBLCLICK, null, null, 0, IMPLIED),
        attr(ONMOUSEDOWN, null, null, 0, IMPLIED),
        attr(ONMOUSEUP, null, null, 0, IMPLIED),
        attr(ONMOUSEOVER, null, null, 0, IMPLIED),
        attr(ONMOUSEMOVE, null, null, 0, IMPLIED),
        attr(ONMOUSEOUT, null, null, 0, IMPLIED),
        attr(ONKEYPRESS, null, null, 0, IMPLIED),
        attr(ONKEYDOWN, null, null, 0, IMPLIED),
        attr(ONKEYUP, null, null, 0, IMPLIED),
        attr(CHARSET, null, null, 0, IMPLIED),
        attr(TYPE, null, null, 0, IMPLIED),
        attr(sNAME, null, null, 0, IMPLIED),
        attr(HREF, null, null, 0, IMPLIED),
        attr(HREFLANG, null, null, 0, IMPLIED),
        attr(TARGET, null, null, 0, IMPLIED),
        attr(REL, null, null, 0, IMPLIED),
        attr(REV, null, null, 0, IMPLIED),
        attr(ACCESSKEY, null, null, 0, IMPLIED),
        attr(SHAPE, RECT,  new String[] { RECT, CIRCLE, POLY,  DEFAULTS },
          0, DEFAULT),
        attr(COORDS, null, null, 0, IMPLIED),
        attr(TABINDEX, null, null, NUMBER, IMPLIED),
        attr(ONFOCUS, null, null, 0, IMPLIED),
        attr(ONBLUR, null, null, 0, IMPLIED)
      }
    );
      defElement(ABBR, 0, false, false, null,
      NONE
      ,
      new String[] {
        PCDATA, A, ABBR, ACRONYM,
        APPLET, B, BASEFONT, BDO, BIG,
        BR, BUTTON, CITE, CODE, DFN,
        EM, FONT, I, IFRAME, IMG,
        INPUT, KBD, LABEL, MAP, OBJECT,
        Q, S, SAMP, SCRIPT, SELECT,
        SMALL, SPAN, STRIKE, STRONG, SUB,
        SUP, TEXTAREA, TT, U, VAR
      }
    ,
      new AttributeList[] {
        attr(sID, null, null, ID, IMPLIED),
        attr(CLASS, null, null, 0, IMPLIED),
        attr(STYLE, null, null, 0, IMPLIED),
        attr(TITLE, null, null, 0, IMPLIED),
        attr(LANG, null, null, 0, IMPLIED),
        attr(DIR, null,  new String[] { LTR, RTL }, 0, IMPLIED),
        attr(ONCLICK, null, null, 0, IMPLIED),
        attr(ONDBLCLICK, null, null, 0, IMPLIED),
        attr(ONMOUSEDOWN, null, null, 0, IMPLIED),
        attr(ONMOUSEUP, null, null, 0, IMPLIED),
        attr(ONMOUSEOVER, null, null, 0, IMPLIED),
        attr(ONMOUSEMOVE, null, null, 0, IMPLIED),
        attr(ONMOUSEOUT, null, null, 0, IMPLIED),
        attr(ONKEYPRESS, null, null, 0, IMPLIED),
        attr(ONKEYDOWN, null, null, 0, IMPLIED),
        attr(ONKEYUP, null, null, 0, IMPLIED)
      }
    );
      defElement(ACRONYM, 0, false, false, null,
      NONE
      ,
      new String[] {
        PCDATA, A, ABBR, ACRONYM,
        APPLET, B, BASEFONT, BDO, BIG,
        BR, BUTTON, CITE, CODE, DFN,
        EM, FONT, I, IFRAME, IMG,
        INPUT, KBD, LABEL, MAP, OBJECT,
        Q, S, SAMP, SCRIPT, SELECT,
        SMALL, SPAN, STRIKE, STRONG, SUB,
        SUP, TEXTAREA, TT, U, VAR
      }
    ,
      new AttributeList[] {
        attr(sID, null, null, ID, IMPLIED),
        attr(CLASS, null, null, 0, IMPLIED),
        attr(STYLE, null, null, 0, IMPLIED),
        attr(TITLE, null, null, 0, IMPLIED),
        attr(LANG, null, null, 0, IMPLIED),
        attr(DIR, null,  new String[] { LTR, RTL }, 0, IMPLIED),
        attr(ONCLICK, null, null, 0, IMPLIED),
        attr(ONDBLCLICK, null, null, 0, IMPLIED),
        attr(ONMOUSEDOWN, null, null, 0, IMPLIED),
        attr(ONMOUSEUP, null, null, 0, IMPLIED),
        attr(ONMOUSEOVER, null, null, 0, IMPLIED),
        attr(ONMOUSEMOVE, null, null, 0, IMPLIED),
        attr(ONMOUSEOUT, null, null, 0, IMPLIED),
        attr(ONKEYPRESS, null, null, 0, IMPLIED),
        attr(ONKEYDOWN, null, null, 0, IMPLIED),
        attr(ONKEYUP, null, null, 0, IMPLIED)
      }
    );
      defElement(ADDRESS, 0, false, false, null,
      NONE
      ,
      new String[] {
        PCDATA, A, ABBR, ACRONYM,
        APPLET, B, BASEFONT, BDO, BIG,
        BR, BUTTON, CITE, CODE, DFN,
        EM, FONT, I, IFRAME, IMG,
        INPUT, KBD, LABEL, MAP, OBJECT,
        Q, S, SAMP, SCRIPT, SELECT,
        SMALL, SPAN, STRIKE, STRONG, SUB,
        SUP, TEXTAREA, TT, U, VAR,
        P
      }
    ,
      new AttributeList[] {
        attr(sID, null, null, ID, IMPLIED),
        attr(CLASS, null, null, 0, IMPLIED),
        attr(STYLE, null, null, 0, IMPLIED),
        attr(TITLE, null, null, 0, IMPLIED),
        attr(LANG, null, null, 0, IMPLIED),
        attr(DIR, null,  new String[] { LTR, RTL }, 0, IMPLIED),
        attr(ONCLICK, null, null, 0, IMPLIED),
        attr(ONDBLCLICK, null, null, 0, IMPLIED),
        attr(ONMOUSEDOWN, null, null, 0, IMPLIED),
        attr(ONMOUSEUP, null, null, 0, IMPLIED),
        attr(ONMOUSEOVER, null, null, 0, IMPLIED),
        attr(ONMOUSEMOVE, null, null, 0, IMPLIED),
        attr(ONMOUSEOUT, null, null, 0, IMPLIED),
        attr(ONKEYPRESS, null, null, 0, IMPLIED),
        attr(ONKEYDOWN, null, null, 0, IMPLIED),
        attr(ONKEYUP, null, null, 0, IMPLIED)
      }
    );
      defElement(APPLET, 0, false, false, null,
      NONE
      ,
      new String[] {
        PCDATA, A, ABBR, ACRONYM,
        APPLET, B, BASEFONT, BDO, BIG,
        BR, BUTTON, CITE, CODE, DFN,
        EM, FONT, I, IFRAME, IMG,
        INPUT, KBD, LABEL, MAP, OBJECT,
        Q, S, SAMP, SCRIPT, SELECT,
        SMALL, SPAN, STRIKE, STRONG, SUB,
        SUP, TEXTAREA, TT, U, VAR,
        ADDRESS, BLOCKQUOTE, CENTER, DIR, DIV,
        DL, FIELDSET, FORM, H1, H2,
        H3, H4, H5, H6, HR,
        ISINDEX, MENU, NOFRAMES, NOSCRIPT, OL,
        P, PRE, TABLE, UL, PARAM
      }
    ,
      new AttributeList[] {
        attr(sID, null, null, ID, IMPLIED),
        attr(CLASS, null, null, 0, IMPLIED),
        attr(STYLE, null, null, 0, IMPLIED),
        attr(TITLE, null, null, 0, IMPLIED),
        attr(CODEBASE, null, null, 0, IMPLIED),
        attr(ARCHIVE, null, null, 0, IMPLIED),
        attr(CODE, null, null, 0, IMPLIED),
        attr(OBJECT, null, null, 0, IMPLIED),
        attr(ALT, null, null, 0, IMPLIED),
        attr(sNAME, null, null, 0, IMPLIED),
        attr(WIDTH, null, null, 0, REQUIRED),
        attr(HEIGHT, null, null, 0, REQUIRED),
        attr(ALIGN, null,  new String[] { TOP, MIDDLE, BOTTOM, LEFT, RIGHT },
          0, IMPLIED),
        attr(HSPACE, null, null, 0, IMPLIED),
        attr(VSPACE, null, null, 0, IMPLIED)
      }
    );
      defElement(AREA, EMPTY, false, true, null,
      NONE
      ,
      NONE
    ,
      new AttributeList[] {
        attr(sID, null, null, ID, IMPLIED),
        attr(CLASS, null, null, 0, IMPLIED),
        attr(STYLE, null, null, 0, IMPLIED),
        attr(TITLE, null, null, 0, IMPLIED),
        attr(LANG, null, null, 0, IMPLIED),
        attr(DIR, null,  new String[] { LTR, RTL }, 0, IMPLIED),
        attr(ONCLICK, null, null, 0, IMPLIED),
        attr(ONDBLCLICK, null, null, 0, IMPLIED),
        attr(ONMOUSEDOWN, null, null, 0, IMPLIED),
        attr(ONMOUSEUP, null, null, 0, IMPLIED),
        attr(ONMOUSEOVER, null, null, 0, IMPLIED),
        attr(ONMOUSEMOVE, null, null, 0, IMPLIED),
        attr(ONMOUSEOUT, null, null, 0, IMPLIED),
        attr(ONKEYPRESS, null, null, 0, IMPLIED),
        attr(ONKEYDOWN, null, null, 0, IMPLIED),
        attr(ONKEYUP, null, null, 0, IMPLIED),
        attr(SHAPE, RECT,  new String[] { RECT, CIRCLE, POLY, DEFAULTS },
          0, DEFAULT),
        attr(COORDS, null, null, 0, IMPLIED),
        attr(HREF, null, null, 0, IMPLIED),
        attr(TARGET, null, null, 0, IMPLIED),
        attr(NOHREF, null,  new String[] { NOHREF }, 0, IMPLIED),
        attr(ALT, null, null, 0, REQUIRED),
        attr(TABINDEX, null, null, NUMBER, IMPLIED),
        attr(ACCESSKEY, null, null, 0, IMPLIED),
        attr(ONFOCUS, null, null, 0, IMPLIED),
        attr(ONBLUR, null, null, 0, IMPLIED)
      }
    );
      defElement(B, 0, false, false, null,
      NONE
      ,
      new String[] {
        PCDATA, A, ABBR, ACRONYM,
        APPLET, B, BASEFONT, BDO, BIG,
        BR, BUTTON, CITE, CODE, DFN,
        EM, FONT, I, IFRAME, IMG,
        INPUT, KBD, LABEL, MAP, OBJECT,
        Q, S, SAMP, SCRIPT, SELECT,
        SMALL, SPAN, STRIKE, STRONG, SUB,
        SUP, TEXTAREA, TT, U, VAR
      }
    ,
      new AttributeList[] {
        attr(sID, null, null, ID, IMPLIED),
        attr(CLASS, null, null, 0, IMPLIED),
        attr(STYLE, null, null, 0, IMPLIED),
        attr(TITLE, null, null, 0, IMPLIED),
        attr(LANG, null, null, 0, IMPLIED),
        attr(DIR, null,  new String[] { LTR, RTL }, 0, IMPLIED),
        attr(ONCLICK, null, null, 0, IMPLIED),
        attr(ONDBLCLICK, null, null, 0, IMPLIED),
        attr(ONMOUSEDOWN, null, null, 0, IMPLIED),
        attr(ONMOUSEUP, null, null, 0, IMPLIED),
        attr(ONMOUSEOVER, null, null, 0, IMPLIED),
        attr(ONMOUSEMOVE, null, null, 0, IMPLIED),
        attr(ONMOUSEOUT, null, null, 0, IMPLIED),
        attr(ONKEYPRESS, null, null, 0, IMPLIED),
        attr(ONKEYDOWN, null, null, 0, IMPLIED),
        attr(ONKEYUP, null, null, 0, IMPLIED)
      }
    );
      defElement(BASE, EMPTY, false, true, null,
      NONE
      ,
      NONE
    ,
      new AttributeList[] {
        attr(HREF, null, null, 0, IMPLIED),
        attr(TARGET, null, null, 0, IMPLIED)
      }
    );
      defElement(BASEFONT, EMPTY, false, true, null,
      NONE
      ,
      NONE
    ,
      new AttributeList[] {
        attr(sID, null, null, ID, IMPLIED),
        attr(SIZE, null, null, 0, REQUIRED),
        attr(COLOR, null, null, 0, IMPLIED),
        attr(FACE, null, null, 0, IMPLIED)
      }
    );
      defElement(BDO, 0, false, false, null,
      NONE
      ,
      new String[] {
        PCDATA, A, ABBR, ACRONYM,
        APPLET, B, BASEFONT, BDO, BIG,
        BR, BUTTON, CITE, CODE, DFN,
        EM, FONT, I, IFRAME, IMG,
        INPUT, KBD, LABEL, MAP, OBJECT,
        Q, S, SAMP, SCRIPT, SELECT,
        SMALL, SPAN, STRIKE, STRONG, SUB,
        SUP, TEXTAREA, TT, U, VAR
      }
    ,
      new AttributeList[] {
        attr(sID, null, null, ID, IMPLIED),
        attr(CLASS, null, null, 0, IMPLIED),
        attr(STYLE, null, null, 0, IMPLIED),
        attr(TITLE, null, null, 0, IMPLIED),
        attr(LANG, null, null, 0, IMPLIED),
        attr(DIR, null,  new String[] { LTR, RTL }, 0, REQUIRED)
      }
    );
      defElement(BIG, 0, false, false, null,
      NONE
      ,
      new String[] {
        PCDATA, A, ABBR, ACRONYM,
        APPLET, B, BASEFONT, BDO, BIG,
        BR, BUTTON, CITE, CODE, DFN,
        EM, FONT, I, IFRAME, IMG,
        INPUT, KBD, LABEL, MAP, OBJECT,
        Q, S, SAMP, SCRIPT, SELECT,
        SMALL, SPAN, STRIKE, STRONG, SUB,
        SUP, TEXTAREA, TT, U, VAR
      }
    ,
      new AttributeList[] {
        attr(sID, null, null, ID, IMPLIED),
        attr(CLASS, null, null, 0, IMPLIED),
        attr(STYLE, null, null, 0, IMPLIED),
        attr(TITLE, null, null, 0, IMPLIED),
        attr(LANG, null, null, 0, IMPLIED),
        attr(DIR, null,  new String[] { LTR, RTL }, 0, IMPLIED),
        attr(ONCLICK, null, null, 0, IMPLIED),
        attr(ONDBLCLICK, null, null, 0, IMPLIED),
        attr(ONMOUSEDOWN, null, null, 0, IMPLIED),
        attr(ONMOUSEUP, null, null, 0, IMPLIED),
        attr(ONMOUSEOVER, null, null, 0, IMPLIED),
        attr(ONMOUSEMOVE, null, null, 0, IMPLIED),
        attr(ONMOUSEOUT, null, null, 0, IMPLIED),
        attr(ONKEYPRESS, null, null, 0, IMPLIED),
        attr(ONKEYDOWN, null, null, 0, IMPLIED),
        attr(ONKEYUP, null, null, 0, IMPLIED)
      }
    );
      defElement(BLOCKQUOTE, 0, false, false, null,
      NONE
      ,
      new String[] {
        PCDATA, A, ABBR, ACRONYM,
        APPLET, B, BASEFONT, BDO, BIG,
        BR, BUTTON, CITE, CODE, DFN,
        EM, FONT, I, IFRAME, IMG,
        INPUT, KBD, LABEL, MAP, OBJECT,
        Q, S, SAMP, SCRIPT, SELECT,
        SMALL, SPAN, STRIKE, STRONG, SUB,
        SUP, TEXTAREA, TT, U, VAR,
        ADDRESS, BLOCKQUOTE, CENTER, DIR, DIV,
        DL, FIELDSET, FORM, H1, H2,
        H3, H4, H5, H6, HR,
        ISINDEX, MENU, NOFRAMES, NOSCRIPT, OL,
        P, PRE, TABLE, UL
      }
    ,
      new AttributeList[] {
        attr(sID, null, null, ID, IMPLIED),
        attr(CLASS, null, null, 0, IMPLIED),
        attr(STYLE, null, null, 0, IMPLIED),
        attr(TITLE, null, null, 0, IMPLIED),
        attr(LANG, null, null, 0, IMPLIED),
        attr(DIR, null,  new String[] { LTR, RTL }, 0, IMPLIED),
        attr(ONCLICK, null, null, 0, IMPLIED),
        attr(ONDBLCLICK, null, null, 0, IMPLIED),
        attr(ONMOUSEDOWN, null, null, 0, IMPLIED),
        attr(ONMOUSEUP, null, null, 0, IMPLIED),
        attr(ONMOUSEOVER, null, null, 0, IMPLIED),
        attr(ONMOUSEMOVE, null, null, 0, IMPLIED),
        attr(ONMOUSEOUT, null, null, 0, IMPLIED),
        attr(ONKEYPRESS, null, null, 0, IMPLIED),
        attr(ONKEYDOWN, null, null, 0, IMPLIED),
        attr(ONKEYUP, null, null, 0, IMPLIED),
        attr(CITE, null, null, 0, IMPLIED)
      }
    );
      defElement(BODY, 0, true, true, null,
      NONE
      ,
      getBodyElements()
      ,
      new AttributeList[] {
        attr(sID, null, null, ID, IMPLIED),
        attr(CLASS, null, null, 0, IMPLIED),
        attr(STYLE, null, null, 0, IMPLIED),
        attr(TITLE, null, null, 0, IMPLIED),
        attr(LANG, null, null, 0, IMPLIED),
        attr(DIR, null,  new String[] { LTR, RTL }, 0, IMPLIED),
        attr(ONCLICK, null, null, 0, IMPLIED),
        attr(ONDBLCLICK, null, null, 0, IMPLIED),
        attr(ONMOUSEDOWN, null, null, 0, IMPLIED),
        attr(ONMOUSEUP, null, null, 0, IMPLIED),
        attr(ONMOUSEOVER, null, null, 0, IMPLIED),
        attr(ONMOUSEMOVE, null, null, 0, IMPLIED),
        attr(ONMOUSEOUT, null, null, 0, IMPLIED),
        attr(ONKEYPRESS, null, null, 0, IMPLIED),
        attr(ONKEYDOWN, null, null, 0, IMPLIED),
        attr(ONKEYUP, null, null, 0, IMPLIED),
        attr(ONLOAD, null, null, 0, IMPLIED),
        attr(ONUNLOAD, null, null, 0, IMPLIED),
        attr(BACKGROUND, null, null, 0, IMPLIED),
        attr(BGCOLOR, null, null, 0, IMPLIED),
        attr(TEXT, null, null, 0, IMPLIED),
        attr(LINK, null, null, 0, IMPLIED),
        attr(VLINK, null, null, 0, IMPLIED),
        attr(ALINK, null, null, 0, IMPLIED)
      }
    );
      defElement(BR, EMPTY, false, true, null,
      NONE
      ,
      NONE
    ,
      new AttributeList[] {
        attr(sID, null, null, ID, IMPLIED),
        attr(CLASS, null, null, 0, IMPLIED),
        attr(STYLE, null, null, 0, IMPLIED),
        attr(TITLE, null, null, 0, IMPLIED),
        attr(CLEAR, "NONE",  new String[] { LEFT, ALL, RIGHT, NONES },
          0, DEFAULT)
      }
    );
      defElement(BUTTON, 0, false, false, null,
      new String[] {
        A, BUTTON, IFRAME, INPUT,
        LABEL, SELECT, TEXTAREA, FIELDSET, FORM,
        ISINDEX
      }
      ,
      new String[] {
        PCDATA, ABBR, ACRONYM, APPLET,
        B, BASEFONT, BDO, BIG, BR,
        CITE, CODE, DFN, EM, FONT,
        I, IMG, KBD, MAP, OBJECT,
        Q, S, SAMP, SCRIPT, SMALL,
        SPAN, STRIKE, STRONG, SUB, SUP,
        TT, U, VAR, ADDRESS, BLOCKQUOTE,
        CENTER, DIR, DIV, DL, H1,
        H2, H3, H4, H5, H6,
        HR, MENU, NOFRAMES, NOSCRIPT, OL,
        P, PRE, TABLE, UL
      }
    ,
      new AttributeList[] {
        attr(sID, null, null, ID, IMPLIED),
        attr(CLASS, null, null, 0, IMPLIED),
        attr(STYLE, null, null, 0, IMPLIED),
        attr(TITLE, null, null, 0, IMPLIED),
        attr(LANG, null, null, 0, IMPLIED),
        attr(DIR, null,  new String[] { LTR, RTL }, 0, IMPLIED),
        attr(ONCLICK, null, null, 0, IMPLIED),
        attr(ONDBLCLICK, null, null, 0, IMPLIED),
        attr(ONMOUSEDOWN, null, null, 0, IMPLIED),
        attr(ONMOUSEUP, null, null, 0, IMPLIED),
        attr(ONMOUSEOVER, null, null, 0, IMPLIED),
        attr(ONMOUSEMOVE, null, null, 0, IMPLIED),
        attr(ONMOUSEOUT, null, null, 0, IMPLIED),
        attr(ONKEYPRESS, null, null, 0, IMPLIED),
        attr(ONKEYDOWN, null, null, 0, IMPLIED),
        attr(ONKEYUP, null, null, 0, IMPLIED),
        attr(sNAME, null, null, 0, IMPLIED),
        attr(VALUE, null, null, 0, IMPLIED),
        attr(TYPE, SUBMIT,  new String[] { BUTTON, SUBMIT, RESET }, 0, DEFAULT),
        attr(DISABLED, null,  new String[] { DISABLED }, 0, IMPLIED),
        attr(TABINDEX, null, null, NUMBER, IMPLIED),
        attr(ACCESSKEY, null, null, 0, IMPLIED),
        attr(ONFOCUS, null, null, 0, IMPLIED),
        attr(ONBLUR, null, null, 0, IMPLIED)
      }
    );
      defElement(CAPTION, 0, false, false, null,
      NONE
      ,
      new String[] {
        PCDATA, A, ABBR, ACRONYM,
        APPLET, B, BASEFONT, BDO, BIG,
        BR, BUTTON, CITE, CODE, DFN,
        EM, FONT, I, IFRAME, IMG,
        INPUT, KBD, LABEL, MAP, OBJECT,
        Q, S, SAMP, SCRIPT, SELECT,
        SMALL, SPAN, STRIKE, STRONG, SUB,
        SUP, TEXTAREA, TT, U, VAR
      }
    ,
      new AttributeList[] {
        attr(sID, null, null, ID, IMPLIED),
        attr(CLASS, null, null, 0, IMPLIED),
        attr(STYLE, null, null, 0, IMPLIED),
        attr(TITLE, null, null, 0, IMPLIED),
        attr(LANG, null, null, 0, IMPLIED),
        attr(DIR, null,  new String[] { LTR, RTL }, 0, IMPLIED),
        attr(ONCLICK, null, null, 0, IMPLIED),
        attr(ONDBLCLICK, null, null, 0, IMPLIED),
        attr(ONMOUSEDOWN, null, null, 0, IMPLIED),
        attr(ONMOUSEUP, null, null, 0, IMPLIED),
        attr(ONMOUSEOVER, null, null, 0, IMPLIED),
        attr(ONMOUSEMOVE, null, null, 0, IMPLIED),
        attr(ONMOUSEOUT, null, null, 0, IMPLIED),
        attr(ONKEYPRESS, null, null, 0, IMPLIED),
        attr(ONKEYDOWN, null, null, 0, IMPLIED),
        attr(ONKEYUP, null, null, 0, IMPLIED),
        attr(ALIGN, null,  new String[] { TOP, BOTTOM, LEFT, RIGHT },
          0, IMPLIED)
      }
    );

  }

  /**
   * Define second sixth of elements of this DTD.
   */
  private void defineElements2()
  {
    /* Define the elements. */
      defElement(CENTER, 0, false, false, null,
      NONE
      ,
      new String[] {
        PCDATA, A, ABBR, ACRONYM,
        APPLET, B, BASEFONT, BDO, BIG,
        BR, BUTTON, CITE, CODE, DFN,
        EM, FONT, I, IFRAME, IMG,
        INPUT, KBD, LABEL, MAP, OBJECT,
        Q, S, SAMP, SCRIPT, SELECT,
        SMALL, SPAN, STRIKE, STRONG, SUB,
        SUP, TEXTAREA, TT, U, VAR,
        ADDRESS, BLOCKQUOTE, CENTER, DIR, DIV,
        DL, FIELDSET, FORM, H1, H2,
        H3, H4, H5, H6, HR,
        ISINDEX, MENU, NOFRAMES, NOSCRIPT, OL,
        P, PRE, TABLE, UL
      }
    ,
      new AttributeList[] {
        attr(sID, null, null, ID, IMPLIED),
        attr(CLASS, null, null, 0, IMPLIED),
        attr(STYLE, null, null, 0, IMPLIED),
        attr(TITLE, null, null, 0, IMPLIED),
        attr(LANG, null, null, 0, IMPLIED),
        attr(DIR, null,  new String[] { LTR, RTL }, 0, IMPLIED),
        attr(ONCLICK, null, null, 0, IMPLIED),
        attr(ONDBLCLICK, null, null, 0, IMPLIED),
        attr(ONMOUSEDOWN, null, null, 0, IMPLIED),
        attr(ONMOUSEUP, null, null, 0, IMPLIED),
        attr(ONMOUSEOVER, null, null, 0, IMPLIED),
        attr(ONMOUSEMOVE, null, null, 0, IMPLIED),
        attr(ONMOUSEOUT, null, null, 0, IMPLIED),
        attr(ONKEYPRESS, null, null, 0, IMPLIED),
        attr(ONKEYDOWN, null, null, 0, IMPLIED),
        attr(ONKEYUP, null, null, 0, IMPLIED)
      }
    );
      defElement(CITE, 0, false, false, null,
      NONE
      ,
      new String[] {
        PCDATA, A, ABBR, ACRONYM,
        APPLET, B, BASEFONT, BDO, BIG,
        BR, BUTTON, CITE, CODE, DFN,
        EM, FONT, I, IFRAME, IMG,
        INPUT, KBD, LABEL, MAP, OBJECT,
        Q, S, SAMP, SCRIPT, SELECT,
        SMALL, SPAN, STRIKE, STRONG, SUB,
        SUP, TEXTAREA, TT, U, VAR
      }
    ,
      new AttributeList[] {
        attr(sID, null, null, ID, IMPLIED),
        attr(CLASS, null, null, 0, IMPLIED),
        attr(STYLE, null, null, 0, IMPLIED),
        attr(TITLE, null, null, 0, IMPLIED),
        attr(LANG, null, null, 0, IMPLIED),
        attr(DIR, null,  new String[] { LTR, RTL }, 0, IMPLIED),
        attr(ONCLICK, null, null, 0, IMPLIED),
        attr(ONDBLCLICK, null, null, 0, IMPLIED),
        attr(ONMOUSEDOWN, null, null, 0, IMPLIED),
        attr(ONMOUSEUP, null, null, 0, IMPLIED),
        attr(ONMOUSEOVER, null, null, 0, IMPLIED),
        attr(ONMOUSEMOVE, null, null, 0, IMPLIED),
        attr(ONMOUSEOUT, null, null, 0, IMPLIED),
        attr(ONKEYPRESS, null, null, 0, IMPLIED),
        attr(ONKEYDOWN, null, null, 0, IMPLIED),
        attr(ONKEYUP, null, null, 0, IMPLIED)
      }
    );
      defElement(CODE, 0, false, false, null,
      NONE
      ,
      new String[] {
        PCDATA, A, ABBR, ACRONYM,
        APPLET, B, BASEFONT, BDO, BIG,
        BR, BUTTON, CITE, CODE, DFN,
        EM, FONT, I, IFRAME, IMG,
        INPUT, KBD, LABEL, MAP, OBJECT,
        Q, S, SAMP, SCRIPT, SELECT,
        SMALL, SPAN, STRIKE, STRONG, SUB,
        SUP, TEXTAREA, TT, U, VAR
      }
    ,
      new AttributeList[] {
        attr(sID, null, null, ID, IMPLIED),
        attr(CLASS, null, null, 0, IMPLIED),
        attr(STYLE, null, null, 0, IMPLIED),
        attr(TITLE, null, null, 0, IMPLIED),
        attr(LANG, null, null, 0, IMPLIED),
        attr(DIR, null,  new String[] { LTR, RTL }, 0, IMPLIED),
        attr(ONCLICK, null, null, 0, IMPLIED),
        attr(ONDBLCLICK, null, null, 0, IMPLIED),
        attr(ONMOUSEDOWN, null, null, 0, IMPLIED),
        attr(ONMOUSEUP, null, null, 0, IMPLIED),
        attr(ONMOUSEOVER, null, null, 0, IMPLIED),
        attr(ONMOUSEMOVE, null, null, 0, IMPLIED),
        attr(ONMOUSEOUT, null, null, 0, IMPLIED),
        attr(ONKEYPRESS, null, null, 0, IMPLIED),
        attr(ONKEYDOWN, null, null, 0, IMPLIED),
        attr(ONKEYUP, null, null, 0, IMPLIED)
      }
    );
      defElement(COL, EMPTY, false, true, null,
      NONE
      ,
      NONE
    ,
      new AttributeList[] {
        attr(sID, null, null, ID, IMPLIED),
        attr(CLASS, null, null, 0, IMPLIED),
        attr(STYLE, null, null, 0, IMPLIED),
        attr(TITLE, null, null, 0, IMPLIED),
        attr(LANG, null, null, 0, IMPLIED),
        attr(DIR, null,  new String[] { LTR, RTL }, 0, IMPLIED),
        attr(ONCLICK, null, null, 0, IMPLIED),
        attr(ONDBLCLICK, null, null, 0, IMPLIED),
        attr(ONMOUSEDOWN, null, null, 0, IMPLIED),
        attr(ONMOUSEUP, null, null, 0, IMPLIED),
        attr(ONMOUSEOVER, null, null, 0, IMPLIED),
        attr(ONMOUSEMOVE, null, null, 0, IMPLIED),
        attr(ONMOUSEOUT, null, null, 0, IMPLIED),
        attr(ONKEYPRESS, null, null, 0, IMPLIED),
        attr(ONKEYDOWN, null, null, 0, IMPLIED),
        attr(ONKEYUP, null, null, 0, IMPLIED),
        attr(SPAN, C_1, null, NUMBER, DEFAULT),
        attr(WIDTH, null, null, 0, IMPLIED),
        attr(ALIGN, null,  new String[] { LEFT, CENTER, RIGHT, JUSTIFY, CHAR },
          0, IMPLIED),
        attr(CHAR, null, null, 0, IMPLIED),
        attr(CHAROFF, null, null, 0, IMPLIED),
        attr(VALIGN, null,  new String[] { TOP, MIDDLE, BOTTOM, BASELINE },
          0, IMPLIED)
      }
    );
      defElement(COLGROUP, 0, false, true, null,
      NONE
      ,
      new String[] {
        COL
      }
    ,
      new AttributeList[] {
        attr(sID, null, null, ID, IMPLIED),
        attr(CLASS, null, null, 0, IMPLIED),
        attr(STYLE, null, null, 0, IMPLIED),
        attr(TITLE, null, null, 0, IMPLIED),
        attr(LANG, null, null, 0, IMPLIED),
        attr(DIR, null,  new String[] { LTR, RTL }, 0, IMPLIED),
        attr(ONCLICK, null, null, 0, IMPLIED),
        attr(ONDBLCLICK, null, null, 0, IMPLIED),
        attr(ONMOUSEDOWN, null, null, 0, IMPLIED),
        attr(ONMOUSEUP, null, null, 0, IMPLIED),
        attr(ONMOUSEOVER, null, null, 0, IMPLIED),
        attr(ONMOUSEMOVE, null, null, 0, IMPLIED),
        attr(ONMOUSEOUT, null, null, 0, IMPLIED),
        attr(ONKEYPRESS, null, null, 0, IMPLIED),
        attr(ONKEYDOWN, null, null, 0, IMPLIED),
        attr(ONKEYUP, null, null, 0, IMPLIED),
        attr(SPAN, C_1, null, NUMBER, DEFAULT),
        attr(WIDTH, null, null, 0, IMPLIED),
        attr(ALIGN, null,  new String[] { LEFT, CENTER, RIGHT, JUSTIFY, CHAR },
          0, IMPLIED),
        attr(CHAR, null, null, 0, IMPLIED),
        attr(CHAROFF, null, null, 0, IMPLIED),
        attr(VALIGN, null,  new String[] { TOP, MIDDLE, BOTTOM, BASELINE },
          0, IMPLIED)
      }
    );
      defElement(DD, 0, false, true, new ContentModel(0,
        new noTagModel( new String[] { DD, DT } ), null ),
      NONE
      ,
      new String[] {
        PCDATA, A, ABBR, ACRONYM,
        APPLET, B, BASEFONT, BDO, BIG,
        BR, BUTTON, CITE, CODE, DFN,
        EM, FONT, I, IFRAME, IMG,
        INPUT, KBD, LABEL, MAP, OBJECT,
        Q, S, SAMP, SCRIPT, SELECT,
        SMALL, SPAN, STRIKE, STRONG, SUB,
        SUP, TEXTAREA, TT, U, VAR,
        ADDRESS, BLOCKQUOTE, CENTER, DIR, DIV,
        DL, FIELDSET, FORM, H1, H2,
        H3, H4, H5, H6, HR,
        ISINDEX, MENU, NOFRAMES, NOSCRIPT, OL,
        P, PRE, TABLE, UL
      }
    ,
      new AttributeList[] {
        attr(sID, null, null, ID, IMPLIED),
        attr(CLASS, null, null, 0, IMPLIED),
        attr(STYLE, null, null, 0, IMPLIED),
        attr(TITLE, null, null, 0, IMPLIED),
        attr(LANG, null, null, 0, IMPLIED),
        attr(DIR, null,  new String[] { LTR, RTL }, 0, IMPLIED),
        attr(ONCLICK, null, null, 0, IMPLIED),
        attr(ONDBLCLICK, null, null, 0, IMPLIED),
        attr(ONMOUSEDOWN, null, null, 0, IMPLIED),
        attr(ONMOUSEUP, null, null, 0, IMPLIED),
        attr(ONMOUSEOVER, null, null, 0, IMPLIED),
        attr(ONMOUSEMOVE, null, null, 0, IMPLIED),
        attr(ONMOUSEOUT, null, null, 0, IMPLIED),
        attr(ONKEYPRESS, null, null, 0, IMPLIED),
        attr(ONKEYDOWN, null, null, 0, IMPLIED),
        attr(ONKEYUP, null, null, 0, IMPLIED)
      }
    );
      defElement(DEL, 0, false, false, null,
      NONE
      ,
      new String[] {
        PCDATA, A, ABBR, ACRONYM,
        APPLET, B, BASEFONT, BDO, BIG,
        BR, BUTTON, CITE, CODE, DFN,
        EM, FONT, I, IFRAME, IMG,
        INPUT, KBD, LABEL, MAP, OBJECT,
        Q, S, SAMP, SCRIPT, SELECT,
        SMALL, SPAN, STRIKE, STRONG, SUB,
        SUP, TEXTAREA, TT, U, VAR,
        ADDRESS, BLOCKQUOTE, CENTER, DIR, DIV,
        DL, FIELDSET, FORM, H1, H2,
        H3, H4, H5, H6, HR,
        ISINDEX, MENU, NOFRAMES, NOSCRIPT, OL,
        P, PRE, TABLE, UL
      }
    ,
      new AttributeList[] {
        attr(sID, null, null, ID, IMPLIED),
        attr(CLASS, null, null, 0, IMPLIED),
        attr(STYLE, null, null, 0, IMPLIED),
        attr(TITLE, null, null, 0, IMPLIED),
        attr(LANG, null, null, 0, IMPLIED),
        attr(DIR, null,  new String[] { LTR, RTL }, 0, IMPLIED),
        attr(ONCLICK, null, null, 0, IMPLIED),
        attr(ONDBLCLICK, null, null, 0, IMPLIED),
        attr(ONMOUSEDOWN, null, null, 0, IMPLIED),
        attr(ONMOUSEUP, null, null, 0, IMPLIED),
        attr(ONMOUSEOVER, null, null, 0, IMPLIED),
        attr(ONMOUSEMOVE, null, null, 0, IMPLIED),
        attr(ONMOUSEOUT, null, null, 0, IMPLIED),
        attr(ONKEYPRESS, null, null, 0, IMPLIED),
        attr(ONKEYDOWN, null, null, 0, IMPLIED),
        attr(ONKEYUP, null, null, 0, IMPLIED),
        attr(CITE, null, null, 0, IMPLIED),
        attr(DATETIME, null, null, 0, IMPLIED)
      }
    );
      defElement(DFN, 0, false, false, null,
      NONE
      ,
      new String[] {
        PCDATA, A, ABBR, ACRONYM,
        APPLET, B, BASEFONT, BDO, BIG,
        BR, BUTTON, CITE, CODE, DFN,
        EM, FONT, I, IFRAME, IMG,
        INPUT, KBD, LABEL, MAP, OBJECT,
        Q, S, SAMP, SCRIPT, SELECT,
        SMALL, SPAN, STRIKE, STRONG, SUB,
        SUP, TEXTAREA, TT, U, VAR
      }
    ,
      new AttributeList[] {
        attr(sID, null, null, ID, IMPLIED),
        attr(CLASS, null, null, 0, IMPLIED),
        attr(STYLE, null, null, 0, IMPLIED),
        attr(TITLE, null, null, 0, IMPLIED),
        attr(LANG, null, null, 0, IMPLIED),
        attr(DIR, null,  new String[] { LTR, RTL }, 0, IMPLIED),
        attr(ONCLICK, null, null, 0, IMPLIED),
        attr(ONDBLCLICK, null, null, 0, IMPLIED),
        attr(ONMOUSEDOWN, null, null, 0, IMPLIED),
        attr(ONMOUSEUP, null, null, 0, IMPLIED),
        attr(ONMOUSEOVER, null, null, 0, IMPLIED),
        attr(ONMOUSEMOVE, null, null, 0, IMPLIED),
        attr(ONMOUSEOUT, null, null, 0, IMPLIED),
        attr(ONKEYPRESS, null, null, 0, IMPLIED),
        attr(ONKEYDOWN, null, null, 0, IMPLIED),
        attr(ONKEYUP, null, null, 0, IMPLIED)
      }
    );
      defElement(DIR, 0, false, false, createListModel(),
      new String[] {
        ADDRESS, BLOCKQUOTE, CENTER, DIR,
        DIV, DL, FIELDSET, FORM, H1,
        H2, H3, H4, H5, H6,
        HR, ISINDEX, MENU, NOFRAMES, NOSCRIPT,
        OL, P, PRE, TABLE, UL
      }
      ,
      new String[] {
        LI, UL, OL
      }
    ,
      new AttributeList[] {
        attr(sID, null, null, ID, IMPLIED),
        attr(CLASS, null, null, 0, IMPLIED),
        attr(STYLE, null, null, 0, IMPLIED),
        attr(TITLE, null, null, 0, IMPLIED),
        attr(LANG, null, null, 0, IMPLIED),
        attr(DIR, null,  new String[] { LTR, RTL }, 0, IMPLIED),
        attr(ONCLICK, null, null, 0, IMPLIED),
        attr(ONDBLCLICK, null, null, 0, IMPLIED),
        attr(ONMOUSEDOWN, null, null, 0, IMPLIED),
        attr(ONMOUSEUP, null, null, 0, IMPLIED),
        attr(ONMOUSEOVER, null, null, 0, IMPLIED),
        attr(ONMOUSEMOVE, null, null, 0, IMPLIED),
        attr(ONMOUSEOUT, null, null, 0, IMPLIED),
        attr(ONKEYPRESS, null, null, 0, IMPLIED),
        attr(ONKEYDOWN, null, null, 0, IMPLIED),
        attr(ONKEYUP, null, null, 0, IMPLIED),
        attr(COMPACT, null,  new String[] { COMPACT }, 0, IMPLIED)
      }
    );
      defElement(DIV, 0, false, false, null,
      NONE
      ,
      new String[] {
        PCDATA, A, ABBR, ACRONYM,
        APPLET, B, BASEFONT, BDO, BIG,
        BR, BUTTON, CITE, CODE, DFN,
        EM, FONT, I, IFRAME, IMG,
        INPUT, KBD, LABEL, MAP, OBJECT,
        Q, S, SAMP, SCRIPT, SELECT,
        SMALL, SPAN, STRIKE, STRONG, SUB,
        SUP, TEXTAREA, TT, U, VAR,
        ADDRESS, BLOCKQUOTE, CENTER, DIR, DIV,
        DL, FIELDSET, FORM, H1, H2,
        H3, H4, H5, H6, HR,
        ISINDEX, MENU, NOFRAMES, NOSCRIPT, OL,
        P, PRE, TABLE, UL
      }
    ,
      new AttributeList[] {
        attr(sID, null, null, ID, IMPLIED),
        attr(CLASS, null, null, 0, IMPLIED),
        attr(STYLE, null, null, 0, IMPLIED),
        attr(TITLE, null, null, 0, IMPLIED),
        attr(LANG, null, null, 0, IMPLIED),
        attr(DIR, null,  new String[] { LTR, RTL }, 0, IMPLIED),
        attr(ONCLICK, null, null, 0, IMPLIED),
        attr(ONDBLCLICK, null, null, 0, IMPLIED),
        attr(ONMOUSEDOWN, null, null, 0, IMPLIED),
        attr(ONMOUSEUP, null, null, 0, IMPLIED),
        attr(ONMOUSEOVER, null, null, 0, IMPLIED),
        attr(ONMOUSEMOVE, null, null, 0, IMPLIED),
        attr(ONMOUSEOUT, null, null, 0, IMPLIED),
        attr(ONKEYPRESS, null, null, 0, IMPLIED),
        attr(ONKEYDOWN, null, null, 0, IMPLIED),
        attr(ONKEYUP, null, null, 0, IMPLIED),
        attr(ALIGN, null,  new String[] { LEFT, CENTER, RIGHT, JUSTIFY },
          0, IMPLIED)
      }
    );
      defElement(DL, 0, false, false, createDefListModel(),
      NONE
      ,
      new String[] {
        DD, DT
      }
    ,
      new AttributeList[] {
        attr(sID, null, null, ID, IMPLIED),
        attr(CLASS, null, null, 0, IMPLIED),
        attr(STYLE, null, null, 0, IMPLIED),
        attr(TITLE, null, null, 0, IMPLIED),
        attr(LANG, null, null, 0, IMPLIED),
        attr(DIR, null,  new String[] { LTR, RTL }, 0, IMPLIED),
        attr(ONCLICK, null, null, 0, IMPLIED),
        attr(ONDBLCLICK, null, null, 0, IMPLIED),
        attr(ONMOUSEDOWN, null, null, 0, IMPLIED),
        attr(ONMOUSEUP, null, null, 0, IMPLIED),
        attr(ONMOUSEOVER, null, null, 0, IMPLIED),
        attr(ONMOUSEMOVE, null, null, 0, IMPLIED),
        attr(ONMOUSEOUT, null, null, 0, IMPLIED),
        attr(ONKEYPRESS, null, null, 0, IMPLIED),
        attr(ONKEYDOWN, null, null, 0, IMPLIED),
        attr(ONKEYUP, null, null, 0, IMPLIED),
        attr(COMPACT, null,  new String[] { COMPACT }, 0, IMPLIED)
      }
    );
      defElement(DT, 0, false, true,
        new ContentModel(0,
         new noTagModel( new String[] { DT, DD } ), null),
        BLOCK
      ,
      new String[] {
        PCDATA, A, ABBR, ACRONYM,
        APPLET, B, BASEFONT, BDO, BIG,
        BR, BUTTON, CITE, CODE, DFN,
        EM, FONT, I, IFRAME, IMG,
        INPUT, KBD, LABEL, MAP, OBJECT,
        Q, S, SAMP, SCRIPT, SELECT,
        SMALL, SPAN, STRIKE, STRONG, SUB,
        SUP, TEXTAREA, TT, U, VAR
      }
    ,
      new AttributeList[] {
        attr(sID, null, null, ID, IMPLIED),
        attr(CLASS, null, null, 0, IMPLIED),
        attr(STYLE, null, null, 0, IMPLIED),
        attr(TITLE, null, null, 0, IMPLIED),
        attr(LANG, null, null, 0, IMPLIED),
        attr(DIR, null,  new String[] { LTR, RTL }, 0, IMPLIED),
        attr(ONCLICK, null, null, 0, IMPLIED),
        attr(ONDBLCLICK, null, null, 0, IMPLIED),
        attr(ONMOUSEDOWN, null, null, 0, IMPLIED),
        attr(ONMOUSEUP, null, null, 0, IMPLIED),
        attr(ONMOUSEOVER, null, null, 0, IMPLIED),
        attr(ONMOUSEMOVE, null, null, 0, IMPLIED),
        attr(ONMOUSEOUT, null, null, 0, IMPLIED),
        attr(ONKEYPRESS, null, null, 0, IMPLIED),
        attr(ONKEYDOWN, null, null, 0, IMPLIED),
        attr(ONKEYUP, null, null, 0, IMPLIED)
      }
    );
      defElement(EM, 0, false, false, null,
      NONE
      ,
      new String[] {
        PCDATA, A, ABBR, ACRONYM,
        APPLET, B, BASEFONT, BDO, BIG,
        BR, BUTTON, CITE, CODE, DFN,
        EM, FONT, I, IFRAME, IMG,
        INPUT, KBD, LABEL, MAP, OBJECT,
        Q, S, SAMP, SCRIPT, SELECT,
        SMALL, SPAN, STRIKE, STRONG, SUB,
        SUP, TEXTAREA, TT, U, VAR
      }
    ,
      new AttributeList[] {
        attr(sID, null, null, ID, IMPLIED),
        attr(CLASS, null, null, 0, IMPLIED),
        attr(STYLE, null, null, 0, IMPLIED),
        attr(TITLE, null, null, 0, IMPLIED),
        attr(LANG, null, null, 0, IMPLIED),
        attr(DIR, null,  new String[] { LTR, RTL }, 0, IMPLIED),
        attr(ONCLICK, null, null, 0, IMPLIED),
        attr(ONDBLCLICK, null, null, 0, IMPLIED),
        attr(ONMOUSEDOWN, null, null, 0, IMPLIED),
        attr(ONMOUSEUP, null, null, 0, IMPLIED),
        attr(ONMOUSEOVER, null, null, 0, IMPLIED),
        attr(ONMOUSEMOVE, null, null, 0, IMPLIED),
        attr(ONMOUSEOUT, null, null, 0, IMPLIED),
        attr(ONKEYPRESS, null, null, 0, IMPLIED),
        attr(ONKEYDOWN, null, null, 0, IMPLIED),
        attr(ONKEYUP, null, null, 0, IMPLIED)
      }
    );
      defElement(FIELDSET, 0, false, false, null,
      NONE
      ,
      new String[] {
        PCDATA, A, ABBR, ACRONYM,
        APPLET, B, BASEFONT, BDO, BIG,
        BR, BUTTON, CITE, CODE, DFN,
        EM, FONT, I, IFRAME, IMG,
        INPUT, KBD, LABEL, MAP, OBJECT,
        Q, S, SAMP, SCRIPT, SELECT,
        SMALL, SPAN, STRIKE, STRONG, SUB,
        SUP, TEXTAREA, TT, U, VAR,
        ADDRESS, BLOCKQUOTE, CENTER, DIR, DIV,
        DL, FIELDSET, FORM, H1, H2,
        H3, H4, H5, H6, HR,
        ISINDEX, MENU, NOFRAMES, NOSCRIPT, OL,
        P, PRE, TABLE, UL, LEGEND
      }
    ,
      new AttributeList[] {
        attr(sID, null, null, ID, IMPLIED),
        attr(CLASS, null, null, 0, IMPLIED),
        attr(STYLE, null, null, 0, IMPLIED),
        attr(TITLE, null, null, 0, IMPLIED),
        attr(LANG, null, null, 0, IMPLIED),
        attr(DIR, null,  new String[] { LTR, RTL }, 0, IMPLIED),
        attr(ONCLICK, null, null, 0, IMPLIED),
        attr(ONDBLCLICK, null, null, 0, IMPLIED),
        attr(ONMOUSEDOWN, null, null, 0, IMPLIED),
        attr(ONMOUSEUP, null, null, 0, IMPLIED),
        attr(ONMOUSEOVER, null, null, 0, IMPLIED),
        attr(ONMOUSEMOVE, null, null, 0, IMPLIED),
        attr(ONMOUSEOUT, null, null, 0, IMPLIED),
        attr(ONKEYPRESS, null, null, 0, IMPLIED),
        attr(ONKEYDOWN, null, null, 0, IMPLIED),
        attr(ONKEYUP, null, null, 0, IMPLIED)
      }
    );

  }

  /**
   * Define third sixth of elements of this DTD.
   */
  private void defineElements3()
  {
    /* Define the elements. */
      defElement(FONT, 0, false, false, null,
      NONE
      ,
      new String[] {
        PCDATA, A, ABBR, ACRONYM,
        APPLET, B, BASEFONT, BDO, BIG,
        BR, BUTTON, CITE, CODE, DFN,
        EM, FONT, I, IFRAME, IMG,
        INPUT, KBD, LABEL, MAP, OBJECT,
        Q, S, SAMP, SCRIPT, SELECT,
        SMALL, SPAN, STRIKE, STRONG, SUB,
        SUP, TEXTAREA, TT, U, VAR
      }
    ,
      new AttributeList[] {
        attr(sID, null, null, ID, IMPLIED),
        attr(CLASS, null, null, 0, IMPLIED),
        attr(STYLE, null, null, 0, IMPLIED),
        attr(TITLE, null, null, 0, IMPLIED),
        attr(LANG, null, null, 0, IMPLIED),
        attr(DIR, null,  new String[] { LTR, RTL }, 0, IMPLIED),
        attr(SIZE, null, null, 0, IMPLIED),
        attr(COLOR, null, null, 0, IMPLIED),
        attr(FACE, null, null, 0, IMPLIED)
      }
    );
      defElement(FORM, 0, false, false, null,
      new String[] {
        FORM
      }
      ,
      new String[] {
        PCDATA, A, ABBR, ACRONYM,
        APPLET, B, BASEFONT, BDO, BIG,
        BR, BUTTON, CITE, CODE, DFN,
        EM, FONT, I, IFRAME, IMG,
        INPUT, KBD, LABEL, MAP, OBJECT,
        Q, S, SAMP, SCRIPT, SELECT,
        SMALL, SPAN, STRIKE, STRONG, SUB,
        SUP, TEXTAREA, TT, U, VAR,
        ADDRESS, BLOCKQUOTE, CENTER, DIR, DIV,
        DL, FIELDSET, H1, H2, H3,
        H4, H5, H6, HR, ISINDEX,
        MENU, NOFRAMES, NOSCRIPT, OL, P,
        PRE, TABLE, UL
      }
    ,
      new AttributeList[] {
        attr(sID, null, null, ID, IMPLIED),
        attr(CLASS, null, null, 0, IMPLIED),
        attr(STYLE, null, null, 0, IMPLIED),
        attr(TITLE, null, null, 0, IMPLIED),
        attr(LANG, null, null, 0, IMPLIED),
        attr(DIR, null,  new String[] { LTR, RTL }, 0, IMPLIED),
        attr(ONCLICK, null, null, 0, IMPLIED),
        attr(ONDBLCLICK, null, null, 0, IMPLIED),
        attr(ONMOUSEDOWN, null, null, 0, IMPLIED),
        attr(ONMOUSEUP, null, null, 0, IMPLIED),
        attr(ONMOUSEOVER, null, null, 0, IMPLIED),
        attr(ONMOUSEMOVE, null, null, 0, IMPLIED),
        attr(ONMOUSEOUT, null, null, 0, IMPLIED),
        attr(ONKEYPRESS, null, null, 0, IMPLIED),
        attr(ONKEYDOWN, null, null, 0, IMPLIED),
        attr(ONKEYUP, null, null, 0, IMPLIED),
        attr(ACTION, null, null, 0, REQUIRED),
        attr(METHOD, GET,  new String[] { GET, POST }, 0, DEFAULT),
        attr(ENCTYPE, APPLICATION_X_WWW_FORM_URLENCODED, null, 0, DEFAULT),
        attr(ACCEPT, null, null, 0, IMPLIED),
        attr(sNAME, null, null, 0, IMPLIED),
        attr(ONSUBMIT, null, null, 0, IMPLIED),
        attr(ONRESET, null, null, 0, IMPLIED),
        attr(TARGET, null, null, 0, IMPLIED),
        attr(ACCEPTCHARSET, null, null, 0, IMPLIED)
      }
    );
      defElement(FRAME, EMPTY, false, true, null,
      NONE
      ,
      NONE
    ,
      new AttributeList[] {
        attr(sID, null, null, ID, IMPLIED),
        attr(CLASS, null, null, 0, IMPLIED),
        attr(STYLE, null, null, 0, IMPLIED),
        attr(TITLE, null, null, 0, IMPLIED),
        attr(LONGDESC, null, null, 0, IMPLIED),
        attr(sNAME, null, null, 0, IMPLIED),
        attr(SRC, null, null, 0, IMPLIED),
        attr(FRAMEBORDER, C_1,  new String[] { C_1, C_0 }, 0, DEFAULT),
        attr(MARGINWIDTH, null, null, PIXELS, IMPLIED),
        attr(MARGINHEIGHT, null, null, PIXELS, IMPLIED),
        attr(NORESIZE, null,  new String[] { NORESIZE }, 0, IMPLIED),
        attr(SCROLLING, AUTO,  new String[] { YES, NO, AUTO }, 0, DEFAULT)
      }
    );
      defElement(FRAMESET, 0, false, false, null,
      NONE
      ,
      new String[] {
        NOFRAMES, FRAME, FRAMESET
      }
    ,
      new AttributeList[] {
        attr(sID, null, null, ID, IMPLIED),
        attr(CLASS, null, null, 0, IMPLIED),
        attr(STYLE, null, null, 0, IMPLIED),
        attr(TITLE, null, null, 0, IMPLIED),
        attr(ROWS, null, null, 0, IMPLIED),
        attr(COLS, null, null, 0, IMPLIED),
        attr(ONLOAD, null, null, 0, IMPLIED),
        attr(ONUNLOAD, null, null, 0, IMPLIED)
      }
    );
      defElement(H1, 0, false, false, null,
      NONE
      ,
      new String[] {
        PCDATA, A, ABBR, ACRONYM,
        APPLET, B, BASEFONT, BDO, BIG,
        BR, BUTTON, CITE, CODE, DFN,
        EM, FONT, I, IFRAME, IMG,
        INPUT, KBD, LABEL, MAP, OBJECT,
        Q, S, SAMP, SCRIPT, SELECT,
        SMALL, SPAN, STRIKE, STRONG, SUB,
        SUP, TEXTAREA, TT, U, VAR
      }
    ,
      new AttributeList[] {
        attr(sID, null, null, ID, IMPLIED),
        attr(CLASS, null, null, 0, IMPLIED),
        attr(STYLE, null, null, 0, IMPLIED),
        attr(TITLE, null, null, 0, IMPLIED),
        attr(LANG, null, null, 0, IMPLIED),
        attr(DIR, null,  new String[] { LTR, RTL }, 0, IMPLIED),
        attr(ONCLICK, null, null, 0, IMPLIED),
        attr(ONDBLCLICK, null, null, 0, IMPLIED),
        attr(ONMOUSEDOWN, null, null, 0, IMPLIED),
        attr(ONMOUSEUP, null, null, 0, IMPLIED),
        attr(ONMOUSEOVER, null, null, 0, IMPLIED),
        attr(ONMOUSEMOVE, null, null, 0, IMPLIED),
        attr(ONMOUSEOUT, null, null, 0, IMPLIED),
        attr(ONKEYPRESS, null, null, 0, IMPLIED),
        attr(ONKEYDOWN, null, null, 0, IMPLIED),
        attr(ONKEYUP, null, null, 0, IMPLIED),
        attr(ALIGN, null,  new String[] { LEFT, CENTER, RIGHT, JUSTIFY },
          0, IMPLIED)
      }
    );
      defElement(H2, 0, false, false, null,
      NONE
      ,
      new String[] {
        PCDATA, A, ABBR, ACRONYM,
        APPLET, B, BASEFONT, BDO, BIG,
        BR, BUTTON, CITE, CODE, DFN,
        EM, FONT, I, IFRAME, IMG,
        INPUT, KBD, LABEL, MAP, OBJECT,
        Q, S, SAMP, SCRIPT, SELECT,
        SMALL, SPAN, STRIKE, STRONG, SUB,
        SUP, TEXTAREA, TT, U, VAR
      }
    ,
      new AttributeList[] {
        attr(sID, null, null, ID, IMPLIED),
        attr(CLASS, null, null, 0, IMPLIED),
        attr(STYLE, null, null, 0, IMPLIED),
        attr(TITLE, null, null, 0, IMPLIED),
        attr(LANG, null, null, 0, IMPLIED),
        attr(DIR, null,  new String[] { LTR, RTL }, 0, IMPLIED),
        attr(ONCLICK, null, null, 0, IMPLIED),
        attr(ONDBLCLICK, null, null, 0, IMPLIED),
        attr(ONMOUSEDOWN, null, null, 0, IMPLIED),
        attr(ONMOUSEUP, null, null, 0, IMPLIED),
        attr(ONMOUSEOVER, null, null, 0, IMPLIED),
        attr(ONMOUSEMOVE, null, null, 0, IMPLIED),
        attr(ONMOUSEOUT, null, null, 0, IMPLIED),
        attr(ONKEYPRESS, null, null, 0, IMPLIED),
        attr(ONKEYDOWN, null, null, 0, IMPLIED),
        attr(ONKEYUP, null, null, 0, IMPLIED),
        attr(ALIGN, null,  new String[] { LEFT, CENTER, RIGHT, JUSTIFY },
          0, IMPLIED)
      }
    );
      defElement(H3, 0, false, false, null,
      NONE
      ,
      new String[] {
        PCDATA, A, ABBR, ACRONYM,
        APPLET, B, BASEFONT, BDO, BIG,
        BR, BUTTON, CITE, CODE, DFN,
        EM, FONT, I, IFRAME, IMG,
        INPUT, KBD, LABEL, MAP, OBJECT,
        Q, S, SAMP, SCRIPT, SELECT,
        SMALL, SPAN, STRIKE, STRONG, SUB,
        SUP, TEXTAREA, TT, U, VAR
      }
    ,
      new AttributeList[] {
        attr(sID, null, null, ID, IMPLIED),
        attr(CLASS, null, null, 0, IMPLIED),
        attr(STYLE, null, null, 0, IMPLIED),
        attr(TITLE, null, null, 0, IMPLIED),
        attr(LANG, null, null, 0, IMPLIED),
        attr(DIR, null,  new String[] { LTR, RTL }, 0, IMPLIED),
        attr(ONCLICK, null, null, 0, IMPLIED),
        attr(ONDBLCLICK, null, null, 0, IMPLIED),
        attr(ONMOUSEDOWN, null, null, 0, IMPLIED),
        attr(ONMOUSEUP, null, null, 0, IMPLIED),
        attr(ONMOUSEOVER, null, null, 0, IMPLIED),
        attr(ONMOUSEMOVE, null, null, 0, IMPLIED),
        attr(ONMOUSEOUT, null, null, 0, IMPLIED),
        attr(ONKEYPRESS, null, null, 0, IMPLIED),
        attr(ONKEYDOWN, null, null, 0, IMPLIED),
        attr(ONKEYUP, null, null, 0, IMPLIED),
        attr(ALIGN, null,  new String[] { LEFT, CENTER, RIGHT, JUSTIFY },
          0, IMPLIED)
      }
    );
      defElement(H4, 0, false, false, null,
      NONE
      ,
      new String[] {
        PCDATA, A, ABBR, ACRONYM,
        APPLET, B, BASEFONT, BDO, BIG,
        BR, BUTTON, CITE, CODE, DFN,
        EM, FONT, I, IFRAME, IMG,
        INPUT, KBD, LABEL, MAP, OBJECT,
        Q, S, SAMP, SCRIPT, SELECT,
        SMALL, SPAN, STRIKE, STRONG, SUB,
        SUP, TEXTAREA, TT, U, VAR
      }
    ,
      new AttributeList[] {
        attr(sID, null, null, ID, IMPLIED),
        attr(CLASS, null, null, 0, IMPLIED),
        attr(STYLE, null, null, 0, IMPLIED),
        attr(TITLE, null, null, 0, IMPLIED),
        attr(LANG, null, null, 0, IMPLIED),
        attr(DIR, null,  new String[] { LTR, RTL }, 0, IMPLIED),
        attr(ONCLICK, null, null, 0, IMPLIED),
        attr(ONDBLCLICK, null, null, 0, IMPLIED),
        attr(ONMOUSEDOWN, null, null, 0, IMPLIED),
        attr(ONMOUSEUP, null, null, 0, IMPLIED),
        attr(ONMOUSEOVER, null, null, 0, IMPLIED),
        attr(ONMOUSEMOVE, null, null, 0, IMPLIED),
        attr(ONMOUSEOUT, null, null, 0, IMPLIED),
        attr(ONKEYPRESS, null, null, 0, IMPLIED),
        attr(ONKEYDOWN, null, null, 0, IMPLIED),
        attr(ONKEYUP, null, null, 0, IMPLIED),
        attr(ALIGN, null,  new String[] { LEFT, CENTER, RIGHT, JUSTIFY },
          0, IMPLIED)
      }
    );
      defElement(H5, 0, false, false, null,
      NONE
      ,
      new String[] {
        PCDATA, A, ABBR, ACRONYM,
        APPLET, B, BASEFONT, BDO, BIG,
        BR, BUTTON, CITE, CODE, DFN,
        EM, FONT, I, IFRAME, IMG,
        INPUT, KBD, LABEL, MAP, OBJECT,
        Q, S, SAMP, SCRIPT, SELECT,
        SMALL, SPAN, STRIKE, STRONG, SUB,
        SUP, TEXTAREA, TT, U, VAR
      }
    ,
      new AttributeList[] {
        attr(sID, null, null, ID, IMPLIED),
        attr(CLASS, null, null, 0, IMPLIED),
        attr(STYLE, null, null, 0, IMPLIED),
        attr(TITLE, null, null, 0, IMPLIED),
        attr(LANG, null, null, 0, IMPLIED),
        attr(DIR, null,  new String[] { LTR, RTL }, 0, IMPLIED),
        attr(ONCLICK, null, null, 0, IMPLIED),
        attr(ONDBLCLICK, null, null, 0, IMPLIED),
        attr(ONMOUSEDOWN, null, null, 0, IMPLIED),
        attr(ONMOUSEUP, null, null, 0, IMPLIED),
        attr(ONMOUSEOVER, null, null, 0, IMPLIED),
        attr(ONMOUSEMOVE, null, null, 0, IMPLIED),
        attr(ONMOUSEOUT, null, null, 0, IMPLIED),
        attr(ONKEYPRESS, null, null, 0, IMPLIED),
        attr(ONKEYDOWN, null, null, 0, IMPLIED),
        attr(ONKEYUP, null, null, 0, IMPLIED),
        attr(ALIGN, null,  new String[] { LEFT, CENTER, RIGHT, JUSTIFY },
          0, IMPLIED)
      }
    );
      defElement(H6, 0, false, false, null,
      NONE
      ,
      new String[] {
        PCDATA, A, ABBR, ACRONYM,
        APPLET, B, BASEFONT, BDO, BIG,
        BR, BUTTON, CITE, CODE, DFN,
        EM, FONT, I, IFRAME, IMG,
        INPUT, KBD, LABEL, MAP, OBJECT,
        Q, S, SAMP, SCRIPT, SELECT,
        SMALL, SPAN, STRIKE, STRONG, SUB,
        SUP, TEXTAREA, TT, U, VAR
      }
    ,
      new AttributeList[] {
        attr(sID, null, null, ID, IMPLIED),
        attr(CLASS, null, null, 0, IMPLIED),
        attr(STYLE, null, null, 0, IMPLIED),
        attr(TITLE, null, null, 0, IMPLIED),
        attr(LANG, null, null, 0, IMPLIED),
        attr(DIR, null,  new String[] { LTR, RTL }, 0, IMPLIED),
        attr(ONCLICK, null, null, 0, IMPLIED),
        attr(ONDBLCLICK, null, null, 0, IMPLIED),
        attr(ONMOUSEDOWN, null, null, 0, IMPLIED),
        attr(ONMOUSEUP, null, null, 0, IMPLIED),
        attr(ONMOUSEOVER, null, null, 0, IMPLIED),
        attr(ONMOUSEMOVE, null, null, 0, IMPLIED),
        attr(ONMOUSEOUT, null, null, 0, IMPLIED),
        attr(ONKEYPRESS, null, null, 0, IMPLIED),
        attr(ONKEYDOWN, null, null, 0, IMPLIED),
        attr(ONKEYUP, null, null, 0, IMPLIED),
        attr(ALIGN, null,  new String[] { LEFT, CENTER, RIGHT, JUSTIFY },
          0, IMPLIED)
      }
    );
      defElement(HEAD, 0, true, true, null,
      new String[] {
        BODY
      }
      ,
      new String[] {
       TITLE, ISINDEX, BASE,
       SCRIPT, STYLE, META, LINK, OBJECT
      }
    ,
      new AttributeList[] {
        attr(LANG, null, null, 0, IMPLIED),
        attr(DIR, null,  new String[] { LTR, RTL }, 0, IMPLIED),
        attr(PROFILE, null, null, 0, IMPLIED)
      }
    );

      defElement(HR, EMPTY, false, true, null,
      NONE
      ,
      NONE
    ,
      new AttributeList[] {
        attr(sID, null, null, ID, IMPLIED),
        attr(CLASS, null, null, 0, IMPLIED),
        attr(STYLE, null, null, 0, IMPLIED),
        attr(TITLE, null, null, 0, IMPLIED),
        attr(LANG, null, null, 0, IMPLIED),
        attr(DIR, null,  new String[] { LTR, RTL }, 0, IMPLIED),
        attr(ONCLICK, null, null, 0, IMPLIED),
        attr(ONDBLCLICK, null, null, 0, IMPLIED),
        attr(ONMOUSEDOWN, null, null, 0, IMPLIED),
        attr(ONMOUSEUP, null, null, 0, IMPLIED),
        attr(ONMOUSEOVER, null, null, 0, IMPLIED),
        attr(ONMOUSEMOVE, null, null, 0, IMPLIED),
        attr(ONMOUSEOUT, null, null, 0, IMPLIED),
        attr(ONKEYPRESS, null, null, 0, IMPLIED),
        attr(ONKEYDOWN, null, null, 0, IMPLIED),
        attr(ONKEYUP, null, null, 0, IMPLIED),
        attr(ALIGN, null,  new String[] { LEFT, CENTER, RIGHT }, 0, IMPLIED),
        attr(NOSHADE, null,  new String[] { NOSHADE }, 0, IMPLIED),
        attr(SIZE, null, null, 0, IMPLIED),
        attr(WIDTH, null, null, 0, IMPLIED)
      }
    );
      defElement(HTML, 0, true, true, createHtmlContentModel(),
      NONE
      ,
      new String[] {
        HEAD, BODY
      }
    ,
      new AttributeList[] {
        attr(LANG, null, null, 0, IMPLIED),
        attr(DIR, null,  new String[] { LTR, RTL }, 0, IMPLIED),
        attr(VERSION, DTD_NAME, null, 0, FIXED)
      }
    );
      defElement(I, 0, false, false, null,
      NONE
      ,
      new String[] {
        PCDATA, A, ABBR, ACRONYM,
        APPLET, B, BASEFONT, BDO, BIG,
        BR, BUTTON, CITE, CODE, DFN,
        EM, FONT, I, IFRAME, IMG,
        INPUT, KBD, LABEL, MAP, OBJECT,
        Q, S, SAMP, SCRIPT, SELECT,
        SMALL, SPAN, STRIKE, STRONG, SUB,
        SUP, TEXTAREA, TT, U, VAR
      }
    ,
      new AttributeList[] {
        attr(sID, null, null, ID, IMPLIED),
        attr(CLASS, null, null, 0, IMPLIED),
        attr(STYLE, null, null, 0, IMPLIED),
        attr(TITLE, null, null, 0, IMPLIED),
        attr(LANG, null, null, 0, IMPLIED),
        attr(DIR, null,  new String[] { LTR, RTL }, 0, IMPLIED),
        attr(ONCLICK, null, null, 0, IMPLIED),
        attr(ONDBLCLICK, null, null, 0, IMPLIED),
        attr(ONMOUSEDOWN, null, null, 0, IMPLIED),
        attr(ONMOUSEUP, null, null, 0, IMPLIED),
        attr(ONMOUSEOVER, null, null, 0, IMPLIED),
        attr(ONMOUSEMOVE, null, null, 0, IMPLIED),
        attr(ONMOUSEOUT, null, null, 0, IMPLIED),
        attr(ONKEYPRESS, null, null, 0, IMPLIED),
        attr(ONKEYDOWN, null, null, 0, IMPLIED),
        attr(ONKEYUP, null, null, 0, IMPLIED)
      }
    );
      defElement(IFRAME, 0, false, false, null,
      NONE
      ,
      new String[] {
        PCDATA, A, ABBR, ACRONYM,
        APPLET, B, BASEFONT, BDO, BIG,
        BR, BUTTON, CITE, CODE, DFN,
        EM, FONT, I, IFRAME, IMG,
        INPUT, KBD, LABEL, MAP, OBJECT,
        Q, S, SAMP, SCRIPT, SELECT,
        SMALL, SPAN, STRIKE, STRONG, SUB,
        SUP, TEXTAREA, TT, U, VAR,
        ADDRESS, BLOCKQUOTE, CENTER, DIR, DIV,
        DL, FIELDSET, FORM, H1, H2,
        H3, H4, H5, H6, HR,
        ISINDEX, MENU, NOFRAMES, NOSCRIPT, OL,
        P, PRE, TABLE, UL
      }
    ,
      new AttributeList[] {
        attr(sID, null, null, ID, IMPLIED),
        attr(CLASS, null, null, 0, IMPLIED),
        attr(STYLE, null, null, 0, IMPLIED),
        attr(TITLE, null, null, 0, IMPLIED),
        attr(LONGDESC, null, null, 0, IMPLIED),
        attr(sNAME, null, null, 0, IMPLIED),
        attr(SRC, null, null, 0, IMPLIED),
        attr(FRAMEBORDER, C_1,  new String[] { C_1, C_0 }, 0, DEFAULT),
        attr(MARGINWIDTH, null, null, PIXELS, IMPLIED),
        attr(MARGINHEIGHT, null, null, PIXELS, IMPLIED),
        attr(SCROLLING, AUTO,  new String[] { YES, NO, AUTO }, 0, DEFAULT),
        attr(ALIGN, null,  new String[] { TOP, MIDDLE, BOTTOM, LEFT, RIGHT },
          0, IMPLIED),
        attr(HEIGHT, null, null, 0, IMPLIED),
        attr(WIDTH, null, null, 0, IMPLIED)
      }
    );
      defElement(IMG, EMPTY, false, true, null,
      NONE
      ,
      NONE
    ,
      new AttributeList[] {
        attr(sID, null, null, ID, IMPLIED),
        attr(CLASS, null, null, 0, IMPLIED),
        attr(STYLE, null, null, 0, IMPLIED),
        attr(TITLE, null, null, 0, IMPLIED),
        attr(LANG, null, null, 0, IMPLIED),
        attr(DIR, null,  new String[] { LTR, RTL }, 0, IMPLIED),
        attr(ONCLICK, null, null, 0, IMPLIED),
        attr(ONDBLCLICK, null, null, 0, IMPLIED),
        attr(ONMOUSEDOWN, null, null, 0, IMPLIED),
        attr(ONMOUSEUP, null, null, 0, IMPLIED),
        attr(ONMOUSEOVER, null, null, 0, IMPLIED),
        attr(ONMOUSEMOVE, null, null, 0, IMPLIED),
        attr(ONMOUSEOUT, null, null, 0, IMPLIED),
        attr(ONKEYPRESS, null, null, 0, IMPLIED),
        attr(ONKEYDOWN, null, null, 0, IMPLIED),
        attr(ONKEYUP, null, null, 0, IMPLIED),
        attr(SRC, null, null, 0, REQUIRED),
        attr(ALT, null, null, 0, REQUIRED),
        attr(LONGDESC, null, null, 0, IMPLIED),
        attr(sNAME, null, null, 0, IMPLIED),
        attr(HEIGHT, null, null, 0, IMPLIED),
        attr(WIDTH, null, null, 0, IMPLIED),
        attr(USEMAP, null, null, 0, IMPLIED),
        attr(ISMAP, null,  new String[] { ISMAP }, 0, IMPLIED),
        attr(ALIGN, null,  new String[] { TOP, MIDDLE, BOTTOM, LEFT, RIGHT },
          0, IMPLIED),
        attr(BORDER, null, null, PIXELS, IMPLIED),
        attr(HSPACE, null, null, 0, IMPLIED),
        attr(VSPACE, null, null, 0, IMPLIED)
      }
    );

  }

  /**
   * Define fourth sixth of elements of this DTD.
   */
  private void defineElements4()
  {
    /* Define the elements. */
      defElement(INPUT, EMPTY, false, true, null,
      NONE
      ,
      NONE
    ,
      new AttributeList[] {
        attr(sID, null, null, ID, IMPLIED),
        attr(CLASS, null, null, 0, IMPLIED),
        attr(STYLE, null, null, 0, IMPLIED),
        attr(TITLE, null, null, 0, IMPLIED),
        attr(LANG, null, null, 0, IMPLIED),
        attr(DIR, null,  new String[] { LTR, RTL }, 0, IMPLIED),
        attr(ONCLICK, null, null, 0, IMPLIED),
        attr(ONDBLCLICK, null, null, 0, IMPLIED),
        attr(ONMOUSEDOWN, null, null, 0, IMPLIED),
        attr(ONMOUSEUP, null, null, 0, IMPLIED),
        attr(ONMOUSEOVER, null, null, 0, IMPLIED),
        attr(ONMOUSEMOVE, null, null, 0, IMPLIED),
        attr(ONMOUSEOUT, null, null, 0, IMPLIED),
        attr(ONKEYPRESS, null, null, 0, IMPLIED),
        attr(ONKEYDOWN, null, null, 0, IMPLIED),
        attr(ONKEYUP, null, null, 0, IMPLIED),
        attr(TYPE, TEXT,  new String[] { TEXT, PASSWORD, CHECKBOX, RADIO,
          SUBMIT, RESET, FILE, HIDDEN, IMAGE, BUTTON }, 0, DEFAULT),
        attr(sNAME, null, null, 0, IMPLIED),
        attr(VALUE, null, null, 0, IMPLIED),
        attr(CHECKED, null,  new String[] { CHECKED }, 0, IMPLIED),
        attr(DISABLED, null,  new String[] { DISABLED }, 0, IMPLIED),
        attr(READONLY, null,  new String[] { READONLY }, 0, IMPLIED),
        attr(SIZE, null, null, 0, IMPLIED),
        attr(MAXLENGTH, null, null, 0, IMPLIED),
        attr(SRC, null, null, 0, IMPLIED),
        attr(ALT, null, null, 0, IMPLIED),
        attr(USEMAP, null, null, 0, IMPLIED),
        attr(ISMAP, null,  new String[] { ISMAP }, 0, IMPLIED),
        attr(TABINDEX, null, null, NUMBER, IMPLIED),
        attr(ACCESSKEY, null, null, 0, IMPLIED),
        attr(ONFOCUS, null, null, 0, IMPLIED),
        attr(ONBLUR, null, null, 0, IMPLIED),
        attr(ONSELECT, null, null, 0, IMPLIED),
        attr(ONCHANGE, null, null, 0, IMPLIED),
        attr(ACCEPT, null, null, 0, IMPLIED),
        attr(ALIGN, null,  new String[] { TOP, MIDDLE, BOTTOM, LEFT, RIGHT },
          0, IMPLIED)
      }
    );
      defElement(INS, 0, false, false, null,
      NONE
      ,
      new String[] {
        PCDATA, A, ABBR, ACRONYM,
        APPLET, B, BASEFONT, BDO, BIG,
        BR, BUTTON, CITE, CODE, DFN,
        EM, FONT, I, IFRAME, IMG,
        INPUT, KBD, LABEL, MAP, OBJECT,
        Q, S, SAMP, SCRIPT, SELECT,
        SMALL, SPAN, STRIKE, STRONG, SUB,
        SUP, TEXTAREA, TT, U, VAR,
        ADDRESS, BLOCKQUOTE, CENTER, DIR, DIV,
        DL, FIELDSET, FORM, H1, H2,
        H3, H4, H5, H6, HR,
        ISINDEX, MENU, NOFRAMES, NOSCRIPT, OL,
        P, PRE, TABLE, UL
      }
    ,
      new AttributeList[] {
        attr(sID, null, null, ID, IMPLIED),
        attr(CLASS, null, null, 0, IMPLIED),
        attr(STYLE, null, null, 0, IMPLIED),
        attr(TITLE, null, null, 0, IMPLIED),
        attr(LANG, null, null, 0, IMPLIED),
        attr(DIR, null,  new String[] { LTR, RTL }, 0, IMPLIED),
        attr(ONCLICK, null, null, 0, IMPLIED),
        attr(ONDBLCLICK, null, null, 0, IMPLIED),
        attr(ONMOUSEDOWN, null, null, 0, IMPLIED),
        attr(ONMOUSEUP, null, null, 0, IMPLIED),
        attr(ONMOUSEOVER, null, null, 0, IMPLIED),
        attr(ONMOUSEMOVE, null, null, 0, IMPLIED),
        attr(ONMOUSEOUT, null, null, 0, IMPLIED),
        attr(ONKEYPRESS, null, null, 0, IMPLIED),
        attr(ONKEYDOWN, null, null, 0, IMPLIED),
        attr(ONKEYUP, null, null, 0, IMPLIED),
        attr(CITE, null, null, 0, IMPLIED),
        attr(DATETIME, null, null, 0, IMPLIED)
      }
    );
      defElement(ISINDEX, EMPTY, false, true, null,
      NONE
      ,
      NONE
    ,
      new AttributeList[] {
        attr(sID, null, null, ID, IMPLIED),
        attr(CLASS, null, null, 0, IMPLIED),
        attr(STYLE, null, null, 0, IMPLIED),
        attr(TITLE, null, null, 0, IMPLIED),
        attr(LANG, null, null, 0, IMPLIED),
        attr(DIR, null,  new String[] { LTR, RTL }, 0, IMPLIED),
        attr(PROMPT, null, null, 0, IMPLIED)
      }
    );
      defElement(KBD, 0, false, false, null,
      NONE
      ,
      new String[] {
        PCDATA, A, ABBR, ACRONYM,
        APPLET, B, BASEFONT, BDO, BIG,
        BR, BUTTON, CITE, CODE, DFN,
        EM, FONT, I, IFRAME, IMG,
        INPUT, KBD, LABEL, MAP, OBJECT,
        Q, S, SAMP, SCRIPT, SELECT,
        SMALL, SPAN, STRIKE, STRONG, SUB,
        SUP, TEXTAREA, TT, U, VAR
      }
    ,
      new AttributeList[] {
        attr(sID, null, null, ID, IMPLIED),
        attr(CLASS, null, null, 0, IMPLIED),
        attr(STYLE, null, null, 0, IMPLIED),
        attr(TITLE, null, null, 0, IMPLIED),
        attr(LANG, null, null, 0, IMPLIED),
        attr(DIR, null,  new String[] { LTR, RTL }, 0, IMPLIED),
        attr(ONCLICK, null, null, 0, IMPLIED),
        attr(ONDBLCLICK, null, null, 0, IMPLIED),
        attr(ONMOUSEDOWN, null, null, 0, IMPLIED),
        attr(ONMOUSEUP, null, null, 0, IMPLIED),
        attr(ONMOUSEOVER, null, null, 0, IMPLIED),
        attr(ONMOUSEMOVE, null, null, 0, IMPLIED),
        attr(ONMOUSEOUT, null, null, 0, IMPLIED),
        attr(ONKEYPRESS, null, null, 0, IMPLIED),
        attr(ONKEYDOWN, null, null, 0, IMPLIED),
        attr(ONKEYUP, null, null, 0, IMPLIED)
      }
    );
      defElement(LABEL, 0, false, false, null,
      new String[] {
        LABEL
      }
      ,
      new String[] {
        PCDATA, A, ABBR, ACRONYM,
        APPLET, B, BASEFONT, BDO, BIG,
        BR, BUTTON, CITE, CODE, DFN,
        EM, FONT, I, IFRAME, IMG,
        INPUT, KBD, MAP, OBJECT, Q,
        S, SAMP, SCRIPT, SELECT, SMALL,
        SPAN, STRIKE, STRONG, SUB, SUP,
        TEXTAREA, TT, U, VAR
      }
    ,
      new AttributeList[] {
        attr(sID, null, null, ID, IMPLIED),
        attr(CLASS, null, null, 0, IMPLIED),
        attr(STYLE, null, null, 0, IMPLIED),
        attr(TITLE, null, null, 0, IMPLIED),
        attr(LANG, null, null, 0, IMPLIED),
        attr(DIR, null,  new String[] { LTR, RTL }, 0, IMPLIED),
        attr(ONCLICK, null, null, 0, IMPLIED),
        attr(ONDBLCLICK, null, null, 0, IMPLIED),
        attr(ONMOUSEDOWN, null, null, 0, IMPLIED),
        attr(ONMOUSEUP, null, null, 0, IMPLIED),
        attr(ONMOUSEOVER, null, null, 0, IMPLIED),
        attr(ONMOUSEMOVE, null, null, 0, IMPLIED),
        attr(ONMOUSEOUT, null, null, 0, IMPLIED),
        attr(ONKEYPRESS, null, null, 0, IMPLIED),
        attr(ONKEYDOWN, null, null, 0, IMPLIED),
        attr(ONKEYUP, null, null, 0, IMPLIED),
        attr(FOR, null, null, 0, IMPLIED),
        attr(ACCESSKEY, null, null, 0, IMPLIED),
        attr(ONFOCUS, null, null, 0, IMPLIED),
        attr(ONBLUR, null, null, 0, IMPLIED)
      }
    );
      defElement(LEGEND, 0, false, false, null,
      NONE
      ,
      new String[] {
        PCDATA, A, ABBR, ACRONYM,
        APPLET, B, BASEFONT, BDO, BIG,
        BR, BUTTON, CITE, CODE, DFN,
        EM, FONT, I, IFRAME, IMG,
        INPUT, KBD, LABEL, MAP, OBJECT,
        Q, S, SAMP, SCRIPT, SELECT,
        SMALL, SPAN, STRIKE, STRONG, SUB,
        SUP, TEXTAREA, TT, U, VAR
      }
    ,
      new AttributeList[] {
        attr(sID, null, null, ID, IMPLIED),
        attr(CLASS, null, null, 0, IMPLIED),
        attr(STYLE, null, null, 0, IMPLIED),
        attr(TITLE, null, null, 0, IMPLIED),
        attr(LANG, null, null, 0, IMPLIED),
        attr(DIR, null,  new String[] { LTR, RTL }, 0, IMPLIED),
        attr(ONCLICK, null, null, 0, IMPLIED),
        attr(ONDBLCLICK, null, null, 0, IMPLIED),
        attr(ONMOUSEDOWN, null, null, 0, IMPLIED),
        attr(ONMOUSEUP, null, null, 0, IMPLIED),
        attr(ONMOUSEOVER, null, null, 0, IMPLIED),
        attr(ONMOUSEMOVE, null, null, 0, IMPLIED),
        attr(ONMOUSEOUT, null, null, 0, IMPLIED),
        attr(ONKEYPRESS, null, null, 0, IMPLIED),
        attr(ONKEYDOWN, null, null, 0, IMPLIED),
        attr(ONKEYUP, null, null, 0, IMPLIED),
        attr(ACCESSKEY, null, null, 0, IMPLIED),
        attr(ALIGN, null,  new String[] { TOP, BOTTOM, LEFT, RIGHT },
          0, IMPLIED)
      }
    );
      // LI has a special content model that will be resolved into
      // by transformer.
      defElement(LI, 0, false, true,
        new ContentModel(0,
          new noTagModel(LI), null),
      NONE
      ,
      new String[] {
        PCDATA, A, ABBR, ACRONYM,
        APPLET, B, BASEFONT, BDO, BIG,
        BR, BUTTON, CITE, CODE, DFN,
        EM, FONT, I, IFRAME, IMG,
        INPUT, KBD, LABEL, MAP, OBJECT,
        Q, S, SAMP, SCRIPT, SELECT,
        SMALL, SPAN, STRIKE, STRONG, SUB,
        SUP, TEXTAREA, TT, U, VAR,
        ADDRESS, BLOCKQUOTE, CENTER, DIR, DIV,
        DL, FIELDSET, FORM, H1, H2,
        H3, H4, H5, H6, HR,
        ISINDEX, MENU, NOFRAMES, NOSCRIPT, OL,
        P, PRE, TABLE, UL
      }
    ,
      new AttributeList[] {
        attr(sID, null, null, ID, IMPLIED),
        attr(CLASS, null, null, 0, IMPLIED),
        attr(STYLE, null, null, 0, IMPLIED),
        attr(TITLE, null, null, 0, IMPLIED),
        attr(LANG, null, null, 0, IMPLIED),
        attr(DIR, null,  new String[] { LTR, RTL }, 0, IMPLIED),
        attr(ONCLICK, null, null, 0, IMPLIED),
        attr(ONDBLCLICK, null, null, 0, IMPLIED),
        attr(ONMOUSEDOWN, null, null, 0, IMPLIED),
        attr(ONMOUSEUP, null, null, 0, IMPLIED),
        attr(ONMOUSEOVER, null, null, 0, IMPLIED),
        attr(ONMOUSEMOVE, null, null, 0, IMPLIED),
        attr(ONMOUSEOUT, null, null, 0, IMPLIED),
        attr(ONKEYPRESS, null, null, 0, IMPLIED),
        attr(ONKEYDOWN, null, null, 0, IMPLIED),
        attr(ONKEYUP, null, null, 0, IMPLIED),
        attr(TYPE, null, null, 0, IMPLIED),
        attr(VALUE, null, null, NUMBER, IMPLIED)
      }
    );
      defElement(LINK, EMPTY, false, true, null,
      NONE
      ,
      NONE
    ,
      new AttributeList[] {
        attr(sID, null, null, ID, IMPLIED),
        attr(CLASS, null, null, 0, IMPLIED),
        attr(STYLE, null, null, 0, IMPLIED),
        attr(TITLE, null, null, 0, IMPLIED),
        attr(LANG, null, null, 0, IMPLIED),
        attr(DIR, null,  new String[] { LTR, RTL }, 0, IMPLIED),
        attr(ONCLICK, null, null, 0, IMPLIED),
        attr(ONDBLCLICK, null, null, 0, IMPLIED),
        attr(ONMOUSEDOWN, null, null, 0, IMPLIED),
        attr(ONMOUSEUP, null, null, 0, IMPLIED),
        attr(ONMOUSEOVER, null, null, 0, IMPLIED),
        attr(ONMOUSEMOVE, null, null, 0, IMPLIED),
        attr(ONMOUSEOUT, null, null, 0, IMPLIED),
        attr(ONKEYPRESS, null, null, 0, IMPLIED),
        attr(ONKEYDOWN, null, null, 0, IMPLIED),
        attr(ONKEYUP, null, null, 0, IMPLIED),
        attr(CHARSET, null, null, 0, IMPLIED),
        attr(HREF, null, null, 0, IMPLIED),
        attr(HREFLANG, null, null, 0, IMPLIED),
        attr(TYPE, null, null, 0, IMPLIED),
        attr(REL, null, null, 0, IMPLIED),
        attr(REV, null, null, 0, IMPLIED),
        attr(MEDIA, null, null, 0, IMPLIED),
        attr(TARGET, null, null, 0, IMPLIED)
      }
    );
      defElement(MAP, 0, false, false, null,
      NONE
      ,
      new String[] {
        ADDRESS, BLOCKQUOTE, CENTER, DIR,
        DIV, DL, FIELDSET, FORM, H1,
        H2, H3, H4, H5, H6,
        HR, ISINDEX, MENU, NOFRAMES, NOSCRIPT,
        OL, P, PRE, TABLE, UL,
        AREA
      }
    ,
      new AttributeList[] {
        attr(sID, null, null, ID, IMPLIED),
        attr(CLASS, null, null, 0, IMPLIED),
        attr(STYLE, null, null, 0, IMPLIED),
        attr(TITLE, null, null, 0, IMPLIED),
        attr(LANG, null, null, 0, IMPLIED),
        attr(DIR, null,  new String[] { LTR, RTL }, 0, IMPLIED),
        attr(ONCLICK, null, null, 0, IMPLIED),
        attr(ONDBLCLICK, null, null, 0, IMPLIED),
        attr(ONMOUSEDOWN, null, null, 0, IMPLIED),
        attr(ONMOUSEUP, null, null, 0, IMPLIED),
        attr(ONMOUSEOVER, null, null, 0, IMPLIED),
        attr(ONMOUSEMOVE, null, null, 0, IMPLIED),
        attr(ONMOUSEOUT, null, null, 0, IMPLIED),
        attr(ONKEYPRESS, null, null, 0, IMPLIED),
        attr(ONKEYDOWN, null, null, 0, IMPLIED),
        attr(ONKEYUP, null, null, 0, IMPLIED),
        attr(sNAME, null, null, 0, REQUIRED)
      }
    );
      defElement(MENU, 0, false, false, createListModel(),
      new String[] {
        ADDRESS, BLOCKQUOTE, CENTER, DIR,
        DIV, DL, FIELDSET, FORM, H1,
        H2, H3, H4, H5, H6,
        HR, ISINDEX, MENU, NOFRAMES, NOSCRIPT,
        OL, P, PRE, TABLE, UL
      }
      ,
      new String[] {
        LI, UL, OL
      }
    ,
      new AttributeList[] {
        attr(sID, null, null, ID, IMPLIED),
        attr(CLASS, null, null, 0, IMPLIED),
        attr(STYLE, null, null, 0, IMPLIED),
        attr(TITLE, null, null, 0, IMPLIED),
        attr(LANG, null, null, 0, IMPLIED),
        attr(DIR, null,  new String[] { LTR, RTL }, 0, IMPLIED),
        attr(ONCLICK, null, null, 0, IMPLIED),
        attr(ONDBLCLICK, null, null, 0, IMPLIED),
        attr(ONMOUSEDOWN, null, null, 0, IMPLIED),
        attr(ONMOUSEUP, null, null, 0, IMPLIED),
        attr(ONMOUSEOVER, null, null, 0, IMPLIED),
        attr(ONMOUSEMOVE, null, null, 0, IMPLIED),
        attr(ONMOUSEOUT, null, null, 0, IMPLIED),
        attr(ONKEYPRESS, null, null, 0, IMPLIED),
        attr(ONKEYDOWN, null, null, 0, IMPLIED),
        attr(ONKEYUP, null, null, 0, IMPLIED),
        attr(COMPACT, null,  new String[] { COMPACT }, 0, IMPLIED)
      }
    );
      defElement(META, EMPTY, false, true, null,
      NONE
      ,
      NONE
    ,
      new AttributeList[] {
        attr(LANG, null, null, 0, IMPLIED),
        attr(DIR, null,  new String[] { LTR, RTL }, 0, IMPLIED),
        attr(HTTPEQUIV, null, null, 0, IMPLIED),
        attr(sNAME, null, null, NAME, IMPLIED),
        attr(CONTENT, null, null, 0, REQUIRED),
        attr(SCHEME, null, null, 0, IMPLIED)
      }
    );
      defElement(NOFRAMES, 0, false, false, null,
      NONE
      ,
      new String[] {
        PCDATA, A, ABBR, ACRONYM,
        APPLET, B, BASEFONT, BDO, BIG,
        BR, BUTTON, CITE, CODE, DFN,
        EM, FONT, I, IFRAME, IMG,
        INPUT, KBD, LABEL, MAP, OBJECT,
        Q, S, SAMP, SCRIPT, SELECT,
        SMALL, SPAN, STRIKE, STRONG, SUB,
        SUP, TEXTAREA, TT, U, VAR,
        ADDRESS, BLOCKQUOTE, CENTER, DIR, DIV,
        DL, FIELDSET, FORM, H1, H2,
        H3, H4, H5, H6, HR,
        ISINDEX, MENU, NOFRAMES, NOSCRIPT, OL,
        P, PRE, TABLE, UL
      }
    ,
      new AttributeList[] {
        attr(sID, null, null, ID, IMPLIED),
        attr(CLASS, null, null, 0, IMPLIED),
        attr(STYLE, null, null, 0, IMPLIED),
        attr(TITLE, null, null, 0, IMPLIED),
        attr(LANG, null, null, 0, IMPLIED),
        attr(DIR, null,  new String[] { LTR, RTL }, 0, IMPLIED),
        attr(ONCLICK, null, null, 0, IMPLIED),
        attr(ONDBLCLICK, null, null, 0, IMPLIED),
        attr(ONMOUSEDOWN, null, null, 0, IMPLIED),
        attr(ONMOUSEUP, null, null, 0, IMPLIED),
        attr(ONMOUSEOVER, null, null, 0, IMPLIED),
        attr(ONMOUSEMOVE, null, null, 0, IMPLIED),
        attr(ONMOUSEOUT, null, null, 0, IMPLIED),
        attr(ONKEYPRESS, null, null, 0, IMPLIED),
        attr(ONKEYDOWN, null, null, 0, IMPLIED),
        attr(ONKEYUP, null, null, 0, IMPLIED)
      }
    );
      defElement(NOSCRIPT, 0, false, false, null,
      NONE
      ,
      new String[] {
        PCDATA, A, ABBR, ACRONYM,
        APPLET, B, BASEFONT, BDO, BIG,
        BR, BUTTON, CITE, CODE, DFN,
        EM, FONT, I, IFRAME, IMG,
        INPUT, KBD, LABEL, MAP, OBJECT,
        Q, S, SAMP, SCRIPT, SELECT,
        SMALL, SPAN, STRIKE, STRONG, SUB,
        SUP, TEXTAREA, TT, U, VAR,
        ADDRESS, BLOCKQUOTE, CENTER, DIR, DIV,
        DL, FIELDSET, FORM, H1, H2,
        H3, H4, H5, H6, HR,
        ISINDEX, MENU, NOFRAMES, NOSCRIPT, OL,
        P, PRE, TABLE, UL
      }
    ,
      new AttributeList[] {
        attr(sID, null, null, ID, IMPLIED),
        attr(CLASS, null, null, 0, IMPLIED),
        attr(STYLE, null, null, 0, IMPLIED),
        attr(TITLE, null, null, 0, IMPLIED),
        attr(LANG, null, null, 0, IMPLIED),
        attr(DIR, null,  new String[] { LTR, RTL }, 0, IMPLIED),
        attr(ONCLICK, null, null, 0, IMPLIED),
        attr(ONDBLCLICK, null, null, 0, IMPLIED),
        attr(ONMOUSEDOWN, null, null, 0, IMPLIED),
        attr(ONMOUSEUP, null, null, 0, IMPLIED),
        attr(ONMOUSEOVER, null, null, 0, IMPLIED),
        attr(ONMOUSEMOVE, null, null, 0, IMPLIED),
        attr(ONMOUSEOUT, null, null, 0, IMPLIED),
        attr(ONKEYPRESS, null, null, 0, IMPLIED),
        attr(ONKEYDOWN, null, null, 0, IMPLIED),
        attr(ONKEYUP, null, null, 0, IMPLIED)
      }
    );
      defElement(OBJECT, 0, false, false, null,
      NONE
      ,
      new String[] {
        PCDATA, A, ABBR, ACRONYM,
        APPLET, B, BASEFONT, BDO, BIG,
        BR, BUTTON, CITE, CODE, DFN,
        EM, FONT, I, IFRAME, IMG,
        INPUT, KBD, LABEL, MAP, OBJECT,
        Q, S, SAMP, SCRIPT, SELECT,
        SMALL, SPAN, STRIKE, STRONG, SUB,
        SUP, TEXTAREA, TT, U, VAR,
        ADDRESS, BLOCKQUOTE, CENTER, DIR, DIV,
        DL, FIELDSET, FORM, H1, H2,
        H3, H4, H5, H6, HR,
        ISINDEX, MENU, NOFRAMES, NOSCRIPT, OL,
        P, PRE, TABLE, UL, PARAM
      }
    ,
      new AttributeList[] {
        attr(sID, null, null, ID, IMPLIED),
        attr(CLASS, null, null, 0, IMPLIED),
        attr(STYLE, null, null, 0, IMPLIED),
        attr(TITLE, null, null, 0, IMPLIED),
        attr(LANG, null, null, 0, IMPLIED),
        attr(DIR, null,  new String[] { LTR, RTL }, 0, IMPLIED),
        attr(ONCLICK, null, null, 0, IMPLIED),
        attr(ONDBLCLICK, null, null, 0, IMPLIED),
        attr(ONMOUSEDOWN, null, null, 0, IMPLIED),
        attr(ONMOUSEUP, null, null, 0, IMPLIED),
        attr(ONMOUSEOVER, null, null, 0, IMPLIED),
        attr(ONMOUSEMOVE, null, null, 0, IMPLIED),
        attr(ONMOUSEOUT, null, null, 0, IMPLIED),
        attr(ONKEYPRESS, null, null, 0, IMPLIED),
        attr(ONKEYDOWN, null, null, 0, IMPLIED),
        attr(ONKEYUP, null, null, 0, IMPLIED),
        attr(DECLARE, null,  new String[] { DECLARE }, 0, IMPLIED),
        attr(CLASSID, null, null, 0, IMPLIED),
        attr(CODEBASE, null, null, 0, IMPLIED),
        attr(DATA, null, null, 0, IMPLIED),
        attr(TYPE, null, null, 0, IMPLIED),
        attr(CODETYPE, null, null, 0, IMPLIED),
        attr(ARCHIVE, null, null, 0, IMPLIED),
        attr(STANDBY, null, null, 0, IMPLIED),
        attr(HEIGHT, null, null, 0, IMPLIED),
        attr(WIDTH, null, null, 0, IMPLIED),
        attr(USEMAP, null, null, 0, IMPLIED),
        attr(sNAME, null, null, 0, IMPLIED),
        attr(TABINDEX, null, null, NUMBER, IMPLIED),
        attr(ALIGN, null,  new String[] { TOP, MIDDLE, BOTTOM, LEFT, RIGHT },
          0, IMPLIED),
        attr(BORDER, null, null, PIXELS, IMPLIED),
        attr(HSPACE, null, null, 0, IMPLIED),
        attr(VSPACE, null, null, 0, IMPLIED)
      }
    );

  }

  /**
   * Define fifth sixth of elements of this DTD.
   */
  private void defineElements5()
  {
    /* Define the elements. */
      defElement(OL, 0, false, false, createListModel(),
      NONE
      ,
      new String[] {
      // See note on the createListModel method
      LI, UL, OL
      }
    ,
      new AttributeList[] {
        attr(sID, null, null, ID, IMPLIED),
        attr(CLASS, null, null, 0, IMPLIED),
        attr(STYLE, null, null, 0, IMPLIED),
        attr(TITLE, null, null, 0, IMPLIED),
        attr(LANG, null, null, 0, IMPLIED),
        attr(DIR, null,  new String[] { LTR, RTL }, 0, IMPLIED),
        attr(ONCLICK, null, null, 0, IMPLIED),
        attr(ONDBLCLICK, null, null, 0, IMPLIED),
        attr(ONMOUSEDOWN, null, null, 0, IMPLIED),
        attr(ONMOUSEUP, null, null, 0, IMPLIED),
        attr(ONMOUSEOVER, null, null, 0, IMPLIED),
        attr(ONMOUSEMOVE, null, null, 0, IMPLIED),
        attr(ONMOUSEOUT, null, null, 0, IMPLIED),
        attr(ONKEYPRESS, null, null, 0, IMPLIED),
        attr(ONKEYDOWN, null, null, 0, IMPLIED),
        attr(ONKEYUP, null, null, 0, IMPLIED),
        attr(TYPE, null, null, 0, IMPLIED),
        attr(COMPACT, null,  new String[] { COMPACT }, 0, IMPLIED),
        attr(START, null, null, 0, IMPLIED)
      }
    );
      defElement(OPTGROUP, 0, false, false, null,
      NONE
      ,
      new String[] {
        OPTION
      }
    ,
      new AttributeList[] {
        attr(sID, null, null, ID, IMPLIED),
        attr(CLASS, null, null, 0, IMPLIED),
        attr(STYLE, null, null, 0, IMPLIED),
        attr(TITLE, null, null, 0, IMPLIED),
        attr(LANG, null, null, 0, IMPLIED),
        attr(DIR, null,  new String[] { LTR, RTL }, 0, IMPLIED),
        attr(ONCLICK, null, null, 0, IMPLIED),
        attr(ONDBLCLICK, null, null, 0, IMPLIED),
        attr(ONMOUSEDOWN, null, null, 0, IMPLIED),
        attr(ONMOUSEUP, null, null, 0, IMPLIED),
        attr(ONMOUSEOVER, null, null, 0, IMPLIED),
        attr(ONMOUSEMOVE, null, null, 0, IMPLIED),
        attr(ONMOUSEOUT, null, null, 0, IMPLIED),
        attr(ONKEYPRESS, null, null, 0, IMPLIED),
        attr(ONKEYDOWN, null, null, 0, IMPLIED),
        attr(ONKEYUP, null, null, 0, IMPLIED),
        attr(DISABLED, null,  new String[] { DISABLED }, 0, IMPLIED),
        attr(LABEL, null, null, 0, REQUIRED)
      }
    );
      defElement(OPTION, 0, false, true, new ContentModel(0,
       new PCDATAonly_model(), null),
       NONE,
       new String[] {
         PCDATA
       }
      ,
      new AttributeList[] {
        attr(sID, null, null, ID, IMPLIED),
        attr(CLASS, null, null, 0, IMPLIED),
        attr(STYLE, null, null, 0, IMPLIED),
        attr(TITLE, null, null, 0, IMPLIED),
        attr(LANG, null, null, 0, IMPLIED),
        attr(DIR, null,  new String[] { LTR, RTL }, 0, IMPLIED),
        attr(ONCLICK, null, null, 0, IMPLIED),
        attr(ONDBLCLICK, null, null, 0, IMPLIED),
        attr(ONMOUSEDOWN, null, null, 0, IMPLIED),
        attr(ONMOUSEUP, null, null, 0, IMPLIED),
        attr(ONMOUSEOVER, null, null, 0, IMPLIED),
        attr(ONMOUSEMOVE, null, null, 0, IMPLIED),
        attr(ONMOUSEOUT, null, null, 0, IMPLIED),
        attr(ONKEYPRESS, null, null, 0, IMPLIED),
        attr(ONKEYDOWN, null, null, 0, IMPLIED),
        attr(ONKEYUP, null, null, 0, IMPLIED),
        attr(SELECTED, null,  new String[] { SELECTED }, 0, IMPLIED),
        attr(DISABLED, null,  new String[] { DISABLED }, 0, IMPLIED),
        attr(LABEL, null, null, 0, IMPLIED),
        attr(VALUE, null, null, 0, IMPLIED)
      }
    );

      // Headers in the paragraph are not allowed.
      defElement(P, 0, false, true, new ContentModel( 0,
       new noTagModel(new String[] { P, H1, H2, H3, H4, H5, H6 }), null),
      NONE
      ,
      new String[] {
        PCDATA, A, ABBR, ACRONYM,
        APPLET, B, BASEFONT, BDO, BIG,
        BR, BUTTON, CITE, CODE, DFN,
        EM, FONT, I, IFRAME, IMG,
        INPUT, KBD, LABEL, MAP, OBJECT,
        Q, S, SAMP, SCRIPT, SELECT,
        SMALL, SPAN, STRIKE, STRONG, SUB,
        SUP, TEXTAREA, TT, U, VAR
      }
    ,
      new AttributeList[] {
        attr(sID, null, null, ID, IMPLIED),
        attr(CLASS, null, null, 0, IMPLIED),
        attr(STYLE, null, null, 0, IMPLIED),
        attr(TITLE, null, null, 0, IMPLIED),
        attr(LANG, null, null, 0, IMPLIED),
        attr(DIR, null,  new String[] { LTR, RTL }, 0, IMPLIED),
        attr(ONCLICK, null, null, 0, IMPLIED),
        attr(ONDBLCLICK, null, null, 0, IMPLIED),
        attr(ONMOUSEDOWN, null, null, 0, IMPLIED),
        attr(ONMOUSEUP, null, null, 0, IMPLIED),
        attr(ONMOUSEOVER, null, null, 0, IMPLIED),
        attr(ONMOUSEMOVE, null, null, 0, IMPLIED),
        attr(ONMOUSEOUT, null, null, 0, IMPLIED),
        attr(ONKEYPRESS, null, null, 0, IMPLIED),
        attr(ONKEYDOWN, null, null, 0, IMPLIED),
        attr(ONKEYUP, null, null, 0, IMPLIED),
        attr(ALIGN, null,  new String[] { LEFT, CENTER, RIGHT, JUSTIFY },
          0, IMPLIED)
      }
    );
      defElement(PARAM, EMPTY, false, true, null,
      NONE
      ,
      NONE
    ,
      new AttributeList[] {
        attr(sID, null, null, ID, IMPLIED),
        attr(sNAME, null, null, 0, REQUIRED),
        attr(VALUE, null, null, 0, IMPLIED),
        attr(VALUETYPE, DATA,  new String[] { DATA, REF, OBJECT }, 0, DEFAULT),
        attr(TYPE, null, null, 0, IMPLIED)
      }
    );
      defElement(PRE, 0, false, false, null,
      new String[] {
        APPLET, BASEFONT, BIG, FONT,
        IMG, OBJECT, SMALL, SUB, SUP
      }
      ,
      new String[] {
        PCDATA, A, ABBR, ACRONYM,
        B, BDO, BR, BUTTON, CITE,
        CODE, DFN, EM, I, IFRAME,
        INPUT, KBD, LABEL, MAP, Q,
        S, SAMP, SCRIPT, SELECT, SPAN,
        STRIKE, STRONG, TEXTAREA, TT, U,
        VAR
      }
    ,
      new AttributeList[] {
        attr(sID, null, null, ID, IMPLIED),
        attr(CLASS, null, null, 0, IMPLIED),
        attr(STYLE, null, null, 0, IMPLIED),
        attr(TITLE, null, null, 0, IMPLIED),
        attr(LANG, null, null, 0, IMPLIED),
        attr(DIR, null,  new String[] { LTR, RTL }, 0, IMPLIED),
        attr(ONCLICK, null, null, 0, IMPLIED),
        attr(ONDBLCLICK, null, null, 0, IMPLIED),
        attr(ONMOUSEDOWN, null, null, 0, IMPLIED),
        attr(ONMOUSEUP, null, null, 0, IMPLIED),
        attr(ONMOUSEOVER, null, null, 0, IMPLIED),
        attr(ONMOUSEMOVE, null, null, 0, IMPLIED),
        attr(ONMOUSEOUT, null, null, 0, IMPLIED),
        attr(ONKEYPRESS, null, null, 0, IMPLIED),
        attr(ONKEYDOWN, null, null, 0, IMPLIED),
        attr(ONKEYUP, null, null, 0, IMPLIED),
        attr(WIDTH, null, null, NUMBER, IMPLIED)
      }
    );
      defElement(Q, 0, false, false, null,
      NONE
      ,
      new String[] {
        PCDATA, A, ABBR, ACRONYM,
        APPLET, B, BASEFONT, BDO, BIG,
        BR, BUTTON, CITE, CODE, DFN,
        EM, FONT, I, IFRAME, IMG,
        INPUT, KBD, LABEL, MAP, OBJECT,
        Q, S, SAMP, SCRIPT, SELECT,
        SMALL, SPAN, STRIKE, STRONG, SUB,
        SUP, TEXTAREA, TT, U, VAR
      }
    ,
      new AttributeList[] {
        attr(sID, null, null, ID, IMPLIED),
        attr(CLASS, null, null, 0, IMPLIED),
        attr(STYLE, null, null, 0, IMPLIED),
        attr(TITLE, null, null, 0, IMPLIED),
        attr(LANG, null, null, 0, IMPLIED),
        attr(DIR, null,  new String[] { LTR, RTL }, 0, IMPLIED),
        attr(ONCLICK, null, null, 0, IMPLIED),
        attr(ONDBLCLICK, null, null, 0, IMPLIED),
        attr(ONMOUSEDOWN, null, null, 0, IMPLIED),
        attr(ONMOUSEUP, null, null, 0, IMPLIED),
        attr(ONMOUSEOVER, null, null, 0, IMPLIED),
        attr(ONMOUSEMOVE, null, null, 0, IMPLIED),
        attr(ONMOUSEOUT, null, null, 0, IMPLIED),
        attr(ONKEYPRESS, null, null, 0, IMPLIED),
        attr(ONKEYDOWN, null, null, 0, IMPLIED),
        attr(ONKEYUP, null, null, 0, IMPLIED),
        attr(CITE, null, null, 0, IMPLIED)
      }
    );
      defElement(S, 0, false, false, null,
      NONE
      ,
      new String[] {
        PCDATA, A, ABBR, ACRONYM,
        APPLET, B, BASEFONT, BDO, BIG,
        BR, BUTTON, CITE, CODE, DFN,
        EM, FONT, I, IFRAME, IMG,
        INPUT, KBD, LABEL, MAP, OBJECT,
        Q, S, SAMP, SCRIPT, SELECT,
        SMALL, SPAN, STRIKE, STRONG, SUB,
        SUP, TEXTAREA, TT, U, VAR
      }
    ,
      new AttributeList[] {
        attr(sID, null, null, ID, IMPLIED),
        attr(CLASS, null, null, 0, IMPLIED),
        attr(STYLE, null, null, 0, IMPLIED),
        attr(TITLE, null, null, 0, IMPLIED),
        attr(LANG, null, null, 0, IMPLIED),
        attr(DIR, null,  new String[] { LTR, RTL }, 0, IMPLIED),
        attr(ONCLICK, null, null, 0, IMPLIED),
        attr(ONDBLCLICK, null, null, 0, IMPLIED),
        attr(ONMOUSEDOWN, null, null, 0, IMPLIED),
        attr(ONMOUSEUP, null, null, 0, IMPLIED),
        attr(ONMOUSEOVER, null, null, 0, IMPLIED),
        attr(ONMOUSEMOVE, null, null, 0, IMPLIED),
        attr(ONMOUSEOUT, null, null, 0, IMPLIED),
        attr(ONKEYPRESS, null, null, 0, IMPLIED),
        attr(ONKEYDOWN, null, null, 0, IMPLIED),
        attr(ONKEYUP, null, null, 0, IMPLIED)
      }
    );
      defElement(SAMP, 0, false, false, null,
      NONE
      ,
      new String[] {
        PCDATA, A, ABBR, ACRONYM,
        APPLET, B, BASEFONT, BDO, BIG,
        BR, BUTTON, CITE, CODE, DFN,
        EM, FONT, I, IFRAME, IMG,
        INPUT, KBD, LABEL, MAP, OBJECT,
        Q, S, SAMP, SCRIPT, SELECT,
        SMALL, SPAN, STRIKE, STRONG, SUB,
        SUP, TEXTAREA, TT, U, VAR
      }
    ,
      new AttributeList[] {
        attr(sID, null, null, ID, IMPLIED),
        attr(CLASS, null, null, 0, IMPLIED),
        attr(STYLE, null, null, 0, IMPLIED),
        attr(TITLE, null, null, 0, IMPLIED),
        attr(LANG, null, null, 0, IMPLIED),
        attr(DIR, null,  new String[] { LTR, RTL }, 0, IMPLIED),
        attr(ONCLICK, null, null, 0, IMPLIED),
        attr(ONDBLCLICK, null, null, 0, IMPLIED),
        attr(ONMOUSEDOWN, null, null, 0, IMPLIED),
        attr(ONMOUSEUP, null, null, 0, IMPLIED),
        attr(ONMOUSEOVER, null, null, 0, IMPLIED),
        attr(ONMOUSEMOVE, null, null, 0, IMPLIED),
        attr(ONMOUSEOUT, null, null, 0, IMPLIED),
        attr(ONKEYPRESS, null, null, 0, IMPLIED),
        attr(ONKEYDOWN, null, null, 0, IMPLIED),
        attr(ONKEYUP, null, null, 0, IMPLIED)
      }
    );
      defElement(SCRIPT, CDATA, false, false, null,
      NONE
      ,
      NONE
    ,
      new AttributeList[] {
        attr(CHARSET, null, null, 0, IMPLIED),
        attr(TYPE, null, null, 0, REQUIRED),
        attr(LANGUAGE, null, null, 0, IMPLIED),
        attr(SRC, null, null, 0, IMPLIED),
        attr(DEFER, null,  new String[] { DEFER }, 0, IMPLIED),
        attr(EVENT, null, null, 0, IMPLIED),
        attr(FOR, null, null, 0, IMPLIED)
      }
    );
      defElement(SELECT, 0, false, false, null,
      NONE
      ,
      new String[] {
        OPTGROUP, OPTION
      }
    ,
      new AttributeList[] {
        attr(sID, null, null, ID, IMPLIED),
        attr(CLASS, null, null, 0, IMPLIED),
        attr(STYLE, null, null, 0, IMPLIED),
        attr(TITLE, null, null, 0, IMPLIED),
        attr(LANG, null, null, 0, IMPLIED),
        attr(DIR, null,  new String[] { LTR, RTL }, 0, IMPLIED),
        attr(ONCLICK, null, null, 0, IMPLIED),
        attr(ONDBLCLICK, null, null, 0, IMPLIED),
        attr(ONMOUSEDOWN, null, null, 0, IMPLIED),
        attr(ONMOUSEUP, null, null, 0, IMPLIED),
        attr(ONMOUSEOVER, null, null, 0, IMPLIED),
        attr(ONMOUSEMOVE, null, null, 0, IMPLIED),
        attr(ONMOUSEOUT, null, null, 0, IMPLIED),
        attr(ONKEYPRESS, null, null, 0, IMPLIED),
        attr(ONKEYDOWN, null, null, 0, IMPLIED),
        attr(ONKEYUP, null, null, 0, IMPLIED),
        attr(sNAME, null, null, 0, IMPLIED),
        attr(SIZE, null, null, NUMBER, IMPLIED),
        attr(MULTIPLE, null,  new String[] { MULTIPLE }, 0, IMPLIED),
        attr(DISABLED, null,  new String[] { DISABLED }, 0, IMPLIED),
        attr(TABINDEX, null, null, NUMBER, IMPLIED),
        attr(ONFOCUS, null, null, 0, IMPLIED),
        attr(ONBLUR, null, null, 0, IMPLIED),
        attr(ONCHANGE, null, null, 0, IMPLIED)
      }
    );
      defElement(SMALL, 0, false, false, null,
      NONE
      ,
      new String[] {
        PCDATA, A, ABBR, ACRONYM,
        APPLET, B, BASEFONT, BDO, BIG,
        BR, BUTTON, CITE, CODE, DFN,
        EM, FONT, I, IFRAME, IMG,
        INPUT, KBD, LABEL, MAP, OBJECT,
        Q, S, SAMP, SCRIPT, SELECT,
        SMALL, SPAN, STRIKE, STRONG, SUB,
        SUP, TEXTAREA, TT, U, VAR
      }
    ,
      new AttributeList[] {
        attr(sID, null, null, ID, IMPLIED),
        attr(CLASS, null, null, 0, IMPLIED),
        attr(STYLE, null, null, 0, IMPLIED),
        attr(TITLE, null, null, 0, IMPLIED),
        attr(LANG, null, null, 0, IMPLIED),
        attr(DIR, null,  new String[] { LTR, RTL }, 0, IMPLIED),
        attr(ONCLICK, null, null, 0, IMPLIED),
        attr(ONDBLCLICK, null, null, 0, IMPLIED),
        attr(ONMOUSEDOWN, null, null, 0, IMPLIED),
        attr(ONMOUSEUP, null, null, 0, IMPLIED),
        attr(ONMOUSEOVER, null, null, 0, IMPLIED),
        attr(ONMOUSEMOVE, null, null, 0, IMPLIED),
        attr(ONMOUSEOUT, null, null, 0, IMPLIED),
        attr(ONKEYPRESS, null, null, 0, IMPLIED),
        attr(ONKEYDOWN, null, null, 0, IMPLIED),
        attr(ONKEYUP, null, null, 0, IMPLIED)
      }
    );
      defElement(SPAN, 0, false, false, null,
      NONE
      ,
      new String[] {
        PCDATA, A, ABBR, ACRONYM,
        APPLET, B, BASEFONT, BDO, BIG,
        BR, BUTTON, CITE, CODE, DFN,
        EM, FONT, I, IFRAME, IMG,
        INPUT, KBD, LABEL, MAP, OBJECT,
        Q, S, SAMP, SCRIPT, SELECT,
        SMALL, SPAN, STRIKE, STRONG, SUB,
        SUP, TEXTAREA, TT, U, VAR
      }
    ,
      new AttributeList[] {
        attr(sID, null, null, ID, IMPLIED),
        attr(CLASS, null, null, 0, IMPLIED),
        attr(STYLE, null, null, 0, IMPLIED),
        attr(TITLE, null, null, 0, IMPLIED),
        attr(LANG, null, null, 0, IMPLIED),
        attr(DIR, null,  new String[] { LTR, RTL }, 0, IMPLIED),
        attr(ONCLICK, null, null, 0, IMPLIED),
        attr(ONDBLCLICK, null, null, 0, IMPLIED),
        attr(ONMOUSEDOWN, null, null, 0, IMPLIED),
        attr(ONMOUSEUP, null, null, 0, IMPLIED),
        attr(ONMOUSEOVER, null, null, 0, IMPLIED),
        attr(ONMOUSEMOVE, null, null, 0, IMPLIED),
        attr(ONMOUSEOUT, null, null, 0, IMPLIED),
        attr(ONKEYPRESS, null, null, 0, IMPLIED),
        attr(ONKEYDOWN, null, null, 0, IMPLIED),
        attr(ONKEYUP, null, null, 0, IMPLIED)
      }
    );
      defElement(STRIKE, 0, false, false, null,
      NONE
      ,
      new String[] {
        PCDATA, A, ABBR, ACRONYM,
        APPLET, B, BASEFONT, BDO, BIG,
        BR, BUTTON, CITE, CODE, DFN,
        EM, FONT, I, IFRAME, IMG,
        INPUT, KBD, LABEL, MAP, OBJECT,
        Q, S, SAMP, SCRIPT, SELECT,
        SMALL, SPAN, STRIKE, STRONG, SUB,
        SUP, TEXTAREA, TT, U, VAR
      }
    ,
      new AttributeList[] {
        attr(sID, null, null, ID, IMPLIED),
        attr(CLASS, null, null, 0, IMPLIED),
        attr(STYLE, null, null, 0, IMPLIED),
        attr(TITLE, null, null, 0, IMPLIED),
        attr(LANG, null, null, 0, IMPLIED),
        attr(DIR, null,  new String[] { LTR, RTL }, 0, IMPLIED),
        attr(ONCLICK, null, null, 0, IMPLIED),
        attr(ONDBLCLICK, null, null, 0, IMPLIED),
        attr(ONMOUSEDOWN, null, null, 0, IMPLIED),
        attr(ONMOUSEUP, null, null, 0, IMPLIED),
        attr(ONMOUSEOVER, null, null, 0, IMPLIED),
        attr(ONMOUSEMOVE, null, null, 0, IMPLIED),
        attr(ONMOUSEOUT, null, null, 0, IMPLIED),
        attr(ONKEYPRESS, null, null, 0, IMPLIED),
        attr(ONKEYDOWN, null, null, 0, IMPLIED),
        attr(ONKEYUP, null, null, 0, IMPLIED)
      }
    );
      defElement(STRONG, 0, false, false, null,
      NONE
      ,
      new String[] {
        PCDATA, A, ABBR, ACRONYM,
        APPLET, B, BASEFONT, BDO, BIG,
        BR, BUTTON, CITE, CODE, DFN,
        EM, FONT, I, IFRAME, IMG,
        INPUT, KBD, LABEL, MAP, OBJECT,
        Q, S, SAMP, SCRIPT, SELECT,
        SMALL, SPAN, STRIKE, STRONG, SUB,
        SUP, TEXTAREA, TT, U, VAR
      }
    ,
      new AttributeList[] {
        attr(sID, null, null, ID, IMPLIED),
        attr(CLASS, null, null, 0, IMPLIED),
        attr(STYLE, null, null, 0, IMPLIED),
        attr(TITLE, null, null, 0, IMPLIED),
        attr(LANG, null, null, 0, IMPLIED),
        attr(DIR, null,  new String[] { LTR, RTL }, 0, IMPLIED),
        attr(ONCLICK, null, null, 0, IMPLIED),
        attr(ONDBLCLICK, null, null, 0, IMPLIED),
        attr(ONMOUSEDOWN, null, null, 0, IMPLIED),
        attr(ONMOUSEUP, null, null, 0, IMPLIED),
        attr(ONMOUSEOVER, null, null, 0, IMPLIED),
        attr(ONMOUSEMOVE, null, null, 0, IMPLIED),
        attr(ONMOUSEOUT, null, null, 0, IMPLIED),
        attr(ONKEYPRESS, null, null, 0, IMPLIED),
        attr(ONKEYDOWN, null, null, 0, IMPLIED),
        attr(ONKEYUP, null, null, 0, IMPLIED)
      }
    );
      defElement(STYLE, CDATA, false, false, null,
      NONE
      ,
      NONE
    ,
      new AttributeList[] {
        attr(LANG, null, null, 0, IMPLIED),
        attr(DIR, null,  new String[] { LTR, RTL }, 0, IMPLIED),
        attr(TYPE, null, null, 0, REQUIRED),
        attr(MEDIA, null, null, 0, IMPLIED),
        attr(TITLE, null, null, 0, IMPLIED)
      }
    );
      defElement(SUB, 0, false, false, null,
      NONE
      ,
      new String[] {
        PCDATA, A, ABBR, ACRONYM,
        APPLET, B, BASEFONT, BDO, BIG,
        BR, BUTTON, CITE, CODE, DFN,
        EM, FONT, I, IFRAME, IMG,
        INPUT, KBD, LABEL, MAP, OBJECT,
        Q, S, SAMP, SCRIPT, SELECT,
        SMALL, SPAN, STRIKE, STRONG, SUB,
        SUP, TEXTAREA, TT, U, VAR
      }
    ,
      new AttributeList[] {
        attr(sID, null, null, ID, IMPLIED),
        attr(CLASS, null, null, 0, IMPLIED),
        attr(STYLE, null, null, 0, IMPLIED),
        attr(TITLE, null, null, 0, IMPLIED),
        attr(LANG, null, null, 0, IMPLIED),
        attr(DIR, null,  new String[] { LTR, RTL }, 0, IMPLIED),
        attr(ONCLICK, null, null, 0, IMPLIED),
        attr(ONDBLCLICK, null, null, 0, IMPLIED),
        attr(ONMOUSEDOWN, null, null, 0, IMPLIED),
        attr(ONMOUSEUP, null, null, 0, IMPLIED),
        attr(ONMOUSEOVER, null, null, 0, IMPLIED),
        attr(ONMOUSEMOVE, null, null, 0, IMPLIED),
        attr(ONMOUSEOUT, null, null, 0, IMPLIED),
        attr(ONKEYPRESS, null, null, 0, IMPLIED),
        attr(ONKEYDOWN, null, null, 0, IMPLIED),
        attr(ONKEYUP, null, null, 0, IMPLIED)
      }
    );

  }

  /**
   * Define last sixth of elements of this DTD.
   */
  private void defineElements6()
  {
    /* Define the elements. */
      defElement(SUP, 0, false, false, null,
      NONE
      ,
      new String[] {
        PCDATA, A, ABBR, ACRONYM,
        APPLET, B, BASEFONT, BDO, BIG,
        BR, BUTTON, CITE, CODE, DFN,
        EM, FONT, I, IFRAME, IMG,
        INPUT, KBD, LABEL, MAP, OBJECT,
        Q, S, SAMP, SCRIPT, SELECT,
        SMALL, SPAN, STRIKE, STRONG, SUB,
        SUP, TEXTAREA, TT, U, VAR
      }
    ,
      new AttributeList[] {
        attr(sID, null, null, ID, IMPLIED),
        attr(CLASS, null, null, 0, IMPLIED),
        attr(STYLE, null, null, 0, IMPLIED),
        attr(TITLE, null, null, 0, IMPLIED),
        attr(LANG, null, null, 0, IMPLIED),
        attr(DIR, null,  new String[] { LTR, RTL }, 0, IMPLIED),
        attr(ONCLICK, null, null, 0, IMPLIED),
        attr(ONDBLCLICK, null, null, 0, IMPLIED),
        attr(ONMOUSEDOWN, null, null, 0, IMPLIED),
        attr(ONMOUSEUP, null, null, 0, IMPLIED),
        attr(ONMOUSEOVER, null, null, 0, IMPLIED),
        attr(ONMOUSEMOVE, null, null, 0, IMPLIED),
        attr(ONMOUSEOUT, null, null, 0, IMPLIED),
        attr(ONKEYPRESS, null, null, 0, IMPLIED),
        attr(ONKEYDOWN, null, null, 0, IMPLIED),
        attr(ONKEYUP, null, null, 0, IMPLIED)
      }
    );
      defElement(TABLE, 0, false, false, createTableContentModel(),
      NONE
      ,
      new String[] {
        CAPTION, COL, COLGROUP, TBODY,
        TFOOT, THEAD
      }
    ,
      new AttributeList[] {
        attr(sID, null, null, ID, IMPLIED),
        attr(CLASS, null, null, 0, IMPLIED),
        attr(STYLE, null, null, 0, IMPLIED),
        attr(TITLE, null, null, 0, IMPLIED),
        attr(LANG, null, null, 0, IMPLIED),
        attr(DIR, null,  new String[] { LTR, RTL }, 0, IMPLIED),
        attr(ONCLICK, null, null, 0, IMPLIED),
        attr(ONDBLCLICK, null, null, 0, IMPLIED),
        attr(ONMOUSEDOWN, null, null, 0, IMPLIED),
        attr(ONMOUSEUP, null, null, 0, IMPLIED),
        attr(ONMOUSEOVER, null, null, 0, IMPLIED),
        attr(ONMOUSEMOVE, null, null, 0, IMPLIED),
        attr(ONMOUSEOUT, null, null, 0, IMPLIED),
        attr(ONKEYPRESS, null, null, 0, IMPLIED),
        attr(ONKEYDOWN, null, null, 0, IMPLIED),
        attr(ONKEYUP, null, null, 0, IMPLIED),
        attr(SUMMARY, null, null, 0, IMPLIED),
        attr(WIDTH, null, null, 0, IMPLIED),
        attr(BORDER, null, null, PIXELS, IMPLIED),
        attr(FRAME, null,  new String[] { VOID, ABOVE, BELOW, HSIDES, LHS, RHS,
         VSIDES, BOX, BORDER }, 0, IMPLIED),
        attr(RULES, null,  new String[] { NONES, GROUPS, ROWS, COLS, ALL },
         0, IMPLIED),
        attr(CELLSPACING, null, null, 0, IMPLIED),
        attr(CELLPADDING, null, null, 0, IMPLIED),
        attr(ALIGN, null,  new String[] { LEFT, CENTER, RIGHT }, 0, IMPLIED),
        attr(BGCOLOR, null, null, 0, IMPLIED),
        attr(DATAPAGESIZE, null, null, 0, IMPLIED)
      }
    );
      defElement(TBODY, 0, true, true, model(TR,'+'),
      NONE
      ,
      new String[] {
        TR
      }
    ,
      new AttributeList[] {
        attr(sID, null, null, ID, IMPLIED),
        attr(CLASS, null, null, 0, IMPLIED),
        attr(STYLE, null, null, 0, IMPLIED),
        attr(TITLE, null, null, 0, IMPLIED),
        attr(LANG, null, null, 0, IMPLIED),
        attr(DIR, null,  new String[] { LTR, RTL }, 0, IMPLIED),
        attr(ONCLICK, null, null, 0, IMPLIED),
        attr(ONDBLCLICK, null, null, 0, IMPLIED),
        attr(ONMOUSEDOWN, null, null, 0, IMPLIED),
        attr(ONMOUSEUP, null, null, 0, IMPLIED),
        attr(ONMOUSEOVER, null, null, 0, IMPLIED),
        attr(ONMOUSEMOVE, null, null, 0, IMPLIED),
        attr(ONMOUSEOUT, null, null, 0, IMPLIED),
        attr(ONKEYPRESS, null, null, 0, IMPLIED),
        attr(ONKEYDOWN, null, null, 0, IMPLIED),
        attr(ONKEYUP, null, null, 0, IMPLIED),
        attr(ALIGN, null,  new String[] { LEFT, CENTER, RIGHT, JUSTIFY, CHAR },
          0, IMPLIED),
        attr(CHAR, null, null, 0, IMPLIED),
        attr(CHAROFF, null, null, 0, IMPLIED),
        attr(VALIGN, null,  new String[] { TOP, MIDDLE, BOTTOM, BASELINE },
          0, IMPLIED)
      }
    );

      defElement(TD, 0, false, true,
       new ContentModel(0,
        new noTagModel(new String[] {"TD", "TH", "TR" } ), null),
      NONE
      ,
      new String[] {
        PCDATA, A, ABBR, ACRONYM,
        APPLET, B, BASEFONT, BDO, BIG,
        BR, BUTTON, CITE, CODE, DFN,
        EM, FONT, I, IFRAME, IMG,
        INPUT, KBD, LABEL, MAP, OBJECT,
        Q, S, SAMP, SCRIPT, SELECT,
        SMALL, SPAN, STRIKE, STRONG, SUB,
        SUP, TEXTAREA, TT, U, VAR,
        ADDRESS, BLOCKQUOTE, CENTER, DIR, DIV,
        DL, FIELDSET, FORM, H1, H2,
        H3, H4, H5, H6, HR,
        ISINDEX, MENU, NOFRAMES, NOSCRIPT, OL,
        P, PRE, TABLE, UL
      }
    ,
      new AttributeList[] {
        attr(sID, null, null, ID, IMPLIED),
        attr(CLASS, null, null, 0, IMPLIED),
        attr(STYLE, null, null, 0, IMPLIED),
        attr(TITLE, null, null, 0, IMPLIED),
        attr(LANG, null, null, 0, IMPLIED),
        attr(DIR, null,  new String[] { LTR, RTL }, 0, IMPLIED),
        attr(ONCLICK, null, null, 0, IMPLIED),
        attr(ONDBLCLICK, null, null, 0, IMPLIED),
        attr(ONMOUSEDOWN, null, null, 0, IMPLIED),
        attr(ONMOUSEUP, null, null, 0, IMPLIED),
        attr(ONMOUSEOVER, null, null, 0, IMPLIED),
        attr(ONMOUSEMOVE, null, null, 0, IMPLIED),
        attr(ONMOUSEOUT, null, null, 0, IMPLIED),
        attr(ONKEYPRESS, null, null, 0, IMPLIED),
        attr(ONKEYDOWN, null, null, 0, IMPLIED),
        attr(ONKEYUP, null, null, 0, IMPLIED),
        attr(ABBR, null, null, 0, IMPLIED),
        attr(AXIS, null, null, 0, IMPLIED),
        attr(HEADERS, null, null, 0, IMPLIED),
        attr(SCOPE, null,  new String[] { ROW, COL, ROWGROUP, COLGROUP },
          0, IMPLIED),
        attr(ROWSPAN, C_1, null, NUMBER, DEFAULT),
        attr(COLSPAN, C_1, null, NUMBER, DEFAULT),
        attr(ALIGN, null,  new String[] { LEFT, CENTER, RIGHT, JUSTIFY, CHAR },
          0, IMPLIED),
        attr(CHAR, null, null, 0, IMPLIED),
        attr(CHAROFF, null, null, 0, IMPLIED),
        attr(VALIGN, null,  new String[] { TOP, MIDDLE, BOTTOM, BASELINE },
          0, IMPLIED),
        attr(NOWRAP, null,  new String[] { NOWRAP }, 0, IMPLIED),
        attr(BGCOLOR, null, null, 0, IMPLIED),
        attr(WIDTH, null, null, 0, IMPLIED),
        attr(HEIGHT, null, null, 0, IMPLIED)
      }
    );
      defElement(TEXTAREA, 0, false, false, null,
      NONE
      ,
      new String[] {
        PCDATA
      }
    ,
      new AttributeList[] {
        attr(sID, null, null, ID, IMPLIED),
        attr(CLASS, null, null, 0, IMPLIED),
        attr(STYLE, null, null, 0, IMPLIED),
        attr(TITLE, null, null, 0, IMPLIED),
        attr(LANG, null, null, 0, IMPLIED),
        attr(DIR, null,  new String[] { LTR, RTL }, 0, IMPLIED),
        attr(ONCLICK, null, null, 0, IMPLIED),
        attr(ONDBLCLICK, null, null, 0, IMPLIED),
        attr(ONMOUSEDOWN, null, null, 0, IMPLIED),
        attr(ONMOUSEUP, null, null, 0, IMPLIED),
        attr(ONMOUSEOVER, null, null, 0, IMPLIED),
        attr(ONMOUSEMOVE, null, null, 0, IMPLIED),
        attr(ONMOUSEOUT, null, null, 0, IMPLIED),
        attr(ONKEYPRESS, null, null, 0, IMPLIED),
        attr(ONKEYDOWN, null, null, 0, IMPLIED),
        attr(ONKEYUP, null, null, 0, IMPLIED),
        attr(sNAME, null, null, 0, IMPLIED),
        attr(ROWS, null, null, NUMBER, REQUIRED),
        attr(COLS, null, null, NUMBER, REQUIRED),
        attr(DISABLED, null,  new String[] { DISABLED }, 0, IMPLIED),
        attr(READONLY, null,  new String[] { READONLY }, 0, IMPLIED),
        attr(TABINDEX, null, null, NUMBER, IMPLIED),
        attr(ACCESSKEY, null, null, 0, IMPLIED),
        attr(ONFOCUS, null, null, 0, IMPLIED),
        attr(ONBLUR, null, null, 0, IMPLIED),
        attr(ONSELECT, null, null, 0, IMPLIED),
        attr(ONCHANGE, null, null, 0, IMPLIED)
      }
    );
      defElement(TFOOT, 0, false, true, model(TR,'+'),
      NONE
      ,
      new String[] {
        TR
      }
    ,
      new AttributeList[] {
        attr(sID, null, null, ID, IMPLIED),
        attr(CLASS, null, null, 0, IMPLIED),
        attr(STYLE, null, null, 0, IMPLIED),
        attr(TITLE, null, null, 0, IMPLIED),
        attr(LANG, null, null, 0, IMPLIED),
        attr(DIR, null,  new String[] { LTR, RTL }, 0, IMPLIED),
        attr(ONCLICK, null, null, 0, IMPLIED),
        attr(ONDBLCLICK, null, null, 0, IMPLIED),
        attr(ONMOUSEDOWN, null, null, 0, IMPLIED),
        attr(ONMOUSEUP, null, null, 0, IMPLIED),
        attr(ONMOUSEOVER, null, null, 0, IMPLIED),
        attr(ONMOUSEMOVE, null, null, 0, IMPLIED),
        attr(ONMOUSEOUT, null, null, 0, IMPLIED),
        attr(ONKEYPRESS, null, null, 0, IMPLIED),
        attr(ONKEYDOWN, null, null, 0, IMPLIED),
        attr(ONKEYUP, null, null, 0, IMPLIED),
        attr(ALIGN, null,  new String[] { LEFT, CENTER, RIGHT, JUSTIFY, CHAR },
          0, IMPLIED),
        attr(CHAR, null, null, 0, IMPLIED),
        attr(CHAROFF, null, null, 0, IMPLIED),
        attr(VALIGN, null,  new String[] { TOP, MIDDLE, BOTTOM, BASELINE },
         0, IMPLIED)
      }
    );
      defElement(TH, 0, false, true, null,
      NONE
      ,
      new String[] {
        PCDATA, A, ABBR, ACRONYM,
        APPLET, B, BASEFONT, BDO, BIG,
        BR, BUTTON, CITE, CODE, DFN,
        EM, FONT, I, IFRAME, IMG,
        INPUT, KBD, LABEL, MAP, OBJECT,
        Q, S, SAMP, SCRIPT, SELECT,
        SMALL, SPAN, STRIKE, STRONG, SUB,
        SUP, TEXTAREA, TT, U, VAR,
        ADDRESS, BLOCKQUOTE, CENTER, DIR, DIV,
        DL, FIELDSET, FORM, H1, H2,
        H3, H4, H5, H6, HR,
        ISINDEX, MENU, NOFRAMES, NOSCRIPT, OL,
        P, PRE, TABLE, UL
      }
    ,
      new AttributeList[] {
        attr(sID, null, null, ID, IMPLIED),
        attr(CLASS, null, null, 0, IMPLIED),
        attr(STYLE, null, null, 0, IMPLIED),
        attr(TITLE, null, null, 0, IMPLIED),
        attr(LANG, null, null, 0, IMPLIED),
        attr(DIR, null,  new String[] { LTR, RTL }, 0, IMPLIED),
        attr(ONCLICK, null, null, 0, IMPLIED),
        attr(ONDBLCLICK, null, null, 0, IMPLIED),
        attr(ONMOUSEDOWN, null, null, 0, IMPLIED),
        attr(ONMOUSEUP, null, null, 0, IMPLIED),
        attr(ONMOUSEOVER, null, null, 0, IMPLIED),
        attr(ONMOUSEMOVE, null, null, 0, IMPLIED),
        attr(ONMOUSEOUT, null, null, 0, IMPLIED),
        attr(ONKEYPRESS, null, null, 0, IMPLIED),
        attr(ONKEYDOWN, null, null, 0, IMPLIED),
        attr(ONKEYUP, null, null, 0, IMPLIED),
        attr(ABBR, null, null, 0, IMPLIED),
        attr(AXIS, null, null, 0, IMPLIED),
        attr(HEADERS, null, null, 0, IMPLIED),
        attr(SCOPE, null,  new String[] { ROW, COL, ROWGROUP, COLGROUP },
          0, IMPLIED),
        attr(ROWSPAN, C_1, null, NUMBER, DEFAULT),
        attr(COLSPAN, C_1, null, NUMBER, DEFAULT),
        attr(ALIGN, null,  new String[] { LEFT, CENTER, RIGHT, JUSTIFY, CHAR },
          0, IMPLIED),
        attr(CHAR, null, null, 0, IMPLIED),
        attr(CHAROFF, null, null, 0, IMPLIED),
        attr(VALIGN, null,  new String[] { TOP, MIDDLE, BOTTOM, BASELINE },
          0, IMPLIED),
        attr(NOWRAP, null,  new String[] { NOWRAP }, 0, IMPLIED),
        attr(BGCOLOR, null, null, 0, IMPLIED),
        attr(WIDTH, null, null, 0, IMPLIED),
        attr(HEIGHT, null, null, 0, IMPLIED)
      }
    );
      defElement(THEAD, 0, false, true, model(TR,'+'),
      NONE
      ,
      new String[] {
        TR
      }
    ,
      new AttributeList[] {
        attr(sID, null, null, ID, IMPLIED),
        attr(CLASS, null, null, 0, IMPLIED),
        attr(STYLE, null, null, 0, IMPLIED),
        attr(TITLE, null, null, 0, IMPLIED),
        attr(LANG, null, null, 0, IMPLIED),
        attr(DIR, null,  new String[] { LTR, RTL }, 0, IMPLIED),
        attr(ONCLICK, null, null, 0, IMPLIED),
        attr(ONDBLCLICK, null, null, 0, IMPLIED),
        attr(ONMOUSEDOWN, null, null, 0, IMPLIED),
        attr(ONMOUSEUP, null, null, 0, IMPLIED),
        attr(ONMOUSEOVER, null, null, 0, IMPLIED),
        attr(ONMOUSEMOVE, null, null, 0, IMPLIED),
        attr(ONMOUSEOUT, null, null, 0, IMPLIED),
        attr(ONKEYPRESS, null, null, 0, IMPLIED),
        attr(ONKEYDOWN, null, null, 0, IMPLIED),
        attr(ONKEYUP, null, null, 0, IMPLIED),
        attr(ALIGN, null,  new String[] { LEFT, CENTER, RIGHT, JUSTIFY, CHAR },
          0, IMPLIED),
        attr(CHAR, null, null, 0, IMPLIED),
        attr(CHAROFF, null, null, 0, IMPLIED),
        attr(VALIGN, null,  new String[] { TOP, MIDDLE, BOTTOM, BASELINE },
          0, IMPLIED)
      }
    );
      defElement(TITLE, 0, false, false, null,
      new String[] {
        OBJECT, SCRIPT, LINK, META,
        STYLE
      }
      ,
      new String[] {
        PCDATA
      }
    ,
      new AttributeList[] {
        attr(LANG, null, null, 0, IMPLIED),
        attr(DIR, null,  new String[] { LTR, RTL }, 0, IMPLIED)
      }
    );
      defElement(TR, 0, false, true,
       new ContentModel(0, new TableRowContentModel(this), null),
      NONE
      ,
      new String[] {
        TD, TH
      }
    ,
      new AttributeList[] {
        attr(sID, null, null, ID, IMPLIED),
        attr(CLASS, null, null, 0, IMPLIED),
        attr(STYLE, null, null, 0, IMPLIED),
        attr(TITLE, null, null, 0, IMPLIED),
        attr(LANG, null, null, 0, IMPLIED),
        attr(DIR, null,  new String[] { LTR, RTL }, 0, IMPLIED),
        attr(ONCLICK, null, null, 0, IMPLIED),
        attr(ONDBLCLICK, null, null, 0, IMPLIED),
        attr(ONMOUSEDOWN, null, null, 0, IMPLIED),
        attr(ONMOUSEUP, null, null, 0, IMPLIED),
        attr(ONMOUSEOVER, null, null, 0, IMPLIED),
        attr(ONMOUSEMOVE, null, null, 0, IMPLIED),
        attr(ONMOUSEOUT, null, null, 0, IMPLIED),
        attr(ONKEYPRESS, null, null, 0, IMPLIED),
        attr(ONKEYDOWN, null, null, 0, IMPLIED),
        attr(ONKEYUP, null, null, 0, IMPLIED),
        attr(ALIGN, null,  new String[] { LEFT, CENTER, RIGHT, JUSTIFY, CHAR },
          0, IMPLIED),
        attr(CHAR, null, null, 0, IMPLIED),
        attr(CHAROFF, null, null, 0, IMPLIED),
        attr(VALIGN, null,  new String[] { TOP, MIDDLE, BOTTOM, BASELINE },
          0, IMPLIED),
        attr(BGCOLOR, null, null, 0, IMPLIED)
      }
    );
      defElement(TT, 0, false, false, null,
      NONE
      ,
      new String[] {
        PCDATA, A, ABBR, ACRONYM,
        APPLET, B, BASEFONT, BDO, BIG,
        BR, BUTTON, CITE, CODE, DFN,
        EM, FONT, I, IFRAME, IMG,
        INPUT, KBD, LABEL, MAP, OBJECT,
        Q, S, SAMP, SCRIPT, SELECT,
        SMALL, SPAN, STRIKE, STRONG, SUB,
        SUP, TEXTAREA, TT, U, VAR
      }
    ,
      new AttributeList[] {
        attr(sID, null, null, ID, IMPLIED),
        attr(CLASS, null, null, 0, IMPLIED),
        attr(STYLE, null, null, 0, IMPLIED),
        attr(TITLE, null, null, 0, IMPLIED),
        attr(LANG, null, null, 0, IMPLIED),
        attr(DIR, null,  new String[] { LTR, RTL }, 0, IMPLIED),
        attr(ONCLICK, null, null, 0, IMPLIED),
        attr(ONDBLCLICK, null, null, 0, IMPLIED),
        attr(ONMOUSEDOWN, null, null, 0, IMPLIED),
        attr(ONMOUSEUP, null, null, 0, IMPLIED),
        attr(ONMOUSEOVER, null, null, 0, IMPLIED),
        attr(ONMOUSEMOVE, null, null, 0, IMPLIED),
        attr(ONMOUSEOUT, null, null, 0, IMPLIED),
        attr(ONKEYPRESS, null, null, 0, IMPLIED),
        attr(ONKEYDOWN, null, null, 0, IMPLIED),
        attr(ONKEYUP, null, null, 0, IMPLIED)
      }
    );
      defElement(U, 0, false, false, null,
      NONE
      ,
      new String[] {
        PCDATA, A, ABBR, ACRONYM,
        APPLET, B, BASEFONT, BDO, BIG,
        BR, BUTTON, CITE, CODE, DFN,
        EM, FONT, I, IFRAME, IMG,
        INPUT, KBD, LABEL, MAP, OBJECT,
        Q, S, SAMP, SCRIPT, SELECT,
        SMALL, SPAN, STRIKE, STRONG, SUB,
        SUP, TEXTAREA, TT, U, VAR
      }
    ,
      new AttributeList[] {
        attr(sID, null, null, ID, IMPLIED),
        attr(CLASS, null, null, 0, IMPLIED),
        attr(STYLE, null, null, 0, IMPLIED),
        attr(TITLE, null, null, 0, IMPLIED),
        attr(LANG, null, null, 0, IMPLIED),
        attr(DIR, null,  new String[] { LTR, RTL }, 0, IMPLIED),
        attr(ONCLICK, null, null, 0, IMPLIED),
        attr(ONDBLCLICK, null, null, 0, IMPLIED),
        attr(ONMOUSEDOWN, null, null, 0, IMPLIED),
        attr(ONMOUSEUP, null, null, 0, IMPLIED),
        attr(ONMOUSEOVER, null, null, 0, IMPLIED),
        attr(ONMOUSEMOVE, null, null, 0, IMPLIED),
        attr(ONMOUSEOUT, null, null, 0, IMPLIED),
        attr(ONKEYPRESS, null, null, 0, IMPLIED),
        attr(ONKEYDOWN, null, null, 0, IMPLIED),
        attr(ONKEYUP, null, null, 0, IMPLIED)
      }
    );
      defElement(UL, 0, false, false, createListModel(),
      NONE
      ,
      new String[] {
        // See note on the createListModel method
        LI, UL, OL
      }
    ,
      new AttributeList[] {
        attr(sID, null, null, ID, IMPLIED),
        attr(CLASS, null, null, 0, IMPLIED),
        attr(STYLE, null, null, 0, IMPLIED),
        attr(TITLE, null, null, 0, IMPLIED),
        attr(LANG, null, null, 0, IMPLIED),
        attr(DIR, null,  new String[] { LTR, RTL }, 0, IMPLIED),
        attr(ONCLICK, null, null, 0, IMPLIED),
        attr(ONDBLCLICK, null, null, 0, IMPLIED),
        attr(ONMOUSEDOWN, null, null, 0, IMPLIED),
        attr(ONMOUSEUP, null, null, 0, IMPLIED),
        attr(ONMOUSEOVER, null, null, 0, IMPLIED),
        attr(ONMOUSEMOVE, null, null, 0, IMPLIED),
        attr(ONMOUSEOUT, null, null, 0, IMPLIED),
        attr(ONKEYPRESS, null, null, 0, IMPLIED),
        attr(ONKEYDOWN, null, null, 0, IMPLIED),
        attr(ONKEYUP, null, null, 0, IMPLIED),
        attr(TYPE, null,  new String[] { DISC, SQUARE, CIRCLE }, 0, IMPLIED),
        attr(COMPACT, null,  new String[] { COMPACT }, 0, IMPLIED)
      }
    );
      defElement(VAR, 0, false, false, null,
      NONE
      ,
      new String[] {
        PCDATA, A, ABBR, ACRONYM,
        APPLET, B, BASEFONT, BDO, BIG,
        BR, BUTTON, CITE, CODE, DFN,
        EM, FONT, I, IFRAME, IMG,
        INPUT, KBD, LABEL, MAP, OBJECT,
        Q, S, SAMP, SCRIPT, SELECT,
        SMALL, SPAN, STRIKE, STRONG, SUB,
        SUP, TEXTAREA, TT, U, VAR
      }
    ,
      new AttributeList[] {
        attr(sID, null, null, ID, IMPLIED),
        attr(CLASS, null, null, 0, IMPLIED),
        attr(STYLE, null, null, 0, IMPLIED),
        attr(TITLE, null, null, 0, IMPLIED),
        attr(LANG, null, null, 0, IMPLIED),
        attr(DIR, null,  new String[] { LTR, RTL }, 0, IMPLIED),
        attr(ONCLICK, null, null, 0, IMPLIED),
        attr(ONDBLCLICK, null, null, 0, IMPLIED),
        attr(ONMOUSEDOWN, null, null, 0, IMPLIED),
        attr(ONMOUSEUP, null, null, 0, IMPLIED),
        attr(ONMOUSEOVER, null, null, 0, IMPLIED),
        attr(ONMOUSEMOVE, null, null, 0, IMPLIED),
        attr(ONMOUSEOUT, null, null, 0, IMPLIED),
        attr(ONKEYPRESS, null, null, 0, IMPLIED),
        attr(ONKEYDOWN, null, null, 0, IMPLIED),
        attr(ONKEYUP, null, null, 0, IMPLIED)
      }
    );

  }

  /**
   * Define all entities in this DTD.
   */
  protected void defineEntities()
  {
    /* Define general entities */
    defineEntity("AElig", 198);
    defineEntity("Aacute", 193);
    defineEntity("Acirc", 194);
    defineEntity("Agrave", 192);
    defineEntity("Alpha", 913);
    defineEntity("Aring", 197);
    defineEntity("Atilde", 195);
    defineEntity("Auml", 196);
    defineEntity("Beta", 914);
    defineEntity("Ccedil", 199);
    defineEntity("Chi", 935);
    defineEntity("Dagger", 8225);
    defineEntity("Delta", 916);
    defineEntity("ETH", 208);
    defineEntity("Eacute", 201);
    defineEntity("Ecirc", 202);
    defineEntity("Egrave", 200);
    defineEntity("Epsilon", 917);
    defineEntity("Eta", 919);
    defineEntity("Euml", 203);
    defineEntity("Gamma", 915);
    defineEntity("Iacute", 205);
    defineEntity("Icirc", 206);
    defineEntity("Igrave", 204);
    defineEntity("Iota", 921);
    defineEntity("Iuml", 207);
    defineEntity("Kappa", 922);
    defineEntity("Lambda", 923);
    defineEntity("Mu", 924);
    defineEntity("Ntilde", 209);
    defineEntity("Nu", 925);
    defineEntity("OElig", 338);
    defineEntity("Oacute", 211);
    defineEntity("Ocirc", 212);
    defineEntity("Ograve", 210);
    defineEntity("Omega", 937);
    defineEntity("Omicron", 927);
    defineEntity("Oslash", 216);
    defineEntity("Otilde", 213);
    defineEntity("Ouml", 214);
    defineEntity("Phi", 934);
    defineEntity("Pi", 928);
    defineEntity("Prime", 8243);
    defineEntity("Psi", 936);
    defineEntity("Rho", 929);
    defineEntity("Scaron", 352);
    defineEntity("Sigma", 931);
    defineEntity("THORN", 222);
    defineEntity("Tau", 932);
    defineEntity("Theta", 920);
    defineEntity("Uacute", 218);
    defineEntity("Ucirc", 219);
    defineEntity("Ugrave", 217);
    defineEntity("Upsilon", 933);
    defineEntity("Uuml", 220);
    defineEntity("Xi", 926);
    defineEntity("Yacute", 221);
    defineEntity("Yuml", 376);
    defineEntity("Zeta", 918);
    defineEntity("aacute", 225);
    defineEntity("acirc", 226);
    defineEntity("acute", 180);
    defineEntity("aelig", 230);
    defineEntity("agrave", 224);
    defineEntity("alefsym", 8501);
    defineEntity("alpha", 945);
    defineEntity("amp", 38);
    defineEntity("and", 8743);
    defineEntity("ang", 8736);
    defineEntity("aring", 229);
    defineEntity("asymp", 8776);
    defineEntity("atilde", 227);
    defineEntity("auml", 228);
    defineEntity("bdquo", 8222);
    defineEntity("beta", 946);
    defineEntity("brvbar", 166);
    defineEntity("bull", 8226);
    defineEntity("cap", 8745);
    defineEntity("ccedil", 231);
    defineEntity("cedil", 184);
    defineEntity("cent", 162);
    defineEntity("chi", 967);
    defineEntity("circ", 710);
    defineEntity("clubs", 9827);
    defineEntity("cong", 8773);
    defineEntity("copy", 169);
    defineEntity("crarr", 8629);
    defineEntity("cup", 8746);
    defineEntity("curren", 164);
    defineEntity("dArr", 8659);
    defineEntity("dagger", 8224);
    defineEntity("darr", 8595);
    defineEntity("deg", 176);
    defineEntity("delta", 948);
    defineEntity("diams", 9830);
    defineEntity("divide", 247);
    defineEntity("eacute", 233);
    defineEntity("ecirc", 234);
    defineEntity("egrave", 232);
    defineEntity("empty", 8709);
    defineEntity("emsp", 8195);
    defineEntity("ensp", 8194);
    defineEntity("epsilon", 949);
    defineEntity("equiv", 8801);
    defineEntity("eta", 951);
    defineEntity("eth", 240);
    defineEntity("euml", 235);
    defineEntity("euro", 8364);
    defineEntity("exist", 8707);
    defineEntity("fnof", 402);
    defineEntity("forall", 8704);
    defineEntity("frac12", 189);
    defineEntity("frac14", 188);
    defineEntity("frac34", 190);
    defineEntity("frasl", 8260);
    defineEntity("gamma", 947);
    defineEntity("ge", 8805);
    defineEntity("gt", 62);
    defineEntity("hArr", 8660);
    defineEntity("harr", 8596);
    defineEntity("hearts", 9829);
    defineEntity("hellip", 8230);
    defineEntity("iacute", 237);
    defineEntity("icirc", 238);
    defineEntity("iexcl", 161);
    defineEntity("igrave", 236);
    defineEntity("image", 8465);
    defineEntity("infin", 8734);
    defineEntity("int", 8747);
    defineEntity("iota", 953);
    defineEntity("iquest", 191);
    defineEntity("isin", 8712);
    defineEntity("iuml", 239);
    defineEntity("kappa", 954);
    defineEntity("lArr", 8656);
    defineEntity("lambda", 955);
    defineEntity("lang", 9001);
    defineEntity("laquo", 171);
    defineEntity("larr", 8592);
    defineEntity("lceil", 8968);
    defineEntity("ldquo", 8220);
    defineEntity("le", 8804);
    defineEntity("lfloor", 8970);
    defineEntity("lowast", 8727);
    defineEntity("loz", 9674);
    defineEntity("lrm", 8206);
    defineEntity("lsaquo", 8249);
    defineEntity("lsquo", 8216);
    defineEntity("lt", 60);
    defineEntity("macr", 175);
    defineEntity("mdash", 8212);
    defineEntity("micro", 181);
    defineEntity("middot", 183);
    defineEntity("minus", 8722);
    defineEntity("mu", 956);
    defineEntity("nabla", 8711);
    defineEntity("nbsp", 160);
    defineEntity("ndash", 8211);
    defineEntity("ne", 8800);
    defineEntity("ni", 8715);
    defineEntity("not", 172);
    defineEntity("notin", 8713);
    defineEntity("nsub", 8836);
    defineEntity("ntilde", 241);
    defineEntity("nu", 957);
    defineEntity("oacute", 243);
    defineEntity("ocirc", 244);
    defineEntity("oelig", 339);
    defineEntity("ograve", 242);
    defineEntity("oline", 8254);
    defineEntity("omega", 969);
    defineEntity("omicron", 959);
    defineEntity("oplus", 8853);
    defineEntity("or", 8744);
    defineEntity("ordf", 170);
    defineEntity("ordm", 186);
    defineEntity("oslash", 248);
    defineEntity("otilde", 245);
    defineEntity("otimes", 8855);
    defineEntity("ouml", 246);
    defineEntity("para", 182);
    defineEntity("part", 8706);
    defineEntity("permil", 8240);
    defineEntity("perp", 8869);
    defineEntity("phi", 966);
    defineEntity("pi", 960);
    defineEntity("piv", 982);
    defineEntity("plusmn", 177);
    defineEntity("pound", 163);
    defineEntity("prime", 8242);
    defineEntity("prod", 8719);
    defineEntity("prop", 8733);
    defineEntity("psi", 968);
    defineEntity("quot", 34);
    defineEntity("rArr", 8658);
    defineEntity("radic", 8730);
    defineEntity("rang", 9002);
    defineEntity("raquo", 187);
    defineEntity("rarr", 8594);
    defineEntity("rceil", 8969);
    defineEntity("rdquo", 8221);
    defineEntity("real", 8476);
    defineEntity("reg", 174);
    defineEntity("rfloor", 8971);
    defineEntity("rho", 961);
    defineEntity("rlm", 8207);
    defineEntity("rsaquo", 8250);
    defineEntity("rsquo", 8217);
    defineEntity("sbquo", 8218);
    defineEntity("scaron", 353);
    defineEntity("sdot", 8901);
    defineEntity("sect", 167);
    defineEntity("shy", 173);
    defineEntity("sigma", 963);
    defineEntity("sigmaf", 962);
    defineEntity("sim", 8764);
    defineEntity("spades", 9824);
    defineEntity("sub", 8834);
    defineEntity("sube", 8838);
    defineEntity("sum", 8721);
    defineEntity("sup", 8835);
    defineEntity("sup1", 185);
    defineEntity("sup2", 178);
    defineEntity("sup3", 179);
    defineEntity("supe", 8839);
    defineEntity("szlig", 223);
    defineEntity("tau", 964);
    defineEntity("there4", 8756);
    defineEntity("theta", 952);
    defineEntity("thetasym", 977);
    defineEntity("thinsp", 8201);
    defineEntity("thorn", 254);
    defineEntity("tilde", 732);
    defineEntity("times", 215);
    defineEntity("trade", 8482);
    defineEntity("uArr", 8657);
    defineEntity("uacute", 250);
    defineEntity("uarr", 8593);
    defineEntity("ucirc", 251);
    defineEntity("ugrave", 249);
    defineEntity("uml", 168);
    defineEntity("upsih", 978);
    defineEntity("upsilon", 965);
    defineEntity("uuml", 252);
    defineEntity("weierp", 8472);
    defineEntity("xi", 958);
    defineEntity("yacute", 253);
    defineEntity("yen", 165);
    defineEntity("yuml", 255);
    defineEntity("zeta", 950);
    defineEntity("zwj", 8205);
    defineEntity("zwnj", 8204);
  }

  /**
   * Crate a content model, consisting of the single
   * element, specified by name.
   */
  protected ContentModel model(String element)
  {
    return new ContentModel(getElement(element));
  }

  /**
   * Crate a chain from the two content models,
   * the last containing the given element and
   * the specified unary operation.
   */
  private ContentModel model(String element, int unary)
  {
    ContentModel ct = model(element);
    ct.type = unary;
    return new ContentModel(0, ct);
  }

  /**
   * Create the model HEAD, BODY
   * @return the HTML content model of the whole document
   */
  protected ContentModel createHtmlContentModel()
  {
    ContentModel head = model(HEAD);
    ContentModel body = model(BODY);
    head.next = body;
    head.type = ',';
    return head;
  }

  /**
   * Create the model
   * ( CAPTION ? , ( COL * | COLGROUP * ) , THEAD ? , TFOOT ? , TBODY + )
   */
  protected ContentModel createTableContentModel()
  {
     ContentModel col_colgroup = new ContentModel
      ('|', model(COL,'*'), model(COLGROUP,'*') );

     col_colgroup = new ContentModel('*', col_colgroup);
     col_colgroup = new ContentModel(',', col_colgroup);

     ContentModel caption = model(CAPTION,'?');
     ContentModel thead   = model(THEAD, '?');
     ContentModel tfoot   = model(TFOOT, '?');
     ContentModel tbody   = model(TBODY, '+');

     caption.next = col_colgroup;
     col_colgroup.next = thead;
     thead.next = tfoot;
     tfoot.next = tbody;

     caption.type = col_colgroup.type = thead.type = tfoot.type =
     tbody.type = ',';

     return caption;
  }

  /**
   * Creates a model for &lt;DL&gt; tag:
   * <code> DT+ | DL+ </code>.
   * @return
   */
  protected ContentModel createDefListModel()
  {
    ContentModel dt = model(DT, '+');
    ContentModel dd = model(DD, '+');

    dt.next = dd;
    dt.type = dd.type = '|';
    return dt;
  }

  /**
   * This model is used for UL, OL, MENU and DIR.
   *  HTML 4.01 specifies LI only, but the nested
   * list seems rendered correctly only if
   * it is not enclosed into <LI>-</LI> of the
   * parent list.
   */
  protected ContentModel createListModel()
  {
    ContentModel li = model(LI, '+');
    ContentModel ul = model(UL, '+');
    ContentModel ol = model(OL, '+');

    li.next = ul;
    ul.next = ol;
    li.type = ul.type = ol.type = '|';
    return li;
  }

  /**
   * Get elements that are allowed in the document body, at the zero level.
   */
  protected String[] getBodyElements()
  {
    return new String[] {
        PCDATA, A, ABBR, ACRONYM,
        APPLET, B, BASEFONT, BDO, BIG,
        BR, BUTTON, CITE, CODE, DFN,
        EM, FONT, I, IFRAME, IMG,
        INPUT, KBD, LABEL, MAP, OBJECT,
        Q, S, SAMP, SCRIPT, SELECT,
        SMALL, SPAN, STRIKE, STRONG, SUB,
        SUP, TEXTAREA, TT, U, VAR,
        ADDRESS, BLOCKQUOTE, CENTER, DEL, DIR,
        DIV, DL, FIELDSET, FORM, H1,
        H2, H3, H4, H5, H6,
        HR, INS, ISINDEX, MENU, NOFRAMES,
        NOSCRIPT, OL, P, PRE, TABLE,
        UL
      };
  }
}
