/* HTML_401Swing.java -- The HTML 4.01 DTD, adapted for HTML rendering in Swing
   Copyright (C) 2006 Free Software Foundation, Inc.

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

import javax.swing.text.html.parser.DTD;

/**
 * This class is necessary because the current implementation of the GNU
 * Classpath Swing requires always enclose the text into paragraphs.
 * 
 * @author Audrius Meskauskas (AudriusA@Bioinformatics.org)
 */
public class HTML_401Swing extends HTML_401F
{
  /**
   * The singleton instance;
   */
  final static HTML_401Swing singleton = new HTML_401Swing();
  
  /**
   * Either takes the document (by name) from DTD table, or
   * creates a new instance and registers it in the tabe.
   * The document is registerd under name "-//W3C//DTD HTML 4.01 Frameset//EN".
   * @return The new or existing DTD for parsing HTML 4.01 Frameset.
   */
  public static DTD getInstance()
  {
    return singleton;
  }  
  
  /**
   * Get elements that are allowed in the document body, at the zero level.
   * This list disallows the text at this level (the implied P tag will be
   * generated). It also disallows A, B, I, U, CITE and other similar
   * elements that have the plain text inside. They will also be placed
   * inside the generated implied P tags.
   */
  protected String[] getBodyElements()
  {
    return new String[] {
        APPLET, BASEFONT,
        BR, BUTTON, 
        IFRAME, IMG,
        INPUT, LABEL, MAP, OBJECT,
        SCRIPT, SELECT,
        TEXTAREA, 
        BLOCKQUOTE, CENTER, DEL, DIR,
        DIV, DL, FIELDSET, FORM, H1,
        H2, H3, H4, H5, H6,
        HR, INS, ISINDEX, MENU, NOFRAMES,
        NOSCRIPT, OL, P, PRE, TABLE,
        UL
      };
  }
}
