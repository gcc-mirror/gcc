/* GnuParserDelegator.java -- The parser delegator which uses Swing DTD
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

import java.io.IOException;
import java.io.Reader;
import java.io.Serializable;

import javax.swing.text.BadLocationException;
import javax.swing.text.SimpleAttributeSet;
import javax.swing.text.html.HTMLEditorKit;
import javax.swing.text.html.HTMLEditorKit.ParserCallback;
import javax.swing.text.html.parser.DTD;
import javax.swing.text.html.parser.ParserDelegator;
import javax.swing.text.html.parser.TagElement;

/**
 * This parser delegator uses the different DTD ({@link HTML_401Swing}).
 * It is derived from the ParserDelegator for the compatibility reasons.
 *
 * @author Audrius Meskauskas (AudriusA@Bioinformatics.org)
 */
public class GnuParserDelegator extends ParserDelegator implements Serializable
{
  class gnuParser
    extends gnu.javax.swing.text.html.parser.support.Parser
  {
    private static final long serialVersionUID = 1;

    gnuParser(DTD d)
    {
      super(d);
    }

    protected final void handleComment(char[] comment)
    {
      callBack.handleComment(comment, hTag.where.startPosition);
    }

    protected final void handleEmptyTag(TagElement tag)
      throws javax.swing.text.ChangedCharSetException
    {
      callBack.handleSimpleTag(tag.getHTMLTag(), getAttributes(),
                               hTag.where.startPosition
                              );
    }

    protected final void handleEndTag(TagElement tag)
    {
      callBack.handleEndTag(tag.getHTMLTag(), hTag.where.startPosition);
    }

    protected final void handleError(int line, String message)
    {
      callBack.handleError(message, hTag.where.startPosition);
    }

    protected final void handleStartTag(TagElement tag)
    {
      SimpleAttributeSet attributes = gnu.getAttributes();

      if (tag.fictional())
        attributes.addAttribute(ParserCallback.IMPLIED, Boolean.TRUE);

      callBack.handleStartTag(tag.getHTMLTag(), attributes,
                              hTag.where.startPosition
                             );
    }

    protected final void handleText(char[] text)
    {
      callBack.handleText(text, hTag.where.startPosition);
    }

    DTD getDTD()
    {
      // Accessing the inherited gnu.javax.swing.text.html.parser.support.Parser
      // field. super. is a workaround, required to support JDK1.3's javac.
      return super.dtd;
    }
  }

  /**
   * Use serialVersionUID for interoperability.
   */
  private static final long serialVersionUID = -1276686502624777206L;

  private DTD theDtd;

  /**
   * The callback.
   * This is package-private to avoid an accessor method.
   */
  HTMLEditorKit.ParserCallback callBack;

  /**
   * The reference to the working class of HTML parser that is
   * actually used to parse the document.
   * This is package-private to avoid an accessor method.
   */
  gnuParser gnu;

  /**
   * Create the parser that uses the given DTD to parse the document.
   *
   * @param theDtd the DTD
   */
  public GnuParserDelegator(DTD theDtd)
  {
    this.theDtd = theDtd;
    gnu = new gnuParser(theDtd);
  }

  /**
   * Parses the HTML document, calling methods of the provided callback. This
   * method must be multithread - safe.
   *
   * @param reader The reader to read the HTML document from
   * @param a_callback The callback that is notifyed about the presence of HTML
   *          elements in the document.
   * @param ignoreCharSet If thrue, any charset changes during parsing are
   *          ignored.
   * @throws java.io.IOException
   */
  public void parse(Reader reader,
                                 HTMLEditorKit.ParserCallback a_callback,
                                 boolean ignoreCharSet) throws IOException
  {
    callBack = a_callback;
    gnu.parse(reader);

    callBack.handleEndOfLineString(gnu.getEndOfLineSequence());
    try
      {
        callBack.flush();
      }
    catch (BadLocationException ex)
      {
        // Convert this into the supported type of exception.
        throw new IOException(ex.getMessage());
      }
  }
}
