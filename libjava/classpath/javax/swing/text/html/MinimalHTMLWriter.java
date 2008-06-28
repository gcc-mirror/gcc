/* MinimalHTMLWriter.java -- 
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

package javax.swing.text.html;

import javax.swing.text.AttributeSet;
import javax.swing.text.AbstractWriter;
import javax.swing.text.BadLocationException;
import javax.swing.text.DefaultStyledDocument;
import javax.swing.text.Element;
import javax.swing.text.ElementIterator;
import javax.swing.text.StyleConstants;
import javax.swing.text.Style;
import javax.swing.text.StyledDocument;
import java.io.Writer;
import java.io.IOException;
import java.util.Enumeration;
import java.util.Stack;
import java.awt.Color;

/**
 * MinimalHTMLWriter,
 * A minimal AbstractWriter implementation for HTML.
 *
 * @author Sven de Marothy
 */
public class MinimalHTMLWriter extends AbstractWriter 
{
  private StyledDocument doc;
  private Stack tagStack;
  private boolean inFontTag = false;

  /**
   * Constructs a MinimalHTMLWriter.
   * @param w - a Writer, for output.
   * @param doc - the document
   */
  public MinimalHTMLWriter(Writer w, StyledDocument doc)
  {
    super(w, doc);
    this.doc = doc;
    tagStack = new Stack();
  }

  /**
   * Constructs a MinimalHTMLWriter.
   * @param w - a Writer, for output.
   * @param doc - the document
   * @param pos - start position
   * @param len - length
   */
  public MinimalHTMLWriter(Writer w, StyledDocument doc, int pos, int len)
  {
    super(w, doc, pos, len);
    this.doc = doc;
    tagStack = new Stack();
  }

  /**
   * Starts a span tag.
   */
  protected void startFontTag(String style) throws IOException
  {
    if( inFontTag() )
      endOpenTags();
    writeStartTag("<span style=\""+style+"\">");
    inFontTag = true;
  }

  /**
   * Returns whether the writer is within two span tags.
   */
  protected boolean inFontTag()
  {
    return inFontTag;
  }

  /**
   * Ends a span tag.
   */
  protected void endFontTag() throws IOException
  {    
    writeEndTag("</span>");
    inFontTag = false;
  }

  /**
   * Write the entire HTML document.
   */
  public synchronized void write() throws IOException, BadLocationException
  {
    writeStartTag("<html>");
    writeHeader();
    writeBody();
    writeEndTag("</html>");
  }

  /**
   * Write a start tag and increment the indent.
   */
  protected void writeStartTag(String tag) throws IOException
  {
    indent();
    write(tag+NEWLINE);
    incrIndent();
  }

  /**
   * Write an ending tag and decrement the indent.
   */
  protected void writeEndTag(String endTag) throws IOException
  {
    decrIndent();
    indent();
    write(endTag+NEWLINE);
  }

  /**
   * Write the HTML header.
   */
  protected void writeHeader() throws IOException 
  {
    writeStartTag("<head>");
    writeStartTag("<style>");
    writeStartTag("<!--");
    writeStyles();
    writeEndTag("-->");
    writeEndTag("</style>");
    writeEndTag("</head>");
  }

  /**
   * Write a paragraph start tag.
   */
  protected void writeStartParagraph(Element elem) throws IOException
  {      
    indent();
    write("<p class=default>"+NEWLINE); // FIXME: Class value = ?
    incrIndent();
  }

  /**
   * Write a paragraph end tag, closes any other open tags.
   */
  protected void writeEndParagraph() throws IOException
  {
    endOpenTags();
    writeEndTag("</p>");
  }

  /**
   * Writes the body of the HTML document.
   */ 
  protected void writeBody() throws IOException, BadLocationException
  {
    writeStartTag("<body>");

    ElementIterator ei = getElementIterator();
    Element e = ei.first();
    boolean inParagraph = false;
    do
      {
	if( e.isLeaf() )
	  {
	    boolean hasNL = (getText(e).indexOf(NEWLINE) != -1);
	    if( !inParagraph && hasText( e ) )
	      {
		writeStartParagraph(e);
		inParagraph = true;
	      }

	    if( hasText( e ) )
	      writeContent(e, true);

	    if( hasNL && inParagraph )
	      {
		writeEndParagraph();
		inParagraph = false;
	      }
	    else
	      endOpenTags();
	  }
      } 
    while((e = ei.next()) != null);

    writeEndTag("</body>");
  }

  protected void text(Element elem) throws IOException, BadLocationException
  {
    write( getText(elem).trim() );
  }

  /**
   * Write bold, indent and underline tags.
   */
  protected void writeHTMLTags(AttributeSet attr) throws IOException
  {
    if(attr.getAttribute(StyleConstants.Bold) != null)
      if(((Boolean)attr.getAttribute(StyleConstants.Bold)).booleanValue())
	{
	  write("<b>");
	  tagStack.push("</b>");
	}
    if(attr.getAttribute(StyleConstants.Italic) != null)
      if(((Boolean)attr.getAttribute(StyleConstants.Italic)).booleanValue())
	{
	  write("<i>");
	  tagStack.push("</i>");
	}
    if(attr.getAttribute(StyleConstants.Underline) != null)
      if(((Boolean)attr.getAttribute(StyleConstants.Underline)).booleanValue())
	{
	  write("<u>");
	  tagStack.push("</u>");
	}
  }

  /**
   * Returns whether the element contains text or not.
   */
  protected boolean isText(Element elem) 
  {
    return (elem.getEndOffset() != elem.getStartOffset());
  }

  /**
   * Writes the content of an element.
   */
  protected void writeContent(Element elem, boolean needsIndenting)
    throws IOException, BadLocationException
  {
    writeNonHTMLAttributes(elem.getAttributes());
    if(needsIndenting)
      indent();
    writeHTMLTags(elem.getAttributes());
    if( isText(elem) )
      text(elem);
    else 
      writeLeaf(elem);

    endOpenTags();
  }

  /**
   * Writes a non-text leaf element.
   */
  protected void writeLeaf(Element e) throws IOException
  {
    // NOTE: Haven't tested if this is correct.
    if(e.getName().equals(StyleConstants.IconElementName))
      writeImage(e);
    else 
      writeComponent(e);
  }

  /**
   * Write the HTML attributes which do not have tag equivalents, 
   * e.g. attributes other than bold/italic/underlined.
   */
  protected void writeNonHTMLAttributes(AttributeSet attr) throws IOException
  {
    String style = "";

    // Alignment? Background?

    if( StyleConstants.getForeground(attr) != null )
      style = style + "color: " + 
	getColor(StyleConstants.getForeground(attr)) + "; ";

    style = style + "font-size: "+StyleConstants.getFontSize(attr)+"pt; ";
    style = style + "font-family: "+StyleConstants.getFontFamily(attr);

    startFontTag(style);
  }

  /**
   * Write the styles used.
   */
  protected void writeStyles() throws IOException
  {
    if(doc instanceof DefaultStyledDocument)
      {
	Enumeration styles = ((DefaultStyledDocument)doc).getStyleNames();
	while(styles.hasMoreElements())
	  writeStyle(doc.getStyle((String)styles.nextElement()));
      }
    else
      { // What else to do here?
	Style s = doc.getStyle("default");
	if(s != null)
	  writeStyle( s );
      }
  }

  /**
   * Write a set of attributes.
   */
  protected void writeAttributes(AttributeSet attr) throws IOException
  {
    Enumeration attribs = attr.getAttributeNames();
    while(attribs.hasMoreElements())
      {
	Object attribName = attribs.nextElement();
	String name = attribName.toString();
	String output = getAttribute(name, attr.getAttribute(attribName));
	if( output != null )
	  {
	    indent();
	    write( output + NEWLINE );
	  }
      }
  }

  /**
   * Deliberately unimplemented, handles component elements.
   */ 
  protected void writeComponent(Element elem) throws IOException
  {
  }

  /**
   * Deliberately unimplemented. 
   * Writes StyleConstants.IconElementName elements.
   */ 
  protected void writeImage(Element elem) throws IOException
  {
  }

  // -------------------- Private methods. --------------------------------

  /**
   * Write a single style attribute
   */
  private String getAttribute(String name, Object a) throws IOException
  {
    if(name.equals("foreground"))
      return "foreground:"+getColor((Color)a)+";";
    if(name.equals("background"))
      return "background:"+getColor((Color)a)+";";
    if(name.equals("italic"))
      return "italic:"+(((Boolean)a).booleanValue() ? "italic;" : ";");
    if(name.equals("bold"))
      return "bold:"+(((Boolean)a).booleanValue() ? "bold;" : "normal;");
    if(name.equals("family"))
      return "family:" + a + ";";
    if(name.equals("size"))
      {
	int size = ((Integer)a).intValue();
	int htmlSize;
	if( size > 24 )
	  htmlSize = 7;
	else if( size > 18 )
	  htmlSize = 6;
	else if( size > 14 )
	  htmlSize = 5;
	else if( size > 12 )
	  htmlSize = 4;
	else if( size > 10 )
	  htmlSize = 3;
	else if( size > 8 )
	  htmlSize = 2;
	else
	  htmlSize = 1;

	return "size:" + htmlSize + ";";
      }

    return null;
  }

  /**
   * Stupid that Color doesn't have a method for this.
   */
  private String getColor(Color c)
  {
    String r = "00" + Integer.toHexString(c.getRed());
    r = r.substring(r.length() - 2);
    String g = "00" + Integer.toHexString(c.getGreen());
    g = g.substring(g.length() - 2);
    String b = "00" + Integer.toHexString(c.getBlue());
    b = b.substring(b.length() - 2);
    return "#" + r + g + b;
  }

  /**
   * Empty the stack of open tags
   */
  private void endOpenTags() throws IOException
  {
    while(!tagStack.empty())
      write((String)tagStack.pop());

    if( inFontTag() )
      {
	write(""+NEWLINE);
	endFontTag();
      }
  }

  /**
   * Output a single style
   */
  private void writeStyle(Style s) throws IOException
  {
    if( s == null )
      return;

    writeStartTag("p."+s.getName()+" {");
    writeAttributes(s);
    writeEndTag("}");
  }

  private boolean hasText(Element e) throws BadLocationException
  {
    return (getText(e).trim().length() > 0);
  }
}
