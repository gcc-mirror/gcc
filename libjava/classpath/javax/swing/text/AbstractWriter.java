/* AbstractWriter.java --
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

package javax.swing.text;

import java.io.IOException;
import java.io.Writer;
import java.util.Arrays;
import java.util.Enumeration;

/**
 * This is an abstract base class for writing Document instances to a
 * Writer.  A concrete subclass must implement a method to iterate
 * over the Elements of the Document and correctly format them.
 */
public abstract class AbstractWriter
{
  /**
   * The default line separator character.
   * @specnote although this is a constant, it is not static in the JDK
   */
  protected static final char NEWLINE = '\n';

  // Where we write.
  private Writer writer;
  // How we iterate over the document.
  private ElementIterator iter;
  // The document over which we iterate.
  private Document document;
  // Maximum number of characters per line.
  private int maxLineLength = 100;
  // Number of characters we have currently written.
  private int lineLength;
  // True if we can apply line wrapping.
  private boolean canWrapLines;	// FIXME default?
  // The number of spaces per indentation level.
  private int indentSpace = 2;
  // The current indentation level.
  private int indentLevel;
  // True if we have indented this line.
  private boolean indented;
  // Starting offset in document.
  private int startOffset;
  // Ending offset in document.
  private int endOffset;
  // The line separator string.
  private String lineSeparator = "" + NEWLINE;
  // The characters making up the line separator.
  private char[] lineSeparatorChars = lineSeparator.toCharArray();

  /**
   * Create a new AbstractWriter with the indicated Writer and
   * Document.  The full range of the Document will be used.  The
   * internal ElementIterator will be initialized with the Document's
   * root node.
   */
  protected AbstractWriter(Writer writer, Document doc)
  {
    this.writer = writer;
    this.iter = new ElementIterator(doc);
    this.document = doc;
    this.startOffset = 0;
    this.endOffset = doc.getLength();
  }

  /**
   * Create a new AbstractWriter with the indicated Writer and
   * Document.  The full range of the Document will be used.  The
   * internal ElementIterator will be initialized with the Document's
   * root node.
   */
  protected AbstractWriter(Writer writer, Document doc, int pos, int len)
  {
    this.writer = writer;
    this.iter = new ElementIterator(doc);
    this.document = doc;
    this.startOffset = pos;
    this.endOffset = pos + len;
  }

  /**
   * Create a new AbstractWriter with the indicated Writer and
   * Element.  The full range of the Element will be used.
   */
  protected AbstractWriter(Writer writer, Element elt)
  {
    this.writer = writer;
    this.iter = new ElementIterator(elt);
    this.document = elt.getDocument();
    this.startOffset = elt.getStartOffset();
    this.endOffset = elt.getEndOffset();
  }

  /**
   * Create a new AbstractWriter with the indicated Writer and
   * Element.  The full range of the Element will be used.  The range
   * will be limited to the indicated range of the Document.
   */
  protected AbstractWriter(Writer writer, Element elt, int pos, int len)
  {
    this.writer = writer;
    this.iter = new ElementIterator(elt);
    this.document = elt.getDocument();
    this.startOffset = pos;
    this.endOffset = pos + len;
  }

  /**
   * Return the ElementIterator for this writer.
   */
  protected ElementIterator getElementIterator()
  {
    return iter;
  }

  /**
   * Return the Writer to which we are writing.
   * @since 1.3
   */
  protected Writer getWriter()
  {
    return writer;
  }

  /**
   * Return this writer's Document.
   */
  protected Document getDocument()
  {
    return document;
  }

  /**
   * This method must be overridden by a concrete subclass.  It is
   * responsible for iterating over the Elements of the Document and
   * writing them out. 
   */
  protected abstract void write() throws IOException, BadLocationException;

  /**
   * Return the text of the Document that is associated with the given
   * Element.  If the Element is not a leaf Element, this will throw
   * BadLocationException.
   *
   * @throws BadLocationException if the element is not a leaf
   */
  protected String getText(Element elt) throws BadLocationException
  {
    if (! elt.isLeaf())
      throw new BadLocationException("Element is not a leaf",
				     elt.getStartOffset());
    return document.getText(elt.getStartOffset(), elt.getEndOffset());
  }

  /**
   * This method calls Writer.write on the indicated data, and updates
   * the current line length.  This method does not look for newlines
   * in the written data; the caller is responsible for that.
   *
   * @since 1.3
   */
  protected void output(char[] data, int start, int len) throws IOException
  {
    writer.write(data, start, len);
    lineLength += len;
  }

  /**
   * Write a line separator using the output method, and then reset
   * the current line length.
   *
   * @since 1.3
   */
  protected void writeLineSeparator() throws IOException
  {
    output(lineSeparatorChars, 0, lineSeparatorChars.length);
    lineLength = 0;
    indented = false;
  }

  /**
   * Write a single character.
   */
  protected void write(char ch) throws IOException
  {
    write(new char[] { ch }, 0, 1);
  }

  /**
   * Write a String.
   */
  protected void write(String s) throws IOException
  {
    char[] v = s.toCharArray();
    write(v, 0, v.length);
  }

  /**
   * Write a character array to the output Writer, properly handling
   * newlines and, if needed, wrapping lines as they are output.
   * @since 1.3
   */
  protected void write(char[] data, int start, int len) throws IOException
  {
    if (getCanWrapLines())
      {
	// FIXME: should we be handling newlines specially here?
	for (int i = 0; i < len; )
	  {
	    int start_i = i;
	    // Find next space.
	    while (i < len && data[start + i] != ' ')
	      ++i;
	    if (i < len && lineLength + i - start_i >= maxLineLength)
	      writeLineSeparator();
	    else if (i < len)
	      {
		// Write the trailing space.
		++i;
	      }
	    // Write out the text.
	    output(data, start + start_i, start + i - start_i);
	  }
      }
    else
      {
	int saved_i = start;
	for (int i = start; i < start + len; ++i)
	  {
	    if (data[i] == NEWLINE)
	      {
		output(data, saved_i, i - saved_i);
		writeLineSeparator();
	      }
	  }
	if (saved_i < start + len - 1)
	  output(data, saved_i, start + len - saved_i);
      }
  }

  /**
   * Indent this line by emitting spaces, according to the current
   * indent level and the current number of spaces per indent.  After
   * this method is called, the current line is no longer considered
   * to be empty, even if no spaces are actually written.
   */
  protected void indent() throws IOException
  {
    int spaces = indentLevel * indentSpace;
    if (spaces > 0)
      {
	char[] v = new char[spaces];
	Arrays.fill(v, ' ');
	write(v, 0, v.length);
      }
    indented = true;
  }

  /**
   * Return the index of the Document at which output starts.
   * @since 1.3
   */
  public int getStartOffset()
  {
    return startOffset;
  }

  /**
   * Return the index of the Document at which output ends.
   * @since 1.3
   */
  public int getEndOffset()
  {
    return endOffset;
  }

  /**
   * Return true if the Element's range overlaps our desired output
   * range; false otherwise.
   */
  protected boolean inRange(Element elt)
  {
    int eltStart = elt.getStartOffset();
    int eltEnd = elt.getEndOffset();
    return ((eltStart >= startOffset && eltStart < endOffset)
	    || (eltEnd >= startOffset && eltEnd < endOffset));
  }

  /**
   * Output the text of the indicated Element, properly clipping it to
   * the range of the Document specified when the AbstractWriter was
   * created.
   */
  protected void text(Element elt) throws BadLocationException, IOException
  {
    int eltStart = elt.getStartOffset();
    int eltEnd = elt.getEndOffset();

    eltStart = Math.max(eltStart, startOffset);
    eltEnd = Math.min(eltEnd, endOffset);
    write(document.getText(eltStart, eltEnd));
  }

  /**
   * Set the maximum line length.
   */
  protected void setLineLength(int maxLineLength)
  {
    this.maxLineLength = maxLineLength;
  }

  /**
   * Return the maximum line length.
   * @since 1.3
   */
  protected int getLineLength()
  {
    return maxLineLength;
  }

  /**
   * Set the current line length.
   * @since 1.3
   */
  protected void setCurrentLineLength(int lineLength)
  {
    this.lineLength = lineLength;
  }

  /**
   * Return the current line length.
   * @since 1.3
   */
  protected int getCurrentLineLength()
  {
    return lineLength;
  }

  /**
   * Return true if the line is empty, false otherwise.  The line is
   * empty if nothing has been written since the last newline, and
   * indent has not been invoked.
   */
  protected boolean isLineEmpty()
  {
    return lineLength == 0 && ! indented;
  }

  /**
   * Set the flag indicating whether lines will wrap.  This affects
   * the behavior of write().
   * @since 1.3
   */
  protected void setCanWrapLines(boolean canWrapLines)
  {
    this.canWrapLines = canWrapLines;
  }

  /**
   * Return true if lines printed via write() will wrap, false
   * otherwise.
   * @since 1.3
   */
  protected boolean getCanWrapLines()
  {
    return canWrapLines;
  }

  /**
   * Set the number of spaces per indent level.
   * @since 1.3
   */
  protected void setIndentSpace(int indentSpace)
  {
    this.indentSpace = indentSpace;
  }

  /**
   * Return the number of spaces per indent level.
   * @since 1.3
   */
  protected int getIndentSpace()
  {
    return indentSpace;
  }

  /**
   * Set the current line separator.
   * @since 1.3
   */
  public void setLineSeparator(String lineSeparator)
  {
    this.lineSeparator = lineSeparator;
    this.lineSeparatorChars = lineSeparator.toCharArray();
  }

  /**
   * Return the current line separator.
   * @since 1.3
   */
  public String getLineSeparator()
  {
    return lineSeparator;
  }

  /**
   * Increment the indent level.
   */
  protected void incrIndent()
  {
    ++indentLevel;
  }

  /**
   * Decrement the indent level.
   */
  protected void decrIndent()
  {
    --indentLevel;
  }

  /**
   * Return the current indent level.
   * @since 1.3
   */
  protected int getIndentLevel()
  {
    return indentLevel;
  }

  /**
   * Print the given AttributeSet as a sequence of assignment-like
   * strings, e.g. "key=value".
   */
  protected void writeAttributes(AttributeSet attrs) throws IOException
  {
    Enumeration e = attrs.getAttributeNames();
    while (e.hasMoreElements())
      {
	Object name = e.nextElement();
	Object val = attrs.getAttribute(name);
	write(name + "=" + val);
	writeLineSeparator();
      }
  }
}
