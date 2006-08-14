/* PlainDocument.java --
   Copyright (C) 2002, 2004, 2006  Free Software Foundation, Inc.

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

import java.util.ArrayList;

/**
 * A simple document class which maps lines to {@link Element}s.
 *
 * @author Anthony Balkissoon (abalkiss@redhat.com)
 * @author Graydon Hoare (graydon@redhat.com)
 * @author Roman Kennke (roman@kennke.org)
 * @author Michael Koch (konqueror@gmx.de)
 * @author Robert Schuster (robertschuster@fsfe.org)
 */
public class PlainDocument extends AbstractDocument
{
  private static final long serialVersionUID = 4758290289196893664L;
    
  public static final String lineLimitAttribute = "lineLimit";
  public static final String tabSizeAttribute = "tabSize";

  /**
   * The default root element of this document. This is made type Element
   * because the RI seems to accept other types of elements as well from
   * createDefaultRoot() (when overridden by a subclass).
   */
  private Element rootElement;
  
  public PlainDocument()
  {
    this(new GapContent());
  }

  public PlainDocument(AbstractDocument.Content content)
  {
    super(content);
    rootElement = createDefaultRoot();

    // This property has been determined using a Mauve test.
    putProperty("tabSize", new Integer(8));
  }

  private void reindex()
  {
    Element[] lines;
    try 
      {
        String str = content.getString(0, content.length());

        ArrayList elts = new ArrayList();
        int j = 0;
        for (int i = str.indexOf('\n', 0); i != -1; i = str.indexOf('\n', i + 1))
          {
            elts.add(createLeafElement(rootElement, SimpleAttributeSet.EMPTY, j, i + 1));
            j = i + 1;
          }
        
        if (j < content.length())
          elts.add(createLeafElement(rootElement, SimpleAttributeSet.EMPTY, j, content.length()));
        
        lines = new Element[elts.size()];
        for (int i = 0; i < elts.size(); ++i)
          lines[i] = (Element) elts.get(i);
      }
    catch (BadLocationException e)
      {
        lines = new Element[1];
        lines[0] = createLeafElement(rootElement, SimpleAttributeSet.EMPTY, 0, 1);
      }

    ((BranchElement) rootElement).replace(0, rootElement.getElementCount(), lines);
  }

  protected AbstractDocument.AbstractElement createDefaultRoot()
  {
    BranchElement root =
      (BranchElement) createBranchElement(null, null);

    Element[] array = new Element[1];
    array[0] = createLeafElement(root, null, 0, 1);
    root.replace(0, 0, array);
    
    return root;
  }

  protected void insertUpdate(DefaultDocumentEvent event,
                              AttributeSet attributes)
  {

    String text = null;
    int offset = event.getOffset();
    int length = event.getLength();
    try
      {
        text = getText(offset, length);
      }
    catch (BadLocationException ex)
      {
        AssertionError err = new AssertionError();
        err.initCause(ex);
        throw err;
      }

    boolean hasLineBreak = text.indexOf('\n') != -1;
    boolean prevCharIsLineBreak = false;
    try
      {
        prevCharIsLineBreak =
          offset > 0 && getText(offset - 1, 1).charAt(0) == '\n';
      }
    catch (BadLocationException ex)
      {
        AssertionError err = new AssertionError();
        err.initCause(ex);
        throw err;
      }
    boolean lastCharIsLineBreak = text.charAt(text.length() - 1) == '\n';
    int lineIndex = -1;
    int lineStart = -1;
    int lineEnd = -1;
    Element[] removed = null;
    BranchElement root = (BranchElement) rootElement;
    boolean updateStructure = true;

    if (prevCharIsLineBreak && ! lastCharIsLineBreak)
      {
        // We must fix the structure a little if the previous char
        // is a linebreak and the last char isn't.
        lineIndex = root.getElementIndex(offset - 1);
        Element prevLine = root.getElement(lineIndex);
        Element nextLine = root.getElement(lineIndex + 1);
        lineStart = prevLine.getStartOffset();
        lineEnd = nextLine.getEndOffset();
        removed = new Element[]{ prevLine, nextLine };
      }
    else if (hasLineBreak)
      {
        lineIndex = root.getElementIndex(offset);
        Element line = root.getElement(lineIndex);
        lineStart = line.getStartOffset();
        lineEnd = line.getEndOffset();
        removed = new Element[]{ line };
      }
    else
      {
        updateStructure = false;
      }

    if (updateStructure)
      {
        // Break the lines between lineStart and lineEnd.
        ArrayList lines = new ArrayList();
        int len = lineEnd - lineStart;
        try
          {
            text = getText(lineStart, len);
          }
        catch (BadLocationException ex)
          {
            AssertionError err = new AssertionError();
            err.initCause(ex);
            throw err;
          }
        int prevLineBreak = 0;
        int lineBreak = text.indexOf('\n');
        do
          {
            lineBreak++;
            lines.add(createLeafElement(root, null, lineStart + prevLineBreak,
                                        lineStart + lineBreak));
            prevLineBreak = lineBreak;
            lineBreak = text.indexOf('\n', prevLineBreak);
          } while (prevLineBreak < len);

        // Update the element structure and prepare document event.
        Element[] added = (Element[]) lines.toArray(new Element[lines.size()]);
        event.addEdit(new ElementEdit(root, lineIndex, removed, added));
        root.replace(lineIndex, removed.length, added);
      }
    super.insertUpdate(event, attributes);
  }

  protected void removeUpdate(DefaultDocumentEvent event)
  {
    super.removeUpdate(event);

    // added and removed are Element arrays used to add an ElementEdit
    // to the DocumentEvent if there were entire lines added or removed
    // from the Document
    Element[] added = new Element[1];
    Element[] removed;
    int p0 = event.getOffset();

    // check if we must collapse some elements
    int i1 = rootElement.getElementIndex(p0);
    int i2 = rootElement.getElementIndex(p0 + event.getLength());
    if (i1 != i2)
      {
        // If there were lines removed then we have to add an ElementEdit
        // to the DocumentEvent so we set it up now by filling the Element
        // arrays "removed" and "added" appropriately
        removed = new Element [i2 - i1 + 1];
        for (int i = i1; i <= i2; i++)
          removed[i-i1] = rootElement.getElement(i);
        
        int start = rootElement.getElement(i1).getStartOffset();
        int end = rootElement.getElement(i2).getEndOffset();        
        added[0] = createLeafElement(rootElement,
                                          SimpleAttributeSet.EMPTY,
                                          start, end);

        // Now create and add the ElementEdit
        ElementEdit e = new ElementEdit(rootElement, i1, removed, added);
        event.addEdit(e);

        // collapse elements if the removal spans more than 1 line
        ((BranchElement) rootElement).replace(i1, i2 - i1 + 1, added);
      }
  }

  public Element getDefaultRootElement()
  {
    return rootElement;
  }

  public Element getParagraphElement(int pos)
  {
    Element root = getDefaultRootElement();
    return root.getElement(root.getElementIndex(pos));
  }

  /**
   * Inserts a string into the document. If the document property
   * '<code>filterNewLines</code>' is set to <code>Boolean.TRUE</code>, then
   * all newlines in the inserted string are replaced by space characters,
   * otherwise the superclasses behaviour is executed.
   *
   * Inserting content causes a write lock to be acquired during this method
   * call.
   *
   * @param offs the offset at which to insert the string
   * @param str the string to be inserted
   * @param atts the text attributes of the string to be inserted
   *
   * @throws BadLocationException
   */
  public void insertString(int offs, String str, AttributeSet atts)
    throws BadLocationException
  {
    String string = str;
    if (str != null && Boolean.TRUE.equals(getProperty("filterNewlines")))
      string = str.replaceAll("\n", " ");
    super.insertString(offs, string, atts);
  }
}
