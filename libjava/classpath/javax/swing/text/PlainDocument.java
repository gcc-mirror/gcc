/* PlainDocument.java --
   Copyright (C) 2002, 2004  Free Software Foundation, Inc.

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

public class PlainDocument extends AbstractDocument
{
  private static final long serialVersionUID = 4758290289196893664L;
    
  public static final String lineLimitAttribute = "lineLimit";
  public static final String tabSizeAttribute = "tabSize";

  private BranchElement rootElement;
  private int tabSize;
  
  public PlainDocument()
  {
    this(new GapContent());
  }

  public PlainDocument(AbstractDocument.Content content)
  {
    super(content);
    tabSize = 8;
    rootElement = (BranchElement) createDefaultRoot();
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
      (BranchElement) createBranchElement(null, SimpleAttributeSet.EMPTY);

    Element[] array = new Element[1];
    array[0] = createLeafElement(root, SimpleAttributeSet.EMPTY, 0, 1);
    root.replace(0, 0, array);
    
    return root;
  }

  protected void insertUpdate(DefaultDocumentEvent event,
                              AttributeSet attributes)
  {
    int offset = event.getOffset();
    int end = offset + event.getLength();
    int elementIndex = rootElement.getElementIndex(offset);
    Element firstElement = rootElement.getElement(elementIndex);
    
    // If we're inserting immediately after a newline we have to fix the 
    // Element structure.
    if (offset > 0)
      {
        try
        {
          String s = getText(offset - 1, 1);
          if (s.equals("\n"))
            {
              int newEl2EndOffset = end;
              boolean replaceNext = false;
              if (rootElement.getElementCount() > elementIndex + 1)
                {
                  replaceNext = true;
                  newEl2EndOffset = 
                    rootElement.getElement(elementIndex + 1).getEndOffset();
                }
              Element newEl1 = 
                createLeafElement(rootElement, firstElement.getAttributes(), 
                                  firstElement.getStartOffset(), offset);
              Element newEl2 = 
                createLeafElement (rootElement, firstElement.getAttributes(), 
                                   offset, newEl2EndOffset);
              if (replaceNext)
                rootElement.replace(elementIndex, 2, new Element[] { newEl1, newEl2 });
              else
                rootElement.replace(elementIndex, 1, new Element[] { newEl1, newEl2 });
              firstElement = newEl2;
              elementIndex ++;
            }
        }
        catch (BadLocationException ble)
        {          
          // This shouldn't happen.
          AssertionError ae = new AssertionError();
          ae.initCause(ble);
          throw ae;
        }        
      }

    // added and removed are Element arrays used to add an ElementEdit
    // to the DocumentEvent if there were entire lines added or removed.
    Element[] removed = new Element[1];
    Element[] added;
    try 
      {
        String str = content.getString(0, content.length());
        ArrayList elts = new ArrayList();

        // Determine how many NEW lines were added by finding the newline
        // characters within the newly inserted text
        int j = firstElement.getStartOffset();
        int i = str.indexOf('\n', offset);
        while (i != -1 && i <= end)
          {            
            // For each new line, create a new element
            elts.add(createLeafElement(rootElement, SimpleAttributeSet.EMPTY,
                                       j, i + 1));
            j = i + 1;
            if (j >= str.length())
              break;
            i = str.indexOf('\n', j);
          }
        // If there were new lines added we have to add an ElementEdit to 
        // the DocumentEvent and we have to call rootElement.replace to 
        // insert the new lines
        if (elts.size() != 0)
          {
            // Set up the ElementEdit by filling the added and removed 
            // arrays with the proper Elements
            added = new Element[elts.size()];
            for (int k = 0; k < elts.size(); ++k)
              added[k] = (Element) elts.get(k);
            removed[0] = firstElement;
            
            // Now create and add the ElementEdit
            ElementEdit e = new ElementEdit(rootElement, elementIndex, removed,
                                            added);
            event.addEdit(e);
            
            // And call replace to actually make the changes
            ((BranchElement) rootElement).replace(elementIndex, 1, added);
          }
      }
    catch (BadLocationException e)
      {
        // This shouldn't happen so we throw an AssertionError
        AssertionError ae = new AssertionError();
        ae.initCause(e);
        throw ae;
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
        rootElement.replace(i1, i2 - i1 + 1, added);
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
