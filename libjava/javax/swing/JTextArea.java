/* JTextArea.java -- 
   Copyright (C) 2004, 2005  Free Software Foundation, Inc.

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
Free Software Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA
02111-1307 USA.

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


package javax.swing;

import java.awt.FontMetrics;

import javax.swing.text.BadLocationException;
import javax.swing.text.Document;
import javax.swing.text.Element;
import javax.swing.text.JTextComponent;
import javax.swing.text.PlainDocument;

/**
 * The <code>JTextArea</code> component provides a multi-line area for displaying
 * and editing plain text.  The component is designed to act as a lightweight
 * replacement for the heavyweight <code>java.awt.TextArea</code> component,
 * which provides similar functionality using native widgets.
 * <p>
 *
 * This component has additional functionality to the AWT class.  It follows
 * the same design pattern as seen in other text components, such as
 * <code>JTextField</code>, <code>JTextPane</code> and <code>JEditorPane</code>,
 * and embodied in <code>JTextComponent</code>.  These classes separate the text
 * (the model) from its appearance within the onscreen component (the view).  The
 * text is held within a <code>javax.swing.text.Document</code> object, which can
 * also maintain relevant style information where necessary.  As a result, it is the
 * document that should be monitored for textual changes, via
 * <code>DocumentEvent</code>s delivered to registered
 * <code>DocumentListener</code>s, rather than this component.
 * <p>
 *
 * Unlike <code>java.awt.TextArea</code>, <code>JTextArea</code> does not
 * handle scrolling.  Instead, this functionality is delegated to a
 * <code>JScrollPane</code>, which can contain the text area and handle
 * scrolling when required.  Likewise, the word wrapping functionality
 * of the AWT component is converted to a property of this component
 * and the <code>rows</code> and <code>columns</code> properties
 * are used in calculating the preferred size of the scroll pane's
 * view port.
 *
 * @author Michael Koch  (konqueror@gmx.de)
 * @author Andrew John Hughes  (gnu_andrew@member.fsf.org)
 * @see java.awt.TextArea
 * @see javax.swing.JTextComponent
 * @see javax.swing.JTextField
 * @see javax.swing.JTextPane
 * @see javax.swing.JEditorPane
 * @see javax.swing.text.Document
 * @see javax.swing.text.DocumentEvent
 * @see javax.swing.text.DocumentListener
 */

public class JTextArea extends JTextComponent
{
  /**
   * Compatible with Sun's JDK
   */
  private static final long serialVersionUID = -6141680179310439825L;
  
  /**
   * The number of rows used by the component.
   */
  private int rows;

  /**
   * The number of columns used by the component.
   */
  private int columns;

  /**
   * Whether line wrapping is enabled or not.
   */
  private boolean lineWrap;

  /**
   * The number of characters equal to a tab within the text.
   */
  private int tabSize = 8;

  private boolean wrapStyleWord;

  /**
   * Creates a new <code>JTextArea</code> object.
   */
  public JTextArea()
  {
    this(null, null, 0, 0);
  }

  /**
   * Creates a new <code>JTextArea</code> object.
   *
   * @param text the initial text
   */
  public JTextArea(String text)
  {
    this(null, text, 0, 0);
  }

  /**
   * Creates a new <code>JTextArea</code> object.
   *
   * @param rows the number of rows
   * @param columns the number of cols
   *
   * @exception IllegalArgumentException if rows or columns are negative
   */
  public JTextArea(int rows, int columns)
  {
    this(null, null, rows, columns);
  }

  /**
   * Creates a new <code>JTextArea</code> object.
   *
   * @param text the initial text
   * @param rows the number of rows
   * @param columns the number of cols
   *
   * @exception IllegalArgumentException if rows or columns are negative
   */
  public JTextArea(String text, int rows, int columns)
  {
    this(null, text, rows, columns);
  }

  /**
   * Creates a new <code>JTextArea</code> object.
   *
   * @param the document model to use
   */
  public JTextArea(Document doc)
  {
    this(doc, null, 0, 0);
  }

  /**
   * Creates a new <code>JTextArea</code> object.
   *
   * @param the document model to use
   * @param text the initial text
   * @param rows the number of rows
   * @param columns the number of cols
   *
   * @exception IllegalArgumentException if rows or columns are negative
   */
  public JTextArea(Document doc, String text, int rows, int columns)
  {
    setDocument(doc == null ? createDefaultModel() : doc);
    setText(text);
    setRows(rows);
    setColumns(columns);
  }

  /**
   * Appends the supplied text to the current contents
   * of the document model.
   *
   * @param toAppend the text to append
   */
  public void append(String toAppend)
  {
      try
	  {
	      getDocument().insertString(getText().length(), toAppend, null);
	  }
      catch (BadLocationException exception)
	  {
	      /* This shouldn't happen in theory -- but, if it does...  */
	      throw new RuntimeException("Unexpected exception occurred.", exception);
	  }
  }

  /**
   * Creates the default document model.
   *
   * @return a new default model
   */
  protected Document createDefaultModel()
  {
    return new PlainDocument();
  }

  /**
   * Returns true if the width of this component should be forced
   * to match the width of a surrounding view port.  When line wrapping
   * is turned on, this method returns true.
   *
   * @return true if lines are wrapped.
   */
  public boolean getScrollableTracksViewportWidth()
  {
    return lineWrap ? true : super.getScrollableTracksViewportWidth();
  }

  /**
   * Returns the UI class ID string.
   *
   * @return the string "TextAreaUI"
   */
  public String getUIClassID()
  {
    return "TextAreaUI";
  }

  /**
   * Returns the current number of columns.
   *
   * @return number of columns
   */
  public int getColumns()
  {
    return columns;
  }
  
  /**
   * Sets the number of rows.
   *
   * @param columns number of columns
   *
   * @exception IllegalArgumentException if columns is negative
   */
  public void setColumns(int columns)
  {
    if (columns < 0)
      throw new IllegalArgumentException();

    this.columns = columns;
  }

  /**
   * Returns the current number of rows.
   *
   * @return number of rows
   */
  public int getRows()
  {
    return rows;
  }

  /**
   * Sets the number of rows.
   *
   * @param columns number of columns
   *
   * @exception IllegalArgumentException if rows is negative
   */
  public void setRows(int rows)
  {
    if (rows < 0)
      throw new IllegalArgumentException();

    this.rows = rows;
  }

  /**
   * Checks whether line wrapping is enabled.
   *
   * @return <code>true</code> if line wrapping is enabled,
   * <code>false</code> otherwise
   */
  public boolean getLineWrap()
  {
    return lineWrap;
  }

  /**
   * Enables/disables line wrapping.
   *
   * @param wrapping <code>true</code> to enable line wrapping,
   * <code>false</code> otherwise
   */
  public void setLineWrap(boolean flag)
  {
    if (lineWrap == flag)
      return;

    boolean oldValue = lineWrap;
    lineWrap = flag;
    firePropertyChange("lineWrap", oldValue, lineWrap);
  }

  /**
   * Checks whether word style wrapping is enabled.
   *
   * @return <code>true</code> if word style wrapping is enabled,
   * <code>false</code> otherwise
   */
  public boolean getWrapStyleWord()
  {
    return wrapStyleWord;
  }
  
  /**
   * Enables/Disables word style wrapping.
   *
   * @param flag <code>true</code> to enable word style wrapping,
   * <code>false</code> otherwise
   */
  public void setWrapStyleWord(boolean flag)
  {
    if (wrapStyleWord == flag)
      return;
    
    boolean oldValue = wrapStyleWord;
    wrapStyleWord = flag;
    firePropertyChange("wrapStyleWord", oldValue, wrapStyleWord);
  }
  
  /**
   * Returns the number of characters used for a tab.
   * This defaults to 8.
   *
   * @return the current number of spaces used for a tab.
   */
  public int getTabSize()
  {
    return tabSize;
  }

  /**
   * Sets the number of characters used for a tab to the
   * supplied value.  If a change to the tab size property
   * occurs (i.e. newSize != tabSize), a property change event
   * is fired.
   * 
   * @param newSize The new number of characters to use for a tab.
   */
  public void setTabSize(int newSize)
  {
    if (tabSize == newSize)
      return;
    
    int oldValue = tabSize;
    tabSize = newSize;
    firePropertyChange("tabSize", oldValue, tabSize);
  }

  protected int getColumnWidth()
  {
    FontMetrics metrics = getToolkit().getFontMetrics(getFont());
    return metrics.charWidth('m');
  }

  public int getLineCount()
  {
    return getDocument().getDefaultRootElement().getElementCount();
  }

  public int getLineStartOffset(int line)
     throws BadLocationException
  {
    int lineCount = getLineCount();
    
    if (line < 0 || line > lineCount)
      throw new BadLocationException("Non-existing line number", line);

    Element lineElem = getDocument().getDefaultRootElement().getElement(line);
    return lineElem.getStartOffset();
  }

  public int getLineEndOffset(int line)
     throws BadLocationException
  {
    int lineCount = getLineCount();
    
    if (line < 0 || line > lineCount)
      throw new BadLocationException("Non-existing line number", line);

    Element lineElem = getDocument().getDefaultRootElement().getElement(line);
    return lineElem.getEndOffset();
  }

  public int getLineOfOffset(int offset)
    throws BadLocationException
  {
    Document doc = getDocument();

    if (offset < doc.getStartPosition().getOffset()
	|| offset >= doc.getEndPosition().getOffset())
      throw new BadLocationException("offset outside of document", offset);

    return doc.getDefaultRootElement().getElementIndex(offset);
  }

  protected int getRowHeight()
  {
    FontMetrics metrics = getToolkit().getFontMetrics(getFont());
    return metrics.getHeight();
  }

  /**
   * Inserts the supplied text at the specified position.  Nothing
   * happens in the case that the model or the supplied string is null
   * or of zero length.
   *
   * @param string The string of text to insert.
   * @param position The position at which to insert the supplied text.
   * @throws IllegalArgumentException if the position is &lt; 0 or greater
   * than the length of the current text.
   */
  public void insert(String string, int position)
  {
    // Retrieve the document model.
    Document doc = getDocument();
      
    // Check the model and string for validity.
    if (doc == null
	|| string == null
	|| string.length() == 0)
      return;

    // Insert the text into the model.
    try
      {
	doc.insertString(position, string, null);
      }
    catch (BadLocationException e)
      {
	throw new IllegalArgumentException("The supplied position, "
					   + position + ", was invalid.");
      }
  }

  public void replaceRange(String text, int start, int end)
  {
    Document doc = getDocument();
    
    if (start > end
	|| start < doc.getStartPosition().getOffset()
	|| end >= doc.getEndPosition().getOffset())
      throw new IllegalArgumentException();

    try
      {
	doc.remove(start, end);
	doc.insertString(start, text, null);
      }
    catch (BadLocationException e)
      {
	// This cannot happen as we check offset above.
      }
  }
}
