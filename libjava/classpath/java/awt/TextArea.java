/* TextArea.java -- A multi-line text entry component
   Copyright (C) 1999, 2004 Free Software Foundation, Inc.

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


package java.awt;

import java.awt.event.KeyEvent;
import java.awt.peer.ComponentPeer;
import java.awt.peer.TextAreaPeer;
import java.util.HashSet;
import java.util.Set;

import javax.accessibility.AccessibleContext;
import javax.accessibility.AccessibleStateSet;


/**
 * A TextArea is a text component capable of displaying multiple lines
 * of user-editable text.  A TextArea handles its own scrolling and
 * can display vertical and horizontal scrollbars as navigation aids.
 *
 * @author Aaron M. Renn (arenn@urbanophile.com)
 */
public class TextArea extends TextComponent implements java.io.Serializable
{
  /**
   * Display both horiztonal and vertical scroll bars.
   */
  public static final int SCROLLBARS_BOTH = 0;

  /**
   * Display vertical scroll bar only.
   */
  public static final int SCROLLBARS_VERTICAL_ONLY = 1;

  /**
   * Display horizatonal scroll bar only.
   */
  public static final int SCROLLBARS_HORIZONTAL_ONLY = 2;

  /**
   * Do not display scrollbars.
   */
  public static final int SCROLLBARS_NONE = 3;

  /**
   * Serialization constant.
   */
  private static final long serialVersionUID = 3692302836626095722L;

  /**
   * @serial The number of columns used in this text area's preferred
   * and minimum size calculations.
   */
  private int columns;

  /**
   * @serial The number of rows used in this text area's preferred and
   * minimum size calculations.
   */
  private int rows;

  /**
   * @serial The scrollbar display policy.  One of SCROLLBARS_BOTH,
   * SCROLLBARS_VERTICAL_ONLY, SCROLLBARS_HORIZONTAL_ONLY,
   * SCROLLBARS_NONE.
   */
  private int scrollbarVisibility;

  /*
   * The number used to generate the name returned by getName.
   */
  private static transient long next_text_number;

  /**
   * Initialize a new instance of <code>TextArea</code> that is empty.
   * Conceptually the <code>TextArea</code> has 0 rows and 0 columns
   * but its initial bounds are defined by its peer or by the
   * container in which it is packed.  Both horizontal and vertical
   * scrollbars will be displayed.
   *
   * @exception HeadlessException if GraphicsEnvironment.isHeadless () is true
   */
  public TextArea ()
  {
    this ("", 0, 0, SCROLLBARS_BOTH);
  }

  /**
   * Initialize a new instance of <code>TextArea</code> that contains
   * the specified text.  Conceptually the <code>TextArea</code> has 0
   * rows and 0 columns but its initial bounds are defined by its peer
   * or by the container in which it is packed.  Both horizontal and
   * veritcal scrollbars will be displayed.  The TextArea initially contains
   * the specified text.  If text specified as <code>null<code>, it will
   * be set to "".
   *
   * @param text The text to display in this text area (<code>null</code> permitted).
   *
   * @exception HeadlessException if GraphicsEnvironment.isHeadless () is true
   */
  public TextArea (String text)
  {
    this (text, 0, 0, SCROLLBARS_BOTH);
  }

  /**
   * Initialize a new instance of <code>TextArea</code> that is empty
   * and can display the specified number of rows and columns of text,
   * without the need to scroll.  Both horizontal and vertical
   * scrollbars will be displayed.
   *
   * @param rows The number of rows in this text area.
   * @param columns The number of columns in this text area.
   *
   * @exception HeadlessException if GraphicsEnvironment.isHeadless () is true
   */
  public TextArea (int rows, int columns)
  {
    this ("", rows, columns, SCROLLBARS_BOTH);
  }

  /**
   * Initialize a new instance of <code>TextArea</code> that can
   * display the specified number of rows and columns of text, without
   * the need to scroll.  The TextArea initially contains the
   * specified text.  If text specified as <code>null<code>, it will
   * be set to "".
   *
   * @param text The text to display in this text area (<code>null</code> permitted).
   * @param rows The number of rows in this text area.
   * @param columns The number of columns in this text area.
   *
   * @exception HeadlessException if GraphicsEnvironment.isHeadless () is true
   */
  public TextArea (String text, int rows, int columns)
  {
    this (text, rows, columns, SCROLLBARS_BOTH);
  }

  /**
   * Initialize a new instance of <code>TextArea</code> that initially
   * contains the specified text.  The TextArea can display the
   * specified number of rows and columns of text, without the need to
   * scroll.  This constructor allows specification of the scroll bar
   * display policy.  The TextArea initially contains the specified text.  
   * If text specified as <code>null<code>, it will be set to "". 
   *
   * @param text The text to display in this text area (<code>null</code> permitted).
   * @param rows The number of rows in this text area.
   * @param columns The number of columns in this text area.
   * @param scrollbarVisibility The scroll bar display policy. One of
   * SCROLLBARS_BOTH, SCROLLBARS_VERTICAL_ONLY,
   * SCROLLBARS_HORIZONTAL_ONLY, SCROLLBARS_NONE.
   *
   * @exception HeadlessException if GraphicsEnvironment.isHeadless () is true
   */
  public TextArea (String text, int rows, int columns, int scrollbarVisibility)
  {
    super (text);

    if (GraphicsEnvironment.isHeadless ())
      throw new HeadlessException ();

    if (rows < 0)
      this.rows = 0;
    else
      this.rows = rows;
    
    if (columns < 0)
      this.columns = 0;
    else
      this.columns = columns;

    if (scrollbarVisibility < 0 || scrollbarVisibility > 4)
      this.scrollbarVisibility = SCROLLBARS_BOTH;
    else
      this.scrollbarVisibility = scrollbarVisibility;

    // TextAreas need to receive tab key events so we override the
    // default forward and backward traversal key sets.
    Set s = new HashSet ();
    s.add (AWTKeyStroke.getAWTKeyStroke (KeyEvent.VK_TAB,
                                         KeyEvent.CTRL_DOWN_MASK));
    setFocusTraversalKeys (KeyboardFocusManager.FORWARD_TRAVERSAL_KEYS, s);
    s = new HashSet ();
    s.add (AWTKeyStroke.getAWTKeyStroke (KeyEvent.VK_TAB,
                                         KeyEvent.SHIFT_DOWN_MASK
                                         | KeyEvent.CTRL_DOWN_MASK));
    setFocusTraversalKeys (KeyboardFocusManager.BACKWARD_TRAVERSAL_KEYS, s);
  }

  /**
   * Retrieve the number of columns that this text area would prefer
   * to display.  This value may or may not correspond to the number
   * of columns that are actually displayed.
   *
   * @return The preferred number of columns.
   */
  public int getColumns ()
  {
    return columns;
  }

  /**
   * Set the preferred number of columns for this text area.  This
   * method does not cause the number of columns displayed by the text
   * area to be updated, if the text area is currently visible.
   *
   * @param columns The preferred number of columns.
   *
   * @exception IllegalArgumentException If columns is less than zero.
   */
  public synchronized void setColumns (int columns)
  {
    if (columns < 0)
      throw new IllegalArgumentException ("Value is less than zero: "
                                          + columns);

    this.columns = columns;
  }

  /**
   * Retrieve the number of rows that this text area would prefer to
   * display.  This value may or may not correspond to the number of
   * rows that are actually displayed.
   *
   * @return The preferred number of rows.
   */
  public int getRows ()
  {
    return rows;
  }

  /**
   * Set the preferred number of rows for this text area.  This method
   * does not cause the number of columns displayed by the text area
   * to be updated, if the text area is currently visible.
   *
   * @param rows The preferred number of rows.
   *
   * @exception IllegalArgumentException If rows is less than zero.
   */
  public synchronized void setRows (int rows)
  {
    if (rows < 1)
      throw new IllegalArgumentException ("Value is less than one: " + rows);

    this.rows = rows;
  }

  /**
   * Retrieve the minimum size for this text area.
   *
   * @return The minimum size for this text field.
   */
  public Dimension getMinimumSize ()
  {
    return getMinimumSize (getRows (), getColumns ());
  }

  /**
   * Retrieve the minimum size for this text area.  If the minimum
   * size has been set, then rows and columns are used in the calculation.
   *
   * @param rows The number of rows to use in the minimum size
   * calculation.
   * @param columns The number of columns to use in the minimum size
   * calculation.
   *
   * @return The minimum size for this text area.
   */
  public Dimension getMinimumSize (int rows, int columns)
  {
    return minimumSize (rows, columns);
  }

  /**
   * Retrieve the minimum size for this text area.
   * 
   * @return The minimum size for this text area.
   *
   * @deprecated This method is deprecated in favor of
   * <code>getMinimumSize ()</code>.
   */
  public Dimension minimumSize ()
  {
    return minimumSize (getRows (), getColumns ());
  }

  /**
   * Retrieve the minimum size for this text area.  If the minimum
   * size has been set, then rows and columns are used in the calculation.
   *
   * @param rows The number of rows to use in the minimum size
   * calculation.
   * @param columns The number of columns to use in the minimum size
   * calculation.
   *
   * @return The minimum size for this text area.
   *
   * @deprecated This method is deprecated in favor of
   * <code>getMinimumSize (int, int)</code>.
   */
  public Dimension minimumSize (int rows, int columns)
  {
    if (isMinimumSizeSet())
      return new Dimension(minSize);
    
    TextAreaPeer peer = (TextAreaPeer) getPeer ();
    if (peer == null)
      return new Dimension (getWidth(), getHeight());

    return peer.getMinimumSize (rows, columns);
  }

  /**
   * Retrieve the preferred size for this text area.
   *
   * @return The preferred size for this text field.
   */
  public Dimension getPreferredSize ()
  {
    return getPreferredSize (getRows (), getColumns ());
  }

  /**
   * Retrieve the preferred size for this text area.  If the preferred
   * size has been set, then rows and columns are used in the calculation.
   *
   * @param rows The number of rows to use in the preferred size
   * calculation.
   * @param columns The number of columns to use in the preferred size
   * calculation.
   *
   * @return The preferred size for this text area.
   */
  public Dimension getPreferredSize (int rows, int columns)
  {
    return preferredSize (rows, columns);
  }

  /**
   * Retrieve the preferred size for this text area.
   *
   * @return The preferred size for this text field.
   *
   * @deprecated This method is deprecated in favor of
   * <code>getPreferredSize ()</code>.
   */
  public Dimension preferredSize ()
  {
    return preferredSize (getRows (), getColumns ());
  }

  /**
   * Retrieve the preferred size for this text area.  If the preferred
   * size has been set, then rows and columns are used in the calculation.
   *
   * @param rows The number of rows to use in the preferred size
   * calculation.
   * @param columns The number of columns to use in the preferred size
   * calculation.
   *
   * @return The preferred size for this text area.
   *
   * @deprecated This method is deprecated in favor of
   * <code>getPreferredSize (int, int)</code>.
   */
  public Dimension preferredSize (int rows, int columns)
  {
    if (isPreferredSizeSet())
      return new Dimension(prefSize);
    
    TextAreaPeer peer = (TextAreaPeer) getPeer ();
    if (peer == null)
      return new Dimension (getWidth(), getHeight());

    return peer.getPreferredSize (rows, columns);
  }

  /**
   * Retrieve the scroll bar display policy -- one of SCROLLBARS_BOTH,
   * SCROLLBARS_VERTICAL_ONLY, SCROLLBARS_HORIZONTAL_ONLY,
   * SCROLLBARS_NONE.
   *
   * @return The current scroll bar display policy.
   */
  public int getScrollbarVisibility ()
  {
    return scrollbarVisibility;
  }

  /**
   * Notify this object that it should create its native peer.
   */
  public void addNotify ()
  {
    if (getPeer () == null)
      setPeer ((ComponentPeer) getToolkit().createTextArea (this));
  }

  /**
   * Append the specified text to the end of the current text.
   *
   * @param str The text to append.
   */
  public void append (String str)
  {
    appendText (str);
  }

  /**
   * Append the specified text to the end of the current text.
   *
   * @param str The text to append.
   *
   * @deprecated This method is deprecated in favor of
   * <code>append ()</code>.
   */
  public void appendText (String str)
  {
    TextAreaPeer peer = (TextAreaPeer) getPeer ();

    if (peer != null)
      peer.insert (str, peer.getText().length ());
    else
      setText(getText() + str);   
  }

  /**
   * Insert the specified text at the specified position.  The first
   * character in the text area is at position zero.
   *
   * @param str The text to insert.
   * @param pos The position at which to insert text.
   */
  public void insert (String str, int pos)
  {
    insertText (str, pos);
  }

  /**
   * Insert the specified text at the specified position.  The first
   * character in the text area is at position zero.
   *
   * @param str The text to insert.
   * @param pos The position at which to insert text.
   *
   * @deprecated This method is deprecated in favor of
   * <code>insert ()</code>.
   */
  public void insertText (String str, int pos)
  {
    String tmp1 = null;
    String tmp2 = null;
    
    TextAreaPeer peer = (TextAreaPeer) getPeer ();

    if (peer != null)
      peer.insert (str, pos);
    else
      {
        tmp1 = getText().substring(0, pos);
        tmp2 = getText().substring(pos, getText().length());
        setText(tmp1 + str + tmp2);
      }
  }

  /**
   * Replace a range of characters with the specified text.  The
   * character at the start position will be replaced, unless start ==
   * end.  The character at the end posistion will not be replaced.
   * The first character in the text area is at position zero.  The
   * length of the replacement text may differ from the length of the
   * text that is replaced.
   *
   * @param str The new text for the range.
   * @param start The start position of the replacement range.
   * @param end The end position of the replacement range.
   */
  public void replaceRange (String str, int start, int end)
  {
    replaceText (str, start, end);
  }

  /**
   * Replace a range of characters with the specified text.  The
   * character at the start position will be replaced, unless start ==
   * end.  The character at the end posistion will not be replaced.
   * The first character in the text area is at position zero.  The
   * length of the replacement text may differ from the length of the
   * text that is replaced.
   *
   * @param str The new text for the range.
   * @param start The start position of the replacement range.
   * @param end The end position of the replacement range.
   *
   * @deprecated This method is deprecated in favor of
   * <code>replaceRange ()</code>.
   */
  public void replaceText (String str, int start, int end)
  {
    String tmp1 = null;
    String tmp2 = null;

    TextAreaPeer peer = (TextAreaPeer) getPeer();

    if (peer != null)
      peer.replaceRange(str, start, end);
    else
      {
        tmp1 = getText().substring(0, start);
        tmp2 = getText().substring(end, getText().length());
        setText(tmp1 + str + tmp2);
      }
  }

  /**
   * Retrieve a debugging string for this text area.
   *
   * @return A debugging string for this text area.
   */
  protected String paramString ()
  {
    String sbVisibility = "";

    switch (scrollbarVisibility)
      {
      case SCROLLBARS_BOTH:
	sbVisibility = "both";
	break;
      case SCROLLBARS_VERTICAL_ONLY:
	sbVisibility = "vertical-only";
	break;
      case SCROLLBARS_HORIZONTAL_ONLY:
	sbVisibility = "horizontal-only";
	break;
      case SCROLLBARS_NONE:
	sbVisibility = "none";
	break;
      }

    String editable = "";
    if (isEditable ())
      editable = "editable,";

    return getName () + "," + getX () + "," + getY () + "," + getWidth ()
           + "x" + getHeight () + "," + "text=" + getText () + "," + editable
           + "selection=" + getSelectionStart () + "-" + getSelectionEnd ()
           + ",rows=" + rows + ",columns=" + columns + ",scrollbarVisibility="
           + sbVisibility;
  }

  /**
   * Generate a unique name for this text area.
   *
   * @return A unique name for this text area.
   */
  String generateName ()
  {
    return "text" + getUniqueLong ();
  }

  private static synchronized long getUniqueLong ()
  {
    return next_text_number++;
  }
  
  protected class AccessibleAWTTextArea extends AccessibleAWTTextComponent
  {
    private static final long serialVersionUID = 3472827823632144419L;

    protected AccessibleAWTTextArea()
    {
    }
    
    public AccessibleStateSet getAccessibleStateSet()
    {
      return super.getAccessibleStateSet();
    }
  }
  
  /**
   * Gets the AccessibleContext associated with this <code>TextArea</code>.
   * The context is created, if necessary.
   *
   * @return the associated context
   */
  public AccessibleContext getAccessibleContext()
  {
    /* Create the context if this is the first request */
    if (accessibleContext == null)
      accessibleContext = new AccessibleAWTTextArea();
    return accessibleContext;
  }
}
