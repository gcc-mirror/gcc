/* TextArea.java -- A multi-line text entry widget
   Copyright (C) 1999 Free Software Foundation, Inc.

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


package java.awt;

import java.awt.peer.TextAreaPeer;
import java.awt.peer.TextComponentPeer;
import java.awt.peer.ComponentPeer;

/**
  * This implements a multi-line text entry widget.
  *
  * @author Aaron M. Renn (arenn@urbanophile.com)
  */
public class TextArea extends TextComponent implements java.io.Serializable
{

/*
 * Static Variables
 */

/**
  * Use both horiztonal and vertical scroll bars.
  */
public static final int SCROLLBARS_BOTH = 0;

/**
  * Use vertical scroll bars only.
  */
public static final int SCROLLBARS_VERTICAL_ONLY = 1;

/**
  * Use horizatonal scroll bars only.
  */
public static final int SCROLLBARS_HORIZONTAL_ONLY = 2;

/**
  * Use no scrollbars.
  */
public static final int SCROLLBARS_NONE = 3;

// Serialization constant
private static final long serialVersionUID = 3692302836626095722L;

/*************************************************************************/

/*
 * Instance Variables
 */

/**
  * @serial The number of columns in this text area.
  */
private int columns;

/**
  * @serial The number of rows in this text area.
  */
private int rows;

/**
  * @serial The type of scrollbars to display, which will be one of
  * the contstants from this class.
  */
private int scrollbarVisibility;

/*************************************************************************/

/*
 * Constructors
 */

/**
  * Initialize a new instance of <code>TextArea</code> that is empty
  * and is one row and one column.  Both horizontal and vertical
  * scrollbars will be used.
  */
public
TextArea()
{
  this("", 1, 1, SCROLLBARS_BOTH);
}

/*************************************************************************/

/**
  * Initializes a new instance of <code>TextArea</code> that 
  * contains the specified string.  Both horizontal and veritcal
  * scrollbars will be used.
  *
  * @param text The text to display in this text area.
  */
public
TextArea(String text)
{
  this(text, 1, text.length(), SCROLLBARS_BOTH);
}

/*************************************************************************/

/**
  * Initializes a new instance of <code>TextArea</code> that is empty
  * and has the specified number of rows and columns.  Both
  * horizontal and vertical scrollbars will be used.
  *
  * @param rows The number of rows in this text area.
  * @param columns The number of columns in this text area.
  */
public
TextArea(int rows, int columns)
{
  this("", rows, columns, SCROLLBARS_BOTH);
}

/*************************************************************************/

/**
  * Initializes a new instance of <code>TextArea</code> that is the
  * specified size and has the specified text.
  *
  * @param text The text to display in this text area.
  * @param rows The number of rows in this text area.
  * @param columns The number of columns in this text area.
  */
public
TextArea(String text, int rows, int columns)
{
  this(text, rows, columns, SCROLLBARS_BOTH);
}

/*************************************************************************/

/** 
  * Initializes a new instance of <code>TextArea</code> with the
  * specified values.  The scrollbar visibility value must be one
  * of the constants in this class.
  *
  * @param text The text to display in this text area.
  * @param rows The number of rows in this text area.
  * @param columns The number of columns in this text area.
  * @param scrollbarVisibility Which scrollbars to display.
  */
public
TextArea(String text, int rows, int columns, int scrollbarVisibility)
{
  super(text);

  if ((rows < 1) || (columns < 0))
    throw new IllegalArgumentException("Bad row or column value");

  if ((scrollbarVisibility != SCROLLBARS_BOTH) &&
      (scrollbarVisibility != SCROLLBARS_VERTICAL_ONLY) &&
      (scrollbarVisibility != SCROLLBARS_HORIZONTAL_ONLY) &&
      (scrollbarVisibility != SCROLLBARS_NONE))
    throw new IllegalArgumentException("Bad scrollbar visibility value");

  this.rows = rows;
  this.columns = columns;
  this.scrollbarVisibility = scrollbarVisibility;
}

/*************************************************************************/

/*
 * Instance Variables
 */

/**
  * Returns the number of columns in the field.
  *
  * @return The number of columns in the field.
  */
public int
getColumns()
{
  return(columns);
}

/*************************************************************************/

/**
  * Sets the number of columns in this field to the specified value.
  *
  * @param columns The new number of columns in the field.
  *
  * @exception IllegalArgumentException If columns is less than zero.
  */
public synchronized void
setColumns(int columns)
{
  if (columns < 0)
    throw new IllegalArgumentException("Value is less than zero: " +
                                       columns);

  this.columns = columns;
  // FIXME: How to we communicate this to our peer?
}

/*************************************************************************/

/**
  * Returns the number of rows in the field.
  *
  * @return The number of rows in the field.
  */
public int
getRows()
{
  return(rows);
}

/*************************************************************************/

/**
  * Sets the number of rows in this field to the specified value.
  *
  * @param rows The new number of rows in the field.
  *
  * @exception IllegalArgumentException If rows is less than zero.
  */
public synchronized void
setRows(int rows)
{
  if (rows < 1)
    throw new IllegalArgumentException("Value is less than one: " +
                                       rows);

  this.rows = rows;
  // FIXME: How to we communicate this to our peer?
}

/*************************************************************************/

/**
  * Returns the minimum size for this text field.
  *
  * @return The minimum size for this text field.
  */
public Dimension
getMinimumSize()
{
  return(getMinimumSize(getRows(), getColumns()));
}

/*************************************************************************/

/**
  * Returns the minimum size of a text field with the specified number
  * of rows and columns.
  *
  * @param rows The number of rows to get the minimum size for.
  * @param columns The number of columns to get the minimum size for.
  */
public Dimension
getMinimumSize(int rows, int columns)
{
  TextAreaPeer tap = (TextAreaPeer)getPeer();
  if (tap == null)
    return(null); // FIXME: What do we do if there is no peer?

  return(tap.getMinimumSize(rows, columns));
}

/*************************************************************************/

/**
  * Returns the minimum size for this text field.
  *
  * @return The minimum size for this text field.
  *
  * @deprecated This method is depcreated in favor of
  * <code>getMinimumSize()</code>.
  */
public Dimension
minimumSize()
{
  return(getMinimumSize(getRows(), getColumns()));
}

/*************************************************************************/

/**
  * Returns the minimum size of a text field with the specified number
  * of rows and columns.
  *
  * @param rows The number of rows to get the minimum size for.
  * @param columns The number of columns to get the minimum size for.
  *
  * @deprecated This method is deprecated in favor of 
  * <code>getMinimumSize(int)</code>.
  */
public Dimension
minimumSize(int rows, int columns)
{
  return(getMinimumSize(rows, columns));
}

/*************************************************************************/

/**
  * Returns the preferred size for this text field.
  *
  * @return The preferred size for this text field.
  */
public Dimension
getPreferredSize()
{
  return(getPreferredSize(getRows(), getColumns()));
}

/*************************************************************************/

/**
  * Returns the preferred size of a text field with the specified number
  * of rows and columns.
  *
  * @param rows The number of rows to get the preferred size for.
  * @param columns The number of columns to get the preferred size for.
  */
public Dimension
getPreferredSize(int rows, int columns)
{
  TextAreaPeer tap = (TextAreaPeer)getPeer();
  if (tap == null)
    return(null); // FIXME: What do we do if there is no peer?

  return(tap.getPreferredSize(rows, columns));
}

/*************************************************************************/

/**
  * Returns the preferred size for this text field.
  *
  * @return The preferred size for this text field.
  *
  * @deprecated This method is deprecated in favor of 
  * <code>getPreferredSize()</code>.
  */
public Dimension
preferredSize()
{
  return(getPreferredSize(getRows(), getColumns()));
}

/*************************************************************************/

/**
  * Returns the preferred size of a text field with the specified number
  * of rows and columns.
  *
  * @param rows The number of rows to get the preferred size for.
  * @param columns The number of columns to get the preferred size for.
  *
  * @deprecated This method is deprecated in favor of 
  * <code>getPreferredSize(int)</code>.
  */
public Dimension
preferredSize(int columns)
{
  return(getPreferredSize(rows, columns));
}

/*************************************************************************/

/**
  * Returns one of the constants from this class indicating which
  * types of scrollbars this object uses, if any.
  *
  * @return The scrollbar type constant for this object.
  */
public int
getScrollbarVisibility()
{
  return(scrollbarVisibility);
}

/*************************************************************************/

/**
  * Notify this object that it should create its native peer.
  */
public void
addNotify()
{
  if (getPeer() != null)
    return;

  setPeer((ComponentPeer)getToolkit().createTextArea(this));
}

/*************************************************************************/

/**
  * Appends the specified text to the end of the current text.
  *
  * @param text The text to append.
  */
public void
append(String str)
{
  TextAreaPeer tap = (TextAreaPeer)getPeer();
  if (tap == null) 
    return;

  tap.insert(str, tap.getText().length());
}

/*************************************************************************/

/**
  * Appends the specified text to the end of the current text.
  *
  * @param text The text to append.
  *
  * @deprecated This method is deprecated in favor of 
  * <code>append()</code>.
  */
public void
appendText(String text)
{
  append(text);
}
 
/*************************************************************************/

/**
  * Inserts the specified text at the specified location.
  *
  * @param text The text to insert.
  * @param pos The insert position.
  */
public void
insert(String text, int pos)
{
  TextAreaPeer tap = (TextAreaPeer)getPeer();
  if (tap == null)
    return;

  tap.insert(text, pos);
}

/*************************************************************************/

/**
  * Inserts the specified text at the specified location.
  *
  * @param text The text to insert.
  * @param pos The insert position.
  *
  * @deprecated This method is depcreated in favor of <code>insert()</code>.
  */
public void
insertText(String text, int pos)
{
  insert(text, pos);
}

/*************************************************************************/

/**
  * Replaces the text bounded by the specified start and end positions
  * with the specified text.
  *
  * @param text The new text for the range.
  * @param start The start position of the replacement range.
  * @param end The end position of the replacement range.
  */
public void
replaceRange(String text, int start, int end)
{
  TextAreaPeer tap = (TextAreaPeer)getPeer();
  if (tap == null)
    return;

  tap.replaceRange(text, start, end);
}

/*************************************************************************/

/**
  * Replaces the text bounded by the specified start and end positions
  * with the specified text.
  *
  * @param text The new text for the range.
  * @param start The start position of the replacement range.
  * @param end The end position of the replacement range.
  *
  * @deprecated This method is deprecated in favor of
  * <code>replaceRange()</code>.
  */
public void
replaceText(String text, int start, int end)
{
  replaceRange(text, start, end);
}

/*************************************************************************/

/**
  * Returns a debugging string for this text area.
  *
  * @return A debugging string for this text area.
  */
protected String
paramString()
{
  return(getClass().getName() + "(rows=" + getRows() + ",columns=" +
         getColumns() + ",scrollbars=" + getScrollbarVisibility() +
         ")");
}

} // class TextArea 

