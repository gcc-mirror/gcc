/* Label.java -- Java label widget
   Copyright (C) 1999, 2000, 2002 Free Software Foundation, Inc.

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

import java.awt.peer.LabelPeer;
import javax.accessibility.Accessible;

/**
  * This component is used for displaying simple text strings that cannot
  * be edited.
  *
  * @author Aaron M. Renn (arenn@urbanophile.com)
  * @author Tom Tromey <tromey@cygnus.com>
  */
public class Label extends Component implements Accessible
{

/*
 * Static Variables
 */

/**
  * Alignment constant aligning the text to the left of its window.
  */
public static final int LEFT = 0;

/**
  * Alignment constant aligning the text in the center of its window.
  */
public static final int CENTER = 1;

/**
  * Alignment constant aligning the text to the right of its window.
  */
public static final int RIGHT = 2;

// Serialization version constant:
private static final long serialVersionUID = 3094126758329070636L;

/*************************************************************************/

/*
 * Instance Variables
 */

/**
  * @serial Indicates the alignment of the text within this label's window.
  * This is one of the constants in this class.  The default value is 
  * <code>LEFT</code>.
  */
private int alignment;

/**
  * @serial The text displayed in the label
  */
private String text;

/*************************************************************************/

/*
 * Constructors
 */

/**
  * Initializes a new instance of <code>Label</code> with no text.
  *
  * @exception HeadlessException If GraphicsEnvironment.isHeadless() is true.
  */
public
Label()
{
  this("", LEFT);
}

/*************************************************************************/

/**
  * Initializes a new instance of <code>Label</code> with the specified
  * text that is aligned to the left.
  *
  * @param text The text of the label.
  *
  * @exception HeadlessException If GraphicsEnvironment.isHeadless() is true.
  */
public
Label(String text)
{
  this(text, LEFT);
}

/*************************************************************************/

/**
  * Initializes a new instance of <code>Label</code> with the specified
  * text and alignment.
  *
  * @param text The text of the label.
  * @param alignment The desired alignment for the text in this label,
  * which must be one of <code>LEFT</code>, <code>CENTER</code>, or
  * <code>RIGHT</code>.
  *
  * @exception HeadlessException If GraphicsEnvironment.isHeadless() is true.
  */
public
Label(String text, int alignment)
{
  setAlignment (alignment);
  setText (text);

  if (GraphicsEnvironment.isHeadless())
    throw new HeadlessException ();
}

/*************************************************************************/

/*
 * Instance Variables
 */

/**
  * Returns the constant indicating the alignment of the text in this
  * label.  The value returned will be one of the alignment constants
  * from this class.
  *
  * @return The alignment of the text in the label.
  */
public int
getAlignment()
{
  return(alignment);
}

/*************************************************************************/

/**
  * Sets the text alignment of this label to the specified value.
  *
  * @param alignment The desired alignment for the text in this label,
  * which must be one of <code>LEFT</code>, <code>CENTER</code>, or
  * <code>RIGHT</code>.
  */
public synchronized void
setAlignment(int alignment)
{
  if (alignment != CENTER && alignment != LEFT && alignment != RIGHT)
    throw new IllegalArgumentException ("invalid alignment: " + alignment);
  this.alignment = alignment;
  if (peer != null)
    {
      LabelPeer lp = (LabelPeer) peer;
      lp.setAlignment (alignment);
    }
}

/*************************************************************************/

/**
  * Returns the text displayed in this label.
  *
  * @return The text for this label.
  */
public String
getText()
{
  return(text);
}

/*************************************************************************/

/**
  * Sets the text in this label to the specified value.
  *
  * @param text The new text for this label.
  */
public synchronized void
setText(String text)
{
  this.text = text;

  if (peer != null)
    {
      LabelPeer lp = (LabelPeer) peer;
      lp.setText (text);
    }
}

/*************************************************************************/

/**
  * Notifies this label that it has been added to a container, causing
  * the peer to be created.  This method is called internally by the AWT
  * system.
  */
public void
addNotify()
{
  if (peer == null)
    peer = getToolkit ().createLabel (this);
  super.addNotify ();
}

/*************************************************************************/

/**
  * Returns a parameter string useful for debugging.
  *
  * @param A debugging string.
  */
protected String
paramString()
{
  return ("text=" + getText() + ",alignment=" +
	  getAlignment() + "," + super.paramString());
}

} // class Label

