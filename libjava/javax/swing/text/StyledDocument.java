/* StyledDcoument.java --
   Copyright (C) 2002 Free Software Foundation, Inc.

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

package javax.swing.text;

// Imports
import java.awt.*;

/**
 * StyledDocument
 * @author	Andrew Selkirk
 * @version	1.0
 */
public interface StyledDocument extends Document {

	//-------------------------------------------------------------
	// Methods ----------------------------------------------------
	//-------------------------------------------------------------

	/**
	 * addStyle
	 * @param nm TODO
	 * @param rent TODO
	 * @returns Style
	 */
	public Style addStyle(String nm, Style parent);

	/**
	 * removeStyle
	 * @param nm TODO
	 */
	public void removeStyle(String nm);

	/**
	 * getStyle
	 * @param nm TODO
	 * @returns Style
	 */
	public Style getStyle(String nm);

	/**
	 * setCharacterAttributes
	 * @param offset TODO
	 * @param length TODO
	 * @param set TODO
	 * @param replace TODO
	 */
	public void setCharacterAttributes(int offset, int length,
		AttributeSet set, boolean replace);

	/**
	 * setParagraphAttributes
	 * @param offset TODO
	 * @param length TODO
	 * @param set TODO
	 * @param replace TODO
	 */
	public void setParagraphAttributes(int offset, int length,
		AttributeSet set, boolean replace);

	/**
	 * getLogicalStyle
	 * @param position TODO
	 * @returns Style
	 */
	public Style getLogicalStyle(int position);

	/**
	 * setLogicalStyle
	 * @param position TODO
	 * @param style TODO
	 */
	public void setLogicalStyle(int position, Style style);

	/**
	 * getParagraphElement
	 * @param position TODO
	 * @returns Element
	 */
	public abstract Element getParagraphElement(int position);

	/**
	 * getCharacterElement
	 * @param position TODO
	 * @returns Element
	 */
	public Element getCharacterElement(int position);

	/**
	 * getForeground
	 * @param set TODO
	 * @returns Color
	 */
	public Color getForeground(AttributeSet set);

	/**
	 * getBackground
	 * @param set TODO
	 * @returns Color
	 */
	public Color getBackground(AttributeSet set);

	/**
	 * getFont
	 * @param set TODO
	 * @returns Font
	 */
	public Font getFont(AttributeSet set);


} // StyledDocument
