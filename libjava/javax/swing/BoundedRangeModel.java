/* BoundedRangeModel.java --
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

package javax.swing;

// Imports
import javax.swing.event.*;

/**
 * BoundedRangeModel
 * @author	Andrew Selkirk
 * @version	1.0
 */
public interface BoundedRangeModel {

	//-------------------------------------------------------------
	// Methods ----------------------------------------------------
	//-------------------------------------------------------------

	/**
	 * getValue
	 * @returns int
	 */
	public int getValue();

	/**
	 * setValue
	 * @param value TODO
	 */
	public void setValue(int value);

	/**
	 * getMinimum
	 * @returns int
	 */
	public int getMinimum();

	/**
	 * setMinimum
	 * @param minimum TODO
	 */
	public void setMinimum(int minimum);

	/**
	 * getMaximum
	 * @returns int
	 */
	public int getMaximum();

	/**
	 * setMaximum
	 * @param maximum TODO
	 */
	public void setMaximum(int maximum);

	/**
	 * getValueIsAdjusting
	 * @returns boolean
	 */
	public boolean getValueIsAdjusting();

	/**
	 * setValueIsAdjusting
	 * @param adjusting TODO
	 */
	public void setValueIsAdjusting(boolean adjusting);

	/**
	 * getExtent
	 * @returns int
	 */
	public int getExtent();

	/**
	 * setExtent
	 * @param extent TODO
	 */
	public void setExtent(int extent);

	/**
	 * setRangeProperties
	 * @param value TODO
	 * @param extent TODO
	 * @param min TODO
	 * @param max TODO
	 * @param adjusting TODO
	 */
	public void setRangeProperties(int value, int extent, int min, 
					int max, boolean adjusting);

	/**
	 * addChangeListener
	 * @param listener TODO
	 */
	public void addChangeListener(ChangeListener listener);

	/**
	 * removeChangeListener
	 * @param listener TODO
	 */
	public void removeChangeListener(ChangeListener listener);


} // BoundedRangeModel
