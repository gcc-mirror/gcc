/* SpinnerListModel.java -- A spinner model backed by a list or an array.
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

package javax.swing;

import java.io.Serializable;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

/**
 * An implementation of <code>SpinnerModel</code> which uses the values
 * contained within a list or an array.  The backing list or array is
 * only stored as a reference within the class.  As a result, changes
 * made elsewhere to the members of the list or array are reflected by
 * this model.
 * <p>
 *
 * The model itself inherits a list of <code>ChangeListener</code>s from
 * <code>AbstractSpinnerModel</code>.  As this code is unaware of changes
 * made to the backing list or array, it is the responsibility of the
 * application using the model to invoke <code>fireStateChanged()</code>,
 * in order to notify any <code>ChangeListener</code>s, when the list or array
 * changes.  The model handles notification when the reference itself
 * is changed via <code>setList()</code> or when the current value is
 * set directly using <code>setValue()</code>.
 *
 * @author Andrew John Hughes (gnu_andrew@member.fsf.org)
 * @see SpinnerModel
 * @see AbstractSpinnerModel
 * @see JSpinner
 * @since 1.4
 */

public class SpinnerListModel
    extends AbstractSpinnerModel
    implements Serializable
{
    /**
     * For compatability with Sun's JDK
     */
    private static final long serialVersionUID = 3358804052191994516L;

    /**
     * The backing list for this model.
     */
    private List list;

    /**
     * The current index in the list.
     */
    private transient int index;

    /**
     * Constructs a default <code>SpinnerListModel</code>.  This
     * is a model backed by a list containing only the single
     * <code>String</code> element, "empty".
     */
    public SpinnerListModel()
    {
	List defaultList;

	/* Create an empty list */
	defaultList = new ArrayList();
	/* Add the string "empty" */
	defaultList.add("empty");
	/* Set the list */
	setList(defaultList);
    }

    /**
     * Constructs a <code>SpinnerListModel</code> using the supplied list.
     * The model maintains a reference to this list, and returns
     * consecutive elements in response to calls to <code>getNextValue()</code>.
     * The initial value is that at position 0, so an initial call
     * to <code>getValue()</code> returns the same as <code>list.get(0)</code>.
     *
     * @param list The list to use for this model.
     * @throws IllegalArgumentException if the list is null or contains no
     *         elements.
     * @see SpinnerListModel#getNextValue()
     * @see SpinnerListModel#getValue()
     */ 
    public SpinnerListModel(List list)
    {
	/* Retain a reference to the valid list */
        setList(list);
    }

    /**
     * Constructs a <code>SpinnerListModel</code> using the supplied array.
     * The model stores a reference to the wrapper list returned by
     * <code>Arrays.asList()</code>.  The wrapper list reflects modifications
     * in the underlying array, so these changes will also be reflected
     * by the model.  The model produces consecutive elements from the array
     * in response to calls to <code>getNextValue()</code>.  The initial
     * value returned by <code>getValue()</code> is the same as
     * <code>array[0]</code>.
     *
     * @param array The array to use for this model.
     * @throws IllegalArgumentException if the array is null or contains
     *         no elements.
     * @see Arrays#asList(Object[])
     * @see SpinnerListModel#getNextValue()
     * @see SpinnerListModel#getValue()
     */
    public SpinnerListModel(Object[] array)
    {
	/* Check for a null or zero-sized array */
	if (array == null || array.length == 0)
	    {
		throw new IllegalArgumentException("The supplied array was invalid.");
	    }
	/* 
	   Retain a reference to a wrapper around the valid array 
	   The array, in list form, will be tested again here, but we can't really
	   avoid this -- a null value to Arrays.asList will throw a NullPointerException
	*/ 
	setList(Arrays.asList(array));
    }

    /**
     * Returns the backing list for this model.
     *
     * @return The backing list.
     */
    public List getList()
    {
	return list;
    }
    
    /**
     * Returns the next value from the list, which is the same as the element
     * stored at the current index + 1.  Null is returned if there are no more
     * values to be returned (the end of the list has been reached).  An
     * ambiguity can occur here, as null may also be returned as a valid list
     * element.  This operation does not change the current value.
     *
     * @return The next value from the list or null.
     */
    public Object getNextValue()
    {
	/* Check for a next value */
	if (index < (list.size() - 1))
	    {
		/* Return the element at the next index */
		return list.get(index + 1);
	    }
	else
	    {
		/* Return null as this is the end of the list */
		return null;
	    }
    }

    /**
     * Returns the previous value from the list, which is the same as the element
     * stored at the current index - 1.  Null is returned if there are no more
     * values to be returned (the start of the list has been reached).  An
     * ambiguity can occur here, as null may also be returned as a valid list
     * element.  This operation does not change the current value.
     *
     * @return The previous value from the list or null.
     */
    public Object getPreviousValue()
    {
	/* Check for a previous value. */
	if (index > 0) 
	    {
		/* Return the element at the previous position */
		return list.get(index - 1);
	    }
	else
	    {
		/* Return null as this is the start of the list */
		return null;
	    }
    }

    /**
     * Returns the current value of the model.  Initially, this will
     * be the element at position 0.  On later invocations, this will
     * be the last element returned by <code>getNextValue()</code>
     * or <code>getPreviousValue()</code>.
     *
     * @return The current value.
     * @see SpinnerListModel#getPreviousValue()
     * @see SpinnerListModel#getNextValue()
     */
    public Object getValue()
    {
	return list.get(index);
    }

    /**
     * Changes the backing list for this model.  The model only stores
     * a reference to the list, so any changes made to the list elsewhere
     * will be reflected in the values returned by the model.  A
     * <code>ChangeEvent</code> is fired if the list being used actually
     * changes (i.e. the new list is not referentially equal (!=) to the
     * old one).
     *
     * @param list The new list to use.
     * @throws IllegalArgumentException if the list is null or contains
     *         no elements.
     * @see ChangeEvent
     */
    public void setList(List list)
    {
	/* Check for null or zero size list */
	if (list == null || list.size() == 0)
	    {
		throw new IllegalArgumentException("The supplied list was invalid.");
	    }
	/* Check for a change of referenced list */
	if (this.list != list)
	    {
		/* Store the new list */
		this.list = list;
		/* Notify listeners of a change */
		fireStateChanged();
	    }
	/* We reset the other values in either case */
	/* Set the index to 0 */
	index = 0;
    }

    /**
     * Sets the current value of the model to be the one supplied.
     * The value must exist within the backing list in order for
     * the change to take place.  Otherwise, an exception is thrown.
     * The value used is the first occurrence of the value within
     * the backing list.  Listeners are notified of this change.
     * Following the change, <code>getNextValue()</code> and
     * <code>getPreviousValue()</code> return the objects following
     * and prior to the supplied value, respectively. 
     *
     * @param value The requested new value of the list.
     * @throws IllegalArgumentException if the supplied value does
     *         not exist in the backing list.
     * @see SpinnerListModel#getPreviousValue()
     * @see SpinnerListModel#getNextValue()
     */
    public void setValue(Object value)
    {
	int valueIndex;

	/* Search for the value in the list */
	valueIndex = list.indexOf(value);
	/* Check for the value being found */
	if (valueIndex == -1)
	    {
		throw new IllegalArgumentException("The supplied value does not "
						   + "exist in this list");
	    }
	/* Make the indices match */
	index = valueIndex;
	/* Notify the listeners */
	fireStateChanged();
    }

}
