/* CheckboxGroup.java -- A grouping class for checkboxes.
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

As a special exception, if you link this library with other files to
produce an executable, this library does not by itself cause the
resulting executable to be covered by the GNU General Public License.
This exception does not however invalidate any other reasons why the
executable file might be covered by the GNU General Public License. */


package java.awt;

/**
  * This class if for combining checkboxes into groups so that only
  * one checkbox in the group can be selected at any one time.
  *
  * @author Aaron M. Renn (arenn@urbanophile.com)
  * @author Tom Tromey <tromey@redhat.com>
  */
public class CheckboxGroup implements java.io.Serializable
{

/*
 * Static Variables
 */

// Serialization constant
private static final long serialVersionUID = 3729780091441768983L;

/*************************************************************************/

/*
 * Instance Variables
 */

/**
  * @serial The currently selected checkbox.
  */
private Checkbox selectedCheckbox;

/*************************************************************************/

/*
 * Constructors
 */

/**
  * Initializes a new instance of <code>CheckboxGroup</code>.
  */
public
CheckboxGroup()
{
}

/*************************************************************************/

/*
 * Instance Methods
 */

/**
  * Returns the currently selected checkbox, or <code>null</code> if none
  * of the checkboxes in this group are selected.
  *
  * @return The selected checkbox.
  */
public Checkbox
getSelectedCheckbox()
{
  return(selectedCheckbox);
} 

/*************************************************************************/

/**
  * Returns the currently selected checkbox, or <code>null</code> if none
  * of the checkboxes in this group are selected.
  *
  * @return The selected checkbox.
  *
  * @deprecated This method is deprecated in favor of 
  * <code>getSelectedCheckbox()</code>.
  */
public Checkbox
getCurrent()
{
  return(selectedCheckbox);
} 

/*************************************************************************/

/**
  * This method sets the specified checkbox to be the selected on in this
  * group, and unsets all others.
  *
  * @param selectedCheckbox The new selected checkbox.
  */
public void
setSelectedCheckbox(Checkbox selectedCheckbox)
{
  if (this.selectedCheckbox != null)
    {
      if (this.selectedCheckbox.getCheckboxGroup() != this)
        return;

      this.selectedCheckbox.setState(false);
    }

  this.selectedCheckbox = selectedCheckbox;
  if (selectedCheckbox != null)
    selectedCheckbox.setState(true);
}

/*************************************************************************/

/**
  * This method sets the specified checkbox to be the selected on in this
  * group, and unsets all others.
  *
  * @param selectedCheckbox The new selected checkbox.
  *
  * @deprecated This method is deprecated in favor of
  * <code>setSelectedCheckbox()</code>.
  */
public void
setCurrent(Checkbox selectedCheckbox)
{
  setSelectedCheckbox(selectedCheckbox);
}

/*************************************************************************/

/**
  * Returns a string representation of this checkbox group.
  *
  * @return A string representation of this checkbox group.
  */
public String
toString()
{
  return(getClass().getName() + "[selectedCheckbox=" + selectedCheckbox + "]");
}

} // class CheckboxGroup 

