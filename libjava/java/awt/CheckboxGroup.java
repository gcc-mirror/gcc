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

