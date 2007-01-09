/* Option.java -- Value class for <option> list model elements
   Copyright (C) 2006 Free Software Foundation, Inc.

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


package javax.swing.text.html;

import javax.swing.text.AttributeSet;

/**
 * Value class for the combobox model that renders <code>&lt;option&gt;</code>
 * elements.
 *
 * @author Roman Kennke (kennke@aicas.com)
 */
public class Option
{

  /**
   * The attributes of the <option> tag.
   */
  private AttributeSet attributes;

  /**
   * The label.
   */
  private String label;

  /**
   * The selected state of this label.
   */
  private boolean selected;

  /**
   * Creates a new <code>Option</code> instance that uses the specified
   * tag attributes.
   *
   * @param attr the attributes to use
   */
  public Option(AttributeSet attr)
  {
    // Protect the attribute set.
    attributes = attr.copyAttributes();
    label = null;
    selected = attr.getAttribute(HTML.Attribute.SELECTED) != null;
  }

  /**
   * Sets the label to use for this <code>&lt;option&gt;</code> tag.
   *
   * @param l the label to set
   */
  public void setLabel(String l)
  {
    label = l;
  }

  /**
   * Returns the label of this <code>&lt;option&gt;</code> tag.
   *
   * @return the label of this <code>&lt;option&gt;</code> tag
   */
  public String getLabel()
  {
    return label;
  }

  /**
   * Returns the attributes used to render this <code>&lt;option&gt;</code>
   * tag.
   *
   * @return the attributes used to render this <code>&lt;option&gt;</code> tag
   */
  public AttributeSet getAttributes()
  {
    return attributes;
  }

  /**
   * Returns a string representation of this <code>&lt;option&gt;</code> tag.
   * This returns the <code>label</code> property.
   *
   * @return a string representation of this <code>&lt;option&gt;</code> tag
   */
  public String toString()
  {
    return label;
  }

  /**
   * Sets the selected state of this <code>&lt;option&gt;</code> tag.
   *
   * @param s the selected state to set
   */
  protected void setSelection(boolean s)
  {
    selected = s;
  }

  /**
   * Returns <code>true</code> when this option is selected, <code>false</code>
   * otherwise.
   *
   * @return <code>true</code> when this option is selected, <code>false</code>
   *         otherwise
   */
  public boolean isSelected()
  {
    return selected;
  }

  /**
   * Returns the string associated with the <code>value</code> attribute or
   * the label, if no such attribute is specified.
   *
   * @return the string associated with the <code>value</code> attribute or
   *         the label, if no such attribute is specified
   */
  public String getValue()
  {
    String value = (String) attributes.getAttribute(HTML.Attribute.VALUE);
    if (value == null)
      value = label;
    return value;
  }
}
