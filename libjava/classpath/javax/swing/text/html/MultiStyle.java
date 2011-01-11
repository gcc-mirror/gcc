/* MultiStyle.java -- Multiplexes between several Styles
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

import java.util.Enumeration;

import javax.swing.event.ChangeListener;
import javax.swing.text.AttributeSet;
import javax.swing.text.SimpleAttributeSet;
import javax.swing.text.Style;

/**
 * A Style implementation that is able to multiplex between several other
 * Styles. This is used for CSS style resolving.
 *
 * @author Roman Kennke (kennke@aicas.com)
 */
class MultiStyle
  extends MultiAttributeSet
  implements Style
{

  // FIXME: Fix the implementation to also return attributes that
  // are added to this style, etc. However, this is not really needed
  // now for CSS, but would be nice for correctness.

  /**
   * The name of the style.
   */
  private String name;

  /**
   * The attributes added to this style.
   */
  private SimpleAttributeSet attributes;

  /**
   * Creates a new instance.
   *
   * @param n the name
   * @param m the styles to multiplex
   */
  public MultiStyle(String n, AttributeSet[] m)
  {
    super(m);
    name = n;
    attributes = new SimpleAttributeSet();
  }

  /**
   * Returns the name of the style.
   *
   * @return the name of the style
   */
  public String getName()
  {
    return name;
  }

  public void addChangeListener(ChangeListener listener)
  {
    // TODO: Implement.
  }

  public void removeChangeListener(ChangeListener listener)
  {
    // TODO: Implement.
  }

  public void addAttribute(Object name, Object value)
  {
    attributes.addAttribute(name, value);
  }

  public void addAttributes(AttributeSet atts)
  {
    attributes.addAttributes(atts);
  }

  public void removeAttribute(Object name)
  {
    attributes.removeAttribute(name);
  }

  public void removeAttributes(Enumeration<?> names)
  {
    attributes.removeAttribute(names);
  }

  public void removeAttributes(AttributeSet atts)
  {
    attributes.removeAttribute(atts);
  }

  public void setResolveParent(AttributeSet parent)
  {
    // TODO: Implement.
  }

}
