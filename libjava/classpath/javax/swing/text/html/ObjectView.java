/* ObjectView.java -- A view for HTML object tags
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

import java.awt.Component;

import javax.swing.text.AttributeSet;
import javax.swing.text.ComponentView;
import javax.swing.text.Element;

/**
 * A view for HTML <code>&lt;object&gt;</code> tags.
 *
 * This is a {@link ComponentView} that creates special components depending
 * on the object specification. If the object tag has a classid attribute, then
 * this view will try to load the class specified by this attribute using the
 * classloader that loaded the associated document. If the class could be
 * loaded, an instance is created and the type narrowed to {@link Component}.
 *
 * It is also possible to set bean properties on the created component using
 * nested <code>&lt;param&gt;</code> tags. For example:
 * <pre>
 * <object classid="javax.swing.JLabel">
 *   <param name="text" value="sample text">
 * </object>
 * </pre>
 *
 * @author Roman Kennke (kennke@aicas.com)
 */
public class ObjectView extends ComponentView
{

  /**
   * Creates a new <code>ObjectView</code>.
   *
   * @param el the element for which to create a view
   */
  public ObjectView(Element el)
  {
    super(el);
  }

  /**
   * Creates a component based on the specification in the element of this
   * view. See the class description for details.
   */
  protected Component createComponent()
  {
    Component comp = null;
    Element el = getElement();
    AttributeSet atts = el.getAttributes();
    String classId = (String) atts.getAttribute("classid");
    try
      {
        Class<?> objectClass = Class.forName(classId);
        Object instance = objectClass.newInstance();
        comp = (Component) instance;
      }
    catch (ClassNotFoundException ex)
      {
        // Ignored.
      }
    catch (IllegalAccessException ex)
      {
        // Ignored.
      }
    catch (InstantiationException ex)
      {
        // Ignored.
      }
    // FIXME: Handle param tags and set bean properties accordingly.
    return comp;
  }
}
