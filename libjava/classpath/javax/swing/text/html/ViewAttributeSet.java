/* ViewAttributeSet.java -- The AttributeSet used by HTML views
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

import java.util.ArrayList;
import java.util.Enumeration;

import javax.swing.text.AttributeSet;
import javax.swing.text.Element;
import javax.swing.text.StyleConstants;
import javax.swing.text.View;

/**
 * An AttributeSet implemenation that is used by the HTML views. This
 * AttributeSet is created by StyleSheet.getViewAttributes() and combines
 * the following attributes:
 * - The original attributes of the View's element.
 * - Any translated (HTML->CSS) attributes, as returned by
 *   StyleSheet.translateHTMLToCS().
 * - CSS Styles as resolved by the CSS stylesheet.
 *
 * In addition to that, it resolves attributes to the parent views, if
 * a CSS attribute is requested that is inheritable.
 *
 * @author Roman Kennke (kennke@aicas.com)
 */
class ViewAttributeSet
  extends MultiAttributeSet
{

  /**
   * The view for which we are the AttributeSet.
   */
  private View view;

  /**
   * The stylesheet to use.
   */
  private StyleSheet styleSheet;

  /**
   * Creates a new instance.
   *
   * @param v the view for which to do the AttributeSet
   */
  ViewAttributeSet(View v, StyleSheet ss)
  {
    styleSheet = ss;
    view = v;
    ArrayList<AttributeSet> atts = new ArrayList<AttributeSet>();

    Element el = v.getElement();
    AttributeSet elAtts = el.getAttributes();
    AttributeSet htmlAtts = styleSheet.translateHTMLToCSS(elAtts);
    if (htmlAtts.getAttributeCount() > 0)
      atts.add(htmlAtts);

    if (el.isLeaf())
      {
        Enumeration<?> n = elAtts.getAttributeNames();
        while (n.hasMoreElements())
          {
            Object key = n.nextElement();
            if (key instanceof HTML.Tag)
              {
                AttributeSet rule = styleSheet.getRule((HTML.Tag) key, el);
                if (rule != null)
                  atts.add(rule);
              }
          }
      }
    else
      {
        HTML.Tag tag =
          (HTML.Tag) elAtts.getAttribute(StyleConstants.NameAttribute);
        AttributeSet rule = styleSheet.getRule(tag, el); 
        if (rule != null)
          atts.add(rule);
      }

    AttributeSet[] atts1 = new AttributeSet[atts.size()];
    atts1 = atts.toArray(atts1);
    init(atts1);
  }

  /**
   * Fetches the attribute for the specific ckey. If the attribute
   * can't be found and the key is a CSS.Attribute that is inherited,
   * then the attribute is looked up in the resolve parent.
   */
  public Object getAttribute(Object key)
  {
    Object val = super.getAttribute(key);
    if (val == null)
      {
        // Didn't find value. If the key is a CSS.Attribute, and is
        // inherited, then ask the resolve parent.
        if (key instanceof CSS.Attribute)
          {
            CSS.Attribute cssKey = (CSS.Attribute) key;
            if (cssKey.isInherited())
              {
                AttributeSet resolveParent = getResolveParent();
                if (resolveParent != null)
                  val = resolveParent.getAttribute(cssKey);
              }
          }
      }
    return val;
  }

  /**
   * Returns the resolve parent of this AttributeSet. This is the AttributeSet
   * returned by the parent view if available.
   */
  public AttributeSet getResolveParent()
  {
    AttributeSet parent = null;
    if (view != null)
      {
        View parentView = view.getParent();
        if (parentView != null)
          parent = parentView.getAttributes();
      }
    return parent;
  }
}
