/* Selector.java -- A CSS selector
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


package gnu.javax.swing.text.html.css;

import java.util.Map;
import java.util.StringTokenizer;

/**
 * A CSS selector. This provides methods to interpret a selector and
 * query matches with an actual HTML element tree. 
 */
public class Selector
{

  /**
   * The actual selector. The selector tokens are stored backwards, that
   * is the last token first. This makes matching easier.
   */
  private String[] selector;

  private String[] elements;
  private String[] ids;
  private String[] classes;

  /**
   * The specificity of the selector.
   */
  private int specificity;

  /**
   * An implicit selector has true here. This is the case for CSS rules that
   * are attached to HTML elements directly via style="<CSS rule>".
   */
  private boolean implicit;

  /**
   * Creates a new Selector instance for the specified selector string.
   *
   * @param sel the selector
   */
  public Selector(String sel)
  {
    StringTokenizer selectorTokens = new StringTokenizer(sel, " ");
    selector = new String[selectorTokens.countTokens()];
    for (int i = selector.length - 1; selectorTokens.hasMoreTokens(); i--)
      {
        selector[i] = selectorTokens.nextToken();
      }
    calculateSpecificity();
  }

  /**
   * Determines if this selector matches the element path specified in the
   * arguments. The arguments hold the element names as well as class
   * and id attibutes of the HTML element to be queried. The first item
   * in the array is the deepest element and the last on the highest up (for
   * instance, the html tag).
   *
   * @param tags
   *
   * @return <code>true</code> when this selector matches the element path,
   *         <code>false</code> otherwise
   */
  public boolean matches(String[] tags, Map[] attributes)
  {
    // TODO: This implements class, id and descendent matching. These are
    // the most commonly used selector matchers in CSS together with HTML.
    // However, the CSS spec defines a couple of more sophisticated matches
    // which should be implemented.
    // http://www.w3.org/TR/CSS21/selector.html
    
    // All parts of the selector must match at some point.
    boolean match = false;
    int numTags = tags.length;
    int numSel = selector.length;
    if (numSel <= numTags)
      {
        match = true;
        int tagIndex = 0;
        for (int j = 0; j < numSel && match; j++)
          {
            boolean tagMatch = false;
            for (; tagIndex < numTags && tagMatch == false; tagIndex++)
              {
                Object pathClass = attributes[tagIndex].get("class");
                // Try pseudo class too.
                Object pseudoClass = attributes[tagIndex].get("_pseudo");
                Object dynClass = attributes[tagIndex].get("_dynamic");
                Object pathId = attributes[tagIndex].get("id");
                String tag = elements[j];
                String clazz = classes[j];
                String id = ids[j];
                tagMatch = tag.equals("") || tag.equals("*")
                           || tag.equals(tags[tagIndex]);
                tagMatch = tagMatch && (clazz.equals("*")
                                        || clazz.equals(dynClass)
                                        || clazz.equals(pseudoClass)
                                        || clazz.equals(pathClass));
                tagMatch = tagMatch && (id.equals("*")
                                        || id.equals(pathId));
                // For the last element in the selector we must not look
                // further.
                if (j == 0)
                  break;
              }
            // If we don't come out here with a matching tag, then we're
            // not matching at all.
            match = tagMatch;
          }
      }
    return match;
  }

  /**
   * Returns the specificity of the selector. This is calculated according
   * to:
   * http://www.w3.org/TR/CSS21/cascade.html#specificity
   *
   * @return the specificity of the selector
   */
  public int getSpecificity()
  {
    return specificity;
  }

  /**
   * Returns a string representation of the selector. This tries to reconstruct
   * the original selector as closely as possible.
   *
   * @return a string representation of the selector
   */
  public String toString()
  {
    StringBuilder b = new StringBuilder();
    for (int i = selector.length - 1; i >= 0; i--)
      {
        b.append(selector[i]);
        if (i > 0)
          b.append(' ');
      }
    return b.toString();
  }

  /**
   * Calculates the specificity of the selector. This is calculated according
   * to:
   * http://www.w3.org/TR/CSS21/cascade.html#specificity
   */
  private void calculateSpecificity()
  {
    int a = implicit ? 1 : 0;
    int b = 0;
    int c = 0;
    int d = 0;
    int numSel = selector.length;
    elements = new String[numSel];
    ids = new String[numSel];
    classes = new String[numSel];
    for (int i = 0; i < numSel; i++)
      {
        String sel = selector[i];
        int clazzIndex = sel.indexOf('.');
        // Try pseudo class too.
        if (clazzIndex == -1)
          clazzIndex = sel.indexOf(':');
        int idIndex = sel.indexOf('#');
        String clazz;
        if (clazzIndex == -1)
          {
            clazz = "*";
            clazzIndex = sel.length();
          }
        else
          {
            c++;
            clazz = sel.substring(clazzIndex + 1,
                                  idIndex > 0 ? Math.min(idIndex, sel.length())
                                                         : sel.length());
          }
        String id;
        if (idIndex == -1)
          {
            id = "*";
            idIndex = sel.length();
          }
        else
          {
            b++;
            id = sel.substring(idIndex + 1,
                               clazzIndex > 0 ? Math.min(clazzIndex, sel.length())
                                              : sel.length());
          }
        String tag = sel.substring(0,
                                   Math.min(Math.min(clazzIndex, idIndex),
                                            sel.length()));
        if (! tag.equals("") && ! tag.equals("*"))
          d++;

        elements[i] = tag;
        ids[i] = id;
        classes[i] = clazz;
      }
    // An order of 20 should be enough for everybody.
    specificity = a * 20 ^ 3 + b * 20 ^ 2 + c * 20 + d;
  }
}
