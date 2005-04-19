/* parameterDefaulter.java --
   Copyright (C) 2005 Free Software Foundation, Inc.

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


package gnu.javax.swing.text.html.parser.support;

import gnu.javax.swing.text.html.parser.htmlAttributeSet;

import java.util.Hashtable;

import javax.swing.text.html.parser.AttributeList;
import javax.swing.text.html.parser.DTD;
import javax.swing.text.html.parser.Element;

/**
 * Returns an attribute set, containing default
 * parameters for the given element. Caches sets of default
 * parameters.
 * @author Audrius Meskauskas, Lithuania (AudriusA@Bioinformatics.org)
 */
public class parameterDefaulter
{
  public final DTD dtd;
  Hashtable sets = new Hashtable();

  /**
   * Create a parameterDefaulter that looks for the default attribute
   * values in the given DTD.
   * @param a_dtd
   */
  public parameterDefaulter(DTD a_dtd)
  {
    dtd = a_dtd;
  }

  /**
   * Get the default parameter set for the given element.
   * @param element The element name (case insensitive).
   * @return the default attrbute set.
   */
  public htmlAttributeSet getDefaultParameters(String element)
  {
    String key = element.toLowerCase();
    htmlAttributeSet atts = (htmlAttributeSet) sets.get(key);

    if (atts == null)
      {
        htmlAttributeSet set = new htmlAttributeSet();
        Element e = (Element) dtd.elementHash.get(element.toLowerCase());

        if (e != null)
          {
            AttributeList a = e.getAttributes();

            while (a != null)
              {
                if (a.value != null)
                  set.addAttribute(a.name, a.value);
                a = a.next;
              }
          }

        if (set.getAttributeCount() > 0)
          sets.put(key, set);
        else
          sets.put(key, htmlAttributeSet.EMPTY_HTML_ATTRIBUTE_SET);

        atts = set;
      }
    return atts;
  }
}
