/* ImplementationSource.java -- 
   Copyright (C) 2004 Free Software Foundation, Inc..

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
package gnu.xml.dom;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Iterator;
import java.util.List;

import org.w3c.dom.DOMImplementation;
import org.w3c.dom.DOMImplementationList;
import org.w3c.dom.DOMImplementationSource;

/**
 * Implementation source for GNU JAXP.
 *
 * @author <a href='mailto:dog@gnu.org'>Chris Burdess</a>
 */
public class ImplementationSource
  implements DOMImplementationSource
{

  private static final String DIGITS = "1234567890";

  /*
   * GNU DOM implementations.
   */
  private static final DOMImplementation[] implementations;

  static
  {
    List acc = new ArrayList();
    acc.add(new gnu.xml.dom.DomImpl());
    try
      {
        Class t = Class.forName("gnu.xml.libxmlj.dom.GnomeDocumentBuilder");
        acc.add(t.newInstance());
      }
    catch (Exception e)
      {
        // libxmlj not available        
      }
    catch (UnsatisfiedLinkError e)
      {
        // libxmlj not available
      }
    implementations = new DOMImplementation[acc.size()];
    acc.toArray(implementations);
  }

  public DOMImplementation getDOMImplementation(String features)
  {
    List available = getImplementations(features);
    if (available.isEmpty())
      {
        return null;
      }
    return (DOMImplementation) available.get(0);
  }

  public DOMImplementationList getDOMImplementationList(String features)
  {
    List available = getImplementations(features);
    return new ImplementationList(available);
  }

  /**
   * Returns a list of the implementations that support the specified
   * features.
   */
  private final List getImplementations(String features)
  {
    List available = new ArrayList(Arrays.asList(implementations));
    for (Iterator i = parseFeatures(features).iterator(); i.hasNext(); )
      {
        String feature = (String) i.next();
        String version = null;
        int si = feature.indexOf(' ');
        if (si != -1)
          {
            version = feature.substring(si + 1);
            feature = feature.substring(0, si);
          }
        for (Iterator j = available.iterator(); j.hasNext(); )
          {
            DOMImplementation impl = (DOMImplementation) j.next();
            if (!impl.hasFeature(feature, version))
              {
                j.remove();
              }
          }
      }
    return available;
  }

  /**
   * Parses the feature list into feature tokens.
   */
  final List parseFeatures(String features)
  {
    List list = new ArrayList();
    int pos = 0, start = 0;
    int len = features.length();
    for (; pos < len; pos++)
      {
        char c = features.charAt(pos);
        if (c == ' ')
          {
            if (pos + 1 < len &&
                DIGITS.indexOf(features.charAt(pos + 1)) == -1)
              {
                list.add(getFeature(features, start, pos));
                start = pos + 1;
              }
          }
      }
    if (pos > start)
      {
        list.add(getFeature(features, start, len));
      }
    return list;
  }

  final String getFeature(String features, int start, int end)
  {
    if (features.length() > 0 && features.charAt(start) == '+')
      {
        return features.substring(start + 1, end);
      }
    return features.substring(start, end);
  }

}
