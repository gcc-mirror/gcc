/* NameTest.java --
   Copyright (C) 2004,2006 Free Software Foundation, Inc.

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

package gnu.xml.xpath;

import javax.xml.XMLConstants;
import javax.xml.namespace.QName;
import org.w3c.dom.Node;

/**
 * Tests whether a node has the specified name.
 *
 * @author <a href='mailto:dog@gnu.org'>Chris Burdess</a>
 */
public final class NameTest
  extends Test
{

  final QName qName;
  final boolean anyLocalName;
  final boolean any;

  public NameTest(QName qName, boolean anyLocalName, boolean any)
  {
    this.anyLocalName = anyLocalName;
    this.any = any;
    this.qName = qName;
  }

  public boolean matchesAny()
  {
    return any;
  }

  public boolean matchesAnyLocalName()
  {
    return anyLocalName;
  }

  public boolean matches(Node node, int pos, int len)
  {
    switch (node.getNodeType())
      {
      case Node.ATTRIBUTE_NODE:
        // Do not match namespace attributes
        String uri = node.getNamespaceURI();
        if (XMLConstants.XMLNS_ATTRIBUTE_NS_URI.equals(uri) ||
            XMLConstants.XMLNS_ATTRIBUTE.equals(node.getPrefix()) ||
            XMLConstants.XMLNS_ATTRIBUTE.equals(node.getNodeName()))
          {
            return false;
          }
        // Fall through
      case Node.ELEMENT_NODE:
        break;
      default:
        return false;
      }
    if (any)
      return true;
    String uri = qName.getNamespaceURI();
    String nodeUri = node.getNamespaceURI();
    if (!equal(uri, nodeUri))
      return false;
    if (anyLocalName)
      return true;
    String localName = qName.getLocalPart();
    String nodeLocalName = getLocalName(node);
    return (localName.equals(nodeLocalName));
  }

  static String getLocalName(Node node)
  {
    String localName = node.getLocalName();
    if (localName == null)
      {
        localName = node.getNodeName();
        int ci = localName.indexOf(':');
        if (ci != -1)
          localName = localName.substring(ci + 1);
      }
    return localName;
  }

  static boolean equal(String s1, String s2)
  {
    return (((s1 == null || s1.length() == 0) &&
             (s2 == null || s2.length() == 0)) ||
            s1 != null && s1.equals(s2));
  }

  public Test clone(Object context)
  {
    return new NameTest(qName, anyLocalName, any);
  }

  public boolean references(QName var)
  {
    return false;
  }

  public String toString ()
  {
    if (any)
      return "*";
    return qName.toString();
  }

}
