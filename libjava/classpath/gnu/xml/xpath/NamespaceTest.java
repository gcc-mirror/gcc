/* NamespaceTest.java --
   Copyright (C) 2004 Free Software Foundation, Inc.

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
 * Tests whether a namespace attribute has the specified name.
 *
 * @author <a href='mailto:dog@gnu.org'>Chris Burdess</a>
 */
public final class NamespaceTest
  extends Test
{

  final QName qName;
  final boolean anyLocalName;
  final boolean any;

  public NamespaceTest(QName qName, boolean anyLocalName, boolean any)
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
        // Only match namespace attributes
        String uri = node.getNamespaceURI();
        if (XMLConstants.XMLNS_ATTRIBUTE_NS_URI.equals(uri) ||
            XMLConstants.XMLNS_ATTRIBUTE.equals(node.getPrefix()) ||
            XMLConstants.XMLNS_ATTRIBUTE.equals(node.getNodeName()))
          break;
        // Fall through
      default:
        // Only process namespace attributes
        return false;
      }
    if (any)
      return true;
    String uri = qName.getNamespaceURI();
    String nodeUri = node.getNamespaceURI();
    if (!NameTest.equal(uri, nodeUri))
      return false;
    if (anyLocalName)
      return true;
    String localName = qName.getLocalPart();
    String nodeLocalName = NameTest.getLocalName(node);
    return (localName.equals(nodeLocalName));
  }

  public Test clone(Object context)
  {
    return new NamespaceTest(qName, anyLocalName, any);
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
