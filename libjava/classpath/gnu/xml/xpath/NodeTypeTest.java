/* NodeTypeTest.java -- 
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

import javax.xml.namespace.QName;
import org.w3c.dom.Node;
import org.w3c.dom.ProcessingInstruction;

/**
 * Tests whether a node is of a given type.
 *
 * @author <a href='mailto:dog@gnu.org'>Chris Burdess</a>
 */
public final class NodeTypeTest
  extends Test
{

  final short type;
  final String data;

  public NodeTypeTest(short type)
  {
    this(type, null);
  }

  public NodeTypeTest(short type, String data)
  {
    this.type = type;
    this.data = data;
  }

  public short getNodeType()
  {
    return type;
  }

  public String getData()
  {
    return data;
  }

  public boolean matches(Node node, int pos, int len)
  {
    short nodeType = node.getNodeType();
    switch (nodeType)
      {
      case Node.ELEMENT_NODE:
      case Node.ATTRIBUTE_NODE:
      case Node.TEXT_NODE:
      case Node.CDATA_SECTION_NODE:
      case Node.COMMENT_NODE:
        if (type > 0)
          {
            if (nodeType != type)
              return false;
          }
        return true;
      case Node.PROCESSING_INSTRUCTION_NODE:
        if (type > 0)
          {
            if (nodeType != type)
              return false;
            if (data != null &&
                !data.equals(((ProcessingInstruction) node).getTarget()))
              return false;
          }
        return true;
      default:
        // Not part of XPath data model
        return false;
      }
  }

  public Test clone(Object context)
  {
    return new NodeTypeTest(type, data);
  }

  public boolean references(QName var)
  {
    return false;
  }

  public String toString()
  {
    switch (type)
      {
      case 0:
        return "node()";
      case Node.TEXT_NODE:
        return "text()";
      case Node.COMMENT_NODE:
        return "comment()";
      case Node.PROCESSING_INSTRUCTION_NODE:
        if (data != null)
          {
            return "processing-instruction('" + data + "')";
          }
        return "processing-instruction()";
      default:
        throw new IllegalStateException();
      }
  }
  
}
