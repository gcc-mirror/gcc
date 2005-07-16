/* DomXPathResult.java -- 
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

package gnu.xml.dom;

import java.util.Collection;
import java.util.Iterator;
import org.w3c.dom.Node;
import org.w3c.dom.xpath.XPathException;
import org.w3c.dom.xpath.XPathResult;

/**
 * An XPath result object.
 *
 * @author <a href='mailto:dog@gnu.org'>Chris Burdess</a>
 */
class DomXPathResult
implements XPathResult
{

  final Object value;
  final short type;
  Iterator iterator;

  DomXPathResult (Object value, short requestedType)
  {
    this.value = value;
    if (value instanceof Boolean)
      {
        type = XPathResult.BOOLEAN_TYPE;
      }
    else if (value instanceof Double)
      {
        type = XPathResult.NUMBER_TYPE;
      }
    else if (value instanceof String)
      {
        type = XPathResult.STRING_TYPE;
      }
    else if (value instanceof Collection)
      {
        Collection ns = (Collection) value;
        switch (requestedType)
          {
          case XPathResult.ANY_TYPE:
          case XPathResult.ANY_UNORDERED_NODE_TYPE:
            type = (ns.size () == 1) ? XPathResult.FIRST_ORDERED_NODE_TYPE :
              XPathResult.ORDERED_NODE_ITERATOR_TYPE;
            break;
          default:
            type = requestedType;
          }
        iterator = ns.iterator ();
      }
    else
      {
        throw new IllegalArgumentException ();
      }
  }

  public boolean getBooleanValue()
  {
    if (type == XPathResult.BOOLEAN_TYPE)
      {
        return ((Boolean) value).booleanValue ();
      }
    throw new XPathException (XPathException.TYPE_ERR, value.toString ());
  }

  public boolean getInvalidIteratorState()
  {
    return iterator == null;
  }

  public double getNumberValue()
  {
    if (type == XPathResult.NUMBER_TYPE)
      {
        return ((Double) value).doubleValue ();
      }
    throw new XPathException (XPathException.TYPE_ERR, value.toString ());
  }

  public short getResultType()
  {
    return type;
  }

  public Node getSingleNodeValue()
  {
    switch (type)
      {
      case XPathResult.FIRST_ORDERED_NODE_TYPE:
      case XPathResult.ORDERED_NODE_ITERATOR_TYPE:
      case XPathResult.ORDERED_NODE_SNAPSHOT_TYPE:
      case XPathResult.UNORDERED_NODE_ITERATOR_TYPE:
      case XPathResult.UNORDERED_NODE_SNAPSHOT_TYPE:
        Collection ns = (Collection) value;
        if (ns.isEmpty ())
          {
            return null;
          }
        else
          {
            return (Node) ns.iterator ().next ();
          }
      }
    throw new XPathException (XPathException.TYPE_ERR, value.toString ());
  }

  public int getSnapshotLength()
  {
    switch (type)
      {
      case XPathResult.FIRST_ORDERED_NODE_TYPE:
      case XPathResult.ORDERED_NODE_ITERATOR_TYPE:
      case XPathResult.ORDERED_NODE_SNAPSHOT_TYPE:
      case XPathResult.UNORDERED_NODE_ITERATOR_TYPE:
      case XPathResult.UNORDERED_NODE_SNAPSHOT_TYPE:
        return ((Collection) value).size ();
      }
    throw new XPathException (XPathException.TYPE_ERR, value.toString ());
  }

  public String getStringValue()
  {
    if (type == XPathResult.STRING_TYPE)
      {
        return (String) value;
      }
    throw new XPathException (XPathException.TYPE_ERR, value.toString ());
  }

  public Node iterateNext()
  {
    if (iterator != null)
      {
        if (iterator.hasNext ())
          {
            return (Node) iterator.next ();
          }
        else
          {
            iterator = null;
            return null;
          }
      }
    throw new XPathException (XPathException.TYPE_ERR, value.toString ());
  }

  public Node snapshotItem(int index)
  {
    switch (type)
      {
      case XPathResult.FIRST_ORDERED_NODE_TYPE:
      case XPathResult.ORDERED_NODE_ITERATOR_TYPE:
      case XPathResult.ORDERED_NODE_SNAPSHOT_TYPE:
      case XPathResult.UNORDERED_NODE_ITERATOR_TYPE:
      case XPathResult.UNORDERED_NODE_SNAPSHOT_TYPE:
        Collection ns = (Collection) value;
        Node[] nodes = new Node[ns.size ()];
        ns.toArray (nodes);
        return nodes[index];
      }
    throw new XPathException (XPathException.TYPE_ERR, value.toString ());
  }

  public String toString ()
  {
    return getClass ().getName () + "[type=" + typeName (type) + ",value=" +
      value + ']';
  }

  private String typeName (short type)
  {
    switch (type)
      {
      case XPathResult.BOOLEAN_TYPE:
        return "BOOLEAN_TYPE";
      case XPathResult.NUMBER_TYPE:
        return "NUMBER_TYPE";
      case XPathResult.STRING_TYPE:
        return "STRING_TYPE";
      case XPathResult.FIRST_ORDERED_NODE_TYPE:
        return "FIRST_ORDERED_NODE_TYPE";
      case XPathResult.ORDERED_NODE_ITERATOR_TYPE:
        return "ORDERED_NODE_ITERATOR_TYPE";
      case XPathResult.ORDERED_NODE_SNAPSHOT_TYPE:
        return "ORDERED_NODE_SNAPSHOT_TYPE";
      case XPathResult.UNORDERED_NODE_ITERATOR_TYPE:
        return "UNORDERED_NODE_ITERATOR_TYPE";
      case XPathResult.UNORDERED_NODE_SNAPSHOT_TYPE:
        return "UNORDERED_NODE_SNAPSHOT_TYPE";
      default:
        return "(unknown)";
      }
  }

}
