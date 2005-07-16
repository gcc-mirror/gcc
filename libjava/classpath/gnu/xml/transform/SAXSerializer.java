/* SAXSerializer.java -- 
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

package gnu.xml.transform;

import java.util.HashMap;
import java.util.Iterator;
import java.util.LinkedList;
import org.w3c.dom.Attr;
import org.w3c.dom.DocumentType;
import org.w3c.dom.NamedNodeMap;
import org.w3c.dom.Node;
import org.xml.sax.Attributes;
import org.xml.sax.ContentHandler;
import org.xml.sax.SAXException;
import org.xml.sax.ext.LexicalHandler;

/**
 * Serializes a DOM node to a sequence of SAX events.
 *
 * @author <a href='mailto:dog@gnu.org'>Chris Burdess</a>
 */
class SAXSerializer
  implements Attributes
{

  transient NamedNodeMap attrs;
  transient LinkedList namespaces = new LinkedList();

  boolean isDefined(String prefix, String uri)
  {
    for (Iterator i = namespaces.iterator(); i.hasNext(); )
      {
        HashMap ctx = (HashMap) i.next();
        if (uri.equals(ctx.get(prefix)))
          {
            return true;
          }
      }
    return false;
  }

  void define(String prefix, String uri)
  {
    for (Iterator i = namespaces.iterator(); i.hasNext(); )
      {
        HashMap ctx = (HashMap) i.next();
        if (ctx.containsKey(prefix))
          {
            HashMap newCtx = new HashMap();
            newCtx.put(prefix, uri);
            namespaces.addFirst(newCtx);
            return;
          }
      }
    HashMap ctx;
    if (namespaces.isEmpty())
      {
        ctx = new HashMap();
        namespaces.add(ctx);
      }
    else
      {
        ctx = (HashMap) namespaces.getFirst();
      }
    ctx.put(prefix, uri);
  }

  void undefine(String prefix, String uri)
  {
    for (Iterator i = namespaces.iterator(); i.hasNext(); )
      {
        HashMap ctx = (HashMap) i.next();
        if (uri.equals(ctx.get(prefix)))
          {
            ctx.remove(prefix);
            if (ctx.isEmpty())
              {
                namespaces.remove(ctx);
              }
            return;
          }
      }
  }

  public int getLength()
  {
    return attrs.getLength();
  }

  public String getURI(int index)
  {
    return attrs.item(index).getNamespaceURI();
  }

  public String getLocalName(int index)
  {
    return attrs.item(index).getLocalName();
  }

  public String getQName(int index)
  {
    return attrs.item(index).getNodeName();
  }

  public String getType(int index)
  {
    Attr attr = (Attr) attrs.item(index);
    return attr.isId() ? "ID" : "CDATA";
  }

  public String getValue(int index)
  {
    return attrs.item(index).getNodeValue();
  }

  public int getIndex(String uri, String localName)
  {
    int len = attrs.getLength();
    for (int i = 0; i < len; i++)
      {
        Node attr = attrs.item(i);
        String a_uri = attr.getNamespaceURI();
        String a_localName = attr.getLocalName();
        if (((a_uri == null && uri == null) ||
             (a_uri != null && a_uri.equals(uri))) &&
            a_localName.equals(localName))
          {
            return i;
          }
      }
    return -1;
  }

  public int getIndex(String qName)
  {
    int len = attrs.getLength();
    for (int i = 0; i < len; i++)
      {
        Node attr = attrs.item(i);
        String a_name = attr.getNodeName();
        if (a_name.equals(qName))
          {
            return i;
          }
      }
    return -1;
  }

  public String getType(String uri, String localName)
  {
    Attr attr = (Attr) attrs.getNamedItemNS(uri, localName);
    return attr.isId() ? "ID" : "CDATA";
  }

  public String getType(String qName)
  {
    Attr attr = (Attr) attrs.getNamedItem(qName);
    return attr.isId() ? "ID" : "CDATA";
  }

  public String getValue(String uri, String localName)
  {
    return attrs.getNamedItemNS(uri, localName).getNodeValue();
  }

  public String getValue(String qName)
  {
    return attrs.getNamedItem(qName).getNodeValue();
  }

  void serialize(Node node, ContentHandler ch, LexicalHandler lh)
    throws SAXException
  {
    attrs = node.getAttributes();
    Node children;
    Node next = node.getNextSibling();
    switch (node.getNodeType())
      {
      case Node.ELEMENT_NODE:
        String uri = node.getNamespaceURI();
        String prefix = node.getPrefix();
        boolean defined = isDefined(prefix, uri);
        if (!defined)
          {
            define(prefix, uri);
            ch.startPrefixMapping(prefix, uri);
          }
        String localName = node.getLocalName();
        String qName = node.getNodeName();
        ch.startElement(uri, localName, qName, this);
        children = node.getFirstChild();
        if (children != null)
          {
            serialize(children, ch, lh);
          }
        ch.endElement(uri, localName, qName);
        if (!defined)
          {
            ch.endPrefixMapping(prefix);
            undefine(prefix, uri);
          }
        break;
      case Node.TEXT_NODE:
        char[] chars = node.getNodeValue().toCharArray();
        ch.characters(chars, 0, chars.length);
        break;
      case Node.CDATA_SECTION_NODE:
        char[] cdata = node.getNodeValue().toCharArray();
        if (lh != null)
          {
            lh.startCDATA();
            ch.characters(cdata, 0, cdata.length);
            lh.endCDATA();
          }
        else
          {
            ch.characters(cdata, 0, cdata.length);
          }
        break;
      case Node.COMMENT_NODE:
        if (lh != null)
          {
            char[] comment = node.getNodeValue().toCharArray();
            lh.comment(comment, 0, comment.length);
          }
        break;
      case Node.DOCUMENT_NODE:
      case Node.DOCUMENT_FRAGMENT_NODE:
        ch.startDocument();
        children = node.getFirstChild();
        if (children != null)
          {
            serialize(children, ch, lh);
          }
        ch.endDocument();
        break;
      case Node.DOCUMENT_TYPE_NODE:
        if (lh != null)
          {
            DocumentType doctype = (DocumentType) node;
            String publicId = doctype.getPublicId();
            String systemId = doctype.getSystemId();
            lh.startDTD(node.getNodeName(), publicId, systemId);
            NamedNodeMap entities = doctype.getEntities();
            int len = entities.getLength();
            for (int i = 0; i < len; i++)
              {
                Node entity = entities.item(i);
                String entityName = entity.getNodeName();
                lh.startEntity(entityName);
                lh.endEntity(entityName);
              }
            lh.endDTD();
          }
        break;
      case Node.PROCESSING_INSTRUCTION_NODE:
        ch.processingInstruction(node.getNodeName(), node.getNodeValue());
        break;
      case Node.ENTITY_REFERENCE_NODE:
        ch.skippedEntity(node.getNodeName());
        break;
      }
    attrs = null;
    if (next != null)
      {
        serialize(next, ch, lh);
      }
  }

}
