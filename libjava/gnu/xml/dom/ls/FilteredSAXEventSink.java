/* FilteredSAXEventSink.java -- 
   Copyright (C) 1999,2000,2001 Free Software Foundation, Inc.

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

package gnu.xml.dom.ls;

import java.util.LinkedList;
import org.w3c.dom.Attr;
import org.w3c.dom.Element;
import org.w3c.dom.Node;
import org.w3c.dom.Text;
import org.w3c.dom.ls.LSParserFilter;
import org.w3c.dom.traversal.NodeFilter;
import org.xml.sax.Attributes;
import org.xml.sax.SAXException;

/**
 * A SAX event sink that calls out to a parser filter in order to decide
 * whether to insert nodes into the tree.
 *
 * @author <a href='mailto:dog@gnu.org'>Chris Burdess</a>
 */
class FilteredSAXEventSink
  extends SAXEventSink
{

  final LSParserFilter filter;
  final int whatToShow;

  /**
   * Stack of elements to insert.
   */
  LinkedList nodes;
  
  /**
   * Corresponding stack of filter decisions about the nodes.
   */
  LinkedList decisions;

  /**
   * True when rejecting child nodes.
   */
  boolean rejecting;

  FilteredSAXEventSink(LSParserFilter filter)
  {
    this.filter = filter;
    whatToShow = filter.getWhatToShow();
  }

  public void startDocument()
    throws SAXException
  {
    if (interrupted)
      {
        return;
      }
    nodes = new LinkedList();
    decisions = new LinkedList();
    
    super.startDocument();
  }

  public void endDocument()
    throws SAXException
  {
    if (interrupted)
      {
        return;
      }
    super.endDocument();

    switch (getDecision(ctx, false))
      {
      case LSParserFilter.FILTER_REJECT:
        ctx = null;
        doc = null;
        break;
      }
    
    nodes = null;
    decisions = null;
  }

  public void startElement(String uri, String localName, String qName,
                           Attributes atts)
    throws SAXException
  {
    if (rejecting || interrupted)
      {
        return;
      }
    Element element = createElement(uri, localName, qName, atts);
    ctx = element;
    
    short decision = getDecision(element, true);
    nodes.addLast(element);
    decisions.addLast(new Short(decision));

    switch (decision)
      {
      case LSParserFilter.FILTER_REJECT:
        rejecting = true;
        break;
      case LSParserFilter.FILTER_INTERRUPT:
        interrupted = true;
        break;
      }
  }

  protected Attr createAttr(Attributes atts, int index)
  {
    Attr attr = super.createAttr(atts, index);
    short decision = getDecision(attr, false);
    switch (decision)
      {
      case LSParserFilter.FILTER_REJECT:
        return null;
      case LSParserFilter.FILTER_INTERRUPT:
        interrupted = true;
        return null;
      }
    return attr;
  }

  public void endElement(String uri, String localName, String qName)
    throws SAXException
  {
    if (rejecting || interrupted)
      {
        return;
      }
    super.endElement(uri, localName, qName);
    
    Element element = (Element) nodes.removeLast();
    Node parent = nodes.isEmpty() ? doc : (Node) nodes.getLast();
    ctx = parent;
    short decision = ((Short) decisions.removeLast()).shortValue();
    switch (decision)
      {
      case LSParserFilter.FILTER_SKIP:
        // Add all children of element to parent
        for (Node child = element.getFirstChild(); child != null;
             child = child.getNextSibling())
          {
            parent.insertBefore(child, element);
          }
        return;
      case LSParserFilter.FILTER_REJECT:
        rejecting = false;
        break;
      }
    decision = getDecision(element, false);
    switch (decision)
      {
      case LSParserFilter.FILTER_ACCEPT:
        parent.appendChild(element);
        break;
      case LSParserFilter.FILTER_INTERRUPT:
        interrupted = true;
        break;
      }
  }

  public void characters(char[] c, int off, int len)
    throws SAXException
  {
    if (rejecting || interrupted)
      {
        return;
      }
    Text text = createText(c, off, len);
    short decision = getDecision(text, false);
    switch (decision)
      {
      case LSParserFilter.FILTER_ACCEPT:
        ctx.appendChild(text);
        break;
      case LSParserFilter.FILTER_INTERRUPT:
        interrupted = true;
        break;
      }
  }

  public void processingInstruction(String target, String data)
    throws SAXException
  {
    if (rejecting || interrupted || inDTD)
      {
        return;
      }
    Node pi = createProcessingInstruction(target, data);
    short decision = getDecision(pi, false);
    switch (decision)
      {
      case LSParserFilter.FILTER_ACCEPT:
        ctx.appendChild(pi);
        break;
      case LSParserFilter.FILTER_INTERRUPT:
        interrupted = true;
        break;
      }
  }

  public void startDTD(String name, String publicId, String systemId)
    throws SAXException
  {
    if (interrupted)
      {
        return;
      }
    Node doctype = createDocumentType(name, publicId, systemId);
    ctx = doctype;
    inDTD = true;
    nodes.addLast(doctype);
    decisions.addLast(new Short(LSParserFilter.FILTER_ACCEPT));
  }

  public void endDTD()
    throws SAXException
  {
    if (interrupted)
      {
        return;
      }
    Node doctype = (Node) nodes.removeLast();
    decisions.removeLast();
    inDTD = false;
    ctx = doc;
    short decision = getDecision(doctype, false);
    switch (decision)
      {
      case LSParserFilter.FILTER_ACCEPT:
        ctx.appendChild(doctype);
        break;
      case LSParserFilter.FILTER_INTERRUPT:
        interrupted = true;
        break;
      }
  }

  public void comment(char[] c, int off, int len)
    throws SAXException
  {
    if (rejecting || interrupted || inDTD)
      {
        return;
      }
    Node comment = createComment(c, off, len);
    short decision = getDecision(comment, false);
    switch (decision)
      {
      case LSParserFilter.FILTER_ACCEPT:
        ctx.appendChild(comment);
        break;
      case LSParserFilter.FILTER_INTERRUPT:
        interrupted = true;
        break;
      }
  }

  // TODO declarations

  short getDecision(Node node, boolean start)
  {
    boolean show = (whatToShow == NodeFilter.SHOW_ALL);
    if (!show)
      {
        switch (node.getNodeType())
          {
          case Node.ATTRIBUTE_NODE:
            show = ((whatToShow & NodeFilter.SHOW_ATTRIBUTE) != 0);
            break;     
          case Node.TEXT_NODE:
            show = ((whatToShow & NodeFilter.SHOW_TEXT) != 0);
            break;     
          case Node.CDATA_SECTION_NODE:
            show = ((whatToShow & NodeFilter.SHOW_CDATA_SECTION) != 0);
            break;     
          case Node.ELEMENT_NODE:
            show = ((whatToShow & NodeFilter.SHOW_ELEMENT) != 0);
            break;     
          case Node.COMMENT_NODE:
            show = ((whatToShow & NodeFilter.SHOW_COMMENT) != 0);
            break;     
          case Node.DOCUMENT_NODE:
            show = ((whatToShow & NodeFilter.SHOW_DOCUMENT) != 0);
            break;     
          case Node.PROCESSING_INSTRUCTION_NODE:
            show = ((whatToShow & NodeFilter.SHOW_PROCESSING_INSTRUCTION) != 0);
            break;     
          case Node.DOCUMENT_FRAGMENT_NODE:
            show = ((whatToShow & NodeFilter.SHOW_DOCUMENT_FRAGMENT) != 0);
            break;     
          case Node.DOCUMENT_TYPE_NODE:
            show = ((whatToShow & NodeFilter.SHOW_DOCUMENT_TYPE) != 0);
            break;     
          case Node.ENTITY_REFERENCE_NODE:
            show = ((whatToShow & NodeFilter.SHOW_ENTITY_REFERENCE) != 0);
            break;     
          case Node.ENTITY_NODE:
            show = ((whatToShow & NodeFilter.SHOW_ENTITY) != 0);
            break;     
          case Node.NOTATION_NODE:
            show = ((whatToShow & NodeFilter.SHOW_NOTATION) != 0);
            break;     
          }
      }
    if (!show)
      {
        return LSParserFilter.FILTER_ACCEPT;
      }
    if (start)
      {
        return filter.startElement((Element) node);
      }
    return filter.acceptNode(node);
  }

}

