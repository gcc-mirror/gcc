/* DomHTMLParser.java --
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


package gnu.xml.dom.html2;

import gnu.javax.swing.text.html.parser.support.Parser;

import java.io.IOException;
import java.io.Reader;

import java.util.Enumeration;
import java.util.Iterator;
import java.util.LinkedList;

import javax.swing.text.AttributeSet;
import javax.swing.text.html.HTML;
import javax.swing.text.html.parser.DTD;
import javax.swing.text.html.parser.TagElement;

import org.w3c.dom.NamedNodeMap;
import org.w3c.dom.Node;
import org.w3c.dom.html2.HTMLDocument;

/**
 * This parser reads HTML from the given stream and stores into
 * {@link HTMLDocument}. The HTML tag becomes the {@link Node}.
 * The tag attributes become the node attributes. The text inside
 * HTML tag is inserted as one or several text nodes. The nested
 * HTML tags are inserted as child nodes.
 * 
 * If the strict tree structure, closing the tag means closing all
 * nested tags. To work around this, this parser closes the nested
 * tags and immediately reopens them after the closed tag.
 * In this way, <code>&lt;b&gt;&lt;i&gt;c&lt;/b&gt;d</code> 
 * is parsed as <code>&lt;b&gt;&lt;i&gt;c&lt;/i&gt;&lt;/b&gt;&lt;i&gt;d</code> .
 *
 * @author Audrius Meskauskas (AudriusA@Bioinformatics.org)
 */
public class DomHTMLParser
  extends gnu.javax.swing.text.html.parser.support.Parser
{
  /**
   * The target where HTML document will be inserted.
   */
  protected DomHTMLDocument document;

  /**
   * The subsequently created new nodes will be inserted as the
   * childs of this cursor.
   */
  protected Node cursor;

  /**
   * Create parser using the given DTD.
   *
   * @param dtd the DTD (for example,
   * {@link gnu.javax.swing.text.html.parser.HTML_401F}).
   */
  public DomHTMLParser(DTD dtd)
  {
    super(dtd);
  }

  /**
   * Parse SGML insertion ( &lt;! ... &gt; ).
   * Currently just treats it as comment.
   */
  public boolean parseMarkupDeclarations(StringBuffer strBuff)
                                  throws java.io.IOException
  {
    Node c = document.createComment(strBuff.toString());
    cursor.appendChild(c);
    return false;
  }

  /**
   * Read the document, present in the given stream, and
   * return the corresponding {@link HTMLDocument}.
   *
   * @param input a stream to read from.
   * @return a document, reflecting the structure of the provided HTML
   * text.
   *
   * @throws IOException if the reader throws one.
   */
  public HTMLDocument parseDocument(Reader input)
                    throws IOException
  {
    try
      {
        document = new DomHTMLDocument();
        document.setCheckWellformedness(false);
        document.setCheckingCharacters(false);
        
        cursor = document;
        
        parse(input);

        DomHTMLDocument h = document;
        document = null;
        return h;
      }
    catch (Exception ex)
      {
        ex.printStackTrace();
        throw new IOException("Exception: " + ex.getMessage());
      }
  }
  
  /**
   * Create a new node.
   * @param name the name of node, case insensitive.
   * @return the created node.
   */
  protected Node createNode(String name)
  {
    Node new_node = document.createElement(name.toLowerCase());
    AttributeSet hatts = getAttributes();
    NamedNodeMap natts = new_node.getAttributes();

    Enumeration enumeration = hatts.getAttributeNames();
    Object key;
    Node attribute;

    while (hatts != null)
      {
        while (enumeration.hasMoreElements())
          {
            key = enumeration.nextElement();
            attribute = document.createAttribute(key.toString());
            attribute.setNodeValue(hatts.getAttribute(key).toString());
            natts.setNamedItem(attribute);
          }

        // The default values are stored in a parent node. 
        hatts = hatts.getResolveParent();
      }

    return new_node;
  }
  
  /**
   * Handle comment by inserting the comment node.
   * @param text the comment text.
   */
  protected void handleComment(char[] text)
  {
    Node c = document.createComment(new String(text));
    cursor.appendChild(c);
  }
  
  /**
   * Handle the tag with no content.
   * @param tag the tag to handle.
   */
  protected void handleEmptyTag(TagElement tag)
  {
    String name = tag.getHTMLTag().toString();

    if (name.equalsIgnoreCase("#pcdata"))
      return;

    Node c = createNode(name);
    cursor.appendChild(c);
  }
  
  /**
   * Close the given tag. Close and reopen all nested tags.
   * @param tag the tag to close.
   */
  protected void handleEndTag(TagElement tag)
  {
    String name = tag.getHTMLTag().toString();
    String nname = cursor.getNodeName();

    // Closing the current tag.
    if (nname != null && nname.equalsIgnoreCase(name))
      {
        cursor = cursor.getParentNode();
      }
    else
      {
        Node nCursor = cursor.getParentNode();

        // Remember the opened nodes.
        LinkedList open = new LinkedList();
        Node close = cursor;
        while (close != null && !close.getNodeName().equalsIgnoreCase(name))
          {
            if (close != document)
              open.addFirst(close);
            close = close.getParentNode();
          }
        if (close == null)
          cursor = document;
        else
          cursor = close.getParentNode();

        // Insert the copies of the opened nodes.   
        Iterator iter = open.iterator();
        while (iter.hasNext())
          {
            Node item = (Node) iter.next();
            cursor.appendChild(item);
            cursor = item;
          }
      }
  }

  /**
   * Handle the start tag by inserting the HTML element.
   * @param tag the tag to handle.
   */
  protected void handleStartTag(TagElement tag)
  {
    HTML.Tag h = tag.getHTMLTag();
    Node c = createNode(h.toString());
    cursor.appendChild(c);
    cursor = c;
  }
  
  /**
   * Handle text by inserting the text node.
   * @param text the text to insert.
   */
  protected void handleText(char[] text)
  {
    Node c = document.createTextNode(text, 0, text.length);
    cursor.appendChild(c);
  }
}
