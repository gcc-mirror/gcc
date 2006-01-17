/* StreamSerializer.java -- 
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

package gnu.xml.transform;

import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.io.OutputStream;
import java.io.UnsupportedEncodingException;
import java.nio.ByteBuffer;
import java.nio.CharBuffer;
import java.nio.charset.Charset;
import java.nio.charset.CharsetEncoder;
import java.util.Collection;
import java.util.Collections;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;
import java.util.LinkedList;
import java.util.Map;
import javax.xml.XMLConstants;
import org.w3c.dom.Attr;
import org.w3c.dom.Document;
import org.w3c.dom.DocumentType;
import org.w3c.dom.NamedNodeMap;
import org.w3c.dom.Node;

/**
 * Serializes a DOM node to an output stream.
 *
 * @author <a href='mailto:dog@gnu.org'>Chris Burdess</a>
 */
public class StreamSerializer
{
  
  static final int SPACE = 0x20;
  static final int BANG = 0x21; // !
  static final int APOS = 0x27; // '
  static final int SLASH = 0x2f; // /
  static final int BRA = 0x3c; // <
  static final int KET = 0x3e; // >
  static final int EQ = 0x3d; // =

  /**
   * HTML 4.01 boolean attributes
   */
  static final Map HTML_BOOLEAN_ATTRIBUTES = new HashMap();
  static
  {
    HashSet set;
    
    set = new HashSet();
    set.add("nohref");
    HTML_BOOLEAN_ATTRIBUTES.put("area", set);

    set = new HashSet();
    set.add("ismap");
    HTML_BOOLEAN_ATTRIBUTES.put("img", set);

    set = new HashSet();
    set.add("declare");
    HTML_BOOLEAN_ATTRIBUTES.put("object", set);
    
    set = new HashSet();
    set.add("noshade");
    HTML_BOOLEAN_ATTRIBUTES.put("hr", set);
    
    set = new HashSet();
    set.add("compact");
    HTML_BOOLEAN_ATTRIBUTES.put("dl", set);
    HTML_BOOLEAN_ATTRIBUTES.put("ol", set);
    HTML_BOOLEAN_ATTRIBUTES.put("ul", set);
    HTML_BOOLEAN_ATTRIBUTES.put("dir", set);
    HTML_BOOLEAN_ATTRIBUTES.put("menu", set);
    
    set = new HashSet();
    set.add("checked");
    set.add("disabled");
    set.add("readonly");
    set.add("ismap");
    HTML_BOOLEAN_ATTRIBUTES.put("input", set);
    
    set = new HashSet();
    set.add("multiple");
    set.add("disabled");
    HTML_BOOLEAN_ATTRIBUTES.put("select", set);
    
    set = new HashSet();
    set.add("disabled");
    HTML_BOOLEAN_ATTRIBUTES.put("optgroup", set);
    
    set = new HashSet();
    set.add("selected");
    set.add("disabled");
    HTML_BOOLEAN_ATTRIBUTES.put("option", set);
    
    set = new HashSet();
    set.add("disabled");
    set.add("readonly");
    HTML_BOOLEAN_ATTRIBUTES.put("textarea", set);
    
    set = new HashSet();
    set.add("disabled");
    HTML_BOOLEAN_ATTRIBUTES.put("button", set);
    
    set = new HashSet();
    set.add("nowrap");
    HTML_BOOLEAN_ATTRIBUTES.put("th", set);
    HTML_BOOLEAN_ATTRIBUTES.put("td", set);
    
    set = new HashSet();
    set.add("noresize");
    HTML_BOOLEAN_ATTRIBUTES.put("frame", set);
    
    set = new HashSet();
    set.add("defer");
    HTML_BOOLEAN_ATTRIBUTES.put("script", set);
  }

  // HTML namespace URIs
  static final HashSet HTML_URIS = new HashSet();
  static {
    HTML_URIS.add("http://www.w3.org/1999/xhtml");
  }

  protected final String encoding;
  final Charset charset;
  final CharsetEncoder encoder;
  final int mode;
  final LinkedList namespaces;
  protected String eol;
  Collection cdataSectionElements = Collections.EMPTY_SET;

  protected boolean discardDefaultContent;
  protected boolean xmlDeclaration = true;
  
  // has a META element with the encoding been added?
  private boolean htmlEncoded;

  public StreamSerializer()
  {
    this(Stylesheet.OUTPUT_XML, null, null);
  }

  public StreamSerializer(String encoding)
  {
    this(Stylesheet.OUTPUT_XML, encoding, null);
  }

  public StreamSerializer(int mode, String encoding, String eol)
  {
    this.mode = mode;
    if (encoding == null)
      encoding = (mode == Stylesheet.OUTPUT_HTML) ? "ISO-8859-1" : "UTF-8";
    this.encoding = encoding.intern();
    charset = Charset.forName(this.encoding);
    encoder = charset.newEncoder();
    this.eol = (eol != null) ? eol : System.getProperty("line.separator");
    namespaces = new LinkedList();
  }

  void setCdataSectionElements(Collection c)
  {
    cdataSectionElements = c;
  }

  public void serialize(final Node node, final OutputStream out)
    throws IOException
  {
    serialize(node, out, false);
  }
  
  void serialize(Node node, final OutputStream out,
                 boolean convertToCdata)
    throws IOException
  {
    while (node != null)
      {
        Node next = node.getNextSibling();
        doSerialize(node, out, convertToCdata);
        node = next;
      }
  }

  private void doSerialize(final Node node, final OutputStream out,
                           boolean convertToCdata)
    throws IOException
  {
    if (out == null)
      throw new NullPointerException("no output stream");
    htmlEncoded = false;
    String value, prefix;
    Node children;
    String uri = node.getNamespaceURI();
    short nt = node.getNodeType();
    if (convertToCdata && nt == Node.TEXT_NODE)
      nt = Node.CDATA_SECTION_NODE;
    switch (nt)
      {
      case Node.ATTRIBUTE_NODE:
        prefix = node.getPrefix();
        if (XMLConstants.XMLNS_ATTRIBUTE_NS_URI.equals(uri) ||
            XMLConstants.XMLNS_ATTRIBUTE.equals(prefix) ||
            (prefix != null && prefix.startsWith("xmlns:")))
          {
            String nsuri = node.getNodeValue();
            if (isDefined(nsuri, prefix))
              break;
            String name = node.getLocalName();
            if (name == null)
              {
                // Namespace-unaware
                name = node.getNodeName();
                int ci = name.indexOf(':');
                if (ci != -1)
                  name = name.substring(ci + 1);
              }
            define(nsuri, name);
          }
        else if (uri != null && !isDefined(uri, prefix))
          {
            prefix = define(uri, prefix);
            String nsname = (prefix == null) ? "xmlns" : "xmlns:" + prefix;
            out.write(SPACE);
            out.write(encodeText(nsname));
            out.write(EQ);
            String nsvalue = "\"" + encode(uri, true, true) + "\"";
            out.write(nsvalue.getBytes(encoding));
          }
        out.write(SPACE);
        String a_nodeName = node.getNodeName();
        out.write(encodeText(a_nodeName));
        String a_nodeValue = node.getNodeValue();
        if (mode == Stylesheet.OUTPUT_HTML &&
            a_nodeName.equals(a_nodeValue) &&
            isHTMLBoolean((Attr) node, a_nodeName))
          break;
        out.write(EQ);
        value = "\"" + encode(a_nodeValue, true, true) + "\"";
        out.write(encodeText(value));
        break;
      case Node.ELEMENT_NODE:
        pushNamespaceContext();
        value = node.getNodeName();
        out.write(BRA);
        out.write(encodeText(value));
        prefix = node.getPrefix();
        if (uri != null && !isDefined(uri, prefix))
          {
            prefix = define(uri, prefix);
            String nsname = (prefix == null) ? "xmlns" : "xmlns:" + prefix;
            out.write(SPACE);
            out.write(encodeText(nsname));
            out.write(EQ);
            String nsvalue = "\"" + encode(uri, true, true) + "\"";
            out.write(encodeText(nsvalue));
          }
        NamedNodeMap attrs = node.getAttributes();
        if (attrs != null)
          {
            int len = attrs.getLength();
            for (int i = 0; i < len; i++)
              {
                Attr attr = (Attr) attrs.item(i);
                if (discardDefaultContent && !attr.getSpecified())
                  {
                    // NOOP
                  }
                else
                  serialize(attr, out, false);
              }
          }
        convertToCdata = cdataSectionElements.contains(value);
        children = node.getFirstChild();
        if (children == null)
          {
            out.write(SLASH);
            out.write(KET);
          }
        else
          {
            out.write(KET);
            serialize(children, out, convertToCdata);
            out.write(BRA);
            out.write(SLASH);
            out.write(encodeText(value));
            out.write(KET);
          }
        popNamespaceContext();
        break;
      case Node.TEXT_NODE:
        value = node.getNodeValue();
        if (!"yes".equals(node.getUserData("disable-output-escaping")))
          value = encode(value, false, false);
        out.write(encodeText(value));
        break;
      case Node.CDATA_SECTION_NODE:
        value = node.getNodeValue();
        // Where any instanceof of ]]> occur, split into multiple CDATA
        // sections
        int bbk = value.indexOf("]]>");
        while (bbk != -1)
          {
            String head = value.substring(0, bbk + 2);
            out.write(encodeText("<![CDATA[" + head + "]]>"));
            value = value.substring(bbk + 2);
            bbk = value.indexOf("]]>");
          }
        // Write final tail value
        out.write(encodeText("<![CDATA[" + value + "]]>"));
        break;
      case Node.COMMENT_NODE:
        value = "<!--" + node.getNodeValue() + "-->";
        out.write(encodeText(value));
        Node cp = node.getParentNode();
        if (cp != null && cp.getNodeType() == Node.DOCUMENT_NODE)
          out.write(encodeText(eol));
        break;
      case Node.DOCUMENT_NODE:
      case Node.DOCUMENT_FRAGMENT_NODE:
        if (mode == Stylesheet.OUTPUT_XML)
          {
            if ("UTF-16".equalsIgnoreCase(encoding))
              {
                out.write(0xfe);
                out.write(0xff);
              }
            if (!"yes".equals(node.getUserData("omit-xml-declaration")) &&
                xmlDeclaration)
              {
                Document doc = (node instanceof Document) ?
                  (Document) node : null;
                String version = (doc != null) ? doc.getXmlVersion() : null;
                if (version == null)
                  version = (String) node.getUserData("version");
                if (version == null)
                  version = "1.0";
                out.write(BRA);
                out.write(0x3f);
                out.write("xml version=\"".getBytes("US-ASCII"));
                out.write(version.getBytes("US-ASCII"));
                out.write(0x22);
                if (!("UTF-8".equalsIgnoreCase(encoding)))
                  {
                    out.write(" encoding=\"".getBytes("US-ASCII"));
                    out.write(encoding.getBytes("US-ASCII"));
                    out.write(0x22);
                  }
                if ((doc != null && doc.getXmlStandalone()) ||
                    "yes".equals(node.getUserData("standalone")))
                  out.write(" standalone=\"yes\"".getBytes("US-ASCII"));
                out.write(0x3f);
                out.write(KET);
                out.write(encodeText(eol));
              }
            // TODO warn if not outputting the declaration would be a
            // problem
          }
        else if (mode == Stylesheet.OUTPUT_HTML)
          {
            // Ensure that encoding is accessible if head element is present
            String mediaType = (String) node.getUserData("media-type");
            if (mediaType == null)
              mediaType = "text/html";
            String contentType = mediaType + "; charset=" +
              ((encoding.indexOf(' ') != -1) ?
                "\"" + encoding + "\"" :
                encoding);
            Document doc = (node instanceof Document) ? (Document) node :
              node.getOwnerDocument();
            Node html = null;
            for (Node ctx = node.getFirstChild(); ctx != null;
                 ctx = ctx.getNextSibling())
              {
                if (ctx.getNodeType() == Node.ELEMENT_NODE &&
                    isHTMLElement(ctx, "html"))
                  {
                    html = ctx;
                    break;
                  }
              }
            if (html != null)
              {
                Node head = null;
                for (Node ctx = html.getFirstChild(); ctx != null;
                     ctx = ctx.getNextSibling())
                  {
                    if (isHTMLElement(ctx, "head"))
                      {
                        head = ctx;
                        break;
                      }
                  }
                if (head != null)
                  {
                    Node meta = null;
                    Node metaContent = null;
                    for (Node ctx = head.getFirstChild(); ctx != null;
                         ctx = ctx.getNextSibling())
                      {
                        if (isHTMLElement(ctx, "meta"))
                          {
                            NamedNodeMap metaAttrs = ctx.getAttributes();
                            int len = metaAttrs.getLength();
                            String httpEquiv = null;
                            Node content = null;
                            for (int i = 0; i < len; i++)
                              {
                                Node attr = metaAttrs.item(i);
                                String attrName = attr.getNodeName();
                                if ("http-equiv".equalsIgnoreCase(attrName))
                                  httpEquiv = attr.getNodeValue();
                                else if ("content".equalsIgnoreCase(attrName))
                                  content = attr;
                              }
                            if ("Content-Type".equalsIgnoreCase(httpEquiv))
                              {
                                meta = ctx;
                                metaContent = content;
                                break;
                              }
                          }
                      }
                    if (meta == null)
                      {
                        meta = doc.createElement("meta");
                        // Insert first
                        Node first = head.getFirstChild();
                        if (first == null)
                          head.appendChild(meta);
                        else
                          head.insertBefore(meta, first);
                        Node metaHttpEquiv = doc.createAttribute("http-equiv");
                        meta.getAttributes().setNamedItem(metaHttpEquiv);
                        metaHttpEquiv.setNodeValue("Content-Type");
                      }
                    if (metaContent == null)
                      {
                        metaContent = doc.createAttribute("content");
                        meta.getAttributes().setNamedItem(metaContent);
                      }
                    metaContent.setNodeValue(contentType);
                    htmlEncoded = true;
                  }
              }
          }
        children = node.getFirstChild();
        if (children != null)
          serialize(children, out, convertToCdata);
        break;
      case Node.DOCUMENT_TYPE_NODE:
        DocumentType doctype = (DocumentType) node;
        out.write(BRA);
        out.write(BANG);
        out.write(encodeText("DOCTYPE "));
        value = doctype.getNodeName();
        out.write(encodeText(value));
        String publicId = doctype.getPublicId();
        if (publicId != null)
          {
            out.write(encodeText(" PUBLIC "));
            out.write(APOS);
            out.write(encodeText(publicId));
            out.write(APOS);
          }
        String systemId = doctype.getSystemId();
        if (systemId != null)
          {
            out.write(encodeText(" SYSTEM "));
            out.write(APOS);
            out.write(encodeText(systemId));
            out.write(APOS);
          }
        String internalSubset = doctype.getInternalSubset();
        if (internalSubset != null)
          {
            out.write(encodeText(internalSubset));
          }
        out.write(KET);
        out.write(eol.getBytes(encoding));
        break;
      case Node.ENTITY_REFERENCE_NODE:
        value = "&" + node.getNodeValue() + ";";
        out.write(encodeText(value));
        break;
      case Node.PROCESSING_INSTRUCTION_NODE:
        value = "<?" + node.getNodeName() + " " + node.getNodeValue() + "?>";
        out.write(encodeText(value));
        Node pp = node.getParentNode();
        if (pp != null && pp.getNodeType() == Node.DOCUMENT_NODE)
          {
            out.write(encodeText(eol));
          }
        break;
      default:
        System.err.println("Unhandled node type: "+nt);
      }
  }

  boolean isHTMLElement(Node node, String name)
  {
    if (node.getNodeType() != Node.ELEMENT_NODE)
      return false;
    String localName = node.getLocalName();
    if (localName == null)
      localName = node.getNodeName();
    if (!name.equalsIgnoreCase(localName))
      return false;
    String uri = node.getNamespaceURI();
    return (uri == null || HTML_URIS.contains(uri));
  }

  boolean isDefined(String uri, String prefix)
  {
    if (XMLConstants.XML_NS_URI.equals(uri))
      return "xml".equals(prefix);
    if (XMLConstants.XMLNS_ATTRIBUTE_NS_URI.equals(uri))
      return "xmlns".equals(prefix);
    if (prefix == null)
      prefix = "";
    for (Iterator i = namespaces.iterator(); i.hasNext(); )
      {
        Map ctx = (Map) i.next();
        String val = (String) ctx.get(uri);
        if (val != null && val.equals(prefix))
          return true;
      }
    return false;
  }

  void pushNamespaceContext()
  {
    namespaces.addFirst(new HashMap());
  }

  String define(String uri, String prefix)
  {
    if (namespaces.isEmpty())
      return prefix;
    HashMap ctx = (HashMap) namespaces.getFirst();
    while (ctx.containsValue(prefix))
      {
        // Fabricate new prefix
        prefix = prefix + "_";
      }
    ctx.put(uri, prefix);
    return prefix;
  }

  void popNamespaceContext()
  {
    namespaces.removeFirst();
  }

  final byte[] encodeText(String text)
    throws IOException
  {
    encoder.reset();
    boolean htmlNeedingEncoding =
      (mode == Stylesheet.OUTPUT_HTML && !htmlEncoded);
    if (!encoder.canEncode(text) || htmlNeedingEncoding)
      {
        // Check each character
        StringBuffer buf = new StringBuffer();
        int len = text.length();
        for (int i = 0; i < len; i++)
          {
            char c = text.charAt(i);
            if (!encoder.canEncode(c))
              {
                // Replace with character entity reference
                String hex = Integer.toHexString((int) c);
                buf.append("&#x");
                buf.append(hex);
                buf.append(';');
              }
            else if (htmlNeedingEncoding)
              {
                String entityName = getHTMLCharacterEntity(c);
                if (entityName != null)
                  {
                    buf.append('&');
                    buf.append(entityName);
                    buf.append(';');
                  }
                else
                  buf.append(c);
              }
            else
              buf.append(c);
          }
        text = buf.toString();
      }
    ByteBuffer encoded = encoder.encode(CharBuffer.wrap(text));
    int len = encoded.limit() - encoded.position();
    if (encoded.hasArray())
      {
        byte[] ret = encoded.array();
        if (ret.length > len)
          {
            // Why?
            byte[] ret2 = new byte[len];
            System.arraycopy(ret, 0, ret2, 0, len);
            ret = ret2;
          }
        return ret;
      }
    encoded.flip();
    byte[] ret = new byte[len];
    encoded.get(ret, 0, len);
    return ret;
  }

  String encode(String text, boolean encodeCtl, boolean inAttr)
  {
    int len = text.length();
    StringBuffer buf = null;
    for (int i = 0; i < len; i++)
      {
        char c = text.charAt(i);
        if (c == '<')
          {
            if (buf == null)
              buf = new StringBuffer(text.substring(0, i));
            buf.append("&lt;");
          }
        else if (c == '>')
          {
            if (buf == null)
              buf = new StringBuffer(text.substring(0, i));
            buf.append("&gt;");
          }
        else if (c == '&')
          {
            if (mode == Stylesheet.OUTPUT_HTML && (i + 1) < len &&
                text.charAt(i + 1) == '{')
              {
                if (buf != null)
                  buf.append(c);
              }
            else
              {
                if (buf == null)
                  buf = new StringBuffer(text.substring(0, i));
                buf.append("&amp;");
              }
          }
        else if (c == '\'' && inAttr)
          {
            if (buf == null)
              buf = new StringBuffer(text.substring(0, i));
            if (mode == Stylesheet.OUTPUT_HTML)
              // HTML does not define &apos;, use character entity ref
              buf.append("&#x27;");
            else
              buf.append("&apos;");
          }
        else if (c == '"' && inAttr)
          {
            if (buf == null)
              buf = new StringBuffer(text.substring(0, i));
            buf.append("&quot;");
          }
        else if (encodeCtl)
          {
            if (c < 0x20)
              {
                if (buf == null)
                  buf = new StringBuffer(text.substring(0, i));
                buf.append('&');
                buf.append('#');
                buf.append((int) c);
                buf.append(';');
              }
            else if (buf != null)
              buf.append(c);
          }
        else if (buf != null)
          buf.append(c);
      }
    return (buf == null) ? text : buf.toString();
  }

  String toString(Node node)
  {
    ByteArrayOutputStream out = new ByteArrayOutputStream();
    try
      {
        serialize(node, out);
        return new String(out.toByteArray(), encoding);
      }
    catch (IOException e)
      {
        throw new RuntimeException(e.getMessage());
      }
  }

  boolean isHTMLBoolean(Attr attr, String attrName)
  {
    attrName = attrName.toLowerCase();
    Node element = attr.getOwnerElement();
    String elementName = element.getLocalName();
    if (elementName == null)
      {
        elementName = element.getNodeName();
      }
    elementName = elementName.toLowerCase();
    Collection attributes =
      (Collection) HTML_BOOLEAN_ATTRIBUTES.get(elementName);
    return (attributes != null && attributes.contains(attrName));
  }

  static String getHTMLCharacterEntity(char c)
  {
    // Hardcode these here to avoid loading the HTML DTD
    switch (c)
      {
      case 160: return "nbsp";
      case 161: return "iexcl";
      case 162: return "cent";
      case 163: return "pound";
      case 164: return "curren";
      case 165: return "yen";
      case 166: return "brvbar";
      case 167: return "sect";
      case 168: return "uml";
      case 169: return "copy";
      case 170: return "ordf";
      case 171: return "laquo";
      case 172: return "not";
      case 173: return "shy";
      case 174: return "reg";
      case 175: return "macr";
      case 176: return "deg";
      case 177: return "plusmn";
      case 178: return "sup2";
      case 179: return "sup3";
      case 180: return "acute";
      case 181: return "micro";
      case 182: return "para";
      case 183: return "middot";
      case 184: return "cedil";
      case 185: return "sup1";
      case 186: return "ordm";
      case 187: return "raquo";
      case 188: return "frac14";
      case 189: return "frac12";
      case 190: return "frac34";
      case 191: return "iquest";
      case 192: return "Agrave";
      case 193: return "Aacute";
      case 194: return "Acirc";
      case 195: return "Atilde";
      case 196: return "Auml";
      case 197: return "Aring";
      case 198: return "AElig";
      case 199: return "Ccedil";
      case 200: return "Egrave";
      case 201: return "Eacute";
      case 202: return "Ecirc";
      case 203: return "Euml";
      case 204: return "Igrave";
      case 205: return "Iacute";
      case 206: return "Icirc";
      case 207: return "Iuml";
      case 208: return "ETH";
      case 209: return "Ntilde";
      case 210: return "Ograve";
      case 211: return "Oacute";
      case 212: return "Ocirc";
      case 213: return "Otilde";
      case 214: return "Ouml";
      case 215: return "times";
      case 216: return "Oslash";
      case 217: return "Ugrave";
      case 218: return "Uacute";
      case 219: return "Ucirc";
      case 220: return "Uuml";
      case 221: return "Yacute";
      case 222: return "THORN";
      case 223: return "szlig";
      case 224: return "agrave";
      case 225: return "aacute";
      case 226: return "acirc";
      case 227: return "atilde";
      case 228: return "auml";
      case 229: return "aring";
      case 230: return "aelig";
      case 231: return "ccedil";
      case 232: return "egrave";
      case 233: return "eacute";
      case 234: return "ecirc";
      case 235: return "euml";
      case 236: return "igrave";
      case 237: return "iacute";
      case 238: return "icirc";
      case 239: return "iuml";
      case 240: return "eth";
      case 241: return "ntilde";
      case 242: return "ograve";
      case 243: return "oacute";
      case 244: return "ocirc";
      case 245: return "otilde";
      case 246: return "ouml";
      case 247: return "divide";
      case 248: return "oslash";
      case 249: return "ugrave";
      case 250: return "uacute";
      case 251: return "ucirc";
      case 252: return "uuml";
      case 253: return "yacute";
      case 254: return "thorn";
      case 255: return "yuml";
      default: return null;
      }
  }

}
