/* DomImpl.java --
   Copyright (C) 1999,2000,2001,2004 Free Software Foundation, Inc.

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

import org.w3c.dom.Document;
import org.w3c.dom.DocumentType;
import org.w3c.dom.DOMException;
import org.w3c.dom.DOMImplementation;
import org.w3c.dom.Element;
import org.w3c.dom.ls.DOMImplementationLS;
import org.w3c.dom.ls.LSInput;
import org.w3c.dom.ls.LSOutput;
import org.w3c.dom.ls.LSParser;
import org.w3c.dom.ls.LSSerializer;
import gnu.xml.dom.html2.DomHTMLImpl;
import gnu.xml.dom.ls.DomLSInput;
import gnu.xml.dom.ls.DomLSOutput;
import gnu.xml.dom.ls.DomLSParser;
import gnu.xml.dom.ls.DomLSSerializer;

/**
 * <p> "DOMImplementation" implementation. </p>
 *
 * <p> At this writing, the following features are supported:
 * "XML" (L1, L2, L3),
 * "Events" (L2), "MutationEvents" (L2), "USER-Events" (a conformant extension),
 * "HTMLEvents" (L2), "UIEvents" (L2), "Traversal" (L2), "XPath" (L3),
 * "LS" (L3) "LS-Async" (L3).
 * It is possible to compile the package so it doesn't support some of these
 * features (notably, Traversal).
 *
 * @author David Brownell
 * @author <a href='mailto:dog@gnu.org'>Chris Burdess</a>
 */
public class DomImpl
  implements DOMImplementation, DOMImplementationLS
{

  /**
   * Constructs a DOMImplementation object which supports
   * "XML" and other DOM Level 2 features.
   */
  public DomImpl()
  {
  }

  /**
   * <b>DOM L1</b>
   * Returns true if the specified feature and version are
   * supported.  Note that the case of the feature name is ignored.
   */
  public boolean hasFeature(String name, String version)
  {
    if (name.length() == 0)
      {
        return false;
      }
    name = name.toLowerCase();
    if (name.charAt(0) == '+')
      {
        name = name.substring(1);
      }

    if ("xml".equals(name) || "core".equals(name))
      {
        return (version == null ||
                "".equals(version) ||
                "1.0".equals(version) ||
                "2.0".equals(version) ||
                "3.0".equals(version));

      }
    else if ("ls".equals(name) || "ls-async".equals(name))
      {
        return (version == null ||
                "".equals(version) ||
                "3.0".equals(version));
      }
    else if ("events".equals(name)
             || "mutationevents".equals(name)
             || "uievents".equals(name)
             // || "mouseevents".equals(name)
             || "htmlevents".equals(name))
      {
        return (version == null ||
                "".equals(version) ||
                "2.0".equals(version));

        // Extension:  "USER-" prefix event types can
        // be created and passed through the DOM.

      }
    else if ("user-events".equals(name))
      {
        return (version == null ||
                "".equals(version) ||
                "0.1".equals(version));

        // NOTE:  "hasFeature" for events is here interpreted to
        // mean the DOM can manufacture those sorts of events,
        // since actually choosing to report the events is more
        // often part of the environment or application.  It's
        // only really an issue for mutation events.

      }
    else if (DomNode.reportMutations
             && "traversal".equals(name))
      {
        return (version == null ||
                "".equals(version) ||
                "2.0".equals(version));
      }
    else if ("xpath".equals(name))
      {
        return (version == null ||
                "".equals(version) ||
                "3.0".equals(version));
      }
    else if ("html".equals(name) || "xhtml".equals(name))
      {
        return (version == null ||
                "".equals(version) ||
                "2.0".equals(version));
      }

    // views
    // stylesheets
    // css, css2
    // range

    return false;
  }

  /**
   * <b>DOM L2</b>
   * Creates and returns a DocumentType, associated with this
   * implementation.  This DocumentType can have no associated
   * objects(notations, entities) until the DocumentType is
   * first associated with a document.
   *
   * <p> Note that there is no implication that this DTD will
   * be parsed by the DOM, or ever have contents.  Moreover, the
   * DocumentType created here can only be added to a document by
   * the createDocument method(below).  <em>That means that the only
   * portable way to create a Document object is to start parsing,
   * queue comment and processing instruction (PI) nodes, and then only
   * create a DOM Document after <b>(a)</b> it's known if a DocumentType
   * object is needed, and <b>(b) the name and namespace of the root
   * element is known.  Queued comment and PI nodes would then be
   * inserted appropriately in the document prologue, both before and
   * after the DTD node, and additional attributes assigned to the
   * root element.</em>
   *(One hopes that the final DOM REC fixes this serious botch.)
   */
  public DocumentType createDocumentType(String rootName,
                                         String publicId,
                                         String systemId)
    // CR2 deleted internal subset, ensuring DocumentType
    // is 100% useless instead of just 90% so.
  {
    DomDocument.checkNCName(rootName, false);
    return new DomDoctype(this, rootName, publicId, systemId, null);
  }

  /**
   * <b>DOM L2</b>
   * Creates and returns a Document, populated only with a root element and
   * optionally a document type(if that was provided).
   */
  public Document createDocument(String namespaceURI,
                                 String rootName,
                                 DocumentType doctype)
  {
    Document doc = createDocument();
    Element root = null;

    if (rootName != null)
      {
        root = doc.createElementNS(namespaceURI, rootName);
        if (rootName.startsWith("xmlns:"))
          {
            throw new DomDOMException(DOMException.NAMESPACE_ERR,
                                      "xmlns is reserved", null, 0);
          }
      }
    // Bleech -- L2 seemingly _requires_ omission of xmlns attributes.
    if (doctype != null)
      {
        doc.appendChild(doctype);               // handles WRONG_DOCUMENT error
      }
    if (root != null)
      {
        doc.appendChild(root);
      }
    return doc;
  }

  protected Document createDocument()
  {
    return new DomDocument(this);
  }

  // DOM Level 3

  public Object getFeature(String feature, String version)
  {
    if (hasFeature(feature, version))
      {
        if ("html".equalsIgnoreCase(feature) ||
            "xhtml".equalsIgnoreCase(feature))
          {
            return new DomHTMLImpl();
          }
        return this;
      }
    return null;
  }

  // -- DOMImplementationLS --

  public LSParser createLSParser(short mode, String schemaType)
    throws DOMException
  {
    return new DomLSParser(mode, schemaType);
  }

  public LSSerializer createLSSerializer()
  {
    return new DomLSSerializer();
  }

  public LSInput createLSInput()
  {
    return new DomLSInput();
  }

  public LSOutput createLSOutput()
  {
    return new DomLSOutput();
  }

}
