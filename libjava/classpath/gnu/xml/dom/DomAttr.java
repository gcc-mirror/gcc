/* DomAttr.java --
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

import gnu.java.lang.CPStringBuilder;

import org.w3c.dom.Attr;
import org.w3c.dom.DOMException;
import org.w3c.dom.Element;
import org.w3c.dom.Node;
import org.w3c.dom.TypeInfo;
import org.w3c.dom.events.MutationEvent;


/**
 * <p> "Attr" implementation.  In DOM, attributes cost quite a lot of
 * memory because their values are complex structures rather than just
 * simple strings.  To reduce your costs, avoid having more than one
 * child of an attribute; stick to a single Text node child, and ignore
 * even that by using the attribute's "nodeValue" property.</p>
 *
 * <p> As a bit of general advice, only look at attribute modification
 * events through the DOMAttrModified event (sent to the associated
 * element).  Implementations are not guaranteed to report other events
 * in the same order, so you're very likely to write nonportable code if
 * you monitor events at the "children of Attr" level.</p>
 *
 * <p> At this writing, not all attribute modifications will cause the
 * DOMAttrModified event to be triggered ... only the ones using the string
 * methods (setNodeValue, setValue, and Element.setAttribute) to modify
 * those values.  That is, if you manipulate those children directly,
 * elements won't get notified that attribute values have changed.
 * The natural fix for that will report other modifications, but won't
 * be able to expose "previous" attribute value; it'll need to be cached
 * or something (at which point why bother using child nodes). </p>
 *
 * <p><em>You are strongly advised not to use "children" of any attribute
 * nodes you work with.</em> </p>
 *
 * @author David Brownell
 * @author <a href='mailto:dog@gnu.org'>Chris Burdess</a>
 */
public class DomAttr
  extends DomNsNode
  implements Attr
{

  private boolean specified;
  private String value; // string value cache

  /**
   * Constructs an Attr node associated with the specified document.
   * The "specified" flag is initialized to true, since this DOM has
   * no current "back door" mechanisms to manage default values so
   * that every value must effectively be "specified".
   *
   * <p>This constructor should only be invoked by a Document as part of
   * its createAttribute functionality, or through a subclass which is
   * similarly used in a "Sub-DOM" style layer.
   *
   * @param owner The document with which this node is associated
   * @param namespaceURI Combined with the local part of the name,
   *    this is used to uniquely identify a type of attribute
   * @param name Name of this attribute, which may include a prefix
   */
  protected DomAttr(DomDocument owner, String namespaceURI, String name)
  {
    super(ATTRIBUTE_NODE, owner, namespaceURI, name);
    specified = true;
    length = 1;

    // XXX register self to get insertion/removal events
    // and character data change events and when they happen,
    // report self-mutation
  }

  /**
   * Constructs an Attr node associated with the specified document.
   * The "specified" flag is initialized to true, since this DOM has
   * no current "back door" mechanisms to manage default values so
   * that every value must effectively be "specified".
   *
   * <p>This constructor should only be invoked by a Document as part of
   * its createAttribute functionality, or through a subclass which is
   * similarly used in a "Sub-DOM" style layer.
   * <p>
   * With this constructor, the prefix and local part are given explicitly
   * rather than being computed.  This allows them to be explicitly set to
   * {@code null} as required by {@link Document#createAttribute(String)}.
   * </p>
   *
   * @param owner The document with which this node is associated
   * @param namespaceURI Combined with the local part of the name,
   *    this is used to uniquely identify a type of attribute
   * @param name Name of this attribute, which may include a prefix
   * @param prefix the namespace prefix of the name.  May be {@code null}.
   * @param localName the local part of the name.  May be {@code null}.
   */
  protected DomAttr(DomDocument owner, String namespaceURI, String name,
                    String prefix, String localName)
  {
    super(ATTRIBUTE_NODE, owner, namespaceURI, name, prefix, localName);
    specified = true;
    length = 1;
  }

  /**
   * <b>DOM L1</b>
   * Returns the attribute name (same as getNodeName)
   */
  public final String getName()
  {
    return getNodeName();
  }

  /**
   * <b>DOM L1</b>
   * Returns true if a parser reported this was in the source text.
   */
  public final boolean getSpecified()
  {
    return specified;
  }

  /**
   * Records whether this attribute was in the source text.
   */
  public final void setSpecified(boolean value)
  {
    specified = value;
  }

  /**
   * <b>DOM L1</b>
   * Returns the attribute value, with character and entity
   * references substituted.
   * <em>NOTE:  entity refs as children aren't currently handled.</em>
   */
  public String getNodeValue()
  {
    // If we have a simple node-value, use that
    if (first == null)
      {
        return (value == null) ? "" : value;
      }
    // Otherwise collect child node-values
    CPStringBuilder buf = new CPStringBuilder();
    for (DomNode ctx = first; ctx != null; ctx = ctx.next)
      {
        switch (ctx.nodeType)
          {
          case Node.TEXT_NODE:
            buf.append(ctx.getNodeValue());
            break;
          case Node.ENTITY_REFERENCE_NODE:
            // TODO
            break;
          }
      }
    return buf.toString();
  }

  /**
   * <b>DOM L1</b>
   * Assigns the value of the attribute; it will have one child,
   * which is a text node with the specified value (same as
   * setNodeValue).
   */
  public final void setValue(String value)
  {
    setNodeValue(value);
  }

  /**
   * <b>DOM L1</b>
   * Returns the value of the attribute as a non-null string; same
   * as getNodeValue.
   * <em>NOTE:  entity refs as children aren't currently handled.</em>
   */
  public final String getValue()
  {
    return getNodeValue();
  }

  /**
   * <b>DOM L1</b>
   * Assigns the attribute value; using this API, no entity or
   * character references will exist.
   * Causes a DOMAttrModified mutation event to be sent.
   */
  public void setNodeValue(String value)
  {
    if (readonly)
      {
        throw new DomDOMException(DOMException.NO_MODIFICATION_ALLOWED_ERR);
      }
    if (value == null)
      {
        value = "";
      }
    String oldValue = getNodeValue();
    while (last != null)
      {
        removeChild(last);
      }
    // don't create a new node just for this...
    /*
     Node text = owner.createTextNode(value);
     appendChild(text);
     */
    this.value = value;
    length = 1;
    specified = true;

    mutating(oldValue, value, MutationEvent.MODIFICATION);
  }

  public final Node getFirstChild()
  {
    // Create a child text node if necessary
    if (first == null)
      {
        length = 0;
        Node text = owner.createTextNode((value == null) ? "" : value);
        appendChild(text);
      }
    return first;
  }

  public final Node getLastChild()
  {
    // Create a child text node if necessary
    if (last == null)
      {
        length = 0;
        Node text = owner.createTextNode((value == null) ? "" : value);
        appendChild(text);
      }
    return last;
  }

  public Node item(int index)
  {
    // Create a child text node if necessary
    if (first == null)
      {
        length = 0;
        Node text = owner.createTextNode((value == null) ? "" : value);
        appendChild(text);
      }
    return super.item(index);
  }

  /**
   * <b>DOM L2</b>
   * Returns the element with which this attribute is associated.
   */
  public final Element getOwnerElement()
  {
    return (Element) parent;
  }

  public final Node getNextSibling()
  {
    return null;
  }

  public final Node getPreviousSibling()
  {
    return null;
  }

  public Node getParentNode()
  {
    return null;
  }

  /**
   * Records the element with which this attribute is associated.
   */
  public final void setOwnerElement(Element e)
  {
    if (parent != null)
      {
        throw new DomDOMException(DOMException.HIERARCHY_REQUEST_ERR);
      }
    if (!(e instanceof DomElement))
      {
        throw new DomDOMException(DOMException.WRONG_DOCUMENT_ERR);
      }
    parent = (DomElement) e;
    depth = parent.depth + 1;
  }

  /**
   * The base URI of an Attr is always <code>null</code>.
   */
  public final String getBaseURI()
  {
    return null;
  }

  /**
   * Shallow clone of the attribute, breaking all ties with any
   * elements.
   */
  public Object clone()
  {
    DomAttr retval = (DomAttr) super.clone();
    retval.specified = true;
    return retval;
  }

  private void mutating(String oldValue, String newValue, short why)
  {
    if (!reportMutations || parent == null || equal(newValue, oldValue))
      {
        return;
      }

    // EVENT:  DOMAttrModified, target = parent,
    //  prev/new values provided, also attr name
    MutationEvent       event;

    event = (MutationEvent) createEvent ("MutationEvents");
    event.initMutationEvent ("DOMAttrModified",
                             true /* bubbles */, false /* nocancel */,
                             null, oldValue, newValue, getNodeName (), why);
    parent.dispatchEvent (event);
  }

  // DOM Level 3 methods

  public TypeInfo getSchemaTypeInfo()
  {
    if (parent != null)
      {
        // DTD implementation
        DomDoctype doctype = (DomDoctype) parent.owner.getDoctype();
        if (doctype != null)
          {
            return doctype.getAttributeTypeInfo(parent.getNodeName(),
                                                getNodeName());
          }
        // TODO XML Schema implementation
      }
    return null;
  }

  public boolean isId()
  {
    if (parent != null)
      {
        DomDoctype doctype = (DomDoctype) parent.owner.getDoctype();
        if (doctype != null)
          {
            DTDAttributeTypeInfo info =
              doctype.getAttributeTypeInfo(parent.getNodeName(),
                                           getNodeName());
            if (info != null && "ID".equals(info.type))
              {
                return true;
              }
          }
        DomElement element = (DomElement) parent;
        if (element.userIdAttrs != null &&
            element.userIdAttrs.contains(this))
          {
            return true;
          }
        // TODO XML Schema implementation
      }
    return false;
  }

}
