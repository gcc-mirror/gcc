/* DomDOMException.java -- 
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

import org.w3c.dom.DOMException;
import org.w3c.dom.Node;

/**
 * <p> DOMException implementation.   The version that
 * is provided by the W3C is abstract, so it can't be instantiated.
 *
 * <p> This also provides a bit more information about the error
 * that is being reported, in terms of the relevant DOM structures
 * and data.
 *
 * @author David Brownell 
 */
public class DomDOMException
  extends DOMException
{

  /** @serial Data that caused an error to be reported */
  private String data;
  
  /** @serial Node associated with the error. */
  private Node node;
  
  /** @serial Data associated with the error. */
  private int value;
  
  /**
   * Constructs an exception, with the diagnostic message
   * corresponding to the specified code.
   */
  public DomDOMException(short code)
  {
    super(code, diagnostic(code));
  }
  
  /**
   * Constructs an exception, with the diagnostic message
   * corresponding to the specified code and additional
   * information as provided.
   */
  public DomDOMException(short code, String data, Node node, int value)
  {
    super(code, diagnostic(code));
    this.data = data;
    this.node = node;
    this.value = value;
  }

  /** Returns the node to which the diagnotic applies, or null. */
  final public Node getNode()
  {
    return node;
  }

  /** Returns data to which the diagnotic applies, or null. */
  final public String getData()
  {
    return data;
  }

  /** Returns data to which the diagnotic applies, or null. */
  final public int getValue()
  {
    return value;
  }

  /**
   * Returns a diagnostic message that may be slightly more useful
   * than the generic one, where possible.
   */
  public String getMessage()
  {
    String retval = super.getMessage();
    
    if (data != null)
      {
        retval += "\nMore Information: " + data;
      }
    if (value != 0)
      {
        retval += "\nNumber: " + value;
      }
    if (node != null)
      {
        retval += "\nNode Name: " + node.getNodeName();
      }
    return retval;
  }

  // these strings should be localizable.
  
  private static String diagnostic(short code)
  {
    switch (code)
      {        
        // DOM L1:
      case INDEX_SIZE_ERR:
        return "An index or size is out of range.";
      case DOMSTRING_SIZE_ERR:
        return "A string is too big.";
      case HIERARCHY_REQUEST_ERR:
        return "The node doesn't belong here.";
      case WRONG_DOCUMENT_ERR:
        return "The node belongs in another document.";
      case INVALID_CHARACTER_ERR:
        return "That character is not permitted.";
      case NO_DATA_ALLOWED_ERR:
        return "This node does not permit data.";
      case NO_MODIFICATION_ALLOWED_ERR:
        return "No changes are allowed.";
      case NOT_FOUND_ERR:
        return "The node was not found in that context.";
      case NOT_SUPPORTED_ERR:
        return "That object is not supported.";
      case INUSE_ATTRIBUTE_ERR:
        return "The attribute belongs to a different element.";
        
        // DOM L2:
      case INVALID_STATE_ERR:
        return "The object is not usable.";
      case SYNTAX_ERR:
        return "An illegal string was provided.";
      case INVALID_MODIFICATION_ERR:
        return "An object's type may not be changed.";
      case NAMESPACE_ERR:
        return "The operation violates XML Namespaces.";
      case INVALID_ACCESS_ERR:
        return "Parameter or operation isn't supported by this node.";
      case TYPE_MISMATCH_ERR:
        return "The type of the argument is incompatible with the expected type.";
      }
    return "Reserved exception number: " + code;
  }

}

