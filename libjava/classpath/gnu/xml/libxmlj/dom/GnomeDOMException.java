/* GnomeDOMException.java - 
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

package gnu.xml.libxmlj.dom;

import org.w3c.dom.DOMException;

class GnomeDOMException
  extends DOMException
{

  GnomeDOMException(short code, String message)
  {
    super(code, createMessage(code, message));
  }

  private static String createMessage(int code, String message)
  {
    if (message != null)
      {
        return message;
      }
    switch (code)
      {
      case INDEX_SIZE_ERR:
        return "INDEX_SIZE_ERR";
      case DOMSTRING_SIZE_ERR:
        return "DOMSTRING_SIZE_ERR";
      case HIERARCHY_REQUEST_ERR:
        return "HIERARCHY_REQUEST_ERR";
      case WRONG_DOCUMENT_ERR:
        return "WRONG_DOCUMENT_ERR";
      case INVALID_CHARACTER_ERR:
        return "INVALID_CHARACTER_ERR";
      case NO_DATA_ALLOWED_ERR:
        return "NO_DATA_ALLOWED_ERR";
      case NO_MODIFICATION_ALLOWED_ERR:
        return "NO_MODIFICATION_ALLOWED_ERR";
      case NOT_FOUND_ERR:
        return "NOT_FOUND_ERR";
      case NOT_SUPPORTED_ERR:
        return "NOT_SUPPORTED_ERR";
      case INUSE_ATTRIBUTE_ERR:
        return "INUSE_ATTRIBUTE_ERR";
      case INVALID_STATE_ERR:
        return "INVALID_STATE_ERR";
      case SYNTAX_ERR:
        return "SYNTAX_ERR";
      case INVALID_MODIFICATION_ERR:
        return "INVALID_MODIFICATION_ERR";
      case NAMESPACE_ERR:
        return "NAMESPACE_ERR";
      case INVALID_ACCESS_ERR:
        return "INVALID_ACCESS_ERR";
      case VALIDATION_ERR:
        return "VALIDATION_ERR";
      case TYPE_MISMATCH_ERR:
        return "TYPE_MISMATCH_ERR";
      default:
        return null;
      }
  }
  
}
