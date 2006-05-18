/* CupsPrintService.java -- Cups specific implementation subclass
   Copyright (C) 2006 Free Software Foundation, Inc.

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


package gnu.javax.print;

import gnu.javax.print.ipp.IppException;
import gnu.javax.print.ipp.IppMultiDocPrintService;
import gnu.javax.print.ipp.IppResponse;

import java.net.URI;

import javax.print.DocFlavor;
import javax.print.attribute.AttributeSet;

/**
 * Implementation of the PrintService/MultiDocPrintService 
 * interface for Cups printers (supports Cups 1.1 and up)
 * 
 * @author Wolfgang Baer (WBaer@gmx.de)
 */
public final class CupsPrintService extends IppMultiDocPrintService
{

  /**
   * Creates a <code>CupsPrintService</code> object.
   * 
   * @param uri the URI of the IPP printer.
   * @param username the user of this print service.
   * @param password the password of the user.
   * 
   * @throws IppException if an error during connection occurs.
   */
  public CupsPrintService(URI uri, String username, String password)
      throws IppException
  {
    super(uri, username, password);
  }  
  
  /**
   * Overridden for CUPS specific handling of the media attribute.
   */
  protected Object handleSupportedAttributeValuesResponse(IppResponse response, 
    Class category)
  {
    //  TODO Implement different behaviour of cups here - actually the Media 
    // printing attribute stuff. For now just use IPP reference implementation.
    return super.handleSupportedAttributeValuesResponse(response, category);
  }

  /**
   * Overridden for CUPS specific handling of the media attribute.
   */
  public Object getDefaultAttributeValue(Class category)
  {
    // TODO Implement media attribute behaviour for cups here
    //if (category.equals(Media.class)
    
    return super.getDefaultAttributeValue(category);
  }

  /**
   * Overridden as CUPS does not implement Validate-Job correctly.
   */
  public AttributeSet getUnsupportedAttributes(DocFlavor flavor, AttributeSet attributes)
  {
    // TODO Implement a heuristic unsupported attribute identification.
    return super.getUnsupportedAttributes(flavor, attributes);
  }
}
