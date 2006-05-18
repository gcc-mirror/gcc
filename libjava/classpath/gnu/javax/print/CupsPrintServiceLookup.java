/* CupsPrintServiceLookup.java -- Implementation based on CUPS
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

import java.util.ArrayList;

import javax.print.DocFlavor;
import javax.print.MultiDocPrintService;
import javax.print.PrintService;
import javax.print.PrintServiceLookup;
import javax.print.attribute.Attribute;
import javax.print.attribute.AttributeSet;

/**
 * The platform default implementation based on CUPS.
 * 
 * @author Wolfgang Baer (WBaer@gmx.de)
 */
public class CupsPrintServiceLookup extends PrintServiceLookup
{  
  private CupsServer server; 

  /**
   * Default constructor checking security access.
   */
  public CupsPrintServiceLookup()
  {
    // security
    SecurityManager sm = System.getSecurityManager();
    if (sm != null)
      sm.checkPrintJobAccess();
     
    // use the localhost cups server
    server = new CupsServer(null, null);
  }

  /**
   * This is the printer marked as default in CUPS.
   * 
   * @return The default lookup service or
   * <code>null</code> if there is no default.
   */
  public PrintService getDefaultPrintService()
  {
    try
      {
        return server.getDefaultPrinter();
      }   
    catch (IppException e)
      {
        // if discovery fails treat as if there is none
        return null;
      }    
  }
  
  /**
  * All printers and printer classes of the CUPS server are checked.
  * If flavors or attributes are null the constraint is not used.
  * 
  * @param flavors the document flavors which have to be supported.
  * @param attributes the attributes which have to be supported.
  * 
  * @return The multidoc print services of the implementing lookup service
  * for the given parameters, or an array of length 0 if none is available.
  */
  public MultiDocPrintService[] getMultiDocPrintServices(DocFlavor[] flavors,
      AttributeSet attributes)
  {
    ArrayList result = new ArrayList();
    PrintService[] services = getPrintServices();   
    
    for (int i=0; i < services.length; i++)
      {
        if (checkMultiDocPrintService(flavors, attributes, services[i]))
          result.add(services[i]);  
      }
    
    return (MultiDocPrintService[]) result.toArray(
      new MultiDocPrintService[result.size()]);
  }

  /**
   * These are all printers and printer classes of the CUPS server.
   * 
   * @return All known print services regardless of supported features, 
   * or an array of length 0 if none is available.
   */
  public PrintService[] getPrintServices()
  {
    ArrayList result = new ArrayList();
    
    try
      {
        result.addAll(server.getAllPrinters());
        result.addAll(server.getAllClasses());
      }
    catch (IppException e)
      {       
        // ignore as this method cannot throw exceptions
        // if print service discovery fails - bad luck
      }
    return (PrintService[]) result.toArray(new PrintService[result.size()]);
  }
  
  
  /**
   * All printers and printer classes of the CUPS server are checked.
   * If flavor or attributes are null the constraint is not used.
   * 
   * @param flavor the document flavor which has to be supported.
   * @param attributes the attributes which have to be supported.
   * 
   * @return The print services of the implementing lookup service
   * for the given parameters, or an array of length 0 if none is available.
   */
  public PrintService[] getPrintServices(DocFlavor flavor,
      AttributeSet attributes)
  {
    ArrayList result = new ArrayList();
    PrintService[] services = getPrintServices();
    
    for (int i=0; i < services.length; i++)
      {
        if (checkPrintService(flavor, attributes, services[i]))
          result.add(services[i]);
      }
    
    return (PrintService[]) result.toArray(new PrintService[result.size()]);
  }
  
  /**
   * Checks the given print service - own method so it can be used also
   * to check application registered print services from PrintServiceLookup.
   * 
   * @param flavor the document flavor which has to be supported.
   * @param attributes the attributes which have to be supported.
   * @param service the service to check
   * 
   * @return <code>true</code> if all constraints match, <code>false</code> 
   * otherwise.
   */
  public boolean checkPrintService(DocFlavor flavor, AttributeSet attributes,
    PrintService service)
  {
    boolean allAttributesSupported = true;
    if (flavor == null || service.isDocFlavorSupported(flavor))
      {
        if (attributes == null || attributes.size() == 0)
          return allAttributesSupported;
       
        Attribute[] atts = attributes.toArray();
        for (int i = 0; i < atts.length; i++)
          {
            if (! service.isAttributeCategorySupported(atts[i].getCategory()))
              {
                allAttributesSupported = false;
                break;
              }
          }
        return allAttributesSupported;
      }
    
    return false;
  }
  
  /**
   * Checks the given print service - own method so it can be used also
   * to check application registered print services from PrintServiceLookup.
   * 
   * @param flavors the document flavors which have to be supported.
   * @param attributes the attributes which have to be supported.
   * @param service the service to check
   * 
   * @return <code>true</code> if all constraints match, <code>false</code> 
   * otherwise.
   */
  public boolean checkMultiDocPrintService(DocFlavor[] flavors, 
    AttributeSet attributes, PrintService service)
  {    
    if (service instanceof MultiDocPrintService)
      { 
        boolean allFlavorsSupported = true;
        boolean allAttributesSupported = true;
        
        if (flavors == null || flavors.length != 0)
          allFlavorsSupported = true;
        else
          {
            for (int k = 0; k < flavors.length; k++)
              {
                if (! service.isDocFlavorSupported(flavors[k]))
                  {
                    allFlavorsSupported = false;
                    break;
                  }
              }
          }
        
        if (attributes == null || attributes.size() == 0)
          allAttributesSupported = true;
        else
          {
            Attribute[] atts = attributes.toArray();
            for (int j = 0; j < atts.length; j++)
              {
                if (! service.isAttributeCategorySupported(
                    atts[j].getCategory()))
                  {
                    allAttributesSupported = false;
                    break;
                  }
              }
          }
        
        if (allAttributesSupported && allFlavorsSupported)
          return true;
      }     
    
    return false;
  }

}
