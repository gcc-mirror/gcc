/* PrintServiceLookup.java --
   Copyright (C) 2004, 2006 Free Software Foundation, Inc.

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


package javax.print;

import gnu.classpath.ServiceFactory;
import gnu.javax.print.CupsPrintServiceLookup;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashSet;
import java.util.Iterator;

import javax.print.attribute.AttributeSet;


/**
 * <code>PrintServiceLookup</code> implementations provide a way to lookup 
 * print services based on different constraints.
 * <p>
 * Implementations are located and loaded automatically through the SPI JAR 
 * file specification. Therefore implementation classes must provide a default 
 * constructor for instantiation. Furthermore, applications are able to 
 * register further instances directly at runtime.
 * </p><p>
 * If an SecurityManager is installed implementors should call 
 * <code>checkPrintJobAccess()</code> to disable access for untrusted code. 
 * This check is to be made in every lookup service implementation for 
 * flexibility. Print services registered by applications through 
 * <code>registerService(PrintService)</code> are suppressed in the 
 * lookup results if a security manager is installed and disallows access. 
 * </p>
 * 
 * @author Michael Koch (konqueror@gmx.de)
 * @author Wolfgang Baer (WBaer@gmx.de)
 */
public abstract class PrintServiceLookup
{
  
  private static final CupsPrintServiceLookup systemProvider;
  private static final HashSet printServices;
  private static final HashSet printServiceLookups;
  
  static
  {
    systemProvider = new CupsPrintServiceLookup();
    
    printServices = new HashSet();
    printServiceLookups = new HashSet();
    
    // check for service providers
    Iterator it = ServiceFactory.lookupProviders(PrintServiceLookup.class);
    
    while (it.hasNext())
      printServiceLookups.add(it.next());    
  }  
  
  /**
   * Constructs a <code>PrintServiceLookup</code> object.
   */
  public PrintServiceLookup()
  {
    // nothing to do here 
  }
  
  /**
   * Explicitly registers the provided print service lookup implementation.
   * <p>
   * The registration will silently fail (returning <code>false</code>) if
   * the lookup service is already registered or the registration somehow
   * else fails.
   * </p>
   *
   * @param sp the print service lookup implementation to register. 
   * @return <code>true</code> if registered, <code>false</code> otherwise.
   */
  public static boolean registerServiceProvider(PrintServiceLookup sp)
  {  
    return printServiceLookups.add(sp);
  }
  
  /**
   * Explicitly registers the provided print service instance.
   * <p>
   * The registration will silently fail (returning <code>false</code>) if
   * the print service instance is already registered or the registration 
   * somehow else fails.
   * </p>
   * @param service the single print service to register. 
   * @return <code>true</code> if registered, <code>false</code> otherwise.
   */
  public static boolean registerService(PrintService service)
  {
    if (service instanceof StreamPrintService)
      return false;

    // security
    try
      {
        SecurityManager sm = System.getSecurityManager();
        if (sm != null)
          sm.checkPrintJobAccess();

        return printServices.add(service);
      }
    catch (SecurityException se)
      {
        return false;
      }
  }
  
  /**
   * Searches print services capable of printing in the given document flavor
   * which supports the specified printing attributes.
   * 
   * @param flavor the document flavor to support. If <code>null</code> this 
   * constraint is ignored during lookup.
   * @param attributes the printing attributes to support. If 
   * <code>null</code> this constraint is ignored during lookup.
   * @return The resulting available print services, or an array of length 0 
   * if none is found. 
   */
  public static final PrintService[] lookupPrintServices(DocFlavor flavor,
    AttributeSet attributes)
  {   
    ArrayList result = new ArrayList();
    
    PrintService[] services = 
      systemProvider.getPrintServices(flavor, attributes);    
    result.addAll(Arrays.asList(services));
             
    for (Iterator it = printServiceLookups.iterator(); it.hasNext(); )
      {
        PrintServiceLookup lookup = (PrintServiceLookup) it.next();
        services = lookup.getPrintServices(flavor, attributes);   
        result.addAll(Arrays.asList(services));
      }
    
    for (Iterator it = printServices.iterator(); it.hasNext(); )
      {
        PrintService service = (PrintService) it.next();
        if (systemProvider.checkPrintService(flavor, attributes, service)) 
          result.add(service);
      }
    
    return (PrintService[]) result.toArray(new PrintService[result.size()]);
  }
  
  /**
   * Searches print services capable of multi document printing in all of the 
   * given document flavors and supporting the specified printing attributes.
   * 
   * @param flavors the document flavors to support. If <code>null</code> this 
   * constraint is ignored during lookup.
   * @param attributes the printing attributes to support. If 
   * <code>null</code> this constraint is ignored during lookup.
   * @return The resulting available multi document print services, or an 
   * array of length 0 if none is found. 
   */
  public static final MultiDocPrintService[] lookupMultiDocPrintServices(
    DocFlavor[] flavors, AttributeSet attributes)
  {
    ArrayList result = new ArrayList();
    
    MultiDocPrintService[] services = 
      systemProvider.getMultiDocPrintServices(flavors, attributes);    
    result.addAll(Arrays.asList(services));
             
    for (Iterator it = printServiceLookups.iterator(); it.hasNext(); )
      {
        PrintServiceLookup lookup = (PrintServiceLookup) it.next();
        services = lookup.getMultiDocPrintServices(flavors, attributes);   
        result.addAll(Arrays.asList(services));
      }
    
    for (Iterator it = printServices.iterator(); it.hasNext(); )
      {
        PrintService service = (PrintService) it.next();
        if (systemProvider.checkMultiDocPrintService(flavors, attributes, service))   
          result.add(service);
      }
    
    return (MultiDocPrintService[]) result.toArray(
      new MultiDocPrintService[result.size()]);
  }


  /**
   * Searches the default print service in the current environment.
   * <p>
   * If multiple lookup services are registered and each has a default
   * print service the result is not specified. Usually the default 
   * print service of the native platform lookup service is returned.
   * </p><p>
   * The GNU classpath implementation will return the CUPS default
   * printing service as the default print service, if available.
   * </p><p>
   * The default print service may be overriden by users through
   * the property <code>javax.print.defaultPrinter</code>. A service 
   * specified must be found to be returned as the default.
   * </p>
   *  
   * @return The default print service, or <code>null</code> if none found.
   */
  public static final PrintService lookupDefaultPrintService()
  {
    // TODO Find out what the property controls and use it
    // String defaultPrinter = System.getProperty("javax.print.defaultPrinter");
       
    // first test for platform specified default services
    PrintService service = systemProvider.getDefaultPrintService();
    
    if (service != null) 
      return service;
          
    // none available by systemDefaultProvider
    // search in other registered ones and take first      
    for (Iterator it = printServiceLookups.iterator(); it.hasNext(); )
      {
        service = ((PrintServiceLookup) it.next()).getDefaultPrintService();
        if (service != null)
          return service;
      }

    return null;
  }
  
  /**
   * Not to be called directly by applications.
   * 
   * @return The default lookup service of the implementing lookup service or
   * <code>null</code> if there is no default one.
   */
  public abstract PrintService getDefaultPrintService();

  /**
   * Not to be called directly by applications.
   * 
   * @param flavors the document flavors which have to be supported.
   * @param attributes the attributes which have to be supported.
   * 
   * @return The multidoc print services of the implementing lookup service
   * for the given parameters, or an array of length 0 if none is available.
   */
  public abstract MultiDocPrintService[] 
    getMultiDocPrintServices(DocFlavor[] flavors, AttributeSet attributes);

  /**
   * Not to be called directly by applications.
   * 
   * @return All known print services of the implementing lookup service
   * regardless of supported features, or an array of length 0 if none is 
   * available.
   */
  public abstract PrintService[] getPrintServices();

  /**
   * Not to be called directly by applications.
   * 
   * @param flavor the document flavor which has to be supported.
   * @param attributes the attributes which have to be supported.
   * 
   * @return The print services of the implementing lookup service
   * for the given parameters, or an array of length 0 if none is available.
   */
  public abstract PrintService[] 
    getPrintServices(DocFlavor flavor, AttributeSet attributes);
}
