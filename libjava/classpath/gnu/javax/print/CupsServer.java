/* CupsServer.java -- 
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
import gnu.javax.print.ipp.IppPrintService;
import gnu.javax.print.ipp.IppRequest;
import gnu.javax.print.ipp.IppResponse;
import gnu.javax.print.ipp.attribute.RequestedAttributes;
import gnu.javax.print.ipp.attribute.supported.PrinterUriSupported;

import java.io.IOException;
import java.net.URI;
import java.net.URISyntaxException;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.Set;

/**
 * <code>CupsServer</code> represents a host running a cups
 * compatible server. It mainly consists of its URI and optional
 * user and password combination if access is restricted.
 * <p>
 * It provides methods for retrival of valid CUPS printer uris 
 * that are used to construct IppPrintService objects.
 * </p>
 * 
 * @author Wolfgang Baer (WBaer@gmx.de)
 */
public class CupsServer
{
  /** 
   * The URI of the CUPS server.
   * This is something like: http://localhost:631
   */
  private transient URI uri;
  
  /**
   * The optional username.
   */
  private transient String username;
  
  /**
   * The optional password for the user.
   */
  private transient String password;

  /**
   * Creates a <code>CupsServer</code> object which
   * tries to connect to a cups server. 
   *
   * If <code>gnu.javax.print.server</code> is explicitly set, then
   * that hostname will be used. Otherwise it will default to localhost.
   * 
   * @param username the username
   * @param password the password for the username.
   */
  public CupsServer(String username, String password)
  {
    this.username = username;
    this.password = password;

    this.uri = null;
    try
      {
	String serv = System.getProperty("gnu.javax.print.server");
	if( serv != null )
	  this.uri = new URI("http://"+serv+":631");
      }
    catch(URISyntaxException use)
      {
	throw new RuntimeException("gnu.javax.print.CupsServer value is not a valid hostname.");
      }
    catch(SecurityException se)
      {
      }

    try
      {
	if( this.uri == null )
	  this.uri = new URI("http://localhost:631");
      }
    catch (URISyntaxException e)
      {
        // does not happen
      }
  }
 
  /**
   * Creates a <code>CupsServer</code> object which
   * tries to connect to a running cups server on the
   * given URI.
   * 
   * @param uri the URI of the server.
   * @param username the username
   * @param password the password for the username.
   */
  public CupsServer(URI uri, String username, String password)
  {
    this.uri = uri;
    this.username = username;
    this.password = password;
  }
  
  /**
   * Requests the default printer from this CUPS server.
   * This is always returned as IppPrintService.
   * 
   * @return The default printer.
   * @throws IppException if problems during request/response processing occur.
   */
  public IppPrintService getDefaultPrinter() throws IppException
  {   
    IppResponse response = null;
   
    try
      {
        IppRequest request = new IppRequest(uri, username, password);    
        request.setOperationID((short)CupsIppOperation.CUPS_GET_DEFAULT);        
        request.setOperationAttributeDefaults();    
        
        RequestedAttributes requestedAttrs 
         = new RequestedAttributes("printer-uri-supported");
        request.addOperationAttribute(requestedAttrs);
        
        response = request.send();
      }   
    catch (IOException e)
      {
        throw new IppException("IOException in IPP request/response.", e);
      }    
        
    Map printerAttributes = (Map) response.getPrinterAttributes().get(0);
    Set uris = (Set) printerAttributes.get(PrinterUriSupported.class);
    PrinterUriSupported uri = (PrinterUriSupported) uris.toArray()[0];
    
    IppPrintService service 
      = new CupsPrintService(uri.getURI(), username, password);
    
    return service;
  }
  
  /**
   * Requests all printers from this CUPS server.
   * 
   * @return The list of available printers.
   * @throws IppException if problems during request/response processing occur.
   */
  public List getAllPrinters() throws IppException
  {   
    IppResponse response = null;
   
    try
      {
        IppRequest request = new IppRequest(uri, username, password);    
        request.setOperationID((short)CupsIppOperation.CUPS_GET_PRINTERS);        
        request.setOperationAttributeDefaults();
        
        RequestedAttributes requestedAttrs 
          = new RequestedAttributes("printer-uri-supported");
        request.addOperationAttribute(requestedAttrs);
        
        response = request.send();
      }   
    catch (IOException e)
      {
        throw new IppException("IOException in IPP request/response.", e);
      }    

    List prAttr = response.getPrinterAttributes();
    List services = new ArrayList();
    
    for (int i=0; i < prAttr.size(); i++)
      {
        Map printerAttributes = (Map) prAttr.get(i);
        Set uris = (Set) printerAttributes.get(PrinterUriSupported.class);
        PrinterUriSupported uri = (PrinterUriSupported) uris.toArray()[0];

        try
          {
            CupsPrintService cups = new CupsPrintService(uri.getURI(),
                                                         username, password);
            services.add(cups);
          }
        catch (IppException e)
          {
            // do nothing, we only catch the IppException which could be
            // thrown during instantiation as single printers may be discovered
            // correctly but not usable due to other security restrictions
          }       
      }    
                     
    return services;
  }
  
  /**
   * Requests all classes from this CUPS server. Classes in cups are
   * collections of printers. This means jobs directed to a class 
   * are forwarded to the first available printer of the collection.
   * 
   * @return The list of available classes.
   * @throws IppException if problems during request/response processing occur.
   */
  public List getAllClasses() throws IppException
  {   
    IppResponse response = null;
   
    try
      {
        IppRequest request = new IppRequest(uri, username, password);    
        request.setOperationID((short)CupsIppOperation.CUPS_GET_CLASSES);        
        request.setOperationAttributeDefaults();
        
        RequestedAttributes requestedAttrs 
          = new RequestedAttributes("printer-uri-supported");
        request.addOperationAttribute(requestedAttrs);
        
        response = request.send();
      }   
    catch (IOException e)
      {
        throw new IppException("IOException in IPP request/response.", e);
      }    
    
    List prAttr = response.getPrinterAttributes();
    List services = new ArrayList();   
    
    for (int i=0; i < prAttr.size(); i++)
      {
        Map printerAttributes = (Map) prAttr.get(i);
        Set uris = (Set) printerAttributes.get(PrinterUriSupported.class);
        PrinterUriSupported uri = (PrinterUriSupported) uris.toArray()[0];
        
        try
          {
            CupsPrintService cups = new CupsPrintService(uri.getURI(),
                                                         username, password);
            services.add(cups);
          }
        catch (IppException e)
          {
            // do nothing, we only catch the IppException which could be
            // thrown during instantiation as single printers may be discovered
            // correctly but not usable due to other security restrictions
          }        
      }    
                     
    return services;
  }

}
