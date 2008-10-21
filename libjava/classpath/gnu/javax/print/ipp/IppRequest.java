/* IppRequest.java -- 
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


package gnu.javax.print.ipp;

import gnu.classpath.debug.Component;
import gnu.classpath.debug.SystemLogger;
import gnu.javax.print.ipp.attribute.CharsetSyntax;
import gnu.javax.print.ipp.attribute.NaturalLanguageSyntax;
import gnu.javax.print.ipp.attribute.RequestedAttributes;
import gnu.javax.print.ipp.attribute.job.AttributesCharset;
import gnu.javax.print.ipp.attribute.job.AttributesNaturalLanguage;
import gnu.javax.print.ipp.attribute.job.JobId;
import gnu.javax.print.ipp.attribute.job.JobUri;
import gnu.javax.print.ipp.attribute.printer.DocumentFormat;

import java.io.DataOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.net.HttpURLConnection;
import java.net.URI;
import java.net.URL;
import java.util.Calendar;
import java.util.Date;
import java.util.GregorianCalendar;
import java.util.List;
import java.util.logging.Logger;

import javax.print.attribute.Attribute;
import javax.print.attribute.AttributeSet;
import javax.print.attribute.DateTimeSyntax;
import javax.print.attribute.EnumSyntax;
import javax.print.attribute.HashAttributeSet;
import javax.print.attribute.IntegerSyntax;
import javax.print.attribute.ResolutionSyntax;
import javax.print.attribute.SetOfIntegerSyntax;
import javax.print.attribute.TextSyntax;
import javax.print.attribute.URISyntax;
import javax.print.attribute.standard.Compression;
import javax.print.attribute.standard.Copies;
import javax.print.attribute.standard.DocumentName;
import javax.print.attribute.standard.Fidelity;
import javax.print.attribute.standard.Finishings;
import javax.print.attribute.standard.JobHoldUntil;
import javax.print.attribute.standard.JobImpressions;
import javax.print.attribute.standard.JobKOctets;
import javax.print.attribute.standard.JobMediaSheets;
import javax.print.attribute.standard.JobName;
import javax.print.attribute.standard.JobOriginatingUserName;
import javax.print.attribute.standard.JobPriority;
import javax.print.attribute.standard.JobSheets;
import javax.print.attribute.standard.Media;
import javax.print.attribute.standard.MultipleDocumentHandling;
import javax.print.attribute.standard.NumberUp;
import javax.print.attribute.standard.OrientationRequested;
import javax.print.attribute.standard.PageRanges;
import javax.print.attribute.standard.PrintQuality;
import javax.print.attribute.standard.PrinterResolution;
import javax.print.attribute.standard.PrinterURI;
import javax.print.attribute.standard.RequestingUserName;
import javax.print.attribute.standard.SheetCollate;
import javax.print.attribute.standard.Sides;

/**
 * <code>IppRequest</code> models a request to an IPP compatible
 * server as described in RFC 2910 - IPP/1.1: Encoding and Transport.
 * <p>
 * The byte stream is structured as follows (for an official description
 * please have a look at the RFC document mentioned above):
 * <ul>
 * <li>version-number          - 2 bytes - required</li>
 * <li>operation-id            - 2 bytes - required</li>
 * <li>request-id              - 4 bytes - required</li>
 * <li>attribute-group         - n bytes - 0 or more</li>
 * <li>end-of-attributes-tag   - 1 byte  - required</li>
 * <li>data                    - q bytes - optional</li>
 * </ul>
 * </p>
 * 
 * @author Wolfgang Baer (WBaer@gmx.de)
 */
public class IppRequest
{

  /**
   * The printer-poll timeout. 
   */
  private static final int timeout = 1000;

  /**
   * Helper class used to write the attributes of a request
   * into the supplied data output stream in the correct way.
   * 
   * @author Wolfgang Baer (WBaer@gmx.de)
   */
  class RequestWriter
  {    
    private DataOutputStream out;
    
    /**
     * Creates a RequestWriter.
     * 
     * @param stream the stream to write to.
     */
    RequestWriter(DataOutputStream stream)
    {
      out = stream;
    }
    
    /**
     * Writes an attribute in IntegerSyntax into the stream.
     * @param attribute the attribute
     * @throws IOException if thrown by the stream
     */
    private void write(IntegerSyntax attribute) throws IOException
    {
      String name = ((Attribute) attribute).getName();
      out.writeByte(IppValueTag.INTEGER);
      out.writeShort(name.length());
      out.write(name.getBytes());
      out.writeShort(4); // length, integer is 4 bytes
      out.writeInt(attribute.getValue());
    }

    /**
     * Writes an attribute in EnumSyntax into the stream.
     * @param attribute the attribute
     * @throws IOException if thrown by the stream
     */
    private void write(EnumSyntax attribute) throws IOException
    {
      // in JPS API enum syntax is used for enums, keyword and boolean types
      String name = ((Attribute) attribute).getName();

      // the enum value types
      if (attribute instanceof Finishings
          || attribute instanceof OrientationRequested
          || attribute instanceof PrintQuality)
        {
          out.writeByte(IppValueTag.ENUM);
          out.writeShort(name.length());
          out.write(name.getBytes());
          out.writeShort(4); // length, enum is 4 bytes
          out.writeInt(attribute.getValue());
        }
      // the boolean value type
      else if (attribute instanceof Fidelity)
        {
          out.writeByte(IppValueTag.BOOLEAN);
          out.writeShort(name.length());
          out.write(name.getBytes());
          out.writeShort(1); // length, boolean is 1 bytes
          out.writeByte(attribute.getValue() == 0 ? 0x00 : 0x01);
        }
      // the keyword value types
      else
        {
          String keyword = attribute.toString();
          out.writeByte(IppValueTag.KEYWORD);
          out.writeShort(name.length());
          out.write(name.getBytes());
          out.writeShort(keyword.length());
          out.write(keyword.getBytes());
        }
    }

    /**
     * Writes an attribute in SetOfIntegerSyntax into the stream.
     * @param attribute the attribute
     * @throws IOException if thrown by the stream
     */
    private void write(SetOfIntegerSyntax attribute) throws IOException
    {
      String name = ((Attribute) attribute).getName();
      int[][] ranges = attribute.getMembers();
      for (int i = 0; i < ranges.length; i++)
        {
          out.writeByte(IppValueTag.RANGEOFINTEGER);
          if (i == 0)
            {
              out.writeShort(name.length());
              out.write(name.getBytes());
            }
          else
            out.writeShort(0x0000); // only name-length 

          out.writeShort(8); // range is 8 bytes
          out.writeInt(ranges[i][0]);
          out.writeInt(ranges[i][1]);
        }
    }

    /**
     * Writes an attribute in ResolutionSyntax into the stream.
     * @param attribute the attribute
     * @throws IOException if thrown by the stream
     */
    private void write(ResolutionSyntax attribute) throws IOException
    {
      String name = ((Attribute) attribute).getName();
      out.writeByte(IppValueTag.RESOLUTION);
      out.writeShort(name.length());
      out.write(name.getBytes());
      out.writeShort(9); // length fixed to 9
      out.writeInt(attribute.getCrossFeedResolution(ResolutionSyntax.DPI));
      out.writeInt(attribute.getFeedResolution(ResolutionSyntax.DPI));
      out.writeByte(ResolutionSyntax.DPI);
    }

    /**
     * Writes an attribute in DateTimeSyntax into the stream.
     * <p>
     * The syntax value is defined as 11 octets follwing the
     * DateAndTime format of RFC 1903. (see IppResponse)
     * </p>
     *
     * @param attribute the attribute
     * @throws IOException if thrown by the stream
     */
    private void write(DateTimeSyntax attribute) throws IOException
    {
      String name = ((Attribute) attribute).getName();
      out.writeByte(IppValueTag.DATETIME);
      out.writeShort(name.length());
      out.write(name.getBytes());
      out.writeShort(11); // length fixed to 11

      Date date = attribute.getValue();
      Calendar cal = new GregorianCalendar();
      cal.setTime(date);

      out.writeShort(cal.get(Calendar.YEAR));
      out.writeByte(cal.get(Calendar.MONTH));
      out.writeByte(cal.get(Calendar.DAY_OF_MONTH));
      out.writeByte(cal.get(Calendar.HOUR_OF_DAY));
      out.writeByte(cal.get(Calendar.MINUTE));
      int second = cal.get(Calendar.SECOND);
      out.writeByte(second == 0 ? 60 : second);
      out.writeByte(cal.get(Calendar.MILLISECOND) / 100);

      int offsetInMillis = cal.get(Calendar.ZONE_OFFSET);
      char directionFromUTC = '+';
      if (offsetInMillis < 0)
        {
          directionFromUTC = '-';
          offsetInMillis = offsetInMillis * (-1);
        }

      out.writeByte(directionFromUTC);
      out.writeByte(offsetInMillis / 3600000); // hours    
      out.writeByte((offsetInMillis % 3600000) / 60000); // minutes
    }

    /**
     * Writes an attribute in TextSyntax into the stream.
     * <p>
     * By default attributes are qritten as TEXT_WITHOUT_LANGUAGE value-tag.
     * As some attributes in the JPS are TextSyntax attributes but actually
     * of NAME value-tag in IPP this method checks for these attributes and
     * writes them as NAME_WITHOUT_LANGUAGE value-tag into the stream.
     * </p>
     * 
     * @param attribute the attribute
     * @param out the stream to write to
     * @throws IOException if thrown by the stream
     */
    private void write(TextSyntax attribute) throws IOException
    {
      // We only use *WithoutLanguage, correct according to spec.
      String name = ((Attribute) attribute).getName();

      if (attribute instanceof RequestingUserName
          || attribute instanceof JobName
          || attribute instanceof DocumentName
          || attribute instanceof JobOriginatingUserName)
        out.writeByte(IppValueTag.NAME_WITHOUT_LANGUAGE);
      else if (attribute instanceof DocumentFormat)
        out.writeByte(IppValueTag.MIME_MEDIA_TYPE);
      else
        out.writeByte(IppValueTag.TEXT_WITHOUT_LANGUAGE);
      
      out.writeShort(name.length());
      out.write(name.getBytes());
      out.writeShort(attribute.getValue().length());
      out.write(attribute.getValue().getBytes());     
    }

    /**
     * Writes an attribute in URISyntax into the stream.
     * @param attribute the attribute
     * @param out the stream to write to
     * @throws IOException if thrown by the stream
     */
    private void write(URISyntax attribute) throws IOException
    {
      // only uriScheme syntax type should not appear
      // in a request (reference-uri-schemes-supported)
      String name = ((Attribute) attribute).getName();
      String uriAscii = attribute.getURI().toASCIIString();
      out.writeByte(IppValueTag.URI);
      out.writeShort(name.length());
      out.write(name.getBytes());
      out.writeShort(uriAscii.length());
      out.write(uriAscii.getBytes());
    }

    /**
     * Writes an attribute in CharsetSyntax into the stream.
     * @param attribute the attribute
     * @param out the stream to write to
     * @throws IOException if thrown by the stream
     */
    private void write(CharsetSyntax attribute) throws IOException
    {      
      String name = ((Attribute) attribute).getName();      
      out.writeByte(IppValueTag.CHARSET);
      out.writeShort(name.length());
      out.write(name.getBytes());
      out.writeShort(attribute.getValue().length());
      out.write(attribute.getValue().getBytes());
    }

    /**
     * Writes an attribute in NaturalLanguageSyntax into the stream.
     * @param attribute the attribute
     * @param out the stream to write to
     * @throws IOException if thrown by the stream
     */
    private void write(NaturalLanguageSyntax attribute) throws IOException
    {
      String name = ((Attribute) attribute).getName();
      out.writeByte(IppValueTag.NATURAL_LANGUAGE);
      out.writeShort(name.length());
      out.write(name.getBytes());
      out.writeShort(attribute.getValue().length());
      out.write(attribute.getValue().getBytes());
    }
    
    /**
     * Writes an attribute in RequestedAttributes into the stream.
     * @param attribute the attribute
     * @param out the stream to write to
     * @throws IOException if thrown by the stream
     */
    private void write(RequestedAttributes attribute) throws IOException
    {
      List values = attribute.getValues();
      
      String name = ((Attribute) attribute).getName();
      out.writeByte(IppValueTag.KEYWORD);
      out.writeShort(name.length());
      out.write(name.getBytes()); 
      out.writeShort(((String) values.get(0)).length());
      out.write(((String) values.get(0)).getBytes());
      
      for (int i=1; i < values.size(); i++)
        {
          out.writeByte(IppValueTag.KEYWORD);
          out.writeShort(0x0000); // length for additional value
          out.writeShort(((String) values.get(i)).length());
          out.write(((String) values.get(i)).getBytes());  
        }
    } 

    
    /**
     * Writes the given operation attribute group of the given map instance
     * (key=group, values=set of attributes) into the supplied data
     * output stream.
     * 
     * @param attributes the set with the attributes.
     * 
     * @throws IOException if thrown by the used DataOutputStream.
     * @throws IppException if unknown attributes occur.
     */
    public void writeOperationAttributes(AttributeSet attributes)
        throws IOException, IppException
    {
      out.write(IppDelimiterTag.OPERATION_ATTRIBUTES_TAG);
      
      // its essential to write these two in this order and as first ones
      Attribute att = attributes.get(AttributesCharset.class);
      write((CharsetSyntax) att);
      
      logger.log(Component.IPP, "Attribute: Name: <" 
        + att.getCategory().getName() + "> Value: <" + att.toString() + ">");  
      
      attributes.remove(AttributesCharset.class);
      
      att = attributes.get(AttributesNaturalLanguage.class);
      write((NaturalLanguageSyntax) att);
      attributes.remove(AttributesNaturalLanguage.class);
      
      logger.log(Component.IPP, "Attribute: Name: <" 
        + att.getCategory().getName() + "> Value: <" + att.toString() + ">");
      
      // furthermore its essential to now write out the target attribute
      PrinterURI printerUri = (PrinterURI) attributes.get(PrinterURI.class);
      JobUri jobUri = (JobUri) attributes.get(JobUri.class);
      JobId jobId = (JobId) attributes.get(JobId.class);
      if (printerUri != null && jobId == null && jobUri == null)
        {
          write(printerUri);
          attributes.remove(PrinterURI.class);
          logger.log(Component.IPP, "Attribute: Name: <" + printerUri
            .getCategory().getName() + "> Value: <" + printerUri.toString() + ">");
        }
      else if (jobUri != null && jobId == null && printerUri == null)
        {
          write(jobUri);
          attributes.remove(JobUri.class);
          logger.log(Component.IPP, "Attribute: Name: <" + jobUri
            .getCategory().getName() + "> Value: <" + jobUri.toString() + ">");
        }
      else if (printerUri != null && jobId != null && jobUri == null)
        {
          write(printerUri); // must be third
          write(jobId);
          attributes.remove(PrinterURI.class);
          attributes.remove(JobId.class);
          logger.log(Component.IPP, "Attribute: Name: <" + printerUri
            .getCategory().getName() + "> Value: <" + printerUri.toString() + ">");
          logger.log(Component.IPP, "Attribute: Name: <" + jobId.getCategory()
            .getName() + "> Value: <" + jobId.toString() + ">");
        }
      else if (jobUri != null && jobId != null)
        {
          write(jobUri);
          attributes.remove(JobUri.class);
          attributes.remove(JobId.class); // MUST NOT redundant
          logger.log(Component.IPP, "Attribute: Name: <" + jobUri.getCategory()
            .getName() + "> Value: <" + jobUri.toString() + ">");
        }
      else
        {
          throw new IppException("Unknown target operation attribute combination.");
        }      
      
      writeAttributes(attributes);
    }
    
    /**
     * Writes the given attribute groups of the given map instance
     * (key=group, values=set of attributes) into the supplied data
     * output stream.
     * 
     * @param attributes the set with the attributes.
     * 
     * @throws IOException if thrown by the used DataOutputStream.
     * @throws IppException if unknown attributes occur.
     */
    public void writeAttributes(AttributeSet attributes)
        throws IOException, IppException
    {
      Attribute[] attributeArray = attributes.toArray();
      for (int i = 0; i < attributeArray.length; i++)
        {
          logger.log(Component.IPP, "Attribute: Name: <" + attributeArray[i]
            .getCategory().getName() + "> Value: <" 
            + attributeArray[i].toString() + ">");          
          
          if (attributeArray[i] instanceof IntegerSyntax)
            write((IntegerSyntax) attributeArray[i]);
          else if (attributeArray[i] instanceof TextSyntax)
            write((TextSyntax) attributeArray[i]);
          else if (attributeArray[i] instanceof DateTimeSyntax)
            write((DateTimeSyntax) attributeArray[i]);
          else if (attributeArray[i] instanceof ResolutionSyntax)
            write((ResolutionSyntax) attributeArray[i]);
          else if (attributeArray[i] instanceof SetOfIntegerSyntax)
            write((SetOfIntegerSyntax) attributeArray[i]);
          else if (attributeArray[i] instanceof EnumSyntax)
            write((EnumSyntax) attributeArray[i]);
          else if (attributeArray[i] instanceof URISyntax)
            write((URISyntax) attributeArray[i]);
          else if (attributeArray[i] instanceof CharsetSyntax)
            write((CharsetSyntax) attributeArray[i]);
          else if (attributeArray[i] instanceof NaturalLanguageSyntax)
            write((NaturalLanguageSyntax) attributeArray[i]);
          else if (attributeArray[i] instanceof RequestedAttributes)
            write((RequestedAttributes) attributeArray[i]);
          else
            throw new IppException("Unknown syntax type");
        }
    }

  }

  /**
   * Logger for tracing - enable by passing
   * -Dgnu.classpath.debug.components=ipp to the vm.
   */
  static final Logger logger = SystemLogger.SYSTEM;

  /**
   * The request id counter simply counts up
   * to give unique request ids per JVM instance.
   */
  private static int requestIdCounter = 1;

  /** The IPP version defaults to 1.1 */
  private static final short VERSION = 0x0101;

  /** Signals if the request is already on its way */
  private boolean alreadySent = false;

  /** The operation type of this request. */
  private short operation_id;

  /** 
   * The request id of this request. This is 
   * assigned automatically by the constructor.
   */
  private final int request_id;

  private AttributeSet operationAttributes;

  private AttributeSet printerAttributes;

  private AttributeSet jobAttributes;

  private Object data;
  
  private URI requestUri;

  /** The underlying connection - IPP is http based */
  private HttpURLConnection  connection;
  
  /**
   * Creates an IPPRequest instance.
   * 
   * @param uri the URI of the request
   * @param user the user if any
   * @param password the password of the supplied user
   */
  public IppRequest(URI uri, String user, String password)
  {   
    request_id = incrementRequestIdCounter();
    requestUri = uri;
    
    try
      {
        URL url = new URL("http", 
                      user == null 
                      ? uri.getHost() : user + ":" 
                      + password + "@" + uri.getHost(), 
                      uri.getPort(), uri.getPath());
       
        connection = (HttpURLConnection) url.openConnection();
        connection.setRequestMethod("POST");
        connection.setDoOutput(true);
        
        connection.setRequestProperty("Content-type", "application/ipp");
        connection.setRequestProperty("Accept", "text/html, image/gif, image/jpeg, *; q=.2, */*; q=.2");        
      }   
    catch (IOException e)
      {
        // MalformedURLException - uri is already checked
        // ProtocolException - POST is correct method type
        // IOException -HTTPURLConnection constructor actually 
        // does never throw this exception.
        logger.log(Component.IPP, "Unexpected IOException", e);
      }
    
    logger.log(Component.IPP, "[IppConnection] Host: " + uri.getHost()
                              + " Port: " + uri.getPort() + " Path: "
                              + uri.getPath());
  }

  /**
   * Synchronized method to be called by the constructor
   * to assign a unique request id to this request.
   * 
   * @return The unique request id.
   */
  private synchronized int incrementRequestIdCounter()
  {
    return IppRequest.requestIdCounter++;
  }

  /**
   * Returns the id of this request.
   * 
   * @return The request ID.
   */
  public int getRequestID()
  {
    return request_id;
  }

  /** 
   * Sets the data of the request. The data used in this 
   * request will be the one of the supplied inputstream
   * instead of the alternative byte array possibility.
   * 
   * @param stream the input stream to use for the data.
   */
  public void setData(InputStream stream)
  {
    data = stream;
  }

  /** 
   * Sets the data of the request. The data used in this 
   * request will be the one of the supplied byte[]
   * instead of the alternative input stream possibility.
   * 
   * @param bytes the byte[] to use for the data.
   */
  public void setData(byte[] bytes)
  {
    data = bytes;
  }

  /**
   * Sets the operation id for this request.
   * 
   * @param id the operation id.
   */
  public void setOperationID(short id)
  {
    operation_id = id;
  }

  /**
   * Adds the default values for the operation
   * attributes "attributes-charset" and 
   * "attributes-natural-language"
   */
  public void setOperationAttributeDefaults()
  {
    if (operationAttributes == null)
      operationAttributes = new HashAttributeSet();

    operationAttributes.add(AttributesCharset.UTF8);
    operationAttributes.add(AttributesNaturalLanguage.EN);
  }
  
  /**
   * Add the job attribute of this request to the given
   * attribute set.
   * 
   * @param attribute the job attribute.
   */
  public void addJobAttribute(Attribute attribute)
  {
    if (jobAttributes == null)
      jobAttributes = new HashAttributeSet();
    
    jobAttributes.add(attribute);
  }
  
  /**
   * Sets the printer attribute of this request to the given
   * attribute set.
   * 
   * @param attribute the printer attribute.
   */
  public void addPrinterAttributes(Attribute attribute)
  {
    if (printerAttributes == null)
      printerAttributes = new HashAttributeSet();
    
    printerAttributes.add(attribute);
  }

  /**
   * Adds the given attribute to the operation attributes set.
   * 
   * @param attribute the operation attribute to add.
   */
  public void addOperationAttribute(Attribute attribute)
  {
    if (operationAttributes == null)
      operationAttributes = new HashAttributeSet();
    
    operationAttributes.add(attribute);
  }
  
  /**
   * Filters from the given attribute set the job operation out
   * and adds them to the operation attributes set.
   * 
   * @param set the attributes to filter, may not be <code>null</code>.
   */
  public void addAndFilterJobOperationAttributes(AttributeSet set)
  {
    if (operationAttributes == null)
      operationAttributes = new HashAttributeSet();
    
    // document-natural-language - not defined in JPS attributes
    // document-format - specified outside, special treatment
    Attribute[] tmp = set.toArray();
    for (int i = 0; i < tmp.length; i++) 
      {        
        if (tmp[i].getCategory().equals(JobName.class)
            || tmp[i].getCategory().equals(Fidelity.class)
            || tmp[i].getCategory().equals(JobImpressions.class)
            || tmp[i].getCategory().equals(JobKOctets.class)
            || tmp[i].getCategory().equals(JobMediaSheets.class)
            || tmp[i].getCategory().equals(Compression.class)
            || tmp[i].getCategory().equals(DocumentName.class)
            || tmp[i].getCategory().equals(RequestingUserName.class))
                
          operationAttributes.add(tmp[i]);            
      }    
  }
  
  /**
   * Filters from the given attribute set the job template attributes
   * out and adds them to the job attributes set.
   * 
   * @param set the attributes to filter, may not be <code>null</code>.
   */
  public void addAndFilterJobTemplateAttributes(AttributeSet set)
  {
    if (jobAttributes == null)
      jobAttributes = new HashAttributeSet();
    
    // document-natural-language - not defined in JPS attributes
    // document-format - specified outside, special treatment
    Attribute[] tmp = set.toArray();
    for (int i = 0; i < tmp.length; i++) 
      {        
        if (tmp[i].getCategory().equals(JobPriority.class)
            || tmp[i].getCategory().equals(JobHoldUntil.class)
            || tmp[i].getCategory().equals(JobSheets.class)
            || tmp[i].getCategory().equals(MultipleDocumentHandling.class)
            || tmp[i].getCategory().equals(Copies.class)
            || tmp[i].getCategory().equals(Finishings.class)
            || tmp[i].getCategory().equals(PageRanges.class)
            || tmp[i].getCategory().equals(NumberUp.class)
            || tmp[i].getCategory().equals(OrientationRequested.class)
            || tmp[i].getCategory().equals(Media.class)
            || tmp[i].getCategory().equals(PrinterResolution.class)
            || tmp[i].getCategory().equals(PrintQuality.class)
            || tmp[i].getCategory().equals(SheetCollate.class)
            || tmp[i].getCategory().equals(Sides.class))
                
          jobAttributes.add(tmp[i]);            
      }    
  }

  /**
   * Does some validation of the supplied parameters and then
   * sends the request to the ipp server or service.
   * 
   * @return The response if any.
   * 
   * @throws IllegalStateException if request is already sent
   * @throws IppException if connection or request failed.
   * @throws IOException if writing of the header, attributes or footer fails. 
   */
  public IppResponse send() throws IppException, IOException
  {
    if (alreadySent)
      throw new IllegalStateException("Request is already sent");
    
    alreadySent = true;
      
    OutputStream stream = connection.getOutputStream();   
    DataOutputStream out = new DataOutputStream(stream);
        
    //  the header 8 bytes long
    out.writeShort(VERSION);
    out.writeShort(operation_id);
    out.writeInt(request_id);
        
    logger.log(Component.IPP, "OperationID: " + Integer.toHexString(operation_id) 
      + " RequestID: " + request_id); 
        
    // Pass stuff the the attribute writer which knows how to
    // write the attributes in correct order
    logger.log(Component.IPP, "Operation Attributes");
        
    RequestWriter writer = new RequestWriter(out);
    writer.writeOperationAttributes(operationAttributes);       
        
    if (jobAttributes != null)
      {
        logger.log(Component.IPP, "Job Attributes");
        out.write(IppDelimiterTag.JOB_ATTRIBUTES_TAG);
        writer.writeAttributes(jobAttributes);
      }           
    if (printerAttributes != null)
      {
        logger.log(Component.IPP, "Printer Attributes");
        out.write(IppDelimiterTag.PRINTER_ATTRIBUTES_TAG);
        writer.writeAttributes(printerAttributes);
      }          

    // write the delimiter to the data
    out.write(IppDelimiterTag.END_OF_ATTRIBUTES_TAG);               

    // check if data is byte[] or inputstream
    if (data instanceof InputStream)
      {
        byte[] readbuf = new byte[2048];
        int len = 0;            
        while( (len = ((InputStream) data).read(readbuf)) > 0)
          out.write(readbuf, 0, len);
      }
    else if (data != null)
      {
        out.write((byte[]) data);
      }
             
    out.flush();
    stream.flush();  
  
    // Set the connection timeout, for if the printer is offline.
    // FIXME: The print services polling should probably be done in its
    // own thread.
    connection.setConnectTimeout( timeout );

    int responseCode = connection.getResponseCode();
    
    if (responseCode == HttpURLConnection.HTTP_OK)
      { 
        IppResponse response = new IppResponse(requestUri, operation_id);        
        response.setResponseData(connection.getInputStream());     
        return response;
      }

    logger.log(Component.IPP, "HTTP-Statuscode: " + responseCode);

    throw new IppException("Request failed got HTTP status code " 
                           + responseCode);
  }

}
