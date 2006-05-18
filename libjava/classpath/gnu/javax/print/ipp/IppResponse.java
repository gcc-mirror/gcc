/* IppResponse.java -- 
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
import gnu.javax.print.ipp.attribute.UnknownAttribute;
import gnu.javax.print.ipp.attribute.defaults.DocumentFormatDefault;
import gnu.javax.print.ipp.attribute.defaults.JobHoldUntilDefault;
import gnu.javax.print.ipp.attribute.defaults.JobSheetsDefault;
import gnu.javax.print.ipp.attribute.defaults.MediaDefault;
import gnu.javax.print.ipp.attribute.defaults.PrinterResolutionDefault;
import gnu.javax.print.ipp.attribute.job.AttributesCharset;
import gnu.javax.print.ipp.attribute.job.AttributesNaturalLanguage;
import gnu.javax.print.ipp.attribute.job.JobMoreInfo;
import gnu.javax.print.ipp.attribute.job.JobPrinterUri;
import gnu.javax.print.ipp.attribute.job.JobUri;
import gnu.javax.print.ipp.attribute.printer.CharsetConfigured;
import gnu.javax.print.ipp.attribute.printer.DocumentFormat;
import gnu.javax.print.ipp.attribute.printer.NaturalLanguageConfigured;
import gnu.javax.print.ipp.attribute.printer.PrinterCurrentTime;
import gnu.javax.print.ipp.attribute.printer.PrinterDriverInstaller;
import gnu.javax.print.ipp.attribute.supported.CharsetSupported;
import gnu.javax.print.ipp.attribute.supported.DocumentFormatSupported;
import gnu.javax.print.ipp.attribute.supported.GeneratedNaturalLanguageSupported;
import gnu.javax.print.ipp.attribute.supported.JobHoldUntilSupported;
import gnu.javax.print.ipp.attribute.supported.JobSheetsSupported;
import gnu.javax.print.ipp.attribute.supported.MediaSupported;
import gnu.javax.print.ipp.attribute.supported.PrinterResolutionSupported;
import gnu.javax.print.ipp.attribute.supported.PrinterUriSupported;

import java.io.ByteArrayOutputStream;
import java.io.DataInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.net.URI;
import java.net.URISyntaxException;
import java.util.ArrayList;
import java.util.Calendar;
import java.util.Date;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.logging.Logger;

import javax.print.attribute.Attribute;
import javax.print.attribute.standard.CopiesSupported;
import javax.print.attribute.standard.DateTimeAtCompleted;
import javax.print.attribute.standard.DateTimeAtCreation;
import javax.print.attribute.standard.DateTimeAtProcessing;
import javax.print.attribute.standard.JobImpressionsSupported;
import javax.print.attribute.standard.JobKOctetsSupported;
import javax.print.attribute.standard.JobMediaSheetsSupported;
import javax.print.attribute.standard.JobStateReason;
import javax.print.attribute.standard.JobStateReasons;
import javax.print.attribute.standard.NumberUpSupported;
import javax.print.attribute.standard.PrinterMoreInfo;
import javax.print.attribute.standard.PrinterMoreInfoManufacturer;
import javax.print.attribute.standard.PrinterStateReason;
import javax.print.attribute.standard.PrinterStateReasons;
import javax.print.attribute.standard.Severity;

/**
 * <code>IppResponse</code> models a response received from an IPP 
 * compatible server as described in RFC 2910 IPP 1.1 Encoding and Transport.
 *  
 * @author Wolfgang Baer (WBaer@gmx.de)
 */
public class IppResponse
{

  /**
   * <code>ResponseReader</code> is responsible for parsing an IPP 1.1
   * response stream. It provides access to the attribute groups after parsing
   * via getter methods.
   * <p>
   * The enconding of a response is structured as follows (for an official 
   * description please have a look at the RFC document mentioned above):
   * <ul>
   * <li>version-number            - 2 bytes - required</li>
   * <li>status-code               - 2 bytes - required</li>
   * <li>request-id                - 4 bytes - required</li>
   * <li>attribute-group           - n bytes - 0 or more</li>
   * <li>end-of-attributes-tag     - 1 byte  - required</li>
   * <li>data                      - q bytes - optional</li>
   * </ul>
   * </p><p> 
   * Where each attribute-group (if any) is encoded as follows:
   * <ul>
   * <li>begin-attribute-group-tag - 1 byte</li>
   * <li>attribute                 - p bytes - 0 or more</li>
   * </ul>
   * </p><p> 
   * Encoding of attributes:
   * <ul>
   * <li>attribute-with-one-value - q bytes</li>
   * <li>additional-value         - r bytes  - 0 or more</li>
   * </ul>
   * </p><p>  
   * Encoding of attribute-with-one-value:
   * <ul>
   * <li>value-tag                  - 1 byte</li>
   * <li>name-length  (value is u)  - 2 bytes</li>
   * <li>name                       - u bytes</li>
   * <li>value-length  (value is v) - 2 bytes</li>
   * <li>value                      - v bytes</li>
   * </ul>
   * </p><p> 
   * Encoding of additional value:
   * <ul>
   * <li>value-tag                       - 1 byte</li>
   * <li>name-length  (value is 0x0000)  - 2 bytes</li>
   * <li>value-length (value is w)       - 2 bytes</li>
   * <li>value                           - w bytes</li>
   * </ul>
   * </p>
   * 
   * @author Wolfgang Baer (WBaer@gmx.de)
   */
  class ResponseReader
  {
    /** The IPP version defaults to 1.1 */
    private static final short VERSION = 0x0101;
   
    /**
     * Parses the inputstream containing the response of the IPP request.
     * @param input the inputstream
     * @throws IppException if unexpected exceptions occur.
     * @throws IOException if IO problems with the underlying inputstream occur.
     */
    public void parseResponse(InputStream input) 
        throws IppException, IOException
    {
      DataInputStream stream = new DataInputStream(input);

      short version = stream.readShort();
      status_code = stream.readShort();
      request_id = stream.readInt();

      if (VERSION != version)
        throw new IppException("Version mismatch - "
          + "implementation does not support other versions than IPP 1.1");

      logger.log(Component.IPP, "Statuscode: " 
        + Integer.toHexString(status_code) + " Request-ID: " + request_id);

      byte tag = 0;
      boolean proceed = true;
      HashMap tmp;
      // iterate over attribute-groups until end-of-attributes-tag is found
      while (proceed)
        {
          if (tag == 0) // only at start time
            tag = stream.readByte();

          logger.log(Component.IPP, "DelimiterTag: " + Integer.toHexString(tag));

          // check if end of attributes
          switch (tag)
            {
            case IppDelimiterTag.END_OF_ATTRIBUTES_TAG:
              proceed = false;
              break;
            case IppDelimiterTag.OPERATION_ATTRIBUTES_TAG:
              tmp = new HashMap();
              tag = parseAttributes(tmp, stream);
              operationAttributes.add(tmp);
              break;
            case IppDelimiterTag.JOB_ATTRIBUTES_TAG:
              tmp = new HashMap();
              tag = parseAttributes(tmp, stream);
              jobAttributes.add(tmp);
              break;
            case IppDelimiterTag.PRINTER_ATTRIBUTES_TAG:
              tmp = new HashMap();
              tag = parseAttributes(tmp, stream);
              printerAttributes.add(tmp);
              break;
            case IppDelimiterTag.UNSUPPORTED_ATTRIBUTES_TAG:
              System.out.println("Called");
              tmp = new HashMap();
              tag = parseAttributes(tmp, stream);
              unsupportedAttributes.add(tmp);
              break;
            default:
              throw new IppException("Unknown tag with value "
                                     + Integer.toHexString(tag) + " occured.");
            }
        }

      // if there are more bytes that has to be data.
      ByteArrayOutputStream byteStream = new ByteArrayOutputStream();
      byte[] readbuf = new byte[2048];
      int len = 0;

      while ((len = stream.read(readbuf)) > 0)
        byteStream.write(readbuf, 0, len);

      byteStream.flush();
      data = byteStream.toByteArray();
    }

    /**
     * The actual parsing of the attributes and further putting into the
     * provided group maps.
     * @param attributes the provided attribute group map.
     * @param stream the provided stream to read from.
     * @return The last read tag byte (normally a DelimiterTag)
     * @throws IppException if unexpected exceptions occur.
     * @throws IOException if IO problems with the underlying inputstream occur.
     */
    private byte parseAttributes(Map attributes, DataInputStream stream)
        throws IppException, IOException
    {
      Attribute lastAttribute = null;
      Attribute attribute = null;

      // declaration of variables
      short nameLength;
      String name;
      short valueLength;
      byte[] value;

      // tmp variables for parsing
      // declared here so no name duplication occurs
      URI uri;
      String str;

      while (true)
        {
          byte tag = stream.readByte();

          if (IppDelimiterTag.isDelimiterTag(tag))
            return tag;

          // it must be a value tag now
          // so we have either a attribute-with-one-value
          // or (if setOf is possible) an additional-value

          // (1) Length of the name
          nameLength = stream.readShort();

          // (2) The name itself
          // may be an additional-value
          if (nameLength == 0x0000)
            name = lastAttribute.getName();
          else
            {
              byte[] nameBytes = new byte[nameLength];
              stream.read(nameBytes);
              name = new String(nameBytes);
            }

          // (3) Length of the value
          valueLength = stream.readShort();

          // (4) The value itself
          value = new byte[valueLength];
          stream.read(value);       

          // the value itself
          switch (tag)
            {
            // out-of-band values
            case IppValueTag.UNSUPPORTED:
            case IppValueTag.UNKNOWN:
            case IppValueTag.NO_VALUE:
              // TODO implement out-of-band handling
              // We currently throw an exception to see when it occurs - not yet :-)              
              throw new IppException(
                    "Unexpected name value for out-of-band value tag");
            case IppValueTag.INTEGER:
              int intValue = IppUtilities.convertToInt(value);
              attribute = IppUtilities.getIntegerAttribute(name, intValue);

              break;
            case IppValueTag.BOOLEAN:
              // JPS API models boolean syntax type as enums
              // 0x01 = true, 0x00 = false - all are enums
              attribute = IppUtilities.getEnumAttribute(name, new Integer(value[0]));
              
              break;
            case IppValueTag.ENUM:
              int intVal = IppUtilities.convertToInt(value);            
              attribute = IppUtilities.getEnumAttribute(name, new Integer(intVal));    

              break;
            case IppValueTag.OCTECTSTRING_UNSPECIFIED:
              // none exists according to spec
              // so lets report as exception to see when it occurs
              throw new IppException("Unspecified octet string occured.");

            case IppValueTag.DATETIME:
              Date date = parseDate(value);
              if (name.equals("printer-current-time"))
                attribute = new PrinterCurrentTime(date);
              else if (name.equals("date-time-at-creation"))
                attribute = new DateTimeAtCreation(date);
              else if (name.equals("date-time-at-processing"))
                attribute = new DateTimeAtProcessing(date);
              else if (name.equals("date-time-at-completed"))
                attribute = new DateTimeAtCompleted(date);

              break;
            case IppValueTag.RESOLUTION:
              int crossFeed = IppUtilities.convertToInt(value[0], value[1], value[2], value[3]);
              int feed = IppUtilities.convertToInt(value[4], value[5], value[6], value[7]);
              int units = value[8];
              
              if (name.equals("printer-resolution-default"))
                attribute = new PrinterResolutionDefault(crossFeed, feed, units);
              else if (name.equals("printer-resolution-supported")) // may be here also
                attribute = new PrinterResolutionSupported(crossFeed, feed, units);

              break;
            case IppValueTag.RANGEOFINTEGER:
              int lower = IppUtilities.convertToInt(value[0], value[1], value[2], value[3]);
              int upper = IppUtilities.convertToInt(value[4], value[5], value[6], value[7]);

              if (name.equals("copies-supported"))
                attribute = new CopiesSupported(lower, upper);
              else if (name.equals("number-up-supported"))
                attribute = new NumberUpSupported(lower, upper);
              else if (name.equals("job-k-octets-supported")) 
                attribute = new JobKOctetsSupported(lower, upper);
              else if (name.equals("job-impressions-supported"))
                attribute = new JobImpressionsSupported(lower, upper);
              else if (name.equals("job-media-sheets-supported"))
                attribute = new JobMediaSheetsSupported(lower, upper);

              break;
            case IppValueTag.TEXT_WITH_LANGUAGE:
            case IppValueTag.TEXT_WITHOUT_LANGUAGE:
            case IppValueTag.NAME_WITH_LANGUAGE:
            case IppValueTag.NAME_WITHOUT_LANGUAGE:
              attribute = IppUtilities.getTextAttribute(name, tag, value);
             
              break;
            case IppValueTag.KEYWORD:
              str = new String(value);
              if (name.equals("job-hold-until-supported")) // may also be name type
                attribute = new JobHoldUntilSupported(str, null);
              else if (name.equals("job-hold-until-default"))
                attribute = new JobHoldUntilDefault(str, null);             
              else if (name.equals("media-supported"))
                attribute = new MediaSupported(str, null);
              else if (name.equals("media-default"))
                  attribute = new MediaDefault(str, null);
              else if (name.equals("job-sheets-default")) 
                attribute = new JobSheetsDefault(str, null);
              else if (name.equals("job-sheets-supported"))
                attribute = new JobSheetsSupported(str, null);
              else if (name.equals("job-state-reasons")) // setOf
                attribute = parseJobStateReasons(value, lastAttribute);
              else if (name.equals("printer-state-reasons")) // setOf
                attribute = parsePrinterStateReasons(value, lastAttribute);
              else
                attribute = IppUtilities.getEnumAttribute(name, str);
              
              // all other stuff is either an enum or needs to be mapped to an 
              // UnknownAttribute instance. Enums catched here are: 
              // ipp-versions-supported, pdl-override-supported, compression-supported
              // uri-authentication-supported, uri-security-supported, sides-supported
              // sides-default, multiple-document-handling-supported, multiple-document-handling-default
                            
              break;
            case IppValueTag.URI:
              try
                {
                  uri = new URI(new String(value));
                }
              catch (URISyntaxException e)
                {
                  throw new IppException("Wrong URI syntax encountered.", e);
                }

              if (name.equals("job-uri"))
                attribute = new JobUri(uri);
              else if (name.equals("job-printer-uri"))
                attribute = new JobPrinterUri(uri);
              else if (name.equals("job-more-info"))
                attribute = new JobMoreInfo(uri);
              else if (name.equals("printer-uri-supported")) // setOf
                attribute = new PrinterUriSupported(uri);
              else if (name.equals("printer-more-info"))
                attribute = new PrinterMoreInfo(uri);
              else if (name.equals("printer-driver-installer"))
                attribute = new PrinterDriverInstaller(uri);
              else if (name.equals("printer-more-info-manufacturer"))
                attribute = new PrinterMoreInfoManufacturer(uri);
              
              break;
            case IppValueTag.URI_SCHEME:
              // only one uri-scheme exists - and its an enum
              if (name.equals("reference-uri-schemes-supported"))
                attribute = IppUtilities.getEnumAttribute(name, new String(value));
              
              break;
            case IppValueTag.CHARSET:
              str = new String(value);
              if (name.equals("attributes-charset"))
                attribute = new AttributesCharset(str);
              else if (name.equals("charset-configured"))
                attribute = new CharsetConfigured(str);
              else if (name.equals("charset-supported")) // setOf
                attribute = new CharsetSupported(str);
              
              break;
            case IppValueTag.NATURAL_LANGUAGE:
              str = new String(value);
              if (name.equals("attributes-natural-language"))
                attribute = new AttributesNaturalLanguage(str);
              else if (name.equals("natural-language-configured"))
                attribute = new NaturalLanguageConfigured(str);
              else if (name.equals("generated-natural-language-supported")) // setOf
                attribute = new GeneratedNaturalLanguageSupported(str);
              
              break;
            case IppValueTag.MIME_MEDIA_TYPE:
              str = new String(value);
              if (name.equals("document-format-default"))
                attribute = new DocumentFormatDefault(str, null);
              else if (name.equals("document-format-supported")) // setOf
                attribute = new DocumentFormatSupported(str, null);
              else if (name.equals("document-format")) // setOf
                attribute = new DocumentFormat(str, null);
              
              break;
            default:
              throw new IppException("Unknown tag with value "
                                     + Integer.toHexString(tag) + " found.");
            }

          if (attribute == null)
            attribute =  new UnknownAttribute(tag, name, value);
          
          addAttribute(attributes, attribute);
          lastAttribute = attribute;
          
          logger.log(Component.IPP, "Attribute: " + name
                     + " Value: " + attribute.toString());
        }
    }

    /**
     * Adds a new attribute to the given attribute group. If this is the fist
     * occurence of this attribute category a new set is created and associated
     * with its category as key.
     * @param attributeGroup
     *          the attribute group
     * @param attribute
     *          the attribute to add
     */
    private void addAttribute(Map attributeGroup, Attribute attribute)
    {
      Class clazz = attribute.getCategory();
      Set attributeValues = (Set) attributeGroup.get(clazz);

      if (attributeValues == null) // first attribute of this category
        {
          attributeValues = new HashSet();
          attributeGroup.put(clazz, attributeValues);
        }

      attributeValues.add(attribute);
    }
    
    /**
     * Parses a name with or without language attribute value from the byte[]
     * and returns the result as an object[].
     * @param value the byte[]
     * @param lastAttr the last attribute
     * @return The attribute.
     */
    private PrinterStateReasons parsePrinterStateReasons(byte[] value, Attribute lastAttr)
    {
      String str = new String(value);
      PrinterStateReasons attribute; 
      
      if (lastAttr instanceof PrinterStateReasons)
        attribute = (PrinterStateReasons) lastAttr;
      else
        attribute = new PrinterStateReasons();
      
      // special case indicating no reasons
      if (str.equals("none")) 
        return attribute;
      
      Severity severity = null;
      PrinterStateReason reason = null;
      
      if (str.endsWith(Severity.WARNING.toString()))
        severity = Severity.WARNING;
      else if (str.endsWith(Severity.REPORT.toString()))
        severity = Severity.REPORT;
      else if (str.endsWith(Severity.ERROR.toString()))
        severity = Severity.ERROR;
      
      if (severity != null)
        str = str.substring(0, str.lastIndexOf('-'));    
      else // we must associate a severity 
        severity = Severity.REPORT;
      
      reason = (PrinterStateReason) 
        IppUtilities.getEnumAttribute("printer-state-reason", str);
      
      attribute.put(reason , severity);
      return attribute;
    }
    
    /**
     * Parses a name with or without language attribute value from the byte[]
     * and returns the result as an object[].
     * @param value the byte[]
     * @param lastAttr the last attribute
     * @return The attribute.
     */
    private JobStateReasons parseJobStateReasons(byte[] value, Attribute lastAttr)
    {
      String str = new String(value);
      JobStateReasons attribute; 
      
      if (lastAttr instanceof JobStateReasons)
        attribute = (JobStateReasons) lastAttr;
      else
        attribute = new JobStateReasons();
      
      // special case indicating no reasons
      if (str.equals("none")) 
        return attribute;
      
      JobStateReason reason = (JobStateReason) 
        IppUtilities.getEnumAttribute("job-state-reason", str);
      
      attribute.add(reason);
      return attribute;
    }
    
    /**
     * Parses a DateTime syntax attribute and returns the constructed Date
     * object.
     * <p>
     * The syntax value is defined as 11 octets follwing the DateAndTime format
     * of RFC 1903:
     * <ul>
     * <li>field | octets | contents | range</li>
     * <li>1 | 1-2 | year | 0..65536</li>
     * <li>2 | 3 | month | 1..12</li>
     * <li>3 | 4 | day | 1..31</li>
     * <li>4 | 5 | hour | 0..23</li>
     * <li>5 | 6 | minutes | 0..59</li>
     * <li>6 | 7 | seconds | 0..60 (use 60 for leap-second)</li>
     * <li>7 | 8 | deci-seconds | 0..9</li>
     * <li>8 | 9 | direction from UTC | '+' / '-'</li>
     * <li>9 | 10 | hours from UTC | 0..11</li>
     * <li>10 | 11 | minutes from UTC | 0..59</li>
     * </ul>
     * </p>
     * 
     * @param value the byte[]
     * @return The date object.
     */
    private Date parseDate(byte[] value)
    {
      short year = IppUtilities.convertToShort(value[0], value[1]);

      Calendar cal = Calendar.getInstance();
      cal.set(Calendar.YEAR, year);
      cal.set(Calendar.MONTH, value[2]);
      cal.set(Calendar.DAY_OF_MONTH, value[3]);
      cal.set(Calendar.HOUR_OF_DAY, value[4]);
      cal.set(Calendar.MINUTE, value[5]);
      cal.set(Calendar.SECOND, value[6]);
      cal.set(Calendar.MILLISECOND, value[7] * 100); // deci-seconds

      // offset from timezone
      int offsetMilli = value[9] * 3600000; // hours to millis
      offsetMilli = offsetMilli + value[10] * 60000; // minutes to millis

      if (((char) value[8]) == '-')
        offsetMilli = offsetMilli * (-1);

      cal.set(Calendar.ZONE_OFFSET, offsetMilli);
      return cal.getTime();
    }
  }
  
  /**
   * Logger for tracing - enable by passing
   * -Dgnu.classpath.debug.components=ipp to the vm.
   */
  static final Logger logger = SystemLogger.SYSTEM;
  
  URI uri;
  short operation_id;
  short status_code;
  int request_id;

  List operationAttributes;
  List printerAttributes;
  List jobAttributes;
  List unsupportedAttributes;

  byte[] data;

  /**
   * Creates an <code>IppResponse</code> instance.
   * 
   * @param uri the uri the request was directy to.
   * @param operation_id the operation id of the request.
   */
  public IppResponse(URI uri, short operation_id)
  {
    this.uri = uri;
    this.operation_id = operation_id;
    operationAttributes = new ArrayList();
    jobAttributes = new ArrayList();
    printerAttributes = new ArrayList();
    unsupportedAttributes = new ArrayList();
  }

  /**
   * Sets the data received from the request sent.
   * 
   * @param input the input stream received.
   * @throws IppException if parsing fails.
   */
  protected void setResponseData(InputStream input) throws IppException
  {
    ResponseReader reader = new ResponseReader();

    try
      {
        reader.parseResponse(input);
      }
    catch (IOException e)
      {
        throw new IppException(
            "Exception during response parsing caused by IOException", e);
      }
  }

  /**
   * Returns the uri of the original request.
   * @return The URI of the request.
   */
  public URI getURI()
  {
    return uri;
  }

  /**
   * Returns the operation id of the original request.
   * @return The operation id of the request.
   */
  public int getOperationID()
  {
    return operation_id;
  }

  /**
   * Returns the set of job attributes group maps.
   * There may occur more than one group of type job attribute in a response
   * because of e.g. multiple job or print service informations requested.
   *  
   * @return The list of job attribute grou maps.
   */
  public List getJobAttributes()
  {
    return jobAttributes;
  }

  /**
   * Returns the set of operation attributes group maps.
   * There may occur more than one group of type job attribute in a response
   * because of e.g. multiple job or print service informations requested.
   *  
   * @return The list of operation attribute grou maps.
   */
  public List getOperationAttributes()
  {
    return operationAttributes;
  }

  /**
   * Returns the set of printer attributes group maps.
   * There may occur more than one group of type job attribute in a response
   * because of e.g. multiple job or print service informations requested.
   *  
   * @return The list of printer attribute grou maps.
   */
  public List getPrinterAttributes()
  {
    return printerAttributes;
  }

  /**
   * Returns the ID of the initial request.
   * 
   * @return The request ID.
   */
  public int getRequestID()
  {
    return request_id;
  }

  /**
   * Returns the status code of the response.
   * Defined in {@link IppStatusCode}.
   * 
   * @return The status code.
   */
  public short getStatusCode()
  {
    return status_code;
  }

  /**
   * Returns the set of unsupported attributes group maps.
   * There may occur more than one group of type job attribute in a response
   * because of e.g. multiple job or print service informations requested.
   *  
   * @return The list of unsupported attribute grou maps.
   */
  public List getUnsupportedAttributes()
  {
    return unsupportedAttributes;
  }

  /**
   * Returns the data of the response.
   * 
   * @return The data as byte[].
   */
  public byte[] getData()
  {
    return data;
  }

}
