/* IppPrintService.java --
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

import gnu.classpath.SystemProperties;
import gnu.classpath.debug.Component;
import gnu.classpath.debug.SystemLogger;
import gnu.javax.print.ipp.attribute.DefaultValueAttribute;
import gnu.javax.print.ipp.attribute.RequestedAttributes;
import gnu.javax.print.ipp.attribute.defaults.CopiesDefault;
import gnu.javax.print.ipp.attribute.defaults.FinishingsDefault;
import gnu.javax.print.ipp.attribute.defaults.JobHoldUntilDefault;
import gnu.javax.print.ipp.attribute.defaults.JobPriorityDefault;
import gnu.javax.print.ipp.attribute.defaults.JobSheetsDefault;
import gnu.javax.print.ipp.attribute.defaults.MediaDefault;
import gnu.javax.print.ipp.attribute.defaults.MultipleDocumentHandlingDefault;
import gnu.javax.print.ipp.attribute.defaults.NumberUpDefault;
import gnu.javax.print.ipp.attribute.defaults.OrientationRequestedDefault;
import gnu.javax.print.ipp.attribute.defaults.PrintQualityDefault;
import gnu.javax.print.ipp.attribute.defaults.PrinterResolutionDefault;
import gnu.javax.print.ipp.attribute.defaults.SidesDefault;
import gnu.javax.print.ipp.attribute.printer.DocumentFormat;
import gnu.javax.print.ipp.attribute.supported.CompressionSupported;
import gnu.javax.print.ipp.attribute.supported.DocumentFormatSupported;
import gnu.javax.print.ipp.attribute.supported.FinishingsSupported;
import gnu.javax.print.ipp.attribute.supported.JobHoldUntilSupported;
import gnu.javax.print.ipp.attribute.supported.JobSheetsSupported;
import gnu.javax.print.ipp.attribute.supported.MediaSupported;
import gnu.javax.print.ipp.attribute.supported.MultipleDocumentHandlingSupported;
import gnu.javax.print.ipp.attribute.supported.OperationsSupported;
import gnu.javax.print.ipp.attribute.supported.OrientationRequestedSupported;
import gnu.javax.print.ipp.attribute.supported.PageRangesSupported;
import gnu.javax.print.ipp.attribute.supported.PrintQualitySupported;
import gnu.javax.print.ipp.attribute.supported.PrinterResolutionSupported;
import gnu.javax.print.ipp.attribute.supported.PrinterUriSupported;
import gnu.javax.print.ipp.attribute.supported.SidesSupported;

import java.io.IOException;
import java.lang.reflect.Field;
import java.net.URI;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Date;
import java.util.HashSet;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.logging.Logger;

import javax.print.DocFlavor;
import javax.print.DocPrintJob;
import javax.print.PrintService;
import javax.print.ServiceUIFactory;
import javax.print.attribute.Attribute;
import javax.print.attribute.AttributeSet;
import javax.print.attribute.AttributeSetUtilities;
import javax.print.attribute.HashAttributeSet;
import javax.print.attribute.HashPrintServiceAttributeSet;
import javax.print.attribute.IntegerSyntax;
import javax.print.attribute.PrintServiceAttribute;
import javax.print.attribute.PrintServiceAttributeSet;
import javax.print.attribute.standard.Compression;
import javax.print.attribute.standard.Copies;
import javax.print.attribute.standard.CopiesSupported;
import javax.print.attribute.standard.Fidelity;
import javax.print.attribute.standard.Finishings;
import javax.print.attribute.standard.JobHoldUntil;
import javax.print.attribute.standard.JobImpressions;
import javax.print.attribute.standard.JobImpressionsSupported;
import javax.print.attribute.standard.JobKOctets;
import javax.print.attribute.standard.JobKOctetsSupported;
import javax.print.attribute.standard.JobMediaSheets;
import javax.print.attribute.standard.JobMediaSheetsSupported;
import javax.print.attribute.standard.JobName;
import javax.print.attribute.standard.JobPriority;
import javax.print.attribute.standard.JobPrioritySupported;
import javax.print.attribute.standard.JobSheets;
import javax.print.attribute.standard.Media;
import javax.print.attribute.standard.MultipleDocumentHandling;
import javax.print.attribute.standard.NumberUp;
import javax.print.attribute.standard.NumberUpSupported;
import javax.print.attribute.standard.OrientationRequested;
import javax.print.attribute.standard.PageRanges;
import javax.print.attribute.standard.PrintQuality;
import javax.print.attribute.standard.PrinterName;
import javax.print.attribute.standard.PrinterResolution;
import javax.print.attribute.standard.PrinterURI;
import javax.print.attribute.standard.RequestingUserName;
import javax.print.attribute.standard.Sides;
import javax.print.event.PrintServiceAttributeListener;


/**
 * Implementation of the PrintService interface
 * for IPP based printers.
 *
 * @author Wolfgang Baer (WBaer@gmx.de)
 */
public class IppPrintService implements PrintService
{
  /**
   * A Map with sets of attributes.
   * key: A attribute category
   * value: A set with values
   *
   * IPP may return sets of attributes e.g. for supported
   * compression methods so we need to map to sets here.
   */
  private Map<Class<? extends Attribute>, Set<Attribute>> printerAttr;

  /** The set of listeners.*/
  private HashSet<PrintServiceAttributeListener> printServiceAttributeListener;

  /** The username. */
  private transient String user;

  /** The password of the user. */
  private transient String passwd;

  /** The name of this print service. */
  private String name;

  /** The list of supported document flavors. */
  private List<DocFlavor> flavors;

  /** The standard printer URI. */
  private PrinterURI printerUri;

  /** The list of all supported printer URIs. */
  private ArrayList<PrinterURI> printerUris;

  /**
   * Logger for tracing - enable by passing
   * -Dgnu.classpath.debug.components=ipp to the vm.
   */
  static final Logger logger = SystemLogger.SYSTEM;

  /**
   * requesting-user-name defaults to the executing user.
   */
  public static final RequestingUserName REQUESTING_USER_NAME;

  /**
   * job-name defaults to "Java Printing".
   */
  public static final JobName JOB_NAME;

  static
  {
    JOB_NAME = new JobName("Java Printing", null);
    REQUESTING_USER_NAME = new RequestingUserName(
      SystemProperties.getProperty("user.name", ""), null);
  }

  // TODO Implement service listener notification and change detection.

  /**
   * Creates a <code>IppPrintService</code> object.
   *
   * @param uri the URI of the IPP printer.
   * @param username the user of this print service.
   * @param password the password of the user.
   *
   * @throws IppException if an error during connection occurs.
   */
  public IppPrintService(URI uri, String username, String password)
    throws IppException
  {
    printerUri = new PrinterURI(uri);
    user = username;
    passwd = password;

    printServiceAttributeListener =
      new HashSet<PrintServiceAttributeListener>();

    printerAttr = getPrinterAttributes();
    processResponse();
  }

  /**
   * Fetches all printer attributes from the IPP printer.
   *
   * @return The Map with the printer attributes.
   * @throws IppException if an error occurs.
   */
  private Map<Class<? extends Attribute>, Set<Attribute>> getPrinterAttributes()
    throws IppException
  {
    IppResponse response = null;

    try
      {
        IppRequest request = new IppRequest(printerUri.getURI(), user, passwd);

        int operation = OperationsSupported.GET_PRINTER_ATTRIBUTES.getValue();
        request.setOperationID((short) operation);
        request.setOperationAttributeDefaults();
        request.addOperationAttribute(printerUri);

        response = request.send();
      }
    catch (IOException e)
      {
        throw new IppException("IOException in IPP request/response.", e);
      }

    return response.getPrinterAttributes().get(0);
  }

  /**
   * Extracts the set of attribute values for a given
   * attribute category from the printer attributes map.
   *
   * @param attributeClass the category
   * @return The set of attributes of the category.
   */
  private <T extends Attribute> Set<T> getPrinterAttributeSet(Class<T> attributeClass)
  {
    Set<Attribute> set = printerAttr.get(attributeClass);
    Set<T> attSet = new HashSet<T>();
    for (Attribute att : set)
      attSet.add(attributeClass.cast(att));
    return attSet;
  }

  /**
   * Extracts the default attribute value for the given
   * default attribute category from the printer attributes map.
   *
   * @param attributeClass the category
   * @return The default attribute.
   *
   * @throws ClassCastException if attributClass is not an
   * instance of <code>DefaultValueAttribute</code>.
   */
  private Attribute getPrinterDefaultAttribute(Class<? extends Attribute> attributeClass)
  {
    Set<Attribute> set = printerAttr.get(attributeClass);
    return ((DefaultValueAttribute) set.toArray()[0]).getAssociatedAttribute();
  }

  /**
   * Processes the response, sorts and splits the attributes.
   */
  private void processResponse()
  {
    // printer name
    PrinterName[] tmp = getPrinterAttributeSet(PrinterName.class).toArray(new PrinterName[1]);
    name = tmp[0].getValue();

    // supported flavors
    // TODO Check if charsets-supported are charsets that are actually supported
    // for text doc flavors as cups doesn't send charset parameters

    // utf-8 is supported at least - so we go with this only for now
    flavors = new ArrayList<DocFlavor>();
    Set<DocumentFormatSupported> flavorAttributes = getPrinterAttributeSet(DocumentFormatSupported.class);
    if (flavorAttributes != null)
      {
        for (DocumentFormatSupported dfs : flavorAttributes)
          {
            String mimeType = dfs.getValue();

            if (mimeType.equals("text/plain"))
              {
                flavors.add(DocFlavor.CHAR_ARRAY.TEXT_PLAIN);
                flavors.add(DocFlavor.READER.TEXT_PLAIN);
                flavors.add(DocFlavor.STRING.TEXT_PLAIN);

                // add utf-8
                mimeType = mimeType + "; charset=utf-8";
              }
            else if (mimeType.equals("text/html"))
              {
                flavors.add(DocFlavor.CHAR_ARRAY.TEXT_HTML);
                flavors.add(DocFlavor.READER.TEXT_HTML);
                flavors.add(DocFlavor.STRING.TEXT_HTML);

                // add utf-8
                mimeType = mimeType + "; charset=utf-8";
              }

            // Process the predefined DocFlavors and if mimetype is
            // equal put them into the flavors array - otherwise
            // just build them as binarie class representation.
            boolean changed = false;
            try
              {
                Class<?>[] clazzes = new Class<?>[] { DocFlavor.BYTE_ARRAY.class,
                    DocFlavor.INPUT_STREAM.class,
                    DocFlavor.URL.class
                    };

                for (int j = 0; j < clazzes.length; j++)
                  {
                    Field[] fields = clazzes[j].getDeclaredFields();
                    for (int i = 0; i < fields.length; i++)
                      {
                        if (fields[i].getType().equals(clazzes[j]))
                          {
                            DocFlavor flavor = (DocFlavor) fields[i].get(null);
                            if (flavor.getMimeType().equals(mimeType))
                              changed = flavors.add(flavor);
                          }
                      }
                  }
                if (!changed) // not in predefined constants of DocFlavor
                  {
                    // everything should be supported as binary stuff
                    flavors.add(new DocFlavor(mimeType, "[B"));
                    flavors.add(new DocFlavor(mimeType, "java.io.InputStream"));
                    flavors.add(new DocFlavor(mimeType, "java.net.URL"));
                  }
              }
            catch (SecurityException e)
              {
                // should not happen
              }
            catch (IllegalArgumentException e)
              {
                // should not happen
              }
            catch (IllegalAccessException e)
              {
                // should not happen, all fields are public
              }
          }

        if (this.getClass()
            .isAssignableFrom(gnu.javax.print.CupsPrintService.class))
          {
//          CUPS always provides filters to convert from Postscript.
//          This logic looks odd, but it's what OpenJDK does.
            flavors.add(DocFlavor.SERVICE_FORMATTED.PAGEABLE);
            flavors.add(DocFlavor.SERVICE_FORMATTED.PRINTABLE);
          }
      }

    // printer uris
    Set<PrinterUriSupported> uris = getPrinterAttributeSet(PrinterUriSupported.class);
    printerUris = new ArrayList<PrinterURI>(uris.size());
    for (PrinterUriSupported uri : uris)
      {
        printerUris.add( new PrinterURI(uri.getURI()));
      }
  }

  /**
   * We always return a implementation implementing CancelablePrintJob.
   *
   * @see javax.print.PrintService#createPrintJob()
   */
  public DocPrintJob createPrintJob()
  {
    return new DocPrintJobImpl(this, user, passwd);
  }


  /**
   * @see javax.print.PrintService#getAttribute(java.lang.Class)
   */
  public <T extends PrintServiceAttribute> T getAttribute(Class<T> category)
  {
    if (category == null)
      throw new NullPointerException("category may not be null");

    if (! PrintServiceAttribute.class.isAssignableFrom(category))
      throw new IllegalArgumentException(
         "category must be of type PrintServiceAttribute");

    Set<T> set = getPrinterAttributeSet(category);
    if (set != null && set.size() > 0)
      return set.iterator().next();

    return null;
  }

  /**
   * @see javax.print.PrintService#getAttributes()
   */
  public PrintServiceAttributeSet getAttributes()
  {
    PrintServiceAttributeSet set = new HashPrintServiceAttributeSet();

    for (Set<Attribute> attrSet : printerAttr.values())
      {
        for (Attribute attr : attrSet)
          {
            if (attr instanceof PrintServiceAttribute)
              set.add(attr);
          }
      }

    return AttributeSetUtilities.unmodifiableView(set);
  }

  /**
   * @see javax.print.PrintService#getDefaultAttributeValue(java.lang.Class)
   */
  public Object getDefaultAttributeValue(Class<? extends Attribute> category)
  {
    // required attributes
    if (category.equals(Fidelity.class))
      return Fidelity.FIDELITY_FALSE;
    if (category.equals(JobName.class))
      return JOB_NAME;
    if (category.equals(RequestingUserName.class))
      return REQUESTING_USER_NAME;

    // optional attributes
    if (category.equals(JobPriority.class)
        && printerAttr.containsKey(JobPriorityDefault.class))
      return getPrinterDefaultAttribute(JobPriorityDefault.class);
    if (category.equals(JobHoldUntil.class)
        && printerAttr.containsKey(JobHoldUntilDefault.class))
      return getPrinterDefaultAttribute(JobHoldUntilDefault.class);
    if (category.equals(JobSheets.class)
        && printerAttr.containsKey(JobSheetsDefault.class))
      return getPrinterDefaultAttribute(JobSheetsDefault .class);
    if (category.equals(MultipleDocumentHandling.class)
        && printerAttr.containsKey(MultipleDocumentHandlingDefault.class))
      return getPrinterDefaultAttribute(MultipleDocumentHandlingDefault.class);
    if (category.equals(Copies.class)
        && printerAttr.containsKey(CopiesDefault.class))
      return getPrinterDefaultAttribute(CopiesDefault.class);
    if (category.equals(Finishings.class)
        && printerAttr.containsKey(FinishingsDefault.class))
      return getPrinterDefaultAttribute(FinishingsDefault.class);
    if (category.equals(Sides.class)
        && printerAttr.containsKey(SidesDefault.class))
      return getPrinterDefaultAttribute(SidesDefault.class);
    if (category.equals(NumberUp.class)
        && printerAttr.containsKey(NumberUpDefault.class))
      return getPrinterDefaultAttribute(NumberUpDefault.class);
    if (category.equals(OrientationRequested.class)
        && printerAttr.containsKey(OrientationRequestedDefault.class))
      return getPrinterDefaultAttribute(OrientationRequestedDefault.class);
    if (category.equals(Media.class)
        && printerAttr.containsKey(MediaDefault.class))
      return getPrinterDefaultAttribute(MediaDefault.class);
    if (category.equals(PrinterResolution.class)
        && printerAttr.containsKey(PrinterResolutionDefault.class))
      return getPrinterDefaultAttribute(PrinterResolutionDefault.class);
    if (category.equals(PrintQuality.class)
        && printerAttr.containsKey(PrintQualityDefault.class))
      return getPrinterDefaultAttribute(PrintQualityDefault.class);
    if (category.equals(Compression.class)
        && printerAttr.containsKey(CompressionSupported.class))
      return Compression.NONE;
    if (category.equals(PageRanges.class))
      return new PageRanges(1, Integer.MAX_VALUE);

    return null;
  }

  /**
   * We return the value of <code>PrinterName</code> here.
   * @see javax.print.PrintService#getName()
   */
  public String getName()
  {
    return name;
  }

  /**
   * We currently provide no factories - just returns null.
   * @see javax.print.PrintService#getServiceUIFactory()
   */
  public ServiceUIFactory getServiceUIFactory()
  {
    // SUN does not provide any service factory for
    // print services (tested on linux/windows)

    // for the moment we do the same - just return null
    // later on we could provide at least the about UI dialog
    return null;
  }

  /**
   * @see javax.print.PrintService#getSupportedAttributeCategories()
   */
  public Class<?>[] getSupportedAttributeCategories()
  {
    Set<Class<? extends Attribute>> categories =
      new HashSet<Class<? extends Attribute>>();

    // Should only be job template attributes as of section 4.2
    if (printerAttr.containsKey(JobPrioritySupported.class))
      categories.add(JobPriority.class);
    if (printerAttr.containsKey(JobHoldUntilSupported.class))
      categories.add(JobHoldUntil.class);
    if (printerAttr.containsKey(JobSheetsSupported.class))
      categories.add(JobSheets.class);
    if (printerAttr.containsKey(MultipleDocumentHandlingSupported.class))
      categories.add(MultipleDocumentHandling.class);
    if (printerAttr.containsKey(CopiesSupported.class))
      categories.add(Copies.class);
    if (printerAttr.containsKey(FinishingsSupported.class))
      {
        // if only none finishing is supported - it does not count as supported
        Set<FinishingsSupported> set = getPrinterAttributeSet(FinishingsSupported.class);
        if (! (set.size() == 1 && set.contains(FinishingsSupported.NONE)))
          categories.add(Finishings.class);
      }
    if (printerAttr.containsKey(PageRangesSupported.class))
      categories.add(PageRanges.class);
    if (printerAttr.containsKey(SidesSupported.class))
      categories.add(Sides.class);
    if (printerAttr.containsKey(NumberUpSupported.class))
      categories.add(NumberUp.class);
    if (printerAttr.containsKey(OrientationRequestedSupported.class))
      categories.add(OrientationRequested.class);
    if (printerAttr.containsKey(MediaSupported.class))
      categories.add(Media.class);
    if (printerAttr.containsKey(PrinterResolutionSupported.class))
      categories.add(PrinterResolution.class);
    if (printerAttr.containsKey(PrintQualitySupported.class))
      categories.add(PrintQuality.class);

    // Chromaticity, Destination, MediaPrintableArea,
    // SheetCollate, PresentationDirection - not IPP attributes

    // attributes outside section 4.2
    if (printerAttr.containsKey(CompressionSupported.class))
      categories.add(Compression.class);
    if (printerAttr.containsKey(JobImpressionsSupported.class))
      categories.add(JobImpressions.class);
    if (printerAttr.containsKey(JobKOctetsSupported.class))
      categories.add(JobKOctets.class);
    if (printerAttr.containsKey(JobMediaSheetsSupported.class))
      categories.add(JobMediaSheets.class);

    // always supported as required by IPP specification
    categories.add(Fidelity.class);
    categories.add(JobName.class);
    categories.add(RequestingUserName.class);

    return categories.toArray(new Class[categories.size()]);
  }

  /**
   * Implemented by a GetPrinterAttributes request. Subclasses providing supported
   * attribute values totally different may override this methods. Subclass only in
   * need of handling the response differently may override the method
   * <code>handleSupportedAttributeValuesResponse(IppResponse, Class)</code> only.
   *
   * @see PrintService#getSupportedAttributeValues(Class, DocFlavor, AttributeSet)
   * @see #handleSupportedAttributeValuesResponse(IppResponse, Class)
   */
  public Object getSupportedAttributeValues(Class<? extends Attribute> category,
                                            DocFlavor flavor, AttributeSet attributes)
  {
    // We currently ignore the attribute set - there is nothing in the IPP
    // specification which would come closer to what we do here.

    if (category == null)
      throw new NullPointerException("category may not be null");

    if (!Attribute.class.isAssignableFrom(category))
      throw new IllegalArgumentException("category must be of type Attribute");

    if (flavor != null && !isDocFlavorSupported(flavor))
      throw new IllegalArgumentException("flavor is not supported");

    if (!isAttributeCategorySupported(category))
      return null;

    // always supported
    if (category.equals(Fidelity.class))
      return new Fidelity[] { Fidelity.FIDELITY_FALSE, Fidelity.FIDELITY_TRUE };
    if (category.equals(JobName.class))
      return JOB_NAME;
    if (category.equals(RequestingUserName.class))
      return REQUESTING_USER_NAME;

    // map category to category-supported
    String categoryName = IppUtilities.getSupportedAttrName(category);

    IppResponse response = null;
    try
      {
        IppRequest request = new IppRequest(printerUri.getURI(), user, passwd);
        request.setOperationID(
          (short) OperationsSupported.GET_PRINTER_ATTRIBUTES.getValue());
        request.setOperationAttributeDefaults();
        request.addOperationAttribute(new RequestedAttributes(categoryName));
        request.addOperationAttribute(printerUri);

        if (flavor != null)
          {
            DocumentFormat f = DocumentFormat.createDocumentFormat(flavor);
            request.addOperationAttribute(f);
          }

        response = request.send();

        int status = response.getStatusCode();
        if (! (status == IppStatusCode.SUCCESSFUL_OK
             || status == IppStatusCode.SUCCESSFUL_OK_IGNORED_OR_SUBSTITUED_ATTRIBUTES
             || status == IppStatusCode.SUCCESSFUL_OK_CONFLICTING_ATTRIBUTES) )
          {
            logger.log(Component.IPP, "Statuscode not OK - got:" + status);
          }
      }
    catch (IOException e)
      {
        // method cannot throw exception - just log
        logger.log(Component.IPP, "IOException", e);
      }
    catch (IppException e)
      {
        // method cannot throw exception - just log
        logger.log(Component.IPP, "IPPException", e);
      }

    return handleSupportedAttributeValuesResponse(response, category);
  }

  /**
   * Called to handle the supported attribute values response for the given
   * category. This might be overridden by subclasses with different requirements
   * for parsing/handling the response from the GetPrinterAttributes.
   *
   * @param response the response of the GetPrinterAttributes IPP request
   * @param category the category for which the supported values are requested
   * @return A object indicating the supported values for the given attribute
   * category, or <code>null</code> if this print service doesn't support the
   * given attribute category at all.
   *
   * @see #getSupportedAttributeValues(Class, DocFlavor, AttributeSet)
   */
  protected Object handleSupportedAttributeValuesResponse(IppResponse response,
                                                          Class<? extends Attribute> category)
  {
    List<Map<Class<? extends Attribute>, Set<Attribute>>> printerAtts =
      response.getPrinterAttributes();

    // only one will be returned
    Map<Class<? extends Attribute>, Set<Attribute>> printerAttribute = printerAtts.get(0);
    Class<? extends Attribute> suppCategory = IppUtilities.getSupportedCategory(category);
    Set<Attribute> attr = printerAttribute.get(suppCategory);

    // We sometime assume its a single instance with arbritrary value just indicating
    // support or an array which is returned. This is because I sometimes just choosed
    // what sounds right to me - as I have yet to find a printer which supports every
    // special category in the SUN implementation to see what they return :-)

    //  Map whats in the JSP API
    if (suppCategory.equals(JobPrioritySupported.class))
      return (JobPrioritySupported) attr.iterator().next();
    if (suppCategory.equals(JobHoldUntilSupported.class))
      return new JobHoldUntil(new Date());
    if (suppCategory.equals(JobSheetsSupported.class))
      return JobSheetsSupported.getAssociatedAttributeArray(attr);
    if (suppCategory.equals(MultipleDocumentHandlingSupported.class))
      return MultipleDocumentHandlingSupported.getAssociatedAttributeArray(attr);
    if (suppCategory.equals(CopiesSupported.class))
      return (CopiesSupported) attr.iterator().next();
    if (suppCategory.equals(FinishingsSupported.class))
      return FinishingsSupported.getAssociatedAttributeArray(attr);
    if (suppCategory.equals(PageRangesSupported.class))
      return new PageRanges[] { new PageRanges(1, Integer.MAX_VALUE) };
    if (suppCategory.equals(OrientationRequestedSupported.class))
      return OrientationRequestedSupported.getAssociatedAttributeArray(attr);
    if (suppCategory.equals(MediaSupported.class))
      return MediaSupported.getAssociatedAttributeArray(attr);
    if (suppCategory.equals(PrinterResolutionSupported.class))
      return PrinterResolutionSupported.getAssociatedAttributeArray(attr);
    if (suppCategory.equals(PrintQualitySupported.class))
      return PrintQualitySupported.getAssociatedAttributeArray(attr);
    if (suppCategory.equals(CompressionSupported.class))
      return CompressionSupported.getAssociatedAttributeArray(attr);
    // Special handling as it might also be in range of integers
    if (suppCategory.equals(NumberUpSupported.class))
      {
        if (attr.size() == 1) // number-up maybe in rangeofintegers
          return attr.iterator().next();

        int[][] members = new int[attr.size()][2];
        Iterator<Attribute> it = attr.iterator();
        for (int j = 0; j < attr.size(); j++)
          {
            int value = ((NumberUpSupported) it.next()).getMembers()[0][0];
            members[j] = new int[] { value, value };
          }

        NumberUpSupported supported = new NumberUpSupported(members);
        return supported;
      }

    return null;
  }

  /**
   * @see javax.print.PrintService#getSupportedDocFlavors()
   */
  public DocFlavor[] getSupportedDocFlavors()
  {
    return flavors.toArray(new DocFlavor[flavors.size()]);
  }

  /**
   * This is done by a validate-job operation and actually implemented in
   * this generic IPP reference implementation. Subclasses which does
   * not correctly support Validate-Job operation might want to override this.
   *
   * @see PrintService#getUnsupportedAttributes(DocFlavor, AttributeSet)
   */
  public AttributeSet getUnsupportedAttributes(DocFlavor flavor,
                                               AttributeSet attributes)
  {
    if (flavor != null && !isDocFlavorSupported(flavor))
      throw new IllegalArgumentException("flavor is not supported");

    IppResponse response = null;
    try
      {
        IppRequest request = new IppRequest(printerUri.getURI(), user, passwd);
        short operationId = (short) OperationsSupported.VALIDATE_JOB.getValue();
        request.setOperationID(operationId);
        request.setOperationAttributeDefaults();
        request.addOperationAttribute(printerUri);
        request.addOperationAttribute(Fidelity.FIDELITY_TRUE);

        if (attributes != null && attributes.size() > 0)
          {
            request.addAndFilterJobOperationAttributes(attributes);
            request.addAndFilterJobTemplateAttributes(attributes);
          }

        if (flavor != null)
          {
            DocumentFormat f = DocumentFormat.createDocumentFormat(flavor);
            request.addOperationAttribute(f);
          }

        response = request.send();

        int status = response.getStatusCode();
        if (! (status == IppStatusCode.SUCCESSFUL_OK
             || status == IppStatusCode.SUCCESSFUL_OK_IGNORED_OR_SUBSTITUED_ATTRIBUTES
             || status == IppStatusCode.SUCCESSFUL_OK_CONFLICTING_ATTRIBUTES) )
          {
            logger.log(Component.IPP, "Statuscode not OK - got:" + status);
          }
      }
    catch (IOException e)
      {
        // method cannot throw exception - just log
        logger.log(Component.IPP, "IOException", e);
      }
    catch (IppException e)
      {
        // method cannot throw exception - just log
        logger.log(Component.IPP, "IPPException", e);
      }

    // Validate Jobs returns only Unsupported and Operation
    List<Map<Class<? extends Attribute>, Set<Attribute>>> unsupportedMaps =
      response.getUnsupportedAttributes();
    if (unsupportedMaps.size() == 0)
      return null;

    Map<Class<? extends Attribute>, Set<Attribute>> unsupportedAttr = unsupportedMaps.get(0);
    if (unsupportedAttr.size() == 0)
      return null;

    // Convert the return map with unsupported attributes
    // into an AttribueSet instance
    HashAttributeSet set = new HashAttributeSet();
    for (Set<Attribute> unsupported : unsupportedAttr.values())
      {
        for (Attribute att : unsupported)
          set.add(att);
      }

    return set;
  }

  /**
   * @see PrintService#isAttributeCategorySupported(Class)
   */
  public boolean isAttributeCategorySupported(Class<? extends Attribute> category)
  {
    if (category == null)
      throw new NullPointerException("category may not be null");

    if (! Attribute.class.isAssignableFrom(category))
      throw new IllegalArgumentException("category must be of type Attribute");

    return Arrays.asList(getSupportedAttributeCategories()).contains(category);
  }

  /**
   * @see PrintService#isAttributeValueSupported(Attribute, DocFlavor, AttributeSet)
   */
  public boolean isAttributeValueSupported(Attribute attrval, DocFlavor flavor,
                                           AttributeSet attributes)
  {
    // just redirect to getSupportedAttributeValues
    Object values = getSupportedAttributeValues(attrval.getCategory(),
                                                flavor, attributes);
    // null means none supported
    if (values == null)
      return false;

    // object may be an array
    if (values.getClass().isArray())
      return Arrays.asList((Object[]) values).contains(attrval);

    // may be a single instance of the category (value is irrelevant)
    if (values.getClass().equals(attrval.getCategory()))
      return true;

    // a single instance of another class to give the bounds
    // copies
    if (values.getClass().equals(CopiesSupported.class))
      return ((CopiesSupported) values).contains((IntegerSyntax) attrval);
    // number up
    if (values.getClass().equals(NumberUpSupported.class))
      return ((NumberUpSupported) values).contains((IntegerSyntax) attrval);
    // job priority
    if (values.getClass().equals(JobPrioritySupported.class))
      {
        JobPriority priority = (JobPriority) attrval;
        JobPrioritySupported maxSupported = (JobPrioritySupported) values;
        if (priority.getValue() < maxSupported.getValue())
          return true;
      }

    // I am unsure if these might also show up - not yet found a printer where
    // Suns implementation supports them:
    // JobImpressionsSupported, JobKOctetsSupported, JobMediaSheetsSupported

    return false;
  }


  /**
   * @see javax.print.PrintService#isDocFlavorSupported(DocFlavor)
   */
  public boolean isDocFlavorSupported(DocFlavor flavor)
  {
    if (flavor == null)
      throw new NullPointerException("DocFlavor may not be null.");

    return flavors.contains(flavor);
  }


  /**
   * @see PrintService#addPrintServiceAttributeListener(PrintServiceAttributeListener)
   */
  public void addPrintServiceAttributeListener(
    PrintServiceAttributeListener listener)
  {
    printServiceAttributeListener.add(listener);
  }

  /**
   * @see PrintService#removePrintServiceAttributeListener(PrintServiceAttributeListener)
   */
  public void removePrintServiceAttributeListener(
    PrintServiceAttributeListener listener)
  {
    printServiceAttributeListener.remove(listener);
  }

  /**
   * Returns "IppPrinter: " + <code>getName()</code>
   * @return The string representation.
   */
  public String toString()
  {
    return "IppPrinter: " + getName();
  }

  /**
   * Returns the printer-uri of this print service.
   *
   * @return The printer-uri attribute.
   */
  public PrinterURI getPrinterURI()
  {
    return printerUri;
  }
}
