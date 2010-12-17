/* IppUtilities.java --
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

import gnu.javax.print.ipp.attribute.DetailedStatusMessage;
import gnu.javax.print.ipp.attribute.DocumentAccessError;
import gnu.javax.print.ipp.attribute.StatusMessage;
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
import gnu.javax.print.ipp.attribute.defaults.SidesDefault;
import gnu.javax.print.ipp.attribute.job.JobDetailedStatusMessages;
import gnu.javax.print.ipp.attribute.job.JobDocumentAccessErrors;
import gnu.javax.print.ipp.attribute.job.JobId;
import gnu.javax.print.ipp.attribute.job.JobStateMessage;
import gnu.javax.print.ipp.attribute.printer.MultipleOperationTimeOut;
import gnu.javax.print.ipp.attribute.printer.PrinterStateMessage;
import gnu.javax.print.ipp.attribute.printer.PrinterUpTime;
import gnu.javax.print.ipp.attribute.supported.CompressionSupported;
import gnu.javax.print.ipp.attribute.supported.FinishingsSupported;
import gnu.javax.print.ipp.attribute.supported.IppVersionsSupported;
import gnu.javax.print.ipp.attribute.supported.JobHoldUntilSupported;
import gnu.javax.print.ipp.attribute.supported.JobSheetsSupported;
import gnu.javax.print.ipp.attribute.supported.MediaSupported;
import gnu.javax.print.ipp.attribute.supported.MultipleDocumentHandlingSupported;
import gnu.javax.print.ipp.attribute.supported.MultipleDocumentJobsSupported;
import gnu.javax.print.ipp.attribute.supported.OperationsSupported;
import gnu.javax.print.ipp.attribute.supported.OrientationRequestedSupported;
import gnu.javax.print.ipp.attribute.supported.PageRangesSupported;
import gnu.javax.print.ipp.attribute.supported.PrintQualitySupported;
import gnu.javax.print.ipp.attribute.supported.PrinterResolutionSupported;
import gnu.javax.print.ipp.attribute.supported.SidesSupported;
import gnu.javax.print.ipp.attribute.supported.UriAuthenticationSupported;
import gnu.javax.print.ipp.attribute.supported.UriSecuritySupported;

import java.lang.reflect.Constructor;
import java.lang.reflect.Field;
import java.lang.reflect.InvocationTargetException;
import java.util.HashMap;
import java.util.Locale;

import javax.print.attribute.Attribute;
import javax.print.attribute.EnumSyntax;
import javax.print.attribute.SupportedValuesAttribute;
import javax.print.attribute.standard.Chromaticity;
import javax.print.attribute.standard.ColorSupported;
import javax.print.attribute.standard.Compression;
import javax.print.attribute.standard.Copies;
import javax.print.attribute.standard.CopiesSupported;
import javax.print.attribute.standard.Fidelity;
import javax.print.attribute.standard.Finishings;
import javax.print.attribute.standard.JobHoldUntil;
import javax.print.attribute.standard.JobImpressionsCompleted;
import javax.print.attribute.standard.JobKOctetsProcessed;
import javax.print.attribute.standard.JobMediaSheetsCompleted;
import javax.print.attribute.standard.JobMessageFromOperator;
import javax.print.attribute.standard.JobName;
import javax.print.attribute.standard.JobOriginatingUserName;
import javax.print.attribute.standard.JobPriority;
import javax.print.attribute.standard.JobPrioritySupported;
import javax.print.attribute.standard.JobSheets;
import javax.print.attribute.standard.JobState;
import javax.print.attribute.standard.JobStateReason;
import javax.print.attribute.standard.Media;
import javax.print.attribute.standard.MediaSizeName;
import javax.print.attribute.standard.MultipleDocumentHandling;
import javax.print.attribute.standard.NumberOfInterveningJobs;
import javax.print.attribute.standard.NumberUp;
import javax.print.attribute.standard.NumberUpSupported;
import javax.print.attribute.standard.OrientationRequested;
import javax.print.attribute.standard.OutputDeviceAssigned;
import javax.print.attribute.standard.PDLOverrideSupported;
import javax.print.attribute.standard.PageRanges;
import javax.print.attribute.standard.PagesPerMinute;
import javax.print.attribute.standard.PagesPerMinuteColor;
import javax.print.attribute.standard.PresentationDirection;
import javax.print.attribute.standard.PrintQuality;
import javax.print.attribute.standard.PrinterInfo;
import javax.print.attribute.standard.PrinterIsAcceptingJobs;
import javax.print.attribute.standard.PrinterLocation;
import javax.print.attribute.standard.PrinterMakeAndModel;
import javax.print.attribute.standard.PrinterMessageFromOperator;
import javax.print.attribute.standard.PrinterName;
import javax.print.attribute.standard.PrinterResolution;
import javax.print.attribute.standard.PrinterState;
import javax.print.attribute.standard.PrinterStateReason;
import javax.print.attribute.standard.QueuedJobCount;
import javax.print.attribute.standard.ReferenceUriSchemesSupported;
import javax.print.attribute.standard.Severity;
import javax.print.attribute.standard.SheetCollate;
import javax.print.attribute.standard.Sides;

/**
 * Collection of static utilities methods used in
 * IPP response parsing and all over the place.
 * <p>
 * Also provides mapping from the attribute name values to
 * the actual class object. Used to construct objects via reflection.
 * </p>
 *
 * @author Wolfgang Baer (WBaer@gmx.de)
 */
public final class IppUtilities
{
  // These are reused in the reflection code to not instantiate an array everytime
  private static Object[] INTEGER_ATT_VALUE = new Object[1];
  private static Class<?>[] INTEGER_CLASS_ARRAY = new Class[] {int.class};
  private static Object[] TEXT_ATT_VALUE = new Object[2];
  private static Class<?>[] TEXT_CLASS_ARRAY = new Class[] {String.class, Locale.class};

  // The map -> Attribute name to Attribute class
  private static HashMap<String,Class<? extends Attribute>> classesByName =
                                                new HashMap<String,Class<? extends Attribute>>();
  // The map -> StandardAttribute class to SupportedAttribute category name
  private static HashMap<Class<? extends Attribute>,SupportedValuesAttribute> instanceByClass =
                                         new HashMap<Class<? extends Attribute>,SupportedValuesAttribute>();

  /**
   * All the currently needed attributes
   */
  static
    {
      // enums
      classesByName.put(JobState.ABORTED.getName(), JobState.class);
      classesByName.put(Sides.DUPLEX.getName(), Sides.class);
      classesByName.put(SheetCollate.COLLATED.getName(), SheetCollate.class);
      classesByName.put(Severity.ERROR.getName(), Severity.class);
      classesByName.put(JobSheets.NONE.getName(), JobSheets.class);
      classesByName.put(Finishings.BIND.getName(), Finishings.class);
      classesByName.put(Fidelity.FIDELITY_FALSE.getName(), Fidelity.class);
      classesByName.put(Compression.GZIP.getName(), Compression.class);
      classesByName.put(Chromaticity.COLOR.getName(), Chromaticity.class);
      classesByName.put(PrintQuality.DRAFT.getName(), PrintQuality.class);
      classesByName.put(PrinterState.IDLE.getName(), PrinterState.class);
      classesByName.put(SidesDefault.ONE_SIDED.getName(), SidesDefault.class);
      classesByName.put(ReferenceUriSchemesSupported.FILE.getName(),
                        ReferenceUriSchemesSupported.class);
      classesByName.put(PrinterStateReason.DOOR_OPEN.getName(),
                        PrinterStateReason.class);
      classesByName.put(PresentationDirection.TOLEFT_TOTOP.getName(),
                        PresentationDirection.class);
      classesByName.put(PDLOverrideSupported.ATTEMPTED.getName(),
                        PDLOverrideSupported.class);
      classesByName.put(OrientationRequested.PORTRAIT.getName(),
                        OrientationRequested.class);
      classesByName.put(MultipleDocumentHandling.SINGLE_DOCUMENT.getName(),
                        MultipleDocumentHandling.class);
      classesByName.put(JobStateReason.JOB_QUEUED.getName(),
                        JobStateReason.class);
      classesByName.put(UriAuthenticationSupported.NONE.getName(),
                        UriAuthenticationSupported.class);
      classesByName.put(OperationsSupported.GET_JOBS.getName(),
                        OperationsSupported.class);
      classesByName.put(UriSecuritySupported.NONE.getName(),
                        UriSecuritySupported.class);
      classesByName.put(FinishingsSupported.NONE.getName(),
                        FinishingsSupported.class);
      classesByName.put(FinishingsDefault.NONE.getName(),
                        FinishingsDefault.class);
      classesByName.put(IppVersionsSupported.V_1_0.getName(),
                        IppVersionsSupported.class);
      classesByName.put(MultipleDocumentHandlingSupported.SINGLE_DOCUMENT.getName(),
                        MultipleDocumentHandlingSupported.class);
      classesByName.put(MultipleDocumentHandlingDefault.SINGLE_DOCUMENT.getName(),
                        MultipleDocumentHandlingDefault.class);
      classesByName.put(CompressionSupported.NONE.getName(),
                        CompressionSupported.class);
      classesByName.put(OrientationRequestedSupported.PORTRAIT.getName(),
                        OrientationRequestedSupported.class);
      classesByName.put(OrientationRequestedDefault.PORTRAIT.getName(),
                        OrientationRequestedDefault.class);
      classesByName.put(SidesSupported.ONE_SIDED.getName(),
                        SidesSupported.class);
      classesByName.put(PrintQualityDefault.DRAFT.getName(),
                        PrintQualityDefault.class);
      classesByName.put(PrintQualitySupported.DRAFT.getName(),
                        PrintQualitySupported.class);
      classesByName.put(ReferenceUriSchemesSupported.FTP.getName(),
                        ReferenceUriSchemesSupported.class);

      // the boolean types
      classesByName.put(ColorSupported.SUPPORTED.getName(), ColorSupported.class);
      classesByName.put(PrinterIsAcceptingJobs.ACCEPTING_JOBS.getName(),
                        PrinterIsAcceptingJobs.class);
      classesByName.put(MultipleDocumentJobsSupported.SUPPORTED.getName(),
                        MultipleDocumentJobsSupported.class);
      classesByName.put(PageRangesSupported.SUPPORTED.getName(),
                        PageRangesSupported.class);

      // TextSyntax derived attributes
      classesByName.put("media-default", MediaDefault.class);
      classesByName.put("media-supported", MediaSupported.class);
      classesByName.put("media", MediaSizeName.class);
      classesByName.put("printer-location", PrinterLocation.class);
      classesByName.put("printer-info", PrinterInfo.class);
      classesByName.put("printer-make-and-model", PrinterMakeAndModel.class);
      classesByName.put("printer-state-message", PrinterStateMessage.class);
      classesByName.put("job-state-message", JobStateMessage.class);
      classesByName.put("job-sheets-default", JobSheetsDefault.class);
      classesByName.put("job-sheets-supported", JobSheetsSupported.class);
      classesByName.put("job-name", JobName.class);
      classesByName.put("printer-name", PrinterName.class);
      classesByName.put("status-message", StatusMessage.class);
      classesByName.put("detailed-status-message", DetailedStatusMessage.class);
      classesByName.put("document-access-error", DocumentAccessError.class);
      classesByName.put("output-device-assigned", OutputDeviceAssigned.class);
      classesByName.put("job-hold-until-default", JobHoldUntilDefault.class);
      classesByName.put("job-originating-user-name",
                        JobOriginatingUserName.class);
      classesByName.put("job-hold-until-supported",
                        JobHoldUntilSupported.class);
      classesByName.put("job-message-from-operator",
                        JobMessageFromOperator.class);
      classesByName.put("printer-message-from-operator",
                        PrinterMessageFromOperator.class);
      classesByName.put("job-detailed-status-messages",
                        JobDetailedStatusMessages.class);
      classesByName.put("job-document-access-errors",
                        JobDocumentAccessErrors.class);

      // IntegerSyntax derived Attributes
      classesByName.put("copies-default", CopiesDefault.class);
      classesByName.put("job-id", JobId.class);
      classesByName.put("job-priority-supported", JobPrioritySupported.class);
      classesByName.put("job-priority-default", JobPriorityDefault.class);
      classesByName.put("number-up-supported", NumberUpSupported.class);
      classesByName.put("number-up-default", NumberUpDefault.class);
      classesByName.put("queued-job-count", QueuedJobCount.class);
      classesByName.put("printer-up-time", PrinterUpTime.class);
      classesByName.put("pages-per-minute", PagesPerMinute.class);
      classesByName.put("pages-per-minute-color", PagesPerMinuteColor.class);
      classesByName.put("job-k-octets-processed", JobKOctetsProcessed.class);
      classesByName.put("number-of-intervening-jobs",
                        NumberOfInterveningJobs.class);
      classesByName.put("job-impressions-completed",
                        JobImpressionsCompleted.class);
      classesByName.put("job-media-sheets-completed",
                        JobMediaSheetsCompleted.class);
      classesByName.put("multiple-operation-time-out",
                        MultipleOperationTimeOut.class);


      // 4.2 job template attributes
      instanceByClass.put(JobPriority.class, new JobPrioritySupported(1));
      instanceByClass.put(JobHoldUntil.class, new JobHoldUntilSupported("", null));
      instanceByClass.put(JobSheets.class, new JobSheetsSupported("", null));
      instanceByClass.put(MultipleDocumentHandling.class, MultipleDocumentHandlingSupported.SINGLE_DOCUMENT);
      instanceByClass.put(Copies.class, new CopiesSupported(1));
      instanceByClass.put(Finishings.class, FinishingsSupported.BIND);
      instanceByClass.put(PageRanges.class, PageRangesSupported.SUPPORTED);
      instanceByClass.put(Sides.class, SidesSupported.DUPLEX);
      instanceByClass.put(NumberUp.class, new NumberUpSupported(1));
      instanceByClass.put(OrientationRequested.class, OrientationRequestedSupported.LANDSCAPE);
      instanceByClass.put(Media.class, new MediaSupported("", null));
      instanceByClass.put(PrinterResolution.class, new PrinterResolutionSupported(1,1,1));
      instanceByClass.put(PrintQuality.class, PrintQualitySupported.DRAFT);

      // 4.4 printer attributes
      instanceByClass.put(Compression.class, CompressionSupported.COMPRESS);
    }

  private IppUtilities()
  {
    // not to be instantiated
  }

  /**
   * Returns the implementing class object for given
   * attribute name objects.
   *
   * @param name the attribute name
   * @return The <code>Class</code> object.
   */
  public static Class<? extends Attribute> getClass(String name)
  {
    return classesByName.get(name);
  }

  /**
   * Returns the name of the supported attribute
   * based on the given standard attribute category.
   *
   * @param clazz the standard attribute category
   * @return The name of the supported attribute category.
   */
  public static String getSupportedAttrName(Class<? extends Attribute> clazz)
  {
    return instanceByClass.get(clazz).getName();
  }

  /**
   * Returns the category of the supported attribute
   * based on the given standard attribute category.
   *
   * @param clazz the standard attribute category
   * @return The supported attribute category.
   */
  public static Class<? extends Attribute> getSupportedCategory(Class<? extends Attribute> clazz)
  {
    return instanceByClass.get(clazz).getCategory();
  }

  /**
   * Helper method to convert to an int.
   * @param b the byte array
   * @return The converted int.
   */
  public static int convertToInt(byte[] b)
  {
    return (((b[0] & 0xff) << 24) | ((b[1] & 0xff) << 16)
            | ((b[2] & 0xff) << 8) | (b[3] & 0xff));
  }

  /**
   * Helper method to convert to an int.
   * @param b1 the 1th byte
   * @param b2 the 2th byte
   * @param b3 the 3th byte
   * @param b4 the 4th byte
   * @return The converted int.
   */
  public static int convertToInt(byte b1, byte b2, byte b3, byte b4)
  {
    return (((b1 & 0xff) << 24) | ((b2 & 0xff) << 16)
            | ((b3 & 0xff) << 8) | (b4 & 0xff));
  }

  /**
   * Helper method to convert to a short.
   * @param b1 the 1th byte
   * @param b2 the 2th byte
   * @return The converted short.
   */
  public static short convertToShort(byte b1, byte b2)
  {
    return (short) ((b1 << 8) | (b2 & 0xff));
  }

  /**
   * Instantiates an <code>EnumSyntax</code> based attribute with the given IPP
   * name and the given value (Enums maybe int or String based).
   *
   * @param name the attribute name of the subclass.
   * @param value the integer value of the specific enum.
   * @return The Attribute (a subclass of EnumSyntax)
   */
  public static Attribute getEnumAttribute(String name, Object value)
  {
    Class<?> attrClass = getClass(name);

    // There might be unknown enums we have no mapped class for
    if (attrClass ==  null)
      return null;

    try
      {
        Field[] fields = attrClass.getDeclaredFields();
        for (int i = 0; i < fields.length; i++)
          {
            Field field = fields[i];
            if (field.getType().equals(attrClass))
              {
                EnumSyntax attr = (EnumSyntax) field.get(null);
                if (value instanceof Integer
                    && attr.getValue() == ((Integer) value).intValue())
                  return (Attribute) attr;
                else if (value instanceof String
                         && attr.toString().equals(value))
                  return (Attribute) attr;
              }
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

    return null;
  }



  /**
   * Instantiates an <code>IntegerSyntax</code> based attribute with the
   * given IPP name for the given int value.
   *
   * @param name the attribute name of the subclass.
   * @param value the integer value
   * @return The Attribute (a subclass of IntegerSyntax)
   */
  public static Attribute getIntegerAttribute(String name, int value)
  {
    Class<?> attrClass = getClass(name);

    // There might be unknown attributes we have no mapped class for
    if (attrClass ==  null)
      return null;

    try
      {
        INTEGER_ATT_VALUE[0] = Integer.valueOf(value);
        Constructor<?> c = attrClass.getDeclaredConstructor(INTEGER_CLASS_ARRAY);
        return (Attribute) c.newInstance(INTEGER_ATT_VALUE);
      }
    catch (SecurityException e)
      {
        // should not happen
      }
    catch (NoSuchMethodException e)
      {
        // should not happen
      }
    catch (IllegalAccessException e)
      {
        // should not happen, all fields are public
      }
    catch (InstantiationException e)
    {
      // should not happen, all fields are public
    }
    catch (InvocationTargetException e)
    {
      // should not happen, all fields are public
    }

    return null;
  }

  /**
   * Instantiates an <code>TextSyntax</code> based attribute with the given
   * IPP name for the given text value (will be decoded).
   *
   * @param name the attribute name of the subclass.
   * @param tag the tag defined in {@link IppValueTag}
   * @param value the byte[] value to be decoded based on the tag value.
   * @return The Attribute (a subclass of TextSyntax)
   */
  public static Attribute getTextAttribute(String name, byte tag, byte[] value)
  {
    // without language tag is rather easy - default locale
    if (tag == IppValueTag.NAME_WITHOUT_LANGUAGE
        || tag == IppValueTag.TEXT_WITHOUT_LANGUAGE)
      {
        TEXT_ATT_VALUE[0] = new String(value);
        TEXT_ATT_VALUE[1] = Locale.getDefault();
      }
    else
      {
        short langLength = convertToShort(value[0], value[1]);
        byte[] tmp = new byte[langLength];
        byte[] tmp2 = new byte[value.length - 4 - langLength];
        System.arraycopy(value, 2, tmp, 0, langLength);

        // parse into language/region
        String language = new String(tmp);
        String text = new String(tmp2);
        Locale locale = null;

        if (language.length() > 2)
          locale = new Locale(language.substring(0, 2), language.substring(3));
        else
          locale = new Locale(language);

        TEXT_ATT_VALUE[0] = text;
        TEXT_ATT_VALUE[1] = locale;
      }

    Class<?> attrClass = getClass(name);

    // There might be unknown attributes we have no mapped class for
    if (attrClass ==  null)
      return null;

    try
      {
        Constructor<?> c = attrClass.getDeclaredConstructor(TEXT_CLASS_ARRAY);
        return (Attribute) c.newInstance(TEXT_ATT_VALUE);
      }
    catch (SecurityException e)
      {
        // should not happen
      }
    catch (NoSuchMethodException e)
      {
        // should not happen
      }
    catch (IllegalAccessException e)
      {
        // should not happen, all fields are public
      }
    catch (InstantiationException e)
      {
        // should not happen, all fields are public
      }
    catch (InvocationTargetException e)
      {
        // should not happen, all fields are public
      }

    return null;
  }
}
