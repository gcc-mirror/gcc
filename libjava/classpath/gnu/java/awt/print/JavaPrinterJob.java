/* JavaPrinterJob.java -- AWT printing implemented on javax.print.
   Copyright (C) 2006  Free Software Foundation, Inc.

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


package gnu.java.awt.print;

import java.awt.HeadlessException;
import java.awt.print.PageFormat;
import java.awt.print.Pageable;
import java.awt.print.Printable;
import java.awt.print.PrinterException;
import java.awt.print.PrinterJob;
import java.util.Locale;

import javax.print.CancelablePrintJob;
import javax.print.DocFlavor;
import javax.print.DocPrintJob;
import javax.print.PrintException;
import javax.print.PrintService;
import javax.print.PrintServiceLookup;
import javax.print.ServiceUI;
import javax.print.attribute.HashPrintRequestAttributeSet;
import javax.print.attribute.IntegerSyntax;
import javax.print.attribute.PrintRequestAttributeSet;
import javax.print.attribute.TextSyntax;
import javax.print.attribute.standard.Copies;
import javax.print.attribute.standard.JobName;
import javax.print.attribute.standard.OrientationRequested;
import javax.print.attribute.standard.RequestingUserName;

/**
 * This is the default implementation of PrinterJob
 *
 * @author Sven de Marothy
 */
public class JavaPrinterJob extends PrinterJob
{
  /**
   * The print service associated with this job
   */
  private PrintService printer = null;

  /**
   * Printing options;
   */
  private PrintRequestAttributeSet attributes;

  /**
   * Available print services
   */
  private static PrintService[] services;

  /**
   * The actual print job.
   */
  private DocPrintJob printJob;

  /**
   * The Printable object to print.
   */
  private Printable printable;

  /**
   * Page format.
   */
  private PageFormat pageFormat;

  /**
   * A pageable, or null
   */
  private Pageable pageable = null;

  /**
   * Cancelled or not
   */
  private boolean cancelled = false;

  static
  {
    // lookup all services without any constraints
    services = PrintServiceLookup.lookupPrintServices
      (DocFlavor.INPUT_STREAM.POSTSCRIPT, null);
  }

  private static final Class copyClass = (new Copies(1)).getClass();
  private static final Class jobNameClass = (new JobName("", null)).getClass();
  private static final Class userNameClass = (new RequestingUserName("", null)).getClass();

  /**
   * Initializes a new instance of <code>PrinterJob</code>.
   */
  public JavaPrinterJob()
  {
    attributes = new HashPrintRequestAttributeSet();
    setCopies(1);
    setJobName("Java Printing");
    pageFormat = new PageFormat(); // default page format.
  }

  private void getPageAttributes()
  {
    OrientationRequested orientation = (OrientationRequested)
      attributes.get( OrientationRequested.LANDSCAPE.getCategory() );
    if( orientation == null)
      return;

    if( orientation.equals(OrientationRequested.PORTRAIT) )
      pageFormat.setOrientation(PageFormat.PORTRAIT);
    else if( orientation.equals(OrientationRequested.LANDSCAPE) )
      pageFormat.setOrientation(PageFormat.LANDSCAPE);
    else if( orientation.equals(OrientationRequested.REVERSE_LANDSCAPE) )
        pageFormat.setOrientation(PageFormat.REVERSE_LANDSCAPE);
  }

  /**
   * Returns the number of copies to be printed.
   *
   * @return The number of copies to be printed.
   */
  public int getCopies()
  {
    return ((IntegerSyntax)attributes.get( jobNameClass )).getValue();
  }

  /**
   * Sets the number of copies to be printed.
   *
   * @param copies The number of copies to be printed.
   */
  public void setCopies(int copies)
  {
    attributes.add( new Copies( copies ) );
  }

  /**
   * Returns the name of the print job.
   *
   * @return The name of the print job.
   */
  public String getJobName()
  {
    return ((TextSyntax)attributes.get( jobNameClass )).getValue();
  }

  /**
   * Sets the name of the print job.
   *
   * @param job_name The name of the print job.
   */
  public void setJobName(String job_name)
  {
    attributes.add( new JobName(job_name, Locale.getDefault()) );
  }

  /**
   * Returns the printing user name.
   *
   * @return The printing username.
   */
  public String getUserName()
  {
    return ((TextSyntax)attributes.get( userNameClass )).getValue();
  }

  /**
   * Cancels an in progress print job.
   */
  public void cancel()
  {
    try
      {
        if(printJob != null && (printJob instanceof CancelablePrintJob))
          {
            ((CancelablePrintJob)printJob).cancel();
            cancelled = true;
          }
      }
    catch(PrintException pe)
      {
      }
  }

  /**
   * Tests whether or not this job has been cancelled.
   *
   * @return <code>true</code> if this job has been cancelled, <code>false</code>
   * otherwise.
   */
  public boolean isCancelled()
  {
    return cancelled;
  }

  /**
   * Clones the specified <code>PageFormat</code> object then alters the
   * clone so that it represents the default page format.
   *
   * @param page_format The <code>PageFormat</code> to clone.
   *
   * @return A new default page format.
   */
  public PageFormat defaultPage(PageFormat page_format)
  {
    return new PageFormat();
  }

  /**
   * Displays a dialog box to the user which allows the page format
   * attributes to be modified.
   *
   * @param page_format The <code>PageFormat</code> object to modify.
   *
   * @return The modified <code>PageFormat</code>.
   */
  public PageFormat pageDialog(PageFormat page_format)
    throws HeadlessException
  {
    return defaultPage(null);
  }

  /**
   * Prints the pages.
   */
  public void print() throws PrinterException
  {
    if( printable == null && pageable == null ) // nothing to print?
      return;

    PostScriptGraphics2D pg = new PostScriptGraphics2D( this );
    SpooledDocument doc = pg.spoolPostScript( printable, pageFormat,
                                              pageable );

    cancelled = false;
    printJob = printer.createPrintJob();
    try
      {
        printJob.print(doc, attributes);
      }
    catch (PrintException pe)
      {
        PrinterException p = new PrinterException();
        p.initCause(pe);
        throw p;
      }
    // no printjob active.
    printJob = null;
  }

  /**
   * Prints the page with given attributes.
   */
  public void print (PrintRequestAttributeSet attributes)
    throws PrinterException
  {
    this.attributes = attributes;
    print();
  }

  /**
   * Displays a dialog box to the user which allows the print job
   * attributes to be modified.
   *
   * @return <code>false</code> if the user cancels the dialog box,
   * <code>true</code> otherwise.
   */
  public boolean printDialog() throws HeadlessException
  {
    return printDialog( attributes );
  }

  /**
   * Displays a dialog box to the user which allows the print job
   * attributes to be modified.
   *
   * @return <code>false</code> if the user cancels the dialog box,
   * <code>true</code> otherwise.
   */
  public boolean printDialog(PrintRequestAttributeSet attributes)
    throws HeadlessException
  {
    PrintService chosenPrinter = ServiceUI.printDialog
      (null, 50, 50, services, null,
       DocFlavor.INPUT_STREAM.POSTSCRIPT, attributes);

    getPageAttributes();

    if( chosenPrinter != null )
      {
        try
          {
            setPrintService( chosenPrinter );
          }
        catch(PrinterException pe)
          {
            // Should not happen.
          }
        return true;
      }
    return false;
  }

  /**
   * This sets the pages that are to be printed.
   *
   * @param pageable The pages to be printed, which may not be <code>null</code>.
   */
  public void setPageable(Pageable pageable)
  {
    if( pageable == null )
      throw new NullPointerException("Pageable cannot be null.");
    this.pageable = pageable;
  }

  /**
   * Sets this specified <code>Printable</code> as the one to use for
   * rendering the pages on the print device.
   *
   * @param printable The <code>Printable</code> for the print job.
   */
  public void setPrintable(Printable printable)
  {
    this.printable = printable;
  }

  /**
   * Sets the <code>Printable</code> and the page format for the pages
   * to be printed.
   *
   * @param printable The <code>Printable</code> for the print job.
   * @param page_format The <code>PageFormat</code> for the print job.
   */
  public void setPrintable(Printable printable, PageFormat page_format)
  {
    this.printable = printable;
    this.pageFormat = page_format;
  }

  /**
   * Makes any alterations to the specified <code>PageFormat</code>
   * necessary to make it work with the current printer.  The alterations
   * are made to a clone of the input object, which is then returned.
   *
   * @param page_format The <code>PageFormat</code> to validate.
   *
   * @return The validated <code>PageFormat</code>.
   */
  public PageFormat validatePage(PageFormat page_format)
  {
    // FIXME
    return page_format;
  }

  /**
   * Change the printer for this print job to service.  Subclasses that
   * support setting the print service override this method.  Throws
   * PrinterException when the class doesn't support setting the printer,
   * the service doesn't support Pageable or Printable interfaces for 2D
   * print output.
   * @param service The new printer to use.
   * @throws PrinterException if service is not valid.
   */
  public void setPrintService(PrintService service)
    throws PrinterException
  {
    if(!service.isDocFlavorSupported(DocFlavor.INPUT_STREAM.POSTSCRIPT))
      throw new PrinterException("This printer service is not supported.");
    printer = service;
  }
}
