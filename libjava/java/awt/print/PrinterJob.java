/* PrinterJob.java -- This job is the printer control class
   Copyright (C) 1999 Free Software Foundation, Inc.

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
Free Software Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA
02111-1307 USA.

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


package java.awt.print;

/**
  * This class controls printing.
  *
  * @author Aaron M. Renn (arenn@urbanophile.com)
  */
public abstract class PrinterJob
{

/*
 * Class Methods
 */

/**
  * Creates a new print job.
  *
  * @return A <code>PrinterJob</code> object for the newly created print job.
  */
public static PrinterJob
getPrinterJob()
{
  // FIXME: Need to fix this to load a default implementation instance.
  return(null);
}

/*************************************************************************/

/*
 * Constructors
 */

/**
  * Initializes a new instance of <code>PrinterJob</code>. 
  */
public
PrinterJob()
{
  ;
}

/*************************************************************************/

/*
 * Instance Methods
 */

/**
  * Returns the number of copies to be printed.
  *
  * @return The number of copies to be printed.
  */
public abstract int
getCopies();

/*************************************************************************/

/**
  * Sets the number of copies to be printed.
  *
  * @param copies The number of copies to be printed.
  */
public abstract void setCopies (int copies);

/*************************************************************************/

/**
  * Returns the name of the print job.
  *
  * @return The name of the print job.
  */
public abstract String
getJobName();

/*************************************************************************/

/**
  * Sets the name of the print job.
  *
  * @param job_name The name of the print job.
  */
public abstract void setJobName (String job_name);

/*************************************************************************/

/**
  * Returns the printing user name.
  *
  * @return The printing username.
  */
public abstract String
getUserName();

/*************************************************************************/

/**
  * Cancels an in progress print job.
  */
public abstract void
cancel();

/*************************************************************************/

/**
  * Tests whether or not this job has been cancelled.
  *
  * @param <code>true</code> if this job has been cancelled, <code>false</code>
  * otherwise.
  */
public abstract boolean
isCancelled();

/*************************************************************************/

/**
  * Returns an instance of the default page which will have the default
  * paper and orientation.
  *
  * @return A default instance of <code>PageFormat</code>.
  */
public PageFormat
defaultPage()
{
  return(new PageFormat());
}

/*************************************************************************/

/**
  * Clones the specified <code>PageFormat</code> object then alters the
  * clone so that it represents the default page format.
  *
  * @param page_format The <code>PageFormat</code> to clone.
  *
  * @return A new default page format.
  */
public abstract PageFormat
defaultPage(PageFormat page_format);

/*************************************************************************/

/**
  * Displays a dialog box to the user which allows the page format
  * attributes to be modified.
  *
  * @param page_format The <code>PageFormat</code> object to modify.
  *
  * @return The modified <code>PageFormat</code>.
  */
public abstract PageFormat
pageDialog(PageFormat page_format);

/*************************************************************************/

/**
  * Prints the pages.
  */
public abstract void print () throws PrinterException;

/**
  * Displays a dialog box to the user which allows the print job
  * attributes to be modified.
  *
  * @return <code>false</code> if the user cancels the dialog box,
  * <code>true</code> otherwise.
  */
public abstract boolean
printDialog();

/*************************************************************************/

/**
  * This sets the pages that are to be printed.
  *
  * @param pageable The pages to be printed, which may not be <code>null</code>.
  */
public abstract void
setPageable(Pageable pageable);

/*************************************************************************/

/**
  * Sets this specified <code>Printable</code> as the one to use for
  * rendering the pages on the print device.
  *
  * @param printable The <code>Printable</code> for the print job.
  */
public abstract void
setPrintable(Printable printable);

/*************************************************************************/

/**
  * Sets the <code>Printable</code> and the page format for the pages
  * to be printed.
  *
  * @param printable The <code>Printable</code> for the print job.
  * @param page_format The <code>PageFormat</code> for the print job.
  */
public abstract void
setPrintable(Printable printable, PageFormat page_format);

/*************************************************************************/

/**
  * Makes any alterations to the specified <code>PageFormat</code>
  * necessary to make it work with the current printer.  The alterations
  * are made to a clone of the input object, which is then returned.
  *
  * @param page_format The <code>PageFormat</code> to validate.
  *
  * @return The validated <code>PageFormat</code>.
  */
public abstract PageFormat
validatePage(PageFormat page);

} // class PrinterJob

