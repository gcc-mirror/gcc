/* PrintJob.java -- A print job class
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


package java.awt;

/**
  * This abstract class represents a print job.
  *
  * @author Aaron M. Renn (arenn@urbanophile.com)
  */
public abstract class PrintJob
{

/*
 * Constructors
 */

/**
  * This method initializes a new instance of <code>PrintJob</code>.
  */
public
PrintJob()
{
}

/*************************************************************************/

/*
 * Instance Methods
 */

/**
  * Returns a graphics context suitable for rendering the next page.
  *
  * @return A graphics context for printing the next page.
  */
public abstract Graphics
getGraphics();

/*************************************************************************/

/**
  * Returns the dimension of the page in pixels.  The resolution will be
  * chosen to be similar to the on screen image.
  *
  * @return The page dimensions.
  */
public abstract Dimension
getPageDimension();

/*************************************************************************/

/**
  * Returns the resolution of the page in pixels per inch.
  *
  * @return The resolution of the page in pixels per inch.
  */
public abstract int
getPageResolution();

/*************************************************************************/

/**
  * Tests whether or not the last page will be printed first.
  *
  * @return <code>true</code> if the last page prints first, <code>false</code>
  * otherwise.
  */
public abstract boolean
lastPageFirst();

/*************************************************************************/

/**
  * Informs the print job that printing is complete.
  */
public abstract void
end();

/*************************************************************************/

/**
  * This method explicitly ends the print job in the event the job
  * becomes un-referenced without the application having done so.
  */
public void
finalize()
{
  end();
}

} // class PrintJob

