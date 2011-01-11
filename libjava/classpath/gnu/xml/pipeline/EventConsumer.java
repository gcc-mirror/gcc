/* EventConsumer.java --
   Copyright (C) 1999,2000,2001 Free Software Foundation, Inc.

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

package gnu.xml.pipeline;

import org.xml.sax.*;


/**
 * Collects the event consumption apparatus of a SAX pipeline stage.
 * Consumers which permit some handlers or other characteristics to be
 * configured will provide methods to support that configuration.
 *
 * <p> Two important categories of consumers include <em>filters</em>, which
 * process events and pass them on to other consumers, and <em>terminus</em>
 * (or <em>terminal</em>) stages, which don't pass events on.  Filters are not
 * necessarily derived from the {@link EventFilter} class, although that
 * class can substantially simplify their construction by automating the
 * most common activities.
 *
 * <p> Event consumers which follow certain conventions for the signatures
 * of their constructors can be automatically assembled into pipelines
 * by the {@link PipelineFactory} class.
 *
 * @author David Brownell
 */
public interface EventConsumer
{
    /** Most stages process these core SAX callbacks. */
    public ContentHandler getContentHandler ();

    /** Few stages will use unparsed entities. */
    public DTDHandler getDTDHandler ();

    /**
     * This method works like the SAX2 XMLReader method of the same name,
     * and is used to retrieve the optional lexical and declaration handlers
     * in a pipeline.
     *
     * @param id This is a URI identifying the type of property desired.
     * @return The value of that property, if it is defined.
     *
     * @exception SAXNotRecognizedException Thrown if the particular
     *  pipeline stage does not understand the specified identifier.
     */
    public Object getProperty (String id)
    throws SAXNotRecognizedException;

    /**
     * This method provides a filter stage with a handler that abstracts
     * presentation of warnings and both recoverable and fatal errors.
     * Most pipeline stages should share a single policy and mechanism
     * for such reports, since application components require consistency
     * in such activities.  Accordingly, typical responses to this method
     * invocation involve saving the handler for use; filters will pass
     * it on to any other consumers they use.
     *
     * @param handler encapsulates error handling policy for this stage
     */
    public void setErrorHandler (ErrorHandler handler);
}
