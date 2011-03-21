/* TextConsumer.java --
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

import java.io.*;

import org.xml.sax.*;

import gnu.xml.util.XMLWriter;


/**
 * Terminates a pipeline, consuming events to print them as well formed
 * XML (or XHTML) text.
 *
 * <p> Input must be well formed, and must include XML names (e.g. the
 * prefixes and prefix declarations must be present), or the output of
 * this class is undefined.
 *
 * @see NSFilter
 * @see WellFormednessFilter
 *
 * @author David Brownell
 */
public class TextConsumer extends XMLWriter implements EventConsumer
{
    /**
     * Constructs an event consumer which echoes its input as text,
     * optionally adhering to some basic XHTML formatting options
     * which increase interoperability with old (v3) browsers.
     *
     * <p> For the best interoperability, when writing as XHTML only
     * ASCII characters are emitted; other characters are turned to
     * entity or character references as needed, and no XML declaration
     * is provided in the document.
     */
    public TextConsumer (Writer w, boolean isXhtml)
    throws IOException
    {
        super (w, isXhtml ? "US-ASCII" : null);
        setXhtml (isXhtml);
    }

    /**
     * Constructs a consumer that writes its input as XML text.
     * XHTML rules are not followed.
     */
    public TextConsumer (Writer w)
    throws IOException
    {
        this (w, false);
    }

    /**
     * Constructs a consumer that writes its input as XML text,
     * encoded in UTF-8.  XHTML rules are not followed.
     */
    public TextConsumer (OutputStream out)
    throws IOException
    {
        this (new OutputStreamWriter (out, "UTF8"), false);
    }

    /** <b>EventConsumer</b> Returns the document handler being used. */
    public ContentHandler getContentHandler ()
        { return this; }

    /** <b>EventConsumer</b> Returns the dtd handler being used. */
    public DTDHandler getDTDHandler ()
        { return this; }

    /** <b>XMLReader</b>Retrieves a property (lexical and decl handlers) */
    public Object getProperty (String propertyId)
    throws SAXNotRecognizedException
    {
        if (EventFilter.LEXICAL_HANDLER.equals (propertyId))
            return this;
        if (EventFilter.DECL_HANDLER.equals (propertyId))
            return this;
        throw new SAXNotRecognizedException (propertyId);
    }
}
