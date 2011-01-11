/* XHTMLWriter.java --
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

package gnu.xml.util;

import java.io.IOException;
import java.io.OutputStream;
import java.io.OutputStreamWriter;
import java.io.Writer;


/**
 * This extends XMLWriter to create a class which defaults to writing
 * XHTML text, preferring the US-ASCII encoding.  It adds no unique
 * functionality, only changing the defaults slightly to simplify writing
 * XHTML processing components by providing a bean class whose properties
 * have more convenient defaults.  An artifact of using the US-ASCII
 * encoding is that no XML declaration is written, so that HTML tools
 * that can't accept them will not become confused.  Components can treat
 * the output as UTF-8, ISO-8859-1, or US-ASCII without incurring any
 * data loss.
 *
 * @author David Brownell
 *
 * @deprecated Please use the javax.xml.stream APIs instead
 */
public class XHTMLWriter extends XMLWriter
{
    /**
     * Constructs this handler with System.out used to write
     * SAX events using the US-ASCII encoding, as XHTML.
     */
    public XHTMLWriter ()
    throws IOException
    {
        this (System.out);
    }

    /**
     * Constructs this handler such that the specified output stream
     * is used to write SAX events in the US-ASCII encoding, as XHTML.
     *
     * @param out Where US-ASCII encoding of the stream of SAX
     *  events will be sent.
     */
    public XHTMLWriter (OutputStream out)
    throws IOException
    {
        // not all JVMs understand "ASCII" as an encoding name, so
        // we use 8859_1 (they all seem to handle that one) and
        // make the echo handler filter out non-ASCII characters
        this (new OutputStreamWriter (out, "8859_1"), "US-ASCII");
    }

    /**
     * Constructs this handler such that the specified output stream
     * is used to write SAX events as XHTML.
     *
     * @param out Where the stream of SAX events will be written.
     */
    public XHTMLWriter (Writer out)
    {
        this (out, null);
    }

    /**
     * Constructs this handler such that the specified output stream
     * is used to write SAX events as XHTML, labeled with the specified
     * encoding.
     *
     * @param out Where the stream of SAX events will be written.
     * @param encoding If non-null, this names the encoding to be
     *  placed in the encoding declaration.
     */
    public XHTMLWriter (Writer out, String encoding)
    {
        super (out, encoding);
        setXhtml (true);
    }
}
