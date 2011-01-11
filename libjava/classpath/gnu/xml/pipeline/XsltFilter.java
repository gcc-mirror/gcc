/* XsltFilter.java --
   Copyright (C) 2001 Free Software Foundation, Inc.

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

import java.io.IOException;

import javax.xml.transform.TransformerFactory;
import javax.xml.transform.TransformerConfigurationException;
import javax.xml.transform.sax.*;
import javax.xml.transform.stream.StreamSource;

import org.xml.sax.SAXException;
import org.xml.sax.ext.LexicalHandler;


/**
 * Packages an XSLT transform as a pipeline component.
 * Note that all DTD events (callbacks to DeclHandler and DTDHandler
 * interfaces) are discarded, although XSLT transforms may be set up to
 * use the LexicalHandler to write DTDs with only an external subset.
 * Not every XSLT engine will necessarily be usable with this filter,
 * but current versions of
 * <a href="http://saxon.sourceforge.net">SAXON</a> and
 * <a href="http://xml.apache.org/xalan-j">Xalan</a> should work well.
 *
 * @see TransformerFactory
 *
 * @author David Brownell
 */
final public class XsltFilter extends EventFilter
{
    /**
     * Creates a filter that performs the specified transform.
     * Uses the JAXP 1.1 interfaces to access the default XSLT
     * engine configured for in the current execution context,
     * and parses the stylesheet without custom EntityResolver
     * or ErrorHandler support.
     *
     * @param stylesheet URI for the stylesheet specifying the
     *  XSLT transform
     * @param next provides the ContentHandler and LexicalHandler
     *  to receive XSLT output.
     * @exception SAXException if the stylesheet can't be parsed
     * @exception IOException if there are difficulties
     *  bootstrapping the XSLT engine, such as it not supporting
     *  SAX well enough to use this way.
     */
    public XsltFilter (String stylesheet, EventConsumer next)
    throws SAXException, IOException
    {
        // First, get a transformer with the stylesheet preloaded
        TransformerFactory      tf = null;
        TransformerHandler      th;

        try {
            SAXTransformerFactory       stf;

            tf = TransformerFactory.newInstance ();
            if (!tf.getFeature (SAXTransformerFactory.FEATURE)  // sax inputs
                    || !tf.getFeature (SAXResult.FEATURE)       // sax outputs
                    || !tf.getFeature (StreamSource.FEATURE)    // stylesheet
                    )
                throw new IOException ("XSLT factory ("
                    + tf.getClass ().getName ()
                    + ") does not support SAX");
            stf = (SAXTransformerFactory) tf;
            th = stf.newTransformerHandler (new StreamSource (stylesheet));
        } catch (TransformerConfigurationException e) {
            throw new IOException ("XSLT factory ("
                + (tf == null
                        ? "none available"
                        : tf.getClass ().getName ())
                + ") configuration error, "
                + e.getMessage ()
                );
        }

        // Hook its outputs up to the pipeline ...
        SAXResult               out = new SAXResult ();

        out.setHandler (next.getContentHandler ());
        try {
            LexicalHandler      lh;
            lh = (LexicalHandler) next.getProperty (LEXICAL_HANDLER);
            out.setLexicalHandler (lh);
        } catch (Exception e) {
            // ignore
        }
        th.setResult (out);

        // ... and make sure its inputs look like ours.
        setContentHandler (th);
        setProperty (LEXICAL_HANDLER, th);
    }
}
