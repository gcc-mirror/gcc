/* DoParse.java --
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

import gnu.java.lang.CPStringBuilder;

import java.io.IOException;

import org.xml.sax.ErrorHandler;
import org.xml.sax.InputSource;
import org.xml.sax.SAXException;
import org.xml.sax.SAXParseException;
import org.xml.sax.XMLReader;
import org.xml.sax.helpers.XMLReaderFactory;

import gnu.xml.pipeline.EventConsumer;
import gnu.xml.pipeline.EventFilter;
import gnu.xml.pipeline.NSFilter;
import gnu.xml.pipeline.PipelineFactory;
import gnu.xml.pipeline.TeeConsumer;
import gnu.xml.pipeline.ValidationConsumer;
import gnu.xml.pipeline.WellFormednessFilter;

/**
 * This class provides a driver which may be invoked from the command line
 * to process a document using a SAX2 parser and a specified XML processing
 * pipeline.
 * This facilitates some common types of command line tools, such as parsing an
 * XML document in order test it for well formedness or validity.
 *
 * <p>The SAX2 XMLReaderFactory should return a SAX2 XML parser which
 * supports both of the standardized extension handlers (for declaration
 * and lexical events).  That parser will be used to produce events.
 *
 * <p>The first parameter to the command gives the name of the document that
 * will be given to that processor.  If it is a file name, it is converted
 * to a URL first.
 *
 * <p>The second parameter describes a simple processing pipeline, and will
 * be used as input to {@link gnu.xml.pipeline.PipelineFactory}
 * methods which identify the processing to be done.  Examples of such a
 * pipeline include <pre>
 *
 *    nsfix | validate                <em>to validate the input document </em>
 *    nsfix | write ( stdout )        <em>to echo the file as XML text</em>
 *    dom | nsfix | write ( stdout )  <em>parse into DOM, print the result</em>
 * </pre>
 *
 * <p> Relatively complex pipelines can be described on the command line, but
 * not all interesting ones will require as little configuration as can be done
 * in that way.  Put filters like "nsfix", perhaps followed by "validate",
 * at the front of the pipeline so they can be optimized out if a parser
 * supports those modes natively.
 *
 * <p> If the parsing is aborted for any reason, the JVM will exit with a
 * failure code.  If a validating parse was done then both validation and
 * well formedness errors will cause a failure.  A non-validating parse
 * will report failure on well formedness errors.
 *
 * @see gnu.xml.pipeline.PipelineFactory
 *
 * @author David Brownell
 */
final public class DoParse
{
    private DoParse () { /* no instances allowed */ }

    // first reported nonrecoverable error
    private static SAXParseException    fatal;

    // error categories
    private static int                  errorCount;
    private static int                  fatalCount;

    /**
     * Command line invoker for this class; pass a filename or URL
     * as the first argument, and a pipeline description as the second.
     * Make sure to use filters to condition the input to stages that
     * require it; an <em>nsfix</em> filter will be a common requirement,
     * to restore syntax that SAX2 parsers delete by default.  Some
     * conditioning filters may be eliminated by setting parser options.
     * (For example, "nsfix" can set the "namespace-prefixes" feature to
     * a non-default value of "true".  In the same way, "validate" can set
     * the "validation" feature to "true".)
     */
    public static void main (String argv [])
    throws IOException
    {
        int             exitStatus = 1;

        if (argv.length != 2) {
            System.err.println ("Usage: DoParse [filename|URL] pipeline-spec");
            System.err.println ("Example pipeline specs:");
            System.err.println ("  'nsfix | validate'");
            System.err.println (
                "       ... restore namespace syntax, validate");
            System.err.println ("  'nsfix | write ( stdout )'");
            System.err.println (
                "       ... restore namespace syntax, write to stdout as XML"
                );
            System.exit (1);
        }

        try {
            //
            // Get input source for specified document (or try ;-)
            //
            argv [0] = Resolver.getURL (argv [0]);
            InputSource input = new InputSource (argv [0]);

            //
            // Get the producer, using the system default parser (which
            // can be overridden for this particular invocation).
            //
            // And the pipeline, using commandline options.
            //
            XMLReader           producer;
            EventConsumer       consumer;

            producer = XMLReaderFactory.createXMLReader ();

            //
            // XXX pipeline factory now has a pre-tokenized input
            // method, use it ... that way at least some params
            // can be written using quotes (have spaces, ...)
            //
            consumer = PipelineFactory.createPipeline (argv [1]);

            //
            // XXX want commandline option for tweaking error handler.
            // Want to be able to present warnings.
            //
            producer.setErrorHandler (new MyErrorHandler ());

            // XXX need facility enabling resolving to local DTDs

            //
            // Parse.  The pipeline may get optimized a bit, so we
            // can't always fail cleanly for validation without taking
            // a look at the filter stages.
            //
            EventFilter.bind (producer, consumer);
            producer.parse (input);

            try {
                if (producer.getFeature (
                        "http://org.xml/sax/features/validation"))
                    exitStatus = ((errorCount + fatalCount) > 0) ? 1 : 0;
                else if (fatalCount == 0)
                    exitStatus = 0;
            } catch (SAXException e) {
                if (hasValidator (consumer))
                    exitStatus = ((errorCount + fatalCount) > 0) ? 1 : 0;
                else if (fatalCount == 0)
                    exitStatus = 0;
            }

        } catch (java.net.MalformedURLException e) {
            System.err.println ("** Malformed URL: " + e.getMessage ());
            System.err.println ("Is '" + argv [0] + "' a non-existent file?");
            e.printStackTrace ();
                // e.g. FNF

        } catch (SAXParseException e) {
            if (e != fatal) {
                System.err.print (printParseException ("Parsing Aborted", e));
                e.printStackTrace ();
                if (e.getException () != null) {
                    System.err.println ("++ Wrapped exception:");
                    e.getException ().printStackTrace ();
                }
            }

        } catch (SAXException e) {
            Exception   x = e;
            if (e.getException () != null)
                x = e.getException ();
            x.printStackTrace ();

        } catch (Throwable t) {
            t.printStackTrace ();
        }

        System.exit (exitStatus);
    }

    // returns true if saw a validator (before end or unrecognized node)
    // false otherwise
    private static boolean hasValidator (EventConsumer e)
    {
        if (e == null)
            return false;
        if (e instanceof ValidationConsumer)
            return true;
        if (e instanceof TeeConsumer) {
            TeeConsumer t = (TeeConsumer) e;
            return hasValidator (t.getFirst ())
                || hasValidator (t.getRest ());
        }
        if (e instanceof WellFormednessFilter
                || e instanceof NSFilter
                )
            return hasValidator (((EventFilter)e).getNext ());

        // else ... gee, we can't know.  Assume not.

        return false;
    }

    static class MyErrorHandler implements ErrorHandler
    {
        // dump validation errors, but continue
        public void error (SAXParseException e)
        throws SAXParseException
        {
            errorCount++;
            System.err.print (printParseException ("Error", e));
        }

        public void warning (SAXParseException e)
        throws SAXParseException
        {
            // System.err.print (printParseException ("Warning", e));
        }

        // try to continue fatal errors, in case a parser reports more
        public void fatalError (SAXParseException e)
        throws SAXParseException
        {
            fatalCount++;
            if (fatal == null)
                fatal = e;
            System.err.print (printParseException ("Nonrecoverable Error", e));
        }
    }

    static private String printParseException (
        String                  label,
        SAXParseException       e
    ) {
        CPStringBuilder buf = new CPStringBuilder ();
        int             temp;

        buf.append ("** ");
        buf.append (label);
        buf.append (": ");
        buf.append (e.getMessage ());
        buf.append ('\n');
        if (e.getSystemId () != null) {
            buf.append ("   URI:  ");
            buf.append (e.getSystemId ());
            buf.append ('\n');
        }
        if ((temp = e.getLineNumber ()) != -1) {
            buf.append ("   line: ");
            buf.append (temp);
            buf.append ('\n');
        }
        if ((temp = e.getColumnNumber ()) != -1) {
            buf.append ("   char: ");
            buf.append (temp);
            buf.append ('\n');
        }

        return buf.toString ();
    }
}
