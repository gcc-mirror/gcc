/* TeeConsumer.java --
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

import org.xml.sax.Attributes;
import org.xml.sax.ContentHandler;
import org.xml.sax.DTDHandler;
import org.xml.sax.ErrorHandler;
import org.xml.sax.Locator;
import org.xml.sax.SAXException;
import org.xml.sax.SAXNotRecognizedException;
import org.xml.sax.ext.DeclHandler;
import org.xml.sax.ext.LexicalHandler;

/**
 * Fans its events out to two other consumers, a "tee" filter stage in an
 * event pipeline.  Networks can be assembled with multiple output points.
 *
 * <p> Error handling should be simple if you remember that exceptions
 * you throw will cancel later stages in that callback's pipeline, and
 * generally the producer will stop if it sees such an exception.  You
 * may want to protect your pipeline against such backflows, making a
 * kind of reverse filter (or valve?) so that certain exceptions thrown by
 * your pipeline will caught and handled before the producer sees them.
 * Just use a "try/catch" block, rememebering that really important
 * cleanup tasks should be in "finally" clauses.
 *
 * <p> That issue isn't unique to "tee" consumers, but tee consumers have
 * the additional twist that exceptions thrown by the first consumer
 * will cause the second consumer not to see the callback (except for
 * the endDocument callback, which signals state cleanup).
 *
 * @author David Brownell
 */
final public class TeeConsumer
        implements EventConsumer,
                ContentHandler, DTDHandler,
                LexicalHandler,DeclHandler
{
    private EventConsumer       first, rest;

    // cached to minimize time overhead
    private ContentHandler      docFirst, docRest;
    private DeclHandler         declFirst, declRest;
    private LexicalHandler      lexFirst, lexRest;


    /**
     * Constructs a consumer which sends all its events to the first
     * consumer, and then the second one.  If the first consumer throws
     * an exception, the second one will not see the event which
     * caused that exception to be reported.
     *
     * @param car The first consumer to get the events
     * @param cdr The second consumer to get the events
     */
    public TeeConsumer (EventConsumer car, EventConsumer cdr)
    {
        if (car == null || cdr == null)
            throw new NullPointerException ();
        first = car;
        rest = cdr;

        //
        // Cache the handlers.
        //
        docFirst = first.getContentHandler ();
        docRest = rest.getContentHandler ();
        // DTD handler isn't cached (rarely needed)

        try {
            declFirst = null;
            declFirst = (DeclHandler) first.getProperty (
                        EventFilter.DECL_HANDLER);
        } catch (SAXException e) {}
        try {
            declRest = null;
            declRest = (DeclHandler) rest.getProperty (
                        EventFilter.DECL_HANDLER);
        } catch (SAXException e) {}

        try {
            lexFirst = null;
            lexFirst = (LexicalHandler) first.getProperty (
                        EventFilter.LEXICAL_HANDLER);
        } catch (SAXException e) {}
        try {
            lexRest = null;
            lexRest = (LexicalHandler) rest.getProperty (
                        EventFilter.LEXICAL_HANDLER);
        } catch (SAXException e) {}
    }

/* FIXME
    /**
     * Constructs a pipeline, and is otherwise a shorthand for the
     * two-consumer constructor for this class.
     *
     * @param first Description of the first pipeline to get events,
     *  which will be passed to {@link PipelineFactory#createPipeline}
     * @param rest The second pipeline to get the events
     * /
        // constructor used by PipelineFactory
    public TeeConsumer (String first, EventConsumer rest)
    throws IOException
    {
        this (PipelineFactory.createPipeline (first), rest);
    }
*/

    /** Returns the first pipeline to get event calls. */
    public EventConsumer getFirst ()
        { return first; }

    /** Returns the second pipeline to get event calls. */
    public EventConsumer getRest ()
        { return rest; }

    /** Returns the content handler being used. */
    final public ContentHandler getContentHandler ()
    {
        if (docRest == null)
            return docFirst;
        if (docFirst == null)
            return docRest;
        return this;
    }

    /** Returns the dtd handler being used. */
    final public DTDHandler getDTDHandler ()
    {
        // not cached (hardly used)
        if (rest.getDTDHandler () == null)
            return first.getDTDHandler ();
        if (first.getDTDHandler () == null)
            return rest.getDTDHandler ();
        return this;
    }

    /** Returns the declaration or lexical handler being used. */
    final public Object getProperty (String id)
    throws SAXNotRecognizedException
    {
        //
        // in degenerate cases, we have no work to do.
        //
        Object  firstProp = null, restProp = null;

        try { firstProp = first.getProperty (id); }
        catch (SAXNotRecognizedException e) { /* ignore */ }
        try { restProp = rest.getProperty (id); }
        catch (SAXNotRecognizedException e) { /* ignore */ }

        if (restProp == null)
            return firstProp;
        if (firstProp == null)
            return restProp;

        //
        // we've got work to do; handle two builtin cases.
        //
        if (EventFilter.DECL_HANDLER.equals (id))
            return this;
        if (EventFilter.LEXICAL_HANDLER.equals (id))
            return this;

        //
        // non-degenerate, handled by both consumers, but we don't know
        // how to handle this.
        //
        throw new SAXNotRecognizedException ("can't tee: " + id);
    }

    /**
     * Provides the error handler to both subsequent nodes of
     * this filter stage.
     */
    public void setErrorHandler (ErrorHandler handler)
    {
        first.setErrorHandler (handler);
        rest.setErrorHandler (handler);
    }


    //
    // ContentHandler
    //
    public void setDocumentLocator (Locator locator)
    {
        // this call is not made by all parsers
        docFirst.setDocumentLocator (locator);
        docRest.setDocumentLocator (locator);
    }

    public void startDocument ()
    throws SAXException
    {
        docFirst.startDocument ();
        docRest.startDocument ();
    }

    public void endDocument ()
    throws SAXException
    {
        try {
            docFirst.endDocument ();
        } finally {
            docRest.endDocument ();
        }
    }

    public void startPrefixMapping (String prefix, String uri)
    throws SAXException
    {
        docFirst.startPrefixMapping (prefix, uri);
        docRest.startPrefixMapping (prefix, uri);
    }

    public void endPrefixMapping (String prefix)
    throws SAXException
    {
        docFirst.endPrefixMapping (prefix);
        docRest.endPrefixMapping (prefix);
    }

    public void skippedEntity (String name)
    throws SAXException
    {
        docFirst.skippedEntity (name);
        docRest.skippedEntity (name);
    }

    public void startElement (String uri, String localName,
            String qName, Attributes atts)
    throws SAXException
    {
        docFirst.startElement (uri, localName, qName, atts);
        docRest.startElement (uri, localName, qName, atts);
    }

    public void endElement (String uri, String localName, String qName)
    throws SAXException
    {
        docFirst.endElement (uri, localName, qName);
        docRest.endElement (uri, localName, qName);
    }

    public void processingInstruction (String target, String data)
    throws SAXException
    {
        docFirst.processingInstruction (target, data);
        docRest.processingInstruction (target, data);
    }

    public void characters (char ch [], int start, int length)
    throws SAXException
    {
        docFirst.characters (ch, start, length);
        docRest.characters (ch, start, length);
    }

    public void ignorableWhitespace (char ch [], int start, int length)
    throws SAXException
    {
        docFirst.ignorableWhitespace (ch, start, length);
        docRest.ignorableWhitespace (ch, start, length);
    }


    //
    // DTDHandler
    //
    public void notationDecl (String name, String publicId, String systemId)
    throws SAXException
    {
        DTDHandler      l1 = first.getDTDHandler ();
        DTDHandler      l2 = rest.getDTDHandler ();

        l1.notationDecl (name, publicId, systemId);
        l2.notationDecl (name, publicId, systemId);
    }

    public void unparsedEntityDecl (String name,
            String publicId, String systemId,
            String notationName
    ) throws SAXException
    {
        DTDHandler      l1 = first.getDTDHandler ();
        DTDHandler      l2 = rest.getDTDHandler ();

        l1.unparsedEntityDecl (name, publicId, systemId, notationName);
        l2.unparsedEntityDecl (name, publicId, systemId, notationName);
    }


    //
    // DeclHandler
    //
    public void attributeDecl (String eName, String aName,
        String type,
        String mode, String value)
    throws SAXException
    {
        declFirst.attributeDecl (eName, aName, type, mode, value);
        declRest.attributeDecl (eName, aName, type, mode, value);
    }

    public void elementDecl (String name, String model)
    throws SAXException
    {
        declFirst.elementDecl (name, model);
        declRest.elementDecl (name, model);
    }

    public void externalEntityDecl (String name,
        String publicId, String systemId)
    throws SAXException
    {
        declFirst.externalEntityDecl (name, publicId, systemId);
        declRest.externalEntityDecl (name, publicId, systemId);
    }

    public void internalEntityDecl (String name, String value)
    throws SAXException
    {
        declFirst.internalEntityDecl (name, value);
        declRest.internalEntityDecl (name, value);
    }


    //
    // LexicalHandler
    //
    public void comment (char ch [], int start, int length)
    throws SAXException
    {
        lexFirst.comment (ch, start, length);
        lexRest.comment (ch, start, length);
    }

    public void startCDATA ()
    throws SAXException
    {
        lexFirst.startCDATA ();
        lexRest.startCDATA ();
    }

    public void endCDATA ()
    throws SAXException
    {
        lexFirst.endCDATA ();
        lexRest.endCDATA ();
    }

    public void startEntity (String name)
    throws SAXException
    {
        lexFirst.startEntity (name);
        lexRest.startEntity (name);
    }

    public void endEntity (String name)
    throws SAXException
    {
        lexFirst.endEntity (name);
        lexRest.endEntity (name);
    }

    public void startDTD (String name, String publicId, String systemId)
    throws SAXException
    {
        lexFirst.startDTD (name, publicId, systemId);
        lexRest.startDTD (name, publicId, systemId);
    }

    public void endDTD ()
    throws SAXException
    {
        lexFirst.endDTD ();
        lexRest.endDTD ();
    }
}
