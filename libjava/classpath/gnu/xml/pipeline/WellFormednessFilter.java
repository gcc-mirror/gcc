/* WellFormednessFilter.java --
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

import java.util.EmptyStackException;
import java.util.Stack;

import org.xml.sax.Attributes;
import org.xml.sax.ErrorHandler;
import org.xml.sax.Locator;
import org.xml.sax.SAXException;
import org.xml.sax.SAXParseException;

/**
 * This filter reports fatal exceptions in the case of event streams that
 * are not well formed.  The rules currently tested include: <ul>
 *
 *      <li>setDocumentLocator ... may be called only before startDocument
 *
 *      <li>startDocument/endDocument ... must be paired, and all other
 *      calls (except setDocumentLocator) must be nested within these.
 *
 *      <li>startElement/endElement ... must be correctly paired, and
 *      may never appear within CDATA sections.
 *
 *      <li>comment ... can't contain "--"
 *
 *      <li>character data ... can't contain "]]&gt;"
 *
 *      <li>whitespace ... can't contain CR
 *
 *      <li>whitespace and character data must be within an element
 *
 *      <li>processing instruction ... can't contain "?&gt;" or CR
 *
 *      <li>startCDATA/endCDATA ... must be correctly paired.
 *
 *      </ul>
 *
 * <p> Other checks for event stream correctness may be provided in
 * the future.  For example, insisting that
 * entity boundaries nest correctly,
 * namespace scopes nest correctly,
 * namespace values never contain relative URIs,
 * attributes don't have "&lt;" characters;
 * and more.
 *
 * @author David Brownell
 */
public final class WellFormednessFilter extends EventFilter
{
    private boolean             startedDoc;
    private Stack               elementStack = new Stack ();
    private boolean             startedCDATA;
    private String              dtdState = "before";


    /**
     * Swallows all events after performing well formedness checks.
     */
        // constructor used by PipelineFactory
    public WellFormednessFilter ()
        { this (null); }


    /**
     * Passes events through to the specified consumer, after first
     * processing them.
     */
        // constructor used by PipelineFactory
    public WellFormednessFilter (EventConsumer consumer)
    {
        super (consumer);

        setContentHandler (this);
        setDTDHandler (this);

        try {
            setProperty (LEXICAL_HANDLER, this);
        } catch (SAXException e) { /* can't happen */ }
    }

    /**
     * Resets state as if any preceding event stream was well formed.
     * Particularly useful if it ended through some sort of error,
     * and the endDocument call wasn't made.
     */
    public void reset ()
    {
        startedDoc = false;
        startedCDATA = false;
        elementStack.removeAllElements ();
    }


    private SAXParseException getException (String message)
    {
        SAXParseException       e;
        Locator                 locator = getDocumentLocator ();

        if (locator == null)
            return new SAXParseException (message, null, null, -1, -1);
        else
            return new SAXParseException (message, locator);
    }

    private void fatalError (String message)
    throws SAXException
    {
        SAXParseException       e = getException (message);
        ErrorHandler            handler = getErrorHandler ();

        if (handler != null)
            handler.fatalError (e);
        throw e;
    }

    /**
     * Throws an exception when called after startDocument.
     *
     * @param locator the locator, to be used in error reporting or relative
     *  URI resolution.
     *
     * @exception IllegalStateException when called after the document
     *  has already been started
     */
    public void setDocumentLocator (Locator locator)
    {
        if (startedDoc)
            throw new IllegalStateException (
                    "setDocumentLocator called after startDocument");
        super.setDocumentLocator (locator);
    }

    public void startDocument () throws SAXException
    {
        if (startedDoc)
            fatalError ("startDocument called more than once");
        startedDoc = true;
        startedCDATA = false;
        elementStack.removeAllElements ();
        super.startDocument ();
    }

    public void startElement (
        String uri, String localName,
        String qName, Attributes atts
    ) throws SAXException
    {
        if (!startedDoc)
            fatalError ("callback outside of document?");
        if ("inside".equals (dtdState))
            fatalError ("element inside DTD?");
        else
            dtdState = "after";
        if (startedCDATA)
            fatalError ("element inside CDATA section");
        if (qName == null || "".equals (qName))
            fatalError ("startElement name missing");
        elementStack.push (qName);
        super.startElement (uri, localName, qName, atts);
    }

    public void endElement (String uri, String localName, String qName)
    throws SAXException
    {
        if (!startedDoc)
            fatalError ("callback outside of document?");
        if (startedCDATA)
            fatalError ("element inside CDATA section");
        if (qName == null || "".equals (qName))
            fatalError ("endElement name missing");

        try {
            String      top = (String) elementStack.pop ();

            if (!qName.equals (top))
                fatalError ("<" + top + " ...>...</" + qName + ">");
            // XXX could record/test namespace info
        } catch (EmptyStackException e) {
            fatalError ("endElement without startElement:  </" + qName + ">");
        }
        super.endElement (uri, localName, qName);
    }

    public void endDocument () throws SAXException
    {
        if (!startedDoc)
            fatalError ("callback outside of document?");
        dtdState = "before";
        startedDoc = false;
        super.endDocument ();
    }


    public void startDTD (String root, String publicId, String systemId)
    throws SAXException
    {
        if (!startedDoc)
            fatalError ("callback outside of document?");
    if ("before" != dtdState)
            fatalError ("two DTDs?");
        if (!elementStack.empty ())
            fatalError ("DTD must precede root element");
        dtdState = "inside";
        super.startDTD (root, publicId, systemId);
    }

    public void notationDecl (String name, String publicId, String systemId)
    throws SAXException
    {
// FIXME: not all parsers will report startDTD() ...
// we'd rather insist we're "inside".
    if ("after" == dtdState)
            fatalError ("not inside DTD");
        super.notationDecl (name, publicId, systemId);
    }

    public void unparsedEntityDecl (String name,
        String publicId, String systemId, String notationName)
    throws SAXException
    {
// FIXME: not all parsers will report startDTD() ...
// we'd rather insist we're "inside".
    if ("after" == dtdState)
            fatalError ("not inside DTD");
        super.unparsedEntityDecl (name, publicId, systemId, notationName);
    }

    // FIXME:  add the four DeclHandler calls too

    public void endDTD ()
    throws SAXException
    {
        if (!startedDoc)
            fatalError ("callback outside of document?");
        if ("inside" != dtdState)
            fatalError ("DTD ends without start?");
        dtdState = "after";
        super.endDTD ();
    }

    public void characters (char ch [], int start, int length)
    throws SAXException
    {
        int here = start, end = start + length;
        if (elementStack.empty ())
            fatalError ("characters must be in an element");
        while (here < end) {
            if (ch [here++] != ']')
                continue;
            if (here == end)    // potential problem ...
                continue;
            if (ch [here++] != ']')
                continue;
            if (here == end)    // potential problem ...
                continue;
            if (ch [here++] == '>')
                fatalError ("character data can't contain \"]]>\"");
        }
        super.characters (ch, start, length);
    }

    public void ignorableWhitespace (char ch [], int start, int length)
    throws SAXException
    {
        int here = start, end = start + length;
        if (elementStack.empty ())
            fatalError ("characters must be in an element");
        while (here < end) {
            if (ch [here++] == '\r')
                fatalError ("whitespace can't contain CR");
        }
        super.ignorableWhitespace (ch, start, length);
    }

    public void processingInstruction (String target, String data)
    throws SAXException
    {
        if (data.indexOf ('\r') > 0)
            fatalError ("PIs can't contain CR");
        if (data.indexOf ("?>") > 0)
            fatalError ("PIs can't contain \"?>\"");
    }

    public void comment (char ch [], int start, int length)
    throws SAXException
    {
        if (!startedDoc)
            fatalError ("callback outside of document?");
        if (startedCDATA)
            fatalError ("comments can't nest in CDATA");
        int here = start, end = start + length;
        while (here < end) {
            if (ch [here] == '\r')
                fatalError ("comments can't contain CR");
            if (ch [here++] != '-')
                continue;
            if (here == end)
                fatalError ("comments can't end with \"--->\"");
            if (ch [here++] == '-')
                fatalError ("comments can't contain \"--\"");
        }
        super.comment (ch, start, length);
    }

    public void startCDATA ()
    throws SAXException
    {
        if (!startedDoc)
            fatalError ("callback outside of document?");
        if (startedCDATA)
            fatalError ("CDATA starts can't nest");
        startedCDATA = true;
        super.startCDATA ();
    }

    public void endCDATA ()
    throws SAXException
    {
        if (!startedDoc)
            fatalError ("callback outside of document?");
        if (!startedCDATA)
            fatalError ("CDATA end without start?");
        startedCDATA = false;
        super.endCDATA ();
    }
}
