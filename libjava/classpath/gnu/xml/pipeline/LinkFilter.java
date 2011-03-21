/* LinkFilter.java --
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

import java.io.IOException;
import java.net.URL;
import java.util.Enumeration;
import java.util.Vector;

import org.xml.sax.Attributes;
import org.xml.sax.SAXException;


/**
 * Pipeline filter to remember XHTML links found in a document,
 * so they can later be crawled.  Fragments are not counted, and duplicates
 * are ignored.  Callers are responsible for filtering out URLs they aren't
 * interested in.  Events are passed through unmodified.
 *
 * <p> Input MUST include a setDocumentLocator() call, as it's used to
 * resolve relative links in the absence of a "base" element.  Input MUST
 * also include namespace identifiers, since it is the XHTML namespace
 * identifier which is used to identify the relevant elements.
 *
 * <p><em>FIXME:</em> handle xml:base attribute ... in association with
 * a stack of base URIs.  Similarly, recognize/support XLink data.
 *
 * @author David Brownell
 */
public class LinkFilter extends EventFilter
{
    // for storing URIs
    private Vector              vector = new Vector ();

        // struct for "full" link record (tbd)
        // these for troubleshooting original source:
        //      original uri
        //      uri as resolved (base, relative, etc)
        //      URI of originating doc
        //      line #
        //      original element + attrs (img src, desc, etc)

        // XLink model of the link ... for inter-site pairups ?

    private String              baseURI;

    private boolean             siteRestricted = false;

    //
    // XXX leverage blacklist info (like robots.txt)
    //
    // XXX constructor w/param ... pipeline for sending link data
    // probably XHTML --> XLink, providing info as sketched above
    //


    /**
     * Constructs a new event filter, which collects links in private data
     * structure for later enumeration.
     */
        // constructor used by PipelineFactory
    public LinkFilter ()
    {
        super.setContentHandler (this);
    }


    /**
     * Constructs a new event filter, which collects links in private data
     * structure for later enumeration and passes all events, unmodified,
     * to the next consumer.
     */
        // constructor used by PipelineFactory
    public LinkFilter (EventConsumer next)
    {
        super (next);
        super.setContentHandler (this);
    }


    /**
     * Returns an enumeration of the links found since the filter
     * was constructed, or since removeAllLinks() was called.
     *
     * @return enumeration of strings.
     */
    public Enumeration getLinks ()
    {
        return vector.elements ();
    }

    /**
     * Removes records about all links reported to the event
     * stream, as if the filter were newly created.
     */
    public void removeAllLinks ()
    {
        vector = new Vector ();
    }


    /**
     * Collects URIs for (X)HTML content from elements which hold them.
     */
    public void startElement (
        String          uri,
        String          localName,
        String          qName,
        Attributes      atts
    ) throws SAXException
    {
        String  link;

        // Recognize XHTML links.
        if ("http://www.w3.org/1999/xhtml".equals (uri)) {

            if ("a".equals (localName) || "base".equals (localName)
                    || "area".equals (localName))
                link = atts.getValue ("href");
            else if ("iframe".equals (localName) || "frame".equals (localName))
                link = atts.getValue ("src");
            else if ("blockquote".equals (localName) || "q".equals (localName)
                    || "ins".equals (localName) || "del".equals (localName))
                link = atts.getValue ("cite");
            else
                link = null;
            link = maybeAddLink (link);

            // "base" modifies designated baseURI
            if ("base".equals (localName) && link != null)
                baseURI = link;

            if ("iframe".equals (localName) || "img".equals (localName))
                maybeAddLink (atts.getValue ("longdesc"));
        }

        super.startElement (uri, localName, qName, atts);
    }

    private String maybeAddLink (String link)
    {
        int             index;

        // ignore empty links and fragments inside docs
        if (link == null)
            return null;
        if ((index = link.indexOf ("#")) >= 0)
            link = link.substring (0, index);
        if (link.equals (""))
            return null;

        try {
            // get the real URI
            URL         base = new URL ((baseURI != null)
                                    ? baseURI
                                    : getDocumentLocator ().getSystemId ());
            URL         url = new URL (base, link);

            link = url.toString ();

            // ignore duplicates
            if (vector.contains (link))
                return link;

            // other than what "base" does, stick to original site:
            if (siteRestricted) {
                // don't switch protocols
                if (!base.getProtocol ().equals (url.getProtocol ()))
                    return link;
                // don't switch servers
                if (base.getHost () != null
                        && !base.getHost ().equals (url.getHost ()))
                    return link;
            }

            vector.addElement (link);

            return link;

        } catch (IOException e) {
            // bad URLs we don't want
        }
        return null;
    }

    /**
     * Reports an error if no Locator has been made available.
     */
    public void startDocument ()
    throws SAXException
    {
        if (getDocumentLocator () == null)
            throw new SAXException ("no Locator!");
    }

    /**
     * Forgets about any base URI information that may be recorded.
     * Applications will often want to call removeAllLinks(), likely
     * after examining the links which were reported.
     */
    public void endDocument ()
    throws SAXException
    {
        baseURI = null;
        super.endDocument ();
    }
}
