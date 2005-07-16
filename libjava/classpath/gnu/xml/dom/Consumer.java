/* Consumer.java -- 
   Copyright (C) 2001,2004 Free Software Foundation, Inc.

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


package gnu.xml.dom;

import org.w3c.dom.DocumentType;
import org.w3c.dom.Node;
import org.w3c.dom.Text;

import org.xml.sax.Attributes;
import org.xml.sax.SAXException;
import org.xml.sax.ext.Attributes2;

import gnu.xml.pipeline.DomConsumer;
import gnu.xml.pipeline.EventConsumer;


/**
 * Event consumer which constructs DOM documents using the implementation
 * in this package, using SAX2 events.  This packages various backdoors
 * into this DOM implementation, as needed to address DOM requirements
 * that can't be met by strictly conforming implementations of DOM.
 *
 * <p> These requirements all relate to {@link DocumentType} nodes and
 * features of that node type.  These features are normally not used,
 * because that interface only exposes a subset of the information found
 * in DTDs.  More, that subset does not include the most important typing
 * information.  For example, it excludes element content models and
 * attribute typing.  It does expose some entity management issues,
 * although entity management doesn't relate to document typing.
 *
 * <p> Note that SAX2 does not expose the literal text of the DTD's
 * internal subset, so it will not be present in DOM trees constructed
 * using this API.  (Though with a good SAX2 implementation, it could
 * be partially recreated...)
 *
 * @author David Brownell
 */
public class Consumer extends DomConsumer
{
    /**
     * Constructs an unconfigured event consumer,
     * as a terminus in a SAX event pipeline.
     */
    // used by PipelineFactory [terminus]
    public Consumer ()
    throws SAXException
    {
	super (DomDocument.class);
	setHandler (new Backdoor (this));
    }

    /**
     * Constructs an unconfigured event consumer,
     * as a stage in a SAX event pipeline.
     */
    // used by PipelineFactory [filter]
    public Consumer (EventConsumer next)
    throws SAXException
    {
	super (DomDocument.class, next);
	setHandler (new Backdoor (this));
    }

    /**
     * Implements the backdoors needed by DOM.
     * All methods in this class use implementation-specific APIs that are
     * implied by the DOM specification (needed to implement testable
     * behavior) but which are excluded from the DOM specification.
     */
    public static class Backdoor extends DomConsumer.Handler
    {
	/**
	 * Constructor.
	 * @param consumer must have been initialized to use the
	 *	{@link DomDocument} class (or a subclass) for
	 *	constructing DOM trees
	 */
	protected Backdoor (DomConsumer consumer)
	throws SAXException
	    { super (consumer); }

	// helper routine
	private DomDoctype getDoctype ()
	throws SAXException
	{
	    DomDocument		doc = (DomDocument) getDocument ();
	    DocumentType	dt = doc.getDoctype ();

	    if (dt == null)
		throw new SAXException ("doctype missing!");
	    return (DomDoctype) dt;
	}

	// SAX2 "lexical" event
	public void startDTD (String name, String publicId, String systemId)
	throws SAXException
	{
	    DomDocument		doc = (DomDocument) getDocument ();

	    super.startDTD (name, publicId, systemId);
	    // DOM L2 doctype creation model is bizarre
	    DomDoctype dt = new DomDoctype (doc, name, publicId, systemId);
	    doc.appendChild (dt);
	}

	// SAX2 "lexical" event
	public void endDTD ()
	throws SAXException
	{
	    super.endDTD ();
	    // DOM L2 has no way to make things readonly
	    getDoctype ().makeReadonly ();
	}

	// SAX1 DTD event
	public void notationDecl (
	    String name,
	    String publicId, String systemId
	) throws SAXException
	{
	    // DOM L2 can't create/save notation nodes
	    getDoctype ().declareNotation (name, publicId, systemId);
	}

	// SAX1 DTD event
	public void unparsedEntityDecl (
	    String name,
	    String publicId, String systemId,
	    String notationName
	) throws SAXException
	{
	    // DOM L2 can't create/save entity nodes
	    getDoctype ().declareEntity (name, publicId, systemId,
	    	notationName);
	}

	// SAX2 declaration event
	public void internalEntityDecl (String name, String value)
	throws SAXException
	{
	    // DOM L2 can't create/save entity nodes
	    // NOTE:  this doesn't save the value as a child of this
	    // node, though it could realistically do so.
	    getDoctype ().declareEntity (name, null, null, null);
	}

	// SAX2 declaration event
	public void externalEntityDecl (
	    String name,
	    String publicId,
	    String systemId
	) throws SAXException
	{
	    // DOM L2 can't create/save entity nodes
	    // NOTE:  DOM allows for these to have children, if
	    // they don't have unbound namespace references.
	    getDoctype ().declareEntity (name, publicId, systemId, null);
	}

	// SAX2 element
	public void startElement (
	    String uri,
	    String localName,
	    String qName,
	    Attributes atts
	) throws SAXException
	{
	    Node		top;

	    super.startElement (uri, localName, qName, atts);

	    // might there be more work?
	    top = getTop ();
	    if (!top.hasAttributes () || !(atts instanceof Attributes2))
		return;

	    // remember any attributes that got defaulted
	    DomNamedNodeMap	map = (DomNamedNodeMap) top.getAttributes ();
	    Attributes2		attrs = (Attributes2) atts;
	    int			length = atts.getLength ();

	    //map.compact ();
	    for (int i = 0; i < length; i++) {
		if (attrs.isSpecified (i))
		    continue;

		// value was defaulted.
		String		temp = attrs.getQName (i);
		DomAttr		attr;

		if ("".equals (temp))
		    attr = (DomAttr) map.getNamedItemNS (attrs.getURI (i),
			    atts.getLocalName (i));
		else
		    attr = (DomAttr) map.getNamedItem (temp);

		// DOM L2 can't write this flag, only read it
		attr.setSpecified (false);
	    }
	}

	public void endElement (
	    String uri,
	    String localName,
	    String qName
	) throws SAXException
	{
	    DomNode	top = (DomNode) getTop ();
	    top.compact ();
	    super.endElement (uri, localName, qName);
	}

	protected Text createText (
	    boolean	isCDATA,
	    char	buf [],
	    int		off,
	    int		len
	) {
	    DomDocument	doc = (DomDocument) getDocument ();

	    if (isCDATA)
		return doc.createCDATASection (buf, off, len);
	    else
		return doc.createTextNode (buf, off, len);
	}

        public void elementDecl(String name, String model)
          throws SAXException
        {
          getDoctype().elementDecl(name, model);
        }

	public void attributeDecl (
	    String	ename,
	    String	aname,
	    String	type,
	    String	mode,
	    String	value
	) throws SAXException
	{
          getDoctype().attributeDecl(ename, aname, type, mode, value);
            /*
	    if (value == null && !"ID".equals (type))
		return;
	    
	    DomDoctype.ElementInfo	info;

	    info = getDoctype ().getElementInfo (ename);
	    if (value != null)
		info.setAttrDefault (aname, value);
	    if ("ID".equals (type))
		info.setIdAttr (aname);
                */
            
	}

	// force duplicate name checking off while we're
	// using parser output (don't duplicate the work)
	public void startDocument () throws SAXException
	{
	    super.startDocument ();
	    
            DomDocument doc = (DomDocument) getDocument ();
            doc.setStrictErrorChecking(false);
            doc.setBuilding(true);
	}

        /**
         * Required by DOM Level 3 to report document parameters
         */
        public void xmlDecl(String version,
                            String encoding,
                            boolean standalone,
                            String inputEncoding)
          throws SAXException
        {
          super.xmlDecl(version, encoding, standalone, inputEncoding);

          DomDocument doc = (DomDocument) getDocument();
          doc.setXmlEncoding(encoding);
          doc.setInputEncoding(inputEncoding);
        }

	public void endDocument ()
	throws SAXException
	{
	    DomDocument doc = (DomDocument) getDocument ();
	    doc.setStrictErrorChecking(true);
            doc.setBuilding(false);
	    doc.compact ();
            DomDoctype doctype = (DomDoctype) doc.getDoctype();
            if (doctype != null)
              {
                doctype.makeReadonly();
              }
	    super.endDocument ();
	}

	// these three methods collaborate to populate entity
	// refs, marking contents readonly on end-of-entity

	public boolean canPopulateEntityRefs ()
	    { return true; }

	public void startEntity (String name)
	throws SAXException
	{
	    if (name.charAt (0) == '%' || "[dtd]".equals (name))
		return;
	    super.startEntity (name);

	    DomNode	top = (DomNode) getTop ();

	    if (top.getNodeType () == Node.ENTITY_REFERENCE_NODE)
		top.readonly = false;
	}

	public void endEntity (String name)
	throws SAXException
	{
	    if (name.charAt (0) == '%' || "[dtd]".equals (name))
		return;
	    DomNode	top = (DomNode) getTop ();

	    if (top.getNodeType () == Node.ENTITY_REFERENCE_NODE) {
		top.compact ();
		top.makeReadonly ();
	    }
	    super.endEntity (name);
	}
    }
}
