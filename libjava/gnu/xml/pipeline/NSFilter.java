/* NSFilter.java -- 
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

package gnu.xml.pipeline;

import java.util.Enumeration;
import java.util.Stack;

import org.xml.sax.Attributes;
import org.xml.sax.ErrorHandler;
import org.xml.sax.Locator;
import org.xml.sax.SAXException;
import org.xml.sax.SAXParseException;
import org.xml.sax.helpers.AttributesImpl;
import org.xml.sax.helpers.NamespaceSupport;

/**
 * This filter ensures that element and attribute names are properly prefixed,
 * and that such prefixes are declared.  Such data is critical for operations
 * like writing XML text, and validating against DTDs:  names or their prefixes
 * may have been discarded, although they are essential to the exchange of
 * information using XML.  There are various common ways that such data
 * gets discarded: <ul>
 *
 *	<li> By default, SAX2 parsers must discard the "xmlns*"
 *	attributes, and may also choose not to report properly prefixed
 *	names for elements or attributes.  (Some parsers may support
 *	changing the <em>namespace-prefixes</em> value from the default
 *	to <em>true</em>, effectively eliminating the need to use this
 *	filter on their output.)
 *
 *	<li> When event streams are generated from a DOM tree, they may
 *	have never have had prefixes or declarations for namespaces; or
 *	the existing prefixes or declarations may have been invalidated
 *	by structural modifications to that DOM tree.
 *
 *	<li> Other software writing SAX event streams won't necessarily
 *	be worrying about prefix management, and so they will need to
 *	have a transparent solution for managing them.
 *
 *	</ul>
 *
 * <p> This filter uses a heuristic to choose the prefix to assign to any
 * particular name which wasn't already corectly prefixed.  The associated
 * namespace will be correct, and the prefix will be declared.  Original
 * structures facilitating text editing, such as conventions about use of
 * mnemonic prefix names or the scoping of prefixes, can't always be
 * reconstructed after they are discarded, as strongly encouraged by the
 * current SAX2 defaults.
 *
 * <p> Note that this can't possibly know whether values inside attribute
 * value or document content involve prefixed names.  If your application
 * requires using prefixed names in such locations you'll need to add some
 * appropriate logic (perhaps adding additional heuristics in a subclass).
 *
 * @author David Brownell
 */
public class NSFilter extends EventFilter
{
    private NamespaceSupport	nsStack = new NamespaceSupport ();
    private Stack		elementStack = new Stack ();

    private boolean		pushedContext;
    private String		nsTemp [] = new String [3];
    private AttributesImpl	attributes = new AttributesImpl ();
    private boolean		usedDefault;

    // gensymmed prefixes use this root name
    private static final String	prefixRoot = "prefix-";

    
    /**
     * Passes events through to the specified consumer, after first
     * processing them.
     *
     * @param next the next event consumer to receive events.
     */
	// constructor used by PipelineFactory
    public NSFilter (EventConsumer next)
    {
	super (next);

	setContentHandler (this);
    }

    private void fatalError (String message)
    throws SAXException
    {
	SAXParseException	e;
	ErrorHandler		handler = getErrorHandler ();
	Locator			locator = getDocumentLocator ();

	if (locator == null)
	    e = new SAXParseException (message, null, null, -1, -1);
	else
	    e = new SAXParseException (message, locator);
	if (handler != null)
	    handler.fatalError (e);
	throw e;
    }


    public void startDocument () throws SAXException
    {
	elementStack.removeAllElements ();
	nsStack.reset ();
	pushedContext = false;
	super.startDocument ();
    }

    /**
     * This call is not passed to the next consumer in the chain.
     * Prefix declarations and scopes are only exposed in the form
     * of attributes; this callback just records a declaration that
     * will be exposed as an attribute.
     */
    public void startPrefixMapping (String prefix, String uri)
    throws SAXException
    {
	if (pushedContext == false) {
	    nsStack.pushContext ();
	    pushedContext = true;
	}

	// this check is awkward, but the paranoia prevents big trouble
	for (Enumeration e = nsStack.getDeclaredPrefixes ();
		e.hasMoreElements ();
		/* NOP */ ) {
	    String	declared = (String) e.nextElement ();

	    if (!declared.equals (prefix))
		continue;
	    if (uri.equals (nsStack.getURI (prefix)))
		return;
	    fatalError ("inconsistent binding for prefix '" + prefix
		+ "' ... " + uri + " (was " + nsStack.getURI (prefix) + ")");
	}

	if (!nsStack.declarePrefix (prefix, uri))
	    fatalError ("illegal prefix declared: " + prefix);
    }

    private String fixName (String ns, String l, String name, boolean isAttr)
    throws SAXException
    {
	if ("".equals (name) || name == null) {
	    name = l;
	    if ("".equals (name) || name == null)
		fatalError ("empty/null name");
	}

	// can we correctly process the name as-is?
	// handles "element scope" attribute names here.
	if (nsStack.processName (name, nsTemp, isAttr) != null
		&& nsTemp [0].equals (ns)
		) {
	    return nsTemp [2];
	}

	// nope, gotta modify the name or declare a default mapping
	int	temp;

	// get rid of any current prefix
	if ((temp = name.indexOf (':')) >= 0) {
	    name = name.substring (temp + 1);

	    // ... maybe that's enough (use/prefer default namespace) ...
	    if (!isAttr && nsStack.processName (name, nsTemp, false) != null
		    && nsTemp [0].equals (ns)
		    ) {
		return nsTemp [2];
	    }
	}

	// must we define and use the default/undefined prefix?
	if ("".equals (ns)) {
	    if (isAttr)
		fatalError ("processName bug");
	    if (attributes.getIndex ("xmlns") != -1)
		fatalError ("need to undefine default NS, but it's bound: "
			+ attributes.getValue ("xmlns"));
	    
	    nsStack.declarePrefix ("", "");
	    attributes.addAttribute ("", "", "xmlns", "CDATA", "");
	    return name;
	}

	// is there at least one non-null prefix we can use?
	for (Enumeration e = nsStack.getDeclaredPrefixes ();
		e.hasMoreElements ();
		/* NOP */) {
	    String prefix = (String) e.nextElement ();
	    String uri = nsStack.getURI (prefix);

	    if (uri == null || !uri.equals (ns))
		continue;
	    return prefix + ":" + name;
	}

	// no such luck.  create a prefix name, declare it, use it.
	for (temp = 0; temp >= 0; temp++) {
	    String	prefix = prefixRoot + temp;

	    if (nsStack.getURI (prefix) == null) {
		nsStack.declarePrefix (prefix, ns);
		attributes.addAttribute ("", "", "xmlns:" + prefix,
			"CDATA", ns);
		return prefix + ":" + name;
	    }
	}
	fatalError ("too many prefixes genned");
	// NOTREACHED
	return null;
    }

    public void startElement (
	String uri, String localName,
	String qName, Attributes atts
    ) throws SAXException
    {
	if (!pushedContext)
	    nsStack.pushContext ();
	pushedContext = false;

	// make sure we have all NS declarations handy before we start
	int	length = atts.getLength ();

	for (int i = 0; i < length; i++) {
	    String	aName = atts.getQName (i);

	    if (!aName.startsWith ("xmlns"))
		continue;

	    String	prefix;

	    if ("xmlns".equals (aName))
		prefix = "";
	    else if (aName.indexOf (':') == 5)
		prefix = aName.substring (6);
	    else	// "xmlnsfoo" etc.
		continue;
	    startPrefixMapping (prefix, atts.getValue (i));
	}

	// put namespace decls at the start of our regenned attlist
	attributes.clear ();
	for (Enumeration e = nsStack.getDeclaredPrefixes ();
		e.hasMoreElements ();
		/* NOP */) {
	    String prefix = (String) e.nextElement ();

	    attributes.addAttribute ("", "",
		    ("".equals (prefix)
			? "xmlns"
			: "xmlns:" + prefix),
		    "CDATA",
		    nsStack.getURI (prefix));
	}

	// name fixups:  element, then attributes.
	// fixName may declare a new prefix or, for the element,
	// redeclare the default (if element name needs it).
	qName = fixName (uri, localName, qName, false);

	for (int i = 0; i < length; i++) {
	    String	aName = atts.getQName (i);
	    String	aNS = atts.getURI (i);
	    String	aLocal = atts.getLocalName (i);
	    String	aType = atts.getType (i);
	    String	aValue = atts.getValue (i);

	    if (aName.startsWith ("xmlns"))
		continue;
	    aName = fixName (aNS, aLocal, aName, true);
	    attributes.addAttribute (aNS, aLocal, aName, aType, aValue);
	}

	elementStack.push (qName);

	// pass event along, with cleaned-up names and decls.
	super.startElement (uri, localName, qName, attributes);
    }

    public void endElement (String uri, String localName, String qName)
    throws SAXException
    {
	nsStack.popContext ();
	qName = (String) elementStack.pop ();
	super.endElement (uri, localName, qName);
    }

    /**
     * This call is not passed to the next consumer in the chain.
     * Prefix declarations and scopes are only exposed in their
     * attribute form.
     */
    public void endPrefixMapping (String prefix)
    throws SAXException
	{ }

    public void endDocument () throws SAXException
    {
	elementStack.removeAllElements ();
	nsStack.reset ();
	super.endDocument ();
    }
}
