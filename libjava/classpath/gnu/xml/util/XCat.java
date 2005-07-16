/* XCat.java -- 
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


package gnu.xml.util;

import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.net.URL;
import java.util.Enumeration;
import java.util.Hashtable;
import java.util.StringTokenizer;
import java.util.Stack;
import java.util.Vector;

import org.xml.sax.Attributes;
import org.xml.sax.ErrorHandler;
import org.xml.sax.InputSource;
import org.xml.sax.Locator;
import org.xml.sax.SAXException;
import org.xml.sax.SAXNotRecognizedException;
import org.xml.sax.SAXParseException;
import org.xml.sax.XMLReader;

import org.xml.sax.ext.DefaultHandler2;
import org.xml.sax.ext.EntityResolver2;

import org.xml.sax.helpers.XMLReaderFactory;

/**
 * Packages <a href=
    "http://www.oasis-open.org/committees/entity/spec-2001-08-06.html"
    >OASIS XML Catalogs</a>,
 * primarily for entity resolution by parsers.
 * That specification defines an XML syntax for mappings between
 * identifiers declared in DTDs (particularly PUBLIC identifiers) and
 * locations.  SAX has always supported such mappings, but conventions for
 * an XML file syntax to maintain them have previously been lacking.
 *
 * <p> This has three main operational modes.  The primary intended mode is
 * to create a resolver, then preloading it with one or more site-standard
 * catalogs before using it with one or more SAX parsers: <pre>
 *	XCat	catalog = new XCat ();
 *	catalog.setErrorHandler (diagnosticErrorHandler);
 *	catalog.loadCatalog ("file:/local/catalogs/catalog.cat");
 *	catalog.loadCatalog ("http://shared/catalog.cat");
 *	...
 *	catalog.disableLoading ();
 *	parser1.setEntityResolver (catalog);
 *	parser2.setEntityResolver (catalog);
 *	...</pre>
 *
 * <p>A second mode is to arrange that your application uses instances of
 * this class as its entity resolver, and automatically loads catalogs
 * referenced by <em>&lt;?oasis-xml-catalog...?&gt;</em> processing
 * instructions found before the DTD in documents it parses.
 * It would then discard the resolver after each parse.
 *
 * <p> A third mode applies catalogs in contexts other than entity
 * resolution for parsers.
 * The {@link #resolveURI resolveURI()} method supports resolving URIs
 * stored in XML application data, rather than inside DTDs.
 * Catalogs would be loaded as shown above, and the catalog could
 * be used concurrently for parser entity resolution and for
 * application URI resolution.
 * </p>
 *
 * <center><hr width='70%'></center>
 *
 * <p>Errors in catalogs implicitly loaded (during resolution) are ignored
 * beyond being reported through any <em>ErrorHandler</em> assigned using
 * {@link #setErrorHandler setErrorHandler()}.  SAX exceptions
 * thrown from such a handler won't abort resolution, although throwing a
 * <em>RuntimeException</em> or <em>Error</em> will normally abort both
 * resolution and parsing.  Useful diagnostic information is available to
 * any <em>ErrorHandler</em> used to report problems, or from any exception
 * thrown from an explicit {@link #loadCatalog loadCatalog()} invocation.
 * Applications can use that information as troubleshooting aids.
 *
 * <p>While this class requires <em>SAX2 Extensions 1.1</em> classes in
 * its class path, basic functionality does not require using a SAX2
 * parser that supports the extended entity resolution functionality.
 * See the original SAX1
 * {@link #resolveEntity(java.lang.String,java.lang.String) resolveEntity()}
 * method for a list of restrictions which apply when it is used with
 * older SAX parsers.
 *
 * @see EntityResolver2
 *
 * @author David Brownell
 */
public class XCat implements EntityResolver2
{
    private Catalog		catalogs [];
    private boolean		usingPublic = true;
    private boolean		loadingPermitted = true;
    private boolean		unified = true;
    private String		parserClass;
    private ErrorHandler	errorHandler;

    // private EntityResolver	next;	// chain to next if we fail...

    //
    // NOTE:  This is a straightforward implementation, and if
    // there are lots of "nextCatalog" or "delegate*" entries
    // in use, two tweaks would be worth considering:
    //
    //	- Centralize some sort of cache (key by URI) for individual
    //	  resolvers.  That'd avoid multiple copies of a given catalog.
    //
    //	- Have resolution track what catalogs (+modes) have been
    //	  searched.  This would support loop detection.
    //


    /**
     * Initializes without preloading a catalog.
     * This API is convenient when you may want to arrange that catalogs
     * are automatically loaded when explicitly referenced in documents,
     * using the <em>oasis-xml-catalog</em> processing instruction.
     * In such cases you won't usually be able to preload catalogs.
     */
    public XCat () { }

    /**
     * Initializes, and preloads a catalog using the default SAX parser.
     * This API is convenient when you operate with one or more standard
     * catalogs.
     *
     * <p> This just delegates to {@link #loadCatalog loadCatalog()};
     * see it for exception information.
     *
     * @param uri absolute URI for the catalog file.
     */
    public XCat (String uri)
    throws SAXException, IOException
	{ loadCatalog (uri); }


    /**
     * Loads an OASIS XML Catalog.
     * It is appended to the list of currently active catalogs, or
     * reloaded if a catalog with the same URI was already loaded.
     * Callers have control over what parser is used, how catalog parsing
     * errors are reported, and whether URIs will be resolved consistently.
     *
     * <p> The OASIS specification says that errors detected when loading
     * catalogs "must recover by ignoring the catalog entry file that
     * failed, and proceeding."  In this API, that action can be the
     * responsibility of applications, when they explicitly load any
     * catalog using this method.
     *
     * <p>Note that catalogs referenced by this one will not be loaded
     * at this time.  Catalogs referenced through <em>nextCatalog</em>
     * or <em>delegate*</em> elements are normally loaded only if needed. 
     *
     * @see #setErrorHandler
     * @see #setParserClass
     * @see #setUnified
     *
     * @param uri absolute URI for the catalog file.
     *
     * @exception IOException As thrown by the parser, typically to
     *	indicate problems reading data from that URI.
     * @exception SAXException As thrown by the parser, typically to
     *	indicate problems parsing data from that URI.  It may also
     *  be thrown if the parser doesn't support necessary handlers. 
     * @exception IllegalStateException When attempting to load a
     *	catalog after loading has been {@link #disableLoading disabled},
     *	such as after any entity or URI lookup has been performed.
     */
    public synchronized void loadCatalog (String uri)
    throws SAXException, IOException
    {
	Catalog		catalog;
	int		index = -1;

	if (!loadingPermitted)
	    throw new IllegalStateException ();
	
	uri = normalizeURI (uri);
	if (catalogs != null) {
	    // maybe just reload
	    for (index = 0; index < catalogs.length; index++)
		if (uri.equals (catalogs [index].catalogURI))
		    break;
	}
	catalog = loadCatalog (parserClass, errorHandler, uri, unified);

	// add to list of catalogs
	if (catalogs == null) {
	    index = 0;
	    catalogs = new Catalog [1];
	} else if (index == catalogs.length) {
	    Catalog		tmp [];

	    tmp = new Catalog [index + 1];
	    System.arraycopy (catalogs, 0, tmp, 0, index);
	    catalogs = tmp;
	}
	catalogs [index] = catalog;
    }


    /**
     * "New Style" external entity resolution for parsers.
     * Calls to this method prevent explicit loading of additional catalogs
     * using {@link #loadCatalog loadCatalog()}.
     *
     * <p>This supports the full core catalog functionality for locating
     * (and relocating) parsed entities that have been declared in a
     * document's DTD.
     *
     * @param name Entity name, such as "dudley", "%nell", or "[dtd]".
     * @param publicId Either a normalized public ID, or null.
     * @param baseURI Absolute base URI associated with systemId.
     * @param systemId URI found in entity declaration (may be
     *	relative to baseURI).
     *
     * @return Input source for accessing the external entity, or null
     *	if no mapping was found.  The input source may have opened
     *	the stream, and will have a fully resolved URI.
     *
     * @see #getExternalSubset
     */
    public InputSource resolveEntity (
	String name,		// UNUSED ... systemId is always non-null
	String publicId,
	String baseURI,		// UNUSED ... it just lets sysId be relative
	String systemId
    ) throws SAXException, IOException
    {
	if (loadingPermitted)
	    disableLoading ();

	try {
	    // steps as found in OASIS XML catalog spec 7.1.2
	    // steps 1, 8 involve looping over the list of catalogs
	    for (int i = 0; i < catalogs.length; i++) {
		InputSource	retval;
		retval = catalogs [i].resolve (usingPublic, publicId, systemId);
		if (retval != null)
		    return retval;;
	    }
	} catch (DoneDelegation x) {
	    // done!
	}
	// step 9 involves returning "no match" 
	return null;
    }


    /**
     * "New Style" parser callback to add an external subset.
     * For documents that don't include an external subset, this may
     * return one according to <em>doctype</em> catalog entries.
     * (This functionality is not a core part of the OASIS XML Catalog
     * specification, though it's presented in an appendix.)
     * If no such entry is defined, this returns null to indicate that
     * this document will not be modified to include such a subset.
     * Calls to this method prevent explicit loading of additional catalogs
     * using {@link #loadCatalog loadCatalog()}.
     *
     * <p><em>Warning:</em> That catalog functionality can be dangerous.
     * It can provide definitions of general entities, and thereby mask
     * certain well formedess errors.
     *
     * @param name Name of the document element, either as declared in
     *	a DOCTYPE declaration or as observed in the text.
     * @param baseURI Document's base URI (absolute).
     *
     * @return Input source for accessing the external subset, or null
     *	if no mapping was found.  The input source may have opened
     *	the stream, and will have a fully resolved URI.
     */
    public InputSource getExternalSubset (String name, String baseURI)
    throws SAXException, IOException
    {
	if (loadingPermitted)
	    disableLoading ();
	try {
	    for (int i = 0; i < catalogs.length; i++) {
		InputSource retval = catalogs [i].getExternalSubset (name);
		if (retval != null)
		    return retval;
	    }
	} catch (DoneDelegation x) {
	    // done!
	}
	return null;
    }


    /**
     * "Old Style" external entity resolution for parsers.
     * This API provides only core functionality.
     * Calls to this method prevent explicit loading of additional catalogs
     * using {@link #loadCatalog loadCatalog()}.
     *
     * <p>The functional limitations of this interface include:</p><ul>
     *
     *	<li>Since system IDs will be absolutized before the resolver
     *	sees them, matching against relative URIs won't work.
     *	This may affect <em>system</em>, <em>rewriteSystem</em>,
     *	and <em>delegateSystem</em> catalog entries.
     *
     *	<li>Because of that absolutization, documents declaring entities
     *	with system IDs using URI schemes that the JVM does not recognize
     *	may be unparsable.  URI schemes such as <em>file:/</em>,
     *	<em>http://</em>, <em>https://</em>, and <em>ftp://</em>
     *	will usually work reliably.
     *
     *	<li>Because missing external subsets can't be provided, the
     *	<em>doctype</em> catalog entries will be ignored.
     *	(The {@link #getExternalSubset getExternalSubset()} method is
     *	a "New Style" resolution option.)
     *
     *	</ul>
     *
     * <p>Applications can tell whether this limited functionality will be
     * used: if the feature flag associated with the {@link EntityResolver2}
     * interface is not <em>true</em>, the limitations apply.  Applications
     * can't usually know whether a given document and catalog will trigger
     * those limitations.  The issue can only be bypassed by operational
     * procedures such as not using catalogs or documents which involve
     * those features.
     *
     * @param publicId Either a normalized public ID, or null
     * @param systemId Always an absolute URI.
     *
     * @return Input source for accessing the external entity, or null
     *	if no mapping was found.  The input source may have opened
     *	the stream, and will have a fully resolved URI.
     */
    final public InputSource resolveEntity (String publicId, String systemId)
    throws SAXException, IOException
    {
	return resolveEntity (null, publicId, null, systemId);
    }


    /**
     * Resolves a URI reference that's not defined to the DTD.
     * This is intended for use with URIs found in document text, such as
     * <em>xml-stylesheet</em> processing instructions and in attribute
     * values, where they are not recognized as URIs by XML parsers.
     * Calls to this method prevent explicit loading of additional catalogs
     * using {@link #loadCatalog loadCatalog()}.
     *
     * <p>This functionality is supported by the OASIS XML Catalog
     * specification, but will never be invoked by an XML parser.
     * It corresponds closely to functionality for mapping system
     * identifiers for entities declared in DTDs; closely enough that
     * this implementation's default behavior is that they be
     * identical, to minimize potential confusion.
     *
     * <p>This method could be useful when implementing the
     * {@link javax.xml.transform.URIResolver} interface, wrapping the
     * input source in a {@link javax.xml.transform.sax.SAXSource}.
     *
     * @see #isUnified
     * @see #setUnified
     *
     * @param baseURI The relevant base URI as specified by the XML Base
     *	specification.  This recognizes <em>xml:base</em> attributes
     *	as overriding the actual (physical) base URI.
     * @param uri Either an absolute URI, or one relative to baseURI
     *
     * @return Input source for accessing the mapped URI, or null
     *	if no mapping was found.  The input source may have opened
     *	the stream, and will have a fully resolved URI.
     */
    public InputSource resolveURI (String baseURI, String uri)
    throws SAXException, IOException
    {
	if (loadingPermitted)
	    disableLoading ();

	// NOTE:  baseURI isn't used here, but caller MUST have it,
	// and heuristics _might_ use it in the future ... plus,
	// it's symmetric with resolveEntity ().

	// steps 1, 6 involve looping
	try {
	    for (int i = 0; i < catalogs.length; i++) {
		InputSource	tmp = catalogs [i].resolveURI (uri);
		if (tmp != null)
		    return tmp;
	    }
	} catch (DoneDelegation x) {
	    // done
	}
	// step 7 reports no match
	return null;
    }


    /** 
     * Records that catalog loading is no longer permitted.
     * Loading is automatically disabled when lookups are performed,
     * and should be manually disabled when <em>startDTD()</em> (or
     * any other DTD declaration callback) is invoked, or at the latest
     * when the document root element is seen.
     */
    public synchronized void disableLoading ()
    {
	// NOTE:  this method and loadCatalog() are synchronized
	// so that it's impossible to load (top level) catalogs
	// after lookups start.  Likewise, deferred loading is also
	// synchronized (for "next" and delegated catalogs) to
	// ensure that parsers can share resolvers.
	loadingPermitted = false;
    }


    /**
     * Returns the error handler used to report catalog errors.
     * Null is returned if the parser's default error handling
     * will be used.
     *
     * @see #setErrorHandler
     */
    public ErrorHandler getErrorHandler ()
	{ return errorHandler; }

    /**
     * Assigns the error handler used to report catalog errors.
     * These errors may come either from the SAX2 parser or
     * from the catalog parsing code driven by the parser. 
     *
     * <p> If you're sharing the resolver between parsers, don't
     * change this once lookups have begun.
     *
     * @see #getErrorHandler
     *
     * @param parser The error handler, or null saying to use the default
     *	(no diagnostics, and only fatal errors terminate loading).
     */
    public void setErrorHandler (ErrorHandler handler)
	{ errorHandler = handler; }


    /**
     * Returns the name of the SAX2 parser class used to parse catalogs.
     * Null is returned if the system default is used.
     * @see #setParserClass
     */
    public String getParserClass ()
	{ return parserClass; }

    /**
     * Names the SAX2 parser class used to parse catalogs.
     *
     * <p> If you're sharing the resolver between parsers, don't change
     * this once lookups have begun.
     *
     * <p> Note that in order to properly support the <em>xml:base</em>
     * attribute and relative URI resolution, the SAX parser used to parse
     * the catalog must provide a {@link Locator} and support the optional
     * declaration and lexical handlers.
     *
     * @see #getParserClass
     *
     * @param parser The parser class name, or null saying to use the
     *	system default SAX2 parser.
     */
    public void setParserClass (String parser)
	{ parserClass = parser; }


    /**
     * Returns true (the default) if all methods resolve
     * a given URI in the same way.
     * Returns false if calls resolving URIs as entities (such as
     * {@link #resolveEntity resolveEntity()}) use different catalog entries
     * than those resolving them as URIs ({@link #resolveURI resolveURI()}),
     * which will generally produce different results.
     *
     * <p>The OASIS XML Catalog specification defines two related schemes
     * to map URIs "as URIs" or "as system IDs".
     * URIs use <em>uri</em>, <em>rewriteURI</em>, and <em>delegateURI</em>
     * elements.  System IDs do the same things with <em>systemId</em>,
     * <em>rewriteSystemId</em>, and <em>delegateSystemId</em>.
     * It's confusing and error prone to maintain two parallel copies of
     * such data.  Accordingly, this class makes that behavior optional.
     * The <em>unified</em> interpretation of URI mappings is preferred,
     * since it prevents surprises where one URI gets mapped to different
     * contents depending on whether the reference happens to have come
     * from a DTD (or not).
     *
     * @see #setUnified
     */
    public boolean isUnified ()
	{ return unified; }

    /**
     * Assigns the value of the flag returned by {@link #isUnified}.
     * Set it to false to be strictly conformant with the OASIS XML Catalog
     * specification.  Set it to true to make all mappings for a given URI
     * give the same result, regardless of the reason for the mapping.
     *
     * <p>Don't change this once you've loaded the first catalog.
     *
     * @param value new flag setting
     */
    public void setUnified (boolean value)
	{ unified = value; }


    /**
     * Returns true (the default) if a catalog's public identifier
     * mappings will be used.
     * When false is returned, such mappings are ignored except when
     * system IDs are discarded, such as for
     * entities using the <em>urn:publicid:</em> URI scheme in their
     * system identifiers.  (See RFC 3151 for information about that
     * URI scheme.  Using it in system identifiers may not work well
     * with many SAX parsers unless the <em>resolve-dtd-uris</em>
     * feature flag is set to false.)
     * @see #setUsingPublic
     */
    public boolean isUsingPublic ()
	{ return usingPublic; }

    /**
     * Specifies which catalog search mode is used.
     * By default, public identifier mappings are able to override system
     * identifiers when both are available.
     * Applications may choose to ignore public
     * identifier mappings in such cases, so that system identifiers
     * declared in DTDs will only be overridden by an explicit catalog
     * match for that system ID.
     *
     * <p> If you're sharing the resolver between parsers, don't
     * change this once lookups have begun.
     * @see #isUsingPublic
     *
     * @param value true to always use public identifier mappings,
     *	false to only use them for system ids using the <em>urn:publicid:</em>
     *	URI scheme.
     */
    public void setUsingPublic (boolean value)
	{ usingPublic = value; }



    // hmm, what's this do? :)
    private static Catalog loadCatalog (
	String		parserClass,
	ErrorHandler	eh,
	String		uri,
	boolean		unified
    ) throws SAXException, IOException
    {
	XMLReader	parser;
	Loader		loader;
	boolean		doesIntern = false;

	if (parserClass == null)
	    parser = XMLReaderFactory.createXMLReader ();
	else
	    parser = XMLReaderFactory.createXMLReader (parserClass);
	if (eh != null)
	    parser.setErrorHandler (eh);
	// resolve-dtd-entities is at default value (unrecognized == true)

	try {
	    doesIntern = parser.getFeature (
		"http://xml.org/sax/features/string-interning");
	} catch (SAXNotRecognizedException e) { }

	loader = new Loader (doesIntern, eh, unified);
	loader.cat.parserClass = parserClass;
	loader.cat.catalogURI = uri;

	parser.setContentHandler (loader);
	parser.setProperty (
	    "http://xml.org/sax/properties/declaration-handler",
	    loader);
	parser.setProperty (
	    "http://xml.org/sax/properties/lexical-handler",
	    loader);
	parser.parse (uri);

	return loader.cat;
    }

    // perform one or both the normalizations for public ids
    private static String normalizePublicId (boolean full, String publicId)
    {
	if (publicId.startsWith ("urn:publicid:")) {
	    StringBuffer	buf = new StringBuffer ();
	    char		chars [] = publicId.toCharArray ();
boolean hasbug = false;

	    for (int i = 13; i < chars.length; i++) {
		switch (chars [i]) {
		case '+': 	buf.append (' '); continue;
		case ':': 	buf.append ("//"); continue;
		case ';': 	buf.append ("::"); continue;
		case '%':
// FIXME unhex that char!  meanwhile, warn and fallthrough ...
		    hasbug = true;
		default:	buf.append (chars [i]); continue;
		}
	    }
	    publicId = buf.toString ();
if (hasbug)
System.err.println ("nyet unhexing public id: " + publicId);
	    full = true;
	}

	// SAX parsers do everything except that URN mapping, but
	// we can't trust other sources to normalize correctly
	if (full) {
	    StringTokenizer	tokens;
	    String		token;

	    tokens = new StringTokenizer (publicId, " \r\n");
	    publicId = null;
	    while (tokens.hasMoreTokens ()) {
		if (publicId == null)
		    publicId = tokens.nextToken ();
		else
		    publicId += " " + tokens.nextToken ();
	    }
	}
	return publicId;
    }

    private static boolean isUriExcluded (int c)
	{ return c <= 0x20 || c >= 0x7f || "\"<>^`{|}".indexOf (c) != -1; }

    private static int hexNibble (int c)
    {
	if (c < 10)
	    return c + '0';
	return ('a' - 10) + c;
    }

    // handles URIs with "excluded" characters
    private static String normalizeURI (String systemId)
    {
	int			length = systemId.length ();

	for (int i = 0; i < length; i++) {
	    char	c = systemId.charAt (i);

	    // escape non-ASCII plus "excluded" characters
	    if (isUriExcluded (c)) {
		byte			buf [];
		ByteArrayOutputStream	out;
		int				b;

		// a JVM that doesn't know UTF8 and 8859_1 is unusable!
		try {
		    buf = systemId.getBytes ("UTF8");
		    out = new ByteArrayOutputStream (buf.length + 10);

		    for (i = 0; i < buf.length; i++) {
			b = buf [i] & 0x0ff;
			if (isUriExcluded (b)) {
			    out.write ((int) '%');
			    out.write (hexNibble (b >> 4));
			    out.write (hexNibble (b & 0x0f));
			} else
			    out.write (b);
		    }
		    return out.toString ("8859_1");
		} catch (IOException e) {
		    throw new RuntimeException (
			"can't normalize URI: " + e.getMessage ());
		}
	    }
	}
	return systemId;
    }

    // thrown to mark authoritative end of a search
    private static class DoneDelegation extends SAXException
    {
	DoneDelegation () { }
    }


    /**
     * Represents a OASIS XML Catalog, and encapsulates much of
     * the catalog functionality.
     */
    private static class Catalog
    {
	// loading infrastructure
	String		catalogURI;
	ErrorHandler	eh;
	boolean		unified;
	String		parserClass;

	// catalog data
	boolean		hasPreference;
	boolean		usingPublic;

	Hashtable	publicIds;
	Hashtable	publicDelegations;

	Hashtable	systemIds;
	Hashtable	systemRewrites;
	Hashtable	systemDelegations;

	Hashtable	uris;
	Hashtable	uriRewrites;
	Hashtable	uriDelegations;

	Hashtable	doctypes;

	Vector		next;

	// nonpublic!
	Catalog () { }

	
	// steps as found in OASIS XML catalog spec 7.1.2
	private InputSource locatePublicId (String publicId)
	throws SAXException, IOException
	{
	    // 5. return (first) 'public' entry
	    if (publicIds != null) {
		String	retval = (String) publicIds.get (publicId);
		if (retval != null) {
		    // IF the URI is accessible ...
		    return new InputSource (retval);
		}
	    }

	    // 6. return delegatePublic catalog match [complex]
	    if (publicDelegations != null)
		return checkDelegations (publicDelegations, publicId,
				publicId, null);
	    
	    return null;
	}

	// steps as found in OASIS XML catalog spec 7.1.2 or 7.2.2
	private InputSource mapURI (
	    String	uri,
	    Hashtable	ids,
	    Hashtable	rewrites,
	    Hashtable	delegations
	) throws SAXException, IOException
	{
	    // 7.1.2: 2. return (first) 'system' entry
	    // 7.2.2: 2. return (first) 'uri' entry
	    if (ids != null) {
		String	retval = (String) ids.get (uri);
		if (retval != null) {
		    // IF the URI is accessible ...
		    return new InputSource (retval);
		}
	    }

	    // 7.1.2: 3. return 'rewriteSystem' entries
	    // 7.2.2: 3. return 'rewriteURI' entries
	    if (rewrites != null) {
		String	prefix = null;
		String	replace = null;
		int	prefixLen = -1;

		for (Enumeration e = rewrites.keys ();
			e.hasMoreElements ();
			/* NOP */) {
		    String	temp = (String) e.nextElement ();
		    int		len = -1;

		    if (!uri.startsWith (temp))
			continue;
		    if (prefix != null
			    && (len = temp.length ()) < prefixLen)
			continue;
		    prefix = temp;
		    prefixLen = len;
		    replace = (String) rewrites.get (temp);
		}
		if (prefix != null) {
		    StringBuffer	buf = new StringBuffer (replace);
		    buf.append (uri.substring (prefixLen));
		    // IF the URI is accessible ...
		    return new InputSource (buf.toString ());
		}
	    }

	    // 7.1.2: 4. return 'delegateSystem' catalog match [complex]
	    // 7.2.2: 4. return 'delegateURI' catalog match [complex]
	    if (delegations != null)
		return checkDelegations (delegations, uri, null, uri);

	    return null;
	}


	/**
	 * Returns a URI for an external entity.
	 */
	public InputSource resolve (
	    boolean	usingPublic,
	    String	publicId,
	    String	systemId
	) throws SAXException, IOException
	{
	    boolean	preferSystem;
	    InputSource	retval;

	    if (hasPreference)
		preferSystem = !this.usingPublic;
	    else
		preferSystem = !usingPublic;
	    
	    if (publicId != null)
		publicId = normalizePublicId (false, publicId);

	    // behavior here matches section 7.1.1 of the oasis spec
	    if (systemId != null) {
		if (systemId.startsWith ("urn:publicid:")) {
		    String	temp = normalizePublicId (true, systemId);
		    if (publicId == null) {
			publicId = temp;
			systemId = null;
		    } else if (!publicId.equals (temp)) {
			// error; ok to recover by:
			systemId = null;
		    }
		} else
		    systemId = normalizeURI (systemId);
	    }

	    if (systemId == null && publicId == null)
		return null;

	    if (systemId != null) {
		retval = mapURI (systemId, systemIds, systemRewrites,
					systemDelegations);
		if (retval != null) {
		    retval.setPublicId (publicId);
		    return retval;
		}
	    }

	    if (publicId != null
		    && !(systemId != null && preferSystem)) {
		retval = locatePublicId (publicId);
		if (retval != null) {
		    retval.setPublicId (publicId);
		    return retval;
		}
	    }

	    // 7. apply nextCatalog entries
	    if (next != null) {
		int	length = next.size ();
		for (int i = 0; i < length; i++) {
		    Catalog	n = getNext (i);
		    retval = n.resolve (usingPublic, publicId, systemId);
		    if (retval != null)
			return retval;
		}
	    }

	    return null;
	}

	/**
	 * Maps one URI into another, for resources that are not defined
	 * using XML external entity or notation syntax.
	 */
	public InputSource resolveURI (String uri)
	throws SAXException, IOException
	{
	    if (uri.startsWith ("urn:publicid:"))
		return resolve (true, normalizePublicId (true, uri), null);

	    InputSource	retval;

	    uri = normalizeURI (uri);

	    // 7.2.2 steps 2-4
	    retval = mapURI (uri, uris, uriRewrites, uriDelegations);
	    if (retval != null)
		return retval;

	    // 7.2.2 step 5. apply nextCatalog entries
	    if (next != null) {
		int	length = next.size ();
		for (int i = 0; i < length; i++) {
		    Catalog	n = getNext (i);
		    retval = n.resolveURI (uri);
		    if (retval != null)
			return retval;
		}
	    }

	    return null;
	}


	/**
	 * Finds the external subset associated with a given root element.
	 */
	public InputSource getExternalSubset (String name)
	throws SAXException, IOException
	{
	    if (doctypes != null) {
		String	value = (String) doctypes.get (name);
		if (value != null) {
		    // IF the URI is accessible ...
		    return new InputSource (value);
		}
	    }
	    if (next != null) {
		int	length = next.size ();
		for (int i = 0; i < length; i++) {
		    Catalog	n = getNext (i);
		    if (n == null)
			continue;
		    InputSource	retval = n.getExternalSubset (name);
		    if (retval != null)
			return retval;
		}
	    }
	    return null;
	}

	private synchronized Catalog getNext (int i)
	throws SAXException, IOException
	{
	    Object	obj;

	    if (next == null || i < 0 || i >= next.size ())
		return null;
	    obj = next.elementAt (i);
	    if (obj instanceof Catalog)
		return (Catalog) obj;
	    
	    // ok, we deferred reading that catalog till now.
	    // load and cache it.
	    Catalog	cat = null;

	    try {
		cat = loadCatalog (parserClass, eh, (String) obj, unified);
		next.setElementAt (cat, i);
	    } catch (SAXException e) {
		// must fail quietly, says the OASIS spec
	    } catch (IOException e) {
		// same applies here
	    }
	    return cat;
	}

	private InputSource checkDelegations (
	    Hashtable	delegations,
	    String	id,
	    String	publicId,	// only one of public/system
	    String	systemId	// will be non-null...
	) throws SAXException, IOException
	{
	    Vector	matches = null;
	    int		length = 0;

	    // first, see if any prefixes match.
	    for (Enumeration e = delegations.keys ();
		    e.hasMoreElements ();
		    /* NOP */) {
		String	prefix = (String) e.nextElement ();

		if (!id.startsWith (prefix))
		    continue;
		if (matches == null)
		    matches = new Vector ();
		
		// maintain in longer->shorter sorted order
		// NOTE:  assumes not many matches will fire!
		int	index;

		for (index = 0; index < length; index++) {
		    String	temp = (String) matches.elementAt (index);
		    if (prefix.length () > temp.length ()) {
			matches.insertElementAt (prefix, index);
			break;
		    }
		}
		if (index == length)
		    matches.addElement (prefix);
		length++;
	    }
	    if (matches == null)
		return null;

	    // now we know the list of catalogs to replace our "top level"
	    // list ... we use it here, rather than somehow going back and
	    // restarting, since this helps avoid reading most catalogs.
	    // this assumes stackspace won't be a problem.
	    for (int i = 0; i < length; i++) {
		Catalog		catalog = null;
		InputSource	result;

		// get this catalog.  we may not have read it yet.
		synchronized (delegations) {
		    Object	prefix = matches.elementAt (i);
		    Object	cat = delegations.get (prefix);

		    if (cat instanceof Catalog)
			catalog = (Catalog) cat;
		    else {
			try {
			    // load and cache that catalog
			    catalog = loadCatalog (parserClass, eh,
				    (String) cat, unified);
			    delegations.put (prefix, catalog);
			} catch (SAXException e) {
			    // must ignore, says the OASIS spec
			} catch (IOException e) {
			    // same applies here
			}
		    }
		}

		// ignore failed loads, and proceed
		if (catalog == null)
		    continue;
		
		// we have a catalog ... resolve!
		// usingPublic value can't matter, there's no choice
		result = catalog.resolve (true, publicId, systemId);
		if (result != null)
		    return result;
	    }

	    // if there were no successes, the entire
	    // lookup failed (all the way to top level)
	    throw new DoneDelegation ();
	}
    }


    /** This is the namespace URI used for OASIS XML Catalogs.  */
    private static final String	catalogNamespace =
    	"urn:oasis:names:tc:entity:xmlns:xml:catalog";


    /**
     * Loads/unmarshals one catalog.
     */
    private static class Loader extends DefaultHandler2
    {
	private boolean		preInterned;
	private ErrorHandler	handler;
	private boolean		unified;
	private int		ignoreDepth;
	private Locator		locator;
	private boolean		started;
	private Hashtable	externals;
	private Stack		bases;

	Catalog			cat = new Catalog ();


	/**
	 * Constructor.
	 * @param flag true iff the parser already interns strings.
	 * @param eh Errors and warnings are delegated to this.
	 * @param unified true keeps one table for URI mappings;
	 *	false matches OASIS spec, storing mappings
	 *	for URIs and SYSTEM ids in parallel tables.
	 */
	Loader (boolean flag, ErrorHandler eh, boolean unified)
	{
	    preInterned = flag;
	    handler = eh;
	    this.unified = unified;
	    cat.unified = unified;
	    cat.eh = eh;
	}


	// strips out fragments
	private String nofrag (String uri)
	throws SAXException
	{
	    if (uri.indexOf ('#') != -1) {
		warn ("URI with fragment: " + uri);
		uri = uri.substring (0, uri.indexOf ('#'));
	    }
	    return uri;
	}

	// absolutizes relative URIs
	private String absolutize (String uri)
	throws SAXException
	{
	    // avoid creating URLs if they're already absolutized,
	    // or if the URI is already using a known scheme
	    if (uri.startsWith ("file:/")
		    || uri.startsWith ("http:/")
		    || uri.startsWith ("https:/")
		    || uri.startsWith ("ftp:/")
		    || uri.startsWith ("urn:")
		    )
		return uri;

	    // otherwise, let's hope the JDK handles this URI scheme.
	    try {
		URL	base = (URL) bases.peek ();
		return new URL (base, uri).toString ();
	    } catch (Exception e) {
		fatal ("can't absolutize URI: " + uri);
		return null;
	    }
	}

	// recoverable error
	private void error (String message)
	throws SAXException
	{
	    if (handler == null)
		return;
	    handler.error (new SAXParseException (message, locator));
	}

	// nonrecoverable error
	private void fatal (String message)
	throws SAXException
	{
	    SAXParseException	spe;
	    
	    spe = new SAXParseException (message, locator);
	    if (handler != null)
		handler.fatalError (spe);
	    throw spe;
	}

	// low severity problem
	private void warn (String message)
	throws SAXException
	{
	    if (handler == null)
		return;
	    handler.warning (new SAXParseException (message, locator));
	}

	// callbacks:

	public void setDocumentLocator (Locator l)
	    { locator = l; }

	public void startDocument ()
	throws SAXException
	{
	    if (locator == null)
		error ("no locator!");
	    bases = new Stack ();
	    String	uri = locator.getSystemId ();
	    try {
		bases.push (new URL (uri));
	    } catch (IOException e) {
		fatal ("bad document base URI: " + uri);
	    }
	}

	public void endDocument ()
	throws SAXException
	{
	    try {
		if (!started)
		    error ("not a catalog!");
	    } finally {
		locator = null;
		handler = null;
		externals = null;
		bases = null;
	    }
	}

	// XML Base support for external entities.

	// NOTE: expects parser is in default "resolve-dtd-uris" mode.
	public void externalEntityDecl (String name, String pub, String sys)
	throws SAXException
	{
	    if (externals == null)
		externals = new Hashtable ();
	    if (externals.get (name) == null)
		externals.put (name, pub);
	}

	public void startEntity (String name)
	throws SAXException
	{
	    if (externals == null)
		return;
	    String uri = (String) externals.get (name);

	    // NOTE: breaks if an EntityResolver substitutes these URIs.
	    // If toplevel loader supports one, must intercept calls...
	    if (uri != null) {
		try {
		    bases.push (new URL (uri));
		} catch (IOException e) {
		    fatal ("entity '" + name + "', bad URI: " + uri);
		}
	    }
	}

	public void endEntity (String name)
	{
	    if (externals == null)
		return;
	    String value = (String) externals.get (name);

	    if (value != null)
		bases.pop ();
	}

	/**
	 * Processes catalog elements, saving their data.
	 */
	public void startElement (String namespace, String local,
	    String qName, Attributes atts)
	throws SAXException
	{
	    // must ignore non-catalog elements, and their contents
	    if (ignoreDepth != 0 || !catalogNamespace.equals (namespace)) {
		ignoreDepth++;
		return;
	    }

	    // basic sanity checks
	    if (!preInterned)
		local = local.intern ();
	    if (!started) {
		started = true;
		if ("catalog" != local)
		    fatal ("root element not 'catalog': " + local);
	    }

	    // Handle any xml:base attribute
	    String	xmlbase = atts.getValue ("xml:base");

	    if (xmlbase != null) {
		URL	base = (URL) bases.peek ();
		try {
		    base = new URL (base, xmlbase);
		} catch (IOException e) {
		    fatal ("can't resolve xml:base attribute: " + xmlbase);
		}
		bases.push (base);
	    } else
		bases.push (bases.peek ());

	    // fetch multi-element attributes, apply standard tweaks
	    // values (uri, catalog, rewritePrefix) get normalized too,
	    // as a precaution and since we may compare the values
	    String	catalog = atts.getValue ("catalog");
	    if (catalog != null)
		catalog = normalizeURI (absolutize (catalog));

	    String	rewritePrefix = atts.getValue ("rewritePrefix");
	    if (rewritePrefix != null)
		rewritePrefix = normalizeURI (absolutize (rewritePrefix));

	    String	systemIdStartString;
	    systemIdStartString = atts.getValue ("systemIdStartString");
	    if (systemIdStartString != null) {
		systemIdStartString = normalizeURI (systemIdStartString);
		// unmatchable <rewriteSystemId>, <delegateSystemId> elements
		if (systemIdStartString.startsWith ("urn:publicid:")) {
		    error ("systemIdStartString is really a publicId!!");
		    return;
		}
	    }

	    String	uri = atts.getValue ("uri");
	    if (uri != null)
		uri = normalizeURI (absolutize (uri));

	    String	uriStartString;
	    uriStartString = atts.getValue ("uriStartString");
	    if (uriStartString != null) {
		uriStartString = normalizeURI (uriStartString);
		// unmatchable <rewriteURI>, <delegateURI> elements
		if (uriStartString.startsWith ("urn:publicid:")) {
		    error ("uriStartString is really a publicId!!");
		    return;
		}
	    }

	    // strictly speaking "group" and "catalog" shouldn't nest
	    // ... arbitrary restriction, no evident motivation

// FIXME stack "prefer" settings (two elements only!) and use
// them to populate different public mapping/delegation tables

	    if ("catalog" == local || "group" == local) {
		String	prefer = atts.getValue ("prefer");

		if (prefer != null && !"public".equals (prefer)) {
		    if (!"system".equals (prefer)) {
			error ("in <" + local + " ... prefer='...'>, "
			    + "assuming 'public'");
			prefer = "public";
		    }
		}
		if (prefer != null) {
		    if ("catalog" == local) {
			cat.hasPreference = true;
			cat.usingPublic = "public".equals (prefer);
		    } else {
			if (!cat.hasPreference || cat.usingPublic
				    != "public".equals (prefer)) {
fatal ("<group prefer=...> case not handled");
			}
		    }
		} else if ("group" == local && cat.hasPreference) {
fatal ("<group prefer=...> case not handled");
		}

	    //
	    // PUBLIC ids:  cleanly set up for id substitution
	    //
	    } else if ("public" == local) {
		String	publicId = atts.getValue ("publicId");
		String	value = null;

		if (publicId == null || uri == null) {
		    error ("expecting <public publicId=... uri=.../>");
		    return;
		}
		publicId = normalizePublicId (true, publicId);
		uri = nofrag (uri);
		if (cat.publicIds == null)
		    cat.publicIds = new Hashtable ();
		else
		    value = (String) cat.publicIds.get (publicId);
		if (value != null) {
		    if (!value.equals (uri))
			warn ("ignoring <public...> entry for " + publicId);
		} else
		    cat.publicIds.put (publicId, uri);

	    } else if ("delegatePublic" == local) {
		String	publicIdStartString;
		Object	value = null;

		publicIdStartString = atts.getValue ("publicIdStartString");
		if (publicIdStartString == null || catalog == null) {
		    error ("expecting <delegatePublic "
			+ "publicIdStartString=... catalog=.../>");
		    return;
		}
		publicIdStartString = normalizePublicId (true,
			publicIdStartString);
		if (cat.publicDelegations == null)
		    cat.publicDelegations = new Hashtable ();
		else
		    value = cat.publicDelegations.get (publicIdStartString);
		if (value != null) {
		    if (!value.equals (catalog))
			warn ("ignoring <delegatePublic...> entry for "
			    + uriStartString);
		} else
		    cat.publicDelegations.put (publicIdStartString, catalog);


	    //
	    // SYSTEM ids:  need substitution due to operational issues
	    //
	    } else if ("system" == local) {
		String	systemId = atts.getValue ("systemId");
		String	value = null;

		if (systemId == null || uri == null) {
		    error ("expecting <system systemId=... uri=.../>");
		    return;
		}
		systemId = normalizeURI (systemId);
		uri = nofrag (uri);
		if (systemId.startsWith ("urn:publicid:")) {
		    error ("systemId is really a publicId!!");
		    return;
		}
		if (cat.systemIds == null) {
		    cat.systemIds = new Hashtable ();
		    if (unified)
			cat.uris = cat.systemIds;
		} else
		    value = (String) cat.systemIds.get (systemId);
		if (value != null) {
		    if (!value.equals (uri))
			warn ("ignoring <system...> entry for " + systemId);
		} else
		    cat.systemIds.put (systemId, uri);

	    } else if ("rewriteSystem" == local) {
		String	value = null;

		if (systemIdStartString == null || rewritePrefix == null
			|| systemIdStartString.length () == 0
			|| rewritePrefix.length () == 0
			) {
		    error ("expecting <rewriteSystem "
			+ "systemIdStartString=... rewritePrefix=.../>");
		    return;
		}
		if (cat.systemRewrites == null) {
		    cat.systemRewrites = new Hashtable ();
		    if (unified)
			cat.uriRewrites = cat.systemRewrites;
		} else
		    value = (String) cat.systemRewrites.get (
		    				systemIdStartString);
		if (value != null) {
		    if (!value.equals (rewritePrefix))
			warn ("ignoring <rewriteSystem...> entry for "
			    + systemIdStartString);
		} else
		    cat.systemRewrites.put (systemIdStartString,
		    		rewritePrefix);

	    } else if ("delegateSystem" == local) {
		Object	value = null;

		if (systemIdStartString == null || catalog == null) {
		    error ("expecting <delegateSystem "
			+ "systemIdStartString=... catalog=.../>");
		    return;
		}
		if (cat.systemDelegations == null) {
		    cat.systemDelegations = new Hashtable ();
		    if (unified)
			cat.uriDelegations = cat.systemDelegations;
		} else
		    value = cat.systemDelegations.get (systemIdStartString);
		if (value != null) {
		    if (!value.equals (catalog))
			warn ("ignoring <delegateSystem...> entry for "
			    + uriStartString);
		} else
		    cat.systemDelegations.put (systemIdStartString, catalog);


	    //
	    // URI:  just like "system" ID support, except that
	    // fragment IDs are disallowed in "system" elements.
	    //
	    } else if ("uri" == local) {
		String	name = atts.getValue ("name");
		String	value = null;

		if (name == null || uri == null) {
		    error ("expecting <uri name=... uri=.../>");
		    return;
		}
		if (name.startsWith ("urn:publicid:")) {
		    error ("name is really a publicId!!");
		    return;
		}
		name = normalizeURI (name);
		if (cat.uris == null) {
		    cat.uris = new Hashtable ();
		    if (unified)
			cat.systemIds = cat.uris;
		} else
		    value = (String) cat.uris.get (name);
		if (value != null) {
		    if (!value.equals (uri))
			warn ("ignoring <uri...> entry for " + name);
		} else
		    cat.uris.put (name, uri);

	    } else if ("rewriteURI" == local) {
		String value = null;

		if (uriStartString == null || rewritePrefix == null
			|| uriStartString.length () == 0
			|| rewritePrefix.length () == 0
			) {
		    error ("expecting <rewriteURI "
			+ "uriStartString=... rewritePrefix=.../>");
		    return;
		}
		if (cat.uriRewrites == null) {
		    cat.uriRewrites = new Hashtable ();
		    if (unified)
			cat.systemRewrites = cat.uriRewrites;
		} else
		    value = (String) cat.uriRewrites.get (uriStartString);
		if (value != null) {
		    if (!value.equals (rewritePrefix))
			warn ("ignoring <rewriteURI...> entry for "
			    + uriStartString);
		} else
		    cat.uriRewrites.put (uriStartString, rewritePrefix);

	    } else if ("delegateURI" == local) {
		Object	value = null;

		if (uriStartString == null || catalog == null) {
		    error ("expecting <delegateURI "
			+ "uriStartString=... catalog=.../>");
		    return;
		}
		if (cat.uriDelegations == null) {
		    cat.uriDelegations = new Hashtable ();
		    if (unified)
			cat.systemDelegations = cat.uriDelegations;
		} else
		    value = cat.uriDelegations.get (uriStartString);
		if (value != null) {
		    if (!value.equals (catalog))
			warn ("ignoring <delegateURI...> entry for "
			    + uriStartString);
		} else
		    cat.uriDelegations.put (uriStartString, catalog);

	    //
	    // NON-DELEGATING approach to modularity
	    //
	    } else if ("nextCatalog" == local) {
		if (catalog == null) {
		    error ("expecting <nextCatalog catalog=.../>");
		    return;
		}
		if (cat.next == null)
		    cat.next = new Vector ();
		cat.next.addElement (catalog);

	    //
	    // EXTENSIONS from appendix E
	    //
	    } else if ("doctype" == local) {
		String	name = atts.getValue ("name");
		String	value = null;

		if (name == null || uri == null) {
		    error ("expecting <doctype name=... uri=.../>");
		    return;
		}
		name = normalizeURI (name);
		if (cat.doctypes == null)
		    cat.doctypes = new Hashtable ();
		else
		    value = (String) cat.doctypes.get (name);
		if (value != null) {
		    if (!value.equals (uri))
			warn ("ignoring <doctype...> entry for "
			    + uriStartString);
		} else
		    cat.doctypes.put (name, uri);
	    

	    //
	    // RESERVED ... ignore (like reserved attributes) but warn
	    //
	    } else {
		warn ("ignoring unknown catalog element: " + local);
		ignoreDepth++;
	    }
	}

	public void endElement (String uri, String local, String qName)
	throws SAXException
	{
	    if (ignoreDepth != 0)
		ignoreDepth--;
	    else
		bases.pop ();
	}
    }
}
