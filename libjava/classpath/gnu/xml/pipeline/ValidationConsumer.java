/* ValidationConsumer.java -- 
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
import java.io.StringReader;
import java.io.StringWriter;
import java.util.EmptyStackException;
import java.util.Enumeration;
import java.util.Hashtable;
import java.util.Stack;
import java.util.StringTokenizer;
import java.util.Vector;

import org.xml.sax.Attributes;
import org.xml.sax.EntityResolver;
import org.xml.sax.ErrorHandler;
import org.xml.sax.InputSource;
import org.xml.sax.Locator;
import org.xml.sax.SAXException;
import org.xml.sax.SAXParseException;
import org.xml.sax.XMLReader;
import org.xml.sax.helpers.XMLReaderFactory;

/**
 * This class checks SAX2 events to report validity errors; it works as
 * both a filter and a terminus on an event pipeline.  It relies on the
 * producer of SAX events to:  </p> <ol>
 *
 *	<li> Conform to the specification of a non-validating XML parser that
 *	reads all external entities, reported using SAX2 events. </li>
 *
 *	<li> Report ignorable whitespace as such (through the ContentHandler
 *	interface).  This is, strictly speaking, optional for nonvalidating
 *	XML processors.  </li>
 *
 *	<li> Make SAX2 DeclHandler callbacks, with default
 *	attribute values already normalized (and without "&lt;").</li>
 *
 *	<li> Make SAX2 LexicalHandler startDTD() and endDTD ()
 *	callbacks. </li>
 *
 *	<li> Act as if the <em>(URI)/namespace-prefixes</em> property were
 *	set to true, by providing XML 1.0 names and all <code>xmlns*</code>
 *	attributes (rather than omitting either or both). </li>
 *
 *	</ol>
 *
 * <p> At this writing, the major SAX2 parsers (such as &AElig;lfred2,
 * Crimson, and Xerces) meet these requirements, and this validation
 * module is used by the optional &AElig;lfred2 validation support.
 * </p>
 *
 * <p> Note that because this is a layered validator, it has to duplicate some
 * work that the parser is doing; there are also other cost to layering.
 * However, <em>because of layering it doesn't need a parser</em> in order
 * to work! You can use it with anything that generates SAX events, such
 * as an application component that wants to detect invalid content in
 * a changed area without validating an entire document, or which wants to
 * ensure that it doesn't write invalid data to a communications partner.</p>
 *
 * <p> Also, note that because this is a layered validator, the line numbers
 * reported for some errors may seem strange.  For example, if an element does
 * not permit character content, the validator
 * will use the locator provided to it.
 * That might reflect the last character of a <em>characters</em> event
 * callback, rather than the first non-whitespace character. </p>
 *
 * <hr />
 *
 * <!--
 * <p> Of interest is the fact that unlike most currently known XML validators,
 * this one can report some cases of non-determinism in element content models.
 * It is a compile-time option, enabled by default.  This will only report
 * such XML errors if they relate to content actually appearing in a document;
 * content models aren't aggressively scanned for non-deterministic structure.
 * Documents which trigger such non-deterministic transitions may be handled
 * differently by different validating parsers, without losing conformance
 * to the XML specification. </p>
 * -->
 *
 * <p> Current limitations of the validation performed are in roughly three
 * categories.  </p>
 *
 * <p> The first category represents constraints which demand violations
 * of software layering:  exposing lexical details, one of the first things
 * that <em>application</em> programming interfaces (APIs) hide.  These
 * invariably relate to XML entity handling, and to historical oddities
 * of the XML validation semantics.  Curiously,
 * recent (Autumn 1999) conformance testing showed that these constraints are
 * among those handled worst by existing XML validating parsers.  Arguments
 * have been made that each of these VCs should be turned into WFCs (most
 * of them) or discarded (popular for the standalone declaration); in short,
 * that these are bugs in the XML specification (not all via SGML): </p><ul>
 *
 *	<li> The <em>Proper Declaration/PE Nesting</em> and
 *	<em>Proper Group/PE Nesting</em> VCs can't be tested because they
 *	require access to particularly low level lexical level information.
 *	In essence, the reason XML isn't a simple thing to parse is that
 *	it's not a context free grammar, and these constraints elevate that
 *	SGML-derived context sensitivity to the level of a semantic rule.
 *
 *	<li> The <em>Standalone Document Declaration</em> VC can't be
 *	tested.  This is for two reasons.  First, this flag isn't made
 *	available through SAX2.  Second, it also requires breaking that
 *	lexical layering boundary.  (If you ever wondered why classes
 *	in compiler construction or language design barely mention the
 *	existence of context-sensitive grammars, it's because of messy
 *	issues like these.)
 *
 *	<li> The <em>Entity Declared</em> VC can't be tested, because it
 *	also requires breaking that lexical layering boundary!  There's also
 *	another issue: the VC wording (and seemingly intent) is ambiguous.
 *	(This is still true in the "Second edition" XML spec.)
 *	Since there is a WFC of the same name, everyone's life would be
 *	easier if references to undeclared parsed entities were always well
 *	formedness errors, regardless of whether they're parameter entities
 *	or not.  (Note that nonvalidating parsers are not required
 *	to report all such well formedness errors if they don't read external
 *	parameter entities, although currently most XML parsers read them
 *	in an attempt to avoid problems from inconsistent parser behavior.)
 *
 *	</ul>
 *
 * <p> The second category of limitations on this validation represent
 * constraints associated with information that is not guaranteed to be
 * available (or in one case, <em>is guaranteed not to be available</em>,
 * through the SAX2 API: </p><ul>
 *
 *	<li> The <em>Unique Element Type Declaration</em> VC may not be
 *	reportable, if the underlying parser happens not to expose
 *	multiple declarations.   (&AElig;lfred2 reports these validity
 *	errors directly.)</li>
 *
 *	<li> Similarly, the <em>Unique Notation Name</em> VC, added in the
 *	14-January-2000 XML spec errata to restrict typing models used by
 *	elements, may not be reportable.  (&AElig;lfred reports these
 *	validity errors directly.) </li>
 *
 *	</ul>
 *
 * <p> A third category relates to ease of implementation.  (Think of this
 * as "bugs".)  The most notable issue here is character handling.  Rather
 * than attempting to implement the voluminous character tables in the XML
 * specification (Appendix B), Unicode rules are used directly from
 * the java.lang.Character class.  Recent JVMs have begun to diverge from
 * the original specification for that class (Unicode 2.0), meaning that
 * different JVMs may handle that aspect of conformance differently.
 * </p>
 *
 * <p> Note that for some of the validity errors that SAX2 does not
 * expose, a nonvalidating parser is permitted (by the XML specification)
 * to report validity errors.  When used with a parser that does so for
 * the validity constraints mentioned above (or any other SAX2 event
 * stream producer that does the same thing), overall conformance is
 * substantially improved.
 *
 * @see gnu.xml.aelfred2.SAXDriver
 * @see gnu.xml.aelfred2.XmlReader
 *
 * @author David Brownell
 */
public final class ValidationConsumer extends EventFilter
{
    // report error if we happen to notice a non-deterministic choice?
    // we won't report buggy content models; just buggy instances
    private static final boolean	warnNonDeterministic = false;

    // for tracking active content models
    private String		rootName;
    private Stack		contentStack = new Stack ();

    // flags for "saved DTD" processing
    private boolean		disableDeclarations;
    private boolean		disableReset;

    //
    // most VCs get tested when we see element start tags.  the per-element
    // info (including attributes) recorded here duplicates that found inside
    // many nonvalidating parsers, hence dual lookups etc ... that's why a
    // layered validator isn't going to be as fast as a non-layered one.
    //

    // key = element name; value = ElementInfo
    private Hashtable		elements = new Hashtable ();

    // some VCs relate to ID/IDREF/IDREFS attributes
    // key = id; value = boolean true (defd) or false (refd)
    private Hashtable		ids = new Hashtable ();

    // we just record declared notation and unparsed entity names.
    // the implementation here is simple/slow; these features
    // are seldom used, one hopes they'll wither away soon
    private Vector		notations = new Vector (5, 5);
    private Vector		nDeferred = new Vector (5, 5);
    private Vector		unparsed = new Vector (5, 5);
    private Vector		uDeferred = new Vector (5, 5);
	
	// note: DocBk 3.1.7 XML defines over 2 dozen notations,
	// used when defining unparsed entities for graphics
	// (and maybe in other places)

    

    /**
     * Creates a pipeline terminus which consumes all events passed to
     * it; this will report validity errors as if they were fatal errors,
     * unless an error handler is assigned.
     *
     * @see #setErrorHandler
     */
	// constructor used by PipelineFactory
	    // ... and want one taking system ID of an external subset
    public ValidationConsumer ()
    {
	this (null);
    }

    /**
     * Creates a pipeline filter which reports validity errors and then
     * passes events on to the next consumer if they were not fatal.
     *
     * @see #setErrorHandler
     */
	// constructor used by PipelineFactory
	    // ... and want one taking system ID of an external subset
	    // (which won't send declaration events)
    public ValidationConsumer (EventConsumer next)
    {
	super (next);

	setContentHandler (this);
	setDTDHandler (this);
	try { setProperty (DECL_HANDLER, this); }
	catch (Exception e) { /* "can't happen" */ }
	try { setProperty (LEXICAL_HANDLER, this); }
	catch (Exception e) { /* "can't happen" */ }
    }

    
    private static final String	fakeRootName
	= ":Nobody:in:their_Right.Mind_would:use:this-name:1x:";
    
    /**
     * Creates a validation consumer which is preloaded with the DTD provided.
     * It does this by constructing a document with that DTD, then parsing
     * that document and recording its DTD declarations.  Then it arranges
     * not to modify that information.
     *
     * <p> The resulting validation consumer will only validate against
     * the specified DTD, regardless of whether some other DTD is found
     * in a document being parsed.
     *
     * @param rootName The name of the required root element; if this is
     *	null, any root element name will be accepted.
     * @param publicId If non-null and there is a non-null systemId, this
     *	identifier provides an alternate access identifier for the DTD's
     *	external subset.
     * @param systemId If non-null, this is a URI (normally URL) that
     *	may be used to access the DTD's external subset.
     * @param internalSubset If non-null, holds literal markup declarations
     *	comprising the DTD's internal subset.
     * @param resolver If non-null, this will be provided to the parser for
     *	use when resolving parameter entities (including any external subset).
     * @param resolver If non-null, this will be provided to the parser for
     *	use when resolving parameter entities (including any external subset).
     * @param minimalElement If non-null, a minimal valid document.
     *
     * @exception SAXNotSupportedException If the default SAX parser does
     *	not support the standard lexical or declaration handlers.
     * @exception SAXParseException If the specified DTD has either
     *	well-formedness or validity errors
     * @exception IOException If the specified DTD can't be read for
     *	some reason
     */
    public ValidationConsumer (
	String		rootName,
	String		publicId,
	String		systemId,
	String		internalSubset,
	EntityResolver	resolver,
	String		minimalDocument
    ) throws SAXException, IOException
    {
	this (null);

	disableReset = true;
	if (rootName == null)
	    rootName = fakeRootName;

	//
	// Synthesize document with that DTD; is it possible to do
	// better for the declaration of the root element?
	//
	// NOTE:  can't use SAX2 to write internal subsets.
	//
	StringWriter	writer = new StringWriter ();

	writer.write ("<!DOCTYPE ");
	writer.write (rootName);
	if (systemId != null) {
	    writer.write ("\n  ");
	    if (publicId != null) {
		writer.write ("PUBLIC '");
		writer.write (publicId);
		writer.write ("'\n\t'");
	    } else
		writer.write ("SYSTEM '");
	    writer.write (systemId);
	    writer.write ("'");
	}
	writer.write (" [ ");
	if (rootName == fakeRootName) {
	    writer.write ("\n<!ELEMENT ");
	    writer.write (rootName);
	    writer.write (" EMPTY>");
	}
	if (internalSubset != null)
	    writer.write (internalSubset);
	writer.write ("\n ]>");

	if (minimalDocument != null) {
	    writer.write ("\n");
	    writer.write (minimalDocument);
	    writer.write ("\n");
	} else {
	    writer.write (" <");
	    writer.write (rootName);
	    writer.write ("/>\n");
	}
	minimalDocument = writer.toString ();

	//
	// OK, load it
	//
	XMLReader	producer;

	producer = XMLReaderFactory.createXMLReader ();
	bind (producer, this);

	if (resolver != null)
	    producer.setEntityResolver (resolver);

	InputSource	in;
	
	in = new InputSource (new StringReader (minimalDocument));
	producer.parse (in);

	disableDeclarations = true;
	if (rootName == fakeRootName)
	    this.rootName = null;
    }

    private void resetState ()
    {
	if (!disableReset) {
	    rootName = null;
	    contentStack.removeAllElements ();
	    elements.clear ();
	    ids.clear ();

	    notations.removeAllElements ();
	    nDeferred.removeAllElements ();
	    unparsed.removeAllElements ();
	    uDeferred.removeAllElements ();
	}
    }


    private void warning (String description)
    throws SAXException
    {
	ErrorHandler		errHandler = getErrorHandler ();
	Locator			locator = getDocumentLocator ();
	SAXParseException	err;

	if (errHandler == null)
	    return;

	if (locator == null)
	    err = new SAXParseException (description, null, null, -1, -1);
	else
	    err = new SAXParseException (description, locator);
	errHandler.warning (err);
    }

    // package private (for ChildrenRecognizer)
    private void error (String description)
    throws SAXException
    {
	ErrorHandler		errHandler = getErrorHandler ();
	Locator			locator = getDocumentLocator ();
	SAXParseException	err;

	if (locator == null)
	    err = new SAXParseException (description, null, null, -1, -1);
	else
	    err = new SAXParseException (description, locator);
	if (errHandler != null)
	    errHandler.error (err);
	else	// else we always treat it as fatal!
	    throw err;
    }

    private void fatalError (String description)
    throws SAXException
    {
	ErrorHandler		errHandler = getErrorHandler ();
	Locator			locator = getDocumentLocator ();
	SAXParseException	err;

	if (locator != null)
	    err = new SAXParseException (description, locator);
	else
	    err = new SAXParseException (description, null, null, -1, -1);
	if (errHandler != null)
	    errHandler.fatalError (err);
	// we always treat this as fatal, regardless of the handler
	throw err;
    }


    private static boolean isExtender (char c)
    {
	// [88] Extender ::= ...
	return c == 0x00b7 || c == 0x02d0 || c == 0x02d1 || c == 0x0387
	       || c == 0x0640 || c == 0x0e46 || c == 0x0ec6 || c == 0x3005
	       || (c >= 0x3031 && c <= 0x3035)
	       || (c >= 0x309d && c <= 0x309e)
	       || (c >= 0x30fc && c <= 0x30fe);
    }


    // use augmented Unicode rules, not full XML rules
    private boolean isName (String name, String context, String id)
    throws SAXException
    {
	char	buf [] = name.toCharArray ();
	boolean	pass = true;

	if (!Character.isUnicodeIdentifierStart (buf [0])
		&& ":_".indexOf (buf [0]) == -1)
	    pass = false;
	else {
	    int max = buf.length;
	    for (int i = 1; pass && i < max; i++) {
		char c = buf [i];
		if (!Character.isUnicodeIdentifierPart (c)
			&& ":-_.".indexOf (c) == -1
			&& !isExtender (c))
		    pass = false;
	    }
	}

	if (!pass)
	    error ("In " + context + " for " + id
		+ ", '" + name + "' is not a name");
	return pass;	// true == OK
    }

    // use augmented Unicode rules, not full XML rules
    private boolean isNmtoken (String nmtoken, String context, String id)
    throws SAXException
    {
	char	buf [] = nmtoken.toCharArray ();
	boolean	pass = true;
	int	max = buf.length;

	// XXX make this share code with isName

	for (int i = 0; pass && i < max; i++) {
		char c = buf [i];
	    if (!Character.isUnicodeIdentifierPart (c)
		    && ":-_.".indexOf (c) == -1
		    && !isExtender (c))
		pass = false;
	}

	if (!pass)
	    error ("In " + context + " for " + id
		+ ", '" + nmtoken + "' is not a name token");
	return pass;	// true == OK
    }

    private void checkEnumeration (String value, String type, String name)
    throws SAXException
    {
	if (!hasMatch (value, type))
	    // VC: Enumeration
	    error ("Value '" + value
		+ "' for attribute '" + name
		+ "' is not permitted: " + type);
    }

    // used to test enumerated attributes and mixed content models
    // package private
    static boolean hasMatch (String value, String orList)
    {
	int len = value.length ();
	int max = orList.length () - len;

	for (int start = 0;
		(start = orList.indexOf (value, start)) != -1;
		start++) {
	    char c;

	    if (start > max)
		break;
	    c = orList.charAt (start - 1);
	    if (c != '|' && c != '('/*)*/)
		continue;
	    c = orList.charAt (start + len);
	    if (c != '|' && /*(*/ c != ')')
		continue;
	    return true;
	}
	return false;
    }

    /**
     * <b>LexicalHandler</b> Records the declaration of the root
     * element, so it can be verified later.
     * Passed to the next consumer, unless this one was
     * preloaded with a particular DTD.
     */
    public void startDTD (String name, String publicId, String systemId)
    throws SAXException
    {
	if (disableDeclarations)
	    return;

	rootName = name;
	super.startDTD (name, publicId, systemId);
    }

    /**
     * <b>LexicalHandler</b> Verifies that all referenced notations
     * and unparsed entities have been declared.
     * Passed to the next consumer, unless this one was
     * preloaded with a particular DTD.
     */
    public void endDTD ()
    throws SAXException
    {
	if (disableDeclarations)
	    return;

	// this is a convenient hook for end-of-dtd checks, but we
	// could also trigger it in the first startElement call.
	// locator info is more appropriate here though.

	// VC: Notation Declared (NDATA can refer to them before decls,
	//	as can NOTATION attribute enumerations and defaults)
	int length = nDeferred.size ();
	for (int i = 0; i < length; i++) {
	    String notation = (String) nDeferred.elementAt (i);
	    if (!notations.contains (notation)) {
		error ("A declaration referred to notation '" + notation
			+ "' which was never declared");
	    }
	}
	nDeferred.removeAllElements ();

	// VC: Entity Name (attribute values can refer to them
	//	before they're declared); VC Attribute Default Legal
	length = uDeferred.size ();
	for (int i = 0; i < length; i++) {
	    String entity = (String) uDeferred.elementAt (i);
	    if (!unparsed.contains (entity)) {
		error ("An attribute default referred to entity '" + entity
			+ "' which was never declared");
	    }
	}
	uDeferred.removeAllElements ();
	super.endDTD ();
    }


    // These are interned, so we can rely on "==" to find the type of
    // all attributes except enumerations ...
    // "(this|or|that|...)" and "NOTATION (this|or|that|...)"
    static final String types [] = {
	"CDATA",
	"ID", "IDREF", "IDREFS",
	"NMTOKEN", "NMTOKENS",
	"ENTITY", "ENTITIES"
    };


    /**
     * <b>DecllHandler</b> Records attribute declaration for later use
     * in validating document content, and checks validity constraints
     * that are applicable to attribute declarations.
     * Passed to the next consumer, unless this one was
     * preloaded with a particular DTD.
     */
    public void attributeDecl (
	String eName,
	String aName,
	String type,
	String mode,
	String value
    ) throws SAXException
    {
	if (disableDeclarations)
	    return;

	ElementInfo	info = (ElementInfo) elements.get (eName);
	AttributeInfo	ainfo = new AttributeInfo ();
	boolean		checkOne = false;
	boolean		interned = false;

	// cheap interning of type names and #FIXED, #REQUIRED
	// for faster startElement (we can use "==")
	for (int i = 0; i < types.length; i++) {
	    if (types [i].equals (type)) {
		type = types [i];
		interned = true;
		break;
	    }
	}
	if ("#FIXED".equals (mode))
	    mode = "#FIXED";
	else if ("#REQUIRED".equals (mode))
	    mode = "#REQUIRED";

	ainfo.type = type;
	ainfo.mode = mode;
	ainfo.value = value;

	// we might not have seen the content model yet
	if (info == null) {
	    info = new ElementInfo (eName);
	    elements.put (eName, info);
	}
	if ("ID" == type) {
	    checkOne = true;
	    if (!("#REQUIRED" == mode || "#IMPLIED".equals (mode))) {
		// VC: ID Attribute Default
		error ("ID attribute '" + aName
		    + "' must be #IMPLIED or #REQUIRED");
	    }

	} else if (!interned && type.startsWith ("NOTATION ")) {
	    checkOne = true;

	    // VC: Notation Attributes (notations must be declared)
	    StringTokenizer	tokens = new StringTokenizer (
		type.substring (10, type.lastIndexOf (')')),
		"|");
	    while (tokens.hasMoreTokens ()) {
		String	token = tokens.nextToken ();
		if (!notations.contains (token))
		    nDeferred.addElement (token);
	    }
	}
	if (checkOne) {
	    for (Enumeration e = info.attributes.keys ();
		    e.hasMoreElements ();
		    /* NOP */) {
		String		name;
		AttributeInfo	ainfo2;

		name = (String) e.nextElement ();
		ainfo2 = (AttributeInfo) info.attributes.get (name);
		if (type == ainfo2.type || !interned /* NOTATION */) {
		    // VC: One ID per Element Type
		    // VC: One Notation per Element TYpe
		    error ("Element '" + eName
			+ "' already has an attribute of type "
			+ (interned ? "NOTATION" : type)
			+ " ('" + name
			+ "') so '" + aName 
			+ "' is a validity error");
		}
	    }
	}

	// VC: Attribute Default Legal
	if (value != null) {

	    if ("CDATA" == type) {
		// event source rejected '<'

	    } else if ("NMTOKEN" == type) {
		// VC: Name Token (is a nmtoken)
		isNmtoken (value, "attribute default", aName);

	    } else if ("NMTOKENS" == type) {
		// VC: Name Token (is a nmtoken; at least one value)
		StringTokenizer	tokens = new StringTokenizer (value);
		if (!tokens.hasMoreTokens ())
		    error ("Default for attribute '" + aName
			+ "' must have at least one name token.");
		else do {
		    String token = tokens.nextToken ();
		    isNmtoken (token, "attribute default", aName);
		} while (tokens.hasMoreTokens ());

	    } else if ("IDREF" == type || "ENTITY" == type) {
		// VC: Entity Name (is a name)
		// VC: IDREF (is a name) (is declared)
		isName (value, "attribute default", aName);
		if ("ENTITY" == type && !unparsed.contains (value))
		    uDeferred.addElement (value);

	    } else if ("IDREFS" == type || "ENTITIES" == type) {
		// VC: Entity Name (is a name; at least one value)
		// VC: IDREF (is a name; at least one value)
		StringTokenizer	names = new StringTokenizer (value);
		if (!names.hasMoreTokens ())
		    error ("Default for attribute '" + aName
			+ "' must have at least one name.");
		else do {
		    String name = names.nextToken ();
		    isName (name, "attribute default", aName);
		    if ("ENTITIES" == type && !unparsed.contains (name))
			uDeferred.addElement (value);
		} while (names.hasMoreTokens ());
	    
	    } else if (type.charAt (0) == '(' /*)*/ ) {
		// VC: Enumeration (must match)
		checkEnumeration (value, type, aName);

	    } else if (!interned && checkOne) {	/* NOTATION */
		// VC: Notation attributes (must be names)
		isName (value, "attribute default", aName);

		// VC: Notation attributes (must be declared)
		if (!notations.contains (value))
		    nDeferred.addElement (value);
		
		// VC: Enumeration (must match)
		checkEnumeration (value, type, aName);

	    } else if ("ID" != type)
		throw new RuntimeException ("illegal attribute type: " + type);
	}

	if (info.attributes.get (aName) == null)
	    info.attributes.put (aName, ainfo);
	/*
	else
	    warning ("Element '" + eName
		+ "' already has an attribute named '" + aName + "'");
	*/

	if ("xml:space".equals (aName)) {
	    if (!("(default|preserve)".equals (type)
		    || "(preserve|default)".equals (type)
			// these next two are arguable; XHTML's DTD doesn't
			// deserve errors.  After all, it's not like any
			// illegal _value_ could pass ...
		    || "(preserve)".equals (type)
		    || "(default)".equals (type)
		    ))
		error (
		    "xml:space attribute type must be like '(default|preserve)'"
		    + " not '" + type + "'"
		    );

	}
	super.attributeDecl (eName, aName, type, mode, value);
    }

    /**
     * <b>DecllHandler</b> Records the element declaration for later use
     * when checking document content, and checks validity constraints that
     * apply to element declarations.  Passed to the next consumer, unless
     * this one was preloaded with a particular DTD.
     */
    public void elementDecl (String name, String model)
    throws SAXException
    {
	if (disableDeclarations)
	    return;

	ElementInfo	info = (ElementInfo) elements.get (name);

	// we might have seen an attribute decl already
	if (info == null) {
	    info = new ElementInfo (name);
	    elements.put (name, info);
	}
	if (info.model != null) {
	    // NOTE:  not all parsers can report such duplicates.
	    // VC: Unique Element Type Declaration
	    error ("Element type '" + name
		+ "' was already declared.");
	} else {
	    info.model = model;

	    // VC: No Duplicate Types (in mixed content models)
	    if (model.charAt (1) == '#') 	// (#PCDATA...
		info.getRecognizer (this);
	}
	super.elementDecl (name, model);
    }

    /**
     * <b>DecllHandler</b> passed to the next consumer, unless this
     * one was preloaded with a particular DTD
     */
    public void internalEntityDecl (String name, String value)
    throws SAXException
    {
	if (!disableDeclarations)
	    super.internalEntityDecl (name, value);
    }

    /**
     * <b>DecllHandler</b> passed to the next consumer, unless this
     * one was preloaded with a particular DTD
     */
    public void externalEntityDecl (String name,
    	String publicId, String systemId)
    throws SAXException
    {
	if (!disableDeclarations)
	    super.externalEntityDecl (name, publicId, systemId);
    }


    /**
     * <b>DTDHandler</b> Records the notation name, for checking
     * NOTATIONS attribute values and declararations of unparsed
     * entities.  Passed to the next consumer, unless this one was
     * preloaded with a particular DTD.
     */
    public void notationDecl (String name, String publicId, String systemId)
    throws SAXException
    {
	if (disableDeclarations)
	    return;

	notations.addElement (name);
	super.notationDecl (name, publicId, systemId);
    }

    /**
     * <b>DTDHandler</b> Records the entity name, for checking
     * ENTITY and ENTITIES attribute values; records the notation
     * name if it hasn't yet been declared.  Passed to the next consumer,
     * unless this one was preloaded with a particular DTD.
     */
    public void unparsedEntityDecl (
	String name,
	String publicId,
	String systemId,
	String notationName
    ) throws SAXException
    {
	if (disableDeclarations)
	    return;

	unparsed.addElement (name);
	if (!notations.contains (notationName))
	    nDeferred.addElement (notationName);
	super.unparsedEntityDecl (name, publicId, systemId, notationName);
    }
    
    
    /**
     * <b>ContentHandler</b> Ensures that state from any previous parse
     * has been deleted.
     * Passed to the next consumer.
     */
    public void startDocument ()
    throws SAXException
    {
	resetState ();
	super.startDocument ();
    }


    private static boolean isAsciiLetter (char c)
    {
	return (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z');
    }


    /**
     * <b>ContentHandler</b> Reports a fatal exception.  Validating
     * XML processors may not skip any entities.
     */
    public void skippedEntity (String name)
    throws SAXException
    {
	fatalError ("may not skip entities");
    }

    /*
     * SAX2 doesn't expand non-PE refs in attribute defaults...
     */
    private String expandDefaultRefs (String s)
    throws SAXException
    {
	if (s.indexOf ('&') < 0)
	    return s;
	
// FIXME: handle &#nn; &#xnn; &name;
	String message = "Can't expand refs in attribute default: " + s;
	warning (message);

	return s;
    }

    /**
     * <b>ContentHandler</b> Performs validity checks against element
     * (and document) content models, and attribute values.
     * Passed to the next consumer.
     */
    public void startElement (
	String		uri,
	String		localName,
	String		qName,
	Attributes	atts
    ) throws SAXException
    {
	//
	// First check content model for the enclosing scope.
	//
	if (contentStack.isEmpty ()) {
	    // VC:  Root Element Type
	    if (!qName.equals (rootName)) {
		if (rootName == null)
		    warning ("This document has no DTD, can't be valid");
		else
		    error ("Root element type '" + qName
			+ "' was declared to be '" + rootName + "'");
	    }
	} else {
	    Recognizer state = (Recognizer) contentStack.peek ();

	    if (state != null) {
		Recognizer newstate = state.acceptElement (qName);

		if (newstate == null)
		    error ("Element type '" + qName
			+ "' in element '" + state.type.name
			+ "' violates content model " + state.type.model
			);
		if (newstate != state) {
		    contentStack.pop ();
		    contentStack.push (newstate);
		}
	    }
	}

	//
	// Then check that this element was declared, and push the
	// object used to validate its content model onto our stack.
	//
	// This is where the recognizer gets created, if needed; if
	// it's a "children" (elements) content model, an NDFA is
	// created.  (One recognizer is used per content type, no
	// matter how complex that recognizer is.)
	//
	ElementInfo		info;

	info = (ElementInfo) elements.get (qName);
	if (info == null || info.model == null) {
	    // VC: Element Valid (base clause)
	    error ("Element type '" + qName + "' was not declared");
	    contentStack.push (null);

	    // for less diagnostic noise, fake a declaration.
	    elementDecl (qName, "ANY");
	} else
	    contentStack.push (info.getRecognizer (this));

	//
	// Then check each attribute present
	//
	int			len;
	String			aname;
	AttributeInfo		ainfo;

	if (atts != null)
	    len = atts.getLength ();
	else
	    len = 0;
	
	for (int i = 0; i < len; i++) {
	    aname = atts.getQName (i);

	    if (info == null
		    || (ainfo = (AttributeInfo) info.attributes.get (aname))
			    == null) {
		// VC: Attribute Value Type
		error ("Attribute '" + aname
		    + "' was not declared for element type " + qName);
		continue;
	    }

	    String value = atts.getValue (i);

	    // note that "==" for type names and "#FIXED" is correct
	    // (and fast) since we've interned those literals.

	    if ("#FIXED" == ainfo.mode) {
		String expanded = expandDefaultRefs (ainfo.value);

		// VC: Fixed Attribute Default
		if (!value.equals (expanded)) {
		    error ("Attribute '" + aname
			+ "' must match " + expanded
			);
		    continue;
		}
	    }

	    if ("CDATA" == ainfo.type)
		continue;
	    
	    //
	    // For all other attribute types, there are various
	    // rules to follow.
	    //
	    
	    if ("ID" == ainfo.type) {
		// VC: ID (must be a name)
		if (isName (value, "ID attribute", aname)) {
		    if (Boolean.TRUE == ids.get (value))
			// VC: ID (appears once)
			error ("ID attribute " + aname
			    + " uses an ID value '" + value
			    + "' which was already declared.");
		    else
			// any forward refs are no longer problems
			ids.put (value, Boolean.TRUE);
		}
		continue;
	    } 

	    if ("IDREF" == ainfo.type) {
		// VC: IDREF (value must be a name)
		if (isName (value, "IDREF attribute", aname)) {
		    // VC: IDREF (must match some ID attribute)
		    if (ids.get (value) == null)
			// new -- assume it's a forward ref
			ids.put (value, Boolean.FALSE);
		}
		continue;
	    } 

	    if ("IDREFS" == ainfo.type) {
		StringTokenizer	tokens = new StringTokenizer (value, " ");

		if (!tokens.hasMoreTokens ()) {
		    // VC: IDREF (one or more values)
		    error ("IDREFS attribute " + aname
			+ " must have at least one ID ref");
		} else do {
		    String id = tokens.nextToken ();

		    // VC: IDREF (value must be a name)
		    if (isName (id, "IDREFS attribute", aname)) {
			// VC: IDREF (must match some ID attribute)
			if (ids.get (id) == null)
			    // new -- assume it's a forward ref
			    ids.put (id, Boolean.FALSE);
		    }
		} while (tokens.hasMoreTokens ());
		continue;
	    }

	    if ("NMTOKEN" == ainfo.type) {
		// VC: Name Token (is a name token)
		isNmtoken (value, "NMTOKEN attribute", aname);
		continue;
	    }

	    if ("NMTOKENS" == ainfo.type) {
		StringTokenizer	tokens = new StringTokenizer (value, " ");

		if (!tokens.hasMoreTokens ()) {
		    // VC: Name Token (one or more values)
		    error ("NMTOKENS attribute " + aname
			+ " must have at least one name token");
		} else do {
		    String token = tokens.nextToken ();

		    // VC: Name Token (is a name token)
		    isNmtoken (token, "NMTOKENS attribute", aname);
		} while (tokens.hasMoreTokens ());
		continue;
	    }

	    if ("ENTITY" == ainfo.type) {
		if (!unparsed.contains (value))
		    // VC: Entity Name
		    error ("Value of attribute '" + aname
			+ "' refers to unparsed entity '" + value
			+ "' which was not declared.");
		continue;
	    }

	    if ("ENTITIES" == ainfo.type) {
		StringTokenizer	tokens = new StringTokenizer (value, " ");

		if (!tokens.hasMoreTokens ()) {
		    // VC: Entity Name (one or more values)
		    error ("ENTITIES attribute " + aname
			+ " must have at least one name token");
		} else do {
		    String entity = tokens.nextToken ();

		    if (!unparsed.contains (entity))
			// VC: Entity Name
			error ("Value of attribute '" + aname
			    + "' refers to unparsed entity '" + entity
			    + "' which was not declared.");
		} while (tokens.hasMoreTokens ());
		continue;
	    }

	    //
	    // check for enumerations last; more expensive
	    //
	    if (ainfo.type.charAt (0) == '(' /*)*/	
		    || ainfo.type.startsWith ("NOTATION ")
		    ) {
		// VC: Enumeration (value must be defined)
		checkEnumeration (value, ainfo.type, aname);
		continue;
	    }
	}

	//
	// Last, check that all #REQUIRED attributes were provided
	//
	if (info != null) {
	    Hashtable	table = info.attributes;

	    if (table.size () != 0) {
		Enumeration	e = table.keys ();

		// XXX table.keys uses the heap, bleech -- slows things

		while (e.hasMoreElements ()) {
		    aname = (String) e.nextElement ();
		    ainfo = (AttributeInfo) table.get (aname);

		    // "#REQUIRED" mode was interned in attributeDecl
		    if ("#REQUIRED" == ainfo.mode
			    && atts.getValue (aname) == null) {
			// VC: Required Attribute
			error ("Attribute '" + aname + "' must be specified "
			    + "for element type " + qName);
		    }
		}
	    }
	}
	super.startElement (uri, localName, qName, atts);
    }

    /**
     * <b>ContentHandler</b> Reports a validity error if the element's content
     * model does not permit character data.
     * Passed to the next consumer.
     */
    public void characters (char ch [], int start, int length)
    throws SAXException
    {
	Recognizer state;

	if (contentStack.empty ())
	    state = null;
	else
	    state = (Recognizer) contentStack.peek ();

	// NOTE:  if this ever supports with SAX parsers that don't
	// report ignorable whitespace as such (only XP?), this class
	// needs to morph it into ignorableWhitespace() as needed ...

	if (state != null && !state.acceptCharacters ())
	    // VC: Element Valid (clauses three, four -- see recognizer)
	    error ("Character content not allowed in element "
		+ state.type.name);
	
	super.characters (ch, start, length);
    }
	

    /**
     * <b>ContentHandler</b> Reports a validity error if the element's content
     * model does not permit end-of-element yet, or a well formedness error
     * if there was no matching startElement call.
     * Passed to the next consumer.
     */
    public void endElement (String uri, String localName, String qName)
    throws SAXException
    {
	try {
	    Recognizer state = (Recognizer) contentStack.pop ();

	    if (state != null && !state.completed ())
		// VC: Element valid (clauses two, three, four; see Recognizer)
		error ("Premature end for element '"
		    + state.type.name
		    + "', content model "
		    + state.type.model);
	    
	    // could insist on match of start element, but that's
	    // something the input stream must to guarantee.

	} catch (EmptyStackException e) {
	    fatalError ("endElement without startElement: " + qName
		+ ((uri == null)
		    ? ""
		    : ( " { '" + uri + "', " + localName + " }")));
	}
	super.endElement (uri, localName, qName);
    }

    /**
     * <b>ContentHandler</b> Checks whether all ID values that were
     * referenced have been declared, and releases all resources. 
     * Passed to the next consumer.
     * 
     * @see #setDocumentLocator
     */
    public void endDocument ()
    throws SAXException
    {
	for (Enumeration idNames = ids.keys ();
		idNames.hasMoreElements ();
		/* NOP */) {
	    String id = (String) idNames.nextElement ();
	    
	    if (Boolean.FALSE == ids.get (id)) {
		// VC: IDREF (must match ID)
		error ("Undeclared ID value '" + id
		    + "' was referred to by an IDREF/IDREFS attribute");
	    }
	}

	resetState ();
	super.endDocument ();
    }


    /** Holds per-element declarations */
    static private final class ElementInfo
    {
	String			name;
	String			model;

	// key = attribute name; value = AttributeInfo
	Hashtable		attributes = new Hashtable (11);

	ElementInfo (String n) { name = n; }

	private Recognizer	recognizer;

	// for validating content models:  one per type, shared,
	// and constructed only on demand ... so unused elements do
	// not need to consume resources.
	Recognizer	getRecognizer (ValidationConsumer consumer)
	throws SAXException
	{
	    if (recognizer == null) {
		if ("ANY".equals (model))
		    recognizer = ANY;
		else if ("EMPTY".equals (model))
		    recognizer = new EmptyRecognizer (this);
		else if ('#' == model.charAt (1))
		    // n.b. this constructor does a validity check
		    recognizer = new MixedRecognizer (this, consumer);
		else
		    recognizer = new ChildrenRecognizer (this, consumer);
	    }
	    return recognizer;
	}
    }

    /** Holds per-attribute declarations */
    static private final class AttributeInfo
    {
	String	type;
	String	mode;		// #REQUIRED, etc (or null)
	String	value;		// or null
    }


    //
    // Content model validation
    //

    static private final Recognizer	ANY = new Recognizer (null);


    // Base class defines the calls used to validate content,
    // and supports the "ANY" content model
    static private class Recognizer
    {
	final ElementInfo	type;

	Recognizer (ElementInfo t) { type = t; }

	// return true iff character data is legal here
	boolean acceptCharacters ()
	throws SAXException
	    // VC: Element Valid (third and fourth clauses)
	    { return true; }

	// null return = failure
	// otherwise, next state (like an FSM)
	// prerequisite: tested that name was declared
	Recognizer acceptElement (String name)
	throws SAXException
	    // VC: Element Valid (fourth clause)
	    { return this; }

	// return true iff model is completed, can finish
	boolean completed ()
	throws SAXException
	    // VC: Element Valid (fourth clause)
	    { return true; }
	
	public String toString ()
	    // n.b. "children" is the interesting case!
	    { return (type == null) ? "ANY" : type.model; }
    }

    // "EMPTY" content model -- no characters or elements
    private static final class EmptyRecognizer extends Recognizer
    {
	public EmptyRecognizer (ElementInfo type)
	    { super (type); }

	// VC: Element Valid (first clause)
	boolean acceptCharacters ()
	    { return false; }

	// VC: Element Valid (first clause)
	Recognizer acceptElement (String name)
	    { return null; }
    }

    // "Mixed" content model -- ANY, but restricts elements
    private static final class MixedRecognizer extends Recognizer
    {
	private String	permitted [];

	// N.B. constructor tests for duplicated element names (VC)
	public MixedRecognizer (ElementInfo t, ValidationConsumer v)
	throws SAXException
	{
	    super (t);

	    // (#PCDATA...)* or (#PCDATA) ==> ... or empty
	    // with the "..." being "|elname|..."
	    StringTokenizer	tokens = new StringTokenizer (
		t.model.substring (8, t.model.lastIndexOf (')')),
		"|");
	    Vector		vec = new Vector ();

	    while (tokens.hasMoreTokens ()) {
		String token = tokens.nextToken ();

		if (vec.contains (token))
		    v.error ("element " + token
			+ " is repeated in mixed content model: "
			+ t.model);
		else
		    vec.addElement (token.intern ());
	    }
	    permitted = new String [vec.size ()];
	    for (int i = 0; i < permitted.length; i++)
		permitted [i] = (String) vec.elementAt (i);
	    
	    // in one large machine-derived DTD sample, most of about
	    // 250 mixed content models were empty, and 25 had ten or
	    // more entries.  2 had over a hundred elements.  Linear
	    // search isn't obviously wrong.
	}

	// VC: Element Valid (third clause)
	Recognizer acceptElement (String name)
	{
	    int		length = permitted.length;

	    // first pass -- optimistic w.r.t. event source interning
	    // (and document validity)
	    for (int i = 0; i < length; i++)
		if (permitted [i] == name)
		    return this;
	    // second pass -- pessimistic w.r.t. event source interning
	    for (int i = 0; i < length; i++)
		if (permitted [i].equals (name))
		    return this;
	    return null;
	}
    }


    // recognizer loop flags, see later
    private static final int		F_LOOPHEAD = 0x01;
    private static final int		F_LOOPNEXT = 0x02;

    // for debugging -- used to label/count nodes in toString()
    private static int			nodeCount;

    /**
     * "Children" content model -- these are nodes in NDFA state graphs.
     * They work in fixed space.  Note that these graphs commonly have
     * cycles, handling features such as zero-or-more and one-or-more.
     *
     * <p>It's readonly, so only one copy is ever needed.  The content model
     * stack may have any number of pointers into each graph, when a model
     * happens to be needed more than once due to element nesting.  Since
     * traversing the graph just moves to another node, and never changes
     * it, traversals never interfere with each other.
     *
     * <p>There is an option to report non-deterministic models.  These are
     * always XML errors, but ones which are not often reported despite the
     * fact that they can lead to different validating parsers giving
     * different results for the same input.  (The XML spec doesn't require
     * them to be reported.)
     *
     * <p><b>FIXME</b> There's currently at least one known bug here, in that
     * it's not actually detecting the non-determinism it tries to detect.
     * (Of the "optional.xml" test, the once-or-twice-2* tests are all non-D;
     * maybe some others.)  This may relate to the issue flagged below as
     * "should not" happen (but it was), which showed up when patching the
     * graph to have one exit node (or more EMPTY nodes).
     */
    private static final class ChildrenRecognizer extends Recognizer
	implements Cloneable
    {
	// for reporting non-deterministic content models
	// ... a waste of space if we're not reporting those!
	// ... along with the 'model' member (in base class)
	private ValidationConsumer	consumer;

	// for CHOICE nodes -- each component is an arc that
	// accepts a different NAME (or is EMPTY indicating
	// NDFA termination).
	private Recognizer		components [];

	// for NAME/SEQUENCE nodes -- accepts that NAME and
	// then goes to the next node (CHOICE, NAME, EMPTY).
	private String			name;
	private Recognizer		next;

	// loops always point back to a CHOICE node. we mark such choice
	// nodes (F_LOOPHEAD) for diagnostics and faster deep cloning.
	// We also mark nodes before back pointers (F_LOOPNEXT), to ensure
	// termination when we patch sequences and loops.
	private int			flags;


	// prevent a needless indirection between 'this' and 'node'
	private void copyIn (ChildrenRecognizer node)
	{
	    // model & consumer are already set
	    components = node.components;
	    name = node.name;
	    next = node.next;
	    flags = node.flags;
	}

	// used to construct top level "children" content models,
	public ChildrenRecognizer (ElementInfo type, ValidationConsumer vc)
	{
	    this (vc, type);
	    populate (type.model.toCharArray (), 0);
	    patchNext (new EmptyRecognizer (type), null);
	}

	// used internally; populating is separate
	private ChildrenRecognizer (ValidationConsumer vc, ElementInfo type)
	{
	    super (type);
	    consumer = vc;
	}


	//
	// When rewriting some graph nodes we need deep clones in one case;
	// mostly shallow clones (what the JVM handles for us) are fine.
	//
	private ChildrenRecognizer shallowClone ()
	{
	    try {
		return (ChildrenRecognizer) clone ();
	    } catch (CloneNotSupportedException e) {
		throw new Error ("clone");
	    }
	}

	private ChildrenRecognizer deepClone ()
	{
	    return deepClone (new Hashtable (37));
	}

	private ChildrenRecognizer deepClone (Hashtable table)
	{
	    ChildrenRecognizer retval;

	    if ((flags & F_LOOPHEAD) != 0) {
		retval = (ChildrenRecognizer) table.get (this);
		if (retval != null)
		    return this;

		retval = shallowClone ();
		table.put (this, retval);
	    } else
		retval = shallowClone ();

	    if (next != null) {
		if (next instanceof ChildrenRecognizer)
		    retval.next = ((ChildrenRecognizer)next)
			    .deepClone (table);
		else if (!(next instanceof EmptyRecognizer))
		    throw new RuntimeException ("deepClone");
	    }

	    if (components != null) {
		retval.components = new Recognizer [components.length];
		for (int i = 0; i < components.length; i++) {
		    Recognizer temp = components [i];

		    if (temp == null)
			retval.components [i] = null;
		    else if (temp instanceof ChildrenRecognizer)
			retval.components [i] = ((ChildrenRecognizer)temp)
				.deepClone (table);
		    else if (!(temp instanceof EmptyRecognizer))
			throw new RuntimeException ("deepClone");
		}
	    }

	    return retval;
	}

	// connect subgraphs, first to next (sequencing)
	private void patchNext (Recognizer theNext, Hashtable table)
	{
	    // backpointers must not be repatched or followed
	    if ((flags & F_LOOPNEXT) != 0)
		return;

	    // XXX this table "shouldn't" be needed, right?
	    // but some choice nodes looped if it isn't there.
	    if (table != null && table.get (this) != null)
		return;
	    if (table == null)
		table = new Hashtable ();

	    // NAME/SEQUENCE
	    if (name != null) {
		if (next == null)
		    next = theNext;
		else if (next instanceof ChildrenRecognizer) {
		    ((ChildrenRecognizer)next).patchNext (theNext, table);
		} else if (!(next instanceof EmptyRecognizer))
		    throw new RuntimeException ("patchNext");
		return;
	    }

	    // CHOICE
	    for (int i = 0; i < components.length; i++) {
		if (components [i] == null)
		    components [i] = theNext;
		else if (components [i] instanceof ChildrenRecognizer) {
		    ((ChildrenRecognizer)components [i])
			    .patchNext (theNext, table);
		} else if (!(components [i] instanceof EmptyRecognizer))
		    throw new RuntimeException ("patchNext");
	    }

	    if (table != null && (flags | F_LOOPHEAD) != 0)
		table.put (this, this);
	}

	/**
	 * Parses a 'children' spec (or recursively 'cp') and makes this
	 * become a regular graph node.
	 *
	 * @return index after this particle
	 */
	private int populate (char parseBuf [], int startPos)
	{
	    int		nextPos = startPos + 1;
	    char	c;

	    if (nextPos < 0 || nextPos >= parseBuf.length)
		throw new IndexOutOfBoundsException ();

	    // Grammar of the string is from the XML spec, but
	    // with whitespace removed by the SAX parser.

	    // children ::= (choice | seq) ('?' | '*' | '+')?
	    // cp ::= (Name | choice | seq) ('?' | '*' | '+')?
	    // choice ::= '(' cp ('|' choice)* ')'
	    // seq ::= '(' cp (',' choice)* ')'

	    // interior nodes only
	    //   cp ::= name ...
	    if (parseBuf [startPos] != '('/*)*/) {
		boolean		done = false;
		do {
		    switch (c = parseBuf [nextPos]) {
			case '?': case '*': case '+':
			case '|': case ',':
			case /*(*/ ')':
			    done = true;
			    continue;
			default:
			    nextPos++;
			    continue;
		    }
		} while (!done);
		name = new String (parseBuf, startPos, nextPos - startPos);

	    // interior OR toplevel nodes
	    //   cp ::= choice ..
	    //   cp ::= seq ..
	    } else {
		// collect everything as a separate list, and merge it
		// into "this" later if we can (SEQUENCE or singleton)
		ChildrenRecognizer	first;
	       
		first = new ChildrenRecognizer (consumer, type);
		nextPos = first.populate (parseBuf, nextPos);
		c = parseBuf [nextPos++];

		if (c == ',' || c == '|') {
		    ChildrenRecognizer	current = first;
		    char		separator = c;
		    Vector		v = null;

		    if (separator == '|') {
			v = new Vector ();
			v.addElement (first);
		    }

		    do {
			ChildrenRecognizer link;

			link = new ChildrenRecognizer (consumer, type);
			nextPos = link.populate (parseBuf, nextPos);

			if (separator == ',') {
			    current.patchNext (link, null);
			    current = link;
			} else
			    v.addElement (link);

			c = parseBuf [nextPos++];
		    } while (c == separator);

		    // choice ... collect everything into one array.
		    if (separator == '|') {
			// assert v.size() > 1
			components = new Recognizer [v.size ()];
			for (int i = 0; i < components.length; i++) {
			    components [i] = (Recognizer)
				    v.elementAt (i);
			}
			// assert flags == 0

		    // sequence ... merge into "this" to be smaller.
		    } else
			copyIn (first);

		// treat singletons like one-node sequences.
		} else
		    copyIn (first);

		if (c != /*(*/ ')')
		    throw new RuntimeException ("corrupt content model");
	    }

	    //
	    // Arity is optional, and the root of all fun.  We keep the
	    // FSM state graph simple by only having NAME/SEQUENCE and
	    // CHOICE nodes (or EMPTY to terminate a model), easily
	    // evaluated.  So we rewrite each node that has arity, using
	    // those primitives.  We create loops here, if needed.
	    //
	    if (nextPos < parseBuf.length) {
		c = parseBuf [nextPos];
		if (c == '?' || c == '*' || c == '+') {
		    nextPos++;

		    // Rewrite 'zero-or-one' "?" arity to a CHOICE:
		    //   - SEQUENCE (clone, what's next)
		    //   - or, what's next
		    // Size cost: N --> N + 1
		    if (c == '?') {
			Recognizer		once = shallowClone ();

			components = new Recognizer [2];
			components [0] = once;
			// components [1] initted to null
			name = null;
			next = null;
			flags = 0;

			    
		    // Rewrite 'zero-or-more' "*" arity to a CHOICE.
		    //   - LOOP (clone, back to this CHOICE)
		    //   - or, what's next
		    // Size cost: N --> N + 1
		    } else if (c == '*') {
			ChildrenRecognizer	loop = shallowClone ();

			loop.patchNext (this, null);
			loop.flags |= F_LOOPNEXT;
			flags = F_LOOPHEAD;

			components = new Recognizer [2];
			components [0] = loop;
			// components [1] initted to null
			name = null;
			next = null;


		    // Rewrite 'one-or-more' "+" arity to a SEQUENCE.
		    // Basically (a)+ --> ((a),(a)*).
		    //   - this
		    //   - CHOICE
		    //	    * LOOP (clone, back to the CHOICE)
		    //	    * or, whatever's next
		    // Size cost: N --> 2N + 1
		    } else if (c == '+') {
			ChildrenRecognizer loop = deepClone ();
			ChildrenRecognizer choice;

			choice = new ChildrenRecognizer (consumer, type);
			loop.patchNext (choice, null);
			loop.flags |= F_LOOPNEXT;
			choice.flags = F_LOOPHEAD;

			choice.components = new Recognizer [2];
			choice.components [0] = loop;
			// choice.components [1] initted to null
			// choice.name, choice.next initted to null

			patchNext (choice, null);
		    }
		}
	    }

	    return nextPos;
	}

	// VC: Element Valid (second clause)
	boolean acceptCharacters ()
	    { return false; }

	// VC: Element Valid (second clause)
	Recognizer acceptElement (String type)
	throws SAXException
	{
	    // NAME/SEQUENCE
	    if (name != null) {
		if (name.equals (type))
		    return next;
		return null;
	    }

	    // CHOICE ... optionally reporting nondeterminism we
	    // run across.  we won't check out every transition
	    // for nondeterminism; only the ones we follow.
	    Recognizer	retval = null;

	    for (int i = 0; i < components.length; i++) {
		Recognizer temp = components [i].acceptElement (type);

		if (temp == null)
		    continue;
		else if (!warnNonDeterministic)
		    return temp;
		else if (retval == null)
		    retval = temp;
		else if (retval != temp)
		    consumer.error ("Content model " + this.type.model
			+ " is non-deterministic for " + type);
	    }
	    return retval;
	}

	// VC: Element Valid (second clause)
	boolean completed ()
	throws SAXException
	{
	    // expecting a specific element
	    if (name != null)
		return false;
	    
	    // choice, some sequences
	    for (int i = 0; i < components.length; i++) {
		if (components [i].completed ())
		    return true;
	    }

	    return false;
	}

/** /
	// FOR DEBUGGING ... flattens the graph for printing.

	public String toString ()
	{
	    StringBuffer buf = new StringBuffer ();

	    // only one set of loop labels can be generated
	    // at a time...
	    synchronized (ANY) {
		nodeCount = 0;

		toString (buf, new Hashtable ());
		return buf.toString ();
	    }
	}

	private void toString (StringBuffer buf, Hashtable table)
	{
	    // When we visit a node, label and count it.
	    // Nodes are never visited/counted more than once.
	    // For small models labels waste space, but if arity
	    // mappings were used the savings are substantial.
	    // (Plus, the output can be more readily understood.)
	    String temp = (String) table.get (this);

	    if (temp != null) {
		buf.append ('{');
		buf.append (temp);
		buf.append ('}');
		return;
	    } else {
		StringBuffer scratch = new StringBuffer (15);

		if ((flags & F_LOOPHEAD) != 0)
		    scratch.append ("loop");
		else
		    scratch.append ("node");
		scratch.append ('-');
		scratch.append (++nodeCount);
		temp = scratch.toString ();

		table.put (this, temp);
		buf.append ('[');
		buf.append (temp);
		buf.append (']');
		buf.append (':');
	    }

	    // NAME/SEQUENCE
	    if (name != null) {
		// n.b. some output encodings turn some name chars into '?'
		// e.g. with Japanese names and ASCII output
		buf.append (name);
		if (components != null)		// bug!
		    buf.append ('$');
		if (next == null)
		    buf.append (",*");
		else if (next instanceof EmptyRecognizer) // patch-to-next
		    buf.append (",{}");
		else if (next instanceof ChildrenRecognizer) {
		    buf.append (',');
		    ((ChildrenRecognizer)next).toString (buf, table);
		} else				// bug!
		    buf.append (",+");
		return;
	    }

	    // CHOICE
	    buf.append ("<");
	    for (int i = 0; i < components.length; i++) {
		if (i != 0)
		    buf.append ("|");
		if (components [i] instanceof EmptyRecognizer) {
		    buf.append ("{}");
		} else if (components [i] == null) {	// patch-to-next
		    buf.append ('*');
		} else {
		    ChildrenRecognizer r;

		    r = (ChildrenRecognizer) components [i];
		    r.toString (buf, table);
		}
	    }
	    buf.append (">");
	}
/**/
    }
}
