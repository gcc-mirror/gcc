/* EventFilter.java -- 
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

import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;

import org.xml.sax.*;
import org.xml.sax.ext.*;
import org.xml.sax.helpers.XMLFilterImpl;

import gnu.xml.aelfred2.ContentHandler2;

/**
 * A customizable event consumer, used to assemble various kinds of filters
 * using SAX handlers and an optional second consumer.  It can be constructed
 * in two ways: <ul>
 *
 *  <li> To serve as a passthrough, sending all events to a second consumer.
 *  The second consumer may be identified through {@link #getNext}.
 *
 *  <li> To serve as a dead end, with all handlers null;
 *  {@link #getNext} returns null.
 *
 * </ul>
 *
 * <p> Additionally, SAX handlers may be assigned, which completely replace
 * the "upstream" view (through {@link EventConsumer}) of handlers, initially
 * null or the "next" consumer provided to the constructor.  To make
 * it easier to build specialized filter classes, this class implements
 * all the standard SAX consumer handlers, and those implementations
 * delegate "downstream" to the consumer accessed by {@link #getNext}.
 *
 * <p> The simplest way to create a custom a filter class is to create a
 * subclass which overrides one or more handler interface methods.  The
 * constructor for that subclass then registers itself as a handler for
 * those interfaces using a call such as <em>setContentHandler(this)</em>,
 * so the "upstream" view of event delivery is modified from the state
 * established in the base class constructor.  That way,
 * the overridden methods intercept those event callbacks
 * as they go "downstream", and
 * all other event callbacks will pass events to any next consumer.
 * Overridden methods may invoke superclass methods (perhaps after modifying
 * parameters) if they wish to delegate such calls.  Such subclasses
 * should use {@link #getErrorHandler} to report errors using the
 * common error reporting mechanism.
 *
 * <p> Another important technique is to construct a filter consisting
 * of only a few specific types of handler.  For example, one could easily
 * prune out lexical events or various declarations by providing handlers
 * which don't pass those events downstream, or by providing null handlers.
 *
 * <hr />
 *
 * <p> This may be viewed as the consumer oriented analogue of the SAX2
 * {@link org.xml.sax.helpers.XMLFilterImpl XMLFilterImpl} class.
 * Key differences include: <ul>
 *
 *	<li> This fully separates consumer and producer roles:  it
 *	does not implement the producer side <em>XMLReader</em> or
 *	<em>EntityResolver</em> interfaces, so it can only be used
 *	in "push" mode (it has no <em>parse()</em> methods).
 *
 *	<li> "Extension" handlers are fully supported, enabling a
 *	richer set of application requirements.
 *	And it implements {@link EventConsumer}, which groups related
 *	consumer methods together, rather than leaving them separated.
 *
 *	<li> The chaining which is visible is "downstream" to the next
 *	consumer, not "upstream" to the preceding producer.
 *	It supports "fan-in", where
 *	a consumer can be fed by several producers.  (For "fan-out",
 *	see the {@link TeeConsumer} class.)
 *
 *	<li> Event chaining is set up differently.  It is intended to
 *	work "upstream" from terminus towards producer, during filter
 *	construction, as described above.
 *	This is part of an early binding model:
 *	events don't need to pass through stages which ignore them.
 *
 *	<li> ErrorHandler support is separated, on the grounds that
 *	pipeline stages need to share the same error handling policy.
 *	For the same reason, error handler setup goes "downstream":
 *	when error handlers get set, they are passed to subsequent
 *	consumers.
 *
 *	</ul>
 *
 * <p> The {@link #chainTo chainTo()} convenience routine supports chaining to
 * an XMLFilterImpl, in its role as a limited functionality event
 * consumer.  Its event producer role ({@link XMLFilter}) is ignored.
 *
 * <hr />
 *
 * <p> The {@link #bind bind()} routine may be used associate event pipelines
 * with any kind of {@link XMLReader} that will produce the events.
 * Such pipelines don't necessarily need to have any members which are
 * implemented using this class.  That routine has some intelligence
 * which supports automatic changes to parser feature flags, letting
 * event piplines become largely independent of the particular feature
 * sets of parsers.
 *
 * @author David Brownell
 */
public class EventFilter
    implements EventConsumer, ContentHandler2, DTDHandler,
	    LexicalHandler, DeclHandler
{
    // SAX handlers
    private ContentHandler		docHandler, docNext;
    private DTDHandler			dtdHandler, dtdNext;
    private LexicalHandler		lexHandler, lexNext;
    private DeclHandler			declHandler, declNext;
    // and ideally, one more for the stuff SAX2 doesn't show

    private Locator			locator;
    private EventConsumer		next;
    private ErrorHandler		errHandler;

    
    /** SAX2 URI prefix for standard feature flags. */
    public static final String		FEATURE_URI
	= "http://xml.org/sax/features/";
    /** SAX2 URI prefix for standard properties (mostly for handlers). */
    public static final String		PROPERTY_URI
	= "http://xml.org/sax/properties/";

    /** SAX2 property identifier for {@link DeclHandler} events */
    public static final String		DECL_HANDLER
	= PROPERTY_URI + "declaration-handler";
    /** SAX2 property identifier for {@link LexicalHandler} events */
    public static final String		LEXICAL_HANDLER
	= PROPERTY_URI + "lexical-handler";
    
    //
    // These class objects will be null if the relevant class isn't linked.
    // Small configurations (pJava and some kinds of embedded systems) need
    // to facilitate smaller executables.  So "instanceof" is undesirable
    // when bind() sees if it can remove some stages.
    //
    // SECURITY NOTE:  assuming all these classes are part of the same sealed
    // package, there's no problem saving these in the instance of this class
    // that's associated with "this" class loader.  But that wouldn't be true
    // for classes in another package.
    //
    private static boolean		loaded;
    private static Class		nsClass;
    private static Class		validClass;
    private static Class		wfClass;
    private static Class		xincClass;

    static ClassLoader getClassLoader ()
    {
        Method m = null;

        try {
            m = Thread.class.getMethod("getContextClassLoader", null);
        } catch (NoSuchMethodException e) {
            // Assume that we are running JDK 1.1, use the current ClassLoader
            return EventFilter.class.getClassLoader();
        }

        try {
            return (ClassLoader) m.invoke(Thread.currentThread(), null);
        } catch (IllegalAccessException e) {
            // assert(false)
            throw new UnknownError(e.getMessage());
        } catch (InvocationTargetException e) {
            // assert(e.getTargetException() instanceof SecurityException)
            throw new UnknownError(e.getMessage());
        }
    }

    static Class loadClass (ClassLoader classLoader, String className)
    {
	try {
	    if (classLoader == null)
		return Class.forName(className);
	    else
		return classLoader.loadClass(className);
	} catch (Exception e) {
	    return null;
	}
    }

    static private void loadClasses ()
    {
	ClassLoader	loader = getClassLoader ();

	nsClass = loadClass (loader, "gnu.xml.pipeline.NSFilter");
	validClass = loadClass (loader, "gnu.xml.pipeline.ValidationConsumer");
	wfClass = loadClass (loader, "gnu.xml.pipeline.WellFormednessFilter");
	xincClass = loadClass (loader, "gnu.xml.pipeline.XIncludeFilter");
	loaded = true;
    }


    /**
     * Binds the standard SAX2 handlers from the specified consumer
     * pipeline to the specified producer.  These handlers include the core
     * {@link ContentHandler} and {@link DTDHandler}, plus the extension
     * {@link DeclHandler} and {@link LexicalHandler}.  Any additional
     * application-specific handlers need to be bound separately.
     * The {@link ErrorHandler} is handled differently:  the producer's
     * error handler is passed through to the consumer pipeline.
     * The producer is told to include namespace prefix information if it
     * can, since many pipeline stages need that Infoset information to
     * work well.
     *
     * <p> At the head of the pipeline, certain standard event filters are
     * recognized and handled specially.  This facilitates construction
     * of processing pipelines that work regardless of the capabilities
     * of the XMLReader implementation in use; for example, it permits
     * validating output of a {@link gnu.xml.util.DomParser}. <ul>
     *
     *	<li> {@link NSFilter} will be removed if the producer can be
     *	told not to discard namespace data, using the "namespace-prefixes"
     *	feature flag.
     *
     *	<li> {@link ValidationConsumer} will be removed if the producer
     *	can be told to validate, using the "validation" feature flag.
     *
     *	<li> {@link WellFormednessFilter} is always removed, on the
     *	grounds that no XMLReader is permitted to producee malformed
     *	event streams and this would just be processing overhead.
     *
     *	<li> {@link XIncludeFilter} stops the special handling, except
     *	that it's told about the "namespace-prefixes" feature of the
     *	event producer so that the event stream is internally consistent.
     *
     *	<li> The first consumer which is not one of those classes stops
     *	such special handling.  This means that if you want to force
     *	one of those filters to be used, you could just precede it with
     *	an instance of {@link EventFilter} configured as a pass-through.
     *	You might need to do that if you are using an {@link NSFilter}
     *	subclass to fix names found in attributes or character data.
     *
     *	</ul>
     *
     * <p> Other than that, this method works with any kind of event consumer,
     * not just event filters.  Note that in all cases, the standard handlers
     * are assigned; any previous handler assignments for the handler will
     * be overridden.
     *
     * @param producer will deliver events to the specified consumer 
     * @param consumer pipeline supplying event handlers to be associated
     *	with the producer (may not be null)
     */
    public static void bind (XMLReader producer, EventConsumer consumer)
    {
	Class	klass = null;
	boolean	prefixes;

	if (!loaded)
	    loadClasses ();

	// DOM building, printing, layered validation, and other
	// things don't work well when prefix info is discarded.
	// Include it by default, whenever possible.
	try {
	    producer.setFeature (FEATURE_URI + "namespace-prefixes",
		true);
	    prefixes = true;
	} catch (SAXException e) {
	    prefixes = false;
	}

	// NOTE:  This loop doesn't use "instanceof", since that
	// would prevent compiling/linking without those classes
	// being present.
	while (consumer != null) {
	    klass = consumer.getClass ();

	    // we might have already changed this problematic SAX2 default.
	    if (nsClass != null && nsClass.isAssignableFrom (klass)) {
		if (!prefixes)
		    break;
		consumer = ((EventFilter)consumer).getNext ();

	    // the parser _might_ do DTD validation by default ...
	    // if not, maybe we can change this setting.
	    } else if (validClass != null
		    && validClass.isAssignableFrom (klass)) {
		try {
		    producer.setFeature (FEATURE_URI + "validation",
			true);
		    consumer = ((ValidationConsumer)consumer).getNext ();
		} catch (SAXException e) {
		    break;
		}

	    // parsers are required not to have such bugs
	    } else if (wfClass != null && wfClass.isAssignableFrom (klass)) {
		consumer = ((WellFormednessFilter)consumer).getNext ();

	    // stop on the first pipeline stage we can't remove
	    } else
		break;
	    
	    if (consumer == null)
		klass = null;
	}

	// the actual setting here doesn't matter as much
	// as that producer and consumer agree
	if (xincClass != null && klass != null
		&& xincClass.isAssignableFrom (klass))
	    ((XIncludeFilter)consumer).setSavingPrefixes (prefixes);

	// Some SAX parsers can't handle null handlers -- bleech
	DefaultHandler2	h = new DefaultHandler2 ();

	if (consumer != null && consumer.getContentHandler () != null)
	    producer.setContentHandler (consumer.getContentHandler ());
	else
	    producer.setContentHandler (h);
	if (consumer != null && consumer.getDTDHandler () != null)
	    producer.setDTDHandler (consumer.getDTDHandler ());
	else
	    producer.setDTDHandler (h);

	try {
	    Object	dh;
	    
	    if (consumer != null)
		dh = consumer.getProperty (DECL_HANDLER);
	    else
		dh = null;
	    if (dh == null)
		dh = h;
	    producer.setProperty (DECL_HANDLER, dh);
	} catch (Exception e) { /* ignore */ }
	try {
	    Object	lh;
	    
	    if (consumer != null)
		lh = consumer.getProperty (LEXICAL_HANDLER);
	    else
		lh = null;
	    if (lh == null)
		lh = h;
	    producer.setProperty (LEXICAL_HANDLER, lh);
	} catch (Exception e) { /* ignore */ }

	// this binding goes the other way around
	if (producer.getErrorHandler () == null)
	    producer.setErrorHandler (h);
	if (consumer != null)
	    consumer.setErrorHandler (producer.getErrorHandler ());
    }
    
    /**
     * Initializes all handlers to null.
     */
	// constructor used by PipelineFactory
    public EventFilter () { }


    /**
     * Handlers that are not otherwise set will default to those from
     * the specified consumer, making it easy to pass events through.
     * If the consumer is null, all handlers are initialzed to null.
     */
	// constructor used by PipelineFactory
    public EventFilter (EventConsumer consumer)
    {
	if (consumer == null)
	    return;

	next = consumer;

	// We delegate through the "xxNext" handlers, and
	// report the "xxHandler" ones on our input side.

	// Normally a subclass would both override handler
	// methods and register itself as the "xxHandler".

	docHandler = docNext = consumer.getContentHandler ();
	dtdHandler = dtdNext = consumer.getDTDHandler ();
	try {
	    declHandler = declNext = (DeclHandler)
		    consumer.getProperty (DECL_HANDLER);
	} catch (SAXException e) { /* leave value null */ }
	try {
	    lexHandler = lexNext = (LexicalHandler)
		    consumer.getProperty (LEXICAL_HANDLER);
	} catch (SAXException e) { /* leave value null */ }
    }

    /**
     * Treats the XMLFilterImpl as a limited functionality event consumer,
     * by arranging to deliver events to it; this lets such classes be
     * "wrapped" as pipeline stages.
     *
     * <p> <em>Upstream Event Setup:</em>
     * If no handlers have been assigned to this EventFilter, then the
     * handlers from specified XMLFilterImpl are returned from this
     * {@link EventConsumer}: the XMLFilterImpl is just "wrapped".
     * Otherwise the specified handlers will be returned.
     *
     * <p> <em>Downstream Event Setup:</em>
     * Subclasses may chain event delivery to the specified XMLFilterImpl
     * by invoking the appropiate superclass methods,
     * as if their constructor passed a "next" EventConsumer to the
     * constructor for this class.
     * If this EventFilter has an ErrorHandler, it is assigned as
     * the error handler for the XMLFilterImpl, just as would be
     * done for a next stage implementing {@link EventConsumer}.
     *
     * @param next the next downstream component of the pipeline.
     * @exception IllegalStateException if the "next" consumer has
     *	already been set through the constructor.
     */
    public void chainTo (XMLFilterImpl next)
    {
	if (this.next != null)
	    throw new IllegalStateException ();

	docNext = next.getContentHandler ();
	if (docHandler == null)
	    docHandler = docNext;
	dtdNext = next.getDTDHandler ();
	if (dtdHandler == null)
	    dtdHandler = dtdNext;

	try {
	    declNext = (DeclHandler) next.getProperty (DECL_HANDLER);
	    if (declHandler == null)
		declHandler = declNext;
	} catch (SAXException e) { /* leave value null */ }
	try {
	    lexNext = (LexicalHandler) next.getProperty (LEXICAL_HANDLER);
	    if (lexHandler == null)
		lexHandler = lexNext;
	} catch (SAXException e) { /* leave value null */ }

	if (errHandler != null)
	    next.setErrorHandler (errHandler);
    }

    /**
     * Records the error handler that should be used by this stage, and
     * passes it "downstream" to any subsequent stage.
     */
    final public void setErrorHandler (ErrorHandler handler)
    {
	errHandler = handler;
	if (next != null)
	    next.setErrorHandler (handler);
    }

    /**
     * Returns the error handler assigned this filter stage, or null
     * if no such assigment has been made.
     */
    final public ErrorHandler getErrorHandler ()
    {
	return errHandler;
    }


    /**
     * Returns the next event consumer in sequence; or null if there
     * is no such handler.
     */
    final public EventConsumer getNext ()
	{ return next; }


    /**
     * Assigns the content handler to use; a null handler indicates
     * that these events will not be forwarded.
     * This overrides the previous settting for this handler, which was
     * probably pointed to the next consumer by the base class constructor.
     */
    final public void setContentHandler (ContentHandler h)
    {
	docHandler = h;
    }

    /** Returns the content handler being used. */
    final public ContentHandler getContentHandler ()
    {
	return docHandler;
    }

    /**
     * Assigns the DTD handler to use; a null handler indicates
     * that these events will not be forwarded.
     * This overrides the previous settting for this handler, which was
     * probably pointed to the next consumer by the base class constructor.
     */
    final public void setDTDHandler (DTDHandler h)
	{ dtdHandler = h; }

    /** Returns the dtd handler being used. */
    final public DTDHandler getDTDHandler ()
    {
	return dtdHandler;
    }

    /**
     * Stores the property, normally a handler; a null handler indicates
     * that these events will not be forwarded.
     * This overrides the previous handler settting, which was probably
     * pointed to the next consumer by the base class constructor.
     */
    final public void setProperty (String id, Object o)
    throws SAXNotRecognizedException, SAXNotSupportedException
    {
	try {
	    Object	value = getProperty (id);

	    if (value == o)
		return;
	    if (DECL_HANDLER.equals (id)) {
		declHandler = (DeclHandler) o;
		return;
	    }
	    if (LEXICAL_HANDLER.equals (id)) {
		lexHandler = (LexicalHandler) o;
		return;
	    }
	    throw new SAXNotSupportedException (id);

	} catch (ClassCastException e) {
	    throw new SAXNotSupportedException (id);
	}
    }

    /** Retrieves a property of unknown intent (usually a handler) */
    final public Object getProperty (String id)
    throws SAXNotRecognizedException
    {
	if (DECL_HANDLER.equals (id)) 
	    return declHandler;
	if (LEXICAL_HANDLER.equals (id))
	    return lexHandler;

	throw new SAXNotRecognizedException (id);
    }

    /**
     * Returns any locator provided to the next consumer, if this class
     * (or a subclass) is handling {@link ContentHandler } events.
     */
    public Locator getDocumentLocator ()
	{ return locator; }


    // CONTENT HANDLER DELEGATIONS

    /** <b>SAX2:</b> passes this callback to the next consumer, if any */
    public void setDocumentLocator (Locator locator)
    {
	this.locator = locator;
	if (docNext != null)
	    docNext.setDocumentLocator (locator);
    }

    /** <b>SAX2:</b> passes this callback to the next consumer, if any */
    public void startDocument () throws SAXException
    {
	if (docNext != null)
	    docNext.startDocument ();
    }

    public void xmlDecl(String version, String encoding, boolean standalone,
                        String inputEncoding)
      throws SAXException
    {
      if (docNext != null && docNext instanceof ContentHandler2)
        {
          ((ContentHandler2) docNext).xmlDecl(version, encoding, standalone,
                                              inputEncoding);
        }
    }

    /** <b>SAX2:</b> passes this callback to the next consumer, if any */
    public void skippedEntity (String name) throws SAXException
    {
	if (docNext != null)
	    docNext.skippedEntity (name);
    }

    /** <b>SAX2:</b> passes this callback to the next consumer, if any */
    public void processingInstruction (String target, String data)
    throws SAXException
    {
	if (docNext != null)
	    docNext.processingInstruction (target, data);
    }

    /** <b>SAX2:</b> passes this callback to the next consumer, if any */
    public void characters (char ch [], int start, int length)
    throws SAXException
    {
	if (docNext != null)
	    docNext.characters (ch, start, length);
    }

    /** <b>SAX2:</b> passes this callback to the next consumer, if any */
    public void ignorableWhitespace (char ch [], int start, int length)
    throws SAXException
    {
	if (docNext != null)
	    docNext.ignorableWhitespace (ch, start, length);
    }

    /** <b>SAX2:</b> passes this callback to the next consumer, if any */
    public void startPrefixMapping (String prefix, String uri)
    throws SAXException
    {
	if (docNext != null)
	    docNext.startPrefixMapping (prefix, uri);
    }

    /** <b>SAX2:</b> passes this callback to the next consumer, if any */
    public void startElement (
	String uri, String localName,
	String qName, Attributes atts
    ) throws SAXException
    {
	if (docNext != null)
	    docNext.startElement (uri, localName, qName, atts);
    }

    /** <b>SAX2:</b> passes this callback to the next consumer, if any */
    public void endElement (String uri, String localName, String qName)
    throws SAXException
    {
	if (docNext != null)
	    docNext.endElement (uri, localName, qName);
    }

    /** <b>SAX2:</b> passes this callback to the next consumer, if any */
    public void endPrefixMapping (String prefix) throws SAXException
    {
	if (docNext != null)
	    docNext.endPrefixMapping (prefix);
    }

    /** <b>SAX2:</b> passes this callback to the next consumer, if any */
    public void endDocument () throws SAXException
    {
	if (docNext != null)
	    docNext.endDocument ();
	locator = null;
    }


    // DTD HANDLER DELEGATIONS
    
    /** <b>SAX1:</b> passes this callback to the next consumer, if any */
    public void unparsedEntityDecl (
	String name,
	String publicId,
	String systemId,
	String notationName
    ) throws SAXException
    {
	if (dtdNext != null)
	    dtdNext.unparsedEntityDecl (name, publicId, systemId, notationName);
    }
    
    /** <b>SAX1:</b> passes this callback to the next consumer, if any */
    public void notationDecl (String name, String publicId, String systemId)
    throws SAXException
    {
	if (dtdNext != null)
	    dtdNext.notationDecl (name, publicId, systemId);
    }
    

    // LEXICAL HANDLER DELEGATIONS

    /** <b>SAX2:</b> passes this callback to the next consumer, if any */
    public void startDTD (String name, String publicId, String systemId)
    throws SAXException
    {
	if (lexNext != null)
	    lexNext.startDTD (name, publicId, systemId);
    }

    /** <b>SAX2:</b> passes this callback to the next consumer, if any */
    public void endDTD ()
    throws SAXException
    {
	if (lexNext != null)
	    lexNext.endDTD ();
    }

    /** <b>SAX2:</b> passes this callback to the next consumer, if any */
    public void comment (char ch [], int start, int length)
    throws SAXException
    {
	if (lexNext != null)
	    lexNext.comment (ch, start, length);
    }

    /** <b>SAX2:</b> passes this callback to the next consumer, if any */
    public void startCDATA ()
    throws SAXException
    {
	if (lexNext != null)
	    lexNext.startCDATA ();
    }

    /** <b>SAX2:</b> passes this callback to the next consumer, if any */
    public void endCDATA ()
    throws SAXException
    {
	if (lexNext != null)
	    lexNext.endCDATA ();
    }

    /**
     * <b>SAX2:</b> passes this callback to the next consumer, if any.
     */
    public void startEntity (String name)
    throws SAXException
    {
	if (lexNext != null)
	    lexNext.startEntity (name);
    }

    /**
     * <b>SAX2:</b> passes this callback to the next consumer, if any.
     */
    public void endEntity (String name)
    throws SAXException
    {
	if (lexNext != null)
	    lexNext.endEntity (name);
    }
    

    // DECLARATION HANDLER DELEGATIONS


    /** <b>SAX2:</b> passes this callback to the next consumer, if any */
    public void elementDecl (String name, String model)
    throws SAXException
    {
	if (declNext != null)
	    declNext.elementDecl (name, model);
    }

    /** <b>SAX2:</b> passes this callback to the next consumer, if any */
    public void attributeDecl (String eName, String aName,
	    String type, String mode, String value)
    throws SAXException
    {
	if (declNext != null)
	    declNext.attributeDecl (eName, aName, type, mode, value);
    }

    /** <b>SAX2:</b> passes this callback to the next consumer, if any */
    public void externalEntityDecl (String name,
    	String publicId, String systemId)
    throws SAXException
    {
	if (declNext != null)
	    declNext.externalEntityDecl (name, publicId, systemId);
    }

    /** <b>SAX2:</b> passes this callback to the next consumer, if any */
    public void internalEntityDecl (String name, String value)
    throws SAXException
    {
	if (declNext != null)
	    declNext.internalEntityDecl (name, value);
    }
}
