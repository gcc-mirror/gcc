/* gnu.java.beans.PersistenceParser
   Copyright (C) 2004, 2005 Free Software Foundation, Inc.

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

package gnu.java.beans.decoder;

import java.beans.ExceptionListener;
import java.beans.XMLDecoder;
import java.io.IOException;
import java.io.InputStream;
import java.util.HashMap;
import java.util.Iterator;
import java.util.LinkedList;
import java.util.List;

import javax.xml.parsers.ParserConfigurationException;
import javax.xml.parsers.SAXParser;
import javax.xml.parsers.SAXParserFactory;

import org.xml.sax.Attributes;
import org.xml.sax.SAXException;
import org.xml.sax.helpers.DefaultHandler;

/** The PersistenceParser parses an XML data stream and delegates actions to ElementHandler
 * instances. The parser catches and recovers from all exception which reside from wrong usage
 * of attributes and tags.
 *
 * @author Robert Schuster
 */
public class PersistenceParser extends DefaultHandler implements Context
{
	/** The ExceptionListener instance which is informed of non-critical parsing exceptions.
	 */
	private ExceptionListener exceptionListener;

	/** When an element was not usable all elements inside it should be skipped.
	 * This is done by skipping startElement() and endElement() invocations whenever
	 * this value is above 0.
	 */
	private int skipElement;

	/** Stores the Creator instances which can instantiate the appropriate handler implementation
	 * for a given element.
	 */
	private HashMap handlerCreators = new HashMap();

	/** Denotes the current ElementHandler. To avoid checking for null-values it is pre-assigned
	 * with a DummyHandler instance which must not be used but acts as a root element.
	 */
	private ElementHandler currentHandler;

	/** The real root element that stores all objects created during parsing.
	 * Package-private to avoid an accessor method.
	 */
	JavaHandler javaHandler;

	/** Stores the decoded objects. */
	private List objects = new LinkedList();

	/** The XMLDecoder instance that started this PersistenceParser */
	private XMLDecoder decoder;

	/** Creates a PersistenceParser which reads XML data from the given InputStream, reports
	 * exceptions to ExceptionListener instance, stores resulting object in the DecoderContext
	 * and uses the given ClassLoader to resolve classes.
	 *
	 * @param inputStream
	 * @param exceptionListener
	 * @param decoderContext
	 * @param cl
	 */
	public PersistenceParser(
		InputStream inputStream,
		ExceptionListener exceptionListener,
		ClassLoader cl,
		XMLDecoder decoder)
	{

		this.exceptionListener = exceptionListener;
		this.decoder = decoder;

		DummyHandler dummyHandler = new DummyHandler();
		currentHandler = dummyHandler;
		javaHandler = new JavaHandler(dummyHandler, this, cl);

		SAXParserFactory factory = SAXParserFactory.newInstance();

		SAXParser parser;
		try
		{
			parser = factory.newSAXParser();
		}
		catch (ParserConfigurationException pce)
		{
			// should not happen when a parser is available because we did
			// not request any requirements on the XML parser
			throw (InternalError) new InternalError(
				"No SAX Parser available.").initCause(
				pce);
		}
		catch (SAXException saxe)
		{
			// should not happen when a parser is available because we did
			// not request any requirements on the XML parser
			throw (InternalError) new InternalError(
				"No SAX Parser available.").initCause(
				saxe);
		}

		// prepares a map of Creator instances which can instantiate a handler which is
		// appropriate for the tag that is used as a key for the Creator
		handlerCreators.put("java", new JavaHandlerCreator());

		// calls methods (properties), constructors, access fields
		handlerCreators.put("object", new ObjectHandlerCreator());
		handlerCreators.put("void", new VoidHandlerCreator());

		handlerCreators.put("array", new ArrayHandlerCreator());

		// these handler directly create an Object (or null)
		handlerCreators.put("class", new ClassHandlerCreator());
		handlerCreators.put("null", new NullHandlerCreator());

		handlerCreators.put("char", new CharHandlerCreator());
		handlerCreators.put("string", new StringHandlerCreator());
		handlerCreators.put("boolean", new BooleanHandlerCreator());
		handlerCreators.put("byte", new ByteHandlerCreator());
		handlerCreators.put("short", new ShortHandlerCreator());
		handlerCreators.put("int", new IntHandlerCreator());
		handlerCreators.put("long", new LongHandlerCreator());
		handlerCreators.put("float", new FloatHandlerCreator());
		handlerCreators.put("double", new DoubleHandlerCreator());

		// parses the data and sends all exceptions to the ExceptionListener
		try
		{
			parser.parse(inputStream, this);
		}
		catch (SAXException saxe)
		{
			exceptionListener.exceptionThrown(
				new IllegalArgumentException("XML data not well-formed."));
		}
		catch (IOException ioe)
		{
			exceptionListener.exceptionThrown(ioe);
		}
	}

	public void startElement(
		String uri,
		String localName,
		String qName,
		Attributes attributes)
		throws SAXException
	{
		/* The element is skipped if
		 * a) the current handler has already failed or a previous error occured
		 * which makes all children obsolete
		 */
		if (currentHandler.hasFailed() || skipElement > 0)
		{
			exceptionListener.exceptionThrown(
				new IllegalArgumentException(
					"Element unusable due to previous error: " + qName));

			skipElement++;

			return;
		}

		/* b) Subelements are not allowed within the current ElementHandler.
		 */
		if (!currentHandler.isSubelementAllowed(qName))
		{
			exceptionListener.exceptionThrown(
				new IllegalArgumentException(
					"Element is not allowed here: " + qName));

			skipElement++;

			return;
		}

		/* c) The tag name is not a key in the map of Creator instances. This means that
		* either the XML data is of a newer version or simply contains a miss-spelled element.
		*/
		if (!handlerCreators.containsKey(qName))
		{
			exceptionListener.exceptionThrown(
				new IllegalArgumentException(
					"Element unusable because tag is unknown: " + qName));

			skipElement++;

			return;
		}

		// creates a new handler for the new element
		AbstractElementHandler handler =
			((Creator) handlerCreators.get(qName)).createHandler(
				currentHandler);

		// makes it the current handler to receive character data
		currentHandler = handler;

		// starts the handler
		currentHandler.start(attributes, exceptionListener);
	}

	public void endElement(String uri, String localName, String qName)
		throws SAXException
	{
		// skips processing the current handler if we are parsing an element
		// which was marked invalid (in startElement() ) 
		if (skipElement > 0)
		{
			skipElement--;
			return;
		}

		// invokes the handler's finishing method
		currentHandler.end(exceptionListener);

		// removes the current handler and reactivates its parent
		currentHandler = currentHandler.getParent();
	}

	/** Transfers character data to the current handler
	 */
	public void characters(char[] ch, int start, int length)
		throws SAXException
	{
		// prevents sending character data of invalid elements
		if (skipElement > 0)
			return;

		currentHandler.characters(ch, start, length);
	}

	/** Creator interface provided a mechanism to instantiate ElementHandler instances
	 * for the appropriate tag.
	 *
	 * @author Robert Schuster
	 */
	interface Creator
	{
		/** Creates an ElementHandler instance using the given ElementHandler as its parent.
		 *
		 * @param parent The parent ElementHandler of the result.
		 * @return A new ElementHandler instance.
		 */
		AbstractElementHandler createHandler(ElementHandler parent);
	}

	class BooleanHandlerCreator implements Creator
	{
		public AbstractElementHandler createHandler(ElementHandler parent)
		{
			return new BooleanHandler(parent);
		}
	}

	class ByteHandlerCreator implements Creator
	{
		public AbstractElementHandler createHandler(ElementHandler parent)
		{
			return new ByteHandler(parent);
		}
	}

	class ShortHandlerCreator implements Creator
	{
		public AbstractElementHandler createHandler(ElementHandler parent)
		{
			return new ShortHandler(parent);
		}
	}

	class IntHandlerCreator implements Creator
	{
		public AbstractElementHandler createHandler(ElementHandler parent)
		{
			return new IntHandler(parent);
		}
	}

	class LongHandlerCreator implements Creator
	{
		public AbstractElementHandler createHandler(ElementHandler parent)
		{
			return new LongHandler(parent);
		}
	}

	class FloatHandlerCreator implements Creator
	{
		public AbstractElementHandler createHandler(ElementHandler parent)
		{
			return new FloatHandler(parent);
		}
	}

	class DoubleHandlerCreator implements Creator
	{
		public AbstractElementHandler createHandler(ElementHandler parent)
		{
			return new DoubleHandler(parent);
		}
	}

	class CharHandlerCreator implements Creator
	{
		public AbstractElementHandler createHandler(ElementHandler parent)
		{
			return new CharHandler(parent);
		}
	}

	class StringHandlerCreator implements Creator
	{
		public AbstractElementHandler createHandler(ElementHandler parent)
		{
			return new StringHandler(parent);
		}
	}

	class JavaHandlerCreator implements Creator
	{
		public AbstractElementHandler createHandler(ElementHandler parent)
		{
			return javaHandler;
		}
	}

	class ObjectHandlerCreator implements Creator
	{
		public AbstractElementHandler createHandler(ElementHandler parent)
		{
			return new ObjectHandler(parent);
		}
	}

	class VoidHandlerCreator implements Creator
	{
		public AbstractElementHandler createHandler(ElementHandler parent)
		{
			return new VoidHandler(parent);
		}
	}

	class ClassHandlerCreator implements Creator
	{
		public AbstractElementHandler createHandler(ElementHandler parent)
		{
			return new ClassHandler(parent);
		}
	}

	class NullHandlerCreator implements Creator
	{
		public AbstractElementHandler createHandler(ElementHandler parent)
		{
			return new NullHandler(parent);
		}
	}

	class ArrayHandlerCreator implements Creator
	{
		public AbstractElementHandler createHandler(ElementHandler parent)
		{
			return new ArrayHandler(parent);
		}
	}

	/** Adds a decoded object to the Context. */
	public void addParameterObject(Object o) throws AssemblyException
	{
		objects.add(o);
	}

	public void notifyStatement(Context outerContext) throws AssemblyException
	{
		// can be ignored because theis Context does not react to statement and expressions
		// differently
	}

	public Object endContext(Context outerContext) throws AssemblyException
	{
		return null;
	}

	public boolean subContextFailed()
	{
		// failing of subcontexts is no problem for the mother of all contexts
		return false;
	}

	public void set(int index, Object o) throws AssemblyException
	{
		// not supported
		throw new AssemblyException(
			new IllegalArgumentException("Set method is not allowed in decoder context."));
	}

	public Object get(int index) throws AssemblyException
	{
		// not supported
		throw new AssemblyException(
			new IllegalArgumentException("Get method is not allowed in decoder context."));
	}

	public Object getResult()
	{
		// returns the XMLDecoder instance which is requested by child contexts this way.
		// That is needed to invoke methods on the decoder.
		return decoder;
	}

	public void setId(String id)
	{
		exceptionListener.exceptionThrown(new IllegalArgumentException("id attribute is not allowed for <java> tag."));
	}

	public String getId()
	{
		// appears to have no id
		return null;
	}

	public boolean isStatement()
	{
		// this context is a statement by definition because it never returns anything to a parent because
		// there is no such parent (DummyContext does not count!)
		return true;
	}

	public void setStatement(boolean b)
	{
		// ignores that because this Context is always a statement
	}

	/** Returns an Iterator instance which returns the decoded objects.
	 * 
	 * This method is used by the XMLDecoder directly. 
	 */ 
	public Iterator iterator()
	{
		return objects.iterator();
	}

}
