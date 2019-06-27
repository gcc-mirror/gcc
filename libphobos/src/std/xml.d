// Written in the D programming language.

/**
$(RED Warning: This module is considered out-dated and not up to Phobos'
      current standards. It will remain until we have a suitable replacement,
      but be aware that it will not remain long term.)

Classes and functions for creating and parsing XML

The basic architecture of this module is that there are standalone functions,
classes for constructing an XML document from scratch (Tag, Element and
Document), and also classes for parsing a pre-existing XML file (ElementParser
and DocumentParser). The parsing classes <i>may</i> be used to build a
Document, but that is not their primary purpose. The handling capabilities of
DocumentParser and ElementParser are sufficiently customizable that you can
make them do pretty much whatever you want.

Example: This example creates a DOM (Document Object Model) tree
    from an XML file.
------------------------------------------------------------------------------
import std.xml;
import std.stdio;
import std.string;
import std.file;

// books.xml is used in various samples throughout the Microsoft XML Core
// Services (MSXML) SDK.
//
// See http://msdn2.microsoft.com/en-us/library/ms762271(VS.85).aspx

void main()
{
    string s = cast(string) std.file.read("books.xml");

    // Check for well-formedness
    check(s);

    // Make a DOM tree
    auto doc = new Document(s);

    // Plain-print it
    writeln(doc);
}
------------------------------------------------------------------------------

Example: This example does much the same thing, except that the file is
    deconstructed and reconstructed by hand. This is more work, but the
    techniques involved offer vastly more power.
------------------------------------------------------------------------------
import std.xml;
import std.stdio;
import std.string;

struct Book
{
    string id;
    string author;
    string title;
    string genre;
    string price;
    string pubDate;
    string description;
}

void main()
{
    string s = cast(string) std.file.read("books.xml");

    // Check for well-formedness
    check(s);

    // Take it apart
    Book[] books;

    auto xml = new DocumentParser(s);
    xml.onStartTag["book"] = (ElementParser xml)
    {
        Book book;
        book.id = xml.tag.attr["id"];

        xml.onEndTag["author"]       = (in Element e) { book.author      = e.text(); };
        xml.onEndTag["title"]        = (in Element e) { book.title       = e.text(); };
        xml.onEndTag["genre"]        = (in Element e) { book.genre       = e.text(); };
        xml.onEndTag["price"]        = (in Element e) { book.price       = e.text(); };
        xml.onEndTag["publish-date"] = (in Element e) { book.pubDate     = e.text(); };
        xml.onEndTag["description"]  = (in Element e) { book.description = e.text(); };

        xml.parse();

        books ~= book;
    };
    xml.parse();

    // Put it back together again;
    auto doc = new Document(new Tag("catalog"));
    foreach (book;books)
    {
        auto element = new Element("book");
        element.tag.attr["id"] = book.id;

        element ~= new Element("author",      book.author);
        element ~= new Element("title",       book.title);
        element ~= new Element("genre",       book.genre);
        element ~= new Element("price",       book.price);
        element ~= new Element("publish-date",book.pubDate);
        element ~= new Element("description", book.description);

        doc ~= element;
    }

    // Pretty-print it
    writefln(join(doc.pretty(3),"\n"));
}
-------------------------------------------------------------------------------
Copyright: Copyright Janice Caron 2008 - 2009.
License:   $(HTTP www.boost.org/LICENSE_1_0.txt, Boost License 1.0).
Authors:   Janice Caron
Source:    $(PHOBOSSRC std/_xml.d)
*/
/*
         Copyright Janice Caron 2008 - 2009.
Distributed under the Boost Software License, Version 1.0.
   (See accompanying file LICENSE_1_0.txt or copy at
         http://www.boost.org/LICENSE_1_0.txt)
*/
module std.xml;

enum cdata = "<![CDATA[";

/**
 * Returns true if the character is a character according to the XML standard
 *
 * Standards: $(LINK2 http://www.w3.org/TR/1998/REC-xml-19980210, XML 1.0)
 *
 * Params:
 *    c = the character to be tested
 */
bool isChar(dchar c) @safe @nogc pure nothrow // rule 2
{
    if (c <= 0xD7FF)
    {
        if (c >= 0x20)
            return true;
        switch (c)
        {
        case 0xA:
        case 0x9:
        case 0xD:
            return true;
        default:
            return false;
        }
    }
    else if (0xE000 <= c && c <= 0x10FFFF)
    {
        if ((c & 0x1FFFFE) != 0xFFFE) // U+FFFE and U+FFFF
            return true;
    }
    return false;
}

@safe @nogc nothrow pure unittest
{
    assert(!isChar(cast(dchar) 0x8));
    assert( isChar(cast(dchar) 0x9));
    assert( isChar(cast(dchar) 0xA));
    assert(!isChar(cast(dchar) 0xB));
    assert(!isChar(cast(dchar) 0xC));
    assert( isChar(cast(dchar) 0xD));
    assert(!isChar(cast(dchar) 0xE));
    assert(!isChar(cast(dchar) 0x1F));
    assert( isChar(cast(dchar) 0x20));
    assert( isChar('J'));
    assert( isChar(cast(dchar) 0xD7FF));
    assert(!isChar(cast(dchar) 0xD800));
    assert(!isChar(cast(dchar) 0xDFFF));
    assert( isChar(cast(dchar) 0xE000));
    assert( isChar(cast(dchar) 0xFFFD));
    assert(!isChar(cast(dchar) 0xFFFE));
    assert(!isChar(cast(dchar) 0xFFFF));
    assert( isChar(cast(dchar) 0x10000));
    assert( isChar(cast(dchar) 0x10FFFF));
    assert(!isChar(cast(dchar) 0x110000));

    debug (stdxml_TestHardcodedChecks)
    {
        foreach (c; 0 .. dchar.max + 1)
            assert(isChar(c) == lookup(CharTable, c));
    }
}

/**
 * Returns true if the character is whitespace according to the XML standard
 *
 * Only the following characters are considered whitespace in XML - space, tab,
 * carriage return and linefeed
 *
 * Standards: $(LINK2 http://www.w3.org/TR/1998/REC-xml-19980210, XML 1.0)
 *
 * Params:
 *    c = the character to be tested
 */
bool isSpace(dchar c) @safe @nogc pure nothrow
{
    return c == '\u0020' || c == '\u0009' || c == '\u000A' || c == '\u000D';
}

/**
 * Returns true if the character is a digit according to the XML standard
 *
 * Standards: $(LINK2 http://www.w3.org/TR/1998/REC-xml-19980210, XML 1.0)
 *
 * Params:
 *    c = the character to be tested
 */
bool isDigit(dchar c) @safe @nogc pure nothrow
{
    if (c <= 0x0039 && c >= 0x0030)
        return true;
    else
        return lookup(DigitTable,c);
}

@safe @nogc nothrow pure unittest
{
    debug (stdxml_TestHardcodedChecks)
    {
        foreach (c; 0 .. dchar.max + 1)
            assert(isDigit(c) == lookup(DigitTable, c));
    }
}

/**
 * Returns true if the character is a letter according to the XML standard
 *
 * Standards: $(LINK2 http://www.w3.org/TR/1998/REC-xml-19980210, XML 1.0)
 *
 * Params:
 *    c = the character to be tested
 */
bool isLetter(dchar c) @safe @nogc nothrow pure // rule 84
{
    return isIdeographic(c) || isBaseChar(c);
}

/**
 * Returns true if the character is an ideographic character according to the
 * XML standard
 *
 * Standards: $(LINK2 http://www.w3.org/TR/1998/REC-xml-19980210, XML 1.0)
 *
 * Params:
 *    c = the character to be tested
 */
bool isIdeographic(dchar c) @safe @nogc nothrow pure
{
    if (c == 0x3007)
        return true;
    if (c <= 0x3029 && c >= 0x3021 )
        return true;
    if (c <= 0x9FA5 && c >= 0x4E00)
        return true;
    return false;
}

@safe @nogc nothrow pure unittest
{
    assert(isIdeographic('\u4E00'));
    assert(isIdeographic('\u9FA5'));
    assert(isIdeographic('\u3007'));
    assert(isIdeographic('\u3021'));
    assert(isIdeographic('\u3029'));

    debug (stdxml_TestHardcodedChecks)
    {
        foreach (c; 0 .. dchar.max + 1)
            assert(isIdeographic(c) == lookup(IdeographicTable, c));
    }
}

/**
 * Returns true if the character is a base character according to the XML
 * standard
 *
 * Standards: $(LINK2 http://www.w3.org/TR/1998/REC-xml-19980210, XML 1.0)
 *
 * Params:
 *    c = the character to be tested
 */
bool isBaseChar(dchar c) @safe @nogc nothrow pure
{
    return lookup(BaseCharTable,c);
}

/**
 * Returns true if the character is a combining character according to the
 * XML standard
 *
 * Standards: $(LINK2 http://www.w3.org/TR/1998/REC-xml-19980210, XML 1.0)
 *
 * Params:
 *    c = the character to be tested
 */
bool isCombiningChar(dchar c) @safe @nogc nothrow pure
{
    return lookup(CombiningCharTable,c);
}

/**
 * Returns true if the character is an extender according to the XML standard
 *
 * Standards: $(LINK2 http://www.w3.org/TR/1998/REC-xml-19980210, XML 1.0)
 *
 * Params:
 *    c = the character to be tested
 */
bool isExtender(dchar c) @safe @nogc nothrow pure
{
    return lookup(ExtenderTable,c);
}

/**
 * Encodes a string by replacing all characters which need to be escaped with
 * appropriate predefined XML entities.
 *
 * encode() escapes certain characters (ampersand, quote, apostrophe, less-than
 * and greater-than), and similarly, decode() unescapes them. These functions
 * are provided for convenience only. You do not need to use them when using
 * the std.xml classes, because then all the encoding and decoding will be done
 * for you automatically.
 *
 * If the string is not modified, the original will be returned.
 *
 * Standards: $(LINK2 http://www.w3.org/TR/1998/REC-xml-19980210, XML 1.0)
 *
 * Params:
 *      s = The string to be encoded
 *
 * Returns: The encoded string
 *
 * Example:
 * --------------
 * writefln(encode("a > b")); // writes "a &gt; b"
 * --------------
 */
S encode(S)(S s)
{
    import std.array : appender;

    string r;
    size_t lastI;
    auto result = appender!S();

    foreach (i, c; s)
    {
        switch (c)
        {
        case '&':  r = "&amp;"; break;
        case '"':  r = "&quot;"; break;
        case '\'': r = "&apos;"; break;
        case '<':  r = "&lt;"; break;
        case '>':  r = "&gt;"; break;
        default: continue;
        }
        // Replace with r
        result.put(s[lastI .. i]);
        result.put(r);
        lastI = i + 1;
    }

    if (!result.data.ptr) return s;
    result.put(s[lastI .. $]);
    return result.data;
}

@safe pure unittest
{
    auto s = "hello";
    assert(encode(s) is s);
    assert(encode("a > b") == "a &gt; b", encode("a > b"));
    assert(encode("a < b") == "a &lt; b");
    assert(encode("don't") == "don&apos;t");
    assert(encode("\"hi\"") == "&quot;hi&quot;", encode("\"hi\""));
    assert(encode("cat & dog") == "cat &amp; dog");
}

/**
 * Mode to use for decoding.
 *
 * $(DDOC_ENUM_MEMBERS NONE) Do not decode
 * $(DDOC_ENUM_MEMBERS LOOSE) Decode, but ignore errors
 * $(DDOC_ENUM_MEMBERS STRICT) Decode, and throw exception on error
 */
enum DecodeMode
{
    NONE, LOOSE, STRICT
}

/**
 * Decodes a string by unescaping all predefined XML entities.
 *
 * encode() escapes certain characters (ampersand, quote, apostrophe, less-than
 * and greater-than), and similarly, decode() unescapes them. These functions
 * are provided for convenience only. You do not need to use them when using
 * the std.xml classes, because then all the encoding and decoding will be done
 * for you automatically.
 *
 * This function decodes the entities &amp;amp;, &amp;quot;, &amp;apos;,
 * &amp;lt; and &amp;gt,
 * as well as decimal and hexadecimal entities such as &amp;#x20AC;
 *
 * If the string does not contain an ampersand, the original will be returned.
 *
 * Note that the "mode" parameter can be one of DecodeMode.NONE (do not
 * decode), DecodeMode.LOOSE (decode, but ignore errors), or DecodeMode.STRICT
 * (decode, and throw a DecodeException in the event of an error).
 *
 * Standards: $(LINK2 http://www.w3.org/TR/1998/REC-xml-19980210, XML 1.0)
 *
 * Params:
 *      s = The string to be decoded
 *      mode = (optional) Mode to use for decoding. (Defaults to LOOSE).
 *
 * Throws: DecodeException if mode == DecodeMode.STRICT and decode fails
 *
 * Returns: The decoded string
 *
 * Example:
 * --------------
 * writefln(decode("a &gt; b")); // writes "a > b"
 * --------------
 */
string decode(string s, DecodeMode mode=DecodeMode.LOOSE) @safe pure
{
    import std.algorithm.searching : startsWith;

    if (mode == DecodeMode.NONE) return s;

    string buffer;
    foreach (ref i; 0 .. s.length)
    {
        char c = s[i];
        if (c != '&')
        {
            if (buffer.length != 0) buffer ~= c;
        }
        else
        {
            if (buffer.length == 0)
            {
                buffer = s[0 .. i].dup;
            }
            if (startsWith(s[i..$],"&#"))
            {
                try
                {
                    dchar d;
                    string t = s[i..$];
                    checkCharRef(t, d);
                    char[4] temp;
                    import std.utf : encode;
                    buffer ~= temp[0 .. encode(temp, d)];
                    i = s.length - t.length - 1;
                }
                catch (Err e)
                {
                    if (mode == DecodeMode.STRICT)
                        throw new DecodeException("Unescaped &");
                    buffer ~= '&';
                }
            }
            else if (startsWith(s[i..$],"&amp;" )) { buffer ~= '&';  i += 4; }
            else if (startsWith(s[i..$],"&quot;")) { buffer ~= '"';  i += 5; }
            else if (startsWith(s[i..$],"&apos;")) { buffer ~= '\''; i += 5; }
            else if (startsWith(s[i..$],"&lt;"  )) { buffer ~= '<';  i += 3; }
            else if (startsWith(s[i..$],"&gt;"  )) { buffer ~= '>';  i += 3; }
            else
            {
                if (mode == DecodeMode.STRICT)
                    throw new DecodeException("Unescaped &");
                buffer ~= '&';
            }
        }
    }
    return (buffer.length == 0) ? s : buffer;
}

@safe pure unittest
{
    void assertNot(string s) pure
    {
        bool b = false;
        try { decode(s,DecodeMode.STRICT); }
        catch (DecodeException e) { b = true; }
        assert(b,s);
    }

    // Assert that things that should work, do
    auto s = "hello";
    assert(decode(s,                DecodeMode.STRICT) is s);
    assert(decode("a &gt; b",       DecodeMode.STRICT) == "a > b");
    assert(decode("a &lt; b",       DecodeMode.STRICT) == "a < b");
    assert(decode("don&apos;t",     DecodeMode.STRICT) == "don't");
    assert(decode("&quot;hi&quot;", DecodeMode.STRICT) == "\"hi\"");
    assert(decode("cat &amp; dog",  DecodeMode.STRICT) == "cat & dog");
    assert(decode("&#42;",          DecodeMode.STRICT) == "*");
    assert(decode("&#x2A;",         DecodeMode.STRICT) == "*");
    assert(decode("cat & dog",      DecodeMode.LOOSE) == "cat & dog");
    assert(decode("a &gt b",        DecodeMode.LOOSE) == "a &gt b");
    assert(decode("&#;",            DecodeMode.LOOSE) == "&#;");
    assert(decode("&#x;",           DecodeMode.LOOSE) == "&#x;");
    assert(decode("&#2G;",          DecodeMode.LOOSE) == "&#2G;");
    assert(decode("&#x2G;",         DecodeMode.LOOSE) == "&#x2G;");

    // Assert that things that shouldn't work, don't
    assertNot("cat & dog");
    assertNot("a &gt b");
    assertNot("&#;");
    assertNot("&#x;");
    assertNot("&#2G;");
    assertNot("&#x2G;");
}

/**
 * Class representing an XML document.
 *
 * Standards: $(LINK2 http://www.w3.org/TR/1998/REC-xml-19980210, XML 1.0)
 *
 */
class Document : Element
{
    /**
     * Contains all text which occurs before the root element.
     * Defaults to &lt;?xml version="1.0"?&gt;
     */
    string prolog = "<?xml version=\"1.0\"?>";
    /**
     * Contains all text which occurs after the root element.
     * Defaults to the empty string
     */
    string epilog;

    /**
     * Constructs a Document by parsing XML text.
     *
     * This function creates a complete DOM (Document Object Model) tree.
     *
     * The input to this function MUST be valid XML.
     * This is enforced by DocumentParser's in contract.
     *
     * Params:
     *      s = the complete XML text.
     */
    this(string s)
    in
    {
        assert(s.length != 0);
    }
    body
    {
        auto xml = new DocumentParser(s);
        string tagString = xml.tag.tagString;

        this(xml.tag);
        prolog = s[0 .. tagString.ptr - s.ptr];
        parse(xml);
        epilog = *xml.s;
    }

    /**
     * Constructs a Document from a Tag.
     *
     * Params:
     *      tag = the start tag of the document.
     */
    this(const(Tag) tag)
    {
        super(tag);
    }

    const
    {
        /**
         * Compares two Documents for equality
         *
         * Example:
         * --------------
         * Document d1,d2;
         * if (d1 == d2) { }
         * --------------
         */
        override bool opEquals(scope const Object o) const
        {
            const doc = toType!(const Document)(o);
            return prolog == doc.prolog
                && (cast(const) this).Element.opEquals(cast(const) doc)
                && epilog == doc.epilog;
        }

        /**
         * Compares two Documents
         *
         * You should rarely need to call this function. It exists so that
         * Documents can be used as associative array keys.
         *
         * Example:
         * --------------
         * Document d1,d2;
         * if (d1 < d2) { }
         * --------------
         */
        override int opCmp(scope const Object o) scope const
        {
            const doc = toType!(const Document)(o);
            if (prolog != doc.prolog)
                return prolog < doc.prolog ? -1 : 1;
            if (int cmp = this.Element.opCmp(doc))
                return cmp;
            if (epilog != doc.epilog)
                return epilog < doc.epilog ? -1 : 1;
            return 0;
        }

        /**
         * Returns the hash of a Document
         *
         * You should rarely need to call this function. It exists so that
         * Documents can be used as associative array keys.
         */
        override size_t toHash() scope const @trusted
        {
            return hash(prolog, hash(epilog, (cast() this).Element.toHash()));
        }

        /**
         * Returns the string representation of a Document. (That is, the
         * complete XML of a document).
         */
        override string toString() scope const @safe
        {
            return prolog ~ super.toString() ~ epilog;
        }
    }
}

@system unittest
{
    // https://issues.dlang.org/show_bug.cgi?id=14966
    auto xml = `<?xml version="1.0" encoding="UTF-8"?><foo></foo>`;

    auto a = new Document(xml);
    auto b = new Document(xml);
    assert(a == b);
    assert(!(a < b));
    int[Document] aa;
    aa[a] = 1;
    assert(aa[b] == 1);

    b ~= new Element("b");
    assert(a < b);
    assert(b > a);
}

/**
 * Class representing an XML element.
 *
 * Standards: $(LINK2 http://www.w3.org/TR/1998/REC-xml-19980210, XML 1.0)
 */
class Element : Item
{
    Tag tag; /// The start tag of the element
    Item[] items; /// The element's items
    Text[] texts; /// The element's text items
    CData[] cdatas; /// The element's CData items
    Comment[] comments; /// The element's comments
    ProcessingInstruction[] pis; /// The element's processing instructions
    Element[] elements; /// The element's child elements

    /**
     * Constructs an Element given a name and a string to be used as a Text
     * interior.
     *
     * Params:
     *      name = the name of the element.
     *      interior = (optional) the string interior.
     *
     * Example:
     * -------------------------------------------------------
     * auto element = new Element("title","Serenity")
     *     // constructs the element <title>Serenity</title>
     * -------------------------------------------------------
     */
    this(string name, string interior=null) @safe pure
    {
        this(new Tag(name));
        if (interior.length != 0) opCatAssign(new Text(interior));
    }

    /**
     * Constructs an Element from a Tag.
     *
     * Params:
     *      tag_ = the start or empty tag of the element.
     */
    this(const(Tag) tag_) @safe pure
    {
        this.tag = new Tag(tag_.name);
        tag.type = TagType.EMPTY;
        foreach (k,v;tag_.attr) tag.attr[k] = v;
        tag.tagString = tag_.tagString;
    }

    /**
     * Append a text item to the interior of this element
     *
     * Params:
     *      item = the item you wish to append.
     *
     * Example:
     * --------------
     * Element element;
     * element ~= new Text("hello");
     * --------------
     */
    void opCatAssign(Text item) @safe pure
    {
        texts ~= item;
        appendItem(item);
    }

    /**
     * Append a CData item to the interior of this element
     *
     * Params:
     *      item = the item you wish to append.
     *
     * Example:
     * --------------
     * Element element;
     * element ~= new CData("hello");
     * --------------
     */
    void opCatAssign(CData item) @safe pure
    {
        cdatas ~= item;
        appendItem(item);
    }

    /**
     * Append a comment to the interior of this element
     *
     * Params:
     *      item = the item you wish to append.
     *
     * Example:
     * --------------
     * Element element;
     * element ~= new Comment("hello");
     * --------------
     */
    void opCatAssign(Comment item) @safe pure
    {
        comments ~= item;
        appendItem(item);
    }

    /**
     * Append a processing instruction to the interior of this element
     *
     * Params:
     *      item = the item you wish to append.
     *
     * Example:
     * --------------
     * Element element;
     * element ~= new ProcessingInstruction("hello");
     * --------------
     */
    void opCatAssign(ProcessingInstruction item) @safe pure
    {
        pis ~= item;
        appendItem(item);
    }

    /**
     * Append a complete element to the interior of this element
     *
     * Params:
     *      item = the item you wish to append.
     *
     * Example:
     * --------------
     * Element element;
     * Element other = new Element("br");
     * element ~= other;
     *    // appends element representing <br />
     * --------------
     */
    void opCatAssign(Element item) @safe pure
    {
        elements ~= item;
        appendItem(item);
    }

    private void appendItem(Item item) @safe pure
    {
        items ~= item;
        if (tag.type == TagType.EMPTY && !item.isEmptyXML)
            tag.type = TagType.START;
    }

    private void parse(ElementParser xml)
    {
        xml.onText = (string s) { opCatAssign(new Text(s)); };
        xml.onCData = (string s) { opCatAssign(new CData(s)); };
        xml.onComment = (string s) { opCatAssign(new Comment(s)); };
        xml.onPI = (string s) { opCatAssign(new ProcessingInstruction(s)); };

        xml.onStartTag[null] = (ElementParser xml)
        {
            auto e = new Element(xml.tag);
            e.parse(xml);
            opCatAssign(e);
        };

        xml.parse();
    }

    /**
     * Compares two Elements for equality
     *
     * Example:
     * --------------
     * Element e1,e2;
     * if (e1 == e2) { }
     * --------------
     */
    override bool opEquals(scope const Object o) const
    {
        const element = toType!(const Element)(o);
        immutable len = items.length;
        if (len != element.items.length) return false;
        foreach (i; 0 .. len)
        {
            if (!items[i].opEquals(element.items[i])) return false;
        }
        return true;
    }

    /**
     * Compares two Elements
     *
     * You should rarely need to call this function. It exists so that Elements
     * can be used as associative array keys.
     *
     * Example:
     * --------------
     * Element e1,e2;
     * if (e1 < e2) { }
     * --------------
     */
    override int opCmp(scope const Object o) @safe const
    {
        const element = toType!(const Element)(o);
        for (uint i=0; ; ++i)
        {
            if (i == items.length && i == element.items.length) return 0;
            if (i == items.length) return -1;
            if (i == element.items.length) return 1;
            if (!items[i].opEquals(element.items[i]))
                return items[i].opCmp(element.items[i]);
        }
    }

    /**
     * Returns the hash of an Element
     *
     * You should rarely need to call this function. It exists so that Elements
     * can be used as associative array keys.
     */
    override size_t toHash() scope const @safe
    {
        size_t hash = tag.toHash();
        foreach (item;items) hash += item.toHash();
        return hash;
    }

    const
    {
        /**
         * Returns the decoded interior of an element.
         *
         * The element is assumed to contain text <i>only</i>. So, for
         * example, given XML such as "&lt;title&gt;Good &amp;amp;
         * Bad&lt;/title&gt;", will return "Good &amp; Bad".
         *
         * Params:
         *      mode = (optional) Mode to use for decoding. (Defaults to LOOSE).
         *
         * Throws: DecodeException if decode fails
         */
        string text(DecodeMode mode=DecodeMode.LOOSE)
        {
            string buffer;
            foreach (item;items)
            {
                Text t = cast(Text) item;
                if (t is null) throw new DecodeException(item.toString());
                buffer ~= decode(t.toString(),mode);
            }
            return buffer;
        }

        /**
         * Returns an indented string representation of this item
         *
         * Params:
         *      indent = (optional) number of spaces by which to indent this
         *          element. Defaults to 2.
         */
        override string[] pretty(uint indent=2) scope
        {
            import std.algorithm.searching : count;
            import std.string : rightJustify;

            if (isEmptyXML) return [ tag.toEmptyString() ];

            if (items.length == 1)
            {
                auto t = cast(const(Text))(items[0]);
                if (t !is null)
                {
                    return [tag.toStartString() ~ t.toString() ~ tag.toEndString()];
                }
            }

            string[] a = [ tag.toStartString() ];
            foreach (item;items)
            {
                string[] b = item.pretty(indent);
                foreach (s;b)
                {
                    a ~= rightJustify(s,count(s) + indent);
                }
            }
            a ~= tag.toEndString();
            return a;
        }

        /**
         * Returns the string representation of an Element
         *
         * Example:
         * --------------
         * auto element = new Element("br");
         * writefln(element.toString()); // writes "<br />"
         * --------------
         */
        override string toString() scope @safe
        {
            if (isEmptyXML) return tag.toEmptyString();

            string buffer = tag.toStartString();
            foreach (item;items) { buffer ~= item.toString(); }
            buffer ~= tag.toEndString();
            return buffer;
        }

        override @property @safe pure @nogc nothrow bool isEmptyXML() const scope { return items.length == 0; }
    }
}

/**
 * Tag types.
 *
 * $(DDOC_ENUM_MEMBERS START) Used for start tags
 * $(DDOC_ENUM_MEMBERS END) Used for end tags
 * $(DDOC_ENUM_MEMBERS EMPTY) Used for empty tags
 *
 */
enum TagType { START, END, EMPTY }

/**
 * Class representing an XML tag.
 *
 * Standards: $(LINK2 http://www.w3.org/TR/1998/REC-xml-19980210, XML 1.0)
 *
 * The class invariant guarantees
 * <ul>
 * <li> that $(B type) is a valid enum TagType value</li>
 * <li> that $(B name) consists of valid characters</li>
 * <li> that each attribute name consists of valid characters</li>
 * </ul>
 */
class Tag
{
    TagType type = TagType.START;   /// Type of tag
    string name;                    /// Tag name
    string[string] attr;            /// Associative array of attributes
    private string tagString;

    invariant()
    {
        string s;
        string t;

        assert(type == TagType.START
            || type == TagType.END
            || type == TagType.EMPTY);

        s = name;
        try { checkName(s,t); }
        catch (Err e) { assert(false,"Invalid tag name:" ~ e.toString()); }

        foreach (k,v;attr)
        {
            s = k;
            try { checkName(s,t); }
            catch (Err e)
                { assert(false,"Invalid atrribute name:" ~ e.toString()); }
        }
    }

    /**
     * Constructs an instance of Tag with a specified name and type
     *
     * The constructor does not initialize the attributes. To initialize the
     * attributes, you access the $(B attr) member variable.
     *
     * Params:
     *      name = the Tag's name
     *      type = (optional) the Tag's type. If omitted, defaults to
     *          TagType.START.
     *
     * Example:
     * --------------
     * auto tag = new Tag("img",Tag.EMPTY);
     * tag.attr["src"] = "http://example.com/example.jpg";
     * --------------
     */
    this(string name, TagType type=TagType.START) @safe pure
    {
        this.name = name;
        this.type = type;
    }

    /* Private constructor (so don't ddoc this!)
     *
     * Constructs a Tag by parsing the string representation, e.g. "<html>".
     *
     * The string is passed by reference, and is advanced over all characters
     * consumed.
     *
     * The second parameter is a dummy parameter only, required solely to
     * distinguish this constructor from the public one.
     */
    private this(ref string s, bool dummy) @safe pure
    {
        import std.algorithm.searching : countUntil;
        import std.ascii : isWhite;
        import std.utf : byCodeUnit;

        tagString = s;
        try
        {
            reqc(s,'<');
            if (optc(s,'/')) type = TagType.END;
            ptrdiff_t i = s.byCodeUnit.countUntil(">", "/>", " ", "\t", "\v", "\r", "\n", "\f");
            name = s[0 .. i];
            s = s[i .. $];

            i = s.byCodeUnit.countUntil!(a => !isWhite(a));
            s = s[i .. $];

            while (s.length > 0 && s[0] != '>' && s[0] != '/')
            {
                i = s.byCodeUnit.countUntil("=", " ", "\t", "\v", "\r", "\n", "\f");
                string key = s[0 .. i];
                s = s[i .. $];

                i = s.byCodeUnit.countUntil!(a => !isWhite(a));
                s = s[i .. $];
                reqc(s,'=');
                i = s.byCodeUnit.countUntil!(a => !isWhite(a));
                s = s[i .. $];

                immutable char quote = requireOneOf(s,"'\"");
                i = s.byCodeUnit.countUntil(quote);
                string val = decode(s[0 .. i], DecodeMode.LOOSE);
                s = s[i .. $];
                reqc(s,quote);

                i = s.byCodeUnit.countUntil!(a => !isWhite(a));
                s = s[i .. $];
                attr[key] = val;
            }
            if (optc(s,'/'))
            {
                if (type == TagType.END) throw new TagException("");
                type = TagType.EMPTY;
            }
            reqc(s,'>');
            tagString.length = tagString.length - s.length;
        }
        catch (XMLException e)
        {
            tagString.length = tagString.length - s.length;
            throw new TagException(tagString);
        }
    }

    const
    {
        /**
         * Compares two Tags for equality
         *
         * You should rarely need to call this function. It exists so that Tags
         * can be used as associative array keys.
         *
         * Example:
         * --------------
         * Tag tag1,tag2
         * if (tag1 == tag2) { }
         * --------------
         */
        override bool opEquals(scope Object o)
        {
            const tag = toType!(const Tag)(o);
            return
                (name != tag.name) ? false : (
                (attr != tag.attr) ? false : (
                (type != tag.type) ? false : (
            true )));
        }

        /**
         * Compares two Tags
         *
         * Example:
         * --------------
         * Tag tag1,tag2
         * if (tag1 < tag2) { }
         * --------------
         */
        override int opCmp(Object o)
        {
            const tag = toType!(const Tag)(o);
            // Note that attr is an AA, so the comparison is nonsensical (bug 10381)
            return
                ((name != tag.name) ? ( name < tag.name ? -1 : 1 ) :
                ((attr != tag.attr) ? ( cast(void *) attr < cast(void*) tag.attr ? -1 : 1 ) :
                ((type != tag.type) ? ( type < tag.type ? -1 : 1 ) :
            0 )));
        }

        /**
         * Returns the hash of a Tag
         *
         * You should rarely need to call this function. It exists so that Tags
         * can be used as associative array keys.
         */
        override size_t toHash()
        {
            return typeid(name).getHash(&name);
        }

        /**
         * Returns the string representation of a Tag
         *
         * Example:
         * --------------
         * auto tag = new Tag("book",TagType.START);
         * writefln(tag.toString()); // writes "<book>"
         * --------------
         */
        override string toString() @safe
        {
            if (isEmpty) return toEmptyString();
            return (isEnd) ? toEndString() : toStartString();
        }

        private
        {
            string toNonEndString() @safe
            {
                import std.format : format;

                string s = "<" ~ name;
                foreach (key,val;attr)
                    s ~= format(" %s=\"%s\"",key,encode(val));
                return s;
            }

            string toStartString() @safe { return toNonEndString() ~ ">"; }

            string toEndString() @safe { return "</" ~ name ~ ">"; }

            string toEmptyString() @safe { return toNonEndString() ~ " />"; }
        }

        /**
         * Returns true if the Tag is a start tag
         *
         * Example:
         * --------------
         * if (tag.isStart) { }
         * --------------
         */
        @property bool isStart() @safe @nogc pure nothrow { return type == TagType.START; }

        /**
         * Returns true if the Tag is an end tag
         *
         * Example:
         * --------------
         * if (tag.isEnd) { }
         * --------------
         */
        @property bool isEnd() @safe @nogc pure nothrow { return type == TagType.END;   }

        /**
         * Returns true if the Tag is an empty tag
         *
         * Example:
         * --------------
         * if (tag.isEmpty) { }
         * --------------
         */
        @property bool isEmpty() @safe @nogc pure nothrow { return type == TagType.EMPTY; }
    }
}

/**
 * Class representing a comment
 */
class Comment : Item
{
    private string content;

    /**
     * Construct a comment
     *
     * Params:
     *      content = the body of the comment
     *
     * Throws: CommentException if the comment body is illegal (contains "--"
     * or exactly equals "-")
     *
     * Example:
     * --------------
     * auto item = new Comment("This is a comment");
     *    // constructs <!--This is a comment-->
     * --------------
     */
    this(string content) @safe pure
    {
        import std.string : indexOf;

        if (content == "-" || content.indexOf("--") != -1)
            throw new CommentException(content);
        this.content = content;
    }

    /**
     * Compares two comments for equality
     *
     * Example:
     * --------------
     * Comment item1,item2;
     * if (item1 == item2) { }
     * --------------
     */
    override bool opEquals(scope const Object o) const
    {
        const item = toType!(const Item)(o);
        const t = cast(const Comment) item;
        return t !is null && content == t.content;
    }

    /**
     * Compares two comments
     *
     * You should rarely need to call this function. It exists so that Comments
     * can be used as associative array keys.
     *
     * Example:
     * --------------
     * Comment item1,item2;
     * if (item1 < item2) { }
     * --------------
     */
    override int opCmp(scope const Object o) scope const
    {
        const item = toType!(const Item)(o);
        const t = cast(const Comment) item;
        return t !is null && (content != t.content
            ? (content < t.content ? -1 : 1 ) : 0 );
    }

    /**
     * Returns the hash of a Comment
     *
     * You should rarely need to call this function. It exists so that Comments
     * can be used as associative array keys.
     */
    override size_t toHash() scope const nothrow { return hash(content); }

    /**
     * Returns a string representation of this comment
     */
    override string toString() scope const @safe pure nothrow { return "<!--" ~ content ~ "-->"; }

    override @property @safe @nogc pure nothrow scope bool isEmptyXML() const { return false; } /// Returns false always
}

@safe unittest // issue 16241
{
    import std.exception : assertThrown;
    auto c = new Comment("==");
    assert(c.content == "==");
    assertThrown!CommentException(new Comment("--"));
}

/**
 * Class representing a Character Data section
 */
class CData : Item
{
    private string content;

    /**
     * Construct a character data section
     *
     * Params:
     *      content = the body of the character data segment
     *
     * Throws: CDataException if the segment body is illegal (contains "]]>")
     *
     * Example:
     * --------------
     * auto item = new CData("<b>hello</b>");
     *    // constructs <![CDATA[<b>hello</b>]]>
     * --------------
     */
    this(string content) @safe pure
    {
        import std.string : indexOf;
        if (content.indexOf("]]>") != -1) throw new CDataException(content);
        this.content = content;
    }

    /**
     * Compares two CDatas for equality
     *
     * Example:
     * --------------
     * CData item1,item2;
     * if (item1 == item2) { }
     * --------------
     */
    override bool opEquals(scope const Object o) const
    {
        const item = toType!(const Item)(o);
        const t = cast(const CData) item;
        return t !is null && content == t.content;
    }

    /**
     * Compares two CDatas
     *
     * You should rarely need to call this function. It exists so that CDatas
     * can be used as associative array keys.
     *
     * Example:
     * --------------
     * CData item1,item2;
     * if (item1 < item2) { }
     * --------------
     */
    override int opCmp(scope const Object o) scope const
    {
        const item = toType!(const Item)(o);
        const t = cast(const CData) item;
        return t !is null && (content != t.content
            ? (content < t.content ? -1 : 1 ) : 0 );
    }

    /**
     * Returns the hash of a CData
     *
     * You should rarely need to call this function. It exists so that CDatas
     * can be used as associative array keys.
     */
    override size_t toHash() scope const nothrow { return hash(content); }

    /**
     * Returns a string representation of this CData section
     */
    override string toString() scope const @safe pure nothrow { return cdata ~ content ~ "]]>"; }

    override @property @safe @nogc pure nothrow scope bool isEmptyXML() const { return false; } /// Returns false always
}

/**
 * Class representing a text (aka Parsed Character Data) section
 */
class Text : Item
{
    private string content;

    /**
     * Construct a text (aka PCData) section
     *
     * Params:
     *      content = the text. This function encodes the text before
     *      insertion, so it is safe to insert any text
     *
     * Example:
     * --------------
     * auto Text = new CData("a < b");
     *    // constructs a &lt; b
     * --------------
     */
    this(string content) @safe pure
    {
        this.content = encode(content);
    }

    /**
     * Compares two text sections for equality
     *
     * Example:
     * --------------
     * Text item1,item2;
     * if (item1 == item2) { }
     * --------------
     */
    override bool opEquals(scope const Object o) const
    {
        const item = toType!(const Item)(o);
        const t = cast(const Text) item;
        return t !is null && content == t.content;
    }

    /**
     * Compares two text sections
     *
     * You should rarely need to call this function. It exists so that Texts
     * can be used as associative array keys.
     *
     * Example:
     * --------------
     * Text item1,item2;
     * if (item1 < item2) { }
     * --------------
     */
    override int opCmp(scope const Object o) scope const
    {
        const item = toType!(const Item)(o);
        const t = cast(const Text) item;
        return t !is null
            && (content != t.content ? (content < t.content ? -1 : 1 ) : 0 );
    }

    /**
     * Returns the hash of a text section
     *
     * You should rarely need to call this function. It exists so that Texts
     * can be used as associative array keys.
     */
    override size_t toHash() scope const nothrow { return hash(content); }

    /**
     * Returns a string representation of this Text section
     */
    override string toString() scope const @safe @nogc pure nothrow { return content; }

    /**
     * Returns true if the content is the empty string
     */
    override @property @safe @nogc pure nothrow scope bool isEmptyXML() const { return content.length == 0; }
}

/**
 * Class representing an XML Instruction section
 */
class XMLInstruction : Item
{
    private string content;

    /**
     * Construct an XML Instruction section
     *
     * Params:
     *      content = the body of the instruction segment
     *
     * Throws: XIException if the segment body is illegal (contains ">")
     *
     * Example:
     * --------------
     * auto item = new XMLInstruction("ATTLIST");
     *    // constructs <!ATTLIST>
     * --------------
     */
    this(string content) @safe pure
    {
        import std.string : indexOf;
        if (content.indexOf(">") != -1) throw new XIException(content);
        this.content = content;
    }

    /**
     * Compares two XML instructions for equality
     *
     * Example:
     * --------------
     * XMLInstruction item1,item2;
     * if (item1 == item2) { }
     * --------------
     */
    override bool opEquals(scope const Object o) const
    {
        const item = toType!(const Item)(o);
        const t = cast(const XMLInstruction) item;
        return t !is null && content == t.content;
    }

    /**
     * Compares two XML instructions
     *
     * You should rarely need to call this function. It exists so that
     * XmlInstructions can be used as associative array keys.
     *
     * Example:
     * --------------
     * XMLInstruction item1,item2;
     * if (item1 < item2) { }
     * --------------
     */
    override int opCmp(scope const Object o) scope const
    {
        const item = toType!(const Item)(o);
        const t = cast(const XMLInstruction) item;
        return t !is null
            && (content != t.content ? (content < t.content ? -1 : 1 ) : 0 );
    }

    /**
     * Returns the hash of an XMLInstruction
     *
     * You should rarely need to call this function. It exists so that
     * XmlInstructions can be used as associative array keys.
     */
    override size_t toHash() scope const nothrow { return hash(content); }

    /**
     * Returns a string representation of this XmlInstruction
     */
    override string toString() scope const @safe pure nothrow { return "<!" ~ content ~ ">"; }

    override @property @safe @nogc pure nothrow scope bool isEmptyXML() const { return false; } /// Returns false always
}

/**
 * Class representing a Processing Instruction section
 */
class ProcessingInstruction : Item
{
    private string content;

    /**
     * Construct a Processing Instruction section
     *
     * Params:
     *      content = the body of the instruction segment
     *
     * Throws: PIException if the segment body is illegal (contains "?>")
     *
     * Example:
     * --------------
     * auto item = new ProcessingInstruction("php");
     *    // constructs <?php?>
     * --------------
     */
    this(string content) @safe pure
    {
        import std.string : indexOf;
        if (content.indexOf("?>") != -1) throw new PIException(content);
        this.content = content;
    }

    /**
     * Compares two processing instructions for equality
     *
     * Example:
     * --------------
     * ProcessingInstruction item1,item2;
     * if (item1 == item2) { }
     * --------------
     */
    override bool opEquals(scope const Object o) const
    {
        const item = toType!(const Item)(o);
        const t = cast(const ProcessingInstruction) item;
        return t !is null && content == t.content;
    }

    /**
     * Compares two processing instructions
     *
     * You should rarely need to call this function. It exists so that
     * ProcessingInstructions can be used as associative array keys.
     *
     * Example:
     * --------------
     * ProcessingInstruction item1,item2;
     * if (item1 < item2) { }
     * --------------
     */
    override int opCmp(scope const Object o) scope const
    {
        const item = toType!(const Item)(o);
        const t = cast(const ProcessingInstruction) item;
        return t !is null
            && (content != t.content ? (content < t.content ? -1 : 1 ) : 0 );
    }

    /**
     * Returns the hash of a ProcessingInstruction
     *
     * You should rarely need to call this function. It exists so that
     * ProcessingInstructions can be used as associative array keys.
     */
    override size_t toHash() scope const nothrow { return hash(content); }

    /**
     * Returns a string representation of this ProcessingInstruction
     */
    override string toString() scope const @safe pure nothrow { return "<?" ~ content ~ "?>"; }

    override @property @safe @nogc pure nothrow bool isEmptyXML() scope const { return false; } /// Returns false always
}

/**
 * Abstract base class for XML items
 */
abstract class Item
{
    /// Compares with another Item of same type for equality
    abstract override bool opEquals(scope const Object o) @safe const;

    /// Compares with another Item of same type
    abstract override int opCmp(scope const Object o) @safe const;

    /// Returns the hash of this item
    abstract override size_t toHash() @safe scope const;

    /// Returns a string representation of this item
    abstract override string toString() @safe scope const;

    /**
     * Returns an indented string representation of this item
     *
     * Params:
     *      indent = number of spaces by which to indent child elements
     */
    string[] pretty(uint indent) @safe scope const
    {
        import std.string : strip;
        string s = strip(toString());
        return s.length == 0 ? [] : [ s ];
    }

    /// Returns true if the item represents empty XML text
    abstract @property @safe @nogc pure nothrow bool isEmptyXML() scope const;
}

/**
 * Class for parsing an XML Document.
 *
 * This is a subclass of ElementParser. Most of the useful functions are
 * documented there.
 *
 * Standards: $(LINK2 http://www.w3.org/TR/1998/REC-xml-19980210, XML 1.0)
 *
 * Bugs:
 *      Currently only supports UTF documents.
 *
 *      If there is an encoding attribute in the prolog, it is ignored.
 *
 */
class DocumentParser : ElementParser
{
    string xmlText;

    /**
     * Constructs a DocumentParser.
     *
     * The input to this function MUST be valid XML.
     * This is enforced by the function's in contract.
     *
     * Params:
     *      xmlText_ = the entire XML document as text
     *
     */
    this(string xmlText_)
    in
    {
        assert(xmlText_.length != 0);
        try
        {
            // Confirm that the input is valid XML
            check(xmlText_);
        }
        catch (CheckException e)
        {
            // And if it's not, tell the user why not
            assert(false, "\n" ~ e.toString());
        }
    }
    body
    {
        xmlText = xmlText_;
        s = &xmlText;
        super();    // Initialize everything
        parse();    // Parse through the root tag (but not beyond)
    }
}

@system unittest
{
    auto doc = new Document("<root><child><grandchild/></child></root>");
    assert(doc.elements.length == 1);
    assert(doc.elements[0].tag.name == "child");
    assert(doc.items == doc.elements);
}

/**
 * Class for parsing an XML element.
 *
 * Standards: $(LINK2 http://www.w3.org/TR/1998/REC-xml-19980210, XML 1.0)
 *
 * Note that you cannot construct instances of this class directly. You can
 * construct a DocumentParser (which is a subclass of ElementParser), but
 * otherwise, Instances of ElementParser will be created for you by the
 * library, and passed your way via onStartTag handlers.
 *
 */
class ElementParser
{
    alias Handler = void delegate(string);
    alias ElementHandler = void delegate(in Element element);
    alias ParserHandler = void delegate(ElementParser parser);

    private
    {
        Tag tag_;
        string elementStart;
        string* s;

        Handler commentHandler = null;
        Handler cdataHandler = null;
        Handler xiHandler = null;
        Handler piHandler = null;
        Handler rawTextHandler = null;
        Handler textHandler = null;

        // Private constructor for start tags
        this(ElementParser parent) @safe @nogc pure nothrow
        {
            s = parent.s;
            this();
            tag_ = parent.tag_;
        }

        // Private constructor for empty tags
        this(Tag tag, string* t) @safe @nogc pure nothrow
        {
            s = t;
            this();
            tag_ = tag;
        }
    }

    /**
     * The Tag at the start of the element being parsed. You can read this to
     * determine the tag's name and attributes.
     */
    @property @safe @nogc pure nothrow const(Tag) tag() const { return tag_; }

    /**
     * Register a handler which will be called whenever a start tag is
     * encountered which matches the specified name. You can also pass null as
     * the name, in which case the handler will be called for any unmatched
     * start tag.
     *
     * Example:
     * --------------
     * // Call this function whenever a <podcast> start tag is encountered
     * onStartTag["podcast"] = (ElementParser xml)
     * {
     *     // Your code here
     *     //
     *     // This is a a closure, so code here may reference
     *     // variables which are outside of this scope
     * };
     *
     * // call myEpisodeStartHandler (defined elsewhere) whenever an <episode>
     * // start tag is encountered
     * onStartTag["episode"] = &myEpisodeStartHandler;
     *
     * // call delegate dg for all other start tags
     * onStartTag[null] = dg;
     * --------------
     *
     * This library will supply your function with a new instance of
     * ElementHandler, which may be used to parse inside the element whose
     * start tag was just found, or to identify the tag attributes of the
     * element, etc.
     *
     * Note that your function will be called for both start tags and empty
     * tags. That is, we make no distinction between &lt;br&gt;&lt;/br&gt;
     * and &lt;br/&gt;.
     */
    ParserHandler[string] onStartTag;

    /**
     * Register a handler which will be called whenever an end tag is
     * encountered which matches the specified name. You can also pass null as
     * the name, in which case the handler will be called for any unmatched
     * end tag.
     *
     * Example:
     * --------------
     * // Call this function whenever a </podcast> end tag is encountered
     * onEndTag["podcast"] = (in Element e)
     * {
     *     // Your code here
     *     //
     *     // This is a a closure, so code here may reference
     *     // variables which are outside of this scope
     * };
     *
     * // call myEpisodeEndHandler (defined elsewhere) whenever an </episode>
     * // end tag is encountered
     * onEndTag["episode"] = &myEpisodeEndHandler;
     *
     * // call delegate dg for all other end tags
     * onEndTag[null] = dg;
     * --------------
     *
     * Note that your function will be called for both start tags and empty
     * tags. That is, we make no distinction between &lt;br&gt;&lt;/br&gt;
     * and &lt;br/&gt;.
     */
    ElementHandler[string] onEndTag;

    protected this() @safe @nogc pure nothrow
    {
        elementStart = *s;
    }

    /**
     * Register a handler which will be called whenever text is encountered.
     *
     * Example:
     * --------------
     * // Call this function whenever text is encountered
     * onText = (string s)
     * {
     *     // Your code here
     *
     *     // The passed parameter s will have been decoded by the time you see
     *     // it, and so may contain any character.
     *     //
     *     // This is a a closure, so code here may reference
     *     // variables which are outside of this scope
     * };
     * --------------
     */
    @property @safe @nogc pure nothrow void onText(Handler handler) { textHandler = handler; }

    /**
     * Register an alternative handler which will be called whenever text
     * is encountered. This differs from onText in that onText will decode
     * the text, whereas onTextRaw will not. This allows you to make design
     * choices, since onText will be more accurate, but slower, while
     * onTextRaw will be faster, but less accurate. Of course, you can
     * still call decode() within your handler, if you want, but you'd
     * probably want to use onTextRaw only in circumstances where you
     * know that decoding is unnecessary.
     *
     * Example:
     * --------------
     * // Call this function whenever text is encountered
     * onText = (string s)
     * {
     *     // Your code here
     *
     *     // The passed parameter s will NOT have been decoded.
     *     //
     *     // This is a a closure, so code here may reference
     *     // variables which are outside of this scope
     * };
     * --------------
     */
    @safe @nogc pure nothrow void onTextRaw(Handler handler) { rawTextHandler = handler; }

    /**
     * Register a handler which will be called whenever a character data
     * segment is encountered.
     *
     * Example:
     * --------------
     * // Call this function whenever a CData section is encountered
     * onCData = (string s)
     * {
     *     // Your code here
     *
     *     // The passed parameter s does not include the opening <![CDATA[
     *     // nor closing ]]>
     *     //
     *     // This is a a closure, so code here may reference
     *     // variables which are outside of this scope
     * };
     * --------------
     */
    @property @safe @nogc pure nothrow void onCData(Handler handler) { cdataHandler = handler; }

    /**
     * Register a handler which will be called whenever a comment is
     * encountered.
     *
     * Example:
     * --------------
     * // Call this function whenever a comment is encountered
     * onComment = (string s)
     * {
     *     // Your code here
     *
     *     // The passed parameter s does not include the opening <!-- nor
     *     // closing -->
     *     //
     *     // This is a a closure, so code here may reference
     *     // variables which are outside of this scope
     * };
     * --------------
     */
    @property @safe @nogc pure nothrow void onComment(Handler handler) { commentHandler = handler; }

    /**
     * Register a handler which will be called whenever a processing
     * instruction is encountered.
     *
     * Example:
     * --------------
     * // Call this function whenever a processing instruction is encountered
     * onPI = (string s)
     * {
     *     // Your code here
     *
     *     // The passed parameter s does not include the opening <? nor
     *     // closing ?>
     *     //
     *     // This is a a closure, so code here may reference
     *     // variables which are outside of this scope
     * };
     * --------------
     */
    @property @safe @nogc pure nothrow void onPI(Handler handler) { piHandler = handler; }

    /**
     * Register a handler which will be called whenever an XML instruction is
     * encountered.
     *
     * Example:
     * --------------
     * // Call this function whenever an XML instruction is encountered
     * // (Note: XML instructions may only occur preceding the root tag of a
     * // document).
     * onPI = (string s)
     * {
     *     // Your code here
     *
     *     // The passed parameter s does not include the opening <! nor
     *     // closing >
     *     //
     *     // This is a a closure, so code here may reference
     *     // variables which are outside of this scope
     * };
     * --------------
     */
    @property @safe @nogc pure nothrow void onXI(Handler handler) { xiHandler = handler; }

    /**
     * Parse an XML element.
     *
     * Parsing will continue until the end of the current element. Any items
     * encountered for which a handler has been registered will invoke that
     * handler.
     *
     * Throws: various kinds of XMLException
     */
    void parse()
    {
        import std.algorithm.searching : startsWith;
        import std.string : indexOf;

        string t;
        const Tag root = tag_;
        Tag[string] startTags;
        if (tag_ !is null) startTags[tag_.name] = tag_;

        while (s.length != 0)
        {
            if (startsWith(*s,"<!--"))
            {
                chop(*s,4);
                t = chop(*s,indexOf(*s,"-->"));
                if (commentHandler.funcptr !is null) commentHandler(t);
                chop(*s,3);
            }
            else if (startsWith(*s,"<![CDATA["))
            {
                chop(*s,9);
                t = chop(*s,indexOf(*s,"]]>"));
                if (cdataHandler.funcptr !is null) cdataHandler(t);
                chop(*s,3);
            }
            else if (startsWith(*s,"<!"))
            {
                chop(*s,2);
                t = chop(*s,indexOf(*s,">"));
                if (xiHandler.funcptr !is null) xiHandler(t);
                chop(*s,1);
            }
            else if (startsWith(*s,"<?"))
            {
                chop(*s,2);
                t = chop(*s,indexOf(*s,"?>"));
                if (piHandler.funcptr !is null) piHandler(t);
                chop(*s,2);
            }
            else if (startsWith(*s,"<"))
            {
                tag_ = new Tag(*s,true);
                if (root is null)
                    return; // Return to constructor of derived class

                if (tag_.isStart)
                {
                    startTags[tag_.name] = tag_;

                    auto parser = new ElementParser(this);

                    auto handler = tag_.name in onStartTag;
                    if (handler !is null) (*handler)(parser);
                    else
                    {
                        handler = null in onStartTag;
                        if (handler !is null) (*handler)(parser);
                    }
                }
                else if (tag_.isEnd)
                {
                    const startTag = startTags[tag_.name];
                    string text;

                    if (startTag.tagString.length == 0)
                        assert(0);

                    immutable(char)* p = startTag.tagString.ptr
                        + startTag.tagString.length;
                    immutable(char)* q = &tag_.tagString[0];
                    text = decode(p[0..(q-p)], DecodeMode.LOOSE);

                    auto element = new Element(startTag);
                    if (text.length != 0) element ~= new Text(text);

                    auto handler = tag_.name in onEndTag;
                    if (handler !is null) (*handler)(element);
                    else
                    {
                        handler = null in onEndTag;
                        if (handler !is null) (*handler)(element);
                    }

                    if (tag_.name == root.name) return;
                }
                else if (tag_.isEmpty)
                {
                    Tag startTag = new Tag(tag_.name);

                    // FIX by hed010gy, for bug 2979
                    // http://d.puremagic.com/issues/show_bug.cgi?id=2979
                    if (tag_.attr.length > 0)
                          foreach (tn,tv; tag_.attr) startTag.attr[tn]=tv;
                    // END FIX

                    // Handle the pretend start tag
                    string s2;
                    auto parser = new ElementParser(startTag,&s2);
                    auto handler1 = startTag.name in onStartTag;
                    if (handler1 !is null) (*handler1)(parser);
                    else
                    {
                        handler1 = null in onStartTag;
                        if (handler1 !is null) (*handler1)(parser);
                    }

                    // Handle the pretend end tag
                    auto element = new Element(startTag);
                    auto handler2 = tag_.name in onEndTag;
                    if (handler2 !is null) (*handler2)(element);
                    else
                    {
                        handler2 = null in onEndTag;
                        if (handler2 !is null) (*handler2)(element);
                    }
                }
            }
            else
            {
                t = chop(*s,indexOf(*s,"<"));
                if (rawTextHandler.funcptr !is null)
                    rawTextHandler(t);
                else if (textHandler.funcptr !is null)
                    textHandler(decode(t,DecodeMode.LOOSE));
            }
        }
    }

    /**
     * Returns that part of the element which has already been parsed
     */
    override string toString() const @nogc @safe pure nothrow
    {
        assert(elementStart.length >= s.length);
        return elementStart[0 .. elementStart.length - s.length];
    }

}

private
{
    template Check(string msg)
    {
        string old = s;

        void fail() @safe pure
        {
            s = old;
            throw new Err(s,msg);
        }

        void fail(Err e) @safe pure
        {
            s = old;
            throw new Err(s,msg,e);
        }

        void fail(string msg2) @safe pure
        {
            fail(new Err(s,msg2));
        }
    }

    void checkMisc(ref string s) @safe pure // rule 27
    {
        import std.algorithm.searching : startsWith;

        mixin Check!("Misc");

        try
        {
                 if (s.startsWith("<!--")) { checkComment(s); }
            else if (s.startsWith("<?"))   { checkPI(s); }
            else                           { checkSpace(s); }
        }
        catch (Err e) { fail(e); }
    }

    void checkDocument(ref string s) @safe pure // rule 1
    {
        mixin Check!("Document");
        try
        {
            checkProlog(s);
            checkElement(s);
            star!(checkMisc)(s);
        }
        catch (Err e) { fail(e); }
    }

    void checkChars(ref string s) @safe pure // rule 2
    {
        // TO DO - Fix std.utf stride and decode functions, then use those
        // instead
        import std.format : format;

        mixin Check!("Chars");

        dchar c;
        ptrdiff_t n = -1;
        // 'i' must not be smaller than size_t because size_t is used internally in
        // aApply.d and it will be cast e.g to (int *) which fails on BigEndian targets.
        foreach (size_t i, dchar d; s)
        {
            if (!isChar(d))
            {
                c = d;
                n = i;
                break;
            }
        }
        if (n != -1)
        {
            s = s[n..$];
            fail(format("invalid character: U+%04X",c));
        }
    }

    void checkSpace(ref string s) @safe pure // rule 3
    {
        import std.algorithm.searching : countUntil;
        import std.ascii : isWhite;
        import std.utf : byCodeUnit;

        mixin Check!("Whitespace");
        ptrdiff_t i = s.byCodeUnit.countUntil!(a => !isWhite(a));
        if (i == -1 && s.length > 0 && isWhite(s[0]))
            s = s[$ .. $];
        else if (i > -1)
            s = s[i .. $];
        if (s is old) fail();
    }

    void checkName(ref string s, out string name) @safe pure // rule 5
    {
        mixin Check!("Name");

        if (s.length == 0) fail();
        ptrdiff_t n;
        // 'i' must not be smaller than size_t because size_t is used internally in
        // aApply.d and it will be cast e.g to (int *) which fails on BigEndian targets.
        foreach (size_t i, dchar c; s)
        {
            if (c == '_' || c == ':' || isLetter(c)) continue;
            if (i == 0) fail();
            if (c == '-' || c == '.' || isDigit(c)
                || isCombiningChar(c) || isExtender(c)) continue;
            n = i;
            break;
        }
        name = s[0 .. n];
        s = s[n..$];
    }

    void checkAttValue(ref string s) @safe pure // rule 10
    {
        import std.algorithm.searching : countUntil;
        import std.utf : byCodeUnit;

        mixin Check!("AttValue");

        if (s.length == 0) fail();
        char c = s[0];
        if (c != '\u0022' && c != '\u0027')
            fail("attribute value requires quotes");
        s = s[1..$];
        for (;;)
        {
            s = s[s.byCodeUnit.countUntil(c) .. $];
            if (s.length == 0) fail("unterminated attribute value");
            if (s[0] == '<') fail("< found in attribute value");
            if (s[0] == c) break;
            try { checkReference(s); } catch (Err e) { fail(e); }
        }
        s = s[1..$];
    }

    void checkCharData(ref string s) @safe pure // rule 14
    {
        import std.algorithm.searching : startsWith;

        mixin Check!("CharData");

        while (s.length != 0)
        {
            if (s.startsWith("&")) break;
            if (s.startsWith("<")) break;
            if (s.startsWith("]]>")) fail("]]> found within char data");
            s = s[1..$];
        }
    }

    void checkComment(ref string s) @safe pure // rule 15
    {
        import std.string : indexOf;

        mixin Check!("Comment");

        try { checkLiteral("<!--",s); } catch (Err e) { fail(e); }
        ptrdiff_t n = s.indexOf("--");
        if (n == -1) fail("unterminated comment");
        s = s[n..$];
        try { checkLiteral("-->",s); } catch (Err e) { fail(e); }
    }

    void checkPI(ref string s) @safe pure // rule 16
    {
        mixin Check!("PI");

        try
        {
            checkLiteral("<?",s);
            checkEnd("?>",s);
        }
        catch (Err e) { fail(e); }
    }

    void checkCDSect(ref string s) @safe pure // rule 18
    {
        mixin Check!("CDSect");

        try
        {
            checkLiteral(cdata,s);
            checkEnd("]]>",s);
        }
        catch (Err e) { fail(e); }
    }

    void checkProlog(ref string s) @safe pure // rule 22
    {
        mixin Check!("Prolog");

        try
        {
            /* The XML declaration is optional
             * http://www.w3.org/TR/2008/REC-xml-20081126/#NT-prolog
             */
            opt!(checkXMLDecl)(s);

            star!(checkMisc)(s);
            opt!(seq!(checkDocTypeDecl,star!(checkMisc)))(s);
        }
        catch (Err e) { fail(e); }
    }

    void checkXMLDecl(ref string s) @safe pure // rule 23
    {
        mixin Check!("XMLDecl");

        try
        {
            checkLiteral("<?xml",s);
            checkVersionInfo(s);
            opt!(checkEncodingDecl)(s);
            opt!(checkSDDecl)(s);
            opt!(checkSpace)(s);
            checkLiteral("?>",s);
        }
        catch (Err e) { fail(e); }
    }

    void checkVersionInfo(ref string s) @safe pure // rule 24
    {
        mixin Check!("VersionInfo");

        try
        {
            checkSpace(s);
            checkLiteral("version",s);
            checkEq(s);
            quoted!(checkVersionNum)(s);
        }
        catch (Err e) { fail(e); }
    }

    void checkEq(ref string s) @safe pure // rule 25
    {
        mixin Check!("Eq");

        try
        {
            opt!(checkSpace)(s);
            checkLiteral("=",s);
            opt!(checkSpace)(s);
        }
        catch (Err e) { fail(e); }
    }

    void checkVersionNum(ref string s) @safe pure // rule 26
    {
        import std.algorithm.searching : countUntil;
        import std.utf : byCodeUnit;

        mixin Check!("VersionNum");

        s = s[s.byCodeUnit.countUntil('\"') .. $];
        if (s is old) fail();
    }

    void checkDocTypeDecl(ref string s) @safe pure // rule 28
    {
        mixin Check!("DocTypeDecl");

        try
        {
            checkLiteral("<!DOCTYPE",s);
            //
            // TO DO -- ensure DOCTYPE is well formed
            // (But not yet. That's one of our "future directions")
            //
            checkEnd(">",s);
        }
        catch (Err e) { fail(e); }
    }

    void checkSDDecl(ref string s) @safe pure // rule 32
    {
        import std.algorithm.searching : startsWith;

        mixin Check!("SDDecl");

        try
        {
            checkSpace(s);
            checkLiteral("standalone",s);
            checkEq(s);
        }
        catch (Err e) { fail(e); }

        int n = 0;
             if (s.startsWith("'yes'") || s.startsWith("\"yes\"")) n = 5;
        else if (s.startsWith("'no'" ) || s.startsWith("\"no\"" )) n = 4;
        else fail("standalone attribute value must be 'yes', \"yes\","~
            " 'no' or \"no\"");
        s = s[n..$];
    }

    void checkElement(ref string s) @safe pure // rule 39
    {
        mixin Check!("Element");

        string sname,ename,t;
        try { checkTag(s,t,sname); } catch (Err e) { fail(e); }

        if (t == "STag")
        {
            try
            {
                checkContent(s);
                t = s;
                checkETag(s,ename);
            }
            catch (Err e) { fail(e); }

            if (sname != ename)
            {
                s = t;
                fail("end tag name \"" ~ ename
                    ~ "\" differs from start tag name \""~sname~"\"");
            }
        }
    }

    // rules 40 and 44
    void checkTag(ref string s, out string type, out string name) @safe pure
    {
        mixin Check!("Tag");

        try
        {
            type = "STag";
            checkLiteral("<",s);
            checkName(s,name);
            star!(seq!(checkSpace,checkAttribute))(s);
            opt!(checkSpace)(s);
            if (s.length != 0 && s[0] == '/')
            {
                s = s[1..$];
                type = "ETag";
            }
            checkLiteral(">",s);
        }
        catch (Err e) { fail(e); }
    }

    void checkAttribute(ref string s) @safe pure // rule 41
    {
        mixin Check!("Attribute");

        try
        {
            string name;
            checkName(s,name);
            checkEq(s);
            checkAttValue(s);
        }
        catch (Err e) { fail(e); }
    }

    void checkETag(ref string s, out string name) @safe pure // rule 42
    {
        mixin Check!("ETag");

        try
        {
            checkLiteral("</",s);
            checkName(s,name);
            opt!(checkSpace)(s);
            checkLiteral(">",s);
        }
        catch (Err e) { fail(e); }
    }

    void checkContent(ref string s) @safe pure // rule 43
    {
        import std.algorithm.searching : startsWith;

        mixin Check!("Content");

        try
        {
            while (s.length != 0)
            {
                old = s;
                     if (s.startsWith("&"))        { checkReference(s); }
                else if (s.startsWith("<!--"))     { checkComment(s); }
                else if (s.startsWith("<?"))       { checkPI(s); }
                else if (s.startsWith(cdata)) { checkCDSect(s); }
                else if (s.startsWith("</"))       { break; }
                else if (s.startsWith("<"))        { checkElement(s); }
                else                               { checkCharData(s); }
            }
        }
        catch (Err e) { fail(e); }
    }

    void checkCharRef(ref string s, out dchar c) @safe pure // rule 66
    {
        import std.format : format;

        mixin Check!("CharRef");

        c = 0;
        try { checkLiteral("&#",s); } catch (Err e) { fail(e); }
        int radix = 10;
        if (s.length != 0 && s[0] == 'x')
        {
            s = s[1..$];
            radix = 16;
        }
        if (s.length == 0) fail("unterminated character reference");
        if (s[0] == ';')
            fail("character reference must have at least one digit");
        while (s.length != 0)
        {
            immutable char d = s[0];
            int n = 0;
            switch (d)
            {
                case 'F','f': ++n;      goto case;
                case 'E','e': ++n;      goto case;
                case 'D','d': ++n;      goto case;
                case 'C','c': ++n;      goto case;
                case 'B','b': ++n;      goto case;
                case 'A','a': ++n;      goto case;
                case '9':     ++n;      goto case;
                case '8':     ++n;      goto case;
                case '7':     ++n;      goto case;
                case '6':     ++n;      goto case;
                case '5':     ++n;      goto case;
                case '4':     ++n;      goto case;
                case '3':     ++n;      goto case;
                case '2':     ++n;      goto case;
                case '1':     ++n;      goto case;
                case '0':     break;
                default: n = 100; break;
            }
            if (n >= radix) break;
            c *= radix;
            c += n;
            s = s[1..$];
        }
        if (!isChar(c)) fail(format("U+%04X is not a legal character",c));
        if (s.length == 0 || s[0] != ';') fail("expected ;");
        else s = s[1..$];
    }

    void checkReference(ref string s) @safe pure // rule 67
    {
        import std.algorithm.searching : startsWith;

        mixin Check!("Reference");

        try
        {
            dchar c;
            if (s.startsWith("&#")) checkCharRef(s,c);
            else checkEntityRef(s);
        }
        catch (Err e) { fail(e); }
    }

    void checkEntityRef(ref string s) @safe pure // rule 68
    {
        mixin Check!("EntityRef");

        try
        {
            string name;
            checkLiteral("&",s);
            checkName(s,name);
            checkLiteral(";",s);
        }
        catch (Err e) { fail(e); }
    }

    void checkEncName(ref string s) @safe pure // rule 81
    {
        import std.algorithm.searching : countUntil;
        import std.ascii : isAlpha;
        import std.utf : byCodeUnit;

        mixin Check!("EncName");

        s = s[s.byCodeUnit.countUntil!(a => !isAlpha(a)) .. $];
        if (s is old) fail();
        s = s[s.byCodeUnit.countUntil('\"', '\'') .. $];
    }

    void checkEncodingDecl(ref string s) @safe pure // rule 80
    {
        mixin Check!("EncodingDecl");

        try
        {
            checkSpace(s);
            checkLiteral("encoding",s);
            checkEq(s);
            quoted!(checkEncName)(s);
        }
        catch (Err e) { fail(e); }
    }

    // Helper functions

    void checkLiteral(string literal,ref string s) @safe pure
    {
        import std.string : startsWith;

        mixin Check!("Literal");

        if (!s.startsWith(literal)) fail("Expected literal \""~literal~"\"");
        s = s[literal.length..$];
    }

    void checkEnd(string end,ref string s) @safe pure
    {
        import std.string : indexOf;
        // Deliberately no mixin Check here.

        auto n = s.indexOf(end);
        if (n == -1) throw new Err(s,"Unable to find terminating \""~end~"\"");
        s = s[n..$];
        checkLiteral(end,s);
    }

    // Metafunctions -- none of these use mixin Check

    void opt(alias f)(ref string s)
    {
        try { f(s); } catch (Err e) {}
    }

    void plus(alias f)(ref string s)
    {
        f(s);
        star!(f)(s);
    }

    void star(alias f)(ref string s)
    {
        while (s.length != 0)
        {
            try { f(s); }
            catch (Err e) { return; }
        }
    }

    void quoted(alias f)(ref string s)
    {
        import std.string : startsWith;

        if (s.startsWith("'"))
        {
            checkLiteral("'",s);
            f(s);
            checkLiteral("'",s);
        }
        else
        {
            checkLiteral("\"",s);
            f(s);
            checkLiteral("\"",s);
        }
    }

    void seq(alias f,alias g)(ref string s)
    {
        f(s);
        g(s);
    }
}

/**
 * Check an entire XML document for well-formedness
 *
 * Params:
 *      s = the document to be checked, passed as a string
 *
 * Throws: CheckException if the document is not well formed
 *
 * CheckException's toString() method will yield the complete hierarchy of
 * parse failure (the XML equivalent of a stack trace), giving the line and
 * column number of every failure at every level.
 */
void check(string s) @safe pure
{
    try
    {
        checkChars(s);
        checkDocument(s);
        if (s.length != 0) throw new Err(s,"Junk found after document");
    }
    catch (Err e)
    {
        e.complete(s);
        throw e;
    }
}

@system pure unittest
{
    import std.string : indexOf;

    try
    {
        check(q"[<?xml version="1.0"?>
        <catalog>
           <book id="bk101">
              <author>Gambardella, Matthew</author>
              <title>XML Developer's Guide</title>
              <genre>Computer</genre>
              <price>44.95</price>
              <publish_date>2000-10-01</publish_date>
              <description>An in-depth look at creating applications
              with XML.</description>
           </book>
           <book id="bk102">
              <author>Ralls, Kim</author>
              <title>Midnight Rain</title>
              <genre>Fantasy</genres>
              <price>5.95</price>
              <publish_date>2000-12-16</publish_date>
              <description>A former architect battles corporate zombies,
              an evil sorceress, and her own childhood to become queen
              of the world.</description>
           </book>
           <book id="bk103">
              <author>Corets, Eva</author>
              <title>Maeve Ascendant</title>
              <genre>Fantasy</genre>
              <price>5.95</price>
              <publish_date>2000-11-17</publish_date>
              <description>After the collapse of a nanotechnology
              society in England, the young survivors lay the
              foundation for a new society.</description>
           </book>
        </catalog>
        ]");
        assert(false);
    }
    catch (CheckException e)
    {
        auto n = e.toString().indexOf("end tag name \"genres\" differs"~
                                      " from start tag name \"genre\"");
        assert(n != -1);
    }
}

@system unittest
{
    string s = q"EOS
<?xml version="1.0"?>
<set>
    <one>A</one>
    <!-- comment -->
    <two>B</two>
</set>
EOS";
    try
    {
        check(s);
    }
    catch (CheckException e)
    {
        assert(0, e.toString());
    }
}

@system unittest
{
    string test_xml = `<?xml version="1.0" encoding='UTF-8'?><r><stream:stream
                        xmlns:stream="http://etherx.'jabber'.org/streams"
                        xmlns="jabber:'client'" from='jid.pl' id="587a5767"
                        xml:lang="en" version="1.0" attr='a"b"c'>
                        </stream:stream></r>`;

    DocumentParser parser = new DocumentParser(test_xml);
    bool tested = false;
    parser.onStartTag["stream:stream"] = (ElementParser p) {
        assert(p.tag.attr["xmlns"] == "jabber:'client'");
        assert(p.tag.attr["from"] == "jid.pl");
        assert(p.tag.attr["attr"] == "a\"b\"c");
        tested = true;
    };
    parser.parse();
    assert(tested);
}

@system unittest
{
    string s = q"EOS
<?xml version="1.0" encoding="utf-8"?> <Tests>
    <Test thing="What &amp; Up">What &amp; Up Second</Test>
</Tests>
EOS";
    auto xml = new DocumentParser(s);

    xml.onStartTag["Test"] = (ElementParser xml) {
        assert(xml.tag.attr["thing"] == "What & Up");
    };

    xml.onEndTag["Test"] = (in Element e) {
        assert(e.text() == "What & Up Second");
    };
    xml.parse();
}

@system unittest
{
    string s = `<tag attr="&quot;value&gt;" />`;
    auto doc = new Document(s);
    assert(doc.toString() == s);
}

/** The base class for exceptions thrown by this module */
class XMLException : Exception { this(string msg) @safe pure { super(msg); } }

// Other exceptions

/// Thrown during Comment constructor
class CommentException : XMLException
{ private this(string msg) @safe pure { super(msg); } }

/// Thrown during CData constructor
class CDataException : XMLException
{ private this(string msg) @safe pure { super(msg); } }

/// Thrown during XMLInstruction constructor
class XIException : XMLException
{ private this(string msg) @safe pure { super(msg); } }

/// Thrown during ProcessingInstruction constructor
class PIException : XMLException
{ private this(string msg) @safe pure { super(msg); } }

/// Thrown during Text constructor
class TextException : XMLException
{ private this(string msg) @safe pure { super(msg); } }

/// Thrown during decode()
class DecodeException : XMLException
{ private this(string msg) @safe pure { super(msg); } }

/// Thrown if comparing with wrong type
class InvalidTypeException : XMLException
{ private this(string msg) @safe pure { super(msg); } }

/// Thrown when parsing for Tags
class TagException : XMLException
{ private this(string msg) @safe pure { super(msg); } }

/**
 * Thrown during check()
 */
class CheckException : XMLException
{
    CheckException err; /// Parent in hierarchy
    private string tail;
    /**
     * Name of production rule which failed to parse,
     * or specific error message
     */
    string msg;
    size_t line = 0; /// Line number at which parse failure occurred
    size_t column = 0; /// Column number at which parse failure occurred

    private this(string tail,string msg,Err err=null) @safe pure
    {
        super(null);
        this.tail = tail;
        this.msg = msg;
        this.err = err;
    }

    private void complete(string entire) @safe pure
    {
        import std.string : count, lastIndexOf;
        import std.utf : toUTF32;

        string head = entire[0..$-tail.length];
        ptrdiff_t n = head.lastIndexOf('\n') + 1;
        line = head.count("\n") + 1;
        dstring t = toUTF32(head[n..$]);
        column = t.length + 1;
        if (err !is null) err.complete(entire);
    }

    override string toString() const @safe pure
    {
        import std.format : format;

        string s;
        if (line != 0) s = format("Line %d, column %d: ",line,column);
        s ~= msg;
        s ~= '\n';
        if (err !is null) s = err.toString() ~ s;
        return s;
    }
}

private alias Err = CheckException;

// Private helper functions

private
{
    inout(T) toType(T)(inout Object o)
    {
        T t = cast(T)(o);
        if (t is null)
        {
            throw new InvalidTypeException("Attempt to compare a "
                ~ T.stringof ~ " with an instance of another type");
        }
        return t;
    }

    string chop(ref string s, size_t n) @safe pure nothrow
    {
        if (n == -1) n = s.length;
        string t = s[0 .. n];
        s = s[n..$];
        return t;
    }

    bool optc(ref string s, char c) @safe pure nothrow
    {
        immutable bool b = s.length != 0 && s[0] == c;
        if (b) s = s[1..$];
        return b;
    }

    void reqc(ref string s, char c) @safe pure
    {
        if (s.length == 0 || s[0] != c) throw new TagException("");
        s = s[1..$];
    }

    char requireOneOf(ref string s, string chars) @safe pure
    {
        import std.string : indexOf;

        if (s.length == 0 || indexOf(chars,s[0]) == -1)
            throw new TagException("");
        immutable char ch = s[0];
        s = s[1..$];
        return ch;
    }

    size_t hash(string s,size_t h=0) @trusted nothrow
    {
        return typeid(s).getHash(&s) + h;
    }

    // Definitions from the XML specification
    immutable CharTable=[0x9,0x9,0xA,0xA,0xD,0xD,0x20,0xD7FF,0xE000,0xFFFD,
        0x10000,0x10FFFF];
    immutable BaseCharTable=[0x0041,0x005A,0x0061,0x007A,0x00C0,0x00D6,0x00D8,
        0x00F6,0x00F8,0x00FF,0x0100,0x0131,0x0134,0x013E,0x0141,0x0148,0x014A,
        0x017E,0x0180,0x01C3,0x01CD,0x01F0,0x01F4,0x01F5,0x01FA,0x0217,0x0250,
        0x02A8,0x02BB,0x02C1,0x0386,0x0386,0x0388,0x038A,0x038C,0x038C,0x038E,
        0x03A1,0x03A3,0x03CE,0x03D0,0x03D6,0x03DA,0x03DA,0x03DC,0x03DC,0x03DE,
        0x03DE,0x03E0,0x03E0,0x03E2,0x03F3,0x0401,0x040C,0x040E,0x044F,0x0451,
        0x045C,0x045E,0x0481,0x0490,0x04C4,0x04C7,0x04C8,0x04CB,0x04CC,0x04D0,
        0x04EB,0x04EE,0x04F5,0x04F8,0x04F9,0x0531,0x0556,0x0559,0x0559,0x0561,
        0x0586,0x05D0,0x05EA,0x05F0,0x05F2,0x0621,0x063A,0x0641,0x064A,0x0671,
        0x06B7,0x06BA,0x06BE,0x06C0,0x06CE,0x06D0,0x06D3,0x06D5,0x06D5,0x06E5,
        0x06E6,0x0905,0x0939,0x093D,0x093D,0x0958,0x0961,0x0985,0x098C,0x098F,
        0x0990,0x0993,0x09A8,0x09AA,0x09B0,0x09B2,0x09B2,0x09B6,0x09B9,0x09DC,
        0x09DD,0x09DF,0x09E1,0x09F0,0x09F1,0x0A05,0x0A0A,0x0A0F,0x0A10,0x0A13,
        0x0A28,0x0A2A,0x0A30,0x0A32,0x0A33,0x0A35,0x0A36,0x0A38,0x0A39,0x0A59,
        0x0A5C,0x0A5E,0x0A5E,0x0A72,0x0A74,0x0A85,0x0A8B,0x0A8D,0x0A8D,0x0A8F,
        0x0A91,0x0A93,0x0AA8,0x0AAA,0x0AB0,0x0AB2,0x0AB3,0x0AB5,0x0AB9,0x0ABD,
        0x0ABD,0x0AE0,0x0AE0,0x0B05,0x0B0C,0x0B0F,0x0B10,0x0B13,0x0B28,0x0B2A,
        0x0B30,0x0B32,0x0B33,0x0B36,0x0B39,0x0B3D,0x0B3D,0x0B5C,0x0B5D,0x0B5F,
        0x0B61,0x0B85,0x0B8A,0x0B8E,0x0B90,0x0B92,0x0B95,0x0B99,0x0B9A,0x0B9C,
        0x0B9C,0x0B9E,0x0B9F,0x0BA3,0x0BA4,0x0BA8,0x0BAA,0x0BAE,0x0BB5,0x0BB7,
        0x0BB9,0x0C05,0x0C0C,0x0C0E,0x0C10,0x0C12,0x0C28,0x0C2A,0x0C33,0x0C35,
        0x0C39,0x0C60,0x0C61,0x0C85,0x0C8C,0x0C8E,0x0C90,0x0C92,0x0CA8,0x0CAA,
        0x0CB3,0x0CB5,0x0CB9,0x0CDE,0x0CDE,0x0CE0,0x0CE1,0x0D05,0x0D0C,0x0D0E,
        0x0D10,0x0D12,0x0D28,0x0D2A,0x0D39,0x0D60,0x0D61,0x0E01,0x0E2E,0x0E30,
        0x0E30,0x0E32,0x0E33,0x0E40,0x0E45,0x0E81,0x0E82,0x0E84,0x0E84,0x0E87,
        0x0E88,0x0E8A,0x0E8A,0x0E8D,0x0E8D,0x0E94,0x0E97,0x0E99,0x0E9F,0x0EA1,
        0x0EA3,0x0EA5,0x0EA5,0x0EA7,0x0EA7,0x0EAA,0x0EAB,0x0EAD,0x0EAE,0x0EB0,
        0x0EB0,0x0EB2,0x0EB3,0x0EBD,0x0EBD,0x0EC0,0x0EC4,0x0F40,0x0F47,0x0F49,
        0x0F69,0x10A0,0x10C5,0x10D0,0x10F6,0x1100,0x1100,0x1102,0x1103,0x1105,
        0x1107,0x1109,0x1109,0x110B,0x110C,0x110E,0x1112,0x113C,0x113C,0x113E,
        0x113E,0x1140,0x1140,0x114C,0x114C,0x114E,0x114E,0x1150,0x1150,0x1154,
        0x1155,0x1159,0x1159,0x115F,0x1161,0x1163,0x1163,0x1165,0x1165,0x1167,
        0x1167,0x1169,0x1169,0x116D,0x116E,0x1172,0x1173,0x1175,0x1175,0x119E,
        0x119E,0x11A8,0x11A8,0x11AB,0x11AB,0x11AE,0x11AF,0x11B7,0x11B8,0x11BA,
        0x11BA,0x11BC,0x11C2,0x11EB,0x11EB,0x11F0,0x11F0,0x11F9,0x11F9,0x1E00,
        0x1E9B,0x1EA0,0x1EF9,0x1F00,0x1F15,0x1F18,0x1F1D,0x1F20,0x1F45,0x1F48,
        0x1F4D,0x1F50,0x1F57,0x1F59,0x1F59,0x1F5B,0x1F5B,0x1F5D,0x1F5D,0x1F5F,
        0x1F7D,0x1F80,0x1FB4,0x1FB6,0x1FBC,0x1FBE,0x1FBE,0x1FC2,0x1FC4,0x1FC6,
        0x1FCC,0x1FD0,0x1FD3,0x1FD6,0x1FDB,0x1FE0,0x1FEC,0x1FF2,0x1FF4,0x1FF6,
        0x1FFC,0x2126,0x2126,0x212A,0x212B,0x212E,0x212E,0x2180,0x2182,0x3041,
        0x3094,0x30A1,0x30FA,0x3105,0x312C,0xAC00,0xD7A3];
    immutable IdeographicTable=[0x3007,0x3007,0x3021,0x3029,0x4E00,0x9FA5];
    immutable CombiningCharTable=[0x0300,0x0345,0x0360,0x0361,0x0483,0x0486,
        0x0591,0x05A1,0x05A3,0x05B9,0x05BB,0x05BD,0x05BF,0x05BF,0x05C1,0x05C2,
        0x05C4,0x05C4,0x064B,0x0652,0x0670,0x0670,0x06D6,0x06DC,0x06DD,0x06DF,
        0x06E0,0x06E4,0x06E7,0x06E8,0x06EA,0x06ED,0x0901,0x0903,0x093C,0x093C,
        0x093E,0x094C,0x094D,0x094D,0x0951,0x0954,0x0962,0x0963,0x0981,0x0983,
        0x09BC,0x09BC,0x09BE,0x09BE,0x09BF,0x09BF,0x09C0,0x09C4,0x09C7,0x09C8,
        0x09CB,0x09CD,0x09D7,0x09D7,0x09E2,0x09E3,0x0A02,0x0A02,0x0A3C,0x0A3C,
        0x0A3E,0x0A3E,0x0A3F,0x0A3F,0x0A40,0x0A42,0x0A47,0x0A48,0x0A4B,0x0A4D,
        0x0A70,0x0A71,0x0A81,0x0A83,0x0ABC,0x0ABC,0x0ABE,0x0AC5,0x0AC7,0x0AC9,
        0x0ACB,0x0ACD,0x0B01,0x0B03,0x0B3C,0x0B3C,0x0B3E,0x0B43,0x0B47,0x0B48,
        0x0B4B,0x0B4D,0x0B56,0x0B57,0x0B82,0x0B83,0x0BBE,0x0BC2,0x0BC6,0x0BC8,
        0x0BCA,0x0BCD,0x0BD7,0x0BD7,0x0C01,0x0C03,0x0C3E,0x0C44,0x0C46,0x0C48,
        0x0C4A,0x0C4D,0x0C55,0x0C56,0x0C82,0x0C83,0x0CBE,0x0CC4,0x0CC6,0x0CC8,
        0x0CCA,0x0CCD,0x0CD5,0x0CD6,0x0D02,0x0D03,0x0D3E,0x0D43,0x0D46,0x0D48,
        0x0D4A,0x0D4D,0x0D57,0x0D57,0x0E31,0x0E31,0x0E34,0x0E3A,0x0E47,0x0E4E,
        0x0EB1,0x0EB1,0x0EB4,0x0EB9,0x0EBB,0x0EBC,0x0EC8,0x0ECD,0x0F18,0x0F19,
        0x0F35,0x0F35,0x0F37,0x0F37,0x0F39,0x0F39,0x0F3E,0x0F3E,0x0F3F,0x0F3F,
        0x0F71,0x0F84,0x0F86,0x0F8B,0x0F90,0x0F95,0x0F97,0x0F97,0x0F99,0x0FAD,
        0x0FB1,0x0FB7,0x0FB9,0x0FB9,0x20D0,0x20DC,0x20E1,0x20E1,0x302A,0x302F,
        0x3099,0x3099,0x309A,0x309A];
    immutable DigitTable=[0x0030,0x0039,0x0660,0x0669,0x06F0,0x06F9,0x0966,
        0x096F,0x09E6,0x09EF,0x0A66,0x0A6F,0x0AE6,0x0AEF,0x0B66,0x0B6F,0x0BE7,
        0x0BEF,0x0C66,0x0C6F,0x0CE6,0x0CEF,0x0D66,0x0D6F,0x0E50,0x0E59,0x0ED0,
        0x0ED9,0x0F20,0x0F29];
    immutable ExtenderTable=[0x00B7,0x00B7,0x02D0,0x02D0,0x02D1,0x02D1,0x0387,
        0x0387,0x0640,0x0640,0x0E46,0x0E46,0x0EC6,0x0EC6,0x3005,0x3005,0x3031,
        0x3035,0x309D,0x309E,0x30FC,0x30FE];

    bool lookup(const(int)[] table, int c) @safe @nogc nothrow pure
    {
        while (table.length != 0)
        {
            auto m = (table.length >> 1) & ~1;
            if (c < table[m])
            {
                table = table[0 .. m];
            }
            else if (c > table[m+1])
            {
                table = table[m+2..$];
            }
            else return true;
        }
        return false;
    }

    string startOf(string s) @safe nothrow pure
    {
        string r;
        foreach (char c;s)
        {
            r ~= (c < 0x20 || c > 0x7F) ? '.' : c;
            if (r.length >= 40) { r ~= "___"; break; }
        }
        return r;
    }

    void exit(string s=null)
    {
        throw new XMLException(s);
    }
}
