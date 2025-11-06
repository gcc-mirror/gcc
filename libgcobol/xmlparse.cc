/*
 * Copyright (c) 2021-2025 Symas Corporation
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions are
 * met:
 *
 * * Redistributions of source code must retain the above copyright
 *   notice, this list of conditions and the following disclaimer.
 * * Redistributions in binary form must reproduce the above
 *   copyright notice, this list of conditions and the following disclaimer
 *   in the documentation and/or other materials provided with the
 *   distribution.
 * * Neither the name of the Symas Corporation nor the names of its
 *   contributors may be used to endorse or promote products derived from
 *   this software without specific prior written permission.
 *
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
 * "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
 * LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
 * A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
 * OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
 * SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
 * LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
 * DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
 * THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
 * (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
 * OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 */

#include <fcntl.h>
#include <unistd.h>

#include <cctype>
#include <cerrno>
#include <cmath>
#include <cfenv>
#include <cstdio>
#include <cstdlib>
#include <cstring>
#include <ctime>

#include <algorithm>
#include <vector>

#include <libxml/SAX2.h>
#include <libxml/parser.h>

#include <syslog.h>

#include "config.h"
#include "libgcobol-fp.h"
#include "ec.h"
#include "common-defs.h"
#include "io.h"
#include "gcobolio.h"
#include "libgcobol.h"

#define COUNT_OF(X) (sizeof(X) / sizeof(X[0]))

void sayso( const char func[], int line,
            int len = 0 , const unsigned char data[] = { 0}  ) {
  if( getenv("XMLPARSE") ) {
    switch(len) {
    case 0:
      fprintf(stderr, "%s:%d Kilroy was here\n", func, line);
      break;
    case -1:
      fprintf(stderr, "%s:%d: '%s'\n", func, line, data);
      break;
    default:
      fprintf(stderr, "%s:%d: '%.*s'\n", func, line, len, data);
      break;
    }
  }
}
#define SAYSO() sayso(__func__, __LINE__)
#define SAYSO_DATAZ(S) sayso(__func__, __LINE__, -1, S)
#define SAYSO_DATA(N, S) sayso(__func__, __LINE__, N, S)

#define CTX ctx __attribute__ ((unused))

struct xml_ec_value_t {
  int ibm_code;
  const char msg[80];
} xml_ec_values[] = {
  // Table 73. XML PARSE exceptions that allow continuation
  { 1, "invalid character between elements" },
  { 2, "invalid start before element content" },
  { 3, "duplicate attribute" },
  { 4, "markup character '<' in an attribute value" },
  { 5, "start/end tag mismatch" },
  { 6, "invalid character in element" },
  { 7, "invalid start in element content. " },
  { 8, "CDATA closing character sequence ']]>' not opened" },
  { 10, "comment the character sequence '--' without '>'" },
  { 11, "invalid character in a processing instruction" },
  { 12, "XML declaration was not start of  document" },
  { 13, "invalid digit in a hexadecimal character reference" },
  { 14, "invalid digit in a decimal character reference" },
  { 15, "encoding declaration value name must start with [a-zA-Z] character" },
  { 16, "character reference did not refer to a legal XML character" },
  { 17, "invalid character in an entity reference name" },
  { 70, "EBCDIC document, supported EBCDIC page, unsupported declaration" },
  { 71, "EBCDIC document, unsupported EBCDIC page " },
  { 72, "EBCDIC document, unsupported EBCDIC page, unsupported  declaration" },
  { 73, "EBCDIC document, unsupported EBCDIC page and declaration " },
  { 80, "ASCII document, supported ASCII page, unsupported declaration" },
  { 81, "ASCII document, unsupported ASCII page " },
  { 82, "ASCII document, unsupported ASCII page, unsupported  declaration" },
  { 83, "ASCII document, unsupported ASCII page and declaration " },
  { 84, "ASCII document, invalid UTF-8, external UTF-8, no declaration. " },
  { 85, "ASCII document, invalid UTF-8, external UTF-8, invalid  declaration" },
  { 86, "ASCII document, invalid UTF-8, external ASCII" },
  { 87, "ASCII document, invalid UTF-8, external and document UTF-8" },
  { 88, "ASCII document, invalid UTF-8, unsupported ASCII/UTF-8, UTF-8 declaration" },
  { 89, "ASCII document, invalid UTF-8, external UTF-8, ASCII declaration" },
  { 92, "alphanumeric document expected, document is UTF-16. " },

  // XML PARSE exceptions that allow continuation (continued)
  //// 100,001 - 165,535 EBCDIC document encoding does not match code page
  //// 200,001 - 265,535 ASCII document encoding does not match code page

  // XML PARSE exceptions that do not allow continuation
  { 100, "end of document before start of XML declaration" },
  { 101, "end of document before end of XML declaration" },
  { 102, "end of document before root element" },
  { 103, "end of document before version information in XML declaration" },
  { 104, "end of document before version information value in XML declaration" },
  { 106, "end of document before encoding declaration value in XML declaration" },
  { 108, "end of document before standalone declaration value in XML declaration" },
  { 109, "end of document before attribute name" },
  { 110, "end of document before attribute value" },
  { 111, "end of document before character/entity reference in attribute value" },
  { 112, "end of document before empty element tag" },
  { 113, "end of document before root element name" },
  { 114, "end of document before element name" },
  { 115, "end of document before character data in element content" },
  { 116, "end of document before processing instruction in element content" },
  { 117, "end of document before comment or CDATA section in element content" },
  { 118, "end of document before comment in element content" },
  { 119, "end of document before CDATA section in element content" },
  { 120, "end of document before character/entity reference in element content" },
  { 121, "end of document before after close of root element" },
  { 122, "possible invalid start of a document type" },
  { 123, "duplicate document type" },
  { 124, "root element name must start with [A-Za-z_:]" },
  { 125, "first attribute name must start with [A-Za-z_:]" },
  { 126, "invalid character in or after element name" },
  { 127, "attribute name not followed by '=' " },
  { 128, "invalid attribute value delimiter" },
  { 130, "attribute name must start with [A-Za-z_:]" },
  { 131, "invalid character in or after attribute name" },
  { 132, "empty element tag not terminated with '/>'" },
  { 133, "element end tag name name must start with [A-Za-z_:]" },
  { 134, "element end tag not terminated with '>'" },
  { 135, "element name must start with [A-Za-z_:]" },
  { 136, "invalid start of comment/CDATA in element" },
  { 137, "invalid start of comment" },
  { 138, "processing instruction target name must start with [A-Za-z_:]" },
  { 139, "invalid character in/afterprocessing instruction target name" },
  { 140, "processing instruction not terminated with '?>'" },
  { 141, "invalid character following '&' in a character/entity reference" },
  { 142, "missing version information in XML declaration" },
  { 143, "missing '=' after 'version' in XML declaration " },
  { 144, "missing XML version declaration " },
  { 145, "invalid character in XML version information" },
  { 146, "invalid character following XML version information value " },
  { 147, "invalid attribute in XML declaration" },
  { 148, "missing '=' after 'encoding' in XML declaration" },
  { 149, "missing XML encoding declaration value" },
  { 150, "invalid XML encoding declaration value" },
  { 151, "invalid character afer XML declaration" },
  { 152, "invalid attribute  XML declaration" },
  { 153, "missing '=' after standalone  XML declaration" },
  { 154, "missing standalone XML declaration value" },
  { 155, "standalone declaration must be 'yes' or 'no'" },
  { 156, "invalid standalone XML declaration value" },
  { 157, "invalid character following XML standalone declaration value" },
  { 158, "unterminated XML declaration " },
  { 159, "start of document type declaration after end of root element" },
  { 160, "start of element after end of root element" },
  { 161, "invalid UTF-8 byte sequence" },
  { 162, "UTF-8 character that has a Unicode code point above x'FFFF'" },
  { 315, "UTF-16 document  little-endian unsupported" },
  { 316, "UCS4 document unsupported" },
  { 317, "unrecognized document encoding" },
  { 318, "UTF-8 document unsupported " },
  { 320, "mismatched national document data item to document encoding EBCDIC" },
  { 321, "mismatched national document data item to document encoding ASCII" },
  { 322, "mismatched native alphanumeric document data item to document encoding EBCDIC" },
  { 323, "mismatched host alphanumeric document data item to document encoding ASCII" },
  { 324, "mismatched national document data item to document encoding UTF-8" },
  { 325, "mismatched host alphanumeric document datat to document encoding UTF-8" },
  { 500, "internal error" },
}, *eoxml_ec_values = xml_ec_values + COUNT_OF(xml_ec_values);

static const xml_ec_value_t *
xml_ec_value_of( int ibm_code ) {
  if( 100000 < ibm_code && ibm_code < 200000 ) {
    static xml_ec_value_t not_ebcdic{ 0, "EBCDIC document encoding "
                                       "does not match code page" };
    not_ebcdic.ibm_code = ibm_code;
    return &not_ebcdic;
  }
  if( 200000 < ibm_code && ibm_code < 300000 ) {
    static xml_ec_value_t not_ascii{ 0, "ASCII document encoding "
                                       "does not match code page" };
    not_ascii.ibm_code = ibm_code;
    return &not_ascii;
  }
  auto p = std::find_if( xml_ec_values, eoxml_ec_values,
                         [ibm_code]( const auto& value ) {
                           return ibm_code == value.ibm_code;
                         } );
  return p < eoxml_ec_values ? &*p : nullptr;
}

const char *
xml_ec_value_str( int ibm_code ) {
  auto p = xml_ec_value_of(ibm_code);
  return p? p->msg : nullptr;
}

#if NEEDED
static bool
xml_fatal( int ibm_code ) {
  if( ibm_code < 100 ) return false;
  if( ibm_code > 100000 ) return false;
  assert(ibm_code < 1000);
  return true;
}
#endif

static callback_t *cobol_callback;

/*
 * Internal handler functions
 */
///////////////
/*

ATTRIBUTE-CHARACTER The single character that corresponds with the predefined entity reference in the attribute value
ATTRIBUTE-CHARACTERS The value within quotation marks or apostrophes. This can be a substring of the attribute value if the value includes an entity reference.
ATTRIBUTE-NAME The attribute name; the string to the left of the equal sign
ATTRIBUTE-NATIONAL-CHARACTER Regardless of the type of the XML document specified by identifier-1 in the XML PARSE statement, XML-TEXT is empty with length zero and XML-NTEXT contains the single national character that corresponds with the numeric character reference.

CONTENT-CHARACTER The single character that corresponds with the predefined entity reference in the element content

CONTENT-NATIONAL-CHARACTER Regardless of the type of the XML document specified by identifier-1 in the XML PARSE statement, XML-TEXT is empty with length zero and XML-NTEXT contains the single national character that corresponds with the numeric character reference.1
DOCUMENT-TYPE-DECLARATION The entire document type declaration, including the opening and closing character sequences "<!DOCTYPE" and ">"
ENCODING-DECLARATION The value, between quotes or apostrophes, of the encoding declaration in the XML declaration
END-OF-CDATA-SECTION The string "]]>"
END-OF-DOCUMENT Empty with length zero

EXCEPTION The part of the document that was successfully scanned, up to and including the point at which the exception was detected.2 Special register XML-CODE contains the unique error code that identifies the exception.

PROCESSING-INSTRUCTION-TARGET The processing instruction target name, which occurs immediately after the processing instruction opening sequence, "<?"
STANDALONE-DECLARATION The value, between quotation marks or apostrophes ("yes" or "no"), of the stand-alone declaration in the XML declaration
START-OF-CDATA-SECTION The string "<![CDATA["
START-OF-DOCUMENT The entire document

UNKNOWN-REFERENCE-IN-CONTENT The entity reference name, not including the "&" and ";" delimiters
UNKNOWN-REFERENCE-IN-ATTRIBUTE The entity reference name, not including the "&" and ";" delimiters
VERSION-INFORMATION The value, between quotation marks or apostrophes, of the version information in the XML declaration

*/
///////////////

extern cblc_field_t __ggsr__xml_event;
extern cblc_field_t __ggsr__xml_code;
extern cblc_field_t __ggsr__xml_text;
extern cblc_field_t __ggsr__xml_ntext;

static void
xml_event( const char event_name[], size_t len, char text[] ) {
  assert(strlen(event_name) < __ggsr__xml_event.allocated);

  auto pend = __ggsr__xml_event.data + __ggsr__xml_event.allocated;
  auto p = std::copy( event_name, event_name + strlen(event_name),
                      PTRCAST(char, __ggsr__xml_event.data) );
  std::fill(PTRCAST(unsigned char, p), pend, 0x20);

  __ggsr__xml_text.data = reinterpret_cast<unsigned char*>(text);
  __ggsr__xml_text.capacity = __ggsr__xml_text.allocated = len;
  __ggsr__xml_code.data = 0;
  cobol_callback();
}

static inline void
xml_event( const char event_name[], char text[] ) {
  xml_event(event_name, strlen(text), text);
}

static inline void
xml_event( const char event_name[], size_t len, const xmlChar * value ) {
  char *text = reinterpret_cast<char*>(const_cast<xmlChar*>(value));
  xml_event(event_name, len, text);
}

static inline void
xml_event( const char event_name[], const xmlChar * value ) {
  char *text = reinterpret_cast<char*>(const_cast<xmlChar*>(value));
  xml_event(event_name, strlen(text), text);
}

/*
 * Many static handler functions are defined but not used while we learn what
 * is needed.
 */
#pragma GCC diagnostic ignored "-Wunused-function"

static void attributeDecl(void * CTX,
                          const xmlChar * elem,
                          const xmlChar * fullname,
                          int type __attribute__ ((unused)),
                          int def  __attribute__ ((unused)),
                          const xmlChar * defaultValue,
                          xmlEnumerationPtr tree __attribute__ ((unused)) )
{
  fprintf(stderr, "%s:%d: elem=%s, name=%s, default=%s\n",
          __func__, __LINE__, elem, fullname, defaultValue);
}

static void cdataBlock(void * CTX,
                       const xmlChar * data,
                       int len)
{
  SAYSO_DATA(len, data);
  xml_event("CONTENT-CHARACTERS", len, data);
}

static void characters(void * CTX,
                       const xmlChar * data,
                       int len)
{
  SAYSO_DATA(len, data);
  xml_event("CONTENT-CHARACTERS", len, data);
}

static void comment(void * CTX, const xmlChar * value) {
  SAYSO_DATAZ(value);
  xml_event("COMMENT", value);
}

static void elementDecl(void * CTX,
                        const xmlChar * name,
                        int type __attribute__ ((unused)),
                        xmlElementContentPtr content __attribute__ ((unused)) )
{ SAYSO_DATAZ(name); }

static void endDocument(void * CTX)
{ SAYSO(); }

static void endElementNs(void * CTX,
                         const xmlChar * localname,
                         const xmlChar * prefix,
                         const xmlChar * URI __attribute__ ((unused)) )
{
  SAYSO_DATAZ(prefix);
  SAYSO_DATAZ(localname);
  xml_event("END-OF-ELEMENT", localname);
}

static void endElement(void * CTX,
                       const xmlChar * name)
{ SAYSO_DATAZ(name); }

static void entityDecl(void * CTX,
                       const xmlChar * name,
                       int type __attribute__ ((unused)),
                       const xmlChar * publicId __attribute__ ((unused)),
                       const xmlChar * systemId __attribute__ ((unused)),
                       xmlChar * content )
{
  SAYSO_DATAZ(name);
  SAYSO_DATAZ(content);
}

static void error(void * CTX, const char * msg, ...)
{
  va_list ap;
  va_start (ap, msg);
  fprintf(stderr, "error: ");
  vfprintf(stderr, msg, ap);
  fprintf(stderr, "\n");
  va_end (ap);
}

static void externalSubset(void * CTX,
                           const xmlChar * name,
                           const xmlChar * ExternalID,
                           const xmlChar * SystemID)
{
  SAYSO_DATAZ(name);
  SAYSO_DATAZ(ExternalID);
  SAYSO_DATAZ(SystemID);
}

static void fatalError(void * CTX, const char * msg, ...)
{
  va_list ap;
  va_start (ap, msg);
  fprintf(stderr, "fatal: ");
  vfprintf(stderr, msg, ap);
  fprintf(stderr, "\n");
  va_end (ap);
}

#if 0

static xmlEntityPtr getEntity(void * CTX,
                              const xmlChar * name)
{ SAYSO_DATAZ(name); }

static xmlEntityPtr getParameterEntity(void * CTX,
                                       const xmlChar * name)
{ SAYSO_DATAZ(name); }
#endif

static int hasExternalSubset(void * CTX)
{ SAYSO(); return 0; }

static int hasInternalSubset(void * CTX)
{ SAYSO(); return 0; }

static void ignorableWhitespace(void * CTX,
                                const xmlChar * ch,
                                int len)
{ SAYSO_DATA(len, ch); }

static void internalSubset(void * CTX,
                           const xmlChar * name,
                           const xmlChar * ExternalID,
                           const xmlChar * SystemID)
{
  SAYSO_DATAZ(name);
  SAYSO_DATAZ(ExternalID);
  SAYSO_DATAZ(SystemID);
}

#if 0
static int isStandalone (void * CTX)
{ SAYSO(); }
#endif

static void notationDecl(void * CTX,
                         const xmlChar * name,
                         const xmlChar * publicId,
                         const xmlChar * systemId)
{
  SAYSO_DATAZ(name);
  SAYSO_DATAZ(publicId);
  SAYSO_DATAZ(systemId);
}

static void processingInstruction(void * CTX,
                                  const xmlChar * target,
                                  const xmlChar * data)
{
  SAYSO_DATAZ(target);
  xml_event("PROCESSING-INSTRUCTION-TARGET", target);
  SAYSO_DATAZ(data);
  xml_event("PROCESSING-INSTRUCTION-DATA", data);
}

static void reference(void * CTX,
                      const xmlChar * name)
{ SAYSO_DATAZ(name); }

#if 0
static xmlParserInputPtr resolveEntity( void * CTX,
                                        const xmlChar * publicId,
                                        const xmlChar * systemId)
{ SAYSO(); }
#endif

static void setDocumentLocator(void * CTX,
                               xmlSAXLocatorPtr loc __attribute__ ((unused)) )
{ SAYSO(); }

/*
 * Called after the XML declaration was parsed.
 * Use xmlCtxtGetVersion(), xmlCtxtGetDeclaredEncoding() and
 * xmlCtxtGetStandalone() to get data from the XML declaration.
 */
static void startDocument(void * CTX)

{
  SAYSO();
}

static void startElementNs(void * CTX,
                           const xmlChar * localname,
                           const xmlChar * prefix,
                           const xmlChar * URI,
                           int nb_namespaces __attribute__ ((unused)),
                           const xmlChar ** namespaces __attribute__ ((unused)),
                           int nb_attributes __attribute__ ((unused)),
                           int nb_defaulted __attribute__ ((unused)),
                           const xmlChar ** attributes __attribute__ ((unused)))
{
  SAYSO_DATAZ(prefix);
  SAYSO_DATAZ(URI);
  SAYSO_DATAZ(localname);
  xml_event("START-OF-ELEMENT", localname);
}

static void startElement(void * CTX,
                         const xmlChar * name,
                         const xmlChar ** atts)
{
  SAYSO_DATAZ(name);
  for( int i=0; atts[i]; i++ ) SAYSO_DATAZ(atts[i]);
}

static void unparsedEntityDecl(void * CTX,
                               const xmlChar * name,
                               const xmlChar * publicId,
                               const xmlChar * systemId,
                               const xmlChar * notationName)
{
  SAYSO_DATAZ(name);
  SAYSO_DATAZ(publicId);
  SAYSO_DATAZ(systemId);
  SAYSO_DATAZ(notationName);
}

static void warning(void * CTX, const char * msg, ... )
{
  va_list ap;
  va_start (ap, msg);
  fprintf(stderr, "warning: ");
  vfprintf(stderr, msg, ap);
  fprintf(stderr, "\n");
  va_end (ap);
}

/*
 * xmlSAXHandler is a structure of function pointers that the SAX parser calls
 * as it encounters XML elements in the input.  Each pointer is a callback
 * function, locally defined in this file. These we term "handlers".
 *
 * Each handler sets the XML registers per IBM, and then calls
 * cobol_callback(), which is a function pointer supplied by the COBOL program
 * to be the processing procedure for XML PARSE.
 *
 * There is no obvious way to abort parsing at the C level.  See:
 *     http://veillard.com/XML/messages/0540.html
 *
 * > The simplest to implement this would not be to add a new SAX
 * > callback but rather modify the xmlParserCtxtPtr passed to the
 * > callbacks. The best seems to be:
 * > - set ctxt->instate to XML_PARSER_EOF
 * > - hack xmlCurrentChar() to return 0
 * > if (ctxt->instate == XML_PARSER_EOF)
 * > Doing both should led to a quick termination of parsing
 * > (but endElement(s)/endDocument will certainly be called anyway).
 *
 * Another hack might be to set the input to all blanks in cobol_callback.
 */

static xmlSAXHandler handlers;

static void
initialize_handlers( callback_t *callback ) {
  handlers = xmlSAXHandler {};
  handlers.initialized = XML_SAX2_MAGIC;

  cobol_callback = callback;

#if 0
  //// Should typically not be modified
  handlers.attributeDecl = attributeDecl;
  handlers.elementDecl = elementDecl;
  handlers.entityDecl = entityDecl;
  handlers.externalSubset = externalSubset;
  handlers.getEntity = getEntity;
  handlers.getParameterEntity = getParameterEntity;
  handlers.internalSubset = internalSubset;
  handlers.notationDecl = notationDecl;
  handlers.resolveEntity = resolveEntity;
  handlers.unparsedEntityDecl = unparsedEntityDecl;

  //// Not supposed to be changed by applications
  handlers.hasExternalSubset = hasExternalSubset;
  handlers.hasInternalSubset = hasInternalSubset;
  handlers.isStandalone = isStandalone;

  //// SAX 1 only
  handlers.startElement = startElement;
  handlers.endElement = endElement;

  //// Everything is available on the context, so this is useless in our case
  handlers.setDocumentLocator = setDocumentLocator;
#endif

  handlers.cdataBlock = cdataBlock;
  handlers.characters = characters;
  handlers.comment = comment;
  handlers.endDocument = endDocument;
  handlers.endElementNs = endElementNs;
  handlers.ignorableWhitespace = ignorableWhitespace;
  handlers.processingInstruction = processingInstruction;
  handlers.reference = reference;
  handlers.startDocument = startDocument;
  handlers.startElementNs = startElementNs;
  handlers.error = error;
  handlers.fatalError = fatalError;
  handlers.warning = warning;
}

static xmlChar *
xmlchar_of( const char input[] ) {
  return const_cast<xmlChar*>( reinterpret_cast<const xmlChar*>(input) );
}

static const char *
xmlParserErrors_str( xmlParserErrors erc, const char name[] ) {
  const char *msg = "???";
  switch( erc ) {
  case XML_ERR_OK:
    msg = "Success";
    break;
  case XML_ERR_INTERNAL_ERROR:
    msg = "Internal assertion failure";
    break;
  case XML_ERR_NO_MEMORY:
    msg = "Out of memory";
    break;
  case XML_ERR_UNSUPPORTED_ENCODING:
    msg = "Unsupported character encoding";
    break;

#if LIBXML_VERSION >= 21400
  case XML_ERR_RESOURCE_LIMIT:
    msg = "Internal resource limit like maximum amplification factor exceeded";
    break;
  case XML_ERR_ARGUMENT:
    msg = "Invalid argument";
    break;
  case XML_ERR_SYSTEM:
    msg = "Unexpected error from the OS or an external library";
    break;
#endif
  case XML_IO_ENOENT:
    msg = "File not found";
    break;
  default:
    msg = strdup(name);
    if( ! msg ) msg = "unknown XML error";
    break;
  }
  return msg;
}

#define xmlerror_str(E) xmlParserErrors_str( (E), #E )

/*
 * The global context is NULL if XML PARSE is not in progress.
 */
static class context_t {
  const int priority;
 public:
  xmlParserCtxt * ctxt;
  context_t() : ctxt(nullptr), priority(LOG_INFO) {
    const int option = LOG_PERROR, facility = LOG_USER;
#if HAVE_DECL_PROGRAM_INVOCATION_SHORT_NAME
    /* Declared in errno.h, when available.  */
    static const char * const ident = program_invocation_short_name;
#elif defined (HAVE_GETPROGNAME)
    /* Declared in stdlib.h.  */
    static const char * const ident = getprogname();
#else
    /* Avoid a NULL entry.  */
    static const char * const ident = "unnamed_COBOL_program";
#endif
    // TODO: Program to set option in library via command-line and/or environment.
    //       Library listens to program, not to the environment.
    openlog(ident, option, facility);

    initialize_handlers(nullptr);
  }

  void
  push( cblc_field_t *input_field, size_t input_offset, size_t len, bool done ) {
    if( ! ctxt ) {
      init();
    }
    assert(cobol_callback); // caller must set

    if( input_offset < len ) {
      int size = len - input_offset;
      const char *chunk = PTRCAST(char, input_field->data + input_offset);
      int terminate = done? 1 : 0;

      auto erc = (xmlParserErrors )xmlParseChunk( ctxt, chunk, size, terminate );
      if( erc != 0 ) {
        auto msg = xmlerror_str(erc);
        syslog(priority, "XML PARSE: XML error: %s", msg);
      }

      if( done ) this->done();

    }
  }

  void done() {
    if( ctxt ) {
      xmlFreeParserCtxt( ctxt );
      ctxt = nullptr;
    }
  }


 protected:
  void init() {
    const char *external_entities = nullptr;
    void * const user_data = nullptr;

    ctxt = xmlCreatePushParserCtxt( &handlers, user_data,
                                    nullptr, 0, external_entities);
  }
} context;

static int
xml_push_parse( cblc_field_t *input_field,
                  size_t        input_offset,
                  size_t        len,
                  cblc_field_t *encoding __attribute__ ((unused)),
                  cblc_field_t *validating __attribute__ ((unused)),
                  int           returns_national __attribute__ ((unused)),
                  void (*callback)(void) )
{
  ::cobol_callback = callback;

  context.push( input_field, input_offset, len, false);

#if LIBXML_VERSION >= 21400
  const xmlChar * version = xmlCtxtGetVersion( context.ctxt );
#else
  const xmlChar * version = xmlchar_of("requires version 2.14");
#endif
  assert(version);
  assert(nullptr == "function not ready and not called");
  return 0;
}


extern "C"  // Parser calls via parser_xml_parse_end, probabably.
int
__gg__xml_parse_done() {
  context.done();
  return 0;
}


extern "C"
int
__gg__xml_parse(  const cblc_field_t *input_field,
                  size_t        input_offset,
                  size_t        len,
                  cblc_field_t *encoding __attribute__ ((unused)),
                  cblc_field_t *validating __attribute__ ((unused)),
                  int           returns_national __attribute__ ((unused)),
                  void (*callback)(void) )
{
  initialize_handlers(callback);

  const char *input = PTRCAST(char, input_field->data + input_offset);

  int erc = xmlSAXUserParseMemory(&handlers, nullptr, input, len);

  if( erc ) {
    const xmlError *msg = xmlCtxtGetLastError(nullptr);
    fprintf(stderr, "XML PARSE: error: line %d: %s (%d: %d.%d.%d)\n",
            msg->line, msg->message, erc, msg->domain, msg->level, msg->code);
  }
  return erc;
}


