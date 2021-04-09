// CODYlib		-*- mode:c++ -*-
// Copyright (C) 2020 Nathan Sidwell, nathan@acm.org
// License: Apache v2.0

#ifndef CODY_HH
#define CODY_HH 1

// If the user specifies this as non-zero, it must be what we expect,
// generally only good for requesting no networking
#if !defined (CODY_NETWORKING)
// Have a known-good list of networking systems
#if defined (__unix__) || defined (__MACH__)
#define CODY_NETWORKING 1
#else
#define CODY_NETWORKING 0
#endif
#if 0  // For testing
#undef CODY_NETWORKING
#define CODY_NETWORKING 0
#endif
#endif

// C++
#include <memory>
#include <string>
#include <vector>
// C
#include <cstddef>
// OS
#include <errno.h>
#include <sys/types.h>
#if CODY_NETWORKING
#include <sys/socket.h>
#endif

namespace Cody {

// Set version to 1, as this is completely incompatible with 0.
// Fortunately both versions 0 and 1 will recognize each other's HELLO
// messages sufficiently to error out
constexpr unsigned Version = 1;

// FIXME: I guess we need a file-handle abstraction here
// Is windows DWORDPTR still?, or should it be FILE *? (ew).

namespace Detail  {

// C++11 doesn't have utf8 character literals :(

template<unsigned I>
constexpr char S2C (char const (&s)[I])
{
  static_assert (I == 2, "only single octet strings may be converted");
  return s[0];
}

/// Internal buffering class.  Used to concatenate outgoing messages
/// and Lex incoming ones.
class MessageBuffer
{
  std::vector<char> buffer;  ///< buffer holding the message
  size_t lastBol = 0;  ///< location of the most recent Beginning Of
		       ///< Line, or position we've readed when writing

public:
  MessageBuffer () = default;
  ~MessageBuffer () = default;
  MessageBuffer (MessageBuffer &&) = default;
  MessageBuffer &operator= (MessageBuffer &&) = default;

public:
  ///
  /// Finalize a buffer to be written.  No more lines can be added to
  /// the buffer.  Use before a sequence of Write calls.
  void PrepareToWrite ()
  {
    buffer.push_back (u8"\n"[0]);
    lastBol = 0;
  }
  ///
  /// Prepare a buffer for reading.  Use before a sequence of Read calls.
  void PrepareToRead ()
  {
    buffer.clear ();
    lastBol = 0;
  }

public:
  /// Begin a message line.  Use before a sequence of Append and
  /// related calls.
  void BeginLine ();
  /// End a message line.  Use after a sequence of Append and related calls.
  void EndLine () {}

public:
  /// Append a string to the current line.  No whitespace is prepended
  /// or appended.
  ///
  /// @param str the string to be written
  /// @param maybe_quote indicate if there's a possibility the string
  /// contains characters that need quoting.  Defaults to false.
  /// It is always safe to set
  /// this true, but that causes an additional scan of the string.
  /// @param len The length of the string.  If not specified, strlen
  /// is used to find the length.
  void Append (char const *str, bool maybe_quote = false,
	       size_t len = ~size_t (0));

  ///
  /// Add whitespace word separator.  Multiple adjacent whitespace is fine.
  void Space ()
  {
    Append (Detail::S2C(u8" "));
  }

public:
  /// Add a word as with Append, but prefixing whitespace to make a
  /// separate word
  void AppendWord (char const *str, bool maybe_quote = false,
		   size_t len = ~size_t (0))
  {
    if (buffer.size () != lastBol)
      Space ();
    Append (str, maybe_quote, len);
  }
  /// Add a word as with AppendWord
  /// @param str the string to append
  /// @param maybe_quote string might need quoting, as for Append
  void AppendWord (std::string const &str, bool maybe_quote = false)
  {
    AppendWord (str.data (), maybe_quote, str.size ());
  }
  ///
  /// Add an integral value, prepending a space.
  void AppendInteger (unsigned u);

private:
  /// Append a literal character.
  /// @param c character to append
  void Append (char c);

public:
  /// Lex the next input line into a vector of words.
  /// @param words filled with a vector of lexed strings
  /// @result 0 if no errors, an errno value on lexxing error such as
  /// there being no next line (ENOENT), or malformed quoting (EINVAL)
  int Lex (std::vector<std::string> &words);

public:
  /// Append the most-recently lexxed line to a string.  May be useful
  /// in error messages.  The unparsed line is appended -- before any
  /// unquoting.
  /// If we had c++17 string_view, we'd simply return a view of the
  /// line, and leave it to the caller to do any concatenation.
  /// @param l string to-which the lexxed line is appended.
  void LexedLine (std::string &l);

public:
  /// Detect if we have reached the end of the input buffer.
  /// I.e. there are no more lines to Lex
  /// @result True if at end
  bool IsAtEnd () const
  {
    return lastBol == buffer.size ();
  }

public:
  /// Read from end point into a read buffer, as with read(2).  This will
  /// not block , unless FD is blocking, and there is nothing
  /// immediately available.
  /// @param fd file descriptor to read from.  This may be a regular
  /// file, pipe or socket.
  /// @result on error returns errno.  If end of file occurs, returns
  /// -1.  At end of message returns 0.  If there is more needed
  /// returns EAGAIN (or possibly EINTR).  If the message is
  /// malformed, returns EINVAL.
  int Read (int fd) noexcept;

public:
  /// Write to an end point from a write buffer, as with write(2).  As
  /// with Read, this will not usually block.
  /// @param fd file descriptor to write to.  This may be a regular
  /// file, pipe or socket.
  /// @result on error returns errno.
  /// At end of message returns 0.  If there is more to write
  /// returns EAGAIN (or possibly EINTR).
  int Write (int fd) noexcept;
};

///
/// Request codes.  Perhaps this should be exposed?  These are likely
/// useful to servers that queue requests.
enum RequestCode
{
  RC_CONNECT,
  RC_MODULE_REPO,
  RC_MODULE_EXPORT,
  RC_MODULE_IMPORT,
  RC_MODULE_COMPILED,
  RC_INCLUDE_TRANSLATE,
  RC_HWM
};

/// Internal file descriptor tuple.  It's used as an anonymous union member.
struct FD
{
  int from;	///< Read from this FD
  int to;	///< Write to this FD
};

}

// Flags for various requests
enum class Flags : unsigned
{
  None,
  NameOnly = 1<<0,  // Only querying for CMI names, not contents
};

inline Flags operator& (Flags a, Flags b)
{
  return Flags (unsigned (a) & unsigned (b));
}
inline Flags operator| (Flags a, Flags b)
{
  return Flags (unsigned (a) | unsigned (b));
}

///
/// Response data for a request.  Returned by Client's request calls,
/// which return a single Packet.  When the connection is Corked, the
/// Uncork call will return a vector of Packets.
class Packet
{
public:
  ///
  /// Packet is a variant structure.  These are the possible content types.
  enum Category { INTEGER, STRING, VECTOR};

private:
  // std:variant is a C++17 thing, so we're doing this ourselves.
  union
  {
    size_t integer;	///< Integral value
    std::string string; ///< String value
    std::vector<std::string> vector;  ///< Vector of string value
  };
  Category cat : 2;  ///< Discriminator

private:
  unsigned short code = 0;  ///< Packet type
  unsigned short request = 0;

public:
  Packet (unsigned c, size_t i = 0)
    : integer (i), cat (INTEGER), code (c)
  {
  }
  Packet (unsigned c, std::string &&s)
    : string (std::move (s)), cat (STRING), code (c)
  {
  }
  Packet (unsigned c, std::string const &s)
    : string (s), cat (STRING), code (c)
  {
  }
  Packet (unsigned c, std::vector<std::string> &&v)
    : vector (std::move (v)), cat (VECTOR), code (c)
  {
  }
  // No non-move constructor from a vector.  You should not be doing
  // that.

  // Only move constructor and move assignment
  Packet (Packet &&t)
  {
    Create (std::move (t));
  }
  Packet &operator= (Packet &&t)
  {
    Destroy ();
    Create (std::move (t));

    return *this;
  }
  ~Packet ()
  {
    Destroy ();
  }

private:
  ///
  /// Variant move creation from another packet
  void Create (Packet &&t);
  ///
  /// Variant destruction
  void Destroy ();

public:
  ///
  /// Return the packet type
  unsigned GetCode () const
  {
    return code;
  }
  ///
  /// Return the packet type
  unsigned GetRequest () const
  {
    return request;
  }
  void SetRequest (unsigned r)
  {
    request = r;
  }
  ///
  /// Return the category of the packet's payload
  Category GetCategory () const
  {
    return cat;
  }

public:
  ///
  /// Return an integral payload.  Undefined if the category is not INTEGER
  size_t GetInteger () const
  {
    return integer;
  }
  ///
  /// Return (a reference to) a string payload.  Undefined if the
  /// category is not STRING
  std::string const &GetString () const
  {
    return string;
  }
  std::string &GetString ()
  {
    return string;
  }
  ///
  /// Return (a reference to) a constant vector of strings payload.
  /// Undefined if the category is not VECTOR
  std::vector<std::string> const &GetVector () const
  {
    return vector;
  }
  ///
  /// Return (a reference to) a non-conatant vector of strings payload.
  /// Undefined if the category is not VECTOR
  std::vector<std::string> &GetVector ()
  {
    return vector;
  }
};

class Server;

///
/// Client-side (compiler) object.
class Client
{
public:
  /// Response packet codes
  enum PacketCode
  {
    PC_CORKED,		///< Messages are corked
    PC_CONNECT,		///< Packet is integer version
    PC_ERROR,		///< Packet is error string
    PC_OK,
    PC_BOOL,
    PC_PATHNAME
  };

private:
  Detail::MessageBuffer write; ///< Outgoing write buffer
  Detail::MessageBuffer read;  ///< Incoming read buffer
  std::string corked; ///< Queued request tags
  union
  {
    Detail::FD fd;   ///< FDs connecting to server
    Server *server;  ///< Directly connected server
  };
  bool is_direct = false;  ///< Discriminator
  bool is_connected = false;  /// Connection handshake succesful

private:
  Client ();
public:
  /// Direct connection constructor.
  /// @param s Server to directly connect
  Client (Server *s)
    : Client ()
  {
    is_direct = true;
    server = s;
  }
  /// Communication connection constructor
  /// @param from file descriptor to read from
  /// @param to file descriptor to write to, defaults to from
  Client (int from, int to = -1)
    : Client ()
  {
    fd.from = from;
    fd.to = to < 0 ? from : to;
  }
  ~Client ();
  // We have to provide our own move variants, because of the variant member.
  Client (Client &&);
  Client &operator= (Client &&);

public:
  ///
  /// Direct connection predicate
  bool IsDirect () const
  {
    return is_direct;
  }
  ///
  /// Successful handshake predicate
  bool IsConnected () const
  {
    return is_connected;
  }

public:
  ///
  /// Get the read FD
  /// @result the FD to read from, -1 if a direct connection
  int GetFDRead () const
  {
    return is_direct ? -1 : fd.from;
  }
  ///
  /// Get the write FD
  /// @result the FD to write to, -1 if a direct connection
  int GetFDWrite () const
  {
    return is_direct ? -1 : fd.to;
  }
  ///
  /// Get the directly-connected server
  /// @result the server, or nullptr if a communication connection
  Server *GetServer () const
  {
    return is_direct ? server : nullptr;
  }

public:
  ///
  /// Perform connection handshake.  All othe requests will result in
  /// errors, until handshake is succesful.
  /// @param agent compiler identification
  /// @param ident compilation identifiation (maybe nullptr)
  /// @param alen length of agent string, if known
  /// @param ilen length of ident string, if known
  /// @result packet indicating success (or deferrment) of the
  /// connection, payload is optional flags
  Packet Connect (char const *agent, char const *ident,
		 size_t alen = ~size_t (0), size_t ilen = ~size_t (0));
  /// std::string wrapper for connection
  /// @param agent compiler identification
  /// @param ident compilation identification
  Packet Connect (std::string const &agent, std::string const &ident)
  {
    return Connect (agent.c_str (), ident.c_str (),
		    agent.size (), ident.size ());
  }

public:
  /// Request compiler module repository
  /// @result packet indicating repo
  Packet ModuleRepo ();

public:
  /// Inform of compilation of a named module interface or partition,
  /// or a header unit
  /// @param str module or header-unit
  /// @param len name length, if known
  /// @result CMI name (or deferrment/error)
  Packet ModuleExport (char const *str, Flags flags, size_t len = ~size_t (0));

  Packet ModuleExport (char const *str)
  {
    return ModuleExport (str, Flags::None, ~size_t (0));
  }
  Packet ModuleExport (std::string const &s, Flags flags = Flags::None)
  {
    return ModuleExport (s.c_str (), flags, s.size ());
  }

public:
  /// Importation of a module, partition or header-unit
  /// @param str module or header-unit
  /// @param len name length, if known
  /// @result CMI name (or deferrment/error)
  Packet ModuleImport (char const *str, Flags flags, size_t len = ~size_t (0));

  Packet ModuleImport (char const *str)
  {
    return ModuleImport (str, Flags::None, ~size_t (0));
  }
  Packet ModuleImport (std::string const &s, Flags flags = Flags::None)
  {
    return ModuleImport (s.c_str (), flags, s.size ());
  }

public:
  /// Successful compilation of a module interface, partition or
  /// header-unit.  Must have been preceeded by a ModuleExport
  /// request.
  /// @param str module or header-unit
  /// @param len name length, if known
  /// @result  OK (or deferment/error)
  Packet ModuleCompiled (char const *str, Flags flags, size_t len = ~size_t (0));

  Packet ModuleCompiled (char const *str)
  {
    return ModuleCompiled (str, Flags::None, ~size_t (0));
  }
  Packet ModuleCompiled (std::string const &s, Flags flags = Flags::None)
  {
    return ModuleCompiled (s.c_str (), flags, s.size ());
  }

  /// Include translation query.
  /// @param str header unit name
  /// @param len name length, if known
  /// @result  Packet indicating include translation boolean, or CMI
  /// name (or deferment/error)
  Packet IncludeTranslate (char const *str, Flags flags,
			   size_t len = ~size_t (0));

  Packet IncludeTranslate (char const *str)
  {
    return IncludeTranslate (str, Flags::None, ~size_t (0));
  }
  Packet IncludeTranslate (std::string const &s, Flags flags = Flags::None)
  {
    return IncludeTranslate (s.c_str (), flags, s.size ());
  }

public:
  /// Cork the connection.  All requests are queued up.  Each request
  /// call will return a PC_CORKED packet.
  void Cork ();

  /// Uncork the connection.  All queued requests are sent to the
  /// server, and a block of responses waited for.
  /// @result A vector of packets, containing the in-order responses to the
  /// queued requests.
  std::vector<Packet> Uncork ();
  ///
  /// Indicate corkedness of connection
  bool IsCorked () const
  {
    return !corked.empty ();
  }

private:
  Packet ProcessResponse (std::vector<std::string> &, unsigned code,
			  bool isLast);
  Packet MaybeRequest (unsigned code);
  int CommunicateWithServer ();
};

/// This server-side class is used to resolve requests from one or
/// more clients.  You are expected to derive from it and override the
/// virtual functions it provides.  The connection resolver may return
/// a different resolved object to service the remainder of the
/// connection -- for instance depending on the compiler that is
/// making the requests.
class Resolver
{
public:
  Resolver () = default;
  virtual ~Resolver ();

protected:
  /// Mapping from a module or header-unit name to a CMI file name.
  /// @param module module name
  /// @result CMI name
  virtual std::string GetCMIName (std::string const &module);

  /// Return the CMI file suffix to use
  /// @result CMI suffix, a statically allocated string
  virtual char const *GetCMISuffix ();

public:
  /// When the requests of a directly-connected server are processed,
  /// we may want to wait for the requests to complete (for instance a
  /// set of subjobs).
  /// @param s directly connected server.
  virtual void WaitUntilReady (Server *s);

public:
  /// Provide an error response.
  /// @param s the server to provide the response to.
  /// @param msg the error message
  virtual void ErrorResponse (Server *s, std::string &&msg);

public:
  /// Connection handshake.  Provide response to server and return new
  /// (or current) resolver, or nullptr.
  /// @param s server to provide response to
  /// @param version the client's version number
  /// @param agent the client agent (compiler identification)
  /// @param ident the compilation identification (may be empty)
  /// @result nullptr in the case of an error.  An error response will
  /// be sent.  If handing off to another resolver, return that,
  /// otherwise this
  virtual Resolver *ConnectRequest (Server *s, unsigned version,
				    std::string &agent, std::string &ident);

public:
  // return 0 on ok, ERRNO on failure, -1 on unspecific error
  virtual int ModuleRepoRequest (Server *s);

  virtual int ModuleExportRequest (Server *s, Flags flags,
				   std::string &module);
  virtual int ModuleImportRequest (Server *s, Flags flags,
				   std::string &module);
  virtual int ModuleCompiledRequest (Server *s, Flags flags,
				     std::string &module);
  virtual int IncludeTranslateRequest (Server *s, Flags flags,
				       std::string &include);
};


/// This server-side (build system) class handles a single connection
/// to a client.  It has 3 states, READING:accumulating a message
/// block froma client, WRITING:writing a message block to a client
/// and PROCESSING:resolving requests.  If the server does not spawn
/// jobs to build needed artifacts, the PROCESSING state will be brief.
class Server
{
public:
  enum Direction
  {
    READING,  // Server is waiting for completion of a (set of)
	      // requests from client.  The next state will be PROCESSING.
    WRITING,  // Server is writing a (set of) responses to client.
	      // The next state will be READING.
    PROCESSING  // Server is processing client request(s).  The next
		// state will be WRITING.
  };

private:
  Detail::MessageBuffer write;
  Detail::MessageBuffer read;
  Resolver *resolver;
  Detail::FD fd;
  bool is_connected = false;
  Direction direction : 2;

public:
  Server (Resolver *r);
  Server (Resolver *r, int from, int to = -1)
    : Server (r)
  {
    fd.from = from;
    fd.to = to >= 0 ? to : from;
  }
  ~Server ();
  Server (Server &&);
  Server &operator= (Server &&);

public:
  bool IsConnected () const
  {
    return is_connected;
  }

public:
  void SetDirection (Direction d)
  {
    direction = d;
  }

public:
  Direction GetDirection () const
  {
    return direction;
  }
  int GetFDRead () const
  {
    return fd.from;
  }
  int GetFDWrite () const
  {
    return fd.to;
  }
  Resolver *GetResolver () const
  {
    return resolver;
  }

public:
  /// Process requests from a directly-connected client.  This is a
  /// small wrapper around ProcessRequests, with some buffer swapping
  /// for communication.  It is expected that such processessing is
  /// immediate.
  /// @param from message block from client
  /// @param to message block to client
  void DirectProcess (Detail::MessageBuffer &from, Detail::MessageBuffer &to);

public:
  /// Process the messages queued in the read buffer.  We enter the
  /// PROCESSING state, and each message line causes various resolver
  /// methods to be called.  Once processed, the server may need to
  /// wait for all the requests to be ready, or it may be able to
  /// immediately write responses back.
  void ProcessRequests ();

public:
  /// Accumulate an error response.
  /// @param error the error message to encode
  /// @param elen length of error, if known
  void ErrorResponse (char const *error, size_t elen = ~size_t (0));
  void ErrorResponse (std::string const &error)
  {
    ErrorResponse (error.data (), error.size ());
  }

  /// Accumulate an OK response
  void OKResponse ();

  /// Accumulate a boolean response
  void BoolResponse (bool);

  /// Accumulate a pathname response
  /// @param path (may be nullptr, or empty)
  /// @param rlen length, if known
  void PathnameResponse (char const *path, size_t plen = ~size_t (0));
  void PathnameResponse (std::string const &path)
  {
    PathnameResponse (path.data (), path.size ());
  }

public:
  /// Accumulate a (successful) connection response
  /// @param agent the server-side agent
  /// @param alen agent length, if known
  void ConnectResponse (char const *agent, size_t alen = ~size_t (0));
  void ConnectResponse (std::string const &agent)
  {
    ConnectResponse (agent.data (), agent.size ());
  }

public:
  /// Write message block to client.  Semantics as for
  /// MessageBuffer::Write.
  /// @result errno or completion (0).
  int Write ()
  {
    return write.Write (fd.to);
  }
  /// Initialize for writing a message block.  All responses to the
  /// incomping message block must be complete  Enters WRITING state.
  void PrepareToWrite ()
  {
    write.PrepareToWrite ();
    direction = WRITING;
  }

public:
  /// Read message block from client.  Semantics as for
  /// MessageBuffer::Read.
  /// @result errno, eof (-1) or completion (0)
  int Read ()
  {
    return read.Read (fd.from);
  }
  /// Initialize for reading a message block.  Enters READING state.
  void PrepareToRead ()
  {
    read.PrepareToRead ();
    direction = READING;
  }
};

// Helper network stuff

#if CODY_NETWORKING
// Socket with specific address
int OpenSocket (char const **, sockaddr const *sock, socklen_t len);
int ListenSocket (char const **, sockaddr const *sock, socklen_t len,
		  unsigned backlog);

// Local domain socket (eg AF_UNIX)
int OpenLocal (char const **, char const *name);
int ListenLocal (char const **, char const *name, unsigned backlog = 0);

// ipv6 socket
int OpenInet6 (char const **e, char const *name, int port);
int ListenInet6 (char const **, char const *name, int port,
		 unsigned backlog = 0);
#endif

// FIXME: Mapping file utilities?

}

#endif // CODY_HH
