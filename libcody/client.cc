// CODYlib		-*- mode:c++ -*-
// Copyright (C) 2020 Nathan Sidwell, nathan@acm.org
// License: Apache v2.0

// Cody
#include "internal.hh"
// C
#include <cerrno>
#include <cstdlib>
#include <cstring>

// Client code

namespace Cody {

// These do not need to be members
static Packet ConnectResponse (std::vector<std::string> &words);
static Packet PathnameResponse (std::vector<std::string> &words);
static Packet OKResponse (std::vector<std::string> &words);
static Packet IncludeTranslateResponse (std::vector<std::string> &words);

// Must be consistently ordered with the RequestCode enum
static Packet (*const responseTable[Detail::RC_HWM])
  (std::vector<std::string> &) =
  {
    &ConnectResponse,
    &PathnameResponse,
    &PathnameResponse,
    &PathnameResponse,
    &OKResponse,
    &IncludeTranslateResponse,
  };

Client::Client ()
{
  fd.from = fd.to = -1;
}

Client::Client (Client &&src)
  : write (std::move (src.write)),
    read (std::move (src.read)),
    corked (std::move (src.corked)),
    is_direct (src.is_direct),
    is_connected (src.is_connected)
{
  if (is_direct)
    server = src.server;
  else
    {
      fd.from = src.fd.from;
      fd.to = src.fd.to;
    }
}

Client::~Client ()
{
}

Client &Client::operator= (Client &&src)
{
  write = std::move (src.write);
  read = std::move (src.read);
  corked = std::move (src.corked);
  is_direct = src.is_direct;
  is_connected = src.is_connected;
  if (is_direct)
    server = src.server;
  else
    {
      fd.from = src.fd.from;
      fd.to = src.fd.to;
    }

  return *this;
}

int Client::CommunicateWithServer ()
{
  write.PrepareToWrite ();
  read.PrepareToRead ();
  if (IsDirect ())
    server->DirectProcess (write, read);
  else
    {
      // Write the write buffer
      while (int e = write.Write (fd.to))
	if (e != EAGAIN && e != EINTR)
	  return e;
      // Read the read buffer
      while (int e = read.Read (fd.from))
	if (e != EAGAIN && e != EINTR)
	  return e;
    }

  return 0;
}

static Packet CommunicationError (int err)
{
  std::string e {u8"communication error:"};
  e.append (strerror (err));

  return Packet (Client::PC_ERROR, std::move (e));
}

Packet Client::ProcessResponse (std::vector<std::string> &words,
			       unsigned code, bool isLast)
{
  if (int e = read.Lex (words))
    {
      if (e == EINVAL)
	{
	  std::string msg (u8"malformed string '");
	  msg.append (words[0]);
	  msg.append (u8"'");
	  return Packet (Client::PC_ERROR, std::move (msg));
	}
      else
	return Packet (Client::PC_ERROR, u8"missing response");
    }

  Assert (!words.empty ());
  if (words[0] == u8"ERROR")
    return Packet (Client::PC_ERROR,
		   words.size () == 2 ? words[1]: u8"malformed error response");

  if (isLast && !read.IsAtEnd ())
    return Packet (Client::PC_ERROR,
		   std::string (u8"unexpected extra response"));

  Assert (code < Detail::RC_HWM);
  Packet result (responseTable[code] (words));
  result.SetRequest (code);
  if (result.GetCode () == Client::PC_ERROR && result.GetString ().empty ())
    {
      std::string msg {u8"malformed response '"};

      read.LexedLine (msg);
      msg.append (u8"'");
      result.GetString () = std::move (msg);
    }
  else if (result.GetCode () == Client::PC_CONNECT)
    is_connected = true;

  return result;
}

Packet Client::MaybeRequest (unsigned code)
{
  if (IsCorked ())
    {
      corked.push_back (code);
      return Packet (PC_CORKED);
    }

  if (int err = CommunicateWithServer ())
    return CommunicationError (err);

  std::vector<std::string> words;
  return ProcessResponse(words, code, true);
}

void Client::Cork ()
{
  if (corked.empty ())
    corked.push_back (-1);
}

std::vector<Packet> Client::Uncork ()
{
  std::vector<Packet> result;

  if (corked.size () > 1)
    {
      if (int err = CommunicateWithServer ())
	result.emplace_back (CommunicationError (err));
      else
	{
	  std::vector<std::string> words;
	  for (auto iter = corked.begin () + 1; iter != corked.end ();)
	    {
	      char code = *iter;
	      ++iter;
	      result.emplace_back (ProcessResponse (words, code,
						    iter == corked.end ()));
	    }
	}
    }

  corked.clear ();

  return result;
}

// Now the individual message handlers

// HELLO $vernum $agent $ident
Packet Client::Connect (char const *agent, char const *ident,
			  size_t alen, size_t ilen)
{
  write.BeginLine ();
  write.AppendWord (u8"HELLO");
  write.AppendInteger (Version);
  write.AppendWord (agent, true, alen);
  write.AppendWord (ident, true, ilen);
  write.EndLine ();

  return MaybeRequest (Detail::RC_CONNECT);
}

// HELLO $version $agent [$flags]
Packet ConnectResponse (std::vector<std::string> &words)
{
  if (words[0] == u8"HELLO" && (words.size () == 3 || words.size () == 4))
    {
      char *eptr;
      unsigned long val = strtoul (words[1].c_str (), &eptr, 10);

      unsigned version = unsigned (val);
      if (*eptr || version != val || version < Version)
	return Packet (Client::PC_ERROR, u8"incompatible version");
      else
	{
	  unsigned flags = 0;
	  if (words.size () == 4)
	    {
	      val = strtoul (words[3].c_str (), &eptr, 10);
	      flags = unsigned (val);
	    }
	  return Packet (Client::PC_CONNECT, flags);
	}
    }

  return Packet (Client::PC_ERROR, u8"");
}

// MODULE-REPO
Packet Client::ModuleRepo ()
{
  write.BeginLine ();
  write.AppendWord (u8"MODULE-REPO");
  write.EndLine ();

  return MaybeRequest (Detail::RC_MODULE_REPO);
}

// PATHNAME $dir | ERROR
Packet PathnameResponse (std::vector<std::string> &words)
{
  if (words[0] == u8"PATHNAME" && words.size () == 2)
    return Packet (Client::PC_PATHNAME, std::move (words[1]));

  return Packet (Client::PC_ERROR, u8"");
}

// OK or ERROR
Packet OKResponse (std::vector<std::string> &words)
{
  if (words[0] == u8"OK")
    return Packet (Client::PC_OK);
  else
    return Packet (Client::PC_ERROR,
		   words.size () == 2 ? std::move (words[1]) : "");
}

// MODULE-EXPORT $modulename [$flags]
Packet Client::ModuleExport (char const *module, Flags flags, size_t mlen)
{
  write.BeginLine ();
  write.AppendWord (u8"MODULE-EXPORT");
  write.AppendWord (module, true, mlen);
  if (flags != Flags::None)
    write.AppendInteger (unsigned (flags));
  write.EndLine ();

  return MaybeRequest (Detail::RC_MODULE_EXPORT);
}

// MODULE-IMPORT $modulename [$flags]
Packet Client::ModuleImport (char const *module, Flags flags, size_t mlen)
{
  write.BeginLine ();
  write.AppendWord (u8"MODULE-IMPORT");
  write.AppendWord (module, true, mlen);
  if (flags != Flags::None)
    write.AppendInteger (unsigned (flags));
  write.EndLine ();

  return MaybeRequest (Detail::RC_MODULE_IMPORT);
}

// MODULE-COMPILED $modulename [$flags]
Packet Client::ModuleCompiled (char const *module, Flags flags, size_t mlen)
{
  write.BeginLine ();
  write.AppendWord (u8"MODULE-COMPILED");
  write.AppendWord (module, true, mlen);
  if (flags != Flags::None)
    write.AppendInteger (unsigned (flags));
  write.EndLine ();

  return MaybeRequest (Detail::RC_MODULE_COMPILED);
}

// INCLUDE-TRANSLATE $includename [$flags]
Packet Client::IncludeTranslate (char const *include, Flags flags, size_t ilen)
{
  write.BeginLine ();
  write.AppendWord (u8"INCLUDE-TRANSLATE");
  write.AppendWord (include, true, ilen);
  if (flags != Flags::None)
    write.AppendInteger (unsigned (flags));
  write.EndLine ();

  return MaybeRequest (Detail::RC_INCLUDE_TRANSLATE);
}

// BOOL $knowntextualness
// PATHNAME $cmifile
Packet IncludeTranslateResponse (std::vector<std::string> &words)
{
  if (words[0] == u8"BOOL" && words.size () == 2)
    {
      if (words[1] == u8"FALSE")
	return Packet (Client::PC_BOOL, 0);
      else if (words[1] == u8"TRUE")
	return Packet (Client::PC_BOOL, 1);
      else
	return Packet (Client::PC_ERROR, u8"");
    }
  else
    return PathnameResponse (words);
}

}

