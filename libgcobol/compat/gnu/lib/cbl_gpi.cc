/*
 * Copyright (c) 2021-2026 Symas Corporation
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

#include <assert.h>
#include <inttypes.h>
#include <cstdint>
#include <cstdio>
#include <cstdlib>
#include <cstring>
#include <vector>
#include <string>

struct gpi_priv
  {
  gpi_priv(const std::vector<std::string> &callstack) :
    callstack(callstack),
    it(this->callstack.crbegin())
    {
    }
  const std::vector<std::string> callstack;
  std::vector<std::string>::const_reverse_iterator it;
  };

/* From the COBOL side, we add no padding bytes at all for groups. */
struct gpi_param_t
  {
  uint32_t size, flags;
  struct gpi_priv *handle;
  void *prog_id;
  uint32_t attrs;
  } __attribute__ ((packed));

struct retbuf_gpiai
  {
  uint32_t size, argc;
  void *reserved1, *reserved2;
  } __attribute__ ((packed));

enum
  {
  /* return cblte-gpi-handle. */
  GPI_PARAM_RET_HANDLE = 1 << 0,
  /* return program basename in name-buf. */
  GPI_PARAM_RET_BASENAME = 1 << 1,
  /* 0: space-terminated, 1: null-terminated. */
  GPI_PARAM_NULL_TERM = 1 << 2,
  /* return in cblte-gpi-attr */
  GPI_PARAM_RET_PRGATTR = 1 << 3,
  /* return for all programs */
  GPI_PARAM_ALL_PROG = 1 << 4,
  /* 0: return full path, 1: return program name. */
  GPI_PARAM_RET_PRGNAME = 1 << 5
  };

union retbuf
  {
  char *basename;
  struct retbuf_gpiai *gpiai ;
  };

enum cbl_backtrace_flags
  {
  CBL_BACKTRACE_AMODE24     = 1 << 0,
  CBL_BACKTRACE_AMODE31     = 1 << 1,
  CBL_BACKTRACE_EBCDIC      = 1 << 2,
  CBL_BACKTRACE_ANS85       = 1 << 3,
  CBL_BACKTRACE_VCS2        = 1 << 4,
  CBL_BACKTRACE_OSVS        = 1 << 5,
  CBL_BACKTRACE_DATA24      = 1 << 6,
  CBL_BACKTRACE_DATACONTEXT = 1 << 7,
  CBL_BACKTRACE_PLI         = 1 << 8,
  /* bits 9-10 reserved */
  CBL_BACKTRACE_BIGENDIAN   = 1 << 11,
  /* bits 12-22 reserved */
  CBL_BACKTRACE_AMODE64     = 1 << 23,
  CBL_BACKTRACE_DATA31      = 1 << 24
  };

static std::vector<std::string>
callstack()
  {
  const std::vector<std::string> &__gg__get_module_names(),
    &names = __gg__get_module_names();
  const char *s = NULL;
  std::vector<std::string> ret;

  for (const auto &name : names)
    {
    /* libgcobol.cc stores module names with a leading character
     * e.g.: 'T', 'N' or 'M'. */
    const char *cname = name.c_str() + 1;

    if (strcasecmp(cname, "CBL_GET_PROGRAM_INFO"))
      ret.push_back(cname);
    else
      break;
    }

  return ret;
  }

static void
setbasename(const std::string &src, const gpi_param_t &p, char *dst, unsigned n)
  {
  strncpy(dst, src.c_str(), n);

  if (n)
    {
    dst[n - 1] = '\0';

    if (!(p.flags & GPI_PARAM_NULL_TERM))
      dst[strlen(dst)] = ' ';
    }
  }

int16_t
cbl_gpi_cur_status(gpi_param_t &params, union retbuf buf, unsigned buflen)
  {
  std::vector<std::string> cs = callstack();

  if (cs.empty())
    return -1;
  else if (params.flags & GPI_PARAM_RET_BASENAME)
    setbasename(cs.back(), params, buf.basename, buflen);

  if (params.flags & GPI_PARAM_RET_HANDLE)
    params.handle = new gpi_priv(cs);

  /* TODO: set status bits */
  return 0;
  }

int16_t
cbl_gpi_named_status(gpi_param_t &params, union retbuf buf, unsigned buflen)
  {
  /* TODO: how are we supposed to return for a "named program"
   * if no name is given?*/
  fprintf(stderr, "%s: TODO\n", __func__);
  return -1;
  }

int16_t
cbl_gpi_parent_status(gpi_param_t &params, union retbuf buf, unsigned buflen)
  {
  gpi_priv *prv = params.handle;

  if (prv->it + 1 >= prv->callstack.crend())
    return -1;
  else if (params.flags & GPI_PARAM_RET_BASENAME)
    setbasename(*++prv->it, params, buf.basename, buflen);

  /* TODO: set status bits */
  return 0;
  }

int16_t
cbl_gpi_close_handle(gpi_param_t &params, union retbuf buf, unsigned buflen)
  {
  delete params.handle;
  /* TODO: set status bits */
  return 0;
  }

int16_t
cbl_gpi_first_ep(gpi_param_t &params, union retbuf buf, unsigned buflen)
  {
  fprintf(stderr, "%s: TODO\n", __func__);
  return -1;
  }

int16_t
cbl_gpi_next_ep(gpi_param_t &params, union retbuf buf, unsigned buflen)
  {
  fprintf(stderr, "%s: TODO\n", __func__);
  return -1;
  }

int16_t
cbl_gpi_cancel_search_ep(gpi_param_t &params, union retbuf buf, unsigned buflen)
  {
  fprintf(stderr, "%s: TODO\n", __func__);
  return -1;
  }

int16_t
cbl_gpi_prg_name(gpi_param_t &params, union retbuf buf, unsigned buflen)
  {
  fprintf(stderr, "%s: TODO\n", __func__);
  return -1;
  }

int16_t
cbl_gpi_nargs(gpi_param_t &params, union retbuf buf, unsigned buflen)
  {
  fprintf(stderr, "%s: TODO\n", __func__);
  return -1;
  }

int16_t
cbl_gpi_reserved(gpi_param_t &params, union retbuf buf, unsigned buflen)
  {
  fprintf(stderr, "%s: TODO\n", __func__);
  return -1;
  }

int16_t
cbl_gpi_path_prg_name(gpi_param_t &params, union retbuf buf, unsigned buflen)
  {
  fprintf(stderr, "%s: TODO\n", __func__);
  return -1;
  }

typedef int16_t (*gpi_func_t)(gpi_param_t &params, union retbuf buf,
  unsigned buflen);

static const struct func
  {
  gpi_func_t func;
  int accept, ignore, unimplemented;
  } funcs[11] =
  {
    {
    cbl_gpi_cur_status,
    GPI_PARAM_RET_HANDLE | GPI_PARAM_RET_BASENAME | GPI_PARAM_NULL_TERM
      | GPI_PARAM_RET_PRGATTR
    },

    {
    cbl_gpi_named_status,
    GPI_PARAM_RET_HANDLE | GPI_PARAM_RET_PRGATTR
    },

    {
    cbl_gpi_parent_status,
    GPI_PARAM_RET_BASENAME | GPI_PARAM_RET_PRGATTR | GPI_PARAM_NULL_TERM
      | GPI_PARAM_ALL_PROG,
    GPI_PARAM_RET_HANDLE
    },

    {
    cbl_gpi_close_handle,
    0,
    GPI_PARAM_RET_HANDLE
    },

    {
    cbl_gpi_first_ep,
    0,
    GPI_PARAM_RET_HANDLE
    },

    {
    cbl_gpi_next_ep,
    0,
    GPI_PARAM_RET_HANDLE
    },

    {
    cbl_gpi_cancel_search_ep,
    0,
    GPI_PARAM_RET_HANDLE
    },

    {
    cbl_gpi_prg_name,
    0,
    GPI_PARAM_RET_HANDLE
    },

    {
    cbl_gpi_nargs,
    0,
    GPI_PARAM_RET_HANDLE
    },

    {
    cbl_gpi_reserved,
    0,
    GPI_PARAM_RET_HANDLE
    },

    {
    cbl_gpi_path_prg_name,
    GPI_PARAM_RET_PRGNAME,
    GPI_PARAM_RET_HANDLE
    }
  };

static int checkflags(uint32_t fn, uint32_t flags)
  {
  const struct func &f = funcs[fn];

  for (uint32_t i = 0; i < 8 * sizeof flags; i++)
    {
    uint32_t mask = 1 << i;

    if (flags & mask)
      {
      if (!(f.accept & mask) && !(f.ignore & mask))
        {
        fprintf(stderr, "bit %" PRIu32 " not allowed for function %" PRIu32
          "\n", i, fn);
        return -1;
        }
      else if (f.unimplemented & mask)
        {
        fprintf(stderr, "bit %" PRIu32 " unimplemented for function %" PRIu32
          "\n", i, fn);
        return -1;
        }
      }
    }

  return 0;
  }

extern "C"
int16_t cbl_gpi(uint32_t fn, struct gpi_param_t &params, union retbuf buf,
  uint32_t *buflen)
  {
  if (fn >= sizeof funcs / sizeof *funcs)
    {
    fprintf(stderr, "invalid function number %u\n", fn);
    return -1;
    }

  if (params.size != sizeof params)
    {
    fprintf(stderr, "invalid cblte-gpi-size, expected %zu, got %u\n",
      sizeof params, params.size);
    assert(params.size == sizeof params);
    }

  if (checkflags(fn, params.flags))
    {
    fprintf(stderr, "invalid bits\n");
    return -1;
    }

  return funcs[fn].func(params, buf, *buflen);
  }
