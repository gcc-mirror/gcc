// Implementation of <debugging> -*- C++ -*-

// Copyright The GNU Toolchain Authors.
//
// This file is part of the GNU ISO C++ Library.  This library is free
// software; you can redistribute it and/or modify it under the
// terms of the GNU General Public License as published by the
// Free Software Foundation; either version 3.

// This library is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.

// Under Section 7 of GPL version 3, you are granted additional
// permissions described in the GCC Runtime Library Exception, version
// 3.1, as published by the Free Software Foundation.

// You should have received a copy of the GNU General Public License and
// a copy of the GCC Runtime Library Exception along with this program;
// see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see
// <http://www.gnu.org/licenses/>.

#include <debugging>

#if __cpp_lib_debugging

#include <csignal> // std::raise

#if _GLIBCXX_USE_PROC_SELF_STATUS
# include <fstream>
# include <string>
#endif

#if _GLIBCXX_HAVE_SYS_PTRACE_H
# include <sys/types.h> // for darwin ptrace
# include <sys/ptrace.h>
# include <errno.h>
#endif

#if _GLIBCXX_HAVE_DEBUGAPI_H
# include <debugapi.h>
#endif

#ifdef _GLIBCXX_HAVE_SYS_SDT_H
# include <sys/sdt.h>
/* We only want to use stap probes starting with v3.  Earlier versions
   added too much startup cost.  */
# if defined (STAP_PROBE) && _SDT_NOTE_TYPE >= 3
#  define PROBE(name) STAP_PROBE(libstdcxx, name)
# endif
#endif

#ifndef PROBE
# define PROBE(name)
#endif

namespace __gnu_cxx
{
  // This should be changed to non-zero by debuggers when they attach
  // and back to zero when they detach.
  // If the value is positive, std::breakpoint() will use it as the argument
  // to std::raise, so it should be a valid signal number, e.g. SIGABRT or
  // SIGTRAP.
  // If the value is negative, std::breakpoint() will use a target-specific
  // trap, e.g. asm("int3") or __builtin_trap().
  volatile int debugger_signal_for_breakpoint = 0;
}

_GLIBCXX_WEAK_DEFINITION
bool
std::is_debugger_present() noexcept
{
  PROBE(std::is_debugger_present);

  if (__gnu_cxx::debugger_signal_for_breakpoint != 0)
    return true;

#if _GLIBCXX_HOSTED
# if _GLIBCXX_USE_PROC_SELF_STATUS
  const string_view prefix = "TracerPid:\t"; // populated since Linux 2.6.0
  ifstream in("/proc/self/status");
  string line;
  while (std::getline(in, line))
    {
      if (!line.starts_with(prefix))
	continue;

      string_view tracer = line;
      tracer.remove_prefix(prefix.size());
      if (tracer.size() == 1 && tracer[0] == '0') [[likely]]
	return false; // Not being traced.

      in.close();
      string_view cmd;
      string proc_dir = "/proc/" + string(tracer) + '/';
      in.open(proc_dir + "comm"); // since Linux 2.6.33
      if (std::getline(in, line)) [[likely]]
	cmd = line;
      else
	{
	  in.close();
	  in.open(proc_dir + "cmdline");
	  if (std::getline(in, line))
	    cmd = line.c_str(); // Only up to first '\0'
	  else
	    return false;
	}

      for (auto i : {"gdb", "gdbserver", "lldb-server"}) // known debuggers
	if (cmd.ends_with(i))
	  return true;

      // We found the TracerPid line, no need to do any more work.
      return false;
    }
# elif _GLIBCXX_USE_PTRACE
  if (::ptrace(PTRACE_TRACEME, 0, 1, 0) == -1)
    return errno == EPERM;
# endif
# if _GLIBCXX_HAVE_DEBUGAPI_H && defined(_WIN32) && !defined(__CYGWIN__)
  return IsDebuggerPresent();
# endif
#endif // HOSTED
  return false;
}

void
std::breakpoint() noexcept
{
  PROBE(std::breakpoint);

  if (__gnu_cxx::debugger_signal_for_breakpoint > 0)
    std::raise(__gnu_cxx::debugger_signal_for_breakpoint);

#if _GLIBCXX_HAVE_DEBUGAPI_H && defined(_WIN32) && !defined(__CYGWIN__)
  DebugBreak();
#elif __has_builtin(__builtin_debugtrap)
  __builtin_debugtrap(); // Clang
#elif defined(__i386__) || defined(__x86_64__)
  // nop is for GDB, see https://sourceware.org/bugzilla/show_bug.cgi?id=31194
  __asm__ volatile ("int $0x3\n\tnop");
#elifdef __thumb__
  __asm__ volatile (".inst 0xde01");
#elifdef __aarch64__
  __asm__ volatile (".inst 0xd4200000");
#elifdef __arm__
  __asm__ volatile (".inst 0xe7f001f0");
#elifdef __riscv
  /* section 2.8 in the RISC-V unprivileged ISA manual says for semi-hosted
   * environments we want the sequence:
   * slli x0, x0, 0x1f     # Entry NOP
   * ebreak         # Break to debugger
   * srai x0, x0, 7    # NOP encoding the semihosting call number 7
   */
  __asm__ volatile (".4byte 0x00100073");
#elif defined __powerpc__ && ! defined _AIX
  __asm__ volatile(".4byte 0x7d821008");
#else
  __builtin_trap();
#endif
} // If the debugger stops here, std::breakpoint() was called.

// This is intentionally not defined inline. A non-inline definition allows
// debuggers to insert a breakpoint on calls to the function, avoiding the
// overhead of calling `is_debugger_present()`.
void
std::breakpoint_if_debugging() noexcept
{
  PROBE(std::breakpoint_if_debugging);

  if (std::is_debugger_present()) [[unlikely]]
    std::breakpoint();
}

#endif // __cpp_lib_debugging
