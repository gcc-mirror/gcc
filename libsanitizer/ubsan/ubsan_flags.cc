//===-- ubsan_flags.cc ----------------------------------------------------===//
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
//
// Runtime flags for UndefinedBehaviorSanitizer.
//
//===----------------------------------------------------------------------===//

#include "ubsan_platform.h"
#if CAN_SANITIZE_UB
#include "ubsan_flags.h"
#include "sanitizer_common/sanitizer_common.h"
#include "sanitizer_common/sanitizer_flags.h"
#include "sanitizer_common/sanitizer_flag_parser.h"

namespace __ubsan {

const char *MaybeCallUbsanDefaultOptions() {
  return (&__ubsan_default_options) ? __ubsan_default_options() : "";
}

Flags ubsan_flags;

void Flags::SetDefaults() {
#define UBSAN_FLAG(Type, Name, DefaultValue, Description) Name = DefaultValue;
#include "ubsan_flags.inc"
#undef UBSAN_FLAG
}

void RegisterUbsanFlags(FlagParser *parser, Flags *f) {
#define UBSAN_FLAG(Type, Name, DefaultValue, Description) \
  RegisterFlag(parser, #Name, Description, &f->Name);
#include "ubsan_flags.inc"
#undef UBSAN_FLAG
}

void InitializeFlags() {
  SetCommonFlagsDefaults();
  {
    CommonFlags cf;
    cf.CopyFrom(*common_flags());
    cf.print_summary = false;
    cf.external_symbolizer_path = GetEnv("UBSAN_SYMBOLIZER_PATH");
    OverrideCommonFlags(cf);
  }

  Flags *f = flags();
  f->SetDefaults();

  FlagParser parser;
  RegisterCommonFlags(&parser);
  RegisterUbsanFlags(&parser, f);

  // Override from user-specified string.
  parser.ParseString(MaybeCallUbsanDefaultOptions());
  // Override from environment variable.
  parser.ParseString(GetEnv("UBSAN_OPTIONS"));
  InitializeCommonFlags();
  if (Verbosity()) ReportUnrecognizedFlags();

  if (common_flags()->help) parser.PrintFlagDescriptions();
}

}  // namespace __ubsan

SANITIZER_INTERFACE_WEAK_DEF(const char *, __ubsan_default_options, void) {
  return "";
}

#endif  // CAN_SANITIZE_UB
