// Symbols and macros for libstdc++ -*- C++ -*-

// @BOTTOM@
//
// Define symbol versioning in assember directives. If symbol
// versioning is beigng used, and the assembler supports this kind of
// thing, then use it.
// NB: _GLIBCXX_AT_AT is a hack to work around quoting issues in m4.
#if _GLIBCXX_SYMVER
  #define _GLIBCXX_ASM_SYMVER(cur, old, version) \
   asm (".symver " #cur "," #old _GLIBCXX_AT_AT #version);
#else
  #define _GLIBCXX_ASM_SYMVER(cur, old, version)
#endif
