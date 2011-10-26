#define _XSTR(X) #X
#define _STR(S) _XSTR(S)

#define DARWIN_DEF_SECT_SYM(TYPE, SECT, SUFFIX) \
	__asm__ (".globl ___" _STR(SECT) "_" _STR(SUFFIX) "\n" \
	"\t.section __" _STR(TYPE) "," _STR(SECT) "\n" \
	"___" _STR(SECT) "_" _STR(SUFFIX) ":\n")

#define DARWIN_DEF_SECT_BEGIN(TYPE, SECT) \
        DARWIN_DEF_SECT_SYM (TYPE, SECT, start)

#define DARWIN_DEF_SECT_END(TYPE, SECT) \
        DARWIN_DEF_SECT_SYM (TYPE, SECT, end)

/* Define UPC sections via __asm__ as zero space cannot be
   allocated on Darwin OS via the usual method.  */

#define UPC_SHARED_SECTION_BEGIN \
	DARWIN_DEF_SECT_BEGIN (DATA, upc_shared); \
	__asm__ (".space 256\n");
#define UPC_SHARED_SECTION_END DARWIN_DEF_SECT_END (DATA, upc_shared);
#define UPC_PGM_INFO_SECTION_BEGIN DARWIN_DEF_SECT_BEGIN (DATA, upc_pgm_info);
#define UPC_PGM_INFO_SECTION_END DARWIN_DEF_SECT_END (DATA, upc_pgm_info);
#define UPC_INIT_SECTION_BEGIN DARWIN_DEF_SECT_BEGIN (TEXT, upc_init);
#define UPC_INIT_SECTION_END DARWIN_DEF_SECT_END (TEXT, upc_init);
#define UPC_INIT_ARRAY_SECTION_BEGIN DARWIN_DEF_SECT_BEGIN (DATA, upc_init_array);
#define UPC_INIT_ARRAY_SECTION_END DARWIN_DEF_SECT_END (DATA, upc_init_array);
