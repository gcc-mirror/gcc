
#define UPC_SHARED_SECTION_BEGIN \
/* Establish a symbol at the beginning of the data section \
   Must take up some space, so that variables don't begin \
   at offset zero.  */ \
char UPC_SHARED_BEGIN_NAME [256]  \
     __attribute__((section(UPC_SHARED_SECTION_NAME)));

#define UPC_PGM_INFO_SECTION_BEGIN \
/* Establish a symbol at the beginning of the progam info data section */ \
char UPC_PGM_INFO_BEGIN_NAME []  \
     __attribute__((section(UPC_PGM_INFO_SECTION_NAME))) = {};

#define UPC_INIT_ARRAY_SECTION_BEGIN \
/* Establish a symbol at the beginning of the section that contains \
   a list of addresses pointing to UPC data initialization procedures. */ \
void (*UPC_INIT_ARRAY_BEGIN_NAME[]) (void) \
     __attribute__((section(UPC_INIT_ARRAY_SECTION_NAME))) = {};

#define UPC_SHARED_SECTION_END \
/* Establish a symbol at the end of the shared data section */ \
char UPC_SHARED_END_NAME []  \
     __attribute__((section(UPC_SHARED_SECTION_NAME))) = {'\0'};

#define UPC_PGM_INFO_SECTION_END \
/* Establish a symbol at the end of the progam info data section */ \
char UPC_PGM_INFO_END_NAME []  \
     __attribute__((section(UPC_PGM_INFO_SECTION_NAME))) = {'\0'};

#define UPC_INIT_ARRAY_SECTION_END \
/* Establish a symbol at the end of the UPC init. procedure table section.  */ \
void (*UPC_INIT_ARRAY_END_NAME[]) (void) \
     __attribute__((section(UPC_INIT_ARRAY_SECTION_NAME))) = {0};

