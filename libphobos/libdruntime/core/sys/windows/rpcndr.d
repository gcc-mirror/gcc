/**
 * Windows API header module
 *
 * Translated from MinGW Windows headers
 *
 * License: $(LINK2 http://www.boost.org/LICENSE_1_0.txt, Boost License 1.0)
 * Source: $(DRUNTIMESRC core/sys/windows/_rpcndr.d)
 */
module core.sys.windows.rpcndr;
version (Windows):
pragma(lib, "rpcrt4");

/* Translation notes:
 RPC_CLIENT_ALLOC*, RPC_CLIENT_FREE* were replaced with PRPC_CLIENT_ALLOC, PRPC_CLIENT_FREE
*/

// TODO: Bitfields in MIDL_STUB_MESSAGE.
//       Macros need to be converted.
enum __RPCNDR_H_VERSION__= 450;

import core.sys.windows.rpcnsip;
import core.sys.windows.rpc, core.sys.windows.rpcdce, core.sys.windows.unknwn, core.sys.windows.windef;
import core.sys.windows.objidl; // for IRpcChannelBuffer, IRpcStubBuffer
import core.sys.windows.basetyps;

extern (Windows):

enum uint NDR_CHAR_REP_MASK      = 0xF,
    NDR_INT_REP_MASK              = 0xF0,
    NDR_FLOAT_REP_MASK            = 0xFF00,
    NDR_LITTLE_ENDIAN             = 0x10,
    NDR_BIG_ENDIAN                = 0,
    NDR_IEEE_FLOAT                = 0,
    NDR_VAX_FLOAT                 = 0x100,
    NDR_ASCII_CHAR                = 0,
    NDR_EBCDIC_CHAR               = 1,
    NDR_LOCAL_DATA_REPRESENTATION = 0x10,
    NDR_LOCAL_ENDIAN              = NDR_LITTLE_ENDIAN;

alias MIDL_user_allocate midl_user_allocate;
alias MIDL_user_free midl_user_free;

alias long hyper;
alias ulong MIDL_uhyper;
alias char small;

enum cbNDRContext=20;
//MACRO #define NDRSContextValue(hContext) (&(hContext)->userContext)
//MACRO #define byte_from_ndr(source, target) { *(target) = *(*(char**)&(source)->Buffer)++; }

//MACRO #define byte_array_from_ndr(Source, LowerIndex, UpperIndex, Target) { NDRcopy ((((char *)(Target))+(LowerIndex)), (Source)->Buffer, (unsigned int)((UpperIndex)-(LowerIndex))); *(unsigned long *)&(Source)->Buffer += ((UpperIndex)-(LowerIndex)); }

//MACRO #define boolean_from_ndr(source, target) { *(target) = *(*(char**)&(source)->Buffer)++; }

//MACRO #define boolean_array_from_ndr(Source, LowerIndex, UpperIndex, Target) { NDRcopy ((((char *)(Target))+(LowerIndex)), (Source)->Buffer, (unsigned int)((UpperIndex)-(LowerIndex))); *(unsigned long *)&(Source)->Buffer += ((UpperIndex)-(LowerIndex)); }

//MACRO #define small_from_ndr(source, target) { *(target) = *(*(char**)&(source)->Buffer)++; }

//MACRO #define small_from_ndr_temp(source, target, format) { *(target) = *(*(char**)(source))++; }

//MACRO #define small_array_from_ndr(Source, LowerIndex, UpperIndex, Target) { NDRcopy ((((char *)(Target))+(LowerIndex)), (Source)->Buffer, (unsigned int)((UpperIndex)-(LowerIndex))); *(unsigned long *)&(Source)->Buffer += ((UpperIndex)-(LowerIndex)); }

//MACRO #define MIDL_ascii_strlen(string) strlen(string)

//MACRO #define MIDL_ascii_strcpy(target,source) strcpy(target,source)

//MACRO #define MIDL_memset(s,c,n) memset(s,c,n)

//MACRO #define _midl_ma1( p, cast ) *(*( cast **)&p)++
//MACRO #define _midl_ma2( p, cast ) *(*( cast **)&p)++
//MACRO #define _midl_ma4( p, cast ) *(*( cast **)&p)++
//MACRO #define _midl_ma8( p, cast ) *(*( cast **)&p)++
//MACRO #define _midl_unma1( p, cast ) *(( cast *)p)++
//MACRO #define _midl_unma2( p, cast ) *(( cast *)p)++
//MACRO #define _midl_unma3( p, cast ) *(( cast *)p)++
//MACRO #define _midl_unma4( p, cast ) *(( cast *)p)++
//MACRO #define _midl_fa2( p ) (p = (RPC_BUFPTR )((LONG_PTR)(p+1) & 0xffffffff_fffffffe))
//MACRO #define _midl_fa4( p ) (p = (RPC_BUFPTR )((LONG_PTR)(p+3) & 0xffffffff_fffffffc))
//MACRO #define _midl_fa8( p ) (p = (RPC_BUFPTR )((LONG_PTR)(p+7) & 0xffffffff_fffffff8))
//MACRO #define _midl_addp( p, n ) (p += n)
//MACRO #define _midl_marsh_lhs( p, cast ) *(*( cast **)&p)++
//MACRO #define _midl_marsh_up( mp, p ) *(*(unsigned long **)&mp)++ = (unsigned long)p
//MACRO #define _midl_advmp( mp ) *(*(unsigned long **)&mp)++
//MACRO #define _midl_unmarsh_up( p ) (*(*(unsigned long **)&p)++)

//MACRO #define NdrMarshConfStringHdr( p, s, l ) (_midl_ma4( p, unsigned long) = s, _midl_ma4( p, unsigned long) = 0, _midl_ma4( p, unsigned long) = l)

//MACRO #define NdrUnMarshConfStringHdr(p, s, l) ((s=_midl_unma4(p,unsigned long), (_midl_addp(p,4)), (l=_midl_unma4(p,unsigned long))

//MACRO #define NdrMarshCCtxtHdl(pc,p) (NDRCContextMarshall( (NDR_CCONTEXT)pc, p ),p+20)
//MACRO #define NdrUnMarshCCtxtHdl(pc,p,h,drep) (NDRCContextUnmarshall((NDR_CONTEXT)pc,h,p,drep), p+20)
//MACRO #define NdrUnMarshSCtxtHdl(pc, p,drep) (pc = NdrSContextUnMarshall(p,drep ))
//MACRO #define NdrMarshSCtxtHdl(pc,p,rd) (NdrSContextMarshall((NDR_SCONTEXT)pc,p, (NDR_RUNDOWN)rd)

//MACRO #define NdrFieldOffset(s,f) (LONG_PTR)(& (((s *)0)->f))
//MACRO #define NdrFieldPad(s,f,p,t) (NdrFieldOffset(s,f) - NdrFieldOffset(s,p) - sizeof(t))
//MACRO #define NdrFcShort(s) (unsigned char)(s & 0xff), (unsigned char)(s >> 8)
//MACRO #define NdrFcLong(s) (unsigned char)(s & 0xff), (unsigned char)((s & 0x0000ff00) >> 8), (unsigned char)((s & 0x00ff0000) >> 16), (unsigned char)(s >> 24)

alias void * NDR_CCONTEXT;
struct tagNDR_SCONTEXT {
    void*[2] pad;
    void *userContext;
}
alias tagNDR_SCONTEXT * NDR_SCONTEXT;

struct SCONTEXT_QUEUE {
    uint NumberOfObjects;
    NDR_SCONTEXT *ArrayOfObjects;
}
alias SCONTEXT_QUEUE * PSCONTEXT_QUEUE;

struct _MIDL_STUB_MESSAGE;
struct _MIDL_STUB_DESC;
struct _FULL_PTR_XLAT_TABLES;

alias ubyte *RPC_BUFPTR;
alias uint RPC_LENGTH;

alias const(char)* PFORMAT_STRING;

struct ARRAY_INFO {
    int Dimension;
    uint *BufferConformanceMark;
    uint *BufferVarianceMark;
    uint *MaxCountArray;
    uint *OffsetArray;
    uint *ActualCountArray;
}
alias ARRAY_INFO * PARRAY_INFO;

RPC_BINDING_HANDLE  NDRCContextBinding(NDR_CCONTEXT);
void  NDRCContextMarshall(NDR_CCONTEXT,void*);
void  NDRCContextUnmarshall(NDR_CCONTEXT*,RPC_BINDING_HANDLE,void*,uint);
void  NDRSContextMarshall(NDR_SCONTEXT,void*,NDR_RUNDOWN);
NDR_SCONTEXT  NDRSContextUnmarshall(void*pBuff,uint);
void  RpcSsDestroyClientContext(void**);
void  NDRcopy(void*,void*,uint);
uint  MIDL_wchar_strlen(wchar *);
void  MIDL_wchar_strcpy(void*,wchar *);
void  char_from_ndr(PRPC_MESSAGE,ubyte*);
void  char_array_from_ndr(PRPC_MESSAGE,uint,uint,ubyte*);
void  short_from_ndr(PRPC_MESSAGE,ushort*);
void  short_array_from_ndr(PRPC_MESSAGE,uint,uint,ushort*);
void  short_from_ndr_temp(ubyte**,ushort*,uint);
void  int_from_ndr(PRPC_MESSAGE,uint*);
void  int_array_from_ndr(PRPC_MESSAGE,uint,uint,uint*);
void  int_from_ndr_temp(ubyte**,uint*,uint);
void  enum_from_ndr(PRPC_MESSAGE,uint*);
void  float_from_ndr(PRPC_MESSAGE,void*);
void  float_array_from_ndr(PRPC_MESSAGE,uint,uint,void*);
void  double_from_ndr(PRPC_MESSAGE,void*);
void  double_array_from_ndr(PRPC_MESSAGE,uint,uint,void*);
void  hyper_from_ndr(PRPC_MESSAGE,hyper*);
void  hyper_array_from_ndr(PRPC_MESSAGE,uint,uint,hyper*);
void  hyper_from_ndr_temp(ubyte**,hyper*,uint);
void  data_from_ndr(PRPC_MESSAGE,void*,char*,ubyte);
void  data_into_ndr(void*,PRPC_MESSAGE,char*,ubyte);
void  tree_into_ndr(void*,PRPC_MESSAGE,char*,ubyte);
void  data_size_ndr(void*,PRPC_MESSAGE,char*,ubyte);
void  tree_size_ndr(void*,PRPC_MESSAGE,char*,ubyte);
void  tree_peek_ndr(PRPC_MESSAGE,ubyte**,char*,ubyte);
void * midl_allocate(int);

align(4):
struct MIDL_STUB_MESSAGE {
    PRPC_MESSAGE RpcMsg;
    ubyte *Buffer;
    ubyte *BufferStart;
    ubyte *BufferEnd;
    ubyte *BufferMark;
    uint BufferLength;
    uint MemorySize;
    ubyte *Memory;
    int IsClient;
    int ReuseBuffer;
    ubyte *AllocAllNodesMemory;
    ubyte *AllocAllNodesMemoryEnd;
    int IgnoreEmbeddedPointers;
    ubyte *PointerBufferMark;
    ubyte fBufferValid;
    ubyte Unused;
    ULONG_PTR MaxCount;
    uint Offset;
    uint ActualCount;
    void* function (uint) pfnAllocate;
    void function (void*) pfnFree;
    ubyte * StackTop;
    ubyte * pPresentedType;
    ubyte * pTransmitType;
    handle_t SavedHandle;
const(_MIDL_STUB_DESC)* StubDesc;
    _FULL_PTR_XLAT_TABLES *FullPtrXlatTables;
    uint FullPtrRefId;
    int fCheckBounds;
    // FIXME:
    byte bit_fields_for_D; // FIXME: Bitfields
//  int fInDontFree :1;
//  int fDontCallFreeInst :1;
//  int fInOnlyParam :1;
//  int fHasReturn :1;
    uint dwDestContext;
    void* pvDestContext;
    NDR_SCONTEXT * SavedContextHandles;
    int ParamNumber;
    IRpcChannelBuffer  pRpcChannelBuffer;
    PARRAY_INFO pArrayInfo;
    uint * SizePtrCountArray;
    uint * SizePtrOffsetArray;
    uint * SizePtrLengthArray;
    void* pArgQueue;
    uint dwStubPhase;
    INT_PTR[5] w2kReserved;
}
alias MIDL_STUB_MESSAGE * PMIDL_STUB_MESSAGE;

extern (Windows) {
    alias void* function (void*) GENERIC_BINDING_ROUTINE;
    alias void function (void*,ubyte*) GENERIC_UNBIND_ROUTINE;
    alias uint function (uint *,uint,void *) USER_MARSHAL_SIZING_ROUTINE;
    alias ubyte * function (uint *,ubyte *,void *) USER_MARSHAL_MARSHALLING_ROUTINE;
    alias ubyte * function (uint *,ubyte *,void *) USER_MARSHAL_UNMARSHALLING_ROUTINE;
    alias void function (uint *,void *) USER_MARSHAL_FREEING_ROUTINE;
    alias void function () NDR_NOTIFY_ROUTINE;
}

align:
struct GENERIC_BINDING_ROUTINE_PAIR {
    GENERIC_BINDING_ROUTINE pfnBind;
    GENERIC_UNBIND_ROUTINE pfnUnbind;
}
alias GENERIC_BINDING_ROUTINE_PAIR * PGENERIC_BINDING_ROUTINE_PAIR;

struct GENERIC_BINDING_INFO {
    void *pObj;
    uint Size;
    GENERIC_BINDING_ROUTINE pfnBind;
    GENERIC_UNBIND_ROUTINE pfnUnbind;
}
alias GENERIC_BINDING_INFO * PGENERIC_BINDING_INFO;


struct XMIT_ROUTINE_QUINTUPLE {
    XMIT_HELPER_ROUTINE pfnTranslateToXmit;
    XMIT_HELPER_ROUTINE pfnTranslateFromXmit;
    XMIT_HELPER_ROUTINE pfnFreeXmit;
    XMIT_HELPER_ROUTINE pfnFreeInst;
}
alias XMIT_ROUTINE_QUINTUPLE * PXMIT_ROUTINE_QUINTUPLE;

struct MALLOC_FREE_STRUCT {
    void* function (uint) pfnAllocate;
    void function (void*) pfnFree;
}

struct COMM_FAULT_OFFSETS {
    short CommOffset;
    short FaultOffset;
}

struct USER_MARSHAL_ROUTINE_QUADRUPLE {
    USER_MARSHAL_SIZING_ROUTINE pfnBufferSize;
    USER_MARSHAL_MARSHALLING_ROUTINE pfnMarshall;
    USER_MARSHAL_UNMARSHALLING_ROUTINE pfnUnmarshall;
    USER_MARSHAL_FREEING_ROUTINE pfnFree;
}

enum IDL_CS_CONVERT {
    IDL_CS_NO_CONVERT,
    IDL_CS_IN_PLACE_CONVERT,
    IDL_CS_NEW_BUFFER_CONVERT
}

struct NDR_CS_SIZE_CONVERT_ROUTINES {
    CS_TYPE_NET_SIZE_ROUTINE pfnNetSize;
    CS_TYPE_TO_NETCS_ROUTINE pfnToNetCs;
    CS_TYPE_LOCAL_SIZE_ROUTINE pfnLocalSize;
    CS_TYPE_FROM_NETCS_ROUTINE pfnFromNetCs;
}

struct NDR_CS_ROUTINES {
    NDR_CS_SIZE_CONVERT_ROUTINES *pSizeConvertRoutines;
    CS_TAG_GETTING_ROUTINE *pTagGettingRoutines;
}

struct MIDL_STUB_DESC {
    void* RpcInterfaceInformation;
    void* function(uint) pfnAllocate;
    void function (void*) pfnFree;
    union _IMPLICIT_HANDLE_INFO {
        handle_t *pAutoHandle;
        handle_t *pPrimitiveHandle;
        PGENERIC_BINDING_INFO pGenericBindingInfo;
    }
    _IMPLICIT_HANDLE_INFO IMPLICIT_HANDLE_INFO;
const(NDR_RUNDOWN)* apfnNdrRundownRoutines;
const(GENERIC_BINDING_ROUTINE_PAIR)* aGenericBindingRoutinePairs;
const(EXPR_EVAL)* apfnExprEval;
const(XMIT_ROUTINE_QUINTUPLE)* aXmitQuintuple;
const(char)* *pFormatTypes;
    int fCheckBounds;
    uint Version;
    MALLOC_FREE_STRUCT *pMallocFreeStruct;
    int MIDLVersion;
const(COMM_FAULT_OFFSETS)* CommFaultOffsets;
const(USER_MARSHAL_ROUTINE_QUADRUPLE)* aUserMarshalQuadruple;
const(NDR_NOTIFY_ROUTINE)* NotifyRoutineTable;
    ULONG_PTR mFlags;
const(NDR_CS_ROUTINES)* CsRoutineTables;
    void *Reserved4;
    ULONG_PTR Reserved5;
}
alias const(MIDL_STUB_DESC)* PMIDL_STUB_DESC;

alias void * PMIDL_XMIT_TYPE;

struct MIDL_FORMAT_STRING {
    short Pad;
    ubyte[1] Format;
}

struct MIDL_SERVER_INFO {
    PMIDL_STUB_DESC pStubDesc;
const(SERVER_ROUTINE)* DispatchTable;
    PFORMAT_STRING ProcString;
const(ushort)* FmtStringOffset;
const(STUB_THUNK)* ThunkTable;
}
alias MIDL_SERVER_INFO * PMIDL_SERVER_INFO;

struct MIDL_STUBLESS_PROXY_INFO {
    PMIDL_STUB_DESC pStubDesc;
    PFORMAT_STRING ProcFormatString;
const(ushort)* FormatStringOffset;
}
alias MIDL_STUBLESS_PROXY_INFO *PMIDL_STUBLESS_PROXY_INFO;

union CLIENT_CALL_RETURN {
    void *Pointer;
    LONG_PTR Simple;
}

enum XLAT_SIDE {
    XLAT_SERVER = 1,
    XLAT_CLIENT
}
struct FULL_PTR_TO_REFID_ELEMENT {
    FULL_PTR_TO_REFID_ELEMENT * Next;
    void* Pointer;
    uint RefId;
    ubyte State;
}
alias FULL_PTR_TO_REFID_ELEMENT * PFULL_PTR_TO_REFID_ELEMENT;

struct FULL_PTR_XLAT_TABLES {
    struct _RefIdToPointer {
        void **XlatTable;
        ubyte *StateTable;
        uint NumberOfEntries;
    }
    void* RefIdToPointer;
    struct _PointerToRefId {
        PFULL_PTR_TO_REFID_ELEMENT *XlatTable;
        uint NumberOfBuckets;
        uint HashMask;
    }
    void* PointerToRefId;

    uint NextRefId;
    XLAT_SIDE XlatSide;
}
alias FULL_PTR_XLAT_TABLES * PFULL_PTR_XLAT_TABLES;


enum STUB_PHASE {
    STUB_UNMARSHAL,
    STUB_CALL_SERVER,
    STUB_MARSHAL,
    STUB_CALL_SERVER_NO_HRESULT
}

enum PROXY_PHASE {
    PROXY_CALCSIZE,
    PROXY_GETBUFFER,
    PROXY_MARSHAL,
    PROXY_SENDRECEIVE,
    PROXY_UNMARSHAL
}

alias TypeDef!(void *) RPC_SS_THREAD_HANDLE;

extern (Windows) {
alias void function (void*) NDR_RUNDOWN;
alias void function (_MIDL_STUB_MESSAGE*) EXPR_EVAL;
alias void function(PMIDL_STUB_MESSAGE) XMIT_HELPER_ROUTINE;
alias void function (RPC_BINDING_HANDLE,uint,uint,IDL_CS_CONVERT*,uint*,error_status_t*) CS_TYPE_NET_SIZE_ROUTINE;
alias void function (RPC_BINDING_HANDLE,uint,uint,IDL_CS_CONVERT*,uint*,error_status_t*) CS_TYPE_LOCAL_SIZE_ROUTINE;
alias void function (RPC_BINDING_HANDLE,uint,void*,uint,byte*,uint*,error_status_t*) CS_TYPE_TO_NETCS_ROUTINE;
alias void function (RPC_BINDING_HANDLE,uint,byte*,uint,uint,void*,uint*,error_status_t*) CS_TYPE_FROM_NETCS_ROUTINE;
alias void function (RPC_BINDING_HANDLE,int,uint*,uint*,uint*,error_status_t*) CS_TAG_GETTING_ROUTINE;

//alias void* RPC_CLIENT_ALLOC(uint);
//alias void RPC_CLIENT_FREE(void*);
alias void* function(uint) PRPC_CLIENT_ALLOC;
alias void function(void*) PRPC_CLIENT_FREE;

    alias void function (PMIDL_STUB_MESSAGE) STUB_THUNK;
    alias int function() SERVER_ROUTINE;
}

void  NdrSimpleTypeMarshall(PMIDL_STUB_MESSAGE,ubyte*,ubyte);
ubyte * NdrPointerMarshall(PMIDL_STUB_MESSAGE,ubyte*,PFORMAT_STRING pFormat);
ubyte * NdrSimpleStructMarshall(PMIDL_STUB_MESSAGE,ubyte*,PFORMAT_STRING);
ubyte * NdrConformantStructMarshall(PMIDL_STUB_MESSAGE,ubyte*,PFORMAT_STRING);
ubyte * NdrConformantVaryingStructMarshall(PMIDL_STUB_MESSAGE,ubyte*,PFORMAT_STRING);
ubyte * NdrHardStructMarshall(PMIDL_STUB_MESSAGE,ubyte*,PFORMAT_STRING);
ubyte * NdrComplexStructMarshall(PMIDL_STUB_MESSAGE,ubyte*,PFORMAT_STRING);
ubyte * NdrFixedArrayMarshall(PMIDL_STUB_MESSAGE,ubyte*,PFORMAT_STRING);
ubyte * NdrConformantArrayMarshall(PMIDL_STUB_MESSAGE,ubyte*,PFORMAT_STRING);
ubyte * NdrConformantVaryingArrayMarshall(PMIDL_STUB_MESSAGE,ubyte*,PFORMAT_STRING);
ubyte * NdrVaryingArrayMarshall(PMIDL_STUB_MESSAGE,ubyte*,PFORMAT_STRING);
ubyte * NdrComplexArrayMarshall(PMIDL_STUB_MESSAGE,ubyte*,PFORMAT_STRING);
ubyte * NdrNonConformantStringMarshall(PMIDL_STUB_MESSAGE,ubyte*,PFORMAT_STRING);
ubyte * NdrConformantStringMarshall(PMIDL_STUB_MESSAGE,ubyte*,PFORMAT_STRING);
ubyte * NdrEncapsulatedUnionMarshall(PMIDL_STUB_MESSAGE,ubyte*,PFORMAT_STRING);
ubyte * NdrNonEncapsulatedUnionMarshall(PMIDL_STUB_MESSAGE,ubyte*,PFORMAT_STRING);
ubyte * NdrByteCountPointerMarshall(PMIDL_STUB_MESSAGE,ubyte*,PFORMAT_STRING);
ubyte * NdrXmitOrRepAsMarshall(PMIDL_STUB_MESSAGE,ubyte*,PFORMAT_STRING);
ubyte * NdrInterfacePointerMarshall(PMIDL_STUB_MESSAGE,ubyte*,PFORMAT_STRING);
void  NdrClientContextMarshall(PMIDL_STUB_MESSAGE,NDR_CCONTEXT,int);
void  NdrServerContextMarshall(PMIDL_STUB_MESSAGE,NDR_SCONTEXT,NDR_RUNDOWN);
void  NdrSimpleTypeUnmarshall(PMIDL_STUB_MESSAGE,ubyte*,ubyte);
ubyte * NdrPointerUnmarshall(PMIDL_STUB_MESSAGE,ubyte**,PFORMAT_STRING,ubyte);
ubyte * NdrSimpleStructUnmarshall(PMIDL_STUB_MESSAGE,ubyte**,PFORMAT_STRING,ubyte);
ubyte * NdrConformantStructUnmarshall(PMIDL_STUB_MESSAGE,ubyte**,PFORMAT_STRING,ubyte);
ubyte * NdrConformantVaryingStructUnmarshall(PMIDL_STUB_MESSAGE,ubyte**,PFORMAT_STRING,ubyte);
ubyte * NdrHardStructUnmarshall(PMIDL_STUB_MESSAGE,ubyte**,PFORMAT_STRING,ubyte);
ubyte * NdrComplexStructUnmarshall(PMIDL_STUB_MESSAGE,ubyte**,PFORMAT_STRING,ubyte);
ubyte * NdrFixedArrayUnmarshall(PMIDL_STUB_MESSAGE,ubyte**,PFORMAT_STRING,ubyte);
ubyte * NdrConformantArrayUnmarshall(PMIDL_STUB_MESSAGE,ubyte**,PFORMAT_STRING,ubyte);
ubyte * NdrConformantVaryingArrayUnmarshall(PMIDL_STUB_MESSAGE,ubyte**,PFORMAT_STRING,ubyte);
ubyte * NdrVaryingArrayUnmarshall(PMIDL_STUB_MESSAGE,ubyte**,PFORMAT_STRING,ubyte);
ubyte * NdrComplexArrayUnmarshall(PMIDL_STUB_MESSAGE,ubyte**,PFORMAT_STRING,ubyte);
ubyte * NdrNonConformantStringUnmarshall(PMIDL_STUB_MESSAGE,ubyte**,PFORMAT_STRING,ubyte);
ubyte * NdrConformantStringUnmarshall(PMIDL_STUB_MESSAGE,ubyte**,PFORMAT_STRING,ubyte);
ubyte * NdrEncapsulatedUnionUnmarshall(PMIDL_STUB_MESSAGE,ubyte**,PFORMAT_STRING,ubyte);
ubyte * NdrNonEncapsulatedUnionUnmarshall(PMIDL_STUB_MESSAGE,ubyte**,PFORMAT_STRING,ubyte);
ubyte * NdrByteCountPointerUnmarshall(PMIDL_STUB_MESSAGE,ubyte**,PFORMAT_STRING,ubyte);
ubyte * NdrXmitOrRepAsUnmarshall(PMIDL_STUB_MESSAGE,ubyte**,PFORMAT_STRING,ubyte);
ubyte * NdrInterfacePointerUnmarshall(PMIDL_STUB_MESSAGE,ubyte**,PFORMAT_STRING,ubyte);
void  NdrClientContextUnmarshall(PMIDL_STUB_MESSAGE,NDR_CCONTEXT*,RPC_BINDING_HANDLE);
NDR_SCONTEXT  NdrServerContextUnmarshall(PMIDL_STUB_MESSAGE);
void  NdrPointerBufferSize(PMIDL_STUB_MESSAGE,ubyte*,PFORMAT_STRING);
void  NdrSimpleStructBufferSize(PMIDL_STUB_MESSAGE,ubyte*,PFORMAT_STRING);
void  NdrConformantStructBufferSize(PMIDL_STUB_MESSAGE,ubyte*,PFORMAT_STRING);
void  NdrConformantVaryingStructBufferSize(PMIDL_STUB_MESSAGE,ubyte*,PFORMAT_STRING);
void  NdrHardStructBufferSize(PMIDL_STUB_MESSAGE,ubyte*,PFORMAT_STRING);
void  NdrComplexStructBufferSize(PMIDL_STUB_MESSAGE,ubyte*,PFORMAT_STRING);
void  NdrFixedArrayBufferSize(PMIDL_STUB_MESSAGE,ubyte*,PFORMAT_STRING);
void  NdrConformantArrayBufferSize(PMIDL_STUB_MESSAGE,ubyte*,PFORMAT_STRING);
void  NdrConformantVaryingArrayBufferSize(PMIDL_STUB_MESSAGE,ubyte*,PFORMAT_STRING);
void  NdrVaryingArrayBufferSize(PMIDL_STUB_MESSAGE,ubyte*,PFORMAT_STRING);
void  NdrComplexArrayBufferSize(PMIDL_STUB_MESSAGE,ubyte*,PFORMAT_STRING);
void  NdrConformantStringBufferSize(PMIDL_STUB_MESSAGE,ubyte*,PFORMAT_STRING);
void  NdrNonConformantStringBufferSize(PMIDL_STUB_MESSAGE,ubyte*,PFORMAT_STRING);
void  NdrEncapsulatedUnionBufferSize(PMIDL_STUB_MESSAGE,ubyte*,PFORMAT_STRING);
void  NdrNonEncapsulatedUnionBufferSize(PMIDL_STUB_MESSAGE,ubyte*,PFORMAT_STRING);
void  NdrByteCountPointerBufferSize(PMIDL_STUB_MESSAGE,ubyte*,PFORMAT_STRING);
void  NdrXmitOrRepAsBufferSize(PMIDL_STUB_MESSAGE,ubyte*,PFORMAT_STRING);
void  NdrInterfacePointerBufferSize(PMIDL_STUB_MESSAGE,ubyte*,PFORMAT_STRING);
void  NdrContextHandleSize(PMIDL_STUB_MESSAGE,ubyte*,PFORMAT_STRING);
uint  NdrPointerMemorySize(PMIDL_STUB_MESSAGE,PFORMAT_STRING);
uint  NdrSimpleStructMemorySize(PMIDL_STUB_MESSAGE,PFORMAT_STRING);
uint  NdrConformantStructMemorySize(PMIDL_STUB_MESSAGE,PFORMAT_STRING);
uint  NdrConformantVaryingStructMemorySize(PMIDL_STUB_MESSAGE,PFORMAT_STRING);
uint  NdrHardStructMemorySize(PMIDL_STUB_MESSAGE,PFORMAT_STRING);
uint  NdrComplexStructMemorySize(PMIDL_STUB_MESSAGE,PFORMAT_STRING);
uint  NdrFixedArrayMemorySize(PMIDL_STUB_MESSAGE,PFORMAT_STRING);
uint  NdrConformantArrayMemorySize(PMIDL_STUB_MESSAGE,PFORMAT_STRING);
uint  NdrConformantVaryingArrayMemorySize(PMIDL_STUB_MESSAGE,PFORMAT_STRING);
uint  NdrVaryingArrayMemorySize(PMIDL_STUB_MESSAGE,PFORMAT_STRING);
uint  NdrComplexArrayMemorySize(PMIDL_STUB_MESSAGE,PFORMAT_STRING);
uint  NdrConformantStringMemorySize(PMIDL_STUB_MESSAGE,PFORMAT_STRING);
uint  NdrNonConformantStringMemorySize(PMIDL_STUB_MESSAGE,PFORMAT_STRING);
uint  NdrEncapsulatedUnionMemorySize(PMIDL_STUB_MESSAGE,PFORMAT_STRING);
uint  NdrNonEncapsulatedUnionMemorySize(PMIDL_STUB_MESSAGE,PFORMAT_STRING);
uint  NdrXmitOrRepAsMemorySize(PMIDL_STUB_MESSAGE,PFORMAT_STRING);
uint  NdrInterfacePointerMemorySize(PMIDL_STUB_MESSAGE,PFORMAT_STRING);
void  NdrPointerFree(PMIDL_STUB_MESSAGE,ubyte*,PFORMAT_STRING);
void  NdrSimpleStructFree(PMIDL_STUB_MESSAGE,ubyte*,PFORMAT_STRING);
void  NdrConformantStructFree(PMIDL_STUB_MESSAGE,ubyte*,PFORMAT_STRING);
void  NdrConformantVaryingStructFree(PMIDL_STUB_MESSAGE,ubyte*,PFORMAT_STRING);
void  NdrHardStructFree(PMIDL_STUB_MESSAGE,ubyte*,PFORMAT_STRING);
void  NdrComplexStructFree(PMIDL_STUB_MESSAGE,ubyte*,PFORMAT_STRING);
void  NdrFixedArrayFree(PMIDL_STUB_MESSAGE,ubyte*,PFORMAT_STRING);
void  NdrConformantArrayFree(PMIDL_STUB_MESSAGE,ubyte*,PFORMAT_STRING);
void  NdrConformantVaryingArrayFree(PMIDL_STUB_MESSAGE,ubyte*,PFORMAT_STRING);
void  NdrVaryingArrayFree(PMIDL_STUB_MESSAGE,ubyte*,PFORMAT_STRING);
void  NdrComplexArrayFree(PMIDL_STUB_MESSAGE,ubyte*,PFORMAT_STRING);
void  NdrEncapsulatedUnionFree(PMIDL_STUB_MESSAGE,ubyte*,PFORMAT_STRING);
void  NdrNonEncapsulatedUnionFree(PMIDL_STUB_MESSAGE,ubyte*,PFORMAT_STRING);
void  NdrByteCountPointerFree(PMIDL_STUB_MESSAGE,ubyte*,PFORMAT_STRING);
void  NdrXmitOrRepAsFree(PMIDL_STUB_MESSAGE,ubyte*,PFORMAT_STRING);
void  NdrInterfacePointerFree(PMIDL_STUB_MESSAGE,ubyte*,PFORMAT_STRING);
void  NdrConvert(PMIDL_STUB_MESSAGE,PFORMAT_STRING);
void  NdrClientInitializeNew(PRPC_MESSAGE,PMIDL_STUB_MESSAGE,PMIDL_STUB_DESC,uint);
ubyte * NdrServerInitializeNew(PRPC_MESSAGE,PMIDL_STUB_MESSAGE,PMIDL_STUB_DESC);
void  NdrClientInitialize(PRPC_MESSAGE,PMIDL_STUB_MESSAGE,PMIDL_STUB_DESC,uint);
ubyte * NdrServerInitialize(PRPC_MESSAGE,PMIDL_STUB_MESSAGE,PMIDL_STUB_DESC);
ubyte * NdrServerInitializeUnmarshall(PMIDL_STUB_MESSAGE,PMIDL_STUB_DESC,PRPC_MESSAGE);
void  NdrServerInitializeMarshall(PRPC_MESSAGE,PMIDL_STUB_MESSAGE);
ubyte * NdrGetBuffer(PMIDL_STUB_MESSAGE,uint,RPC_BINDING_HANDLE);
ubyte * NdrNsGetBuffer(PMIDL_STUB_MESSAGE,uint,RPC_BINDING_HANDLE);
ubyte * NdrSendReceive(PMIDL_STUB_MESSAGE,ubyte*);
ubyte * NdrNsSendReceive(PMIDL_STUB_MESSAGE,ubyte*,RPC_BINDING_HANDLE*);
void  NdrFreeBuffer(PMIDL_STUB_MESSAGE);

CLIENT_CALL_RETURN  NdrClientCall(PMIDL_STUB_DESC,PFORMAT_STRING,...);

int  NdrStubCall(IRpcStubBuffer, IRpcChannelBuffer,PRPC_MESSAGE,uint*);
void  NdrServerCall(PRPC_MESSAGE);
int  NdrServerUnmarshall(IRpcChannelBuffer, PRPC_MESSAGE,PMIDL_STUB_MESSAGE,PMIDL_STUB_DESC,PFORMAT_STRING,void*);
void  NdrServerMarshall(IRpcStubBuffer, IRpcChannelBuffer,PMIDL_STUB_MESSAGE,PFORMAT_STRING);
RPC_STATUS  NdrMapCommAndFaultStatus(PMIDL_STUB_MESSAGE,uint*,uint*,RPC_STATUS);
int  NdrSH_UPDecision(PMIDL_STUB_MESSAGE,ubyte**,RPC_BUFPTR);
int  NdrSH_TLUPDecision(PMIDL_STUB_MESSAGE,ubyte**);
int  NdrSH_TLUPDecisionBuffer(PMIDL_STUB_MESSAGE,ubyte**);
int  NdrSH_IfAlloc(PMIDL_STUB_MESSAGE,ubyte**,uint);
int  NdrSH_IfAllocRef(PMIDL_STUB_MESSAGE,ubyte**,uint);
int  NdrSH_IfAllocSet(PMIDL_STUB_MESSAGE,ubyte**,uint);
RPC_BUFPTR  NdrSH_IfCopy(PMIDL_STUB_MESSAGE,ubyte**,uint);
RPC_BUFPTR  NdrSH_IfAllocCopy(PMIDL_STUB_MESSAGE,ubyte**,uint);
uint  NdrSH_Copy(ubyte*,ubyte*,uint);
void  NdrSH_IfFree(PMIDL_STUB_MESSAGE,ubyte*);
RPC_BUFPTR  NdrSH_StringMarshall(PMIDL_STUB_MESSAGE,ubyte*,uint,int);
RPC_BUFPTR  NdrSH_StringUnMarshall(PMIDL_STUB_MESSAGE,ubyte**,int);
void* RpcSsAllocate(uint);
void  RpcSsDisableAllocate();
void  RpcSsEnableAllocate();
void  RpcSsFree(void*);
RPC_SS_THREAD_HANDLE  RpcSsGetThreadHandle();
void  RpcSsSetClientAllocFree(PRPC_CLIENT_ALLOC,PRPC_CLIENT_FREE);
void  RpcSsSetThreadHandle(RPC_SS_THREAD_HANDLE);
void  RpcSsSwapClientAllocFree(PRPC_CLIENT_ALLOC,PRPC_CLIENT_FREE,PRPC_CLIENT_ALLOC*,PRPC_CLIENT_FREE*);
void* RpcSmAllocate(uint,RPC_STATUS*);
RPC_STATUS  RpcSmClientFree(void*);
RPC_STATUS  RpcSmDestroyClientContext(void**);
RPC_STATUS  RpcSmDisableAllocate();
RPC_STATUS  RpcSmEnableAllocate();
RPC_STATUS  RpcSmFree(void*);
RPC_SS_THREAD_HANDLE  RpcSmGetThreadHandle(RPC_STATUS*);
RPC_STATUS  RpcSmSetClientAllocFree(PRPC_CLIENT_ALLOC,PRPC_CLIENT_FREE);
RPC_STATUS  RpcSmSetThreadHandle(RPC_SS_THREAD_HANDLE);
RPC_STATUS  RpcSmSwapClientAllocFree(PRPC_CLIENT_ALLOC,PRPC_CLIENT_FREE,PRPC_CLIENT_ALLOC*,PRPC_CLIENT_FREE*);
void  NdrRpcSsEnableAllocate(PMIDL_STUB_MESSAGE);
void  NdrRpcSsDisableAllocate(PMIDL_STUB_MESSAGE);
void  NdrRpcSmSetClientToOsf(PMIDL_STUB_MESSAGE);
void* NdrRpcSmClientAllocate(uint);
void  NdrRpcSmClientFree(void*);
void* NdrRpcSsDefaultAllocate(uint);
void  NdrRpcSsDefaultFree(void*);
PFULL_PTR_XLAT_TABLES  NdrFullPointerXlatInit(uint,XLAT_SIDE);
void  NdrFullPointerXlatFree(PFULL_PTR_XLAT_TABLES);
int  NdrFullPointerQueryPointer(PFULL_PTR_XLAT_TABLES,void*,ubyte,uint*);
int  NdrFullPointerQueryRefId(PFULL_PTR_XLAT_TABLES,uint,ubyte,void**);
void  NdrFullPointerInsertRefId(PFULL_PTR_XLAT_TABLES,uint,void*);
int  NdrFullPointerFree(PFULL_PTR_XLAT_TABLES,void*);
void* NdrAllocate(PMIDL_STUB_MESSAGE,uint);
void  NdrClearOutParameters(PMIDL_STUB_MESSAGE,PFORMAT_STRING,void*);
void* NdrOleAllocate(uint);
void  NdrOleFree(void*);
ubyte* NdrUserMarshalMarshall(PMIDL_STUB_MESSAGE,ubyte*,PFORMAT_STRING);
ubyte* NdrUserMarshalUnmarshall(PMIDL_STUB_MESSAGE,ubyte**,PFORMAT_STRING,ubyte);
void  NdrUserMarshalBufferSize(PMIDL_STUB_MESSAGE,ubyte*,PFORMAT_STRING);
uint  NdrUserMarshalMemorySize(PMIDL_STUB_MESSAGE,PFORMAT_STRING);
void  NdrUserMarshalFree(PMIDL_STUB_MESSAGE,ubyte*,PFORMAT_STRING);
