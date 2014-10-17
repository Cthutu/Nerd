//----------------------------------------------------------------------------------------------------
// Implementation of the Nerd Language
// Copyright (C)2013 Matt Davies, all rights reserved
//----------------------------------------------------------------------------------------------------
//
// Index of sections:   (Incrementally search for '{' + name)
//
//  _SESSION        Nerd structure
//  UTILS           Utility functions
//  CONFIG          Configuration management
//  SESSION         Session management
//  MEMORY          Memory management
//  BUFFER          Buffer management
//  SCRATCH         Scratch buffer management
//  POOL            Pool buffers
//  GC              Garbage collection
//  HASH            Hash-encoding
//  CELL            Cell management
//  STRING          String management
//  TABLE           Table management
//  KEYVALUE        Key/value management
//  SYMBOL          Symbol management
//  NUMBER          Number management
//  FUNCTION        Function management
//  STACK           User stack management
//  OUTPUT          Output
//  ERROR           Error handling
//  LEX             Lexical analysis
//  READ            Reading
//  EXEC            Execution
//  DEBUG           Debugging
//  NATIVE          Native function management
//  STANDARD        Standard natives
//
//----------------------------------------------------------------------------------------------------

#include "nerd-int.h"
#include <stdlib.h>
#include <stdio.h>
#include <stdarg.h>
#include <string.h>

//----------------------------------------------------------------------------------------------------
// Debug flags
//----------------------------------------------------------------------------------------------------

#define NE_DEBUG_MEMORY                 1       // Detect memory leaks and store information per allocation
#define NE_DEBUG_LOG_MEMORY             0       // Log memory operations to memory-log.txt
#define NE_DEBUG_CHECK_MEMORY           0       // Check state of memory on every operation
#define NE_DEBUG_GC                     0       // Garbage collection
#define NE_DEBUG_TRACE_EVAL             0       // Trace evaluation
#define NE_DEBUG_TO_FILE                0       // Output to "nerd-debug.log" all output

//----------------------------------------------------------------------------------------------------
// System definition
//----------------------------------------------------------------------------------------------------

#if defined(_WIN32)
#   define NE_WIN32
#   define NE_ROOT_DIR "C:\\"
#elif defined(__APPLE__)
#   define NE_APPLE
#   define NE_ROOT_DIR "/tmp/"
#endif

#if defined(_DEBUG)
#   define NE_DEBUG
#endif

//----------------------------------------------------------------------------------------------------
// Constants
//----------------------------------------------------------------------------------------------------

#define NE_DEFAULT_SEED                 0x12345678UL    
#define NE_INITIAL_SCRATCHPAD_SIZE      2048
#define NE_DEFAULT_HEAP_SIZE            1024
#define NE_DEFAULT_BUFFER_SIZE          256
#define NE_MAX_TABLE_LOGSIZE            30

//----------------------------------------------------------------------------------------------------
// Garbage collectible object header structure
// All garbage collectible objects start with this header so that they can be tracked
//----------------------------------------------------------------------------------------------------

typedef struct _NeGcObject
{
    NE_GC_HEADER
    struct _NeGcObject* mNext;
}
NeGcObject, *NeGcObjectRef;

//----------------------------------------------------------------------------------------------------
// Internal structures
//----------------------------------------------------------------------------------------------------

//
// NeStringInfo structure heads every string value in Nerd
//
typedef struct _NeStringInfo
{
    NeGcObject      mGcObj;         // All strings are chained and garbage collectible
    NeUInt32        mHash;          // 32-bit hash of the string (for fast comparison)
    NeUInt          mLength;        // Number of bytes in the string (not including null terminator)
    char            mString[0];     // String characters and null terminator follow structure
}
NeStringInfo, *NeStringInfoRef;

//
// NeNode structure contains a single key/value pair that can be chained.  It is used in the
// table implementation
//
typedef struct _NeNode
{
    NeValue             mKey;
    NeValue             mValue;
    struct _NeNode*     mNext;
}
NeNode, *NeNodeRef;

//
// NeTable is used to hold multiple key/value combinations and enables fast look up by using the
// key.
//
typedef struct _NeTable
{
    NeGcObject          mGcObj;             // This is a garbage collected object
    NeUInt8             mLogNumNodes;       // Log 2 size of the node array
    NeNode*             mNodes;             // Array of nodes
    NeNodeRef           mLastFreeNode;      // Used for quick fetching of a node
    struct _NeTable*    mParent;            // Table that this was cloned from
}
NeTable, *NeTableRef;

//----------------------------------------------------------------------------------------------------
// NePool
// A lot of the structures need to be created and destroyed regularly, so we use pools that are
// customised to the structure and allows quick creation and recycling.  It is also integrated
// into the garbage collection system too.  So all structures must start with a NeGcObject field.
// Also all pooled structures need to be a multiple of 16 bytes, since the lower 4 bits of the
// address of all elements are 0.  CreatePool() will assert this.
//----------------------------------------------------------------------------------------------------

typedef void(*NePoolElemDestroyFunc) (Nerd N, void* elem);

typedef struct _NePool
{
    NeUInt                  mElemSize;          // Size of a single element in the pool.
    NeUInt                  mNumElems;          // Number of elements in a pool heap.  A pool is a collection of chained heaps.
    NeUInt                  mNumAvail;          // Number of elements available.
    Nerd                    mSession;           // The session that the memory is managed by.
    NeGcObjectRef           mFreeList;          // Pointer to the first free element.
    void*                   mChain;             // Pointer to first heap.
    NePoolElemDestroyFunc   mDestroyFunc;       // Function to destroy an element.
}
NePool, *NePoolRef;

//----------------------------------------------------------------------------------------------------
// NeBuffer
// A NeBuffer is an expandable buffer that can expand its size.  This is required a lot for building
// temporary buffers and for generation of byte-code, for example.
//----------------------------------------------------------------------------------------------------

typedef struct _NeBuffer
{
    NeUInt          mCapacity;      // Size of the allocated memory
    NeUInt          mIncrement;     // The amount the buffer should expand by
    NeUInt          mCursor;        // The current point to insert new data
    NeUInt			mStackPointer;	// Pointer to the saved data
    NeUInt			mCommit;		// Data before this has been committed
    Nerd            mSession;       // Session that owns this buffer
    char            mData[0];
}
NeBuffer, *NeBufferRef;

//----------------------------------------------------------------------------------------------------
// NeMemoryInfo
// When NE_DEBUG_MEMORY is defined to be 1, this structure is used before each allocation to
// store information about the allocations.  NE_DEBUG_MEMORY should be only used for development
// of Nerd, as from the user's point of view, the only leak they need to worry about is whether
// to match a NeClose() for each NeOpen() or NeSpawn() call.
//----------------------------------------------------------------------------------------------------

#if NE_DEBUG_MEMORY

typedef struct _NeMemoryInfo
{
    NeUInt                  mSize;
    union
    {
        NeUInt                  mPadding[6];
        struct {
            const char*             mFileName;
            NeUInt16                mLineNumber;
            NeUInt16                mIndex;
            NeInt16                 mType;
            struct _NeMemoryInfo*   mPrev;
            struct _NeMemoryInfo*   mNext;
        };
    };
    NeUInt                  mMagic;
}
NeMemoryInfo, *NeMemoryInfoRef;

#endif

//----------------------------------------------------------------------------------------------------{_SESSION}
// Nerd structure
// The session is split up into two sections: local and global.  The local part is the only part that
// is accessible from the public API.  This allows new sessions to be cloned from another and
// still share global data.
//----------------------------------------------------------------------------------------------------

typedef struct _NeGlobalSession
{
    NeConfig		        mConfig;		    // All connected sessions share the same configuration.
    NeUInt			        mRefCount;		    // Counts how many local sessions are referencing it.

    // Memory management
#if NE_DEBUG_MEMORY
    NeMemoryInfoRef         mFirstAlloc;        // Pointer to first allocation in the chain.
    NeUInt16                mMemoryIndex;       // Current memory index.
#endif

    // Garbage collection information
    NeUInt                  mMarkColour;        // The next mark colour to be used in a garbage collect.

    // String information
    NeGcObjectRef           mFirstString;       // Start of a linked list of all strings.

    // Scatchpad information
    NeBufferRef             mScratch;           // The scratch pad.

    // Pools
    NePool                  mCellsPool;         // Contains all instances of NeCell.
    NePool                  mTablesPool;        // Contains all instances of NeTable.
    NePool					mNumbersPool;		// Contains all instances of NeNumber.

    // Symbols
    NeValue                 mSymbolTable;       // Table used for fast-look up of symbols.

    // Environments
    NeValue                 mCoreEnv;           // The base environment that contains all the natives and compiler functions.
    NeValue                 mGlobalEnv;         // Derived from the core environment, and contains all global variables.

    // Natives and compiler functions
    NeBufferRef             mNativeFuncBuffer;  // Array of function pointers used for native functions.
}
NeGlobalSession, *NeGlobalSessionRef;

typedef struct _Nerd
{
    NeGlobalSessionRef      mGlobalSession;     // Pointer to shared global session.

    // User stack
    NeValue*                mStack;             // Stack for communicating with user.
    NeUInt                  mTop;               // The stack pointer.

    // Process state
    NeBool                  mDebugMode;         // True if an error has occurred.

};

#define G(field) N->mGlobalSession->field

//----------------------------------------------------------------------------------------------------{UTILS}
//----------------------------------------------------------------------------------------------------
// U T I L I T Y   F U N C T I O N S
//----------------------------------------------------------------------------------------------------
//----------------------------------------------------------------------------------------------------

#define StrLen(str) (NeUInt)strlen(str)
#define ClearMemory(memory, size) memset((memory), 0, (size_t)(size))
#define CopyMemory(dest, src, size) memcpy((dest), (src), (size_t)(size))
#define MoveMemory(dest, src, size) memmove((dest), (src), (size_t)(size))
#define CopyString(dest, src, size) strncpy((dest), (src), (size_t)(size))
#define FormatPrint(buffer, size, format, args) vsnprintf((buffer), (size), (format), (args))

// Alignment define - alignIndex must be a 2^n value (i.e. binary 1 followed by n zeros)
//
#define NE_ALIGN(i, alignIndex) (((i) + ((alignIndex)-1)) & ~((alignIndex)-1))

// Compare two values and return NE_YES if they are equal.
//
NeBool NeEqual(NeValue v1, NeValue v2)
{
    // Do the quick checks first
    if (v1 == v2) return NE_YES;
    if (NE_TYPEOF(v1) != NE_TYPEOF(v2)) return NE_NO;

    // At this point the value bits are different, but let's see if they are really different
    switch (NE_TYPEOF(v1))
    {
    case NE_PT_STRING:
        {
            NeStringInfoRef s1 = NE_CAST(v1, NeStringInfo);
            NeStringInfoRef s2 = NE_CAST(v1, NeStringInfo);

            return ((s1->mHash != s2->mHash) ||
                    (s1->mLength != s2->mLength) ||
                    (strcmp(s1->mString, s2->mString) != 0)) ? NE_NO : NE_YES;
        }

    default:
        return NE_NO;
    }
}

// Append a new cell at the end of a list.
//
static NeBool AppendCell(Nerd N, NE_IN_OUT NeValue* rootRef, NE_IN_OUT NeValue* lastCellRef,
    NeValue newCell)
{
    NE_ASSERT(N);
    NE_ASSERT(rootRef);
    NE_ASSERT(lastCellRef);
    NE_ASSERT(newCell);
    NE_ASSERT(NE_IS_CELL(newCell));

    if (*rootRef)
    {
        NE_TAIL(*lastCellRef) = newCell;
    }
    else
    {
        *rootRef = newCell;
    }
    *lastCellRef = newCell;

    return NE_YES;
}

// Append a new value at the end of a list by creating a new cons-cell and calling AppendCell().
//
static NeBool AppendItem(Nerd N, NE_IN_OUT NeValue* rootRef, NE_IN_OUT NeValue* lastCellRef,
    NeValue value)
{
    NeValue newCell = 0;

    NE_ASSERT(N);
    NE_ASSERT(rootRef);
    NE_ASSERT(lastCellRef);

    newCell = NeCreateCons(N, value, 0);
    if (!newCell)
    {
        return NeOutOfMemory(N);
    }

    return AppendCell(N, rootRef, lastCellRef, newCell);
}

NeBool NeCheckArgType(Nerd N, NeValue arg, NeUInt index, NeType expectedArgType)
{
    NeType argType = NeGetType(arg);
    if ((argType != expectedArgType) &&
        !((expectedArgType == NeType_List) && (argType == NeType_Nil)))
    {
        return NeError(N, "Argument %u should be of type '%s', but found type '%s'.",
            index,
            NeGetTypeName(expectedArgType),
            NeGetTypeName(argType));
    }

    return NE_YES;
}

NeBool NeCheckNumArgs(Nerd N, NeValue args, NeUInt count, NeBool exactCount)
{
    // TODO: Make this faster by having the arguments in an array
    NeUInt countArgs = 0;

    while (args)
    {
        ++countArgs;
        if (exactCount)
        {
            // Check that we haven't got too many arguments
            if (count < countArgs)
            {
                return NeError(N, "Too many arguments given, expected %u argument%s.", count, 1 == count ? "" : "s");
            }
        }
        else
        {
            // Check that we have enough
            if (count <= countArgs)
            {
                return NE_YES;
            }
        }

        args = NE_TAIL(args);
    }

    if (exactCount && (countArgs != count))
    {
        return NeError(N, "Not enough arguments given, expected exactly %u argument%s",
            count, 1 == count ? "" : "s");
    }
    else if (!exactCount)
    {
        return NeError(N, "Not enough arguments given, expected at least %u argument%s",
            count, 1 == count ? "" : "s");
    }

    return NE_YES;
}

NeType NeGetType(NeValue v)
{
    static NeType primaryTypes[15] =
    {
        // Cell-hierarchy based types
        NeType_List,
        NeType_KeyValue,
        NeType_Function,
        NeType_Undefined,
        NeType_Undefined,
        NeType_Undefined,
        NeType_Undefined,
        NeType_Undefined,

        // Structure address based types
        NeType_Table,
        NeType_Symbol,
        NeType_String,
        NeType_Symbol,
        NeType_Undefined,
        NeType_Undefined,
        NeType_Number,
    };

    static NeType extendedTypes[] =
    {
        NeType_Undefined,
        NeType_Number,
        NeType_Number,
        NeType_Number,
        NeType_Boolean,
        NeType_Native,
    };

    NeUInt type = 0;
    NeType result = NeType_Undefined;

    if (0 == v) return NeType_Nil;

    type = NE_TYPEOF(v);
    if (type < NE_PT_EXTENDED)
    {
        result = primaryTypes[type];
    }
    else
    {
        type = NE_EXTENDED_TYPEOF(v) >> 4;
        result = (type < sizeof(extendedTypes) / sizeof(extendedTypes[0])) ? extendedTypes[type] : NeType_Undefined;
    }

    return result;
}

NeString NeGetTypeName(NeType t)
{
    static NeString typeNames[NeType_COUNT] = {
        "nil",
        "list",
        "key/value",
        "function",
        "table",
        "symbol",
        "string",
        "number",
        "undefined",
        "boolean",
        "native"
    };

    return typeNames[t];
}

// We use this to store the output of a value into a temporary buffer.  This is useful if we need to 
// output it via NeOut since that function uses the scratch buffer.
//
static char* AllocDescription(Nerd N, NeValue v)
{
    char* str = 0;
    if (v == G(mSymbolTable))
    {
        // The symbol table is unprintable since the keys are not real values
        str = NE_ALLOC(char, N, 15, NeMemoryType_Temp);
        CopyMemory(str, "<SYMBOL-TABLE>", 15);
    }
    else
    {
        NeString desc = NeDescribe(N, v);
        NeUInt len = StrLen(desc) + 1;
        str = NE_ALLOC(char, N, len, NeMemoryType_Temp);
        CopyMemory(str, desc, len);
    }
    return str;
}

// Deallocate the memory allocated with AllocDescription().
//
static void FreeDescription(Nerd N, char* str)
{
    NE_FREE(N, str, StrLen(str) + 1, NeMemoryType_Temp);
}

//----------------------------------------------------------------------------------------------------{CONFIG}
//----------------------------------------------------------------------------------------------------
//  C O N F I G U R A T I O N   M A N A G E M E N T
//----------------------------------------------------------------------------------------------------
//----------------------------------------------------------------------------------------------------

#ifdef NE_WIN32
#   define REALLOC(P, S) _aligned_realloc(P, S, 16)
#   define FREE(P) _aligned_free(P)
#else
#   define REALLOC(P, S) realloc(P, S)
#   define FREE(P) free(P)
#endif

typedef struct _NeDebugMemoryOp
{
    Nerd            mSession;       // The session that is making the memory operation
    void*           mAddress;       // The address for deallocations and reallocations of the original buffer
    NeUInt          mOldSize;       // The size of the buffer before this operation
    NeUInt          mNewSize;       // The intended size of the buffer after this operation
    NeMemoryType    mType;          // The type of memory being allocated
    const char*     mFile;          // Name of file
    int             mLine;          // Line number of allocation
}
NeDebugMemoryOp, *NeDebugMemoryOpRef;

#if NE_DEBUG_MEMORY

static void DumpMemoryInfo(Nerd N, NeMemoryInfoRef info, const char* label)
{
    static const char* types[NeMemoryType_COUNT] = {
        "No type",
        "Temporary",
        "User Stack",
        "Session",
        "String",
        "Table Nodes",
        "Buffer",
        "Pool Heap",
    };
    NeOut(N, "%s(%d): %s: [%u] %p (%llu) of type '%s'\n",
        info->mFileName,
        (int)info->mLineNumber,
        label,
        (unsigned)info->mIndex,
        info + 1,
        (unsigned long long)info->mSize,
        types[info->mType]);
}

static void DumpMemoryAllocs(Nerd N, const char* label)
{
    NeMemoryInfoRef info = G(mFirstAlloc);

    while (info)
    {
        if ((void *)(info + 1) != (void *)(G(mScratch)))
        {
            DumpMemoryInfo(N, info, label);
        }
        info = info->mNext;
    }
}

#if NE_DEBUG_LOG_MEMORY
FILE* gMemoryLog = 0;
#endif

#define NE_GUARD_BEGIN      0xaaaaaaaaaaaaaaaaull
#define NE_GUARD_END        0xbbbbbbbbbbbbbbbbull

static void* DefaultMemoryCallback(NeMemoryOpRef memoryOperation)
{
    NeDebugMemoryOpRef op = (NeDebugMemoryOpRef)memoryOperation;
    NeMemoryInfoRef newInfo = 0;
    NeMemoryInfoRef info = op->mAddress ? ((NeMemoryInfoRef)op->mAddress) - 1 : 0;
    NeUInt actualSize = op->mNewSize ? op->mNewSize + sizeof(NeMemoryInfo)+sizeof(NeUInt) : 0;
    NeGlobalSessionRef G = op->mSession ? op->mSession->mGlobalSession : 0;
    NeUInt index = G ? ++G->mMemoryIndex : 0;

    if ((op->mAddress == 0) && (op->mNewSize == 0))
    {
        // This is a no-operation
        return 0;
    }

#if NE_DEBUG_CHECK_MEMORY
    if (G)
    {
        NeMemoryInfo* scan = G->mFirstAlloc;
        while (scan)
        {
            NE_ASSERT(scan->mMagic == NE_GUARD_BEGIN);
            NE_ASSERT(*(NeUInt *)(&((char *)(scan + 1))[scan->mSize]) == NE_GUARD_END);
            scan = scan->mNext;
        }
    }
#endif

    // realloc and free only use a size_t type for the size of allocation.  It is possible that NeUInt can hold
    // a value greater than what a size_t can hold.  This check protects against that.
    if (op->mNewSize > (NeUInt)(size_t)-1)
    {
        return 0;
    }

    if (0 == op->mNewSize)
    {
        // We are freeing memory here if address is set.
        if (op->mAddress)
        {
            // Check to see if the sizes match up.  This ensures that the user callback will get a good oldSize.  If
            // this fails it doesn't match.  Go back to the code and investigate why the oldSize doesn't match the original
            // allocation size.
            NE_ASSERT(info->mSize == op->mOldSize);

            // Remove the allocation from the chain
            if (G)
            {
                if (info->mPrev)
                {
                    info->mPrev->mNext = info->mNext;
                }
                else
                {
                    G->mFirstAlloc = info->mNext;
                }
                if (info->mNext) info->mNext->mPrev = info->mPrev;
            }

            // Free the memory
            FREE(info);
        }
    }
    else
    {
        NeMemoryInfoRef prev = info ? info->mPrev : 0;
        NeMemoryInfoRef next = info ? info->mNext : 0;

        // We are either reallocating or allocating new memory here
        NE_ASSERT(0 == actualSize || (actualSize > sizeof(NeMemoryInfo)));

        // For reallocations, make some checks that the operation information matches the previous allocation/reallocation.
        NE_ASSERT(!info || (info->mSize == op->mOldSize));
        NE_ASSERT(!info || (info->mType = op->mType));

        newInfo = (NeMemoryInfoRef)REALLOC(info, (size_t)actualSize);

        // Set up the memory information header and chain it to the other allocations
        if (newInfo)
        {
            newInfo->mMagic = NE_GUARD_BEGIN;
            *(NeUInt *)(&((char *)(newInfo + 1))[op->mNewSize]) = NE_GUARD_END;
            newInfo->mFileName = op->mFile;
            newInfo->mLineNumber = op->mLine;
            newInfo->mIndex = (NeUInt16)index;
            newInfo->mSize = op->mNewSize;
            newInfo->mType = op->mType;

            if (!info)
            {
                // New allocation so we add it to the beginning of the chain
                newInfo->mPrev = 0;

                if (G)
                {
                    newInfo->mNext = G->mFirstAlloc;
                    if (G->mFirstAlloc) G->mFirstAlloc->mPrev = newInfo;
                    G->mFirstAlloc = newInfo;
                }
                else
                {
                    newInfo->mNext = 0;
                }
            }
            else
            {
                // We insert the allocation at the same place in the chain
                if (prev) prev->mNext = newInfo; else G->mFirstAlloc = newInfo;
                if (next) next->mPrev = newInfo;
                newInfo->mPrev = prev;
                newInfo->mNext = next;
            }
        }
    }

#if NE_DEBUG_LOG_MEMORY
    {
        int opcode = op->mNewSize ? (op->mAddress ? 2 : 1) : 0;
        gMemoryLog = fopen(NE_ROOT_DIR "memory-log.txt", gMemoryLog ? "a" : "w");
        switch (opcode)
        {
        case 0:     // FREE
            fprintf(gMemoryLog, "   FREE: %s(%6d): [%5u] %p (%llu)\n",
                op->mFile, (int)op->mLine,
                (unsigned)index,
                op->mAddress, op->mOldSize);
            break;

        case 1:     // ALLOC
            fprintf(gMemoryLog, "  ALLOC: %s(%6d): [%5u] %p (%llu)\n",
                op->mFile, (int)op->mLine,
                (unsigned)index,
                newInfo + 1, op->mNewSize);
            break;

        case 2:     // REALLOC
            fprintf(gMemoryLog, "REALLOC: %s(%6d): [%5u] %p <- %p (%llu)\n",
                op->mFile, (int)op->mLine,
                (unsigned)index,
                newInfo + 1, op->mAddress, op->mNewSize);
            break;
        }
        fclose(gMemoryLog);
    }
#endif

    return newInfo ? (void *)(newInfo + 1) : 0;
}

#else

static void* DefaultMemoryCallback(NeMemoryOpRef op)
{
    void* newAddress = 0;

    if (0 == op->mNewSize)
    {
        FREE(op->mAddress);
    }
    else
    {
        newAddress = REALLOC(op->mAddress, (size_t)op->mNewSize);
    }

    return newAddress;
}

#endif

static void DefaultOutputCallback(Nerd N, const char* text)
{
#if NE_DEBUG_TO_FILE
    FILE* f = fopen("nerd-debug.log", "a");
    fprintf(f, "%s", text);
    fclose(f);
#endif
    fprintf(stdout, "%s", text);
}

// Initialises a configuration structure to default settings.  These settings are:
//
//      mCallbacks.mMemoryCallback          Realloc/free based function with memory tracking on debug builds.
//      mCallbacks.mOutputCallback          Outputs to stdout
//
void NeSetConfigToDefault(NeConfigRef config)
{
    config->mCallbacks.mMemoryCallback = &DefaultMemoryCallback;
    config->mCallbacks.mOutputCallback = &DefaultOutputCallback;
    config->mStackSize = 256;
    config->mProcessStackSize = 1024;
}

//----------------------------------------------------------------------------------------------------{SESSION}
//----------------------------------------------------------------------------------------------------
// S E S S I O N   M A N A G E M E N T
//----------------------------------------------------------------------------------------------------
//----------------------------------------------------------------------------------------------------

static void CreatePool(NePoolRef pool, Nerd N, NeUInt elemSize, NeUInt numElemsPerHeap,
                       NePoolElemDestroyFunc destroyFunc);
static void DestroyPool(NePoolRef pool);
static NeBufferRef CreateBuffer(Nerd N, NeUInt startSize);
static void InitTable(Nerd N, NeTableRef table, NeTableRef parent);
static void DestroyTableElement(Nerd N, void* tableObject);
static void DestroyBuffer(NeBufferRef buffer);
static NeBool RegisterCoreNatives(Nerd N);

const char* gNeOpenError = 0;

Nerd NeOpen(NeConfigRef config)
{
    NeGlobalSessionRef globalSession = 0;
    Nerd N = 0;
    NeConfig defaultConfig;
    NeDebugMemoryOp memOp;

    gNeOpenError = "Out of Memory";

    if (0 == config)
    {
        config = &defaultConfig;
        NeSetConfigToDefault(config);
    }

    // Check the sizes of the data types
    if ((sizeof(NeInt) != 8) ||
        (sizeof(NeUInt) != 8))
    {
        gNeOpenError = "Size of NeInt is not 64-bit on this platform";
        goto error;
    }

#if NE_DEBUG_MEMORY
    if ((sizeof(NeMemoryInfo) % 16) != 0)
    {
        gNeOpenError = "NeMemoryInfo structure is not a multiple of 16 bytes on this platform";
        goto error;
    }
#endif

    // Check that the configuration is correct
    if (!config->mCallbacks.mMemoryCallback)
    {
        gNeOpenError = "No memory allocation callback has been set in the NeConfig structure";
        goto error;
    }

    // Allocate memory for the global session structure and initialise it
    memOp.mFile = 0;
    memOp.mLine = 0;
    memOp.mAddress = 0;
    memOp.mNewSize = sizeof(struct _NeGlobalSession);
    memOp.mOldSize = 0;
    memOp.mSession = 0;
    memOp.mType = NeMemoryType_Session;
    globalSession = config->mCallbacks.mMemoryCallback((NeMemoryOpRef)&memOp);
    if (!globalSession) goto error;
    ClearMemory(globalSession, sizeof(struct _NeGlobalSession));
    globalSession->mConfig = *config;
    globalSession->mRefCount = 1;

    // Allocate memory for the local session structure and initialise it
    memOp.mNewSize = sizeof(struct _Nerd);
    N = config->mCallbacks.mMemoryCallback((NeMemoryOpRef)&memOp);
    if (!N) goto error;
    ClearMemory(N, sizeof(struct _Nerd));
    N->mGlobalSession = globalSession;

    // Initialise the pools
    CreatePool(&G(mCellsPool), N, sizeof(NeCell), NE_DEFAULT_HEAP_SIZE, 0);
    CreatePool(&G(mTablesPool), N, sizeof(NeTable), NE_DEFAULT_HEAP_SIZE, &DestroyTableElement);
    CreatePool(&G(mNumbersPool), N, sizeof(NeNumber), NE_DEFAULT_HEAP_SIZE, 0);

    // Intialise the symbol table
    G(mSymbolTable) = NeCloneTable(N, 0);

    // Initialise the user stack
    N->mStack = NE_ALLOC(NeValue, N, sizeof(NeValue)* config->mStackSize, NeMemoryType_UserStack);
    if (!N->mStack) goto error;

    // Initialise the environments
    G(mCoreEnv) = NeCloneTable(N, 0);
    G(mGlobalEnv) = NeCloneTable(N, G(mCoreEnv));

    // Initialise the scratch pad
    G(mScratch) = CreateBuffer(N, NE_DEFAULT_BUFFER_SIZE);
    if (!G(mScratch)) goto error;

    // Register core functions
    G(mNativeFuncBuffer) = CreateBuffer(N, 16);
    if (!G(mNativeFuncBuffer)) goto error;
    RegisterCoreNatives(N);

    // We are successful in creating a session!
    return N;

error:
    NeClose(N);
    return 0;
}

void NeClose(Nerd N)
{
    if (N)
    {
        NeGlobalSessionRef G = 0;

        // Destroy the stack
        NE_FREE(N, N->mStack, sizeof(NeValue)* N->mGlobalSession->mConfig.mStackSize, NeMemoryType_UserStack);

        // Destroy the pools
        DestroyPool(&G(mCellsPool));
        DestroyPool(&G(mTablesPool));
        DestroyPool(&G(mNumbersPool));

        // Free the strings
        while (G(mFirstString))
        {
            NeStringInfoRef strInfo = (NeStringInfoRef)G(mFirstString);
            G(mFirstString) = G(mFirstString)->mNext;
            NE_FREE(N, strInfo, sizeof(NeStringInfo)+strInfo->mLength + 1, NeMemoryType_String);
        }

        // Free the shared global session structure if necessary
        if (--G(mRefCount) == 0) G = N->mGlobalSession;

        // Destroy the buffers
        DestroyBuffer(G(mNativeFuncBuffer));

        // Check for leaks!
#if NE_DEBUG_MEMORY
        DumpMemoryAllocs(N, "LEAK");
#endif

        // Destroy the scratch buffer (DumpMemoryAllocs ignores this buffer)
        DestroyBuffer(G(mScratch));

        // Free the session structure
        NE_FREE(N, N, sizeof(struct _Nerd), NeMemoryType_Session);

        if (G)
        {
            // No local session is referencing the global session data any more
            NeDebugMemoryOp op;
            op.mAddress = G;
            op.mFile = 0;
            op.mLine = 0;
            op.mNewSize = 0;
            op.mOldSize = sizeof(NeGlobalSession);
            op.mSession = 0;
            op.mType = NeMemoryType_Session;
            G->mConfig.mCallbacks.mMemoryCallback((NeMemoryOpRef)&op);
        }
    }
}

//----------------------------------------------------------------------------------------------------{MEMORY}
//----------------------------------------------------------------------------------------------------
// M E M O R Y   M A N A G E M E N T
//----------------------------------------------------------------------------------------------------
//----------------------------------------------------------------------------------------------------

void* _NeAlloc(Nerd N, NeUInt size, NeMemoryType memoryType, const char* file, int line)
{
    NeDebugMemoryOp op;
    void* address;

    op.mSession = N;
    op.mAddress = 0;
    op.mOldSize = 0;
    op.mNewSize = size;
    op.mType = memoryType;
    op.mFile = file;
    op.mLine = line;

    address = N->mGlobalSession->mConfig.mCallbacks.mMemoryCallback((NeMemoryOpRef)&op);
    NE_ASSERT((((NeUInt)address) & 0x0f) == 0);
    return address;
}

void* _NeRealloc(Nerd N, void* address, NeUInt oldSize, NeUInt newSize, NeMemoryType memoryType, const char* file, int line)
{
    NeDebugMemoryOp op;
    void* newAddress;

    op.mSession = N;
    op.mAddress = address;
    op.mOldSize = oldSize;
    op.mNewSize = newSize;
    op.mType = memoryType;
    op.mFile = file;
    op.mLine = line;

    newAddress = N->mGlobalSession->mConfig.mCallbacks.mMemoryCallback((NeMemoryOpRef)&op);
    NE_ASSERT((((NeUInt)address) & 0x0f) == 0);
    return newAddress;
}

void _NeFree(Nerd N, void* address, NeUInt oldSize, NeMemoryType memoryType, const char* file, int line)
{
    NeDebugMemoryOp op;
    op.mSession = N;
    op.mAddress = address;
    op.mOldSize = oldSize;
    op.mNewSize = 0;
    op.mType = memoryType;
    op.mFile = file;
    op.mLine = line;

    N->mGlobalSession->mConfig.mCallbacks.mMemoryCallback((NeMemoryOpRef)&op);
}

//----------------------------------------------------------------------------------------------------{BUFFER}
//----------------------------------------------------------------------------------------------------
//  B U F F E R   M A N A G E M E N T
//----------------------------------------------------------------------------------------------------
//----------------------------------------------------------------------------------------------------

// Create a new expandable buffer.
//
static NeBufferRef CreateBuffer(Nerd N, NeUInt startSize)
{
    NeUInt finalSize = startSize + sizeof(NeBuffer);
    NeBufferRef newBuffer = NE_ALLOC(NeBuffer, N, finalSize, NeMemoryType_Buffer);

    if (newBuffer)
    {
        newBuffer->mCapacity = startSize;
        newBuffer->mCursor = 0;
        newBuffer->mIncrement = startSize;
        newBuffer->mSession = N;
        newBuffer->mCommit = 0;
        newBuffer->mStackPointer = startSize;
    }

    return newBuffer;
}

// Destroy the buffer.
//
static void DestroyBuffer(NeBufferRef buffer)
{
    if (buffer)
    {
        NE_FREE(buffer->mSession, buffer, buffer->mCapacity + sizeof(struct _NeBuffer), NeMemoryType_Buffer);
    }
}

// Expand a buffer.
//
static NeBool ExpandBuffer(NeBufferRef* buffer)
{
    NeUInt newSize = (*buffer)->mCapacity + (*buffer)->mIncrement;
    NeUInt stackSize = (*buffer)->mCapacity - (*buffer)->mStackPointer;
    NeBufferRef newBuffer = NE_REALLOC(NeBuffer,
        (*buffer)->mSession, (*buffer),
        (*buffer)->mCapacity + sizeof(struct _NeBuffer),
        newSize + sizeof(struct _NeBuffer),
        NeMemoryType_Buffer);
    if (newBuffer)
    {
        newBuffer->mCapacity = newSize;
        newBuffer->mStackPointer = newBuffer->mCapacity - stackSize;
        MoveMemory(newBuffer->mData + newBuffer->mStackPointer, newBuffer->mData + (*buffer)->mStackPointer, stackSize);
        *buffer = newBuffer;
        return NE_YES;
    }
    else
    {
        return NE_NO;
    }
}

// Reset a buffer so it can be re-used.
//
static void ResetBuffer(NeBufferRef buffer)
{
    buffer->mCursor = 0;
}

// Return the number of bytes left in this buffer before it must expand again.
//
static NeUInt BufferSpace(NeBufferRef buffer)
{
    return buffer->mStackPointer - buffer->mCursor;
}

// Create a string based on a printf-style format and parameters, and add it to the buffer.
//
static NeBool BufferAddFormatArgs(NeBufferRef* buffer, const char* format, va_list args)
{
    int formatResult = -1;

    while (formatResult == -1)
    {
        NeUInt spaceLeft = BufferSpace(*buffer);

        if (spaceLeft)
        {
            formatResult = vsnprintf(&(*buffer)->mData[(*buffer)->mCursor], (size_t)spaceLeft, format, args);
        }

        if (-1 == formatResult)
        {
            // Not enough space to hold text
            if (!ExpandBuffer(buffer))
            {
                // We don't have enough memory to extend it
                return NE_NO;
            }
        }
        else
        {
            (*buffer)->mCursor += (NeUInt)formatResult;
            (*buffer)->mData[(*buffer)->mCursor] = 0;
        }
    }

    return NE_YES;
}

// The version of BufferAddFormatArgs that uses variable arguments
//
//static NeBool BufferAddFormat(NeBufferRef* buffer, const char* format, ...)
//{
//    va_list args;
//    NeBool result;
//
//    va_start(args, format);
//    result = BufferAddFormatArgs(buffer, format, args);
//    va_end(args);
//
//    return result;
//}

// Allocate some space on the buffer and return the address.  It will be 16 byte aligned
//
static NeBool BufferAlloc(NeBufferRef* buffer, NeUInt size, NE_OUT void** address)
{
    NeBool haveSpace = NE_YES;

    while ((size > BufferSpace(*buffer)) && (haveSpace = ExpandBuffer(buffer)));

    if (haveSpace)
    {
        *address = &(*buffer)->mData[(*buffer)->mCursor];
        (*buffer)->mCursor += size;
    }

    return haveSpace;
}

// Add a memory block to our buffer
//
static void* BufferAdd(NeBufferRef* buffer, void* memBlock, NeUInt size)
{
    void* address;

    if (!BufferAlloc(buffer, size, &address)) return 0;
    CopyMemory(address, memBlock, size);

    return address;
}

// Set a block of memory within the buffer at a position from the beginning of the non-committed data.
//
static void BufferSet(NeBufferRef buffer, NeUInt position, void* memBlock, NeUInt size)
{
    NE_ASSERT((buffer->mCommit + position + size) <= buffer->mCursor);
    CopyMemory(&buffer->mData[buffer->mCommit + position], memBlock, size);
}

// Get a block of memory within a buffer according to position from the beginning of the non-committed data.
static void* BufferGet(NeBufferRef buffer, NeUInt position, NeUInt size)
{
    NE_ASSERT((buffer->mCommit + position + size) <= buffer->mCursor);
    return &buffer->mData[buffer->mCommit + position];
}

static void BufferShrink(NeBufferRef* buffer)
{
    NeUInt origSize = sizeof(NeBuffer)+(*buffer)->mCapacity;
    NeUInt finalSize = sizeof(NeBuffer)+(*buffer)->mCursor;
    NeBufferRef newBuffer = NE_REALLOC(NeBuffer, (*buffer)->mSession, *buffer, origSize, finalSize, NeMemoryType_Buffer);

    // We cannot have any saved data when we shrink
    NE_ASSERT((*buffer)->mStackPointer == (*buffer)->mCapacity);

    if (newBuffer)
    {
        *buffer = newBuffer;
        (*buffer)->mCapacity = (*buffer)->mCursor;
    }
}

static NeUInt BufferLength(NeBufferRef buffer)
{
    return buffer->mCursor;
}

static NeUInt BufferPosition(NeBufferRef buffer)
{
    return buffer->mCursor - buffer->mCommit;
}

// This will store the data written to the buffer away for later editing.  It will only save the data
// written since the last commit.  Use RestoreBuffer() to get it back.  You can nest the calls as long
// as you match each SaveBuffer with each RestoreBuffer.
//
static NeBool SaveBuffer(NeBufferRef* buffer)
{
    NeUInt sizeToSave = (*buffer)->mCursor - (*buffer)->mCommit;

    NE_ASSERT((*buffer)->mCursor >= (*buffer)->mCommit);

    // Check to see we have enough space for the operation.  We need at least sizeof(NeUInt) free to
    // store a size.
    if (BufferSpace(*buffer) < sizeof(NeUInt))
    {
        if (!ExpandBuffer(buffer)) return NE_NO;
    }

    (*buffer)->mStackPointer -= sizeToSave;
    MoveMemory((*buffer)->mData + (*buffer)->mStackPointer, (*buffer)->mData + (*buffer)->mCommit, sizeToSave);
    (*buffer)->mStackPointer -= sizeof(NeUInt);
    *(NeUInt *)((*buffer)->mData + (*buffer)->mStackPointer) = sizeToSave;

    (*buffer)->mCursor = (*buffer)->mCommit;
    return NE_YES;
}

// Restore previously saved contents.  There cannot have any data written since last commit, otherwise
// it will fail an assert.
//
static void RestoreBuffer(NeBufferRef buffer)
{
    NeUInt sizeToRestore;

    NE_ASSERT(buffer->mStackPointer < buffer->mCapacity);
    NE_ASSERT(buffer->mCursor == buffer->mCommit);

    sizeToRestore = *(NeUInt *)&buffer->mData[buffer->mStackPointer];
    buffer->mStackPointer += sizeof(NeUInt);

    MoveMemory(&buffer->mData[buffer->mCursor], &buffer->mData[buffer->mStackPointer], sizeToRestore);
    buffer->mStackPointer += sizeToRestore;
    NE_ASSERT(buffer->mStackPointer <= buffer->mCapacity);

    buffer->mCursor += sizeToRestore;
}

static void CommitBuffer(NeBufferRef buffer)
{
    buffer->mCommit = buffer->mCursor;
}

static NeUInt StartOfNonCommittedData(NeBufferRef buffer)
{
    return buffer->mCommit;
}

//----------------------------------------------------------------------------------------------------{SCRATCH}
//----------------------------------------------------------------------------------------------------
// S C R A T C H P A D   M A N A G E M E N T
//----------------------------------------------------------------------------------------------------
//----------------------------------------------------------------------------------------------------

static void ResetScratch(Nerd N)
{
    ResetBuffer(G(mScratch));
}

static NeBool FormatScratchArgs(Nerd N, const char* format, va_list args)
{
    return BufferAddFormatArgs(&G(mScratch), format, args);
}

static NeBool FormatScratch(Nerd N, const char* format, ...)
{
    va_list args;
    NeBool result;

    va_start(args, format);
    result = FormatScratchArgs(N, format, args);
    va_end(args);

    return result;
}

static NeBool AddScratchBuffer(Nerd N, void* buffer, NeUInt size)
{
    return BufferAdd(&G(mScratch), buffer, size) != 0 ? NE_YES : NE_NO;
}

static NeBool AddScratchChar(Nerd N, char c)
{
    return AddScratchBuffer(N, &c, 1);
}

static const char* GetScratch(Nerd N)
{
    return G(mScratch)->mData;
}

static NeUInt GetScratchLength(Nerd N)
{
    return G(mScratch)->mCursor;
}

//----------------------------------------------------------------------------------------------------{POOL}
//----------------------------------------------------------------------------------------------------
// P O O L   M A N A G E M E N T
//----------------------------------------------------------------------------------------------------
//----------------------------------------------------------------------------------------------------

// Initialise a pool structure to the attributes passed into this function.
//
static void CreatePool(NePoolRef pool, Nerd N, NeUInt elemSize, NeUInt numElemsPerHeap,
    NePoolElemDestroyFunc destroyFunc)
{
    NeUInt padding = elemSize % 16;

    NE_ASSERT(numElemsPerHeap > 2);
    NE_ASSERT(elemSize > sizeof(void *));

    if (padding != 0)
    {
        elemSize = (elemSize + 16) - padding;
    }

    pool->mSession = N;
    pool->mElemSize = elemSize;
    pool->mNumElems = numElemsPerHeap;
    pool->mFreeList = 0;
    pool->mChain = 0;
    pool->mDestroyFunc = destroyFunc;
}

#define NE_NEXT_ELEM(p, pool) ((void *)(((char *)p) + (pool)->mElemSize))

// Reset a pool structure and free all the memory allocations used for the heaps.
//
static void DestroyPool(NePoolRef pool)
{
    void** scan = pool->mChain;
    void* nextHeap = 0;

    while (scan)
    {
        if (pool->mDestroyFunc)
        {
            NeGcObjectRef elem = (NeGcObjectRef)NE_NEXT_ELEM(scan, pool);
            NeUInt i = 1;

            for (; i < pool->mNumElems; ++i)
            {
                if (elem->mUsed)
                {
                    pool->mDestroyFunc(pool->mSession, elem);
                }
                elem = (NeGcObjectRef)NE_NEXT_ELEM(elem, pool);
            }
        }
        nextHeap = *scan;
        NE_FREE(pool->mSession, scan, pool->mElemSize * pool->mNumElems, NeMemoryType_PoolHeap);
        scan = nextHeap;
    }

    ClearMemory(pool, sizeof(NePool));
}

// Garbage collect all elements in this pool that are used and do not match the mark colour
//
static void PoolCollect(NePoolRef pool, void (*TraceFunc) (Nerd, void*))
{
    void* heap = pool->mChain;
    NeGcObjectRef lastElem = 0;
    NeUInt markColour = pool->mSession->mGlobalSession->mMarkColour;

    while (heap)
    {
        NeGcObjectRef elem = (NeGcObjectRef)NE_NEXT_ELEM(heap, pool);
        NeUInt i = 1;

        for (; i < pool->mNumElems; ++i)
        {
            if ((elem->mMarked != markColour) &&
                (elem->mUsed))
            {
                // This element needs to be recycled
#if NE_DEBUG_GC
                if (TraceFunc) TraceFunc(pool->mSession, elem);
#endif
                if (pool->mDestroyFunc) pool->mDestroyFunc(pool->mSession, elem);
                elem->mUsed = 0;
                elem->mNext = 0;
                if (lastElem) lastElem->mNext = elem;
                lastElem = elem;
            }

            elem = (NeGcObjectRef)NE_NEXT_ELEM(elem, pool);
        }

        heap = *((void **)heap);
    }
}

// Expand the pool by another heap.
//
static void* ExpandPool(NePoolRef pool)
{
    char* bytes = 0;
    void** header;
    NeUInt i;

    // Allocate the memory
    bytes = NE_ALLOC(char, pool->mSession, pool->mElemSize * pool->mNumElems, NeMemoryType_PoolHeap);
    if (!bytes) return 0;
    ClearMemory(bytes, pool->mElemSize * pool->mNumElems);

    // Set up the header
    header = (void **)bytes;
    *header = pool->mChain;

    // Prepare the elements so that each one points to the next one
    for (i = 1; i < pool->mNumElems; ++i)
    {
        NeGcObjectRef gcObj = (NeGcObjectRef)(&bytes[i * pool->mElemSize]);
        gcObj->mNext = (NeGcObjectRef)(&bytes[(i + 1) * pool->mElemSize]);
        gcObj->mUsed = 0;
        // If this fails the element size is not a multiple of 16 bytes.
        NE_ASSERT((((NeUInt)gcObj) & 0xf) == 0);
    }

    ((NeGcObjectRef)(&bytes[(pool->mNumElems - 1) * pool->mElemSize]))->mNext = pool->mFreeList;
    pool->mFreeList = (NeGcObjectRef)(&bytes[1 * pool->mElemSize]);

    pool->mNumAvail += pool->mNumElems - 1;

    return (pool->mChain = bytes);
}

// Acquire a number of elements from this pool.
//
static void* PoolAcquire(NePoolRef pool, NeUInt numElems, NeUInt type)
{
    NeGcObjectRef newElems = 0;
    NeGcObjectRef scan = 0;
    NeGcObjectRef lastElem = 0;

    if (0 == numElems) return 0;

    while (pool->mNumAvail < numElems)
    {
        if (!ExpandPool(pool)) return 0;
    }

    newElems = scan = (NeGcObjectRef)pool->mFreeList;
    pool->mNumAvail -= numElems;

    while (numElems--)
    {
        lastElem = scan;
        scan->mMarked = pool->mSession->mGlobalSession->mMarkColour;
        scan->mUsed = 1;
        scan = scan->mNext;
    }

    // Terminate the list of elements
    pool->mFreeList = scan;
    lastElem->mNext = 0;

    return newElems;
}

// Recycle a single element and return it back to the pool.  Usually PoolCollect()
// will do the recycling, but there are occasions that it is more efficient to recycle
// an element immediately.  Such as when you acquire temporary elements.
//
//static void PoolRecycle(NePoolRef pool, void* elem)
//{
//    NeGcObjectRef obj = (NeGcObjectRef)elem;
//
//    // Destroy the element
//    if (pool->mDestroyFunc) pool->mDestroyFunc(pool->mSession, elem);
//
//    // Add it to the free list chain
//    obj->mNext = pool->mFreeList;
//    pool->mFreeList = obj;
//    
//    // Mark it as unused
//    obj->mUsed = 0;
//}

//----------------------------------------------------------------------------------------------------{GC}
//----------------------------------------------------------------------------------------------------
// G A R B A G E   C O L L E C T I O N
//----------------------------------------------------------------------------------------------------
//----------------------------------------------------------------------------------------------------

static void MarkTable(Nerd N, NeTableRef table, NeBool markKeys);

// Mark a value has being used and therefore not available for garbage collection
//
static void MarkValue(Nerd N, NeValue v)
{
#if NE_DEBUG_GC
    {
        char* desc = AllocDescription(N, v);
        NeOut(N, "MARK: %s\n", desc);
        FreeDescription(N, desc);
    }
#endif
    if (NE_TYPEOF(v) < 8)
    {
        // Mark the cells
        NeCellRef cell = NE_CAST(v, NeCell);
        NeValue tail;

        while (cell)
        {
            if (cell->mMarked == G(mMarkColour)) return;
            cell->mMarked = G(mMarkColour);

            MarkValue(N, cell->mHead);

            tail = cell->mTail;
            if (!NE_IS_CELL_HIERARCHY(tail) && !NE_IS_NATIVE(v))
            {
                MarkValue(N, tail);
                cell = 0;
            }
            else
            {
                cell = NE_CAST(cell->mTail, NeCell);
            }
        }
    }
    else
    {
        // Non-cells
        switch (NE_TYPEOF(v))
        {
        case NE_PT_TABLE:
            {
                NeBool markKeys = NE_YES;
                NeTableRef table = NE_CAST(v, NeTable);

                if (v == G(mSymbolTable))
                {
                    markKeys = NE_NO;
                }

                MarkTable(N, table, markKeys);
            }
            break;

        case NE_PT_SYMBOL:
        case NE_PT_STRING:
        case NE_PT_NUMBER:
            // Objects descended from NeGcObject
            {
                NeGcObjectRef obj = NE_CAST(v, NeGcObject);
                obj->mMarked = G(mMarkColour);
            }
            break;

            // These are cells but we only make the head.  The tail are not real values.
            {
            }
            break;

        default:;
        }
    }
}

// GC debugging collection routines for PoolCollect calls
//
static void GcTraceCellPool(Nerd N, void* object)
{
    NeCellRef cell = (NeCellRef)object;
    char* desc = AllocDescription(N, NE_BOX(cell, NE_PT_CELL));
    NeOut(N, "COLLECT: %s\n", desc);
    FreeDescription(N, desc);
}

static void GcTraceTablePool(Nerd N, void* object)
{
    NeTableRef table = (NeTableRef)object;
    char* desc = AllocDescription(N, NE_BOX(table, NE_PT_TABLE));
    NeOut(N, "COLLECT: %s\n", desc);
    FreeDescription(N, desc);
}

static void GcTraceNumberPool(Nerd N, void* object)
{
    NeNumberRef number = (NeNumberRef)object;
    char* desc = AllocDescription(N, NE_BOX(number, NE_PT_NUMBER));
    NeOut(N, "COLLECT: %s\n", desc);
    FreeDescription(N, desc);
}

// Garbage collect all unused values in memory
//
void NeGarbageCollect(Nerd N)
{
    NeUInt i;

    // Step 0 - Change the next mark colour
    G(mMarkColour) = !G(mMarkColour);

    // Step 1 - Mark all the root values
    // Step 1.a - Mark all the values in the user stack
    for (i = 0; i < N->mTop; ++i)
    {
        MarkValue(N, N->mStack[i]);
    }

    // Step 1.b - Mark all the values in the global and core environment
    MarkValue(N, G(mGlobalEnv));

    // Step 1.d - Mark all the symbols
    MarkValue(N, G(mSymbolTable));

#if NE_DEBUG_GC == 2
    NeOut(N, "-------------------------------------------------------------------------------\nBEFORE GC:\n");
    DumpMemoryAllocs(N, "MEM");
#endif

    // Step 2.a - the pools
    PoolCollect(&G(mCellsPool), &GcTraceCellPool);
    PoolCollect(&G(mTablesPool), &GcTraceTablePool);
    PoolCollect(&G(mNumbersPool), &GcTraceNumberPool);

    // Step 2 - Release resources that are not marked
    // Step 2.b - the strings
    {
        NeStringInfoRef lastString = 0;
        NeStringInfoRef scan = (NeStringInfoRef)G(mFirstString);

        while (scan)
        {
            NeStringInfoRef next = (NeStringInfoRef)scan->mGcObj.mNext;

            if (scan->mGcObj.mMarked != G(mMarkColour))
            {
#if NE_DEBUG_GC
                NeOut(N, "COLLECT: \"%s\"\n", scan->mString);
#endif
                // Unlink from the chain
                if (lastString) lastString->mGcObj.mNext = scan->mGcObj.mNext;
                else G(mFirstString) = scan->mGcObj.mNext;

                NE_FREE(N, scan, sizeof(NeStringInfo)+scan->mLength + 1, NeMemoryType_String);
            }
            else
            {
                lastString = scan;
            }

            scan = next;
        }
    }

    // Step 2.b - the pools
    PoolCollect(&G(mCellsPool), &GcTraceCellPool);
    PoolCollect(&G(mTablesPool), &GcTraceTablePool);
    PoolCollect(&G(mNumbersPool), &GcTraceNumberPool);

#if NE_DEBUG_GC == 2
    NeOut(N, "-------------------------------------------------------------------------------\nAFTER GC:\n");
    DumpMemoryAllocs(N, "MEM");
#endif
}

// Link a garbage collected data structure to a list pointed to by root.
//
static void LinkObject(Nerd N, NE_IN_OUT NeGcObjectRef* root, NeGcObjectRef object, NeUInt type)
{
    object->mMarked = 0;
    object->mUsed = 1;
    object->mNext = *root;
    object->mType = type;
    *root = object;
}

//----------------------------------------------------------------------------------------------------{HASH}
//----------------------------------------------------------------------------------------------------
// H A S H I N G
//----------------------------------------------------------------------------------------------------
//----------------------------------------------------------------------------------------------------
// Thanks to the wonderful Austin Appleby, we can use the Murmur 2 hashing algorithm to convert data
// into a 32-bit hash.  This code is now in the public domain (that makes Austin Appleby even more
// wonderful in my books!).
//----------------------------------------------------------------------------------------------------

static NeUInt32 Hash(const void* buffer, NeUInt size, NeUInt32 seed)
{
    const NeUInt32 m = 0x5bd1e995;
    const int r = 24;
    NeUInt32 h = seed ^ (NeUInt32)size;
    const unsigned char* data = (const unsigned char*)buffer;

    while (size >= 4)
    {
        NeUInt32 k;

        k = data[0];
        k |= data[1] << 8;
        k |= data[2] << 16;
        k |= data[3] << 24;

        k *= m;
        k ^= k >> r;
        k *= m;

        h *= m;
        h ^= k;

        data += 4;
        size -= 4;
    }

    switch (size)
    {
    case 3:		h ^= data[2] << 16;
    case 2:		h ^= data[1] << 8;
    case 1:		h ^= data[0];
        h *= m;
    }

    h ^= h >> 13;
    h *= m;
    h ^= h >> 15;

    return h;
}

NeUInt32 HashValue(NeValue value)
{
    return Hash(&value, sizeof(value), NE_DEFAULT_SEED);
}

//----------------------------------------------------------------------------------------------------{CELL}
//----------------------------------------------------------------------------------------------------
//  C E L L   M A N A G E M E N T
//----------------------------------------------------------------------------------------------------
//----------------------------------------------------------------------------------------------------

NeValue NeCreateCons(Nerd N, NeValue head, NeValue tail)
{
    NeCellRef newCell = (NeCellRef)PoolAcquire(&G(mCellsPool), 1, NE_PT_CELL);
    if (newCell)
    {
        newCell->mHead = head;
        newCell->mTail = tail;
    }

    return NE_BOX(newCell, NE_PT_CELL);
}

//----------------------------------------------------------------------------------------------------{STRING}
//----------------------------------------------------------------------------------------------------
//  S T R I N G   M A N A G E M E N T
//----------------------------------------------------------------------------------------------------
//----------------------------------------------------------------------------------------------------
// All string are separate allocations in Nerd, each with a header of type MeStringInfo, that
// contains length and hashing information.
//----------------------------------------------------------------------------------------------------
//----------------------------------------------------------------------------------------------------

// Allocate memory to hold string information and its data including the null terminator.
//
static NeStringInfoRef AllocString(Nerd N, NeUInt numCharacters)
{
    NeUInt sizeMem = sizeof(NeStringInfo)+numCharacters + 1;
    NeStringInfoRef newString = NE_ALLOC(NeStringInfo, N, sizeMem, NeMemoryType_String);

    if (newString)
    {
        LinkObject(N, &G(mFirstString), (NeGcObjectRef)newString, NE_PT_STRING);
        newString->mLength = numCharacters;
        newString->mHash = 0;
        newString->mString[numCharacters] = 0;
    }

    return newString;
}

// Given a string buffer, hash and number of bytes, create a Nerd string and return the value.
//
NeValue CreateString(Nerd N, const char* str, NeUInt size, NeUInt32 hash)
{
    NeStringInfoRef strInfo;
    NeValue result = 0;

    strInfo = AllocString(N, size);
    if (strInfo)
    {
        CopyMemory(strInfo->mString, str, size);
        strInfo->mHash = hash;
        result = NE_BOX(strInfo, NE_PT_STRING);
    }
    else
    {
        NeOutOfMemory(N);
    }

    return result;
}

// Given a string buffer and number of bytes, create a Nerd string and return the value.
//
NeValue NeCreateString(Nerd N, const char* str, NeUInt size)
{
    if (-1 == size) size = StrLen(str);

    return CreateString(N, str, size, Hash(str, size, NE_DEFAULT_SEED));
}

// Get the string part of the value
//
NeString NeGetString(NeValue v)
{
    NeStringInfoRef info = NE_CAST(v, NeStringInfo);

    if (NE_IS_STRING(v) || NE_IS_SYMBOL(v))
    {
        return info->mString;
    }
    else
    {
        return "";
    }
}

// Get the string part of the value
//
NeUInt NeGetStringLength(Nerd N, NeValue v)
{
    NeStringInfoRef info = NE_CAST(v, NeStringInfo);

    if (NE_IS_STRING(v) || NE_IS_SYMBOL(v))
    {
        return info->mLength;
    }
    else
    {
        return 0;
    }
}

//----------------------------------------------------------------------------------------------------{TABLE}
//----------------------------------------------------------------------------------------------------
// T A B L E S   M A N A G E M E N T
//----------------------------------------------------------------------------------------------------
//----------------------------------------------------------------------------------------------------

// Initialise a table structure
//
static void InitTable(Nerd N, NeTableRef table, NeTableRef parent)
{
    table->mLogNumNodes = 0;
    table->mNodes = 0;
    table->mLastFreeNode = 0;
    table->mParent = parent;
}

// This function is called by the garbage collector when it needs to delete a table and free all
// memory associated with it
//
static void DestroyTableElement(Nerd N, void* tableObject)
{
    NeTableRef table = (NeTableRef)tableObject;
    NeUInt numNodes = table->mNodes ? 1ull << table->mLogNumNodes : 0;
    NE_FREE(N, table->mNodes, numNodes * sizeof(NeNode), NeMemoryType_TableNodes);
    InitTable(N, table, 0);
}

// Mark a table for garbage collection
//
static void MarkTable(Nerd N, NeTableRef table, NeBool markKeys)
{
    NeUInt i = 0;

    while (table)
    {
        NeUInt numNodes = 1ull << table->mLogNumNodes;

        if (table->mGcObj.mMarked != G(mMarkColour))
        {
            table->mGcObj.mMarked = G(mMarkColour);
            if (table->mNodes)
            {
                for (i = 0; i < numNodes; ++i)
                {
                    NeNodeRef node = &table->mNodes[i];
                    if (node->mKey)
                    {
                        if (markKeys) MarkValue(N, node->mKey);
                        MarkValue(N, node->mValue);
                    }
                }
            }
            table = table->mParent;
        }
        else
        {
            table = 0;
        }
    }
}

// Create an empty table
//
NeTableRef CloneTable(Nerd N, NeTableRef parentTable)
{
    NeTableRef table = (NeTableRef)PoolAcquire(&G(mTablesPool), 1, NE_PT_TABLE);

    if (!table)
    {
        NeOutOfMemory(N);
        return 0;
    }

    InitTable(N, table, parentTable);

    return table;
}

// Create an empty table based on values
//
NeValue NeCloneTable(Nerd N, NeValue parentTable)
{
    NeTableRef parent = NE_CAST(parentTable, NeTable);
    NeTableRef newTable = 0;

    // Check to see if the parent is a valid table
    if (parentTable && !NE_IS_TABLE(parentTable))
    {
        NeError(N, "Attempting to clone a table from a non-table.");
        return 0;
    }

    newTable = CloneTable(N, parent);

    return newTable ? NE_BOX(newTable, NE_PT_TABLE) : 0;
}

// This function calculates the perfect position in the table's node array a node should be
// given its key.  If there are no nodes allocated it will return 0;
//
static NeNodeRef TablePosition(NeTableRef table, NeValue key)
{
    NeUInt32 hash;
    NeNodeRef position = 0;

    if (table->mNodes)
    {
        hash = HashValue(key);
        position = &table->mNodes[hash % (1 << table->mLogNumNodes)];
    }

    return position;
}

// Allocate memory for an array of nodes for a given size.
//
static NeBool SetNodeArray(Nerd N, NeTableRef table, NeUInt logSize)
{
    NeUInt numNodes = 0;
    NeUInt i;

    //     if ((0 == numNodes) && table->mNodes)
    //     {
    //         NeFree(N, table->mNodes, sizeof(NeNode) * (1 << table->mLogNumNodes), NeMemoryType_TableNodes);
    //         table->mNumNodes = 0;
    //         table->mLogNumNodes = 0;
    //         table->mNodes = 0;
    //     }
    //     else
    {
        NeNode* nodes;

        if (logSize > NE_MAX_TABLE_LOGSIZE)
        {
            return NeError(N, "Table is too big.");
        }

        // Increase the number of nodes to the next 2^n value and allocate the memory
        numNodes = 1ull << logSize;
        nodes = NE_ALLOC(NeNode, N, sizeof(NeNode)* numNodes, NeMemoryType_TableNodes);
        if (!nodes)
        {
            return NeOutOfMemory(N);
        }
        for (i = 0; i < numNodes; ++i)
        {
            NeNode* n = &nodes[i];
            n->mKey = 0;
            n->mValue = 0;
            n->mNext = 0;
        }
        table->mNodes = nodes;
    }

    table->mLogNumNodes = (NeUInt8)logSize;
    table->mLastFreeNode = &table->mNodes[numNodes];

    return NE_YES;
}

// Return the address of a value with the given key that already exists.  If deep is NE_YES,
// it will search for the key in the parent tables.  All nodes that share the same table
// position, as defined by TablePosition(), are chained via the mNext field in NeNode.
//
static NeValueRef GetTableSlot(NeTableRef table, NeValue key, NeBool deepSearch)
{
    NeNodeRef node;

    while (table)
    {
        for (node = TablePosition(table, key); node; node = node->mNext)
        {
            if (NeEqual(key, node->mKey))
            {
                return &node->mValue;
            }
        }

        // Not found?  Try the parent table if doing deep search.
        if (!deepSearch) return 0;
        table = table->mParent;
    }

    return 0;
}

// Public function that returns the address of a value of a given key it it exists.
//
NeValueRef NeGetTableSlot(Nerd N, NeValue table, NeValue key, NeBool deepSearch)
{
    NeTableRef tableRef = NE_CAST(table, NeTable);
    if (!NE_IS_TABLE(table))
    {
        NeError(N, "Attempting to search a table but the value is not a table.");
        return 0;
    }

    return GetTableSlot(tableRef, key, deepSearch);
}

// Create a new key or return one that already exists.
//
static NeValue* NewTableSlot(Nerd N, NeTableRef table, NeValue key)
{
    NeNode* mp;
    NeNode* scan;

    // Check the validity of the key
    if (0 == key)
    {
        NeError(N, "Invalid table key.");
        return 0;
    }

    // Calculate the main position of the key (where its node should actually be with the
    // nodes array).
    mp = TablePosition(table, key);

    if (mp && (mp->mKey != key))
    {
        // Do a quick search to ensure we don't already have the key by following the chain
        for (scan = mp->mNext; scan != 0; scan = scan->mNext)
        {
            if (scan->mKey == key) return &scan->mValue;
        }
    }

    // If we have a position and the key there matches our key (i.e the key is already
    // stored in the table), we return the value address.
    if (mp && (mp->mKey == key)) return &mp->mValue;

    // If there is no main position (no nodes array), or the main position is already taken by
    // another node, we need to do some manipulation of the table.
    if (!mp || mp->mKey)
    {
        NeNodeRef otherNode = 0;
        NeNodeRef freeNode = 0;

        // We need a new node position that we can fetch from the free list.  The free list intially
        // points at the end of the nodes array.  We search backwards until we find a free slot.
        // We remember the position we searched to for next time so we don't have to keep searching
        // from the end of the nodes array.
        while (table->mLastFreeNode > table->mNodes)
        {
            --table->mLastFreeNode;
            if (!table->mLastFreeNode->mKey)
            {
                freeNode = table->mLastFreeNode;
                break;
            }
        }

        // If all the nodes are filled, freeNode will be 0 at this point.  When this happens we have
        // to increase the size of the nodes array, and reinsert all the nodes.
        if (!freeNode)
        {
            NeNode* oldNodes = table->mNodes;
            NeUInt oldNodesSize = table->mLogNumNodes;
            NeValueRef slot = 0;
            NeUInt i;

            if (!SetNodeArray(N, table, table->mNodes ? table->mLogNumNodes + 1 : 0))
            {
                return 0;
            }

            // Reinsert elements from hash part
            if (oldNodes)
            {
                for (i = (1 << oldNodesSize) - 1; i != -1; --i)
                {
                    NeNode* oldNode = oldNodes + i;
                    if (oldNode->mKey)
                    {
                        NeValue* slot = NewTableSlot(N, table, oldNode->mKey);
                        *slot = oldNode->mValue;
                    }
                }
            }
            NE_FREE(N, oldNodes, sizeof(NeNode)* (1ull << oldNodesSize), NeMemoryType_TableNodes);

            // Retry inserting the new key/value.  We will have enough nodes now.
            slot = NewTableSlot(N, table, key);

            return slot;
        } // if (!freeNode)

        // We have some spare nodes to insert our new key/value.  We calculate the main position
        // for the node that's in our main position.  If it is the same, this node belongs here,
        // and we need to find another position for our new node.  If it doesn't, we can
        // reposition that one, and have our node in the correct position.
        otherNode = TablePosition(table, mp->mKey);
        if (otherNode != mp)
        {
            // The other node does not belong here, so let's search for the previous node in the chain.
            // We put the misplaced node into a free slot and add our new node on the end of
            // the chain.
            while (otherNode->mNext != mp) otherNode = otherNode->mNext;
            otherNode->mNext = freeNode;
            *freeNode = *mp;
            mp->mNext = 0;
            mp->mValue = 0;
        }
        else
        {
            // The other node is in the correct position, so we'll use the free node as our new node
            // and chain it.
            freeNode->mNext = mp->mNext;
            mp->mNext = freeNode;
            mp = freeNode;
        }
    }

    // mp points to the new node position.
    mp->mKey = key;
    return &mp->mValue;
}

// The public function for creating a new slot
//
NeValue* NeNewTableSlot(Nerd N, NeValue table, NeValue key)
{
    NeTableRef tableRef = NE_CAST(table, NeTable);
    if (!NE_IS_TABLE(table))
    {
        NeError(N, "Attempt to add a new key to a table value that is not a table.");
        return 0;
    }

    return NewTableSlot(N, tableRef, key);
}

//----------------------------------------------------------------------------------------------------{KEYVALUE}
//----------------------------------------------------------------------------------------------------
// K E Y / V A L U E   M A N A G E M E N T
//----------------------------------------------------------------------------------------------------
//----------------------------------------------------------------------------------------------------

NeValue NeGetKey(NeValue kv)
{
    return NE_IS_KEYVALUE(kv) ? NE_HEAD(kv) : 0;
}

NeValue NeGetValue(NeValue kv)
{
    return NE_IS_KEYVALUE(kv) ? NE_TAIL(kv) : 0;
}

//----------------------------------------------------------------------------------------------------{SYMBOL}
//----------------------------------------------------------------------------------------------------
// S Y M B O L S   M A N A G E M E N T
//----------------------------------------------------------------------------------------------------
//----------------------------------------------------------------------------------------------------

//
// Symbols take this form:
//
//      ( symbol-name . 0 )
//
// They are stored in the symbol table whose keys are the hash values of the symbol name.
//

static NeBool StringEqual(NeStringInfoRef strInfo, const char* str, NeUInt size, NeUInt32 hash)
{
    if (strInfo->mHash != hash) return NE_NO;
    if (strInfo->mLength != size) return NE_NO;

    return (strncmp(strInfo->mString, str, (size_t)size) == 0) ? NE_YES : NE_NO;
}

NeValue CreateSymbol(Nerd N, const char* str, NeUInt size, NeUInt32 hash, NeValue type)
{
    NeValue* slot = 0;
    NeValue newSym = 0;
    NeValue symString = 0;

    if (0 == size)
    {
        NeError(N, "Attempted to create a symbol of zero length.");
        return 0;
    }

    if (-1 == size)
    {
        size = StrLen(str);
    }

    slot = NewTableSlot(N, NE_CAST(G(mSymbolTable), NeTable), (NeValue)hash);
    if (slot)
    {
        // A symbol is stored in the table as (name . next), where name is a string value and next
        // it a cell pointer to the next string that shares the same hash string.
        if (*slot)
        {
            // This slot already has a string value.  We need to traverse the list to check that
            // we don't already have the symbol.
            NeValue scan = *slot;
            while (scan)
            {
                NeStringInfoRef strInfo = NE_CAST(NE_HEAD(scan), NeStringInfo);

                if (StringEqual(strInfo, str, size, hash))
                {
                    // We've found a matching string
                    return NE_BOX(strInfo, type);
                }

                scan = NE_TAIL(scan);
            }
        }

        // If we reach this point, we don't have the symbol.  We add the cell (name . 0) to the
        // head of the list stored in the table slot.
        symString = CreateString(N, str, size, hash);
        if (symString)
        {
            newSym = NeCreateCons(N, symString, 0);
            if (newSym)
            {
                *slot = newSym;
                newSym = NE_BOX(symString, type);
            }
        }
    }

    return newSym;
}

NeValue NeCreateSymbol(Nerd N, const char* str, NeUInt size)
{
    if (-1 == size)
    {
        size = StrLen(str);
    }

    return CreateSymbol(N, str, size, Hash(str, size, NE_DEFAULT_SEED), NE_PT_SYMBOL);
}

NeValue NeCreateKeyword(Nerd N, const char* str, NeUInt size)
{
    if (0 == size)
    {
        NeError(N, "Attempted to create a keyword of zero length.");
        return 0;
    }

    if (-1 == size) size = StrLen(str);

    return CreateSymbol(N, str, size, Hash(str, size, NE_DEFAULT_SEED), NE_PT_KEYWORD);
}

NeString NeGetSymbolName(NeValue symbol)
{
    return NeGetString(symbol);
}

//----------------------------------------------------------------------------------------------------{NUMBER}
//----------------------------------------------------------------------------------------------------
//	N U M B E R S   M A N A G E M E N T
//----------------------------------------------------------------------------------------------------
//----------------------------------------------------------------------------------------------------

#define NE_MAX_INT(bits)	((1ll << (bits - 1)) - 1)
#define NE_MIN_INT(bits)	-(1ll << (bits - 1))

static NeBool NewNumber(Nerd N, NE_OUT NeNumberRef* number)
{
    *number = (NeNumberRef)PoolAcquire(&G(mNumbersPool), 1, NE_PT_NUMBER);
    return *number ? NE_YES : (NeOutOfMemory(N), NE_NO);
}

NeValue NeCreateInteger(Nerd N, NeInt i)
{
    NeNumber num;
    num.mNumType = NeNumberType_Integer;
    num.mInteger = i;
    return NeSetNumber(N, 0, &num);
}

NeValue NeCreateFloat(Nerd N, NeFloat f)
{
    NeNumber num;
    num.mNumType = NeNumberType_Float;
    num.mFloat = f;
    return NeSetNumber(N, 0, &num);
}

NeValue NeCreateRatio(Nerd N, NeInt n, NeInt d)
{
    NeNumber num;
    num.mNumType = NeNumberType_Ratio;
    num.mNumerator = n;
    num.mDenominator = d;
    return NeSetNumber(N, 0, &num);
}

NeBool NeGetNumber(NeValue value, NE_OUT NeNumberRef number)
{
    NeInt i;

    switch (NE_TYPEOF(value))
    {
    case NE_PT_NUMBER:
        *number = *NE_CAST(value, NeNumber);
        break;

    case NE_PT_EXTENDED:
        i = NE_EXTENDED_SIGNED_VALUE(value);
        switch (NE_EXTENDED_TYPEOF(value))
        {
        case NE_XT_SHORTINT:
            number->mNumType = NeNumberType_Integer;
            number->mInteger = i;
            break;

        case NE_XT_SHORTFLOAT:
            i <<= 8;
            number->mNumType = NeNumberType_Float;
            number->mFloat = *(NeFloat *)&i;
            break;

        case NE_XT_SHORTRATIO:
            {
                NeInt denominator = i & 0xfffffffull;
                NeInt numerator = i >> 28;
                number->mNumType = NeNumberType_Ratio;
                number->mNumerator = numerator;
                number->mDenominator = denominator;
            }
            break;

        default:
            return NE_NO;
        }
        break;

    default:
        return NE_NO;

    }

    return NE_YES;
}

static void SimplifyNumber(NeNumberRef number)
{
    if ((number->mNumType == NeNumberType_Ratio) &&
        (number->mDenominator == 1))
    {
        number->mNumType = NeNumberType_Integer;
    }
}

static NeInt GetGCD(NeInt a, NeInt b)
{
    NeInt shift;

    // GCD(0,b) == b, GCD(a,0) == a, GCD(0,0) == 0
    if (a == 0) return b;
    if (b == 0) return a;

    /* Let shift := lg K, where K is the greatest power of 2
    dividing both u and v. */
    for (shift = 0; ((a | b) & 1) == 0; ++shift) {
        a >>= 1;
        b >>= 1;
    }

    while ((a & 1) == 0)
        a >>= 1;

    // From here on, a is always odd.
    do {
        // Remove all factors of 2 in b -- they are not common.
        // Note: b is not zero, so while will terminate
        while ((b & 1) == 0)
            b >>= 1;

        // Now a and b are both odd. Swap if necessary so a <= b,
        // then set b = b - a (which is even). For big numbers, the
        // swapping is just pointer movement, and the subtraction
        // can be done in-place.
        if (a > b)
        {
            NeUInt t = b;
            b = a;
            a = t;
        }
        b = b - a;                       // Here b >= a.
    } while (b != 0);

    // Restore common factors of 2
    return a << shift;
}

static void SimplifyRatio(NeInt* a, NeInt* b)
{
    NeInt aa = (NeInt)(*a < 0 ? -*a : *a);
    NeInt bb = (NeInt)(*b < 0 ? -*b : *b);
    NeInt gcd = GetGCD(aa, bb);

    if (1 != gcd)
    {
        *a /= gcd;
        *b /= gcd;
    }

    if (*b < 0)
    {
        *a = -*a;
        *b = -*b;
    }
}

NeValue NeSetNumber(Nerd N, NeValue origValue, const NeNumberRef number)
{
    NeNumberRef newNum = 0;

    if (number->mNumType == NeNumberType_Ratio)
    {
        if (number->mDenominator == 0) return NeError(N, "Division by zero.");
        SimplifyRatio(&number->mNumerator, &number->mDenominator);
    }
    SimplifyNumber(number);

    // TODO: Convert to short values
    switch (number->mNumType)
    {
    case NeNumberType_Integer:
        if (number->mInteger <= NE_MAX_INT(56) &&
            number->mInteger >= NE_MIN_INT(56))
        {
            return NE_MAKE_EXTENDED_VALUE(NE_XT_SHORTINT, number->mInteger);
        }
        break;

    case NeNumberType_Ratio:
        if (number->mNumerator <= NE_MAX_INT(28) &&
            number->mNumerator >= NE_MIN_INT(28) &&
            number->mDenominator <= NE_MAX_INT(29))
        {
            // This can be stored as a short ratio.
            // The numerator is stored as a signed in the top 28 bits, whereas the denominator (which is always positive),
            // is stored unsigned in the next 28 bits.
            NeUInt d = number->mDenominator & 0xfffffffull;
            NeUInt n = number->mNumerator << 28;
            NeUInt i = *(NeUInt *)&d + *(NeUInt *)&n;
            return NE_MAKE_EXTENDED_VALUE(NE_XT_SHORTRATIO, i);
        }
        break;

    case NeNumberType_Float:
    {
                               NeUInt floatBits = *(NeUInt *)&number->mFloat;
                               if ((floatBits & 0xff) == 0)
                               {
                                   NeValue result = NE_MAKE_EXTENDED_VALUE(NE_XT_SHORTFLOAT, (floatBits >> 8));
                                   return result;
                               }
    }
        break;
    }

    // It's a long number.
    if (NE_IS_PRIMARY_TYPE(origValue, NE_PT_NUMBER))
    {
        // Large number
        newNum = NE_CAST(origValue, NeNumber);
    }

    if (!newNum && !NewNumber(N, &newNum)) return 0;
    *newNum = *number;

    return NE_BOX(newNum, NE_PT_NUMBER);
}

// This function is important as it compares two numbers and promotes either one so that they are
// of equal type.  Nerd supports integers, floats and ratios.  The order of promotion is:
//
//      1.  Integers
//      2.  Ratios
//      3.  Float
//
// This order by the way is the same order as the NeNumberType enums.
//
// This algorithm tries determines which number to promote.  For example, if we compare an integer
// and ratio, the integer will need to be promoted to a ratio, so that we can do processing on
// two ratios.
//
static void PromoteNumbers(NeNumberRef a, NeNumberRef b)
{
    // Let's eleminate the trivial case now.
    if (a->mNumType == b->mNumType) return;

    // Now we order it so that a < b
    if (a->mNumType > b->mNumType)
    {
        NeNumberRef t = a;
        a = b;
        b = t;
    }

    // Let's do the promotion
    while (a->mNumType < b->mNumType)
    {
        switch (a->mNumType)
        {
        case NeNumberType_Integer:
            a->mDenominator = 1;
            a->mNumType = NeNumberType_Ratio;
            break;

        case NeNumberType_Ratio:
            a->mNumType = NeNumberType_Float;
            a->mFloat = ((NeFloat)a->mNumerator / (NeFloat)a->mDenominator);
            break;

        default:;
        }
    }
}

static void AddNumbers(NeNumberRef a, NeNumberRef b, NeNumberRef out)
{
    PromoteNumbers(a, b);

    out->mNumType = a->mNumType;

    switch (a->mNumType)
    {
    case NeNumberType_Integer:
        out->mInteger = a->mInteger + b->mInteger;
        break;

    case NeNumberType_Ratio:
        out->mNumerator = a->mNumerator * b->mDenominator + a->mDenominator * b->mNumerator;
        out->mDenominator = a->mDenominator * b->mDenominator;
        break;

    case NeNumberType_Float:
        out->mFloat = a->mFloat + b->mFloat;
        break;
    }
}

static void SubtractNumbers(NeNumberRef a, NeNumberRef b, NeNumberRef out)
{
    PromoteNumbers(a, b);

    out->mNumType = a->mNumType;

    switch (a->mNumType)
    {
    case NeNumberType_Integer:
        out->mInteger = a->mInteger - b->mInteger;
        break;

    case NeNumberType_Ratio:
        out->mNumerator = a->mNumerator * b->mDenominator - a->mDenominator * b->mNumerator;
        out->mDenominator = a->mDenominator * b->mDenominator;
        break;

    case NeNumberType_Float:
        out->mFloat = a->mFloat - b->mFloat;
        break;
    }
}

static void MultiplyNumbers(NeNumberRef a, NeNumberRef b, NeNumberRef out)
{
    PromoteNumbers(a, b);

    out->mNumType = a->mNumType;

    switch (a->mNumType)
    {
    case NeNumberType_Integer:
        out->mInteger = a->mInteger * b->mInteger;
        break;

    case NeNumberType_Ratio:
        out->mNumerator = a->mNumerator * b->mNumerator;
        out->mDenominator = a->mDenominator * b->mDenominator;
        break;

    case NeNumberType_Float:
        out->mFloat = a->mFloat * b->mFloat;
        break;
    }
}

static void DivideNumbers(NeNumberRef a, NeNumberRef b, NeNumberRef out)
{
    PromoteNumbers(a, b);

    switch (a->mNumType)
    {
    case NeNumberType_Integer:
        out->mNumType = NeNumberType_Ratio;
        out->mNumerator = a->mInteger;
        out->mDenominator = b->mInteger;
        break;

    case NeNumberType_Ratio:
        out->mNumType = NeNumberType_Ratio;
        out->mNumerator = a->mNumerator * b->mDenominator;
        out->mDenominator = a->mDenominator * b->mNumerator;
        break;

    case NeNumberType_Float:
        out->mNumType = NeNumberType_Float;
        out->mFloat = a->mFloat / b->mFloat;
        break;
    }
}

static void ModNumbers(NeNumberRef a, NeNumberRef b, NeNumberRef out)
{
    NE_ASSERT(a->mNumType == NeNumberType_Integer);
    NE_ASSERT(b->mNumType == NeNumberType_Integer);
    out->mNumType = NeNumberType_Integer;
    out->mInteger = a->mInteger % b->mInteger;
}

static NeBool IsZero(NeNumberRef a)
{
    switch (a->mNumType)
    {
    case NeNumberType_Integer:  return a->mInteger == 0;
    case NeNumberType_Ratio:    return a->mNumerator == 0;
    case NeNumberType_Float:    return (a->mFloat == 0) || (a->mFloat == -0);
    default:                    return NE_YES;
    }
}

NeInt NeGetInteger(Nerd N, NeValue a)
{
    switch (NE_TYPEOF(a))
    {
    case NE_PT_NUMBER:
    case NE_PT_EXTENDED:
        {
            NeNumber n;

            if (NE_EXTENDED_TYPEOF(a) != NE_XT_SHORTINT &&
                NE_EXTENDED_TYPEOF(a) != NE_XT_SHORTRATIO &&
                NE_EXTENDED_TYPEOF(a) != NE_XT_SHORTFLOAT)
            {
                return 0;
            }

            NeGetNumber(a, &n);
            switch (n.mNumType)
            {
            case NeNumberType_Integer:
                return n.mInteger;

            case NeNumberType_Float:
                return (NeInt)n.mFloat;

            case NeNumberType_Ratio:
                return (NeInt)((NeFloat)n.mNumerator / (NeFloat)n.mDenominator);
            }
            return 0;
        }

    default:
        return 0;
    }
}

NeFloat NeGetFloat(Nerd N, NeValue a)
{
    switch (NE_TYPEOF(a))
    {
    case NE_PT_NUMBER:
    case NE_PT_EXTENDED:
        {
            NeNumber n;

            if (NE_EXTENDED_TYPEOF(a) != NE_XT_SHORTINT &&
                NE_EXTENDED_TYPEOF(a) != NE_XT_SHORTRATIO &&
                NE_EXTENDED_TYPEOF(a) != NE_XT_SHORTFLOAT)
            {
                return 0.0;
            }

            NeGetNumber(a, &n);
            switch (n.mNumType)
            {
            case NeNumberType_Integer:
                return (NeFloat)n.mInteger;

            case NeNumberType_Float:
                return n.mFloat;

            case NeNumberType_Ratio:
                return (NeFloat)n.mNumerator / (NeFloat)n.mDenominator;
        }
}

    default:
        return 0.0;
    }
}

void NeGetRatio(Nerd N, NeValue a, NeInt* n, NeInt* d)
{
    switch (NE_TYPEOF(a))
    {
    case NE_PT_NUMBER:
    case NE_PT_EXTENDED:
        {
            NeNumber num;

            if (NE_EXTENDED_TYPEOF(a) != NE_XT_SHORTINT &&
                NE_EXTENDED_TYPEOF(a) != NE_XT_SHORTRATIO &&
                NE_EXTENDED_TYPEOF(a) != NE_XT_SHORTFLOAT)
            {
                *n = 0;
                *d = 1;
            }

            NeGetNumber(a, &num);
            switch (num.mNumType)
            {
            case NeNumberType_Integer:
                *n = num.mInteger;
                *d = 1;
                break;

            case NeNumberType_Float:
                {
                    NeInt ln = 0, ld = 1;
                    NeInt hn = 1, hd = 0;
                    NeFloat epsilon = 0.001;
                    NeFloat q = num.mFloat;
                    NeBool neg;

                    if (q < 0.0)
                    {
                        q = -q;
                        neg = NE_YES;
                    }
                    else
                    {
                        neg = NE_NO;
                    }

                    for (;;)
                    {
                        NeInt a = ln + hn;
                        NeInt b = ld + hd;
                        NeFloat m = (NeFloat)a / (NeFloat)b;

                        if ((q - m) > epsilon)
                        {
                            // Q > M -> Replace L by M
                            ln = a;
                            ld = b;
                        }
                        else if ((m - q) > epsilon)
                        {
                            // Q < M -> Replace H by M
                            hn = a;
                            hd = b;
                        }
                        else
                        {
                            *n = a;
                            *d = b;
                            break;
                        }
                    } // for
                    break;
                } // case NeNumberType_Float

            case NeNumberType_Ratio:
                {
                    *n = num.mNumerator;
                    *d = num.mDenominator;
                    break;
                }

            default:
                *n = 0;
                *d = 1;
            } // switch (n.mType)
            break;
        } // NE_PT_NUMBER

    default:
        *n = 0;
        *d = 1;

    } // switch (NE_PRIMARY_TYPE(a))
}

NeNumberType NeGetNumberType(NeValue value)
{
    NeNumber n;

    assert(NE_IS_NUMBER(value));

    NeGetNumber(value, &n);
    return n.mNumType;
}

static NeValue NeAddNumbers(Nerd N, NeValue a, NeValue b)
{
    NeNumber aa, bb, result;

    NeGetNumber(a, &aa);
    NeGetNumber(b, &bb);

    AddNumbers(&aa, &bb, &result);

    return NeSetNumber(N, 0, &result);
}

static NeValue NeSubtractNumbers(Nerd N, NeValue a, NeValue b)
{
    NeNumber aa, bb, result;

    NeGetNumber(a, &aa);
    NeGetNumber(b, &bb);

    SubtractNumbers(&aa, &bb, &result);

    return NeSetNumber(N, 0, &result);
}

static NeValue NeMultiplyNumbers(Nerd N, NeValue a, NeValue b)
{
    NeNumber aa, bb, result;

    NeGetNumber(a, &aa);
    NeGetNumber(b, &bb);

    MultiplyNumbers(&aa, &bb, &result);

    return NeSetNumber(N, 0, &result);
}

static NeValue NeDivideNumbers(Nerd N, NeValue a, NeValue b)
{
    NeNumber aa, bb, result;

    NeGetNumber(a, &aa);
    NeGetNumber(b, &bb);

    DivideNumbers(&aa, &bb, &result);

    return NeSetNumber(N, 0, &result);
}

static NeValue NeModNumbers(Nerd N, NeValue a, NeValue b)
{
    NeNumber aa, bb, result;

    NeGetNumber(a, &aa);
    NeGetNumber(b, &bb);

    ModNumbers(&aa, &bb, &result);

    return NeSetNumber(N, 0, &result);
}

//----------------------------------------------------------------------------------------------------{FUNCTION}
//----------------------------------------------------------------------------------------------------
//  F U N C T I O N   M A N A G E M E N T
//----------------------------------------------------------------------------------------------------
//----------------------------------------------------------------------------------------------------

NeValue NeCreateClosure(Nerd N, NeValue args, NeValue body, NeValue environment)
{
    // A function will be of the form:
    //
    //      ((args . body) . environment)
    //
    NeValue argsBodyCell = 0;
    NeValue funcCell = 0;
    
    if (!(argsBodyCell = NeCreateCons(N, args, body)))
    {
        NeOutOfMemory(N);
        return 0;
    }

    if (!(funcCell = NeCreateCons(N, argsBodyCell, environment)))
    {
        NeOutOfMemory(N);
        return 0;
    }

    return NE_BOX(funcCell, NE_PT_FUNCTION);
}

//----------------------------------------------------------------------------------------------------{STACK}
//----------------------------------------------------------------------------------------------------
//	U S E R   S T A C K   M A N A G E M E N T
//----------------------------------------------------------------------------------------------------
//----------------------------------------------------------------------------------------------------

void NeClearStack(Nerd N)
{
    NE_ASSERT(N);

    N->mTop = 0;
}

NeUInt NeStackSize(Nerd N)
{
    NE_ASSERT(N);

    return N->mTop;
}

NeBool NePushValue(Nerd N, NeValue v)
{
    NE_ASSERT(N);

    // Check to see if we have room on the stack before we push the value
    if (N->mTop == N->mGlobalSession->mConfig.mStackSize) return NE_NO;

    N->mStack[N->mTop++] = v;
    return NE_YES;
}

static NeBool NePopValue(Nerd N, NE_OUT NeValue* v)
{
    NE_ASSERT(N);
    NE_ASSERT(v);

    // Check to see if the stack is not empty
    if (0 == N->mTop) return NE_NO;

    *v = N->mStack[--N->mTop];
    return NE_YES;
}

//
// Push functions
//

NeBool NePushString(Nerd N, const char* str, NeUInt size)
{
    NeValue v;

    NE_ASSERT(N);
    NE_ASSERT(str);
    NE_ASSERT(size);

    v = NeCreateString(N, str, size);
    if (v)
    {
        return NePushValue(N, v);
    }
    else
    {
        return NE_NO;
    }
}

NeBool NePushSymbol(Nerd N, const char* str, NeUInt size)
{
    NeValue v;

    NE_ASSERT(N);
    NE_ASSERT(str);
    NE_ASSERT(size);

    v = NeCreateSymbol(N, str, size);
    if (v)
    {
        return NePushValue(N, v);
    }
    else
    {
        return NE_NO;
    }
}

//
// Pop functions
//

NeString NePopString(Nerd N)
{
    NeValue v;

    if (!NePopValue(N, &v)) return NE_NO;
    return NeGetString(v);
}

//
// Conversion
//

static NeValue* GetStackElementFromIndex(Nerd N, NeInt stackIndex)
{
    NeUInt index = stackIndex;

    if (stackIndex < 0)
    {
        stackIndex = -stackIndex;
        if (stackIndex >(NeInt)N->mTop)
        {
            NeError(N, "Invalid stack index.");
            return 0;
        }
        else
        {
            index = (NeUInt)(N->mTop - stackIndex);
        }
    }
    else
    {
        if (stackIndex >= (NeInt)N->mTop)
        {
            NeError(N, "Invalid stack index.");
            return 0;
        }
    }

    return &N->mStack[index];
}

NeBool ConvertNumber(Nerd N, NeValue v)
{
    NeNumber num;

    if (!NeGetNumber(v, &num)) return NE_NO;

    switch (num.mNumType)
    {
    case NeNumberType_Integer:
        return FormatScratch(N, "%lld", num.mInteger);
    case NeNumberType_Ratio:
        return FormatScratch(N, "%lld/%lld", num.mNumerator, num.mDenominator);
    case NeNumberType_Float:
        return FormatScratch(N, "%g", num.mFloat);
    }

    return NE_NO;
}

NeBool ConvertToString(Nerd N, NeValue v, int convertMode)
{
    switch (NE_TYPEOF(v))
    {
    case NE_PT_CELL:
        {
            if (0 == v)
            {
                if (convertMode != NE_CONVERT_MODE_NORMAL)
                {
                    return FormatScratch(N, "nil");
                }
                else
                {
                    return NE_YES;
                }
            }

            // (1 2 3):
            //      NORMAL:     123
            //      REPL:       (1 2 3)
            //      CODE:       (1 2 3)
            //
            if (convertMode != NE_CONVERT_MODE_NORMAL)
            {
                if (!AddScratchChar(N, '(')) return NE_NO;
            }
            while (v)
            {
                if (!ConvertToString(N, NE_HEAD(v), convertMode)) return NE_NO;
                v = NE_TAIL(v);
                if ((convertMode != NE_CONVERT_MODE_NORMAL) && v)
                {
                    if (!AddScratchChar(N, ' ')) return NE_NO;
                }
            }
            if (convertMode != NE_CONVERT_MODE_NORMAL)
            {
                if (!AddScratchChar(N, ')')) return NE_NO;
            }
        }
        break;

    case NE_PT_KEYVALUE:
        {
            if (!ConvertToString(N, NE_HEAD(v), convertMode)) return NE_NO;
            if (!FormatScratch(N, ": ")) return NE_NO;
            if (!ConvertToString(N, NE_TAIL(v), convertMode)) return NE_NO;
        }
        break;

    case NE_PT_FUNCTION:
        {
            return FormatScratch(N, (convertMode == NE_CONVERT_MODE_REPL) ? "<function:%p>" : "", NE_CAST(v, void));
        }
        break;

    case NE_PT_TABLE:
        {
            NeTableRef table = NE_CAST(v, NeTable);
            NeUInt numNodes = table->mNodes ? 1 << table->mLogNumNodes : 0;
            NeNodeRef node = table->mNodes;
            NeUInt i = 0;
            NeBool printSpace = NE_NO;

            if (!FormatScratch(N, "[")) return NE_NO;
            for (; i < numNodes; ++i, ++node)
            {
                NeNodeRef node = &table->mNodes[i];

                if (node->mKey != 0)
                {
                    if (printSpace)
                    {
                        if (!FormatScratch(N, " ")) return NE_NO;
                    }
                    else
                    {
                        printSpace = NE_YES;
                    }

                    if (!ConvertToString(N, node->mKey, convertMode)) return NE_NO;
                    if (!FormatScratch(N, ": ")) return NE_NO;
                    if (!ConvertToString(N, node->mValue, convertMode)) return NE_NO;
                }
            }

            return FormatScratch(N, "]");
        }
        break;

    case NE_PT_SYMBOL:
        {
            NeStringInfoRef strInfo = NE_CAST(v, NeStringInfo);
            return FormatScratch(N, "%s", strInfo->mString);
        }

    case NE_PT_KEYWORD:
        {
            NeStringInfoRef strInfo = NE_CAST(v, NeStringInfo);
            return FormatScratch(N, ":%s", strInfo->mString);
        }

    case NE_PT_STRING:
        if (convertMode != NE_CONVERT_MODE_NORMAL)
        {
            NeStringInfoRef strInfo = NE_CAST(v, NeStringInfo);
            char* str = strInfo->mString;

            // In non-normal mode we show the quotes and the backslashed characters.
            if (!AddScratchChar(N, '"')) return NE_NO;
            for (; *str != 0; ++str)
            {
                char c = *str;
                if (c < 32)
                {
                    // TODO: Show backslashed characters and octal values if necessary
                    if (!AddScratchChar(N, c)) return NE_NO;
                }
                else
                {
                    if (!AddScratchChar(N, c)) return NE_NO;
                }
            } // for
            if (!AddScratchChar(N, '"')) return NE_NO;
            break;
        }

        // Purposefully flow into next case statement...

    case NE_PT_NUMBER:
        return ConvertNumber(N, v);

    case NE_PT_EXTENDED:
        switch (NE_EXTENDED_TYPEOF(v))
        {
        case NE_XT_UNDEFINED:	    return FormatScratch(N, (convertMode == NE_CONVERT_MODE_REPL) ? "<undefined>" : "");
        case NE_XT_SHORTINT:
        case NE_XT_SHORTFLOAT:
        case NE_XT_SHORTRATIO:
            return ConvertNumber(N, v);

        case NE_XT_BOOLEAN:         return FormatScratch(N, NE_EXTENDED_VALUE(v) ? "yes" : "no");

        case NE_XT_NATIVE:		return FormatScratch(N, (convertMode == NE_CONVERT_MODE_REPL) ? "<native:%u>" : "", NE_EXTENDED_VALUE(v));

        default:;
            // Flows into outer switch default!
            //		|
            //		|
            //		V
        }

    default:
        if (convertMode == NE_CONVERT_MODE_CODE)
        {
            return FormatScratch(N, "nil");
        }
        else
        {
            return FormatScratch(N, "<Invalid Value>");
        }
    }

    return NE_YES;
}

NeBool NeToString(Nerd N, NeInt index, int convertMode)
{
    NeValue v;
    NeValue* slot = GetStackElementFromIndex(N, index);

    if (slot)
    {
        v = *slot;
        ResetScratch(N);

        if (!ConvertToString(N, v, convertMode)) return NE_NO;

        // Scratch contains the string now
        v = NeCreateString(N, GetScratch(N), GetScratchLength(N));
        if (!v) return NE_NO;

        *slot = v;
        return NE_YES;
    }
    else
    {
        return NE_NO;
    }
}

NeString NeDescribe(Nerd N, NeValue v)
{
    ResetScratch(N);
    if (!ConvertToString(N, v, NE_CONVERT_MODE_REPL)) return 0;
    if (!AddScratchChar(N, 0)) return 0;
    return GetScratch(N);
}

NeBool NeDuplicate(Nerd N, NeInt index)
{
    NeValue* slot = GetStackElementFromIndex(N, index);
    return slot ? NePushValue(N, *slot) : NE_NO;
}

//----------------------------------------------------------------------------------------------------{OUTPUT}
//----------------------------------------------------------------------------------------------------
// O U T P U T
//----------------------------------------------------------------------------------------------------
//----------------------------------------------------------------------------------------------------

void NeOutArgs(Nerd N, const char* format, va_list args)
{
    NeOutputCallback output = N->mGlobalSession->mConfig.mCallbacks.mOutputCallback;

    ResetScratch(N);
    FormatScratchArgs(N, format, args);

    if (output)
    {
        output(N, GetScratch(N));
    }
}

void NeOut(Nerd N, const char* format, ...)
{
    va_list args;

    va_start(args, format);
    NeOutArgs(N, format, args);
    va_end(args);
}

//----------------------------------------------------------------------------------------------------{ERROR}
//----------------------------------------------------------------------------------------------------
// E R R O R   H A N D L I N G
//----------------------------------------------------------------------------------------------------
//----------------------------------------------------------------------------------------------------

static void ErrorArgs(Nerd N, const char* format, va_list args)
{
    ResetScratch(N);
    FormatScratchArgs(N, format, args);
    NePushString(N, GetScratch(N), GetScratchLength(N));
}

NeBool NeError(Nerd N, const char* format, ...)
{
    va_list args;

    va_start(args, format);
    ErrorArgs(N, format, args);
    va_end(args);

    return NE_NO;
}

NeBool NeOutOfMemory(Nerd N)
{
    return NeError(N, "Out of memory!");
}

//----------------------------------------------------------------------------------------------------{LEX}
//----------------------------------------------------------------------------------------------------
// L E X I C A L   A N A L Y S I S
//----------------------------------------------------------------------------------------------------
//----------------------------------------------------------------------------------------------------
// It is the job the lexical analyser to break down a character stream into tokens, ignoring all
// comments and keeping track of the source position.  It's output is fed into the reader that
// constructs a code-list out of it.
//----------------------------------------------------------------------------------------------------

typedef enum _NeToken
{
    // Errors
    NeToken_Unknown = -100,
    NeToken_Error,

    // End of token stream
    NeToken_EOF = 0,

    // Literals
    NeToken_Symbol,             // e.g. foo
    NeToken_String,             // e.g. "foo"
    NeToken_Keyword,            // e.g. :foo
    NeToken_Number,				// e.g. 42, 3.14 or -1/5

    // Characters
    NeToken_Colon,              // :
    NeToken_OpenList,           // (
    NeToken_CloseList,          // )
    NeToken_OpenTable,          // [
    NeToken_CloseTable,         // ]

    // Keywords
    NeToken_KEYWORDS,
    NeToken_Nil,                // nil
    NeToken_Yes,                // yes
    NeToken_No,                 // no
    NeToken_True,               // true
    NeToken_False,              // false

    NeToken_COUNT
}
NeToken;

// This table represents the validity of a name (symbol or keyword) character.
//
//		0 = Cannot be found within a name.
//		1 = Can be found within a name.
//		2 = Can be found within a name but not as the initial character.
//
static const char gNameChar[128] =
{
    //			00	01	02	03	04	05	06	07	08	09	0a	0b	0c	0d	0e	0f	// Characters
    /* 00 */	0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,	// 
    /* 10 */	0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,	// 
    /* 20 */	0, 1, 0, 1, 1, 1, 1, 0, 0, 0, 1, 1, 0, 1, 0, 1,	//  !"#$%&' ()*+,-./
    /* 30 */	2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 0, 0, 1, 1, 1, 1,	// 01234567 89:;<=>?
    /* 40 */	1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,	// @ABCDEFG HIJKLMNO
    /* 50 */	1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0, 0, 0, 1, 1,	// PQRSTUVW XYZ[\]^_
    /* 60 */	0, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,	// `abcdefg hijklmno
    /* 70 */	1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0, 1, 0, 1, 0,	// pqrstuvw xyz{|}~
};

static const unsigned int gKeyWordHashes[] =
{
    /* 0 */		0,
    /* 1 */		NeToken_False,
    /* 2 */		NeToken_True,
    /* 3 */		0,
    /* 4 */		NeToken_Nil,
    /* 5 */		NeToken_Yes,
    /* 6 */		0,
    /* 7 */		0,
    /* 8 */		0,
    /* 9 */		0,
    /* A */		0,
    /* B */		0,
    /* C */		NeToken_No,
    /* D */		0,
    /* E */		0,
    /* F */		0,
};

// The order of this array MUST match the order of the enums after
// NeToken_KEYWORDS.
static const char* gKeywords[NeToken_COUNT - NeToken_KEYWORDS] =
{
    0,
    "3nil",
    "3yes",
    "2no",
    "4true",
    "5false",
};

typedef struct _NeLex
{
    Nerd        mMachine;       // The virtual machine that started the lexical analyser
    const char*         mSource;        // The source code name passed to the lexical analyser
    NeUInt              mLine;          // The current line number
    NeUInt              mLastLine;      // The line number of the last character
    const char*         mCursor;        // The current read position in the stream
    const char*         mEnd;           // Points past the last character in the stream
    const char*         mLastCursor;    // The read position of the last character
    void*               mTempBuffer;    // A copy of the source code if it isn't NULL terminated.
    NeUInt              mTempBufferSize;

    NeToken             mToken;         // The last token read

    // Last token information
    const char*         mStartToken;    // Points to the first character of the token just read
    const char*         mEndToken;      // Points past the last character of the token just read
    NeUInt32            mHash;          // The hash of the last token
    NeNumber			mNumber;		// If the token is a number, it is stored here
    NeBool				mWsFound;		// Set to true if whitespace is found before parsing
}
NeLex, *NeLexRef;

// Initialise a lexical analyser structure for a reading session
//
static NeBool InitLex(Nerd N, const char* source, const char* buffer, NeUInt size,
    NE_IN_OUT NeLex* L)
{
    if (buffer[size - 1] != 0)
    {
        // We need to NULL terminate the input buffer because of the crappy strtod function.
        // This is to avoid a buffer overrun.  The solution is to copy the buffer.
        char* newBuffer = NE_ALLOC(char, N, size + 1, NeMemoryType_Temp);
        if (newBuffer)
        {
            CopyMemory(newBuffer, buffer, size);
            newBuffer[size] = 0;
            L->mTempBuffer = newBuffer;
            L->mTempBufferSize = ++size;
            buffer = newBuffer;
        }
        else
        {
            return NeOutOfMemory(N);
        }
    }
    else
    {
        L->mTempBuffer = 0;
        L->mTempBufferSize = 0;
    }

    L->mMachine = N;
    L->mSource = source;
    L->mLine = 1;
    L->mLastLine = 1;
    L->mCursor = L->mLastCursor = buffer;
    L->mEnd = buffer + size;
    L->mStartToken = L->mEndToken = 0;
    L->mToken = NeToken_Unknown;
    L->mWsFound = NE_YES;

    return NE_YES;
}

// Destroy the memory created from a lexical analyser session
//
static void DestroyLex(NeLexRef L)
{
    NE_FREE(L->mMachine, L->mTempBuffer, L->mTempBufferSize, NeMemoryType_Temp);
}

// Fetch the next character in the stream.  This function keeps track of the newlines.  All the
// different newline representations are converted to just '\n'.  If there are no more characters
// in the stream, a 0 is returned.
//
static char NextChar(NeLexRef L)
{
    char c;

    L->mLastCursor = L->mCursor;
    L->mLastLine = L->mLine;

    if (L->mCursor == L->mEnd) return 0;

    c = *L->mCursor++;

    if ('\r' == c || '\n' == c)
    {
        // Handle newlines
        ++L->mLine;

        if (c == '\r')
        {
            if ((L->mCursor < L->mEnd) && (*L->mCursor == '\n'))
            {
                ++L->mCursor;
            }

            c = '\n';
        }
    }

    return c;
}

// Put a previously read character back on the character stream.  Can only be done once per fetch.
//
static void UngetChar(NeLexRef L)
{
    L->mLine = L->mLastLine;
    L->mCursor = L->mLastCursor;
}

#define NE_IS_WHITESPACE(c) (' ' == (c) || '\t' == (c) || '\n' == c)
#define NE_LEX_RETURN(token) return (L->mWsFound = NE_NO, L->mToken = (token))
#define NE_LEX_ERROR(format, ...) NE_LEX_RETURN(LexError(L, format __VA_ARGS__))

// Show an error while reading
//
static NeToken LexError(NeLexRef L, const char* format, ...)
{
    va_list args;
    Nerd N = L->mMachine;

    ResetScratch(N);

    FormatScratch(N, "%s(%d): READ ERROR: ", L->mSource, L->mLine);
    va_start(args, format);
    FormatScratchArgs(N, format, args);
    va_end(args);

    NeError(N, "%s", GetScratch(N));

    return NeToken_Error;
}

// Fetch the next token from the stream
static NeToken NextToken(NeLexRef L)
{
    char c;
    char prefixChar = 0;

    // Check for EOF
    if (NeToken_EOF == L->mToken) return NeToken_EOF;

    // Find the next meaningful character, skipping whitespace and comments.  Comments are delimited
    // by ';' or '# ' (notice the space) to the end of the line and between '#|' and '|#'.  The last
    // comment type is nestable.
    c = NextChar(L);

    for (;;)
    {
        if (0 == c)
        {
            // End of stream reached
            L->mStartToken = L->mEndToken = L->mCursor;
            NE_LEX_RETURN(NeToken_EOF);
        }

        // Check for whitespace.  If found, ignore, and get the next character in the stream.
        if (NE_IS_WHITESPACE(c))
        {
            c = NextChar(L);
            L->mWsFound = NE_YES;
            continue;
        }

        // Check for comments
        if (';' == c)
        {
            while ((c != 0) && (c != '\n')) c = NextChar(L);
            L->mWsFound = NE_YES;
            continue;
        }
        else if ('#' == c)
        {
            c = NextChar(L);

            if ('|' == c)
            {
                // Nestable, multi-line comment
                NeUInt depth = 1;
                while (c != 0 && depth)
                {
                    c = NextChar(L);
                    if ('#' == c)
                    {
                        // Check for nested #|...|#
                        c = NextChar(L);
                        if ('|' == c)
                        {
                            ++depth;
                        }
                    }
                    else if ('|' == c)
                    {
                        // Check for terminator |#
                        c = NextChar(L);
                        if ('#' == c)
                        {
                            --depth;
                        }
                    }
                }
                L->mWsFound = NE_YES;
                continue;
            }
            else if (NE_IS_WHITESPACE(c))
            {
                // Line-base comment
                while (c != 0 && c != '\n') c = NextChar(L);
                L->mWsFound = NE_YES;
                continue;
            }
            else
            {
                // Prefix character
                prefixChar = c;
                continue;
            }
        } // '#' == c

        // If we've reached this point, we have a meaningful character.
        break;
    } // for(;;)

    // At this point c contains the first character of a meaningful token, and prefixChar has
    // the prefix character if any.
    L->mStartToken = L->mCursor - 1;
    L->mEndToken = L->mCursor;

    //----------------------------------------------------------------------------------------------------
    // Check for Integers and floats
    //----------------------------------------------------------------------------------------------------
    if (
        // Check for digit
        (c >= '0' && c <= '9') ||
        // Check for integer starting with '-' or '+'
        (('-' == c || '+' == c) && (L->mCursor < L->mEnd) &&
        ((*L->mCursor >= '0' && *L->mCursor <= '9') || *L->mCursor == '.')) ||
        // Check for integer starting with '.'
        (('.' == c) && (L->mCursor < L->mEnd) && ((*L->mCursor >= '0' && *L->mCursor <= '9'))))
    {
        // Number
        int state = 0;
        int isFloat = 0;
        int isRatio = 0;
        const char* floatStart = L->mCursor - 1;

        NeInt sign = 1;
        NeInt exponent = 0;
        NeInt intPart = 0;
        NeInt numerator = 0;
        NeInt base = 10;

        char floatBuffer[32];
        char* floatText = floatBuffer;

        // Each state always fetches the next character for the next state
        for (;;)
        {
            switch (state)
            {
            case 0:     // START
                if (c == '-' || c == '+') state = 1;
                else if (c == '.') state = 2;
                else state = 3;
                break;

            case 1:     // +/-
                if (c == '-') sign = -1;
                c = NextChar(L);
                state = 3;
                break;

            case 2:     // .
                if (isRatio) goto bad;
                isFloat = 1;
                state = 4;
                c = NextChar(L);
                break;

            case 3:     // Decide if we have 0 (hex or octal), 1-9 (decimal), or '.' (float)
                if (c == '.') state = 2;
                else if (c == '0') state = 5;
                else if (c >= '1' && c <= '9') state = 6;
                else goto bad;
                break;

            case 4:     // Digits 0-9 in float part
            {
                            while ((c >= '0') && (c <= '9'))
                            {
                                c = NextChar(L);
                            }
                            if (c == 'e' || c == 'E') state = 7;
                            else state = 100;
            }
                break;

            case 5:     // '0' - decide whether we are octal or hexadecimal
                c = NextChar(L);
                if (c == 'x')
                {
                    c = NextChar(L);
                    base = 16;
                }
                else if (c == '.')
                {
                    state = 2;
                    break;
                }
                else if ((c >= '0') && (c <= '9'))
                {
                    base = 8;
                }
                else
                {
                    // Value is 0
                    L->mToken = NeToken_Number;
                    L->mNumber.mNumType = NeNumberType_Integer;
                    L->mNumber.mInteger = 0;
                    state = 100;
                    break;
                }

                state = 8;
                break;

            case 6:     // Integer digits
                while ((c >= '0') && (c <= '9'))
                {
                    NeInt last = intPart;
                    intPart *= 10;
                    intPart += (c - '0');
                    if (intPart < last) goto overflow;
                    c = NextChar(L);
                }
                if (c == '.') state = 2;
                else if (c == 'e' || c == 'E') state = 7;
                else state = 100;
                break;

            case 7:     // Exponent part
                c = NextChar(L);
                if (c == '-' || c == '+') state = 9;
                else if ((c >= '0') && (c <= '9')) state = 10;
                else goto bad;
                break;

            case 8:     // Non-decimal integer digits
                {
                    NeInt last;
                    NeInt x = ((c >= '0') && (c <= '9')) ? (c - '0') :
                        ((c >= 'a') && (c <= 'f')) ? (c - 'a' + 10) :
                        ((c >= 'A') && (c <= 'F')) ? (c - 'A' + 10) : -1;
                    if ((-1 == x) || (x >= base)) state = 100;
                    else
                    {
                        last = intPart;
                        intPart *= base;
                        intPart += x;
                        if (intPart < last) goto overflow;
                        c = NextChar(L);
                    }
                }
                break;

            case 9:     // Exponent sign
                if (c == '-') exponent = -exponent;
                c = NextChar(L);
                state = 10;
                break;

            case 10:    // Exponent digits
                while ((c >= '0') && (c <= '9'))
                {
                    NeInt last = exponent;
                    exponent *= 10;
                    exponent += (c - '0');
                    if (exponent < last) goto overflow;
                    c = NextChar(L);
                }
                state = 100;
                break;

            case 100:   // End of number (possibly)
                // End of number
                if (isFloat)
                {
                    NeUInt len = (NeUInt)(L->mCursor - floatStart - 1);
                    if (c == '/') goto bad;

                    UngetChar(L);

                    if (len > 31)
                    {
                        floatText = (char *)malloc((size_t)(len + 1));
                    }
                    memcpy(floatText, floatStart, (size_t)len);
                    floatText[len] = 0;

                    L->mNumber.mNumType = NeNumberType_Float;
                    L->mNumber.mFloat = strtod(floatText, 0);
                    L->mToken = NeToken_Number;

                    if (floatText != floatBuffer)
                    {
                        free(floatText);
                    }
                    goto finished;
                }
                else
                {
                    // Integrate the exponent and sign if any
                    if (exponent < 0)
                    {
                        goto bad;
                    }
                    else if (exponent > 0)
                    {
                        for (; exponent != 0; exponent--)
                        {
                            NeInt last = intPart;
                            intPart *= 10;
                            if (intPart < last) goto overflow;
                        }
                    }
                    intPart *= sign;

                    if (c == '/')
                    {
                        if (isRatio) goto bad;
                        isRatio = 1;
                        numerator = intPart;
                        intPart = 0;
                        sign = 1;
                        exponent = 0;
                        state = 0;
                        c = NextChar(L);
                    }
                    else
                    {
                        L->mToken = NeToken_Number;
                        if (isRatio)
                        {
                            L->mNumber.mNumType = NeNumberType_Ratio;
                            L->mNumber.mNumerator = numerator;
                            L->mNumber.mDenominator = intPart;
                            goto finished;
                        }
                        else
                        {
                            L->mNumber.mNumType = NeNumberType_Integer;
                            L->mNumber.mInteger = intPart;
                            goto finished;
                        }
                    } // (c == '/')
                } // (isFloat)
                break;
            }
        } // for(;;)

    finished:
        UngetChar(L);
        L->mWsFound = NE_NO;
        return L->mToken;

    bad:
        NE_LEX_ERROR("Invalid number found.");

    overflow:
        NE_LEX_ERROR("Overflow detected in number.  Number is too big.");
    } // if (... digits)

    //----------------------------------------------------------------------------------------------------
    // Check for strings
    //----------------------------------------------------------------------------------------------------

    if ('"' == c)
    {
        L->mStartToken = L->mCursor;
        ResetScratch(L->mMachine);

        for (c = NextChar(L); (0 != c) && ('\n' != c) && ('"' != c); c = NextChar(L))
        {
            if ('\\' == c)
            {
                // Special character
                c = NextChar(L);
                switch (c)
                {
                case 0:
                    {
                        NE_LEX_ERROR("Invalid character found in string.");
                    }

                case 't':   AddScratchChar(L->mMachine, '\t');      break;
                case 'n':   AddScratchChar(L->mMachine, '\n');      break;
                case '0':   AddScratchChar(L->mMachine, '\0');      break;
                case '"':   AddScratchChar(L->mMachine, '\"');      break;
                case '\\':  AddScratchChar(L->mMachine, '\\');      break;
                case 'r':   AddScratchChar(L->mMachine, '\r');      break;
                case 'b':   AddScratchChar(L->mMachine, '\b');      break;
                case 'f':   AddScratchChar(L->mMachine, '\f');      break;
                }
            }
            else
            {
                AddScratchChar(L->mMachine, c);
            }
        }

        if (0 == c || '\n' == c)
        {
            NE_LEX_ERROR("Unterminated string found.");
        }

        L->mEndToken = L->mCursor - 1;
        NE_LEX_RETURN(NeToken_String);
    }

    //----------------------------------------------------------------------------------------------------
    // Check for symbols and keywords
    //----------------------------------------------------------------------------------------------------

    else if ((gNameChar[c] == 1) || (c == ':'))
    {
        NeUInt sizeToken = 0;
        NeBool isKeyword = NE_NO;

        // Check for user keyword (e.g. :foo), and set the flag isKeyword if true.
        if (':' == c)
        {
            // Possible keyword
            if (!L->mWsFound)
            {
                // A ':' immediately after a token (i.e. not whitespace) must be a colon.  For a colon
                // to qualify a symbol to a keyword, whitespace or the beginning of the code must be
                // immediately before it.
                L->mEndToken = L->mCursor;
                NE_LEX_RETURN(NeToken_Colon);
            }

            // Otherwise we check for a following valid initial name character.
            c = NextChar(L);
            if (gNameChar[c] != 1)
            {
                // This is a colon symbol, i.e. part of a key/value pair
                UngetChar(L);
                L->mEndToken = L->mCursor;
                NE_LEX_RETURN(NeToken_Colon);
            }
            isKeyword = NE_YES;
            ++L->mStartToken;
        }

        // Possible symbol or keyword.
        while (gNameChar[c]) c = NextChar(L);
        UngetChar(L);

        L->mEndToken = L->mCursor;
        sizeToken = (NeUInt)(L->mEndToken - L->mStartToken);
        L->mHash = Hash(L->mStartToken, sizeToken, NE_DEFAULT_SEED);

        // Now we determine if this is a built-in keyword or not with a separate token.
        if (!isKeyword)
        {
            NeUInt tokens = gKeyWordHashes[L->mHash & 0xf];
            while (tokens != 0)
            {
                int index = (tokens & 0xff) - NeToken_KEYWORDS;
                tokens >>= 8;

                if ((NeUInt)(*gKeywords[index] - '0') == sizeToken)
                {
                    // The length of the tokens match with the keyword we're currently checking against.
                    if (strncmp(L->mStartToken, gKeywords[index] + 1, (size_t)sizeToken) == 0)
                    {
                        // It is a keyword
                        NE_LEX_RETURN(NeToken_KEYWORDS + index);
                    }
                }
            }
        }

        // If we reach this point, we don't have a built-in keyword, so it must be a symbol or user
        // keyword.
        NE_LEX_RETURN(isKeyword ? NeToken_Keyword : NeToken_Symbol);
    }

    //----------------------------------------------------------------------------------------------------
    // Check for characters
    //----------------------------------------------------------------------------------------------------

    //----------------------------------------------------------------------------------------------------
    // Check for punctuation
    //----------------------------------------------------------------------------------------------------

    else if ('(' == c)      NE_LEX_RETURN(NeToken_OpenList);
    else if (')' == c)      NE_LEX_RETURN(NeToken_CloseList);
    else if ('[' == c)      NE_LEX_RETURN(NeToken_OpenTable);
    else if (']' == c)      NE_LEX_RETURN(NeToken_CloseTable);

    //----------------------------------------------------------------------------------------------------
    // If we've reached this point, we don't know what the token is
    //----------------------------------------------------------------------------------------------------

    NE_LEX_ERROR("Unknown token found.");
}

//----------------------------------------------------------------------------------------------------{READ}
//----------------------------------------------------------------------------------------------------
// R E A D I N G
//----------------------------------------------------------------------------------------------------
//----------------------------------------------------------------------------------------------------

static NeBool ReadExpressions(Nerd N, NeLexRef lex, NeToken terminatingToken, NE_OUT NeValue* result);

// Interpret a token and convert it to a NeValue value.
//
static NeBool InterpretToken(Nerd N, NeLexRef lex, NeToken token, NE_OUT NeValue* result)
{
    const char* start = lex->mStartToken;
    const char* end = lex->mEndToken;
    NeUInt size = (NeUInt)(end - start);

    *result = 0;

    switch (token)
    {
    case NeToken_String:
        *result = CreateString(N, start, size, lex->mHash);
        if (!*result) return NE_NO;
        break;

    case NeToken_Symbol:
        *result = CreateSymbol(N, start, size, lex->mHash, NE_PT_SYMBOL);
        if (!*result) return NE_NO;
        break;

    case NeToken_Keyword:
        *result = CreateSymbol(N, start, size, lex->mHash, NE_PT_KEYWORD);
        if (!*result) return NE_NO;
        break;

    case NeToken_Number:
        *result = NeSetNumber(N, 0, &lex->mNumber);
        if (!*result) return NE_NO;
        break;

    case NeToken_OpenList:
        if (!ReadExpressions(N, lex, NeToken_CloseList, result)) return NE_NO;
        break;

    case NeToken_OpenTable:
        if (!ReadExpressions(N, lex, NeToken_CloseTable, result)) return NE_NO;
        break;

    case NeToken_Error:
        // Don't do anything, the error is handled by NextToken().
        return NE_NO;

    case NeToken_Nil:
        *result = 0;
        break;

    case NeToken_Yes:
    case NeToken_True:
        *result = NE_BOOLEAN_VALUE(1);
        break;

    case NeToken_No:
    case NeToken_False:
        *result = NE_BOOLEAN_VALUE(0);
        break;

    default:
        {
            char* tokenString = NE_ALLOC(char, N, size + 1, NeMemoryType_Temp);
            CopyString(tokenString, start, size);
            tokenString[size] = 0;
            LexError(lex, "Unknown token '%s', encountered while reading.", tokenString);
            NE_FREE(N, tokenString, size + 1, NeMemoryType_Temp);
        }
        return NE_NO;
    }

    return NE_YES;
}

// Reads a series of tokens that terminates with a specific one and creates a list out of it.
//
static NeBool ReadExpressions(Nerd N, NeLexRef lex, NeToken terminatingToken,
    NE_OUT NeValue* result)
{
    NeValue root = 0, lastCell = 0;
    NeValue elem;

    for (;;)
    {
        NeToken token = NextToken(lex);

        // Check to see if we've reached our terminating token
        if (token == terminatingToken)
        {
            if (terminatingToken == NeToken_CloseTable)
            {
                // Convert the list into a table
                NeValue newTable = NeCloneTable(N, 0);
                NeValueRef valueRef = 0;
                NeInt index = 0;

                while (root)
                {
                    NeValue elem = NE_HEAD(root);
                    NeValue key = 0;

                    if (NE_IS_KEYVALUE(elem))
                    {
                        key = NeGetKey(elem);

                        if (!NE_IS_SYMBOL(key))
                        {
                            return NeError(N, "Tables can only have symbol keys.");
                        }

                        elem = NeGetValue(elem);
                    }
                    else
                    {
                        // Array entry
                        key = NeCreateInteger(N, index++);
                    }

                    valueRef = NeNewTableSlot(N, newTable, key);
                    if (!valueRef) return NE_NO;
                    *valueRef = elem;

                    root = NE_TAIL(root);
                }

                *result = newTable;
            }
            else
            {
                *result = root;
            }
            return NE_YES;
        }

        // Check to see we haven't prematurely reached EOF before the end of the expression.
        // If the end of the expression is EOF, the previous if statement would have picked that up.
        if (NeToken_EOF == token)
        {
            LexError(lex, "Unterminated expression found.");
            return NE_NO;
        }
        else if (NeToken_Colon == token)
        {
            NeValue key, value;

            // Handle key/value syntax (i.e. key:value).  If there is no last cell or the next token
            // is the terminating token or EOF we have invalid syntax.  We must have had a previous
            // token to interpret and a following one.
            if (!lastCell)
            {
                LexError(lex, "Invalid key/value expression found without a key.");
                return NE_NO;
            }

            key = NE_HEAD(lastCell);

            // Grab next token and interpret it.
            token = NextToken(lex);
            if (NeToken_EOF == token || terminatingToken == token)
            {
                LexError(lex, "Invalid key/value expression found without a value.");
                return NE_NO;
            }
            if (!InterpretToken(N, lex, token, &value)) return NE_NO;

            // Amend the data structures to tuen the key value at the end of the list to a key/value
            // at the end of the list.
            value = NeCreateCons(N, key, value);
            if (!value) return NE_NO;
            value = NE_BOX(value, NE_PT_KEYVALUE);

            NE_HEAD(lastCell) = value;
            continue;
        }
        else if (!InterpretToken(N, lex, token, &elem))
        {
            return NE_NO;
        }

        // At this point we have a valid value to add to our list
        if (!AppendItem(N, &root, &lastCell, elem)) return NE_NO;
    }
}

// Read a buffer and convert it into a code-list (a list of values).  The most common use of this
// function is to prepare code for the compiler, but you can use this to parse data as well.
//
static NeBool Read(Nerd N, const char* source, const char* str, NeUInt size, NE_OUT NeValue* codeList)
{
    NeBool success;
    NeLex lex;

    NE_ASSERT(N);
    NE_ASSERT(str);
    NE_ASSERT(codeList);

    InitLex(N, source, str, size, &lex);
    success = ReadExpressions(N, &lex, NeToken_EOF, codeList);
    DestroyLex(&lex);

    return success;
}

//----------------------------------------------------------------------------------------------------{EVAL}
//----------------------------------------------------------------------------------------------------
//  E V A L U A T I O N
//----------------------------------------------------------------------------------------------------
//----------------------------------------------------------------------------------------------------

static NeBool Evaluate(Nerd N, NeValue expression, NeTableRef environment, NE_OUT NeValueRef result);
static NeBool EvaluateList(Nerd N, NeValue codeList, NeTableRef environment, NE_OUT NeValueRef result);

static NeBool Assign(Nerd N, NeValue symbol, NeValue value, NeTableRef environment)
{
    NeValueRef slot = 0;

    // Check that the key is a symbol
    if (!NE_IS_SYMBOL(symbol)) return NeError(N, "Attempted assignment to a non-symbol.");

    // Handle the assignment
    slot = GetTableSlot(environment, symbol, NE_NO);
    if (slot)
    {
        // Symbol is already defined.  Cannot redefine symbol.
        return NeError(N, "Symbol '%s' is already defined.", NeGetString(symbol));
    }
    else
    {
#if NE_DEBUG_TRACE_EVAL
        {
            char* varDesc = AllocDescription(N, symbol);
            char* valDesc = AllocDescription(N, value);

            NeOut(N, "ASSIGN [");
            if (environment == NE_CAST(G(mGlobalEnv), NeTable)) NeOut(N, "GLOBAL"); else NeOut(N, "%p", environment);
            NeOut(N, "] %s = %s\n", varDesc, valDesc);

            FreeDescription(N, varDesc);
            FreeDescription(N, valDesc);
        }
#endif
        slot = NewTableSlot(N, environment, symbol);
        if (!slot) return NeOutOfMemory(N);
        *slot = value;
    }

    return NE_YES;
}

NeValue GetFunctionEnv(NeValue func, NeValue env)
{
    return NE_TAIL(func) ? NE_TAIL(func) : env;
}

NeValue GetFunctionArgs(NeValue func)
{
    return NE_HEAD(NE_HEAD(func));
}

NeValue GetFunctionBody(NeValue func)
{
    return NE_TAIL(NE_HEAD(func));
}

static NeBool ExtendEnvironment(Nerd N, NeTableRef callerEnv, NeValue funcEnv, NeValue argNames, NeValue argValues, NE_OUT NeValueRef execEnv)
{
    NeTableRef env;
    NeTableRef funcEnvTable = NE_CAST(funcEnv, NeTable);
    *execEnv = NeCloneTable(N, funcEnv);
    env = NE_CAST(*execEnv, NeTable);

#if NE_DEBUG_TRACE_EVAL
    {
        NeOut(N, "ASSIGN [");
        if (callerEnv == NE_CAST(G(mGlobalEnv), NeTable)) NeOut(N, "GLOBAL"); else NeOut(N, "%p", callerEnv);
        NeOut(N, "] --> [");
        if (env == NE_CAST(G(mGlobalEnv), NeTable)) NeOut(N, "GLOBAL"); else NeOut(N, "%p", env);
        NeOut(N, " <-- ");
        if (funcEnvTable == NE_CAST(G(mGlobalEnv), NeTable)) NeOut(N, "GLOBAL"); else NeOut(N, "%p", funcEnvTable);
        NeOut(N, "]\n");
    }
#endif
    while (argNames && argValues)
    {
        NeValue result = 0;
        if (!Evaluate(N, NE_HEAD(argValues), callerEnv, &result)) return NE_NO;
        if (!Assign(N, NE_HEAD(argNames), result, env)) return NE_NO;
        argNames = NE_TAIL(argNames);
        argValues = NE_TAIL(argValues);
    }

    if (argNames)
    {
        return NeError(N, "Not enough arguments given to function.");
    }
    if (argValues)
    {
        return NeError(N, "Too many arguments given to function.");
    }

    return NE_YES;
}

static NeBool Apply(Nerd N, NeValue func, NeValue args, NeTableRef environment, NE_OUT NeValueRef result)
{
    switch (NE_TYPEOF(func))
    {
    case NE_PT_FUNCTION:
        {
            NeValue funcEnv = GetFunctionEnv(func, NE_BOX(environment, NE_PT_TABLE));
            NeValue funcArgs = GetFunctionArgs(func);
            NeValue funcBody = GetFunctionBody(func);
            NeValue execEnv = 0;

            // Set up the function environment.
            if (!ExtendEnvironment(N, environment, funcEnv, funcArgs, args, &execEnv)) return NE_NO;

            // Execute the function.
            return EvaluateList(N, funcBody, NE_CAST(execEnv, NeTable), result);
        }
        break;

    case NE_PT_EXTENDED:
        switch (NE_EXTENDED_TYPEOF(func))
        {
        case NE_XT_NATIVE:
            // Execute a native function
            {
                NeUInt index = NE_EXTENDED_VALUE(func);
                NeNativeFunc* func;

                func = BufferGet(G(mNativeFuncBuffer), index * sizeof(NeNativeFunc), sizeof(NeNativeFunc));
                return (*func)(N, args, NE_BOX(environment, NE_PT_TABLE), result);
            }

        default:
            // Purposefully fall into following default statement
            //      |
            //      |
            //      V
            ;
        }

    default:
        return NeError(N, "Invalid procedure.");
    }
}

static NeBool Evaluate(Nerd N, NeValue expression, NeTableRef environment, NE_OUT NeValueRef result)
{
    NeBool success = NE_YES;

#if NE_DEBUG_TRACE_EVAL

    if (environment == NE_CAST(G(mGlobalEnv), NeTable))
    {
        NeOut(N, "--> [GLOBAL] ");
    }
    else if (environment == NE_CAST(G(mCoreEnv), NeTable))
    {
        NeOut(N, "--> [CORE] ");
    }
    else
    {
        NeOut(N, "--> [%p] ", environment);
    }
    NePushValue(N, expression);
    NeToString(N, -1, NE_CONVERT_MODE_REPL);
    NeOut(N, "%s\n", NePopString(N));

#endif // NE_DEBUG_TRACE_EVAL

    switch (NE_TYPEOF(expression))
    {
    case NE_PT_CELL:
        {
            // Evaluate a cell.
            NeValue func = 0;

            if (0 == expression)
            {
                *result = 0;
                return NE_YES;
            }

            success = Evaluate(N, NE_HEAD(expression), environment, &func) &&
                Apply(N, func, NE_TAIL(expression), environment, result) ? NE_YES : NE_NO;
            break;
        }

    case NE_PT_KEYVALUE:
        // Evaluate a key/value assignment.
        {
           NeValue key = NeGetKey(expression);
           NeValue value = NeGetValue(expression);

           // Evaluate the value
           success = Evaluate(N, value, environment, &value) &&
               Assign(N, key, value, environment) ? NE_YES : NE_NO;
           if (success) *result = value;
           break;
        }

    case NE_PT_SYMBOL:
        // Evaluate a symbol in the current environment.
        {
            NeValueRef slot = 0;
            slot = GetTableSlot(environment, expression, NE_YES);

            if (!slot) success = NeError(N, "Symbol '%s' undefined.", NeGetString(expression));
            else *result = *slot;
            break;
        }

    default:
        // All other expressions evaluate as themselves.
        *result = expression;
    }

#if NE_DEBUG_TRACE_EVAL

    NeOut(N, "<-- ");
    if (success)
    {
        NePushValue(N, *result);
        NeToString(N, -1, NE_CONVERT_MODE_REPL);
        NeOut(N, "%s\n", NePopString(N));
    }
    else
    {
        NeOut(N, "ERROR!\n");
    }

#endif // NE_DEBUG_TRACE_EVAL

    return success;
}

static NeBool EvaluateList(Nerd N, NeValue codeList, NeTableRef environment, NE_OUT NeValueRef result)
{
    NeValue v = 0;
    while (codeList)
    {
        NeValue expression = NE_HEAD(codeList);
        if (!Evaluate(N, expression, environment, &v)) return NE_NO;
        codeList = NE_TAIL(codeList);
    }

    *result = v;
    return NE_YES;
}

static NeBool NeEval(Nerd N, NeValue expression, NeValue environment, NE_OUT NeValueRef result)
{
    NeTableRef env = NE_CAST(environment ? environment : G(mGlobalEnv), NeTable);
    return Evaluate(N, expression, env, result);
}

//----------------------------------------------------------------------------------------------------{EXEC}
//----------------------------------------------------------------------------------------------------
//  E X E C U T I O N
//----------------------------------------------------------------------------------------------------
//----------------------------------------------------------------------------------------------------

static NeBool Run(Nerd N, const char* source, const char* str, NeUInt size)
{
    NeValue codeList = 0;
    NeValue result = 0;

    // If size is -1 we need to calculate the size of the buffer.  We add 1 because we know we have
    // a null terminator and we want to include it in the buffer, since all buffers must be null terminated due
    // to the rare case of code ending in a float, and the crappy nature of the strtod function!
    if (-1 == size) size = StrLen(str) + 1;

    // Step 1 - parse the code into a list of values (called a code-list).
    if (!Read(N, source, str, size, &codeList)) return NE_NO;
    NE_ASSERT(NE_IS_CELL(codeList));

    // Step 2 - Evaluate the codeList push the result back on.
    if (!EvaluateList(N, codeList, NE_CAST(G(mGlobalEnv), NeTable), &result)) return NE_NO;
    NePushValue(N, result);

    return NE_YES;
}

NeBool NeRun(Nerd N, const char* source, const char* str, NeUInt size)
{
    // Check to see if the process can execute top-level code.
    if (N->mDebugMode)
    {
        return NeError(N, "Process is not ready to execute top-level code.  Need to call NeDebugReset() first.");
    }

    return Run(N, source, str, size);
}

//----------------------------------------------------------------------------------------------------{DEBUG}
//----------------------------------------------------------------------------------------------------
// D E B U G G I N G
//----------------------------------------------------------------------------------------------------
//----------------------------------------------------------------------------------------------------

void NeDebugReset(Nerd N)
{
    N->mDebugMode = NE_NO;
}

//----------------------------------------------------------------------------------------------------{NATIVE}
//----------------------------------------------------------------------------------------------------
// N A T I V E S
//----------------------------------------------------------------------------------------------------
//----------------------------------------------------------------------------------------------------

NeBool NeRegisterNative(Nerd N, const char* nativeName, NeValue environment, NeNativeFunc func)
{
    NeUInt index = BufferLength(G(mNativeFuncBuffer)) / sizeof(NeNativeFunc);
    NeValue sym;
    NeTableRef env;
    
    if (index >= NE_MAX_INT(56))
    {
        return NeError(N, "Too many compiler functions have been registered.");
    }

    // Create a symbol to house the function
    sym = NeCreateSymbol(N, nativeName, -1);
    if (!sym) return NE_NO;

    // Store the function pointer in our buffer
    if (!BufferAdd(&G(mNativeFuncBuffer), &func, sizeof(NeNativeFunc))) return NE_NO;

    // Assign the compiler function value to the symbol in the environment
    env = NE_CAST(environment ? environment : G(mCoreEnv), NeTable);
    return Assign(N, sym, NE_MAKE_EXTENDED_VALUE(NE_XT_NATIVE, index), env);
}

NeBool NeRegisterNatives(Nerd N, NeNativeInfoRef nativeList, NeValue environment)
{
    for (; nativeList->mName != 0; ++nativeList)
    {
        if (!NeRegisterNative(N, nativeList->mName, environment, nativeList->mFunc)) return NE_NO;
    }

    return NE_YES;
}

//----------------------------------------------------------------------------------------------------{STANDARD}
//----------------------------------------------------------------------------------------------------
// S T A N D A R D   N A T I V E S
//----------------------------------------------------------------------------------------------------
//----------------------------------------------------------------------------------------------------

//----------------------------------------------------------------------------------------------------
// Arithmetic
//----------------------------------------------------------------------------------------------------

//
// The special functions for operations on a single operand
//
//  (- x) ==> -x
//  (/ x) ==> 1/x
//

static NeValue SpecialSubtract(Nerd N, NeValue x)
{
    NeValue zero = NeCreateInteger(N, 0);
    return NeSubtractNumbers(N, zero, x);
}

static NeValue SpecialDivide(Nerd N, NeValue x)
{
    NeValue one = NeCreateInteger(N, 1);
    return NeDivideNumbers(N, one, x);
}

//
// The main processor for arithmetic
//

static NeValue DoArith(Nerd N, NeValue args, NeValue env,
    NeValue(*OpFunc) (Nerd N, NeValue a, NeValue b),
    NeValue(*OpSpecialFunc) (Nerd N, NeValue x))
{
    NeValue result;
    NeValue arg;
    NeUInt index = 2;

    // TOOD: Re-use intermediate result so not to keep creating floats.

    NE_NEED_NUM_ARGS(N, args, 1);
    NE_EVAL(N, NE_HEAD(args), env, arg);
    NE_CHECK_ARG_TYPE(N, arg, 1, NeType_Number);

    // Get the first argument.  This determines which one of the sub-functions is called.
    result = arg;

    args = NE_TAIL(args);
    if (OpSpecialFunc && !args)
    {
        return OpSpecialFunc(N, result);
    }
    else
    {
        // Continue the calculation.
        while (args)
        {
            NE_EVAL(N, NE_HEAD(args), env, arg);
            NE_CHECK_ARG_TYPE(N, arg, index++, NeType_Number);
            result = OpFunc(N, result, arg);
            args = NE_TAIL(args);
        }
    }

    return result;
}

static NeBool N_Add(Nerd N, NeValue args, NeValue env, NE_OUT NeValueRef result)
{
    NeValue r = DoArith(N, args, env, &NeAddNumbers, 0);
    if (r)
    {
        *result = r;
        return NE_YES;
    }
    else
    {
        return NE_NO;
    }
}

static NeBool N_Subtract(Nerd N, NeValue args, NeValue env, NE_OUT NeValueRef result)
{
    NeValue r = DoArith(N, args, env, &NeSubtractNumbers, &SpecialSubtract);
    if (r)
    {
        *result = r;
        return NE_YES;
    }
    else
    {
        return NE_NO;
    }
}

static NeBool N_Multiply(Nerd N, NeValue args, NeValue env, NE_OUT NeValueRef result)
{
    NeValue r = DoArith(N, args, env, &NeMultiplyNumbers, 0);
    if (r)
    {
        *result = r;
        return NE_YES;
    }
    else
    {
        return NE_NO;
    }
}

static NeBool N_Divide(Nerd N, NeValue args, NeValue env, NE_OUT NeValueRef result)
{
    NeValue r = DoArith(N, args, env, &NeDivideNumbers, &SpecialDivide);
    if (r)
    {
        *result = r;
        return NE_YES;
    }
    else
    {
        return NE_NO;
    }
}

static NeBool N_Mod(Nerd N, NeValue args, NeValue env, NE_OUT NeValueRef result)
{
    NeValue a, b;

    NE_NEED_EXACTLY_NUM_ARGS(N, args, 2);

    NE_EVAL(N, NE_1ST(args), env, a);
    NE_EVAL(N, NE_2ND(args), env, b);
    
    if (!NE_IS_INTEGER(a) || !NE_IS_INTEGER(b))
    {
        return NeError(N, "Both arguments must be an integer for modulus operation.");
    }
    if (NE_EXTENDED_SIGNED_VALUE(b) == 0)
    {
        return NeError(N, "Division by zero.");
    }

    *result = NeModNumbers(N, a, b);
    return NE_YES;
}

static NeBool N_Fn(Nerd N, NeValue args, NeValue env, NE_OUT NeValueRef result)
{
    NeValue body;
    NeValue scan;
    NeUInt index = 1;
    NeValue func;

    // Get body of function
    NE_NEED_NUM_ARGS(N, args, 2);
    body = NE_TAIL(args);

    // Get arguments of function and check them
    args = NE_HEAD(args);
    NE_CHECK_ARG_TYPE(N, args, 1, NeType_List);

    for (scan = args; scan; scan = NE_TAIL(scan), ++index)
    {
        NeValue arg = NE_HEAD(scan);
        if (!NE_IS_SYMBOL(arg))
        {
            return NeError(N, "Arguments declaration in lambda is invalid.  Argument %u must be a symbol.", index);
        }
    }

    func = NeCreateClosure(N, args, body, env);
    if (func)
    {
        *result = func;
    }

    return func ? NE_YES : NE_NO;
}

NeBool RegisterCoreNatives(Nerd N)
{
    NeNativeInfo info[] = {
        NE_NATIVE("+", N_Add)
        NE_NATIVE("-", N_Subtract)
        NE_NATIVE("*", N_Multiply)
        NE_NATIVE("/", N_Divide)
        NE_NATIVE("%", N_Mod)
        NE_NATIVE("fn", N_Fn)
        NE_END_NATIVES
    };

    return NeRegisterNatives(N, info, 0);
}

//----------------------------------------------------------------------------------------------------
//----------------------------------------------------------------------------------------------------
