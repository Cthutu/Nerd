//----------------------------------------------------------------------------------------------------
// Nerd language
// Copyright (C)2013 Matt Davies, all rights reserved.
//----------------------------------------------------------------------------------------------------

#ifndef __B1758AE7_BADD_434B_8F5F_AF33A9F49C53_NERD_H
#define __B1758AE7_BADD_434B_8F5F_AF33A9F49C53_NERD_H
#pragma once

#ifdef __cplusplus
extern "C" {
#endif

//----------------------------------------------------------------------------------------------------
//----------------------------------------------------------------------------------------------------
// U S E R ' S   I N T E R F A C E
//----------------------------------------------------------------------------------------------------
//----------------------------------------------------------------------------------------------------
// This interface is used by host programs that just need to embed the Titan virtual machine
// within them.  For native function writers include <titan-ext.h>.
//----------------------------------------------------------------------------------------------------

#define NE_STRINGISE(str)       #str
#define NE_STR(str)             NE_STRINGISE(str)

#define NE_MAJOR_VERSION        0
#define NE_MINOR_VERSION        1
#define NE_BUILD_VERSION        0

#define NE_VERSION_STRING NE_STR(NE_MAJOR_VERSION) "." NE_STR(NE_MINOR_VERSION) "." NE_STR(NE_BUILD_VERSION)

#define NE_COPYRIGHT_STRING     "Copyright (C)2012-2014 Matt Davies, all rights reserved."

//----------------------------------------------------------------------------------------------------
// Basic types and definitions
// Nerd is 64-bit even in 32-bit builds and so uses the basic integer type NeInt and NeUInt.  These
// are always 64-bit regardless of platform.
//
// NeOpen() does some sizeof checks to ensure that the types are defined well.
//----------------------------------------------------------------------------------------------------

typedef long long NeInt;            // Standard integer size of Nerd - always 64-bit
typedef double NeFloat;             // Standard float of Nerd - always 64-bit
typedef char NeBool;                // Boolean result.  Always either NE_YES or NE_NO.
typedef char NeChar;                // Character allowed in Nerd (currently ASCII only).
typedef const NeChar* NeString;     // This string is managed - can be freed by the garbage collector

#define NE_YES (1)
#define NE_NO (0)

#define NE_OUT                      // Marks a parameter as an output parameter.
#define NE_IN_OUT                   // An input parameter that can be updated.

// The opaque type that represents a Nerd virtual machine (NVM).
typedef struct _Nerd* Nerd;

typedef unsigned long long NeBits;  // NeBits is an unsigned version of NeInt and used for values 
                                    // where bit position is important (like NeValue and bit fields).
typedef NeBits NeValue;
typedef NeValue* NeValueRef;

//----------------------------------------------------------------------------------------------------
// User-defined callbacks
// Nerd has no dependency on any 3rd party libraries.  To be able to allocate memory, read from
// files etc, the host program must provide this functionality through the callbacks defined below.
// The NeConfigCallbacks structure is contained in the NeConfig structure that is passed to NeOpen()
// when creating a Nerd session.
//----------------------------------------------------------------------------------------------------

//
// Passed to the memory callback function as a hint about what type of allocation is occurring
//
typedef enum _NeMemoryType
{
    NeMemoryType_None,
    NeMemoryType_Temp,
    NeMemoryType_UserStack,
    NeMemoryType_Session,
    NeMemoryType_String,
    NeMemoryType_TableNodes,
    NeMemoryType_Buffer,
    NeMemoryType_PoolHeap,
    NeMemoryType_Object,

    NeMemoryType_COUNT
}
NeMemoryType;

typedef struct _NeMemoryOp
{
    Nerd            mSession;       // The session that is making the memory operation
    void*           mAddress;       // The address for deallocations and reallocations of the original buffer
    NeInt           mOldSize;       // The size of the buffer before this operation
    NeInt           mNewSize;       // The intended size of the buffer after this operation
    NeMemoryType    mType;          // The type of memory being allocated
}
NeMemoryOp, *NeMemoryOpRef;

typedef void* (*NeMemoryCallback) (NeMemoryOpRef memoryOperation);
typedef void (*NeOutputCallback) (Nerd N, const char* outputString);

typedef struct _NeConfigCallbacks
{
    NeMemoryCallback    mMemoryCallback;
    NeOutputCallback    mOutputCallback;
}
NeConfigCallbacks;

//----------------------------------------------------------------------------------------------------
// Configuration structure
// This is used to set up a Nerd session using the function NeOpen().  It is used to integrate
// Nerd into the host program.
//----------------------------------------------------------------------------------------------------

typedef struct _NeConfig
{
    NeConfigCallbacks   mCallbacks;             // All the callbacks
    NeInt               mProcessStackSize;      // Size of the stacks used by processes
}
NeConfig, *NeConfigRef;

//
// Initialises the NeConfig structure to default settings.
//
void NeSetConfigToDefault(NeConfigRef config);

//----------------------------------------------------------------------------------------------------
// Session management
//----------------------------------------------------------------------------------------------------

extern const char* gNeOpenError;

//
// Create a new session to run Titan code
// Returns 0 if an invalid configuration was provided or there was an out of memory problem.  If 0
// is returned, the global variable gNeOpenError will point to a string describing the error.  This
// should be used for development purposes.  In production code, a return value of 0 should just
// mean out of memory and gNeOpenError will be "Out of Memory".
//
Nerd NeOpen(NeConfigRef config);

//
// Close a previously created session.
//
void NeClose(Nerd N);

//
// Garbage collect all unused data in a session.
//
void NeGarbageCollect(Nerd N);

//----------------------------------------------------------------------------------------------------
// Variables & environments
//----------------------------------------------------------------------------------------------------

// Retrieve a value from an environment.  Use 0 for the global environment of the VM.
NeValue NeGetSymbolValue(Nerd N, NeValue env, const char* varName);

// Associate a value to a symbol name in a particular environment.  Use 0 for the global environment
// of the VM.  Will return NE_YES if successful or NE_NO if out of memory.
NeBool NeSetSymbolValue(Nerd N, NeValue env, const char* varName, NeValue value);

// Discover if a symbol is defined in a particular environment.  Use 0 for the global environment.
NeBool NeIsDefined(Nerd N, NeValue env, const char* symName);

//----------------------------------------------------------------------------------------------------
// Value conversions
//----------------------------------------------------------------------------------------------------

#define NE_CONVERT_MODE_NORMAL      0
#define NE_CONVERT_MODE_REPL        1
#define NE_CONVERT_MODE_CODE        2

// For conversion to string, there is also a convert mode which determines what form the final 
// form.  There are 3 supported modes:
//
//      NE_CONVERT_MODE_NORMAL      Just the contents of the values.
//      NE_CONVERT_MODE_REPL        Type decoration is added (e.g. strings have quotes, lists have parentheses).
//      NE_CONVERT_MODE_CODE        A string of the value that can be compiled as that value, or nil if it can't
//
NeString NeToString(Nerd N, NeValue v, int convertMode);

// Convert a value to an integer.
// If the value is not compatible, it will return 0.
NeInt NeToInt(Nerd N, NeValue v);

// Convert a value to a float.
NeFloat NeToFloat(Nerd N, NeValue v);

//
// Creation functions
//

// Create a cons-cell given the head and tail.  Will return 0 if there is an out of memory error.
//
NeValue NeCreateCons(Nerd N, NeValue head, NeValue tail);

// Create a value from a string.  Use -1 for size if the string is null terminated and you want
// to calculate this programmatically.
//
NeValue NeCreateString(Nerd N, const char* str, NeInt size);

//----------------------------------------------------------------------------------------------------
// Execution of code
//----------------------------------------------------------------------------------------------------

// Current error state.  Will return 0 if there is no error or a string value that describes
// the error.
NeString NeGetError(Nerd N);

// NeRun will take a buffer of code, read and evaluate it leaving the result or error
// message (as a string) on the stack.  It will return NE_YES if it compiled and executed correctly.
// A compilation error will just cause the function to exit immediately.  A runtime error, will also
// cause the function to exit immediately but will put the VM into a debug state where the next
// time you run code it will be in the context of where it failed.  At this point, the debug functions
// called NeDebugXXX() will be enabled.  You will need to call NeDebugReset() on the session to
// reset the VM and allow the use of NeRun again.  To execute code in debug mode, use NeDebugRun().
//
// The source parameter is a string that describes where the code came from.  This is used in error
// messages to help the user pinpoint them.
//
NeBool NeRun(Nerd N, const char* source, const char* str, NeInt size, NE_OUT NeValueRef result);

// This function will run a string that doesn't originate from a file.  It's a convenient wrapper
// around NeRun.
NeBool NeQuickRun(Nerd N, const char* str, NE_OUT NeValueRef result);

//----------------------------------------------------------------------------------------------------
// Debugging
//----------------------------------------------------------------------------------------------------

// When an error occurs, the process that failed is in a debug state and cannot be run any further.  This
// function resets the process' state to WAITING so you can once more run NeRun.
//
void NeDebugReset(Nerd N);

//----------------------------------------------------------------------------------------------------
//----------------------------------------------------------------------------------------------------

#ifdef __cplusplus
}
#endif

#endif // __B1758AE7_BADD_434B_8F5F_AF33A9F49C53_NERD_H
