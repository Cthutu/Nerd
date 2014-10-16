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

#define NE_STRINGISE(str)		#str
#define NE_STR(str)				NE_STRINGISE(str)

#define NE_MAJOR_VERSION		0
#define NE_MINOR_VERSION		0
#define NE_BUILD_VERSION		0

#define NE_VERSION_STRING NE_STR(NE_MAJOR_VERSION) "." NE_STR(NE_MINOR_VERSION) "." NE_STR(NE_BUILD_VERSION)

#define NE_COPYRIGHT_STRING		"Copyright (C)2012-2013 Matt Davies, all rights reserved."

//----------------------------------------------------------------------------------------------------
// Basic types and definitions
// Nerd is 64-bit even in 32-bit builds and so uses the basic integer type NeInt and NeUInt.  These
// are always 64-bit regardless of platform.
//
// NeOpen() does some sizeof checks to ensure that the types are defined well.
//----------------------------------------------------------------------------------------------------

typedef long long NeInt;            // Standard integer size of Nerd - always 64-bit
typedef unsigned long long NeUInt;  // Unsigned version of standard integer
typedef double NeFloat;             // Standard float of Nerd - always 64-bit
typedef char NeBool;                // Boolean result.  Always either NE_YES or NE_NO.
typedef const char* NeString;       // This string is managed - can be freed by the garbage collector

#define NE_YES (1)
#define NE_NO (0)

#define NE_OUT                      // Marks a parameter as an output parameter.
#define NE_IN_OUT                   // An input parameter that can be updated.

// The opaque type that represents a Nerd virtual machine (NVM).
typedef struct _Nerd* Nerd;

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

    NeMemoryType_COUNT
}
NeMemoryType;

typedef struct _NeMemoryOp
{
    Nerd            mSession;       // The session that is making the memory operation
    void*           mAddress;       // The address for deallocations and reallocations of the original buffer
    NeUInt          mOldSize;       // The size of the buffer before this operation
    NeUInt          mNewSize;       // The intended size of the buffer after this operation
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
    NeUInt              mStackSize;				// Size of the user stack
    NeUInt				mProcessStackSize;		// Size of the stacks used by processes
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
// User stack
//----------------------------------------------------------------------------------------------------

//
// Results are placed on the stack and can be fetched as C types uses the TiPopXXX() functions.
// Lists can be expanded on to the stack for iteration, or compressed to a list from a sequence of 
// values.
//

//
// Stack control
//

//
// Empty the stack completely.
//
void NeClearStack(Nerd N);

//
// Ask for the number of values on the stack.
//
NeUInt NeStackSize(Nerd N);

//
// Stack manipulations functions.
//
NeBool NeDuplicate(Nerd N, NeInt index);

//
// Push commands.
//
NeBool NePushString(Nerd N, const char* str, NeUInt size);
NeBool NePushSymbol(Nerd N, const char* str, NeUInt size);

//
// Pop commands.
//
NeString NePopString(Nerd N);

//
// Convert commands.
// These work on the stack.  The index is determines which element in the stack is converted.
// 0 and positive indices index from the base of the stack (0 = first item pushed, 1 = second item
// etc) and negative indices count back from the top (-1 = last item pushed, -2 second-last item
// pushed etc).
//

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
NeBool NeToString(Nerd N, NeInt index, int convertMode);

//----------------------------------------------------------------------------------------------------
// Execution of code
//----------------------------------------------------------------------------------------------------

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
NeBool NeRun(Nerd N, const char* source, const char* str, NeUInt size);

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