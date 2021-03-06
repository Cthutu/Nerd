//----------------------------------------------------------------------------------------------------
// Nerd language extensions
// Copyright (C)2013 Matt Davies, all rights reserved.
//----------------------------------------------------------------------------------------------------
// This header file is required for people wishing to write native functions to extend the core
// language.  For examples of using this header, refer to source code in nerd.c or the "Nerd
// Extension Writer Manual".
//----------------------------------------------------------------------------------------------------

#ifndef __B1758AE7_BADD_434B_8F5F_AF33A9F49C53_NERD_INT_H
#define __B1758AE7_BADD_434B_8F5F_AF33A9F49C53_NERD_INT_H
#pragma once

#ifdef __cplusplus
extern "C"
{
#endif

#include "nerd.h"
#include <stdarg.h>
#include <assert.h>
#include <stdint.h>

//----------------------------------------------------------------------------------------------------
// Macros
//----------------------------------------------------------------------------------------------------

#define NE_ASSERT(cond, ...) assert(cond)

// Convert a NeValue to a structure pointer
#define NE_CAST(v, t) ((t *)((v) & ~0xfull))

// Convert a NeValue to a NeCell pointer
#define NE_CELL(v) NE_CAST(v, NeCell)

// Converts an address and a Nerd type (4 bit code) into a NeValue
#define NE_BOX(p, tt) (((NeBits)(p) & ~0xfull) | (tt))

// Gets the type of a TiValue
#define NE_TYPEOF(v) ((v) & 0x0f)
#define NE_EXTENDED_TYPEOF(v) ((v) & 0xf0)

// Type creation macros
#define NE_MAKE_EXTENDED_VALUE(xt, value)       (0xfull | (xt) | ((value) << 8))

// Obtaining values from the extended types
#define NE_EXTENDED_VALUE(v) ((v) >> 8)
#define NE_EXTENDED_SIGNED_VALUE(v) ((NeInt)(v) >> 8)

//----------------------------------------------------------------------------------------------------
// Basic internal types
//----------------------------------------------------------------------------------------------------

// A value has a specific bit format.  The lower 4 bits determine the type and fall into two categories:
// cell hierarchy types (values that are completely formed by cells with Nerd values); and non-pure
// types, which are often addresses aligned to 16 bytes (NeAlloc guarantees this property).
//
// The description of the 4 lower bits are as follows:
//
//  BITS    VALUE   TYPE            TYPE    DESCRIPTION
//
//  --- Cell-hierarchy based values ------------------------------------------------
//  0000    0       Cell            L       Points to a NeCell structure.
//  0001    1       Key/Value       L       Points to a NeCell structure.
//  0010    2       Function        N       Is of form ((Block args) body ...)
//  0011    3       Macro           N       Same as Function
//  0100    4       Sequence        L       Points to a NeCell structure.
//  0101    5       ?
//  0110    6       ?
//  0111    7       ?
//
//  --- Structure address based values ---------------------------------------------
//  1000    8       Table           L       Points to a NeTable structure.
//  1001    9       Symbol          L       Points to a NeStringInfo structure.
//  1010    A       String          L       Points to a NeStringInfo structure.
//  1011    B       Keyword         L       Points to a NeStringInfo structure.
//  1100    C       Block           N       Points to a NeBlock structure.
//  1101    D       ?
//  1110    E       Number          L       Points to a NeNumber structure.
//
//  --- Extended values ------------------------------------------------------------
//  1111    F       Extended        Bits 4-7 determine type, and the other 56 bits 
//                                      determine specific information.
//
// All non-extended values (i.e. types 0-14) are actually addresses.  Types 0-7 always point to a
// NeCell structure.  To access the address, use the macro NE_CELL:
//
//      NeCell* cell = NE_CELL(value);
//
// Types 0-14 pointer to different structure types.  For example, to get the string information from
// a value that you know is a string, you use the NE_CAST macro:
//
//      NeStringInfoRef str = NE_CAST(value, NeStringInfoRef);
//
// NE_CELL is actually a NE_CAST to a NeCellRef pointer.
//
// For extended types, we use bits 4-7 to determine the type and the other 56 bits
// are specific to the type:
//
//  BITS    VALUE   TYPE                TYPE    BITS 8-63           DESCRIPTION
//  0000    0f      Constant types      N       Index of type       Represents a value that is constant
//  0001    1f      Short int           L       56-bit int          Represents a small integer value
//  0010    2f      Short float         L       56-bit float        Represents a small 56-bit float
//  0011    3f      Short ratio         L       28-bit values       Represents a small ratio
//  0100    4f      Boolean             L       0=no, 1=yes         Represents a boolean value
//  0101    5f      Native function     N       Index of func       Represents a C function that can be called from Titan code
//  0110    6f      Character           L       8-bit character     Represents a single ASCII character
//  0111    7f      ?
//  1000    8f      ?
//  1001    9f      ?
//  1010    af      ?
//  1011    bf      ?
//  1100    cf      ?
//  1101    df      ?
//  1110    ef      ?
//  1111    ff      ?
//
// The constant types supported for NE_XT_CONSTANT are:
//
//      Type    Literal     Description
//      0       undefined   Represents an undefined type.
//      1       quote       Represents a ' symbol.  This is transformed by the reader so should never evaluate it.
//      2       lambda      Represents a -> keyword.  This is transformed by the reader to (fn ...).
//      3       macrosym    Represents a => keyword.  This is transformed by the reader to (macro ...).
//      4       backquote   Represents a ` keyword.  This is transformed by the reader to (backquote ...).
//      5       comma       Represents a , keyword.
//      6       splice      Represents a ,@ keyword.
//      7       colon       Represents a : symbol.  This is transformed by the reader to a keyvalue.
//
// Checklist for adding new value type:
//
//      1.  Add NE_PT_xxx or NE_XT_xxx define below.
//      2.  Add NE_IS_xxx macro.
//      3.  Add support for type in NeEqual().
//      4.  Add it to tables above.
//      5.  Implement marking in MarkValue().
//      6.  Add support in BufferAddValue().
//      7.  Add support for ConvertToString().
//
// The type codes describe the categories of the value types:
//
//      L = Literal         Value can be represented by Nerd code.
//      N = Non-literal     Value cannot be represented by Nerd code, but a variable can hold a value.
//

// Types
typedef enum _NeType
{
    // Basic
    NeType_Nil,
    NeType_List,
    NeType_KeyValue,
    NeType_Function,
    NeType_Macro,
    NeType_Sequence,
    NeType_Table,
    NeType_Symbol,
    NeType_String,
    NeType_Keyword,
    NeType_Block,
    NeType_Object,
    NeType_Number,

    // Extended
    NeType_Undefined,
    NeType_Boolean,
    NeType_Native,
    NeType_Character,

    // Constants
    NeType_Special,

    NeType_COUNT
}
NeType;

// Primary types:
#define NE_PT_CELL          0
#define NE_PT_KEYVALUE      1
#define NE_PT_FUNCTION      2
#define NE_PT_MACRO         3
#define NE_PT_SEQUENCE      4
#define NE_PT_TABLE         8
#define NE_PT_SYMBOL        9
#define NE_PT_STRING        10
#define NE_PT_KEYWORD       11
#define NE_PT_BLOCK         12
#define NE_PT_OBJECT        13
#define NE_PT_NUMBER        14
#define NE_PT_EXTENDED      15

// Extended types:
#define NE_XT_CONSTANT      (0 << 4)
#define NE_XT_SHORTINT      (1 << 4)
#define NE_XT_SHORTFLOAT    (2 << 4)
#define NE_XT_SHORTRATIO    (3 << 4)
#define NE_XT_BOOLEAN       (4 << 4)
#define NE_XT_NATIVE        (5 << 4)
#define NE_XT_CHARACTER     (6 << 4)

// Constant types:
#define NE_C_UNDEFINED      0
#define NE_C_QUOTE          1
#define NE_C_LAMBDA         2
#define NE_C_MACROSYM       3
#define NE_C_BACKQUOTE      4
#define NE_C_COMMA          5
#define NE_C_SPLICE         6
#define NE_C_COLON          7
#define NE_C_DOT            8

// Type checking macros
#define NE_IS_PRIMARY_TYPE(v, tt)       (NE_TYPEOF(v) == (tt))
#define NE_IS_EXTENDED_TYPE(v, xt)      ((NE_TYPEOF(v) == NE_PT_EXTENDED) && (NE_EXTENDED_TYPEOF(v) == (xt)))
#define NE_IS_CELL(v)                   NE_IS_PRIMARY_TYPE((v), NE_PT_CELL)
#define NE_IS_KEYVALUE(v)               NE_IS_PRIMARY_TYPE((v), NE_PT_KEYVALUE)
#define NE_IS_FUNCTION(v)               NE_IS_PRIMARY_TYPE((v), NE_PT_FUNCTION)
#define NE_IS_MACRO(v)                  NE_IS_PRIMARY_TYPE((v), NE_PT_MACRO)
#define NE_IS_SEQUENCE(v)               NE_IS_PRIMARY_TYPE((v), NE_PT_SEQUENCE)
#define NE_IS_TABLE(v)                  NE_IS_PRIMARY_TYPE((v), NE_PT_TABLE)
#define NE_IS_SYMBOL(v)                 NE_IS_PRIMARY_TYPE((v), NE_PT_SYMBOL)
#define NE_IS_STRING(v)                 NE_IS_PRIMARY_TYPE((v), NE_PT_STRING)
#define NE_IS_KEYWORD(v)                NE_IS_PRIMARY_TYPE((v), NE_PT_KEYWORD)
#define NE_IS_BLOCK(v)                  NE_IS_PRIMARY_TYPE((v), NE_PT_BLOCK)
#define NE_IS_OBJECT(v)                 NE_IS_PRIMARY_TYPE((v), NE_PT_OBJECT)
#define NE_IS_NUMBER(v)                 (NE_IS_PRIMARY_TYPE((v), NE_PT_NUMBER) || NE_IS_EXTENDED_TYPE((v), NE_XT_SHORTINT) || \
                                         NE_IS_EXTENDED_TYPE((v), NE_XT_SHORTFLOAT) || NE_IS_EXTENDED_TYPE((v), NE_XT_SHORTRATIO))
#define NE_IS_INTEGER(v)                ((NE_IS_PRIMARY_TYPE((v), NE_PT_NUMBER) && (NE_CAST((v), NeNumber)->mNumType == NeNumberType_Integer)) || \
                                         (NE_IS_EXTENDED_TYPE((v), NE_XT_SHORTINT)))
#define NE_IS_FLOAT(v)                  ((NE_IS_PRIMARY_TYPE((v), NE_PT_NUMBER) && (NE_CAST((v), NeNumber)->mNumType == NeNumberType_Float)) || \
                                         (NE_IS_EXTENDED_TYPE((v), NE_XT_SHORTFLOAT)))
#define NE_IS_RATIO(v)                  ((NE_IS_PRIMARY_TYPE((v), NE_PT_NUMBER) && (NE_CAST((v), NeNumber)->mNumType == NeNumberType_Ratio)) || \
                                         (NE_IS_EXTENDED_TYPE((v), NE_XT_SHORTRATIO)))
#define NE_IS_UNDEFINED(v)              (NE_IS_EXTENDED_TYPE((v), NE_XT_CONSTANT) && (NE_EXTENDED_VALUE((v)) == NE_C_UNDEFINED))
#define NE_IS_QUOTE(v)                  (NE_IS_EXTENDED_TYPE((v), NE_XT_CONSTANT) && (NE_EXTENDED_VALUE((v)) == NE_C_QUOTE))
#define NE_IS_LAMBDA(v)                 (NE_IS_EXTENDED_TYPE((v), NE_XT_CONSTANT) && (NE_EXTENDED_VALUE((v)) == NE_C_LAMBDA))
#define NE_IS_MACROSYM(v)               (NE_IS_EXTENDED_TYPE((v), NE_XT_CONSTANT) && (NE_EXTENDED_VALUE((v)) == NE_C_MACROSYM))
#define NE_IS_BACKQUOTE(v)              (NE_IS_EXTENDED_TYPE((v), NE_XT_CONSTANT) && (NE_EXTENDED_VALUE((v)) == NE_C_BACKQUOTE))
#define NE_IS_COMMA(v)                  (NE_IS_EXTENDED_TYPE((v), NE_XT_CONSTANT) && (NE_EXTENDED_VALUE((v)) == NE_C_COMMA))
#define NE_IS_SPLICE(v)                 (NE_IS_EXTENDED_TYPE((v), NE_XT_CONSTANT) && (NE_EXTENDED_VALUE((v)) == NE_C_SPLICE))
#define NE_IS_COLON(v)                  (NE_IS_EXTENDED_TYPE((v), NE_XT_CONSTANT) && (NE_EXTENDED_VALUE((v)) == NE_C_COLON))
#define NE_IS_DOT(v)                    (NE_IS_EXTENDED_TYPE((v), NE_XT_CONSTANT) && (NE_EXTENDED_VALUE((v)) == NE_C_DOT))
#define NE_IS_READER_UNARY_OP(v)        (NE_IS_QUOTE(v) || NE_IS_BACKQUOTE(v) || NE_IS_COMMA(v) || NE_IS_SPLICE(v))
#define NE_IS_READER_LBINARY_OP(v)      (NE_IS_DOT(v))
#define NE_IS_READER_RBINARY_OP(v)      (NE_IS_LAMBDA(v) || NE_IS_MACROSYM(v) || NE_IS_COLON(v))
#define NE_IS_BOOLEAN(v)                NE_IS_EXTENDED_TYPE((v), NE_XT_BOOLEAN)
#define NE_IS_NATIVE(v)                 NE_IS_EXTENDED_TYPE((v), NE_XT_NATIVE)
#define NE_IS_CHARACTER(v)              NE_IS_EXTENDED_TYPE((v), NE_XT_CHARACTER)
#define NE_IS_CELL_HIERARCHY(v)         (NE_TYPEOF(v) < 8)

#define NE_IS_INTERNAL(v)               (((v) & 0x8f) == 0x8f)

// Value creation macros
#define NE_UNDEFINED_VALUE              NE_MAKE_EXTENDED_VALUE(NE_XT_CONSTANT, NE_C_UNDEFINED)
#define NE_QUOTE_VALUE                  NE_MAKE_EXTENDED_VALUE(NE_XT_CONSTANT, NE_C_QUOTE)
#define NE_LAMBDA_VALUE                 NE_MAKE_EXTENDED_VALUE(NE_XT_CONSTANT, NE_C_LAMBDA)
#define NE_MACROSYM_VALUE               NE_MAKE_EXTENDED_VALUE(NE_XT_CONSTANT, NE_C_MACROSYM)
#define NE_BOOLEAN_VALUE(exp)           NE_MAKE_EXTENDED_VALUE(NE_XT_BOOLEAN, ((exp) ? (NeInt)1 : (NeInt)0))
#define NE_YES_VALUE                    NE_MAKE_EXTENDED_VALUE(NE_XT_BOOLEAN, 1)
#define NE_NO_VALUE                     NE_MAKE_EXTENDED_VALUE(NE_XT_BOOLEAN, 0)
#define NE_BACKQUOTE_VALUE              NE_MAKE_EXTENDED_VALUE(NE_XT_CONSTANT, NE_C_BACKQUOTE)
#define NE_COMMA_VALUE                  NE_MAKE_EXTENDED_VALUE(NE_XT_CONSTANT, NE_C_COMMA)
#define NE_SPLICE_VALUE                 NE_MAKE_EXTENDED_VALUE(NE_XT_CONSTANT, NE_C_SPLICE)
#define NE_COLON_VALUE                  NE_MAKE_EXTENDED_VALUE(NE_XT_CONSTANT, NE_C_COLON)
#define NE_DOT_VALUE                    NE_MAKE_EXTENDED_VALUE(NE_XT_CONSTANT, NE_C_DOT)

// Values of guaranteed bit size - checked by NeOpen()
typedef int16_t NeInt16;
typedef uint8_t NeUInt8;
typedef uint16_t NeUInt16;
typedef uint32_t NeUInt32;

//----------------------------------------------------------------------------------------------------
// Cell type and macros
//----------------------------------------------------------------------------------------------------

// Cell access macros
//
// Cells are the building blocks for most data and code.
//
#define NE_GC_HEADER    NeBits mMarked:1; NeBits mUsed:1; NeBits mType:4;

typedef struct _NeCell
{
    NE_GC_HEADER
    NeValue             mTail;              // IMPORTANT: this must be the first field.  The pool system requires it to be.
    NeValue             mHead;
}
NeCell, *NeCellRef;

#define NE_HEAD(value)      (NE_CAST((value), NeCell)->mHead)
#define NE_TAIL(value)      (NE_CAST((value), NeCell)->mTail)

//----------------------------------------------------------------------------------------------------
// Numbers are represented by a NeNumber structure.  For optimising purposes, numbers can be stored
// within a single NeValue as well if they are small enough.  Otherwise, all numbers are stored in the
// numbers pool.
//----------------------------------------------------------------------------------------------------

typedef enum _NeNumberType
{
    NeNumberType_Integer,
    NeNumberType_Ratio,
    NeNumberType_Float,
}
NeNumberType;

typedef struct _NeNumber
{
    struct {
        NE_GC_HEADER        // Header wrapped in structure to stop mType being merged into the NeBits
    };
    NeNumberType    mNumType;
    union {
        NeFloat         mFloat;
        NeInt           mInteger;
        struct {
            NeInt           mNumerator;
            NeInt           mDenominator;
        };
    };
}
NeNumber, *NeNumberRef;

//----------------------------------------------------------------------------------------------------
// Memory management
// All memory allocations need to be 16-bit aligned (i.e. the lower 4 bits of the pointer are all
// zero).  These functions will assert that this is the case.
//----------------------------------------------------------------------------------------------------

void* _NeAlloc(Nerd N, NeInt size, NeMemoryType memoryType, const char* file, int line);
void* _NeRealloc(Nerd N, void* address, NeInt oldSize, NeInt newSize, NeMemoryType memoryType, const char* file, int line);
void _NeFree(Nerd N, void* address, NeInt oldSize, NeMemoryType memoryType, const char* file, int line);

// Macros that should be used instead of calling the functions above directly
#define NE_ALLOC(pointerType, N, size, type) (pointerType *)_NeAlloc((N), (size), (type), __FILE__, __LINE__)
#define NE_REALLOC(pointerType, N, address, oldSize, newSize, type) (pointerType *)_NeRealloc((N), (address), (oldSize), (newSize), (type), __FILE__, __LINE__)
#define NE_FREE(N, address, oldSize, type) _NeFree((N), (address), (oldSize), (type), __FILE__, __LINE__)

//----------------------------------------------------------------------------------------------------
// Buffer management
// A NeBuffer is an expandable buffer that can expand its size.  This is required a lot for building
// temporary buffers and for generation of byte-code, for example.
//
// NOTE: Some function have the NeBufferRef* type.  This means that the memory where the buffer
// is stored could change when the function exists.  So a pointer to a l-value is required.
//----------------------------------------------------------------------------------------------------

typedef struct _NeBuffer NeBuffer, *NeBufferRef;

// Create a new expandable buffer.
NeBufferRef NeCreateBuffer(Nerd N, NeInt startSize);

// Destroy a buffer.
void NeDestroyBuffer(NeBufferRef buffer);

// Write to the buffer at it's cursor.
NeBool NeBufferAddFormat(NeBufferRef* buffer, const char* format, ...);
NeBool NeBufferAddFormatArgs(NeBufferRef* buffer, const char* format, va_list args);

// Allocate some space on the buffer and return the address.  It will be 16-bytes aligned.
NeBool NeBufferAlloc(NeBufferRef* buffer, NeInt size, NE_OUT void** address);

// Add a memory block to our buffer.
void* NeBufferAdd(NeBufferRef* buffer, const void* memBlock, NeInt size);

// Set a block of memory within the buffer at a position from the beginning of the non-committed data.
void NeBufferSet(NeBufferRef buffer, NeInt position, const void* memBlock, NeInt size);

// Get a block of memory within a buffer according to position from the beginning of the non-committed 
// data.
void* NeBufferGet(NeBufferRef buffer, NeInt position, NeInt size);

// Reallocate the buffer memory to be the smallest it can be to conserve memory.
void NeBufferShrink(NeBufferRef* buffer);

// Return the length of the buffer written so far.
NeInt NeBufferLength(NeBufferRef buffer);

// This will store the data written to the buffer away for later editing.  It will only save the data
// written since the last commit.  Use RestoreBuffer() to get it back.  You can nest the calls as long
// as you match each NeBufferSave with each NeBufferRestore.
NeBool NeBufferSave(NeBufferRef* buffer);

// Restore previously saved contents and overwrite any uncommited data.
void NeBufferRestore(NeBufferRef buffer);

// Commit the data that's been already written to.
void NeBufferCommit(NeBufferRef buffer);

//----------------------------------------------------------------------------------------------------
// Output
//----------------------------------------------------------------------------------------------------

// Uses the user-defined callback to output a message.
//
void NeOut(Nerd N, const char* format, ...);

// Same as TiOut but uses va_list instead.
//
void NeOutArgs(Nerd N, const char* format, va_list args);

//----------------------------------------------------------------------------------------------------
// Error handling
//----------------------------------------------------------------------------------------------------

// Output a message, set the virtual machine into error mode and return NE_NO (most common return
// value to return in calling functions).
//
NeBool NeError(Nerd N, const char* format, ...);

//
// The following functions are for common errors.  You are encouraged to use them to ensure
// consistency with the error messages.
//

// Show an out of memory error.  
//
NeBool NeOutOfMemory(Nerd N);

//----------------------------------------------------------------------------------------------------
// Stack control
//----------------------------------------------------------------------------------------------------

// Push a value on to the stack
//
NeBool NePushValue(Nerd N, NeValue value);

//----------------------------------------------------------------------------------------------------
// Utilities
//----------------------------------------------------------------------------------------------------

// Compare two values and return NE_YES if they are exactly the same.  Will return NE_NO if the types
// cannot be compared.
//
NeBool NeEqual(NeValue v1, NeValue v2);

// Compare two values and returns NE_YES in result if v1 < v2, or v1 comes before v2 (as in strings).
// Will return NE_NO if there is an error in comparison (for example, comparing incompatible types).
// Will call NeError() if an error occurs.
//
NeBool NeLessThan(Nerd N, NeValue v1, NeValue v2, NE_OUT NeBool* result);
    
// Return the true-ness of a value.  A value is true if it isn't 'no' or nil.
//
NeBool NeIsTrue(NeValue v);

// Assign a value to a source.  The types are any valid types that can be used in a key/value
// expression for assignment.  There are two types of assignments: functional and non-functional.
// Functional assignments can only be made to new variables of the same scope.  Non-functional
// assignments can be made to new or current variables in any scope.  Non-functional assignments
// can only be made when evaluating in non-functional mode.
//
NeBool NeAssign(Nerd N, NeValue source, NeValue value, NeValue env, NeBool functional);

//----------------------------------------------------------------------------------------------------
// Cells
//----------------------------------------------------------------------------------------------------

// Recycle a cons-cell that you don't need any more.
//
void NeRecycleCons(Nerd N, NeValue v);

// Create a list with a given number of elements
//
NeValue NeCreateList(Nerd N, NeInt numElems);

//----------------------------------------------------------------------------------------------------
// String management
//----------------------------------------------------------------------------------------------------

// Get the string part of the string or symbol.  Will return "" if v is not a string or symbol.
//
NeString NeGetString(NeValue v);

// Get the string or symbol's length.  Will return 0 if v is not a string or symbol.
//
NeInt NeGetStringLength(Nerd N, NeValue v);

// Compare a string, symbol or keyword value with a C string.  Keywords will not compare the initial
// colons.
NeInt NeCompareString(Nerd N, NeValue strValue, const char* cString, NeInt size);

//----------------------------------------------------------------------------------------------------
// Symbol management
//----------------------------------------------------------------------------------------------------

// Creates a symbol, if new, or just returns a previous reference if created before.
//
NeValue NeCreateSymbol(Nerd N, const char* str, NeInt size);

// Creates a keyword, if new, or just returns a previous reference if created before.
//
NeValue NeCreateKeyword(Nerd N, const char* str, NeInt size);

// Return the symbol's name
//
NeString NeGetSymbolName(NeValue symbol);

//----------------------------------------------------------------------------------------------------
// Table management
//----------------------------------------------------------------------------------------------------

// Clone a table from another.  If you want to create a new table, just pass 0 as the parent.
//
NeValue NeCloneTable(Nerd N, NeValue parentTable);

// Get the address of the value stored in a table of a given key.  If the key does not exist,
// 0 is returned.  Set 'deepSearch' to NE_YES to search the parent tables.
//
NeValueRef NeGetTableSlot(Nerd N, NeValue table, NeValue key, NeBool deepSearch);

// Get the address of the value stored in a table of a given key, creating it if necessary.  This
// function does not search parent tables.
//
NeValueRef NeNewTableSlot(Nerd N, NeValue table, NeValue key);

//----------------------------------------------------------------------------------------------------
// Key/value management
//----------------------------------------------------------------------------------------------------

// Create a key/value pair.  Will return 0 if there is no memory.
//
NeValue NeCreateKeyValue(Nerd N, NeValue key, NeValue value);

// Extract the key from a key/value pair.  If the value is not a key/value, the function will return
// 0.
//
NeValue NeGetKey(NeValue kv);

// Extract the value from a key/value pair.  If the value is not a key/value, the function will
// return 0.
//
NeValue NeGetValue(NeValue kv);

//----------------------------------------------------------------------------------------------------
// Number management
//----------------------------------------------------------------------------------------------------

// Fill a NeNumber structure from a NeValue.
//
NeBool NeGetNumber(NeValue value, NE_OUT NeNumberRef number);

// Create a NeValue from a NeNumber structure.  You can pass in the original NeValue used in a
// NeGetNumber if you'd like so that the system can re-use NeNumber structures in the pool.
//
NeValue NeSetNumber(Nerd N, NeValue origValue, const NeNumberRef number);

// These functions create an NeValue based on a number.
//
NeValue NeCreateInteger(Nerd N, NeInt i);
NeValue NeCreateRatio(Nerd N, NeInt numerator, NeInt denominator);
NeValue NeCreateFloat(Nerd N, NeFloat f);

// These functions convert a TiValue into a number of a specific type.
//
NeInt NeGetInteger(Nerd N, NeValue value);
void NeGetRatio(Nerd N, NeValue value, NE_OUT NeInt* outNumerator, NE_OUT NeInt* outDenominator);
NeFloat NeGetFloat(Nerd N, NeValue value);

// Basic arithmetic
//
NeValue NeAddNumbers(Nerd N, NeValue a, NeValue b);
NeValue NeSubtractNumbers(Nerd N, NeValue a, NeValue b);
NeValue NeMultiplyNumbers(Nerd N, NeValue a, NeValue b);
NeValue NeDivideNumbers(Nerd N, NeValue a, NeValue b);

//----------------------------------------------------------------------------------------------------
// Function management
//----------------------------------------------------------------------------------------------------

// Create a closure with the given arguments, body and environment.
//
NeValue NeCreateClosure(Nerd N, NeValue args, NeValue body, NeValue environment);

//----------------------------------------------------------------------------------------------------
// Object API
//----------------------------------------------------------------------------------------------------

typedef struct _NeClass NeClass;
typedef NeClass* NeClassRef;

typedef struct _NeObject
{
    NeClassRef          mClass;
    struct _NeObject*   mNext;
    NeBits              mMarked : 1;
}
*NeObject, NeObjectHeader;

typedef NeBool          (*NeClassCreateFunc)    (Nerd N, void* newObject, va_list args);
typedef const char*     (*NeClassNameFunc)      ();
typedef void            (*NeClassDeleteFunc)    (Nerd N, void* object);
typedef void            (*NeClassTraceFunc)     (void* object, NeInt colour);
typedef NeBool          (*NeClassStringFunc)    (Nerd N, const void* object, int printMode, 
                                                 NeBufferRef* buffer);
typedef NeBool          (*NeClassApplyFunc)     (Nerd N, NeObject object, NeValue args, NeValue env,
                                                 NE_OUT NeValueRef result);

struct _NeClass
{
    NeClassCreateFunc   mCreateFunc;            // 0 = Uses class size and set everything to 0.
    NeClassNameFunc     mNameFunc;              // The class name
    NeClassDeleteFunc   mDeleteFunc;            // 0 = do nothing when object is deleted.
    NeClassTraceFunc    mTraceFunc;             // 0 = no sub-objects to mark.
    NeClassStringFunc   mStringFunc;            // 0 = "<class-name:address>"
    NeClassApplyFunc    mApplyFunc;             // 0 = invalid procedure.
    NeInt               mSize;                  // Size of an instance.
};

// Create an instance of an object.  Returns 0 if creation fails.  Create function must call
// NeError in this case.
NeValue NeCreateObject(Nerd N, NeClassRef cl, ...);

// Return the name of the object
const char* NeObjectName(NeObject object);

// Apply some arguments to an object
NeBool NeApplyObject(Nerd N, NeObject object, NeValue args, NeValue env, NE_OUT NeValueRef result);

// Return true if object is of a type class
NeBool NeIsObjectOfClass(NeObject object, NeClassRef cl);

// Check that a value is an object of a certain type and produce errors if not.
NeBool NeCheckObjectType(Nerd N, NeValue v, NeClassRef cl);
    
//----------------------------------------------------------------------------------------------------
// Reading
//----------------------------------------------------------------------------------------------------

// Read a string in a generate a sequence
//
NeBool NeRead(Nerd N, const char* source, const char* code, NeInt size, NeValueRef result);
    
//----------------------------------------------------------------------------------------------------
// Compilation
//----------------------------------------------------------------------------------------------------

// Compile a sequence into a block.
//
NeBool NeCompile(Nerd N, NeValue sequence, NeValueRef blockValue);
    
//----------------------------------------------------------------------------------------------------
// Evaluation
//----------------------------------------------------------------------------------------------------

// Return NE_YES if expression was evaluated.  If so, the output parameter 'result' will contain
// the result of the evaluated expression.  0 can be passed as the environment and in which case
// the global environment of the Nerd VM will be used.  If an error occurs, NE_NO will be returned
// and result will be unchanged.
//
NeBool NeEval(Nerd N, NeValue expression, NeValue environment, NE_OUT NeValueRef result);

// Apply an expression to some args in the given environment.
//
NeBool NeApply(Nerd N, NeValue exp, NeValue args, NeValue environment, NE_OUT NeValueRef result);

//----------------------------------------------------------------------------------------------------
// Natives
// Natives are C functions that can be assigned to a Nerd symbol and be called from a Nerd program.
// They act like Nerd macros in that the arguments are not evaluated when the C function is called.
//----------------------------------------------------------------------------------------------------

// Here is the signature that all native functions should have.  The native C function should return
// NE_YES if the function succeeded without an error, or NE_NO if it did.  The output parameter
// 'result' should contain the result of the function.  If the function failed, result should be
// unchanged.
//
typedef NeBool (*NeNativeFunc) (Nerd N, NeValue arguments, NeValue environment, NE_OUT NeValueRef result);

// Register a native function to a symbol name in a particular environment.  Use 0 to register to
// the core environment, which is the base environment to the global one.
//
NeBool NeRegisterNative(Nerd N, const char* nativeName, NeValue environment, NeNativeFunc func);

// A structure used to store native information for the NeRegisterNatives function.
//
typedef struct _NeNativeInfo
{
    const char*     mName;      // Name of symbol to contain native function.
    NeNativeFunc    mFunc;      // C function that will implement the native function.
}
NeNativeInfo, *NeNativeInfoRef;

#define NE_NATIVE(name, func) { name, &func },
#define NE_END_NATIVES { 0, 0 }

// Register a list of native functions.  The array of NeNativeInfos should be terminated with
// a { 0, 0 }.  You can use the NE_END_NATIVES macro for this.
//
// If you provide a non-null 'featureName', the global variable *features* will have a symbol added
// to the head of the list.  This allows programs to query the configuration of the VM.
//
NeBool NeRegisterNatives(Nerd N, NeNativeInfoRef nativeList, NeValue environment, const char* featureName);

//----------------------------------------------------------------------------------------------------
// Native writing utilities
//----------------------------------------------------------------------------------------------------

// These macros fetch the nth value in the list in O(n) time.
//
#define NE_1ST(v)       NE_HEAD(v)
#define NE_2ND(v)       NE_1ST(NE_TAIL(v))
#define NE_3RD(v)       NE_2ND(NE_TAIL(v))
#define NE_4TH(v)       NE_3RD(NE_TAIL(v))
#define NE_5TH(v)       NE_4TH(NE_TAIL(v))
#define NE_6TH(v)       NE_5TH(NE_TAIL(v))
#define NE_7TH(v)       NE_6TH(NE_TAIL(v))
#define NE_8TH(v)       NE_7TH(NE_TAIL(v))
#define NE_9TH(v)       NE_8TH(NE_TAIL(v))
#define NE_10TH(v)       NE_9TH(NE_TAIL(v))

// Returns the type of a value.
//
NeType NeGetType(NeValue v);

// Returns the name of an argument type.
//
NeString NeGetTypeName(NeType t);

// Returns true if the argument is of the correct type.
//
NeBool NeCheckArgType(Nerd N, NeValue arg, NeInt index, NeType expectedArgType);

// Returns NE_YES if the list of arguments has n arguments.  If exactCount is NE_YES, then the list
// must have exactly that number, otherwise at least that number is sufficient for a pass.
//
NeBool NeCheckNumArgs(Nerd N, NeValue args, NeInt count, NeBool exactCount);

// This makes a runtime check to see if the number of arguments is at least n arguments.
//
#define NE_NEED_NUM_ARGS(N, args, n) if (!NeCheckNumArgs((N), (args), (n), NE_NO)) return NE_NO

// This makes a runtime check to see if the number of arguments is exactly n arguments.
//
#define NE_NEED_EXACTLY_NUM_ARGS(N, args, n) if (!NeCheckNumArgs((N), (args), (n), NE_YES)) return NE_NO

// This will evaluate the expression 'exp' in the environment 'env' and put the result in the 
// NeValue variable 'result'.  If it fails, it will exit the function with NE_NO.
//
#define NE_EVAL(N, exp, env, result) if (!NeEval((N), (exp), (env), &result)) return NE_NO

// Check that an argument conforms to a NeType enumeration.  If it fails the check, it will exit
// the function with NE_NO.
//
#define NE_CHECK_ARG_TYPE(N, arg, index, type) if (!NeCheckArgType((N), (arg), (index), (type))) return NE_NO

//----------------------------------------------------------------------------------------------------
// Debug routines
//----------------------------------------------------------------------------------------------------

// Display <name>: <value>.
void NeDebugOutValue(Nerd N, const char* name, NeValue value);

//----------------------------------------------------------------------------------------------------
//----------------------------------------------------------------------------------------------------

#ifdef __cplusplus
}
#endif

#endif // __B1758AE7_BADD_434B_8F5F_AF33A9F49C53_NERD_INT_H
