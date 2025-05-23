#ifndef CHOCOPY_LLVM_BASIC_LLVM_H
#define CHOCOPY_LLVM_BASIC_LLVM_H

// Do not proliferate #includes here, require clients to #include their
// dependencies.
// Casting.h has complex templates that cannot be easily forward declared.
#include "llvm/Support/Casting.h"
// Add this header as a workaround to prevent `too few template arguments for
// class template 'SmallVector'` building error with build compilers like XL.
#include "llvm/ADT/SmallVector.h"

namespace llvm {
// ADT's.
class StringRef;
class Twine;
class VersionTuple;
template <typename T> class ArrayRef;
template <typename T> class MutableArrayRef;
template <typename T> class OwningArrayRef;
template <unsigned InternalLen> class SmallString;
template <typename T, unsigned N> class SmallVector;
template <typename T> class SmallVectorImpl;
template <class T> class Expected;

template <typename T> struct SaveAndRestore;

// Reference counting.
template <typename T> class IntrusiveRefCntPtr;
template <typename T> struct IntrusiveRefCntPtrInfo;
template <class Derived> class RefCountedBase;

class raw_ostream;
class raw_pwrite_stream;
// TODO: DenseMap, ...
} // namespace llvm

namespace chocopy {
// Casting operators.
using llvm::cast;
using llvm::cast_if_present;
using llvm::cast_or_null;
using llvm::dyn_cast;
using llvm::dyn_cast_if_present;
using llvm::dyn_cast_or_null;
using llvm::isa;
using llvm::isa_and_nonnull;
using llvm::isa_and_present;

// ADT's.
using llvm::ArrayRef;
using llvm::MutableArrayRef;
using llvm::OwningArrayRef;
using llvm::SaveAndRestore;
using llvm::SmallString;
using llvm::SmallVector;
using llvm::SmallVectorImpl;
using llvm::StringRef;
using llvm::Twine;
using llvm::VersionTuple;

// Error handling.
using llvm::Expected;

// Reference counting.
using llvm::IntrusiveRefCntPtr;
using llvm::IntrusiveRefCntPtrInfo;
using llvm::RefCountedBase;

using llvm::raw_ostream;
using llvm::raw_pwrite_stream;
} // namespace chocopy

#endif // CHOCOPY_LLVM_BASIC_LLVM_H
