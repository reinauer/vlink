/* $VER: vlink errors.c V0.18 (31.12.24)
 *
 * This file is part of vlink, a portable linker for multiple
 * object formats.
 * Copyright (c) 1997-2024  Frank Wille
 */


#define ERRORS_C
#include "vlink.h"


/* error flags */
#define EF_NONE 0
#define EF_WARNING 1
#define EF_ERROR 2
#define EF_FATAL 3
#define EF_TYPEMASK 0xff
#define EF_DISABLED (1<<8)


static struct {
  char *txt;
  int flags;
} errors[] = {
  /* startup, command line, !!! are referenced dynamically - don't delete! */
  "",EF_NONE,
  "Out of memory",EF_FATAL,                                         /* 01 */
  "Unrecognized option '%s'",EF_WARNING,
  "Unknown link mode: %s",EF_WARNING,
  "Unknown argument for option -d: %c",EF_WARNING,
  "Option '-%c' requires an argument",EF_FATAL,                     /* 05 */
  "No input files",EF_FATAL,
  "File \"%s\" has a read error",EF_FATAL,
  "Cannot open \"%s\"",EF_FATAL,
  "Invalid target format \"%s\"",EF_FATAL,
  "Directory \"%s\" could not be examined",EF_ERROR,                /* 10 */
  "%s: File format not recognized",EF_FATAL,
  "\"%s\" is already an executable file",EF_WARNING,
  "%s: File format corrupted",EF_FATAL,
  "%s (%s): Illegal relocation type %d at %s+%x",EF_FATAL,
  "%s: Unexpected end of section %s in %s",EF_FATAL,                /* 15 */
  "%s: %s appeared twice in %s",EF_FATAL,
  "%s: Misplaced %s in %s",EF_FATAL,
  "%s: Symbol definition %s in %s uses unsupported type %d",EF_FATAL,
  "%s: Global symbol %s from %s is already defined in %s",EF_ERROR,
  "%s: Unresolved reference to symbol %s in %s uses "               /* 20 */
    "unsupported type %d",EF_FATAL,
  "%s (%s+%#lx): Reference to undefined symbol %s",EF_ERROR,
/* FIXME! "Attributes of section %s were changed from %s in %s to %s in %s",EF_WARNING,*/
  "Attributes of section %s were changed from %s to %s in %s",EF_WARNING,
  "%s: %s expected",EF_FATAL,
  "%s (%s+%#lx): Illegal relative reference to %s+%#llx",EF_ERROR,  /* 24 */
  "%s (%s+%#lx): %dbit %s reference to %s%c%#llx (value to write: %s%#llx) "
    "out of range",EF_ERROR,
  "%s (%s+%#lx): Referenced absolute symbol %s=%#llx%c%#llx "       /* !!! */
    "(value to write: %s%#llx) doesn't fit into %d bits",EF_ERROR,
  "%s (%s+%#lx): Illegal relative reference to symbol %s",EF_ERROR,
  "%s (%s+%#lx): Relative reference to relocatable symbol %s=%#llx%c%#llx "
    "(value to write: %s%#llx) doesn't fit into %d bits",EF_ERROR,  /* !!! */
  "Can't create output file %s",EF_ERROR,
  "Target file format doesn't support shared objects",EF_ERROR,     /* 30 */
  "Error while writing to %s",EF_FATAL,
  "Target %s: Unsupported relocation type %s (offset=%d, size=%d, "
    "mask=%llx) at %s+%#x",EF_ERROR,
  "Target %s: Can't reproduce symbol %s, which is a %s%s%s",EF_ERROR,
  "Option '%s' requires an argument",EF_FATAL,
  "%s (%s+%#lx): from %s (%s+%#x): Calculated value %s%#llx doesn't fit into "
    "relocation type %s (offset=%d, size=%d, mask=%#llx)",EF_ERROR,/* 35 */
  "%s (%s+%#lx): Base relative reference to relocatable symbol "   /* !!! */
    "%s=%#llx%c%#llx (value to write: %s%#llx) doesn't fit into %d bits",EF_ERROR,
  "%s: Malformatted archive member %s",EF_FATAL,
  "%s: Empty archive ignored",EF_WARNING,
  "%s: %s doesn't support shared objects in library archives",EF_WARNING,
  "%s: %s doesn't support executables in library archives",EF_WARNING,/* 40 */
  "%s (%s): Illegal format / file corrupted",EF_FATAL,
  "%s: Consistency check for archive member %s failed",EF_FATAL,
  "%s: Invalid ELF section header index (%d) in %s",EF_FATAL,
  "%s: ELF section header #%d has illegal offset in %s",EF_FATAL,
  "%s: ELF section header string table has illegal type in %s",     /* 45 */
    EF_ERROR,
  "%s: ELF section header string table has illegal offset in %s",EF_ERROR,
  "%s: ELF program header table in %s was ignored",EF_WARNING,
  "%s: ELF section header type %d in %s is not needed in "
    "relocatable objects",EF_WARNING,
  "%s: Illegal section offset for %s in %s",EF_FATAL,
  "%s: ELF %s table has illegal type in %s",EF_FATAL,               /* 50 */
  "%s: ELF %s table has illegal offset in %s",EF_FATAL,
  "%s: %s in %s defines relocations relative to a non-existing "
    "section with index=%d",EF_FATAL,
  "%s: Symbol %s, defined in %s, has an invalid reference to "
    "a non-existing section with index=%d",EF_FATAL,
  "%s: Illegal symbol type %d for %s in %s",EF_ERROR,
  "%s: Symbol %s has illegal binding type %d in %s",EF_ERROR,       /* 55 */
  "%s: Symbol %s in %s is multiply defined",EF_ERROR,
  "%s: Merging a code section with name \"__MERGED\"",EF_WARNING,
  "Relative references between %s section \"%s\" and %s section "
    "\"%s\" (%s) force a combination of the two",EF_WARNING,
  "Can't define %s as ctors/dtors label. Symbol already exists",EF_ERROR,
  "%s: ELF section header type %d in %s is not needed in "          /* 60 */
    "shared objects",EF_WARNING,
  "%s: Endianness differs from previous objects",EF_FATAL,
  "Target file format doesn't support relocatable objects",EF_ERROR,
  "Predefined limits of destination memory region %s "
    "for output section %s were exceeded (%#llx)",EF_FATAL,
  "Section %s(%s) was not recognized by target linker script",EF_WARNING,
  "%s line %d: Unknown keyword <%s> ignored",EF_ERROR,              /* 65 */
  "%s line %d: '%c' expected",EF_ERROR,
  "%s line %d: Absolute number expected",EF_ERROR,
  "%s line %d: Keyword <%s> expected",EF_ERROR,
  "%s line %d: GNU command <%s> ignored",EF_WARNING,
  "%s line %d: Unknown memory region <%s>",EF_ERROR,                /* 70 */
  "%s line %d: Multiple constructor types in output file",EF_ERROR,
  "UNUSED! %s line %d: Unknown keyword <%s>",EF_ERROR,
  "%s line %d: Assertion failed: %s",EF_FATAL,
  "%s line %d: SECTIONS block defined twice",EF_ERROR,
  "%s line %d: Segment %s is closed and can't be reused",EF_ERROR,  /* 75 */
  "%s line %d: Address overrides specified %cMA memory region",EF_WARNING,
  "%s line %d: Segment %s must include both, FILEHDR and PHDR",EF_ERROR,
  "%s line %d: Missing argument",EF_ERROR,
  "%s line %d: Undefined section: <%s>",EF_ERROR,
  "%s line %d: Section %s was assigned to more than one PT_LOAD "   /* 80 */
    "segment",EF_ERROR,
  "Multiple use of section <%s> in linker script",EF_FATAL,
  "Intermediate uninitialized sections in ELF segment <%s> (first=<%s>, "
    "last=<%s>) will be turned into initialized",EF_WARNING,
  "Section <%s> (%#llx-%#llx) conflicts with ELF segment <%s> "
    "(currently: %#llx-%#llx)",EF_ERROR,
  "%s: QMAGIC is deprecated and will no longer be supported",EF_ERROR,
  "%s: a.out %s table has illegal offset or size in %s",EF_FATAL,   /* 85 */
  "%s: a.out %s table size in <%s> is not a multiple of %d",EF_ERROR,
  "%s: a.out symbol name has illegal offset %ld in %s",EF_FATAL,
  "%s: a.out symbol %s has illegal binding type %d in %s",EF_ERROR,
  "%s: a.out relocations without an appropriate section in %s",EF_FATAL,
  "%s: illegal a.out relocation in section %s of %s at offset "     /* 90 */
    "%#08lx: <pcrel=%d len=%d ext=%d brel=%d jmptab=%d rel=%d copy=%d>",EF_ERROR,
  "%s: illegal a.out external reference to symbol %s in %s, which is no "
    "external symbol",EF_ERROR,
  "%s: illegal nlist type %lu in a.out relocation in section %s of %s "
    "at offset %#08lx",EF_ERROR,
  "Target %s: Common symbol %s is unreferenced and will disappear",EF_WARNING,
  "Target file format doesn't support executable files",EF_ERROR,
  "%s: a.out relocation <pcrel=%d len=%d ext=%d brel=%d jmptab=%d " /* 95 */
    "rel=%d copy=%d> is treated as a normal relocation in "
    "section %s of %s at offset %#08lx",EF_WARNING,
  "%s: size %d for a.out symbol %s in %s was ignored",EF_WARNING,
  "Target %s: %s section must not be absent for a valid executable file",
    EF_FATAL,
  "Target %s: Section %s is overlapping %s",EF_FATAL,
  "%s line %d: Illegal PHDR type: <%s>",EF_ERROR,
  "%s line %d: <%s> behind SECTIONS ignored",EF_ERROR,              /* 100 */
  "%s line %d: Address symbol '.' invalid outside SECTIONS block",EF_ERROR,
  "%s line %d: Reference to non-absolute symbol <%s> outside SECTIONS",
    EF_ERROR,
  "%s line %d: Division by zero",EF_ERROR,
  "%s line %d: Unknown symbol or function: <%s>",EF_ERROR,
  "%s line %d: No function-calls allowed here",EF_ERROR,            /* 105 */
  "%s line %d: Symbol <%s> is not yet assigned",EF_ERROR,
  "%s line %d: Command <%s> not allowed outside SECTIONS block",EF_ERROR,
  "%s line %d: Address symbol '.' cannot be provided",EF_ERROR,
  "%s line %d: Symbol <%s> already defined",EF_ERROR,
  "%s line %d: Only absolute expressions may be assigned outside "  /* 110 */
    "SECTIONS block",EF_ERROR,
  "%s line %d: Unknown PHDR: <%s>",EF_ERROR,
  "%s (%s+%#x): Cannot resolve reference to %s, because section "
    "%s was not recognized by the linker script",EF_ERROR,
  "%s (%s): %d bits per byte are not supported",EF_FATAL,
  "%s (%s): %d bytes per target-address are not supported",EF_FATAL,
  "%s (%s): Relocation type %d (offset=%lld, bit-offset=%d "        /* 115 */
    "bit-size=%d mask=%#llx referring to symbol <%s> (type %d) is "
    "not supported",EF_ERROR,
  "%s (%s): Symbol type %d for <%s> in section %s is not supported",EF_FATAL,
  "%s (%s+%#lx): Cannot resolve %s reference to %s, because host "
    "section %s is invalid",EF_ERROR,
  "%s: Malformatted ELF %s section in %s",EF_FATAL,
  "%s: Ignoring junk at end of ELF %s section in %s",EF_WARNING,
  "%s (%s+%#lx): Relocation based on missing %s section",EF_ERROR,  /* 120 */
  "%s (%s+%#lx): Base-relative reference to code section",EF_WARNING,
  "Relocation table format not supported by selected output format - "
    "reverting to %s's standard",EF_WARNING,
  "Unknown relocation table format '%s' ignored",EF_WARNING,
  "Target %s: multiple small-data sections not allowed",EF_ERROR,
  ".ctors/.dtors spread over multiple sections",EF_ERROR,           /* 125 */
  "Dynamic symbol reference not supported by target %s",EF_ERROR,
  "%s: ELF symbol name has illegal offset %#lx in %s",EF_FATAL,
  "%s: Unknown endianness defaults to %s-endian. "
    "Consider using -EB/-EL",EF_WARNING,
  "Resetting the same attribute for section %s",EF_WARNING,
  "Bad assignment after option '%s'",EF_FATAL,                      /* 130 */
  "Need a valid symbolic entry when using -gc-all",EF_FATAL,
  "Executable code section in first object required when using -gc-all",
    EF_FATAL,
  "Unsupported absolute relocation (offs=%lld pos=%d siz=%d msk=%#llx) "
    "in resident data section",EF_ERROR,
  "%s (%s+%#lx): Absolute reference to resident data section (%s)",EF_WARNING,
  "%s line %d: Undefined memory region: <%s>",EF_ERROR,             /* 135 */
  "Executable section <%s> in data segment not allowed",EF_ERROR,
  "Not enough space for the module header (%u of %u)",EF_ERROR,
  "Target %s: multiple %s sections not allowed:<%s> and <%s>",EF_ERROR,
  "%s: symbol index %u is out of range",EF_FATAL,
  "%s: %s is chained",EF_WARNING,                                   /* 140 */
  "Maximum file option size exceeded (%u)",EF_ERROR,
  "%s: Ignoring weak symbol %s",EF_WARNING,
  "%s: Unexpected relocations for section with index=%d",EF_FATAL,
  "Bad error number: %d",EF_FATAL,
  "Error number %d is not a warning",EF_FATAL,                      /* 145 */
  "%s (%s): alternating bits per byte in object files (from %d to %d)",EF_FATAL,
  "%s (%s): alternating bytes per address in object files (from %d to %d)",EF_FATAL,
  "Endianness is unknown. Default to host endianness",EF_WARNING,
  "Mismatching target address sizes in input/output formats",EF_FATAL,
  "%s: Hunk format corrupted: DEBUG hunk used like a section "      /* 150 */
    "with name \"%s\" in unit \"%s\". Trying to ignore",EF_WARNING,
  "%s: Duplicate con/destructor name %s definition ignored",EF_WARNING,
  "Warnings treated as errors",EF_ERROR,
  "%s: Kickstart 1.x cannot initialize bss sections >256k to zero",EF_WARNING,
  "No CPU defined for VOBJ output. Consider using -vobjcpu",EF_WARNING,
  "Indirect symbol %s -> %s not allowed in %s",EF_FATAL,            /* 155 */
  "Alternating CPU definitions from VOBJ input. Keeping the first one (%s)",
  EF_WARNING,
  "Bad symbol file format",EF_ERROR,
  "No space to fit section %s(%s) with end address %#llx into memory region",
  EF_ERROR,
  "%s (%s+%#lx): Reference from overlayed section %s to %s (symbol %s) "
    "is not allowed (NOCROSSREFS)",EF_ERROR,
  "Target %s: Too many symbols for selected output format",EF_FATAL,/* 160 */
  "Invalid version. Assuming V%d.%d",EF_WARNING,
  "Target %s: Output section %s exceeds maximum size %lu",EF_WARNING,
  "%s: No valid bank size defined",EF_ERROR,
  "%s (%s): Section exceeds maximum bank size of %lu",EF_ERROR,
  "%s (%s+%#lx): output section %s (referenced symbol: %s) has no " /* 165 */
    "memory id defined, assuming zero",EF_WARNING,
  "%s (%s+%#lx): reference to memory id of relocatable symbol "     /* !!! */
    "%s=%#llx%c%#llx (value to write: %s%#llx) doesn't fit into %d bits",EF_ERROR,
  "%s line %d: Maximum of %d sorting levels exceeded",EF_ERROR,
  "%s line %d: Multiple EXCLUDE_%s commands in one pattern",EF_ERROR,
};


void disable_warning(int errn)
{
  if (errn<0 || errn>=sizeof(errors)/sizeof(errors[0])) {
    error(144,errn);
    return;
  }
  if ((errors[errn].flags & EF_TYPEMASK) == EF_WARNING)
    errors[errn].flags |= EF_DISABLED;
  else
    error(145,errn);
}


void ierror(char *errtxt,...)
/* display internal error and quit */
{
  struct GlobalVars *gv = &gvars;
  va_list vl;

  fprintf(stderr,"\nINTERNAL ERROR: ");
  va_start(vl,errtxt);
  vfprintf(stderr,errtxt,vl);
  va_end(vl);
  fprintf(stderr,".\nAborting.\n");
  gv->returncode = EXIT_FAILURE;
  cleanup(gv);
}


void error(int errn,...)
/* prints errors and warnings */
{
  struct GlobalVars *gv = &gvars;
  va_list vl;
  char *errtype;
  int flags = errors[errn].flags & EF_TYPEMASK;

  if ((flags == EF_WARNING) &&
      (gv->dontwarn || (errors[errn].flags & EF_DISABLED)))
    return;
  switch(flags) {
    case EF_WARNING:
      ++gv->warncnt;
      errtype = "Warning";
      break;
    case EF_ERROR:
      gv->returncode = EXIT_FAILURE;
      errtype = "Error";
      break;
    case EF_FATAL:
      gv->returncode = EXIT_FAILURE;
      errtype = "Fatal error";
      break;
    default:
      ierror("Illegal error type %d",flags);
      gv->returncode = EXIT_FAILURE;
      errtype = "";
      break;
  }

  /* print error message */
  fprintf(stderr,"%s %d: ",errtype,errn);
  va_start(vl,errn);
  vfprintf(stderr,errors[errn].txt,vl);
  va_end(vl);
  fprintf(stderr,".\n");

  switch(flags) {
    case EF_ERROR:
      /* check if maximum number of errors reached */
      if (++gv->errcnt >= gv->maxerrors) {
        gv->errcnt = 0;
        fprintf(stdout,"Do you want to continue (y/n) ? ");
        fflush(stdin);
        if (toupper((unsigned char)getchar()) == 'N')
          cleanup(gv);
      }
      /* avoid writing of output file in error case */
      gv->errflag = TRUE;
      break;
    case EF_FATAL:
      fprintf(stderr,"Aborting.\n");  /* fatal error aborts the linker */
      cleanup(gv);
      break;
  }
}
