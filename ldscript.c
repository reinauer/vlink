/* $VER: vlink ldscript.c V0.18 (31.12.24)
 *
 * This file is part of vlink, a portable linker for multiple
 * object formats.
 * Copyright (c) 1997-2024  Frank Wille
 */


#define LDSCRIPT_C
#include "vlink.h"
#include "elfcommon.h"
#include "ldscript.h"

#define DUMMY_SEC_FROM_PATTERN 1


static const char *scriptbase;
static const char *scriptname;
static struct ObjectUnit *script_obj;
static struct MemoryDescr *memory_blocks;
static struct MemoryDescr *defmem,*atdefmem,*vdefmem,*ldefmem;
static const char *defmemname = "default";
static bool preparse;     /* no sym-assignments in SECTIONS when true */
static int level;         /* 0=outside SECTIONS,1=inside,2=section def. */
static struct LinkedSection *current_ls; /* current section in work */
static const char *new_ls_name;          /* newly defined sect. name (pass 1) */

/* BYTE, SHORT, LONG, etc. data commands */
static int datasize;  /* datasize != 0 enables data command */
static lword dataval;

/* for 2nd pass over the SECTIONS block during linking: */
static char *secblkbase;
static int secblkline;

/* Default segment names (including a blank to prevent redefinitions) */
static const char *defhdr = " headers";
static const char *defint = " interp";
static const char *defload =" load";
static const char *deftxt = " text";
static const char *defdat = " data";
static const char *defdyn = " dynamic";
static const char *defnot = " note";


/* Linker script functions: */
static int sf_addr(struct GlobalVars *,lword,lword *);
static int sf_align(struct GlobalVars *,lword,lword *);
static int sf_length(struct GlobalVars *,lword,lword *);
static int sf_loadaddr(struct GlobalVars *,lword,lword *);
static int sf_max(struct GlobalVars *,lword,lword *);
static int sf_min(struct GlobalVars *,lword,lword *);
static int sf_origin(struct GlobalVars *,lword,lword *);
static int sf_sizeof(struct GlobalVars *,lword,lword *);
static int sf_sizeofheaders(struct GlobalVars *,lword,lword *);

struct ScriptFunc ldFunctions[] = {
  { "ADDR",sf_addr },
  { "ALIGN",sf_align },
  { "LENGTH",sf_length },
  { "LOADADDR",sf_loadaddr },
  { "MAX",sf_max },
  { "MIN",sf_min },
  { "ORIGIN",sf_origin },
  { "SIZEOF",sf_sizeof },
  { "SIZEOF_HEADERS",sf_sizeofheaders },
  { NULL,NULL }
};


/* Linker script commands: */
static void sc_ctors_gnu(struct GlobalVars *);
static void sc_ctors_vbcc(struct GlobalVars *);
static void sc_ctors_vbcc_elf(struct GlobalVars *);
static void sc_assert(struct GlobalVars *);
static void sc_entry(struct GlobalVars *);
static void sc_extern(struct GlobalVars *);
static void sc_fill8(struct GlobalVars *);
static void sc_fill16(struct GlobalVars *);
static void sc_input(struct GlobalVars *);
static void sc_provide(struct GlobalVars *);
static void sc_searchdir(struct GlobalVars *);
static void sc_byte(struct GlobalVars *);
static void sc_short(struct GlobalVars *);
static void sc_long(struct GlobalVars *);
static void sc_quad(struct GlobalVars *);
static void sc_reserve(struct GlobalVars *);

struct ScriptCmd ldCommands[] = {
  { "ASSERT",SCMDF_PAREN|SCMDF_GLOBAL,sc_assert },
  { "BYTE",SCMDF_PAREN|SCMDF_SEMIC|SCMDF_SECDEF,sc_byte },
  { "CONSTRUCTORS",SCMDF_GLOBAL,sc_ctors_gnu },
  { "ENTRY",SCMDF_PAREN|SCMDF_GLOBAL,sc_entry },
  { "EXTERN",SCMDF_PAREN|SCMDF_GLOBAL,sc_extern },
  { "FILL8",SCMDF_PAREN|SCMDF_GLOBAL,sc_fill8 },
  { "FILL16",SCMDF_PAREN|SCMDF_GLOBAL,sc_fill16 },
  { "GROUP",SCMDF_PAREN|SCMDF_GLOBAL,sc_input },
  { "INPUT",SCMDF_PAREN|SCMDF_GLOBAL,sc_input },
  { "LONG",SCMDF_PAREN|SCMDF_SEMIC|SCMDF_SECDEF,sc_long },
  { "OUTPUT_ARCH",SCMDF_PAREN|SCMDF_GLOBAL,NULL },
  { "OUTPUT_FORMAT",SCMDF_PAREN|SCMDF_GLOBAL,NULL },
  { "PROVIDE",SCMDF_PAREN|SCMDF_SEMIC|SCMDF_GLOBAL,sc_provide },
  { "QUAD",SCMDF_PAREN|SCMDF_SEMIC|SCMDF_SECDEF,sc_quad },
  { "SEARCH_DIR",SCMDF_PAREN|SCMDF_GLOBAL,sc_searchdir },
  { "SHORT",SCMDF_PAREN|SCMDF_SEMIC|SCMDF_SECDEF,sc_short },
  { "SQUAD",SCMDF_PAREN|SCMDF_SEMIC|SCMDF_SECDEF,sc_quad },
  { "RESERVE",SCMDF_PAREN|SCMDF_SEMIC|SCMDF_SECDEF,sc_reserve },
  { "VBCC_CONSTRUCTORS",SCMDF_GLOBAL,sc_ctors_vbcc },
  { "VBCC_CONSTRUCTORS_ELF",SCMDF_GLOBAL,sc_ctors_vbcc_elf },
  { NULL,0,NULL }
};



static char *getword(void)
{
  return getarg(1+32);
}


static char *getalnum(void)
{
  return getarg(1+2);
}


static char *getpattern(void)
{
  return getarg(1+4+32);
}


static char *getfilename(void)
{
  if (getchr() == '\"') {
    back(1);
    return getquoted();
  }

  back(1);
  return getarg(27);
}


#if 0
/* Not used at the moment!
   When this function is reused, the 'provided' case needs to be implemented!
*/
static int parse_assignment_expr(lword caddr,lword *result,int provided)
{
  int abs;

  abs = parse_expr(caddr,result);
  if (provided) {
    ...
  }
  if (getchr() != ';')
    error(66,scriptname,getlineno(),';');  /* ';' expected */
  else
    back(1);
  return abs;
}
#endif


static bool startofblock(char c)
{
  if (getchr() != c) {
    error(66,scriptname,getlineno(),c);
    back(1);
    return FALSE;
  }
  return TRUE;
}


static bool endofblock(char c1,char c2)
{
  if (getchr() != c2) {
    error(66,scriptname,getlineno(),c2);
    skipblock(1,c1,c2);
    return FALSE;
  }
  return TRUE;
}


static lword term_absexp(void)
{
  lword val = 0;

  if (startofblock('(')) {
    if (!parse_expr(-1,&val))
      error(67,scriptname,getlineno());  /* Absolute number expected */
    endofblock('(',')');
  }
  return val;
}


static void term_two_exp(lword caddr,
                         int *abs1,lword *val1,int *abs2,lword *val2)
{
  *abs1 = *abs2 = 1;
  *val1 = *val2 = 0;

  if (startofblock('(')) {
    *abs1 = parse_expr(caddr,val1);
    skip();
    if (getchr() == ',') {
      *abs2 = parse_expr(caddr,val2);
      endofblock('(',')');
    }
    else {
      error(66,scriptname,getlineno(),',');  /* ',' expected */
      skipblock(1,'(',')');
    }
  }
}


static struct Phdr *add_phdr(struct GlobalVars *gv,const char *name,
                             uint32_t type,uint16_t flags,lword addr,
                             struct MemoryDescr *vmreg,struct MemoryDescr *lmreg)
{
  struct Phdr *p,*new;

  if (type == PT_PHDR) {
    flags |= PHDR_USED;
  }
  else if ((flags&(PHDR_FILEHDR|PHDR_PHDRS))==PHDR_FILEHDR ||
           (flags&(PHDR_FILEHDR|PHDR_PHDRS))==PHDR_PHDRS) {
    /* must include both: FILEHDR and PHDR */
    error(77,scriptname,getlineno(),name);
    return NULL;
  }

  new = alloczero(sizeof(struct Phdr));
  new->name = allocstring(name);
  new->type = type;
  new->flags = flags;
  new->start = addr;
  new->start_vma = ADDR_NONE;
  new->vmregion = vmreg;
  new->lmregion = lmreg;

  if (type == PT_LOAD) {
    if (!gv->no_page_align)
      new->alignment = shiftcnt(fff[gv->dest_format]->page_size);
  }
  else if (type == PT_PHDR)
    new->alignment = 2;

  if (p = gv->phdrlist) {
    while (p->next) {
      if (type==PT_PHDR && p->type==PT_PHDR) {
        free(new);
        return NULL;  /* reject double PHDR segment */
      }
      p = p->next;
    }
    p->next = new;
  }
  else
    gv->phdrlist = new;

  return new;
}


static struct Phdr *find_phdr(struct GlobalVars *gv,const char *name,
                              struct MemoryDescr *vmreg,
                              struct MemoryDescr *lmreg)
{
  struct Phdr *p;

  if (p = gv->phdrlist) {
    do {
      if (!strcmp(p->name,name) && p->vmregion==vmreg && p->lmregion==lmreg)
        break;
    }
    while (p = p->next);
  }
  return p;
}


static void use_segment(struct GlobalVars *gv,const char *name,uint32_t type,
                        struct MemoryDescr *vmreg,struct MemoryDescr *lmreg)
{
  struct Phdr *p;

  if (p = find_phdr(gv,name,vmreg,lmreg)) {
    if (p->type == PT_NULL)
      return;
  }
  else {
    if (!(p = add_phdr(gv,name,type,0,ADDR_NONE,vmreg,lmreg)))
      return;
  }
  p->flags |= PHDR_USED;
}


static uint16_t guess_special_segment(const char *secname,const char **segname)
{
  uint16_t type;

  if (!strncmp(secname,".interp",7)) {
    *segname = defint;
    type = PT_INTERP;
  }
  else if (!strncmp(secname,".dynamic",8)) {
    *segname = defdyn;
    type = PT_DYNAMIC;
  }
  else if (!strncmp(secname,".note",5)) {
    *segname = defnot;
    type = PT_NOTE;
  }
  else {
    *segname = NULL;
    type = 0;
  }

  return type;
}


static void scriptsymbol(struct GlobalVars *gv,char *name,int try,
                         lword val,uint8_t type,uint8_t flags)
{
  struct Symbol *sym;
  struct Symbol **chain = &script_obj->objsyms[elf_hash(name)%OBJSYMHTABSIZE];

  while (sym = *chain) {
    if (!strcmp(name,sym->name)) {
      if (!try)
        error(109,scriptname,getlineno(),name);  /* already defined */
      return;
    }
    chain = &sym->obj_chain;
  }
  sym = alloczero(sizeof(struct Symbol));
  if (check_protection(gv,name))
    flags |= SYMF_PROTECTED;

  sym->name = allocstring(name);
  sym->value = val;
  sym->type = type;
  sym->flags = flags;
  sym->bind = SYMB_GLOBAL;

  /* These symbols will be moved from scriptsymbols list into
     LinkedSection.symbols during linker_copy() - at least
     the remaining, absolute ones. */
  if (addglobsym(gv,sym)) {
    *chain = sym;
    addtail(&gv->scriptsymbols,&sym->n);
  }
  else
    free(sym);
}


bool is_ld_script(struct ObjectUnit *obj)
{
  return obj==NULL ? FALSE : strncmp(obj->objname,"Linker Script ",14)==0;
}


static void add_cmdline_lnksyms(struct GlobalVars *gv)
{
  struct SymNames *sn;

  for (sn=gv->lnk_syms; sn!=NULL; sn=sn->next)
    scriptsymbol(gv,(char *)sn->name,0,sn->value,SYM_ABS,0);
}


static struct MemoryDescr *find_memblock(char *name)
{
  struct MemoryDescr *md = memory_blocks;

  while (md) {
    if (!strcmp(md->name,name))
      return md;
    md = md->next;
  }
  return NULL;
}


lword update_address(struct MemoryDescr *rmd,struct MemoryDescr *dmd,
                     unsigned long addbytes)
{
  const char *secname = current_ls ? current_ls->name : defmemname;

  if (rmd->current+(lword)addbytes > rmd->org+rmd->len)
    return rmd->current+(lword)addbytes;  /* failed reloc address */
  if (dmd!=rmd && dmd->current+(lword)addbytes > dmd->org+dmd->len)
    return dmd->current+(lword)addbytes;  /* failed destination address */

  rmd->current += (lword)addbytes;
  if (dmd != rmd)
    dmd->current += (lword)addbytes;
  return 0;  /* ok */
}


lword align_address(struct MemoryDescr *rmd,struct MemoryDescr *dmd,
                    unsigned long alignment)
{
  unsigned long addbytes = align(rmd->current,alignment);

  return update_address(rmd,dmd,addbytes);
}


static lword change_address(struct MemoryDescr *md,lword newval)
{
  const char *secname = current_ls ? current_ls->name : defmemname;
  lword oldval = md->current;

  md->current = newval;
  if ((md->current < md->org) || (md->current > md->org + md->len)) {
    /* Fatal: Size of memory region exceeded! */
    error(63,md->name,secname,(unsigned long long)md->current);
  }
  return newval-oldval;
}


static void skip_expr(int provided)
/* skip normal or provided expression */
{
  char c;

  if (provided)
    skipblock(1,'(',')');

  do {
    c = getchr();
  }
  while (c!=';' && c!='}' && c!='\0');
  if (c != ';')
    error(66,scriptname,getlineno(),';');  /* ';' expected */   
}


#if !DUMMY_SEC_FROM_PATTERN
static struct Section *get_dummy_sec(const char *name)
/* return dummy section of our artificial script-object with name */
{
  struct Section *sec;

  for (sec=(struct Section *)script_obj->sections.first;
       sec->n.next!=NULL; sec=(struct Section *)sec->n.next) {
    if (!strcmp(sec->name,name))
      break;
  }
  if (sec->n.next == NULL)
    ierror("get_dummy_sec(): Dummy section \"%s\" not found",name);
  return sec;
}
#endif


static void symbol_assignment(struct GlobalVars *gv,
                              char *symword,uint8_t symflags)
{
  char *fn = "symbol_assignment(): ";
  struct LinkedSection *cls = current_ls;
  struct MemoryDescr *rmd = cls ? cls->relocmem : vdefmem;
  struct MemoryDescr *dmd = cls ? cls->destmem : vdefmem;
  char symname[MAXLEN];
  struct Symbol *sym;
  lword expr_val;
  int try;
  
  try = testchr('?');  /* =? only assign undefined symbols */

  strcpy(symname,symword);  /* symword is guaranteed to fit into MAXLEN */
  if (!strcmp(symname,".")) {
    if (level >= 1) {
      if (!preparse) {
        if (!(symflags & SYMF_PROVIDED)) {
          lword offs;

          parse_expr(rmd->current,&expr_val);
          offs = change_address(rmd,expr_val);
          if (dmd != rmd) {
            /* change address in destmem by the same amount */
            change_address(dmd,dmd->current+offs);
          }
        }
        else {
          /* Address symbol '.' cannot be provided */
          error(108,scriptname,getlineno());
        }
      }
    }
    else {
      /* Address symbol '.' invalid outside SECTIONS block */
      error(101,scriptname,getlineno());
    }
  }

  else {
    /* real symbol assignment */

    if (level < 1) {
      if (preparse) {   /* level 0 (outside SECTION) is only parsed once! */
        if (parse_expr(-1,&expr_val)) {
          scriptsymbol(gv,symname,try,expr_val,SYM_ABS,symflags);
        }
        else {
          /* Only absolute expr. may be assigned outside SECTIONS block */
          error(110,scriptname,getlineno());
        }
      }
    }

    else {
      if (preparse) {
        scriptsymbol(gv,symname,try,0,SYM_ABS,symflags);
      }
      else {
        if (sym = findsymbol(gv,NULL,symname,0)) {
          int abs = parse_expr(rmd->current,&expr_val);

          if (level < 2)
            abs = 1;
          sym->type = abs ? SYM_ABS : SYM_RELOC;
          if (!abs) {
            if (current_ls) {
              /* assign section and remove from scriptsymbol list,
                 will be moved into section's symbol list in linker_copy() */
              #if !DUMMY_SEC_FROM_PATTERN
              if (listempty(&current_ls->sections))
                sym->relsect = get_dummy_sec(current_ls->name);
              else
              #endif
                sym->relsect = (struct Section *)current_ls->sections.first;
              /* a section with symbols must be allocated? @@@ */
              current_ls->flags |= SF_ALLOC;
              remnode(&sym->n);
            }
            else
              ierror("%sNo current LinkedSection set, while "
                     "defining %s",fn,symname);
          }
          sym->value = abs ? expr_val : (expr_val - current_ls->base);
        }
        else
          ierror("%s%s disappeared",fn,symname);
      }
    }
  }

  skip_expr(symflags&SYMF_PROVIDED);
}


static struct LinkedSection *getsection(struct GlobalVars *gv)
{
  struct LinkedSection *ls = NULL;
  char *sname;

  if (sname = getword()) {
    if (!(ls = find_lnksec(gv,sname,0,0,0,0)))
      error(79,scriptname,getlineno(),sname);  /* Undefined section */
  }
  else
    error(78,scriptname,getlineno());

  return ls;
}


static struct MemoryDescr *getmemblock(struct GlobalVars *gv)
{
  struct MemoryDescr *md = NULL;
  char *mname;

  if (mname = getword()) {
    if (!(md = find_memblock(mname)))
      error(135,scriptname,getlineno(),mname);  /* Undefined memory region */
  }
  else
    error(78,scriptname,getlineno());

  return md;
}


#if DUMMY_SEC_FROM_PATTERN
static struct Section *make_dummy_sec_from_pattern(struct GlobalVars *gv,
                                                   struct LinkedSection *ls)
/* The first section is always an empty dummy section, which is needed
   for all symbol definitions by the linker script.
   Its 'lnksec' reference and 'va' will be set during linking.
   Its name is build from the first section pattern found. */
{
  char *pp,*name,*np,c;
  struct Section *dummy;

  pp = getpattern();
  if (pp == NULL)
    return NULL;
  np = name = alloc(strlen(pp)+1);
  while (c = *pp++) {
    switch (c) {
      case '*':
        break;
      case '?':
        *np++ = '_';
        break;
      case '[':
        /* @@@ ! is not supported */
        if (*pp)
          *np++ = *pp++;
        while (*pp!='\0' && *pp!=']')
          pp++;
        if (*pp == ']')
          pp++;
        break;
      default:
        *np++ = c;
        break;
    }
  }
  *np = '\0';

  dummy = create_section(script_obj,name,NULL,0);
  dummy->lnksec = ls;  /* assign to ls */
  addtail(&ls->sections,&dummy->n);
  return dummy;
}
#endif


static int sf_addr(struct GlobalVars *gv,lword addr,lword *res)
{
  struct LinkedSection *ls;

  if (startofblock('(')) {
    if (ls = getsection(gv))
      *res = ls->base;
    endofblock('(',')');
  }
  return 1;
}


static int sf_align(struct GlobalVars *gv,lword addr,lword *res)
{
  lword a = term_absexp();

  addr += a - 1;
  *res = addr - addr % a;
  return 0;
}


static int sf_length(struct GlobalVars *gv,lword addr,lword *res)
{
  struct MemoryDescr *md;

  if (startofblock('(')) {
    if (md = getmemblock(gv))
      *res = md->len;
    endofblock('(',')');
  }
  return 1;
}


static int sf_loadaddr(struct GlobalVars *gv,lword addr,lword *res)
{
  struct LinkedSection *ls;

  if (startofblock('(')) {
    if (ls = getsection(gv))
      *res = ls->copybase;
    endofblock('(',')');
  }
  return 1;
}


static int sf_max(struct GlobalVars *gv,lword addr,lword *res)
{
  int abs1,abs2;
  lword val1,val2;

  term_two_exp(addr,&abs1,&val1,&abs2,&val2);
  if (val1 < val2) {
    *res = val2;
    return abs2;
  }
  *res = val1;
  return abs1;
}


static int sf_min(struct GlobalVars *gv,lword addr,lword *res)
{
  int abs1,abs2;
  lword val1,val2;

  term_two_exp(addr,&abs1,&val1,&abs2,&val2);
  if (val1 > val2) {
    *res = val2;
    return abs2;
  }
  *res = val1;
  return abs1;
}


static int sf_origin(struct GlobalVars *gv,lword addr,lword *res)
{
  struct MemoryDescr *md;

  if (startofblock('(')) {
    if (md = getmemblock(gv))
      *res = md->org;
    endofblock('(',')');
  }
  return 1;
}


static int sf_sizeof(struct GlobalVars *gv,lword addr,lword *res)
{
  struct LinkedSection *ls;

  if (startofblock('(')) {
    if (ls = getsection(gv))
      *res = ls->size;
    endofblock('(',')');
  }
  return 1;
}


static int sf_sizeofheaders(struct GlobalVars *gv,lword addr,lword *res)
{
  /* SIZEOF_HEADERS means that we want to include File- and PHDR in
     the first ELF segment */
  if (!find_phdr(gv,defhdr,defmem,defmem))
    add_phdr(gv,defhdr,PT_PHDR,PHDR_PHDRS,ADDR_NONE,defmem,defmem);

  *res = fff[gv->dest_format]->headersize(gv);
  return 1;
}


static void set_ctors_type(struct GlobalVars *gv,uint8_t type)
{
  if (gv->collect_ctors_type!=CCDT_NONE &&
      gv->collect_ctors_type!=type)
    error(71,scriptname,getlineno());  /* multiple constructor types */

  gv->collect_ctors_type = type;

  if (new_ls_name)
    gv->collect_ctors_secname = new_ls_name;
}


static void sc_ctors_gnu(struct GlobalVars *gv)
/* CONSTRUCTORS */
{
  set_ctors_type(gv,CCDT_GNU);
}


static void sc_ctors_vbcc(struct GlobalVars *gv)
/* VBCC_CONSTRUCTORS */
{
  set_ctors_type(gv,CCDT_VBCC);
}


static void sc_ctors_vbcc_elf(struct GlobalVars *gv)
/* VBCC_CONSTRUCTORS_ELF */
{
  set_ctors_type(gv,CCDT_VBCC_ELF);
}


static void sc_assert(struct GlobalVars *gv)
/* ASSERT(expression,"message") */
{
  if (startofblock('(')) {
    struct LinkedSection *cls = current_ls;
    struct MemoryDescr *md = cls ? cls->relocmem : vdefmem;
    lword val = -1;
    char *msg;

    if (!preparse)
      parse_expr(md->current,&val);
    else
      parse_expr(-2,&val);

    if (getchr() == ',')
      msg = getquoted();
    else
      msg = NULL;

    if (val == 0)
      error(73,scriptname,getlineno(),msg?msg:noname);

    endofblock('(',')');
  }
}


static void sc_entry(struct GlobalVars *gv)
/* ENTRY(symbol or address) */
{
  if (startofblock('(')) {
    char *entryname;

    if (entryname = getalnum()) {
      if (gv->entry_name == NULL)
        gv->entry_name = allocstring(entryname);
    }
    else
      error(78,scriptname,getlineno());
    endofblock('(',')');
  }
}


static void sc_extern(struct GlobalVars *gv)
/* EXTERN(symbol [symbol...]) */
{
  if (startofblock('(')) {
    char c;

    do {
      char *name = getword();

      if (*name)
        add_symnames(&gv->undef_syms,allocstring(name),0);
      else
        error(78,scriptname,getlineno());   /* missing argument */

      if (c = getchr())
        back(1);
    }
    while (c!='\0' && c!=')');

    endofblock('(',')');
  }
}


static void dofill(struct GlobalVars *gv,int sz)
/* fill with 8/16-bit value */
{
  if (startofblock('(')) {
    lword val;

    if (parse_expr(preparse ? -1 : 0,&val))
      gv->filldata = sz==1 ? (uint16_t)((val << 8) | (val & 0xff))
                     : (uint16_t)(val & 0xffff);
    else
      error(67,scriptname,getlineno());  /* Absolute number expected */
    endofblock('(',')');
  }
}


static void sc_fill8(struct GlobalVars *gv)
/* FILL8(data8) */
{
  dofill(gv,1);
}


static void sc_fill16(struct GlobalVars *gv)
/* FILL16(data16) */
{
  dofill(gv,2);
}


static bool get_dataval(void)
{
  if (startofblock('(')) {
    struct LinkedSection *cls = current_ls;
    struct MemoryDescr *md = cls ? cls->relocmem : vdefmem;

    if (!preparse)
      parse_expr(md->current,&dataval);
    return endofblock('(',')');
  }
  return FALSE;
}


static void sc_byte(struct GlobalVars *gv)
/* BYTE(data8) */
{
  if (get_dataval())
    datasize = 1;
}


static void sc_short(struct GlobalVars *gv)
/* SHORT(data16) */
{
  if (get_dataval())
    datasize = 2;
}


static void sc_long(struct GlobalVars *gv)
/* LONG(data32) */
{
  if (get_dataval())
    datasize = 4;
}


static void sc_quad(struct GlobalVars *gv)
/* QUAD(data64) */
{
  if (get_dataval())
    datasize = 8;
}


static void sc_reserve(struct GlobalVars *gv)
/* RESERVE(space) */
{
  if (get_dataval())
    datasize = -1;
}


static void sc_input(struct GlobalVars *gv)
/* INPUT(file1 [file2...]) */
{
  if (startofblock('(')) {
    char c,*fname;

    while (fname = getfilename()) {
      struct InputFile *ifn = alloc(sizeof(struct InputFile));

      if (strlen(fname)>2 && !strncmp(fname,"-l",2)) {
        fname += 2;
        while (isspace((unsigned)*fname))
          fname++;
        ifn->lib = TRUE;
        ifn->dynamic = gv->dynamic;
        ifn->so_ver = 0;  /* @@@ */
      }
      else
        ifn->lib = FALSE;
      ifn->name = allocstring(fname);
      ifn->flags = 0;  /* @@@ add support for clr/set flags? */
      ifn->renames = getsecrename();
      addtail(&gv->inputlist,&ifn->n);

      if ((c = getchr()) == ',')
        continue;
      back(1);
      if (c=='\0' || c==')')
        break;
    }

    endofblock('(',')');
  }
}


static void sc_provide(struct GlobalVars *gv)
/* PROVIDE(symbol = expression); */
{
  if (startofblock('(')) {
    char *symname;

    if (symname = getword()) {
      if (getchr()=='=' && *symname!='\0') {
        symbol_assignment(gv,symname,SYMF_PROVIDED);
      }
      else {
        error(66,scriptname,getlineno(),'=');  /* '=' expected */
        back(1);
      }
    }
    else
      error(78,scriptname,getlineno());
  }
}


static void sc_searchdir(struct GlobalVars *gv)
/* SEARCH_DIR(path) */
{
  if (startofblock('(')) {
    char *path;

    if (path = getfilename()) {
      struct LibPath *libp = alloc(sizeof(struct LibPath));

      libp->path = allocstring(path);
      addtail(&gv->libpaths,&libp->n);
    }
    else
      error(78,scriptname,getlineno());

    endofblock('(',')');
  }
}


static struct MemoryDescr *add_memblock(const char *name,
                                        lword org,lword len,lword id)
{
  struct MemoryDescr *last,*new = alloc(sizeof(struct MemoryDescr));

  if (last = memory_blocks) {
    while (last->next)
      last = last->next;
    last->next = new;
  }
  else
    memory_blocks = new;

  new->next = NULL;
  new->name = allocstring(name);
  new->org = new->current = org;
  if (org+len < org)
    len -= org+len+1;
  new->len = len;
  new->id = id;
  return new;
}


static int startofsecdef(lword *s_addr,unsigned *s_flags,lword *s_lma,
                         lword *s_banksz,lword *s_bankof)
/* Parse syntax of section definition until '{' and return VMA address,
   LMA address and type, when given.
   Returns bitfield. 0 when syntax is incorrect, 1 for correct,
   3 for s_addr set, 5 for s_lma set and 7 for both.
   Syntax: [addr-expr] [(BANKSIZE=n[,o])] [(NOLOAD)] : [AT(lma)] { */
{
  int ret = 1;
  lword caddr = -2; /* this prevents expression evaluation during pre-parse */
  char *buf,c;

  if (current_ls)
    caddr = current_ls->relocmem->current;

  *s_flags = 0;
  *s_banksz = *s_bankof = 0;

  c = getchr();
  if (c != ':') {
    if (c != '(') {
      back(1);
      parse_expr(caddr,s_addr);
#if 0 /* @@@ not needed? address must be within memory region */
      if (current_ls) {
        if (current_ls->relocmem != defmem) {
          /* warning: address overrides specified VMA memory region */
          error(76,scriptname,getlineno(),'V');
          current_ls->relocmem = defmem;
        }
      }
#endif
      ret |= 2;
      c = getchr();
    }

    while (c == '(') {  /* (BANKSIZE=n[,o]), (NOLOAD), etc. */
      if (buf = getalnum()) {
        if (!strcmp(buf,"BANKSIZE")) {
          if (getchr() == '=') {
            if (!preparse)
              parse_expr(-1,s_banksz);
            else
              parse_expr(-2,s_banksz);
            if (getchr() == ',') {  /* optional offset into bank */
              if (!preparse)
                parse_expr(-1,s_bankof);
              else
                parse_expr(-2,s_bankof);
            }
            else
              back(1);
          }
          else {
            error(66,scriptname,getlineno(),'=');  /* '=' expected */
            back(1);
            ret = 0;
          }
        }
        else if (!strcmp(buf,"NOLOAD"))
          *s_flags = 1;  /* NOLOAD-flag */
        else
          error(65,scriptname,getlineno(),buf);  /* unknown keyword ignored */

        if (getchr() != ')') {
          error(66,scriptname,getlineno(),')');  /* ')' expected */
          back(1);
          ret = 0;
        }
        c = getchr();
      }
      else
        break;
    }
  }

  if (c != ':') {
    error(66,scriptname,getlineno(),':');  /* ':' expected */
    back(1);
    return 0;
  }

  if (buf = getword()) {
    if (!strcmp(buf,"AT")) {
      if (getchr()=='(') {
        parse_expr(caddr,s_lma);
#if 0 /* @@@ not needed? address must be within memory region */
        if (current_ls) {
          if (current_ls->destmem != defmem) {
            if (current_ls->destmem != current_ls->relocmem) {
              /* warning: address overrides specified LMA memory region */
              error(76,scriptname,getlineno(),'L');
            }
            current_ls->destmem = atdefmem;
          }
        }
#endif
        if (ret)
          ret |= 4;
        if (getchr() != ')') {
          error(66,scriptname,getlineno(),')');  /* ')' expected */
          back(1);
          ret = 0;
        }
      }
      else {
        error(66,scriptname,getlineno(),'(');  /* '(' expected */
        back(1);
        ret = 0;
      }
    }
    else
      error(65,scriptname,getlineno(),buf);  /* unknown keyword ignored */
  }

  if (!startofblock('{'))
    ret = 0;

  return ret;
}


static struct Phdr **endofsecdef(struct GlobalVars *gv,
                                 struct LinkedSection *ls)
/* Parses syntax of everything which follows the '}' of a section
   definition. Memory-region pointers and fill-value will be initialized
   when the section-pointer 'ls' is given.
   On success the function returns a list of pointers to Phdr structures
   which were defined (terminated by a NULL pointer). This list must
   be deallocated by the caller!
   Syntax: } [>memregion] [AT>lma-region] [:phdr ...] [=fill] */
{
  const char *fn = "endofsecdef(): ";
  struct Phdr **pp = alloc(64 * sizeof(struct Phdr *));  /* it's ugly! */
  struct Phdr *phdr;
  struct MemoryDescr *md;
  int phdrcnt=0,done=0,err=0;
  char c,*buf;
  lword val;

  endofblock('{','}');

  while (!done) {
    switch (c = getchr()) {
      case '>':
      case '@':
      case 'A':
        if (c == 'A') {
          if (getchr() == 'T') {
            if (getchr() != '>') {
              back(1);
              error(66,scriptname,getlineno(),'>');  /* '>' expected */
            }
          }
          else {
            /* it's not AT, but maybe an ASSERT keyword, so get out */
            back(2);
            done = 1;
            break;
          }
        }
        if (buf = getword()) {
          if (md = find_memblock(buf)) {
            if (c == '>') {
              if (ls) {
                ls->relocmem = md;
                vdefmem = md;
                if (ls->destmem == NULL) {
                  ls->destmem = md;
                  ldefmem = md;
                }
              }
            }
            else if (ls) {
              ls->destmem = md;
              ldefmem = md;
            }
          }
          else {
            error(70,scriptname,getlineno(),buf);  /* Unknown memory region */
            err = 1;
          }
        }
        else {
          error(78,scriptname,getlineno());   /* missing argument */
          err = 1;
        }
        break;

      case ':':
        if (buf = getword()) {
          if (phdr = find_phdr(gv,buf,NULL,NULL)) {
            pp[phdrcnt++] = phdr;
            if (phdrcnt >= 64)
              ierror("%sphdrcnt overrun",fn);
          }
          else {
            error(111,scriptname,getlineno(),buf);  /* unknown PHDR */
            err = 1;
          }
        }
        else
          error(78,scriptname,getlineno());
        break;

      case '=':
        if (parse_expr(-1,&val)) {
          gv->filldata = (uint16_t)(val & 0xffff);
        }
        else {
          error(67,scriptname,getlineno());  /* Absolute number expected */
          err = 1;
        }
        break;

      case '\0':
        done = 1;
        break;

      default:
        done = 1;
        back(1);
        break;
    }
  }

  if (err) {
    free(pp);
    pp = NULL;
  }
  else
    pp[phdrcnt] = NULL;

  return pp;
}


static int check_command(struct GlobalVars *gv,char *name,uint32_t flags)
{
  struct ScriptCmd *scptr;

  datasize = 0;
  for (scptr=ldCommands; scptr->name; scptr++) {
    if (!strcmp(scptr->name,name))
      break;
  }

  if (scptr->name) {
    if (scptr->flags & flags) {
      if (scptr->cmdptr) {
        scptr->cmdptr(gv);  /* execute linker-script command */
      }
      else {
        error(69,scriptname,getlineno(),name);  /* command ignored */
        if (scptr->flags & SCMDF_PAREN)
          skipblock(0,'(',')');
      }
    }
    else {
      if (!(flags & SCMDF_IGNORE)) {
        /* command not allowed outside SECTIONS block */
        error(107,scriptname,getlineno(),name);
      }
      if (scptr->flags & SCMDF_PAREN)
        skipblock(0,'(',')');
      if (scptr->flags & SCMDF_SEMIC)
        skip_expr(0);
    }
    return 1;
  }

  return 0;
}


static lword readmemparam(struct GlobalVars *gv,char *key)
{
  char *str;
  lword val = 0;

  if (str = getword()) {
    if (toupper((unsigned char)*str) == *key) {
      if (getchr() == '=')
        parse_expr(gv->start_addr,&val);  /* . is start address */
      else
        error(66,scriptname,getlineno(),'=');
    }
    else
      error(68,scriptname,getlineno(),key);  /* keyword expected */
  }
  else
    error(68,scriptname,getlineno(),key);
  return val;
}


static void define_memory(struct GlobalVars *gv)
/* Syntax: */
/* <memblockname> [(attr)]: o[rg] = <base address>, l[en] = <maxlen> */
{
  char memname[MAXLEN];
  char *str,c;
  lword org,len,id;

  if (startofblock('{')) {
    while (str = getword()) {
      strcpy(memname,str);
      if ((c = getchr()) == '(') {
        /* @@@ memory attributes are ignored! */
        skipblock(1,'(',')');
        c = getchr();
      }
      if (c == ':') {
        org = readmemparam(gv,"ORIGIN");
        if (getchr() == ',') {
          len = readmemparam(gv,"LENGTH");
          if (getchr() == ',') {
            id = readmemparam(gv,"ID");
          }
          else {
            back(1);
            id = MEM_NOID;
          }
          add_memblock(memname,org,len,id);
        }
        else
          error(66,scriptname,getlineno(),',');  /* ',' expected */
      }
      else
        error(66,scriptname,getlineno(),':');    /* ':' expected */
    }
    endofblock('{','}');
  }
}


static void define_phdrs(struct GlobalVars *gv)
/* Syntax: */
/* <phdrname> <type> [FILEHDR] [PHDRS] [AT(addr)] [FLAGS(flags)]; */
{
  char phdrname[MAXLEN];
  char *str;
  struct Phdr *p;
  int need_phdr;

  if (!startofblock('{'))
    return;

  while (str = getword()) {
    char c;
    uint32_t type;
    uint16_t flags = 0;
    lword addr = ADDR_NONE;

    strcpy(phdrname,str);
    if (str = getword()) {

      if (!strncmp(str,"PT_",3)) {
        switch (str[3]) {
          case 'L': type = PT_LOAD; break;
          case 'D': type = PT_DYNAMIC; break;
          case 'I': type = PT_INTERP; break;
          case 'N': type = PT_NOTE; break;
          case 'S': type = PT_SHLIB; break;
          case 'P': type = PT_PHDR; break;
          default:
            error(99,scriptname,getlineno(),str);   /* Illegal PHDR type */
            type = PT_NULL;
            break;
        }

        while (str = getword()) {
          if (!strcmp(str,"FILEHDR")) {
            flags |= PHDR_FILEHDR;
          }
          else if (!strcmp(str,"PHDRS")) {
            flags |= PHDR_PHDRS;
          }
          else if (!strcmp(str,"AT")) {
            addr = term_absexp();
            flags |= PHDR_ADDR;
          }
          else if (!strcmp(str,"FLAGS")) {
            flags |= PHDR_FLAGS | ((uint32_t)term_absexp() & 7);
          }
          else
            error(65,scriptname,getlineno(),str);  /* unknown keyword ignored */
        }
        c = getchr();
        if (c != ';') {
          error(66,scriptname,getlineno(),';');  /* ',' expected */
          if (c == '}')
            return;
        }

        add_phdr(gv,phdrname,type,flags,addr,NULL,NULL);
      }
      else {
        error(99,scriptname,getlineno(),str);   /* Illegal PHDR type */
        skipblock(1,0,';');
      }
    }
    else {
      error(78,scriptname,getlineno());   /* missing argument */
      skipblock(1,0,';');
    }
  }
  endofblock('{','}');

  /* add a default PHDR when no PT_PHDR was defined, but was requested
     to be part of a PT_LOAD PHDR */
  for (p=gv->phdrlist,need_phdr=0; p; p=p->next) {
    if (p->type == PT_PHDR)
      return;
    if (p->flags & PHDR_PHDRS)
      need_phdr = 1;
  }
  if (need_phdr)
    add_phdr(gv,defhdr,PT_PHDR,PHDR_PHDRS,ADDR_NONE,defmem,defmem);
}


static void predefine_sections(struct GlobalVars *gv)
/* Syntax: */
/* <secname> [addr] [BANKSIZE(size[,offs])] [(type)] : [AT(lma)] { ... } */
/*           [>region] [AT>lma-region] [:PHDR ...] [=FillExp] */
{
  char *keyword;
  struct Phdr **defplist = NULL;

  if (!startofblock('{'))
    return;
  skip();
  secblkbase = gettxtptr();
  secblkline = getlineno();

  do {
    while (keyword = getword()) {
      if (check_command(gv,keyword,SCMDF_GLOBAL|SCMDF_IGNORE)) {
        continue;
      }
      else if (getchr() == '=') {
        symbol_assignment(gv,keyword,0);
      }

      else {
        /* check for section definition */
        struct LinkedSection *ls;
        struct Section *dummy_sec;
        struct Phdr **plist;
        const char *s_name;
        lword s_addr,s_lma,s_banksz,s_bankof;
        unsigned s_flags;
        char c;
        int fl;

        back(1);
        s_name = allocstring(keyword);

        if (fl = startofsecdef(&s_addr,&s_flags,&s_lma,&s_banksz,&s_bankof)) {
          dummy_sec = NULL;
          ls = create_lnksect(gv,s_name,ST_UNDEFINED,0,0,0,0);
          if (s_flags & 1)
            ls->ld_flags |= LSF_NOLOAD;
          if (fl & 4)
            ldefmem = atdefmem;  /* AT(addr) in section definition */

          new_ls_name = ls->name;
          level = 2;
          do {
            while (keyword = getpattern()) {
              if (check_command(gv,keyword,SCMDF_GLOBAL|SCMDF_IGNORE)) {
                continue;
              }
              else {
                c = getchr();
                if (c == '=') {
                  symbol_assignment(gv,keyword,0);
                }
                else if (c == '(') {
                  /* skip section pattern */
                  #if DUMMY_SEC_FROM_PATTERN
                  if (!dummy_sec) {
                    dummy_sec = make_dummy_sec_from_pattern(gv,ls);
                    if (dummy_sec == NULL)
                      error(65,scriptname,getlineno(),keyword);
                  }
                  #else
                  if (!dummy_sec) {
                    dummy_sec = create_section(script_obj,s_name,NULL,0);
                    dummy_sec->lnksec = ls;  /* assign to ls */
                    addtail(&script_obj->sections,&dummy_sec->n);
                  }
                  #endif
                  skipblock(1,'(',')');
                }
                else {
                  /* unknown keyword ignored */
                  error(65,scriptname,getlineno(),keyword);
                  back(1);
                }
              }
            }
          }
          while (getchr() == ';');
          back(1);
          level = 1;
          new_ls_name = NULL;

          if (plist = endofsecdef(gv,ls)) {
            /* check segment assignments */
            struct Phdr *p;

            /* set default memory region when needed */
            if (ls->relocmem == NULL)
              ls->relocmem = vdefmem;
            if (ls->destmem == NULL)
              ls->destmem = ldefmem;

            if (*plist==NULL && defplist!=NULL) {
              /* use last PHDR assignments, when none given */
              free(plist);
              plist = defplist;
            }
            if (*plist!=NULL && plist!=defplist) {
              if (defplist)
                free(defplist);
              defplist = plist;
            }

            if (*plist) {
              while (p = *plist++) {
                if (p->type != PT_NULL)
                  p->flags |= PHDR_USED;
              }
            }
            else {
              /* no PHDR assignment - select segment by memory region */
              const char *pname;
              uint32_t type;

              if (ls->relocmem==defmem && ls->destmem==defmem) {
                use_segment(gv,deftxt,PT_LOAD,defmem,defmem);
                use_segment(gv,defdat,PT_LOAD,defmem,defmem);
              }
              else
                use_segment(gv,defload,PT_LOAD,ls->relocmem,ls->destmem);

              /* check other segments (interp, dynamic, note) */
              if (type = guess_special_segment(s_name,&pname))
                use_segment(gv,pname,type,ls->relocmem,ls->destmem);
            }
          }
          else {
            /* set default memory regions in error case to avoid segfault */
            if (ls->relocmem == NULL)
              ls->relocmem = vdefmem;
            if (ls->destmem == NULL)
              ls->destmem = ldefmem;
          }
        }
        else {
          free((void *)s_name);
          error(65,scriptname,getlineno(),s_name); /* unknown keyword ignored */
        }
      }
    }
  }
  while (getchr() == ';');
  back(1);

  if (defplist)
    free(defplist);
  endofblock('{','}');
}


static void add_section_to_segments(struct GlobalVars *gv,
                                    struct LinkedSection *ls,
                                    struct Phdr **plist)
{
  struct Phdr **pp = plist;
  struct Phdr *p,*p2;
  bool loadseg_present = FALSE;

  if (!(ls->flags & SF_ALLOC) || (ls->ld_flags & LSF_NOLOAD) ||
      ls->type==ST_UNDEFINED) {
    /* non-allocated, NOLOAD or undef. sections are not part of any segment! */
    ls->flags &= ~SF_ALLOC;
    return;
  }

  while (p = *pp++) {
    if (p->flags & PHDR_CLOSED) {
      /* segment is closed and can't be reused */
      error(75,scriptname,getlineno(),p->name);
    }
    else {
      if (p->type == PT_LOAD) {
        if (loadseg_present) {
          /* Section was assigned to more than one PT_LOAD segment */
          error(80,scriptname,getlineno(),ls->name);
          p->flags &= ~PHDR_USED;
          continue;
        }
        else
          loadseg_present = TRUE;
      }
      if (p->start == ADDR_NONE)
        p->start = p->file_end = ls->copybase;
      if (p->start_vma == ADDR_NONE)
        p->start_vma = ls->base;
      if (ls->copybase < p->mem_end) {
        /* section conflicts with segment - it doesn't cleanly attach to it */
        error(83,ls->name,(unsigned long long)ls->copybase,
              (unsigned long long)ls->copybase+ls->size,p->name,
              (unsigned long long)p->start,(unsigned long long)p->mem_end);
      }
      else {
        p->mem_end = ls->copybase + ls->size;
        if (ls->filesize)
          p->file_end = ls->copybase + ls->filesize;
        if (ls->alignment > p->alignment)
          p->alignment = ls->alignment;
      }
    }

    /* move referenced segment to the end of the list */
    if (gv->phdrlist == p) {
      gv->phdrlist = p->next;
    }
    else {
      for (p2=gv->phdrlist; p2; p2=p2->next) {
        if (p2->next == p)
          p2->next = p->next;
      }
    }
    if (gv->phdrlist == NULL) {
      gv->phdrlist = p;
    }
    else {
      for (p2=gv->phdrlist; p2->next; p2=p2->next);
      p2->next = p;
    }
    p->next = NULL;
  }

  /* close all unreferenced segments */
  for (p=gv->phdrlist; p; p=p->next) {
    for (pp=plist; *pp; pp++) {
      if (p == *pp)
        break;
    }
    if (*pp == NULL) {
      if (p->start!=ADDR_NONE && p->vmregion==defmem && p->lmregion==defmem)
        /* @@@ only close PHDRs wenn not using '>' into memory region ??? */
        p->flags |= PHDR_CLOSED;  /* close no longer used segment */
    }
  }
}


static void freepatlist(char **patlist)
{
  char **p;

  if ((p = patlist) != NULL) {
    while (*p) {
      free(*p);
      p++;
    }
    free(patlist);
  }
}


void free_patterns(struct Patterns *pat)
/* free file-pattern and section-pattern list, allocated in next_pattern(),
   clear structure for reuse */
{
  if (pat->fmatch)
    free(pat->fmatch);
  freepatlist(pat->fexclude);
  freepatlist(pat->smatch);
  freepatlist(pat->sexclude);
  memset(pat,0,sizeof(struct Patterns));
}


static char **store_pattern(int idx,char *str)
/* store pattern string pointer at index idx of a dynamic array */
{
  static char **patarr;
  static int maxidx;

  if (patarr == NULL) {
    maxidx = 4;
    patarr = alloc(maxidx*sizeof(char *));
  }
  if (idx >= maxidx) {
    maxidx += maxidx;
    patarr = re_alloc(patarr,maxidx*sizeof(char *));
  }
  patarr[idx] = str;
  return patarr;
}


static char **makeplist(char **plist,int pcnt)
{
  char **new = alloc((pcnt+1) * sizeof(char *));

  memcpy(new,plist,pcnt*sizeof(char *));
  new[pcnt] = NULL;
  return new;
}


static char **getpatlist(void)
/* get list of one or multiple file/section patterns */
{
  char *pstr,**plist,**newplist;
  int pcnt = 0;

  while (pstr = getpattern())
    plist = store_pattern(pcnt++,allocstring(pstr));

  return makeplist(plist,pcnt);
}


static bool filepatcmd(struct GlobalVars *gv,char *keyword,struct Patterns *pat)
{
  if (!strcmp(keyword,"KEEP")) {
    pat->flags |= PFL_KEEP;
    return TRUE;
  }
  return FALSE;
}


static bool maxsortlev(struct Patterns *pat)
{
  if (pat->ssortlev >= PSORT_MAXLEV) {
    error(167,scriptname,getlineno(),PSORT_MAXLEV);  /* max sorting level */
    return FALSE;
  }
  return TRUE;
}


static void pat_newsort(struct Patterns *pat,unsigned smode)
{
  if (maxsortlev(pat)) {
    pat->ssort[pat->ssortlev] &= ~PSORT_MODE;
    pat->ssort[pat->ssortlev++] |= smode;
  }
}


static bool sectpatcmd(struct GlobalVars *gv,char *keyword,struct Patterns *pat)
{
  if (keyword == NULL)
    return FALSE;

  if (getchr() == '(') {
    if (!strcmp(keyword,"SORT_BY_NAME") || !strcmp(keyword,"SORT")) {
      pat_newsort(pat,PSORT_NAME);
      return TRUE;
    }
    else if (!strcmp(keyword,"SORT_BY_ALIGNMENT")) {
      pat_newsort(pat,PSORT_ALIGN);
      return TRUE;
    }
    else if (!strcmp(keyword,"SORT_BY_SIZE")) {
      pat_newsort(pat,PSORT_SIZE);
      return TRUE;
    }
    else if (!strcmp(keyword,"REVERSE")) {
      if (maxsortlev(pat))
        pat->ssort[pat->ssortlev] ^= PSORT_REV;
      return TRUE;
    }
  }

  back(1);
  return FALSE;
}


static bool excludecmd(struct GlobalVars *gv,char *keyword,struct Patterns *pat)
{
  if (keyword) {
    if (getchr() == '(') {
      char **plist = NULL;

      if (!strcmp(keyword,"EXCLUDE_FILE")) {
        if (pat->fexclude)
          error(168,scriptname,getlineno(),"FILE");  /* multiple EXCLUDE_FILE */
        pat->fexclude = plist = getpatlist();
      }
      else if (!strcmp(keyword,"EXCLUDE_SECTION")) {
        if (pat->sexclude)
          error(168,scriptname,getlineno(),"SECTION");  /* multiple EXCLUDE_SECTION */
        pat->sexclude = plist = getpatlist();
      }
      else {
        back(1);
        return FALSE;
      }

      if (!expectchr(')')) {
        freepatlist(plist);
        return FALSE;
      }
      return TRUE;
    }
    back(1);
  }
  return FALSE;
}


static bool parse_pattern(struct GlobalVars *gv,
                          char *keyword,struct Patterns *pat)
/* Parse file/section-pattern and allocate pattern-lists.
   Note, that a keyword/pattern and an opening parentheses was already parsed! */
{
  int parens = 1;  /* parentheses counter */
  int pcnt = 0;    /* number of patterns in list */
  char **plist;

  pat->flags = 0;
  pat->ssort[0] = pat->ssort[1] = PSORT_NONE;

  /* must start with a file-pattern or a command */
  while (filepatcmd(gv,keyword,pat)) {
    if (keyword = getpattern()) {
      if (!expectchr('('))
        return FALSE;
    }
    else {
      error(78,scriptname,getlineno());   /* missing argument */
      return FALSE;
    }
    parens++;
  }

  /* store file pattern */
  pat->fmatch = allocstring(keyword);

  /* exclude-commands come first in front of the section patterns */
  do {
    keyword = getpattern();
  } while (excludecmd(gv,keyword,pat));

  /* handle potential sort-commands arround the section patterns */
  while (sectpatcmd(gv,keyword,pat)) {
    if ((keyword = getpattern()) == NULL) {
      error(78,scriptname,getlineno());   /* missing argument */
      return FALSE;
    }
    parens++;
  }

  /* store section patterns */
  if (keyword == NULL)
    keyword = "*";
  do {
    plist = store_pattern(pcnt++,allocstring(keyword));
  } while (keyword = getpattern());
  pat->smatch = makeplist(plist,pcnt);

  /* finally collect all missing closing parentheses */
  while (parens--) {
    if (!endofblock('(',')')) {
      free_patterns(pat);
      return FALSE;
    }
  }
  return TRUE;  /* Pattern record is valid */
}


static struct Section *make_data_element(struct GlobalVars *gv)
/* Construct a section for a new data element (BYTE, SHORT, ...) */
{
  uint8_t *data = alloc(tbytes(gv,datasize));
  struct Section *sec;
  const char *name;

  #if !DUMMY_SEC_FROM_PATTERN
  if (listempty(&current_ls->sections))
    name = get_dummy_sec(current_ls->name);
  else
  #endif
    name = current_ls->name;  /* name is not important */

  switch (datasize) {
    case 1: writetbyte(gv,data,dataval); break;
    case 2: writetbytes(gv,2,data,dataval); break;
    case 4: writetbytes(gv,4,data,dataval); break;
    case 8: writetbytes(gv,8,data,dataval); break;
    default: ierror("make_data_element"); break;
  }

  sec = create_section(script_obj,name,data,datasize);
  sec->type = ST_DATA;
  addtail(&script_obj->sections,&sec->n);
  return sec;
}


static struct Section *reserve_space(struct GlobalVars *gv)
/* make an empty section at an address after a reserved number of bytes */
{
  if (level>=1 && !preparse) {
    struct LinkedSection *cls = current_ls;
    struct MemoryDescr *rmd = cls ? cls->relocmem : vdefmem;
    struct MemoryDescr *dmd = cls ? cls->destmem : vdefmem;
    struct Section *sec;
    const char *name;

    /* advance address over reserved space */
    if (change_address(rmd,rmd->current+dataval) != dataval)
      ierror(0);
    if (dmd != rmd) {
      /* change address in destmem by the same amount */
      change_address(dmd,dmd->current+dataval);
    }

    /* create an empty section at that address */
    #if !DUMMY_SEC_FROM_PATTERN
    if (listempty(&current_ls->sections))
      name = get_dummy_sec(current_ls->name);
    else
    #endif
      name = current_ls->name;  /* name is not important */

    /* NOTE: passing the noname pointer for the empty data section makes sure
       that the gap until the section's start address is properly filled! */
    sec = create_section(script_obj,name,(uint8_t *)noname,0);
    sec->type = ST_DATA;
    addtail(&script_obj->sections,&sec->n);
    return sec;
  }
  else
    ierror(0);
  return NULL;  /* not reached */
}


struct Section *next_pattern(struct GlobalVars *gv,struct Patterns *pat)
/* Provides next file-pattern and a list of section-patterns, when present.
   Returns NULL, when section definition is closed,
   a section pointer for a data element to insert (BYTE, SHORT, LONG, ...),
   and VALIDPAT for valid file/section-patterns in fpat/spatlist. */
{
  static char *fn = "next_pattern(): ";
  static struct Phdr **defplist;
  struct Phdr **plist;
  char c,*keyword;

  level = 2;

  do {
    while (keyword = getpattern()) {
      if (check_command(gv,keyword,SCMDF_GLOBAL|SCMDF_SECDEF)) {
        if (datasize < 0)
          return reserve_space(gv);
        else if (datasize)
          return make_data_element(gv);
        continue;
      }
      else {
        c = getchr();
        if (c == '=') {
          symbol_assignment(gv,keyword,0);
        }
        else if (c == '(') {
          if (parse_pattern(gv,keyword,pat))
            return VALIDPAT;
        }
        else
          back(1);
      }
    }
  }
  while (getchr() == ';');
  back(1);

  level = 1;
  if (plist = endofsecdef(gv,NULL)) {
    struct Phdr *guess_plist[3];

    if (*plist == NULL) {
      free(plist);
      if (!defplist) {
        /* No PHDR specification available, so we have to guess:
           In this default mode there are two PT_LOAD-segments
           available: text(non-writable,executable) and data(read-write).
           Dynamic, Interp and Note segments will be recognized by name */
        static char *phdr_miss_err = "%sPHDR %s missing";
        const char *pname;

        plist = guess_plist;

        if (!(current_ls->flags & SF_ALLOC)) {
          plist[0] = NULL;  /* not part of any segment */
        }

        else if (current_ls->relocmem==defmem &&
                 current_ls->destmem==defmem) {

          if (current_ls->type==ST_CODE ||
              (current_ls->protection & SP_EXEC) ||
              !(current_ls->protection & SP_WRITE)) {
            if (!(plist[0] = find_phdr(gv,deftxt,defmem,defmem)))
              ierror(phdr_miss_err,fn,deftxt);
            if (plist[0]->flags & PHDR_CLOSED) {
              if (!(plist[0] = find_phdr(gv,defdat,defmem,defmem)))
                ierror(phdr_miss_err,fn,defdat);
            }
          }
          else {
            if (!(plist[0] = find_phdr(gv,defdat,defmem,defmem)))
              ierror(phdr_miss_err,fn,defdat);
          }

          if (guess_special_segment(current_ls->name,&pname)) {
            if (!(plist[1] = find_phdr(gv,pname,defmem,defmem)))
              ierror(phdr_miss_err,fn,pname);
            plist[2] = NULL;
          }
          else
            plist[1] = NULL;
        }

        else {
          if (!(plist[0] = find_phdr(gv,defload,current_ls->relocmem,
                                     current_ls->destmem)))
            ierror(phdr_miss_err,fn,defload);

          if (guess_special_segment(current_ls->name,&pname)) {
            if (!(plist[1] = find_phdr(gv,pname,current_ls->relocmem,
                                       current_ls->destmem)))
              ierror(phdr_miss_err,fn,pname);
            plist[2] = NULL;
          }
          else
            plist[1] = NULL;
        }

      }
      else
        plist = defplist;
    }
    else {
      if (defplist)
        free(defplist);
      defplist = plist;
    }

    /* at this point there is at least one PHDR reference in the list */
    add_section_to_segments(gv,current_ls,plist);
  }

  vdefmem = current_ls->relocmem;
  ldefmem = current_ls->destmem;
  current_ls = NULL;
  return NULL;
}


struct LinkedSection *next_secdef(struct GlobalVars *gv)
/* Find next section definition in script and return the machting
   structure. */
{
  char *keyword;
  /*struct Phdr *p;*/

  level = 1;
  current_ls = NULL;

  do {
    while (keyword = getword()) {
      if (check_command(gv,keyword,SCMDF_GLOBAL)) {
        continue;
      }
      else if (getchr() == '=') {
        symbol_assignment(gv,keyword,0);
      }

      else {
        /* check for section definition */
        struct LinkedSection *ls;
        int ret;
        lword s_addr,s_lma,s_banksz,s_bankof;
        unsigned s_flags;

        back(1);
        if (ls = find_lnksec(gv,keyword,0,0,0,0)) {
          current_ls = ls;
          if (ret = startofsecdef(&s_addr,&s_flags,&s_lma,&s_banksz,&s_bankof)) {
            if (s_banksz) {
              ls->banksize = s_banksz;
              ls->bankoffs = s_bankof;
            }
            if (ret & 2)
              change_address(ls->relocmem,s_addr);
            if (ret & 4) {
              if (ls->destmem == ls->relocmem)
                ls->destmem = add_memblock("lma",MEM_DEFORG,MEM_DEFLEN,MEM_NOID);
              change_address(ls->destmem,s_lma);
            }

            return ls;
          }
        }
        else
          ierror("next_secdef(): No Section for %s defined",keyword);
      }
    }
  }
  while (getchr() == ';');
  back(1);

#if 0 /* @@@ don't do it - it modifies header size !!! */
  /* scan for empty segments and declare them as unused */
  for (p=gv->phdrlist; p; p=p->next) {
    if (p->type!=PT_PHDR && (p->start==ADDR_NONE || p->start_vma==ADDR_NONE)) {
      p->start = p->start_vma = p->file_end = p->mem_end = 0;
      p->flags &= ~PHDR_USED;
    }
  }
#endif
  return NULL;
}


void init_secdef_parse(struct GlobalVars *gv)
{
  if (secblkbase)
    init_parser(gv,scriptname,secblkbase,secblkline);
}


static struct ObjectUnit *make_ld_objunit(struct GlobalVars *gv)
/* creates a LinkFile node and a ObjectUnit for symbols defined
   by the linker script */
{
  char namebuf[MAXLEN];
  const char *n;
  struct ObjectUnit *ou;

  snprintf(namebuf,MAXLEN,"Linker Script <%s>",scriptname);
  n = allocstring(namebuf);
  if (ou = art_objunit(gv,n,NULL,0)) {
    ou->flags |= OUF_LINKED | OUF_SCRIPT;
    /*ou->lnkfile->type = ID_OBJECT; @@@ should stay ID_ARTIFICIAL? */
    addhead(&gv->selobjects,&ou->n);
  }
  return ou;
}


void init_ld_script(struct GlobalVars *gv)
{
  char c;

  if (!gv->dest_object) {
    scriptbase = gv->ldscript ? gv->ldscript :
                                fff[gv->dest_format]->exeldscript;
  }
  else if (gv->dest_sharedobj) {
    scriptbase = gv->ldscript ? gv->ldscript :
                                fff[gv->dest_format]->soldscript;
  }

  if (scriptbase) {
    bool secdef = FALSE;
    char *keyword;

    /* initialization */
    preparse = TRUE;
    current_ls = NULL;
    level = 0; /* outside SECTIONS block */
    defmem = add_memblock(defmemname,MEM_DEFORG,MEM_DEFLEN,MEM_NOID);
    atdefmem = add_memblock("lmadefault",MEM_DEFORG,MEM_DEFLEN,MEM_NOID);
    vdefmem = ldefmem = defmem;
    change_address(defmem,gv->start_addr);
    if (!(scriptname = gv->scriptname))
      scriptname = "built-in script";
    script_obj = make_ld_objunit(gv);

    /* pre-parse script: get memory-regions and symbol definitions */
    init_parser(gv,scriptname,scriptbase,1);
    add_cmdline_lnksyms(gv);

    do {
      while (keyword = getword()) {

        if (!strcmp(keyword,"MEMORY")) {
          gv->use_ldscript = TRUE;
          if (secdef == FALSE) {
            define_memory(gv);
          }
          else {
            /* MEMORY behind SECTIONS ignored */
            error(100,scriptname,getlineno(),keyword);
            skipblock(0,'{','}');
          }
        }

        else if (!strcmp(keyword,"PHDRS")) {
          gv->use_ldscript = TRUE;
          if (secdef == FALSE) {
            define_phdrs(gv);
          }
          else {
            /* PHDRS behind SECTIONS ignored */
            error(100,scriptname,getlineno(),keyword);
            skipblock(0,'{','}');
          }
        }

        else if (!strcmp(keyword,"SECTIONS")) {
          gv->use_ldscript = TRUE;
          if (secdef)
            error(74,scriptname,getlineno());  /* SECTIONS block def. twice */
          else
            secdef = TRUE;

          /* Define default PHDRs */
          if (gv->phdrlist == NULL) {
            add_phdr(gv,deftxt,PT_LOAD,PHDR_FLAGS|PF_X|PF_R,
                     ADDR_NONE,defmem,defmem);
            add_phdr(gv,defdat,PT_LOAD,PHDR_FLAGS|PF_R|PF_W,
                     ADDR_NONE,defmem,defmem);
          }
          level = 1;
          predefine_sections(gv);
          level = 0;
        }

        else {
          if (check_command(gv,keyword,SCMDF_GLOBAL)) {
            /* script-command executed */
            continue;
          }

          else if (getchr() == '=') {
            /* symbol assignment */
            symbol_assignment(gv,keyword,0);
          }

          else {
            /* unknown keyword ignored */
            error(65,scriptname,getlineno(),keyword);
            back(1);
          }
        }
      }
    }
    while ((c = getchr()) == ';');

    if (c) {
      back(1);
      error(78,scriptname,getlineno());   /* missing argument */
    }
    preparse = FALSE;
  }
}
