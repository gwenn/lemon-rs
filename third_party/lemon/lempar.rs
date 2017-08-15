/*
** 2000-05-29
**
** The author disclaims copyright to this source code.  In place of
** a legal notice, here is a blessing:
**
**    May you do good and not evil.
**    May you find forgiveness for yourself and forgive others.
**    May you share freely, never taking more than you give.
**
*************************************************************************
** Driver template for the LEMON parser generator.
**
** The "lemon" program processes an LALR(1) input grammar file, then uses
** this template to construct a parser.  The "lemon" program inserts text
** at each "%%" line.  Also, any "P-a-r-s-e" identifer prefix (without the
** interstitial "-" characters) contained in this template is changed into
** the value of the %name directive from the grammar.  Otherwise, the content
** of this template is copied straight through into the generate parser
** source file.
**
** The following is the concatenation of all %include directives from the
** input grammar file:
*/
/************ Begin %include sections from the grammar ************************/
%%
/**************** End of %include directives **********************************/
/* These constants specify the various numeric values for terminal symbols
** in a format understandable to "makeheaders".  This section is blank unless
** "lemon" is run with the "-m" command-line option.
***************** Begin makeheaders token definitions *************************/
%%
/**************** End makeheaders token definitions ***************************/

/* The next sections is a series of control #defines.
** various aspects of the generated parser.
**    YYCODETYPE         is the data type used to store the integer codes
**                       that represent terminal and non-terminal symbols.
**                       "unsigned char" is used if there are fewer than
**                       256 symbols.  Larger types otherwise.
**    YYNOCODE           is a number of type YYCODETYPE that is not used for
**                       any terminal or nonterminal symbol.
**    YYFALLBACK         If defined, this indicates that one or more tokens
**                       (also known as: "terminal symbols") have fall-back
**                       values which should be used if the original symbol
**                       would not parse.  This permits keywords to sometimes
**                       be used as identifiers, for example.
**    YYACTIONTYPE       is the data type used for "action codes" - numbers
**                       that indicate what to do in response to the next
**                       token.
**    ParseTOKENTYPE     is the data type used for minor type for terminal
**                       symbols.  Background: A "minor type" is a semantic
**                       value associated with a terminal or non-terminal
**                       symbols.  For example, for an "ID" terminal symbol,
**                       the minor type might be the name of the identifier.
**                       Each non-terminal can have a different minor type.
**                       Terminal symbols all have the same minor type, though.
**                       This macros defines the minor type for terminal
**                       symbols.
**    YYMINORTYPE        is the data type used for all minor types.
**                       This is typically a union of many types, one of
**                       which is ParseTOKENTYPE.  The entry in the union
**                       for terminal symbols is called "yy0".
**    YYSTACKDEPTH       is the maximum depth of the parser's stack.  If
**                       zero the stack is dynamically sized using realloc()
**    ParseARG_SDECL     A static variable declaration for the %extra_argument
**    ParseARG_PDECL     A parameter declaration for the %extra_argument
**    ParseARG_STORE     Code to store %extra_argument into yypParser
**    YYERRORSYMBOL      is the code number of the error symbol.  If not
**                       defined, then do no error processing.
**    YYNSTATE           the combined number of states.
**    YYNRULE            the number of rules in the grammar
**    YY_MAX_SHIFT       Maximum value for shift actions
**    YY_MIN_SHIFTREDUCE Minimum value for shift-reduce actions
**    YY_MAX_SHIFTREDUCE Maximum value for shift-reduce actions
**    YY_MIN_REDUCE      Maximum value for reduce actions
**    YY_ERROR_ACTION    The yy_action[] code for syntax error
**    YY_ACCEPT_ACTION   The yy_action[] code for accept
**    YY_NO_ACTION       The yy_action[] code for no-op
*/
#ifndef INTERFACE
# define INTERFACE 1
#endif
/************* Begin control #defines *****************************************/
%%
/************* End control #defines *******************************************/

/* Define the yytestcase() macro to be a no-op if is not already defined
** otherwise.
**
** Applications can choose to define yytestcase() in the %include section
** to a macro that can assist in verifying code coverage.  For production
** code the yytestcase() macro should be turned off.  But it is useful
** for testing.
*/
#ifndef yytestcase
# define yytestcase(X)
#endif


/* Next are the tables used to determine what action to take based on the
** current state and lookahead token.  These tables are used to implement
** functions that take a state number and lookahead value and return an
** action integer.
**
** Suppose the action integer is N.  Then the action is determined as
** follows
**
**   0 <= N <= YY_MAX_SHIFT             Shift N.  That is, push the lookahead
**                                      token onto the stack and goto state N.
**
**   N between YY_MIN_SHIFTREDUCE       Shift to an arbitrary state then
**     and YY_MAX_SHIFTREDUCE           reduce by rule N-YY_MIN_SHIFTREDUCE.
**
**   N between YY_MIN_REDUCE            Reduce by rule N-YY_MIN_REDUCE
**     and YY_MAX_REDUCE
**
**   N == YY_ERROR_ACTION               A syntax error has occurred.
**
**   N == YY_ACCEPT_ACTION              The parser accepts its input.
**
**   N == YY_NO_ACTION                  No such action.  Denotes unused
**                                      slots in the yy_action[] table.
**
** The action table is constructed as a single large table named yy_action[].
** Given state S and lookahead X, the action is computed as either:
**
**    (A)   N = yy_action[ yy_shift_ofst[S] + X ]
**    (B)   N = yy_default[S]
**
** The (A) formula is preferred.  The B formula is used instead if:
**    (1)  The yy_shift_ofst[S]+X value is out of range, or
**    (2)  yy_lookahead[yy_shift_ofst[S]+X] is not equal to X, or
**    (3)  yy_shift_ofst[S] equal YY_SHIFT_USE_DFLT.
** (Implementation note: YY_SHIFT_USE_DFLT is chosen so that
** YY_SHIFT_USE_DFLT+X will be out of range for all possible lookaheads X.
** Hence only tests (1) and (2) need to be evaluated.)
**
** The formulas above are for computing the action when the lookahead is
** a terminal symbol.  If the lookahead is a non-terminal (as occurs after
** a reduce action) then the yy_reduce_ofst[] array is used in place of
** the yy_shift_ofst[] array and YY_REDUCE_USE_DFLT is used in place of
** YY_SHIFT_USE_DFLT.
**
** The following are the tables generated in this section:
**
**  yy_action[]        A single table containing all actions.
**  yy_lookahead[]     A table containing the lookahead for each entry in
**                     yy_action.  Used to detect hash collisions.
**  yy_shift_ofst[]    For each state, the offset into yy_action for
**                     shifting terminals.
**  yy_reduce_ofst[]   For each state, the offset into yy_action for
**                     shifting non-terminals after a reduce.
**  yy_default[]       Default action for each state.
**
*********** Begin parsing tables **********************************************/
%%
/********** End of lemon-generated parsing tables *****************************/

/* The next table maps tokens (terminal symbols) into fallback tokens.
** If a construct like the following:
**
**      %fallback ID X Y Z.
**
** appears in the grammar, then ID becomes a fallback token for X, Y,
** and Z.  Whenever one of the tokens X, Y, or Z is input to the parser
** but it does not parse, the type of the token is changed to ID and
** the parse is retried before an error is thrown.
**
** This feature can be used, for example, to cause some keywords in a language
** to revert to identifiers if they keyword does not apply in the context where
** it appears.
*/
#ifdef YYFALLBACK
%%
#endif /* YYFALLBACK */

/* The following structure represents a single element of the
** parser's stack.  Information stored includes:
**
**   +  The state number for the parser at this level of the stack.
**
**   +  The value of the token stored at this level of the stack.
**      (In other words, the "major" token.)
**
**   +  The semantic value stored at this level of the stack.  This is
**      the information used by the action routines in the grammar.
**      It is sometimes called the "minor" token.
**
** After the "shift" half of a SHIFTREDUCE action, the stateno field
** actually contains the reduce action for the second half of the
** SHIFTREDUCE.
*/
#[derive(Default)]
struct yyStackEntry {
  stateno: YYACTIONTYPE,  /* The state-number, or reduce action in SHIFTREDUCE */
  major: YYCODETYPE,      /* The major token value.  This is the code
                         ** number for the token at this stack level */
  minor: YYMINORTYPE,     /* The user-supplied minor token value.  This
                         ** is the value of the token  */
}

/* The state of the parser is completely contained in an instance of
** the following structure */
pub struct yyParser {
  yyidx: isize,                  /* Index to top element of the stack */
#ifdef YYTRACKMAXSTACKDEPTH
  yyhwm: usize,                  /* High-water mark of the stack */
#endif
#ifndef YYNOERRORRECOVERY
  yyerrcnt: i32,                 /* Shifts left before out of the error */
#endif
  ParseARG_SDECL                 /* A place to hold %extra_argument */
#if YYSTACKDEPTH<=0
  yystack: Vec<yyStackEntry>,    /* The parser's stack */
#else
  yystack: [yyStackEntry; YYSTACKDEPTH],  /* The parser's stack */
#endif
}

#ifndef NDEBUG
use log::LogLevel::Debug;
static TARGET: &'static str = "Parse";
#endif /* NDEBUG */

#ifndef NDEBUG
#endif /* NDEBUG */

#ifndef NDEBUG
/* For tracing shifts, the names of all terminals and nonterminals
** are required.  The following table supplies these names */
%%
#endif /* NDEBUG */

#ifndef NDEBUG
/* For tracing reduce actions, the names of all rules are required.
*/
%%
#endif /* NDEBUG */


#if YYSTACKDEPTH<=0
/*
** Try to increase the size of the parser stack.  Return the number
** of errors.  Return 0 on success.
*/
impl yyParser {
    fn yyGrowStack(&mut self) -> bool {
        let capacity = self.yystack.capacity();
        let additional = capacity + 100;
        self.yystack.reserve(additional);
#ifndef NDEBUG
    debug!(target: TARGET, "Stack grows from {} to {} entries.",
            capacity, self.yystack.capacity());
#endif
        false
    }
}
#endif

/* Initialize a new parser.
*/
impl yyParser {
    pub fn new(
        ParseARG_PDECL               /* Optional %extra_argument parameter */
    ) -> yyParser {
        let p = yyParser {
  yyidx: 0,
#ifdef YYTRACKMAXSTACKDEPTH
  yyhwm: 0,
#endif
#if YYSTACKDEPTH<=0
  yystack: Vec::with_capacity(100),
#else
  yystack: [yyStackEntry::default(); YYSTACKDEPTH],
#endif
#ifndef YYNOERRORRECOVERY
  yyerrcnt: -1,
#endif
  ParseARG_STORE
      };
#if YYSTACKDEPTH<=0
  p.yystack.push(yyStackEntry::default());
#endif
      p
  }
}

/*
** Pop the parser's stack once.
*/
impl yyParser {
    fn yy_pop_parser_stack(&mut self){
        use std::mem::replace;
  let yytos = replace(&mut self.yystack[self.yyidx], yyStackEntry::default());
  self.yyidx -= 1;
#ifndef NDEBUG
  debug!(target: TARGET, "Popping {}",
    yyTokenName[yytos.major]);
#endif
  }
}

/*
** Clear all secondary memory allocations from the parser
*/
impl yyParser {
    pub fn ParseFinalize(&mut self){
  while( self.yyidx > 0 ) self.yy_pop_parser_stack();
    }
}

/*
** Return the peak depth of the stack for a parser.
*/
#ifdef YYTRACKMAXSTACKDEPTH
impl yyParser {
    pub fn ParseStackPeak(&self) -> usize {
  self.yyhwm
    }
}
#endif

/*
** Find the appropriate action for a parser given the terminal
** look-ahead token iLookAhead.
*/
impl yyParser {
    fn yy_find_shift_action(
  &self,                     /* The parser */
  iLookAhead: YYCODETYPE     /* The look-ahead token */
    ) -> YYACTIONTYPE {
  let mut i;
  let stateno = self.yystack[self.yyidx].stateno;

  if stateno>=YY_MIN_REDUCE { return stateno };
  assert!( stateno <= YY_SHIFT_COUNT );
  loop{
    i = yy_shift_ofst[stateno];
    assert!( iLookAhead!=YYNOCODE );
    i += iLookAhead;
    if i<0 || i>=YY_ACTTAB_COUNT || yy_lookahead[i]!=iLookAhead {
#ifdef YYFALLBACK
      let mut iFallback: YYCODETYPE;            /* Fallback token */
      if iLookAhead<yyFallback.len()
             && (iFallback = yyFallback[iLookAhead])!=0 {
#ifndef NDEBUG
        debug!(target: TARGET, "FALLBACK {} => {}",
           yyTokenName[iLookAhead], yyTokenName[iFallback]);
#endif
        assert_eq!( yyFallback[iFallback], 0 ); /* Fallback loop must terminate */
        iLookAhead = iFallback;
        continue;
      }
#endif
#ifdef YYWILDCARD
      {
        let j = i - iLookAhead + YYWILDCARD;
        if
#if YY_SHIFT_MIN+YYWILDCARD<0
          j>=0 &&
#endif
#if YY_SHIFT_MAX+YYWILDCARD>=YY_ACTTAB_COUNT
          j<YY_ACTTAB_COUNT &&
#endif
          yy_lookahead[j]==YYWILDCARD && iLookAhead>0
        {
#ifndef NDEBUG
          debug!(target: TARGET, "WILDCARD {} => {}",
             yyTokenName[iLookAhead],
             yyTokenName[YYWILDCARD]);
#endif /* NDEBUG */
          return yy_action[j];
        }
      }
#endif /* YYWILDCARD */
      return yy_default[stateno];
    }else{
      return yy_action[i];
    }
  }
    }
}

/*
** Find the appropriate action for a parser given the non-terminal
** look-ahead token iLookAhead.
*/
fn yy_find_reduce_action(
  stateno: YYACTIONTYPE,     /* Current state number */
  iLookAhead: YYCODETYPE     /* The look-ahead token */
    ) -> YYACTIONTYPE {
  let mut i;
#ifdef YYERRORSYMBOL
  if stateno>YY_REDUCE_COUNT {
    return yy_default[stateno];
  }
#else
  assert!( stateno<=YY_REDUCE_COUNT );
#endif
  i = yy_reduce_ofst[stateno];
  assert_ne!( i, YY_REDUCE_USE_DFLT );
  assert_ne!( iLookAhead, YYNOCODE );
  i += iLookAhead;
#ifdef YYERRORSYMBOL
  if i<0 || i>=YY_ACTTAB_COUNT || yy_lookahead[i]!=iLookAhead {
    return yy_default[stateno];
  }
#else
  assert!( i>=0 && i<YY_ACTTAB_COUNT );
  assert_eq!( yy_lookahead[i], iLookAhead );
#endif
  return yy_action[i];
}

/*
** The following routine is called if the stack overflows.
*/
impl yyParser {
    fn yyStackOverflow(&mut self){
#ifndef NDEBUG
   error!(target: TARGET, "Stack Overflow!");
#endif
   while( self.yyidx > 0 ) self.yy_pop_parser_stack();
   /* Here code is inserted which will execute if the parser
   ** stack every overflows */
/******** Begin %stack_overflow code ******************************************/
%%
/******** End %stack_overflow code ********************************************/
    }
}

/*
** Print tracing information for a SHIFT action
*/
#ifndef NDEBUG
impl yyParser {
    fn yyTraceShift(&self, yyNewState: YYACTIONTYPE){
  let yytos = self.yystack[self.yyidx];
  if yyNewState<YYNSTATE {
    debug!(target: TARGET, "Shift '{}', go to state {}",
       yyTokenName[yytos.major],
       yyNewState);
  }else{
    debug!(target: TARGET, "Shift '{}'",
       yyTokenName[yytos.major]);
  }
    }
}
#else
# define yyTraceShift(X,Y)
#endif

/*
** Perform a shift action.
*/
impl yyParser {
    fn yy_shift(
  &mut self,                    /* The parser to be shifted */
  yyNewState: YYACTIONTYPE,     /* The new state to shift in */
  yyMajor: YYCODETYPE,          /* The major token to shift in */
  yyMinor: ParseTOKENTYPE       /* The minor token to shift in */
    ){
  self.yyidx += 1;
#ifdef YYTRACKMAXSTACKDEPTH
  if self.yyidx>self.yyhwm {
    self.yyhwm += 1;
    assert_eq!( self.yyhwm, self.yyidx );
  }
#endif
#if YYSTACKDEPTH>0
  if self.yyidx>=YYSTACKDEPTH {
    self.yyidx -= 1;
    self.yyStackOverflow();
    return;
  }
#else
  if self.yyidx>= self.yystack.len() {
    if self.yyGrowStack() {
      self.yyidx -= 1;
      self.yyStackOverflow();
      return;
    }
  }
#endif
  if( yyNewState > YY_MAX_SHIFT ){
    yyNewState += YY_MIN_REDUCE - YY_MIN_SHIFTREDUCE;
  }
  let yytos = yyStackEntry {stateno: yyNewState, major: yyMajor, minor: YYMINORTYPE::YY0(yyMinor)};
#if YYSTACKDEPTH>0
  self.yystack[self.yyidx] = yytos;
#else
  self.yystack.push(yytos);
#endif
  self.yyTraceShift(yyNewState);
    }
}

/* The following table contains information about every rule that
** is used during the reduce.
*/
struct yyRuleInfoEntry {
  lhs: YYCODETYPE,  /* Symbol on the left-hand side of the rule */
  nrhs: i8,         /* Negative of the number of RHS symbols in the rule */
}
%%

/*
** Perform a reduce action and the shift that must immediately
** follow the reduce.
*/
impl yyParser {
    fn yy_reduce(
  &mut self,             /* The parser */
  yyruleno: usize        /* Number of the rule by which to reduce */
    ){
  let yygoto: YYCODETYPE;     /* The next state */
  let yyact: YYACTIONTYPE;    /* The next action */
  let yysize: i8;             /* Amount to pop the stack */
#ifndef NDEBUG
  if yyruleno<yyRuleName.len() {
    let yysize = yyRuleInfo[yyruleno].nrhs;
    debug!(target: TARGET, "Reduce [{}], go to state {}.",
      yyRuleName[yyruleno], self.yystack[self.yyidx+yysize].stateno);
  }
#endif /* NDEBUG */

  /* Check that the stack is large enough to grow by a single entry
  ** if the RHS of the rule is empty.  This ensures that there is room
  ** enough on the stack to push the LHS value */
  if yyRuleInfo[yyruleno].nrhs==0 {
#ifdef YYTRACKMAXSTACKDEPTH
    if self.yyidx>self.yyhwm {
      self.yyhwm += 1;
      assert_eq!( self.yyhwm, self.yyidx);
    }
#endif
#if YYSTACKDEPTH>0
    if self.yyidx>=YYSTACKDEPTH-1 {
      self.yyStackOverflow();
      return;
    }
#else
    if self.yyidx>=yystack.len()-1 {
      if self.yyGrowStack() {
        self.yyStackOverflow();
        return;
      }
    }
#endif
  }

  let mut yylhsminor = YYMINORTYPE::default();
  match yyruleno {
  /* Beginning here are the reduction cases.  A typical example
  ** follows:
  **   case 0:
  **  #line <lineno> <grammarfile>
  **     { ... }           // User supplied code
  **  #line <lineno> <thisfile>
  **     break;
  */
/********** Begin reduce actions **********************************************/
%%
/********** End reduce actions ************************************************/
  };
  assert!( yyruleno<yyRuleInfo.len() );
  yygoto = yyRuleInfo[yyruleno].lhs;
  yysize = yyRuleInfo[yyruleno].nrhs;
  yyact = yy_find_reduce_action(self.yystack[self.yyidx+yysize].stateno,yygoto);

  /* There are no SHIFTREDUCE actions on nonterminals because the table
  ** generator has simplified them to pure REDUCE actions. */
  assert!( !(yyact>YY_MAX_SHIFT && yyact<=YY_MAX_SHIFTREDUCE) );

  /* It is not possible for a REDUCE to be followed by an error */
  assert_ne!( yyact, YY_ERROR_ACTION );

  if yyact==YY_ACCEPT_ACTION {
    self.yyidx += yysize;
    self.yy_accept();
  }else{
    self.yyidx += yysize+1;
    let yymsp = self.yystack[self.yyidx];
    yymsp.stateno = yyact;
    yymsp.major = yygoto;
    self.yyTraceShift(yyact);
  }
    }
}

/*
** The following code executes when the parse fails
*/
#ifndef YYNOERRORRECOVERY
impl yyParser {
    fn yy_parse_failed(
  &mut self           /* The parser */
    ){
#ifndef NDEBUG
  error!(target: TARGET, "Fail!");
#endif
  while( self.yyidx > 0 ) self.yy_pop_parser_stack();
  /* Here code is inserted which will be executed whenever the
  ** parser fails */
/************ Begin %parse_failure code ***************************************/
%%
/************ End %parse_failure code *****************************************/
    }
}
#endif /* YYNOERRORRECOVERY */

/*
** The following code executes when a syntax error first occurs.
*/
impl yyParser {
    fn yy_syntax_error(
  &mut self,                     /* The parser */
  yymajor: YYCODETYPE,           /* The major type of the error token */
  yyminor: ParseTOKENTYPE        /* The minor type of the error token */
    ){
#define TOKEN yyminor
/************ Begin %syntax_error code ****************************************/
%%
/************ End %syntax_error code ******************************************/
    }
}

/*
** The following is executed when the parser accepts
*/
impl yyParser {
    fn yy_accept(
  &mut self           /* The parser */
    ){
#ifndef NDEBUG
  debug!(target: TARGET, "Accept!");
#endif
#ifndef YYNOERRORRECOVERY
  self.yyerrcnt = -1;
#endif
  assert_eq!( yyidx, 0 );
  /* Here code is inserted which will be executed whenever the
  ** parser accepts */
/*********** Begin %parse_accept code *****************************************/
%%
/*********** End %parse_accept code *******************************************/
    }
}

/* The main parser program.
** The first argument is a pointer to a structure obtained from
** "ParseAlloc" which describes the current state of the parser.
** The second argument is the major token number.  The third is
** the minor token.  The fourth optional argument is whatever the
** user wants (and specified in the grammar) and is available for
** use by the action routines.
**
** Inputs:
** <ul>
** <li> A pointer to the parser (an opaque structure.)
** <li> The major token number.
** <li> The minor token number.
** <li> An option argument of a grammar-specified type.
** </ul>
**
** Outputs:
** None.
*/
impl yyParser {
    fn Parse(
  &mut self,                   /* The parser */
  yymajor: YYCODETYPE,         /* The major token code number */
  yyminor: ParseTOKENTYPE      /* The value for the token */
    ){
  let mut yyact: YYACTIONTYPE;   /* The parser action. */
#if !defined(YYERRORSYMBOL) && !defined(YYNOERRORRECOVERY)
  let mut yyendofinput: bool;     /* True if we are at the end of input */
#endif
#ifdef YYERRORSYMBOL
  let mut yyerrorhit: bool = false;   /* True if yymajor has invoked an error */
#endif

  //assert_ne!( self.yystack[self.yyidx], null );
#if !defined(YYERRORSYMBOL) && !defined(YYNOERRORRECOVERY)
  yyendofinput = (yymajor==0);
#endif

#ifndef NDEBUG
  debug!(target: TARGET, "Input '{}'",yyTokenName[yymajor]);
#endif

  do{
    yyact = self.yy_find_shift_action(yymajor);
    if yyact <= YY_MAX_SHIFTREDUCE {
      self.yy_shift(yyact,yymajor,yyminor);
#ifndef YYNOERRORRECOVERY
      self.yyerrcnt -= 1;
#endif
      yymajor = YYNOCODE;
    }else if yyact <= YY_MAX_REDUCE {
      self.yy_reduce(yyact-YY_MIN_REDUCE);
    }else{
      assert_eq!( yyact, YY_ERROR_ACTION );
#ifdef YYERRORSYMBOL
      let yymx;
#endif
#ifndef NDEBUG
      debug!(target: TARGET, "Syntax Error!");
#endif
#ifdef YYERRORSYMBOL
      /* A syntax error has occurred.
      ** The response to an error depends upon whether or not the
      ** grammar defines an error token "ERROR".
      **
      ** This is what we do if the grammar does define ERROR:
      **
      **  * Call the %syntax_error function.
      **
      **  * Begin popping the stack until we enter a state where
      **    it is legal to shift the error symbol, then shift
      **    the error symbol.
      **
      **  * Set the error count to three.
      **
      **  * Begin accepting and shifting new tokens.  No new error
      **    processing will occur until three tokens have been
      **    shifted successfully.
      **
      */
      if self.yyerrcnt<0 {
        self.yy_syntax_error(yymajor,yyminor);
      }
      yymx = self.yystack[self.yyidx].major;
      if yymx==YYERRORSYMBOL || yyerrorhit {
#ifndef NDEBUG
        debug!(target: TARGET, "Discard input token {}",
           yyTokenName[yymajor]);
#endif
        yymajor = YYNOCODE;
      }else{
        while self.yyidx >= 0
            && yymx != YYERRORSYMBOL
            && (yyact = yy_find_reduce_action(
                        self.yystack[self.yyidx].stateno,
                        YYERRORSYMBOL)) >= YY_MIN_REDUCE
        {
          self.yy_pop_parser_stack();
        }
        if yyidx < 0 || yymajor==0 {
          self.yy_parse_failed();
#ifndef YYNOERRORRECOVERY
          self.yyerrcnt = -1;
#endif
          yymajor = YYNOCODE;
        }else if yymx!=YYERRORSYMBOL {
          self.yy_shift(yyact,YYERRORSYMBOL,yyminor);
        }
      }
      self.yyerrcnt = 3;
      yyerrorhit = true;
#elif defined(YYNOERRORRECOVERY)
      /* If the YYNOERRORRECOVERY macro is defined, then do not attempt to
      ** do any kind of error recovery.  Instead, simply invoke the syntax
      ** error routine and continue going as if nothing had happened.
      **
      ** Applications can set this macro (for example inside %include) if
      ** they intend to abandon the parse upon the first syntax error seen.
      */
      self.yy_syntax_error(yymajor, yyminor);
      yymajor = YYNOCODE;

#else  /* YYERRORSYMBOL is not defined */
      /* This is what we do if the grammar does not define ERROR:
      **
      **  * Report an error message, and throw away the input token.
      **
      **  * If the input token is $, then fail the parse.
      **
      ** As before, subsequent error messages are suppressed until
      ** three input tokens have been successfully shifted.
      */
      if self.yyerrcnt<=0 {
        self.yy_syntax_error(yymajor, yyminor);
      }
      self.yyerrcnt = 3;
      if yyendofinput {
        self.yy_parse_failed();
#ifndef YYNOERRORRECOVERY
        self.yyerrcnt = -1;
#endif
      }
      yymajor = YYNOCODE;
#endif
    }
  }while( yymajor!=YYNOCODE && self.yyidx>0 );
#ifndef NDEBUG
  if log_enabled!(target: TARGET, Debug) {
    let mut cDiv = '[';
    let mut msg = String::new();
    for entry in self.yystack[1...self.yyidx] {
      msg.push(cDiv);
      msg.push_str(yyTokenName[entry.major]);
      cDiv = ' ';
    }
    debug!(target: TARGET, "Return Stack=[{}]", msg);
  }
#endif
  return;
    }
}
