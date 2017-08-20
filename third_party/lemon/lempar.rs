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
#![feature(untagged_unions)]
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
//#ifndef yytestcase
//# define yytestcase(X)
//#endif


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
%%

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
#[allow(non_camel_case_types)]
#[derive(Default)]
pub struct yyStackEntry {
    stateno: YYACTIONTYPE, /* The state-number, or reduce action in SHIFTREDUCE */
    major: YYCODETYPE,     /* The major token value.  This is the code
                            ** number for the token at this stack level */
    minor: YYMINORTYPE,    /* The user-supplied minor token value.  This
                            ** is the value of the token  */
}

use smallvec::SmallVec;

/* The state of the parser is completely contained in an instance of
** the following structure */
#[allow(non_camel_case_types)]
pub struct yyParser {
    yyidx: usize, /* Index to top element of the stack */
    #[cfg(feature = "YYTRACKMAXSTACKDEPTH")]
    yyhwm: usize, /* High-water mark of the stack */
    #[cfg(not(feature = "YYNOERRORRECOVERY"))]
    yyerrcnt: i32, /* Shifts left before out of the error */
%%                               /* A place to hold %extra_argument */
    yystack: SmallVec<[yyStackEntry; YYSTACKDEPTH]>, /* The parser's stack */
}

use std::ops::Neg;
impl yyParser {
    fn shift(&self, shift: i8) -> usize {
        if shift == 0 {
            self.yyidx
        } else if shift > 0 {
            self.yyidx + shift as usize
        } else {
            self.yyidx.checked_sub(shift.neg() as usize).unwrap()
        }
    }

    fn yyidx_shift(&mut self, shift: i8) {
        if shift == 0 {
        } else if shift > 0 {
            self.yyidx += shift as usize;
        } else {
            self.yyidx = self.yyidx.checked_sub(shift.neg() as usize).unwrap()
        }
    }

    fn get_and_reset(&mut self, shift: i8) -> yyStackEntry {
         use std::mem::replace;
        let idx = self.shift(shift);
        replace(&mut self.yystack[idx], yyStackEntry::default())
    }
}

use std::ops::{Index, IndexMut};
impl Index<i8> for yyParser {
    type Output = yyStackEntry;

    fn index(&self, shift: i8) -> &yyStackEntry {
        let idx = self.shift(shift);
        &self.yystack[idx]
    }
}
impl IndexMut<i8> for yyParser {
    fn index_mut(&mut self, shift: i8) -> &mut yyStackEntry {
        let idx = self.shift(shift);
        &mut self.yystack[idx]
    }
}

#[cfg(not(feature = "NDEBUG"))]
use log::LogLevel::Debug;
#[cfg(not(feature = "NDEBUG"))]
static TARGET: &'static str = "Parse";


/* For tracing shifts, the names of all terminals and nonterminals
** are required.  The following table supplies these names */
#[cfg(not(feature = "NDEBUG"))]
%%

/* For tracing reduce actions, the names of all rules are required.
*/
#[cfg(not(feature = "NDEBUG"))]
%%

/*
** Try to increase the size of the parser stack.  Return the number
** of errors.  Return 0 on success.
*/
impl yyParser {
    fn yy_grow_stack_if_needed(&mut self) -> bool {
        if self.yyidx >= self.yystack.capacity() {
            if self.yyGrowStack() {
                self.yyidx -= 1;
                self.yyStackOverflow();
                return true;
            }
        }
        false
    }
    fn yy_grow_stack_for_push(&mut self) -> bool {
        if self.yyidx >= self.yystack.capacity() - 1 {
            if self.yyGrowStack() {
                self.yyStackOverflow();
                return true;
            }
        }
        false
    }
    #[allow(non_snake_case)]
    fn yyGrowStack(&mut self) -> bool {
        let capacity = self.yystack.capacity();
        let additional = capacity + 100;
        self.yystack.reserve(additional);
        if cfg!(not(feature = "NDEBUG")) {
            debug!(
                target: TARGET,
                "Stack grows from {} to {} entries.",
                capacity,
                self.yystack.capacity()
            );
        }
        false
    }
    fn push(&mut self, entry: yyStackEntry) {
        if self.yyidx == self.yystack.len() {
            self.yystack.push(entry);
        } else {
            self.yystack[self.yyidx] = entry;
        }
    }
}

/* Initialize a new parser.
*/
impl yyParser {
    pub fn new(
%%               /* Optional %extra_argument parameter */
    ) -> yyParser {
        let mut p = yyParser {
            yyidx: 0,
            #[cfg(feature = "YYTRACKMAXSTACKDEPTH")]
            yyhwm: 0,
            yystack: SmallVec::new(),
            #[cfg(not(feature = "YYNOERRORRECOVERY"))]
            yyerrcnt: -1,
%%
        };
        p.push(yyStackEntry::default());
        p
    }
}

/*
** Pop the parser's stack once.
*/
impl yyParser {
    fn yy_pop_parser_stack(&mut self) {
        let yytos = self.yystack.pop().unwrap();
        self.yyidx -= 1;
        assert_eq!(self.yyidx+1, self.yystack.len());
        if cfg!(not(feature = "NDEBUG")) {
            debug!(
                target: TARGET,
                "Popping {}",
                yyTokenName[yytos.major as usize]
            );
        }
    }
}

/*
** Clear all secondary memory allocations from the parser
*/
impl yyParser {
    #[allow(non_snake_case)]
    pub fn ParseFinalize(&mut self) {
        while self.yyidx > 0 {
            self.yy_pop_parser_stack();
        }
    }
}

/*
** Return the peak depth of the stack for a parser.
*/
#[cfg(feature = "YYTRACKMAXSTACKDEPTH")]
impl yyParser {
    #[allow(non_snake_case)]
    pub fn ParseStackPeak(&self) -> usize {
        self.yyhwm
    }
    fn yyhwm_incr(&mut self) {
        if self.yyidx > self.yyhwm {
            self.yyhwm += 1;
            assert_eq!(self.yyhwm, self.yyidx);
        }
    }
}
#[cfg(not(feature = "YYTRACKMAXSTACKDEPTH"))]
impl yyParser {
    #[inline]
    fn yyhwm_incr(&mut self) {}
}

/*
** Find the appropriate action for a parser given the terminal
** look-ahead token iLookAhead.
*/
impl yyParser {
    #[allow(non_snake_case)]
    fn yy_find_shift_action(
        &self,
        iLookAhead: YYCODETYPE, /* The look-ahead token */
    ) -> YYACTIONTYPE {
        let mut iLookAhead = iLookAhead;
        let mut i: i32;
        let stateno = self[0].stateno;

        if stateno >= YY_MIN_REDUCE {
            return stateno;
        }
        assert!(stateno <= YY_SHIFT_COUNT);
        loop {
            i = yy_shift_ofst[stateno as usize] as i32;
            assert_ne!(iLookAhead, YYNOCODE);
            i += iLookAhead as i32;
            if i < 0 || i >= (YY_ACTTAB_COUNT as i32) || yy_lookahead[i as usize] != iLookAhead {
                if YYFALLBACK {
                    let mut iFallback: YYCODETYPE = 0; /* Fallback token */
                    if (iLookAhead as usize) < yyFallback.len() && {
                        iFallback = yyFallback[iLookAhead as usize];
                        iFallback
                    } != 0
                    {
                        if cfg!(not(feature = "NDEBUG")) {
                            debug!(
                                target: TARGET,
                                "FALLBACK {} => {}",
                                yyTokenName[iLookAhead as usize],
                                yyTokenName[iFallback as usize]
                            );
                        }
                        assert_eq!(yyFallback[iFallback as usize], 0); /* Fallback loop must terminate */
                        iLookAhead = iFallback;
                        continue;
                    }
                }
                if YYWILDCARD > 0 {
                    let j = i - (iLookAhead + YYWILDCARD) as i32;
                    if (YY_SHIFT_MIN + YYWILDCARD >= 0 || j >= 0) &&
                        (YY_SHIFT_MAX + YYWILDCARD < YY_ACTTAB_COUNT || j < YY_ACTTAB_COUNT as i32) &&
                        yy_lookahead[j as usize] == YYWILDCARD &&
                        iLookAhead > 0
                    {
                        if cfg!(not(feature = "NDEBUG")) {
                            debug!(
                                target: TARGET,
                                "WILDCARD {} => {}",
                                yyTokenName[iLookAhead as usize],
                                yyTokenName[YYWILDCARD as usize]
                            );
                        }
                        return yy_action[j as usize];
                    }
                } /* YYWILDCARD */
                return yy_default[stateno as usize];
            } else {
                return yy_action[i as usize];
            }
        }
    }
}

/*
** Find the appropriate action for a parser given the non-terminal
** look-ahead token iLookAhead.
*/
#[allow(non_snake_case)]
fn yy_find_reduce_action(
    stateno: YYACTIONTYPE,  /* Current state number */
    iLookAhead: YYCODETYPE, /* The look-ahead token */
) -> YYACTIONTYPE {
    let mut i: i32;
    if YYERRORSYMBOL > 0 {
        if stateno > YY_REDUCE_COUNT {
            return yy_default[stateno as usize];
        }
    } else {
        assert!(stateno <= YY_REDUCE_COUNT);
    }
    i = yy_reduce_ofst[stateno as usize] as i32;
    assert_ne!(i, YY_REDUCE_USE_DFLT as i32);
    assert_ne!(iLookAhead, YYNOCODE);
    i += iLookAhead as i32;
    if YYERRORSYMBOL > 0 {
        if i < 0 || i >= (YY_ACTTAB_COUNT as i32) || yy_lookahead[i as usize] != iLookAhead {
            return yy_default[stateno as usize];
        }
    } else {
        assert!(i >= 0 && i < (YY_ACTTAB_COUNT as i32));
        assert_eq!(yy_lookahead[i as usize], iLookAhead);
    }
    yy_action[i as usize]
}

/*
** The following routine is called if the stack overflows.
*/
impl yyParser {
    #[allow(non_snake_case)]
    fn yyStackOverflow(&mut self) {
        if cfg!(not(feature = "NDEBUG")) {
            error!(target: TARGET, "Stack Overflow!");
        }
        while self.yyidx > 0 {
            self.yy_pop_parser_stack();
        }
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
impl yyParser {
    #[allow(non_snake_case)]
    fn yyTraceShift(&self, yyNewState: YYACTIONTYPE) {
        if cfg!(not(feature = "NDEBUG")) {
            let yytos = &self[0];
            if yyNewState < YYNSTATE {
                debug!(
                    target: TARGET,
                    "Shift '{}', go to state {}",
                    yyTokenName[yytos.major as usize],
                    yyNewState
                );
            } else {
                debug!(
                    target: TARGET,
                    "Shift '{}'",
                    yyTokenName[yytos.major as usize]
                );
            }
        }
    }
}

/*
** Perform a shift action.
*/
impl yyParser {
    #[allow(non_snake_case)]
    fn yy_shift(
        &mut self,
        yyNewState: YYACTIONTYPE, /* The new state to shift in */
        yyMajor: YYCODETYPE,      /* The major token to shift in */
        yyMinor: ParseTOKENTYPE,  /* The minor token to shift in */
    ) {
        let mut yyNewState = yyNewState;
        self.yyidx_shift(1);
        self.yy_grow_stack_if_needed();
        if yyNewState > YY_MAX_SHIFT {
            yyNewState += YY_MIN_REDUCE - YY_MIN_SHIFTREDUCE;
        }
        let yytos = yyStackEntry {
            stateno: yyNewState,
            major: yyMajor,
            minor: YYMINORTYPE { yy0: yyMinor },
        };
        self.push(yytos);
        self.yyTraceShift(yyNewState);
    }
}

/* The following table contains information about every rule that
** is used during the reduce.
*/
#[allow(non_camel_case_types)]
struct yyRuleInfoEntry {
    lhs: YYCODETYPE, /* Symbol on the left-hand side of the rule */
    nrhs: i8,        /* Negative of the number of RHS symbols in the rule */
}
%%

/*
** Perform a reduce action and the shift that must immediately
** follow the reduce.
*/
impl yyParser {
    fn yy_reduce(
        &mut self,
        yyruleno: YYACTIONTYPE, /* Number of the rule by which to reduce */
    ) {
        let yygoto: YYCODETYPE; /* The next state */
        let yyact: YYACTIONTYPE; /* The next action */
        let yysize: i8; /* Amount to pop the stack */
        if cfg!(not(feature = "NDEBUG")) && (yyruleno as usize) < yyRuleName.len() {
            let yysize = yyRuleInfo[yyruleno as usize].nrhs;
            debug!(
                target: TARGET,
                "Reduce [{}], go to state {}.",
                yyRuleName[yyruleno as usize],
                self[yysize].stateno
            );
        }

        /* Check that the stack is large enough to grow by a single entry
         ** if the RHS of the rule is empty.  This ensures that there is room
         ** enough on the stack to push the LHS value */
        if yyRuleInfo[yyruleno as usize].nrhs == 0 {
            self.yyhwm_incr();
            self.yy_grow_stack_for_push();
        }

        let mut yylhsminor = YYMINORTYPE::default();
        unsafe {
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
        }
        assert!((yyruleno as usize) < yyRuleInfo.len());
        yygoto = yyRuleInfo[yyruleno as usize].lhs;
        yysize = yyRuleInfo[yyruleno as usize].nrhs;
        yyact = yy_find_reduce_action(self[yysize].stateno, yygoto);

        /* There are no SHIFTREDUCE actions on nonterminals because the table
         ** generator has simplified them to pure REDUCE actions. */
        assert!(!(yyact > YY_MAX_SHIFT && yyact <= YY_MAX_SHIFTREDUCE));

        /* It is not possible for a REDUCE to be followed by an error */
        assert_ne!(yyact, YY_ERROR_ACTION);

        if yyact == YY_ACCEPT_ACTION {
            self.yyidx_shift(yysize);
            self.yy_accept();
        } else {
            self.yyidx_shift(yysize + 1);
            {
                let yymsp = &mut self[0];
                yymsp.stateno = yyact;
                yymsp.major = yygoto;
            }
            self.yyTraceShift(yyact);
        }
    }
}

/*
** The following code executes when the parse fails
*/
#[cfg(not(feature = "YYNOERRORRECOVERY"))]
impl yyParser {
    fn yy_parse_failed(&mut self) {
        if cfg!(not(feature = "NDEBUG")) {
            error!(target: TARGET, "Fail!");
        }
        while self.yyidx > 0 {
            self.yy_pop_parser_stack();
        }
        /* Here code is inserted which will be executed whenever the
         ** parser fails */
        /************ Begin %parse_failure code ***************************************/
%%
        /************ End %parse_failure code *****************************************/
    }
}

/*
** The following code executes when a syntax error first occurs.
*/
impl yyParser {
    fn yy_syntax_error(
        &mut self,
        _yymajor: YYCODETYPE,    /* The major type of the error token */
        yyminor: ParseTOKENTYPE, /* The minor type of the error token */
    ) {
        /************ Begin %syntax_error code ****************************************/
%%
        /************ End %syntax_error code ******************************************/
    }
}

/*
** The following is executed when the parser accepts
*/
impl yyParser {
    fn yy_accept(&mut self) {
        if cfg!(not(feature = "NDEBUG")) {
            debug!(target: TARGET, "Accept!");
        }
        if cfg!(not(feature = "YYNOERRORRECOVERY")) {
            self.yyerrcnt = -1;
        }
        assert_eq!(self.yyidx, 0);
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
    #[allow(non_snake_case)]
    fn Parse(
        &mut self,
        yymajor: YYCODETYPE,     /* The major token code number */
        yyminor: ParseTOKENTYPE, /* The value for the token */
    ) {
        let mut yymajor = yymajor;
        let mut yyact: YYACTIONTYPE; /* The parser action. */
        //#[cfg(all(not(feature = "YYERRORSYMBOL"), not(feature = "YYNOERRORRECOVERY")))]
        let mut yyendofinput: bool = false; /* True if we are at the end of input */
        //#[cfg(feature = "YYERRORSYMBOL")]
        let mut yyerrorhit: bool = false; /* True if yymajor has invoked an error */

        //assert_ne!( self[0], null );
        if YYERRORSYMBOL == 0 && cfg!(not(feature = "YYNOERRORRECOVERY")) {
            yyendofinput = yymajor == 0;
        }

        if cfg!(not(feature = "NDEBUG")) {
            debug!(target: TARGET, "Input '{}'", yyTokenName[yymajor as usize]);
        }

        loop {
            yyact = self.yy_find_shift_action(yymajor);
            if yyact <= YY_MAX_SHIFTREDUCE {
                self.yy_shift(yyact, yymajor, yyminor);
                if cfg!(not(feature = "YYNOERRORRECOVERY")) {
                    self.yyerrcnt -= 1;
                }
                yymajor = YYNOCODE;
            } else if yyact <= YY_MAX_REDUCE {
                self.yy_reduce(yyact - YY_MIN_REDUCE);
            } else {
                assert_eq!(yyact, YY_ERROR_ACTION);
                if cfg!(not(feature = "NDEBUG")) {
                    debug!(target: TARGET, "Syntax Error!");
                }
                if YYERRORSYMBOL > 0 {
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
                    if self.yyerrcnt < 0 {
                        self.yy_syntax_error(yymajor, yyminor);
                    }
                    let yymx = self[0].major;
                    if yymx == YYERRORSYMBOL || yyerrorhit {
                        if cfg!(not(feature = "NDEBUG")) {
                            debug!(
                                target: TARGET,
                                "Discard input token {}",
                                yyTokenName[yymajor as usize]
                            );
                        }
                        yymajor = YYNOCODE;
                    } else {
                        while self.yyidx >= 0 && yymx != YYERRORSYMBOL && {
                            yyact = yy_find_reduce_action(self[0].stateno, YYERRORSYMBOL);
                            yyact
                        } >= YY_MIN_REDUCE
                        {
                            self.yy_pop_parser_stack();
                        }
                        if self.yyidx < 0 || yymajor == 0 {
                            self.yy_parse_failed();
                            if cfg!(not(feature = "YYNOERRORRECOVERY")) {
                                self.yyerrcnt = -1;
                            }
                            yymajor = YYNOCODE;
                        } else if yymx != YYERRORSYMBOL {
                            self.yy_shift(yyact, YYERRORSYMBOL, yyminor);
                        }
                    }
                    self.yyerrcnt = 3;
                    yyerrorhit = true;
                } else if cfg!(feature = "YYNOERRORRECOVERY") {
                    /* If the YYNOERRORRECOVERY macro is defined, then do not attempt to
                     ** do any kind of error recovery.  Instead, simply invoke the syntax
                     ** error routine and continue going as if nothing had happened.
                     **
                     ** Applications can set this macro (for example inside %include) if
                     ** they intend to abandon the parse upon the first syntax error seen.
                     */
                    self.yy_syntax_error(yymajor, yyminor);
                    yymajor = YYNOCODE;

                } else {
                    /* YYERRORSYMBOL is not defined */
                    /* This is what we do if the grammar does not define ERROR:
                     **
                     **  * Report an error message, and throw away the input token.
                     **
                     **  * If the input token is $, then fail the parse.
                     **
                     ** As before, subsequent error messages are suppressed until
                     ** three input tokens have been successfully shifted.
                     */
                    if self.yyerrcnt <= 0 {
                        self.yy_syntax_error(yymajor, yyminor);
                    }
                    self.yyerrcnt = 3;
                    if yyendofinput {
                        self.yy_parse_failed();
                        if cfg!(not(feature = "YYNOERRORRECOVERY")) {
                            self.yyerrcnt = -1;
                        }
                    }
                    yymajor = YYNOCODE;
                }
            }
            if yymajor == YYNOCODE || self.yyidx <= 0 {
                break;
            }
        }
        if cfg!(not(feature = "NDEBUG")) {
            if log_enabled!(target: TARGET, Debug) {
                let msg = self.yystack[1..self.yyidx + 1].iter()
                    .map(|entry| yyTokenName[entry.major as usize])
                    .collect::<Vec<&str>>().join(" ");
                debug!(target: TARGET, "Return Stack=[{}]", msg);
            }
        }
        return;
    }
}
