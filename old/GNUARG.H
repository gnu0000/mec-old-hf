/*
 * GnuArg.h
 *
 * (c) 1993-1994 Info Tech.
 * Craig Fitzgerald
 *
 * Command line argument parsing library
 *
 * -------------------------------------------------------------------------
 * | Setup Functions                                //                     |
 * | -------------------------                      //                     |
 * | USHORT ArgBuildBlk (PSZ pszArgDef);            // init fn             |
 * | USHORT ArgFillBlk (PSZ argv[]);                // for cmd line        |
 * | USHORT ArgFillBlk2 (PSZ pszArgStr);            // for env strings etc.|
 * |                                                //                     |
 * | Query Functions                                //                     |
 * | -------------------------                      //                     |
 * | USHORT ArgIs (PSZ pszArg);                     // # times entered     |
 * | PSZ    ArgGet (PSZ pszArg, USHORT uIndex);     // getarg value        |
 * | USHORT ArgGetIndex (PSZ pszArg, USHORT uIndex);// position in cmd line|
 * | USHORT ArgEnum (pszArg, pszVal, USHORT i);     // get i-th argument   |
 * |                                                //                     |
 * | Error Functions                                //                     |
 * | -------------------------                      //                     |
 * | USHORT ArgIsErr (void);                        // was there an error? |
 * | PSZ    ArgGetErr (void);                       // get error string    |
 * | void   ArgDump(void);                          // for debug           |
 * -------------------------------------------------------------------------
 *
 *
 * GNUARG provides a means to easily access your command line
 * arguments.  To use this module you need to perform 3 steps
 * (in order):
 *
 *  1> Create an Argument Block (using ArgBuildBlk)
 *
 *  2> Fill the argument Block (ArgFillBlk and/or ArgFillBlk2)
 *
 *  3> Query Info about the parameters (ArgIs, ArgGet, ArgGetIndex)
 *
 *  See individual Fn descriptions for their use.
 */


/* This function sets up what parameters are valid and how they
 * may be specified. This is done in an Argument Definition string
 * Its form is one or more of this: (separated by spaces)
 *
 *  *^PName@
 *  ||  |  |
 *  ||  |  | Parameter value specifier
 *  ||  |  | ------------------------
 *  ||  |    - (space) Parameter has no value
 *  ||  |  - - Parameter has no value and is 'combinable'
 *  ||  |  $ - Parameter value is preceeded by whitespace
 *  ||  |  @ - Parameter value is not preceeded by whitespace
 *  ||  |  = - Parameter value is preceeded by = sign
 *  ||  |  : - Parameter value is preceeded by : sign
 *  ||  |  % - Parameter value is type @ $ = or :, all ok
 *  ||  |  ? - Parameter may have a value (if val, type : or =)
 *  ||  |  # - like % but allow val to start with '-' or '/'
 *  ||  |  & - like = or : but allow val to start with '-' or '/'
 *  ||  |
 *  ||  Parameter name to look for
 *  ||
 *  |Case Insensitivity Flag, Leave out to be Case Sensitive
 *  |
 *  Minimal Matching Flag, leave out for exact match only
 *
 *
 * 'combinable' means that multiple options may immediatly follow
 * one another. for example, having -a and -b specified as -ab.
 *
 * EXAMPLES:
 *
 *  ArgBuildBlk ("^a- ^b- ^c- ^x- ^y- ^z-")
 *     several simple combinable arguments. They are all case Insensitive
 *     and have no values.  This would accept a command line like this:
 *        -aBc -x -Y /z /azy
 *
 *  ArgBuildBlk ("*^XSize$ *^YSize$")
 *     these are minimal matching, case Insensitive, and have values.
 *     The following command line parameters would be legal:
 *        -XSize 100 -YSize 100
 *        -X 100 -Y 100 -xs 200
 *        -y 100 -Xs 100
 *
 *  ArgBuildBlk ("*^XSize% *^YSize%")
 *     Like above but would also allow:
 *        -X100 -y=100 -xs = 125 -x:123 -x :456
 *
 *  a nonzero return signifies an error, see ArgGetErr to see error string.
 */
USHORT ArgBuildBlk (PSZ pszArgDef);



/*
 * This function you just call with your argv and it will add the
 * command line parameters to the argument block.
 *
 *  a nonzero return signifies an error, see ArgGetErr to see error string
 */
USHORT ArgFillBlk (PSZ argv[]);



/*
 * This function is added so you may add additional parameters, say from
 * an enviroment variable or a file. Input is a string.
 *
 *  a nonzero return signifies an error, see ArgGetErr to see error string
 */
USHORT ArgFillBlk2 (PSZ pszArgStr);



/*
 * Use this function to see if an argument has been entered.
 * Use the full argument name as used in ArgBuildBlk. (i.e. no
 * minimal match string name) but case is not important.
 * If you use NULL for pszArg, this will return the number of
 * 'free' arguments.  'free' arguments are arguments that are not
 * preceeded by a switched (- or /) character
 *
 * EXAMPLE:  i = ArgIs("XSize");
 *
 * The return value is the number of times it has been specified.
 * a 0 return may also signify an error.
 */
USHORT ArgIs (PSZ pszArg);



/*
 * This will retrieve values from parameters that have values.
 * pszArg is the arg whose value you want, and uIndex is the
 *  instance you want (0 based)
 * If you use NULL for pszArg, this will return a 'free' parameter.
 * If you use 0xFFFF for uIndex, the last instance will be returned,
 *  provided there is at least one.
 *
 * EXAMPLE: your code has: ArgBuildBlk ("*^XSize$ *^YSize$")
 *          cmd line is:   -Y 5 -X 0 -Y 6 -X 50 -X 150 -X 300 -X 440 jim joe
 *          then:          pszVal = ArgGet ("XSize", 2);
 *               pszVal would be "150"
 *          and:           pszVal = ArgGet (NULL, 1);
 *               pszVal would be "jim"
 *          and:           pszVal = ArgGet ("XSize", 0xFFFF);
 *               pszVal would be "440"
 *
 * a NULL return signifies an error, see ArgGetErr to see error
 */
PSZ ArgGet (PSZ pszArg, USHORT uIndex);


/*
 * all parameters encountered are numbered and stored sequentially
 * starting from 0. This value has nothing to do with its argc
 * position (mainly to do with combinable params & values)
 * use this fn like the one above.
 *
 * EXAMPLE:  if setup is like above example, then
 *            uIndex = ArgGet ("XSize", 2);
 *            uIndex == 4
 *
 * a 0xFFFF return signifies an error.
 */
USHORT ArgGetIndex (PSZ pszArg, USHORT uIndex);



/*
 * This will return the parameters in the order entered.
 * pass the uIndex of the parameter to receive (0 based)
 * pszArg will contain the argument (unless ptr is null)
 * pszVal will contain the argument value (unless ptr is null)
 * Free arguments will have no pszVal.
 *
 * a return of 0 means uIndex > count of parameters
 * a return of 1 means a regular parameter
 * a return of 2 means parameter is of type 'free'
 */
USHORT ArgEnum (PSZ pszArg, PSZ pszVal, USHORT uIndex);



/*
 * returns current state
 * 0 means no error.
 */
USHORT ArgIsErr (void);


/*
 * if in an error state, calling this will
 * give a descriptive error string
 */
PSZ ArgGetErr (void);


/*
 * for debug
 * this prints all info about all parameters
 */
void ArgDump(void);

