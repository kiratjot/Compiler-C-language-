/*
* File Name: scanner.c
* Compiler: MS Visual Studio 2015 Community
* Author: Gurkirat Singh 040876315, Shubham Ujinwal 040885893
* Course: CST 8152  Compilers
* Lab Section: 12
* Assignment: 2
* Date: 8 November 2018
* Professor: Sv. Ranev
* Purpose: Functions implementing a Lexical Analyzer (Scanner)
* Function list: scanner_init();
*				 malar_next_token();
*			     get_next_state();
*			     char_class();
*				 aa_func02();
*				 aa_func03();
*				 aa_func05();
*				 aa_func08();
*				 aa_func10();
*				 aa_func11();
*				 aa_func12();
*				 iskeyword();
*/

/* The #define _CRT_SECURE_NO_WARNINGS should be used in MS Visual Studio projects
* to suppress the warnings about using "unsafe" functions like fopen()
* and standard sting library functions defined in string.h.
* The define does not have any effect in Borland compiler projects.
*/
#define _CRT_SECURE_NO_WARNINGS

#include <stdio.h>   /* standard input / output */
#include <ctype.h>   /* conversion functions */
#include <stdlib.h>  /* standard library functions and constants */
#include <string.h>  /* string functions */
#include <limits.h>  /* integer types constants */
#include <float.h>   /* floating-point types constants */

/*#define NDEBUG        to suppress assert() call */
#include <assert.h>  /* assert() prototype */

/* project header files */
#include "buffer.h"
#include "token.h"
#include "table.h"

#define DEBUG  /* for conditional processing */
#undef  DEBUG

/* Global objects - variables */
/* This buffer is used as a repository for string literals.
It is defined in platy_st.c */
extern Buffer * str_LTBL = NULL; /*String literal table */
int line; /* current line number of the source code */
extern int scerrnum;     /* defined in platy_st.c - run-time error number */

						 /* Local(file) global objects - variables */
static Buffer *lex_buf;/*pointer to temporary lexeme buffer*/
static pBuffer sc_buf; /*pointer to input source buffer*/

					   /* scanner.c static(local) function  prototypes */
static int char_class(char c); /* character class function */
static int get_next_state(int, char, int *); /* state machine function */
static int iskeyword(char * kw_lexeme); /*keywords lookup functuion */



										/****************************************************************************************************************
										* Purpose: This function is used to initialise the scanner
										* Author:  Svillen Ranev
										* History/Versions: 1.0 - 1 October 2018
										* Called functions: b_isempty(), b_rewind(), b_clear()
										* Parameters: Buffer*
										* Return Value: Token, RT_FAIL_1, POSITIVE1, ZERO
										* Algorithm:
										****************************************************************************************************************/
int scanner_init(Buffer * psc_buf)
{
	if (b_isempty(psc_buf)) return EXIT_FAILURE;/*1*/
												/* in case the buffer has been read previously  */
	b_rewind(psc_buf);
	b_clear(str_LTBL);
	line = 1;
	sc_buf = psc_buf;
	return EXIT_SUCCESS; /*0*/
						 /*   scerrnum = 0;  */
						 /*no need - global ANSI C */
}

/****************************************************************************************************************
* Purpose: This function is used to generate token
* Author:  Gurkirat Singh, Shubham Ujinwal
* History/Versions: 1.0 - 4 November 2018
* Called functions: b_getc(), b_retract(), b_mark(), b_getcoffset(),
* get_next_state(), b_allocate(), strcpy(), b_reset(), b_addc(),
* b_location(), free()
* Parameters: void
* Return Value: Token t
* Algorithm: All the characters are read one by one and the appropriate token gets generated. If it is a keyword,
* the token is generated with the help of the keyword tables. Otherwise, the transition table is used to recognise
* the state and the appropriate function is called to generate the token.
****************************************************************************************************************/
Token malar_next_token(void)
{

	Token t; /* token to return after pattern recognition. Set all structure members to 0 */
	unsigned char c; /* input symbol */
	int state = 0; /* initial state of the FSM */
	short lexstart;  /*start offset of a lexeme in the input char buffer (array) */
	short lexend;    /*end   offset of a lexeme in the input char buffer (array)*/
	int oCounter = 1; /* declaring variable to keep record of how many times we need to retract if the operator is not .AND. or .OR.*/
	int r; /* index for loop */
	int accept = NOAS;  /* type of state - initially not accepting */
	Buffer * lex_buf; /* declaring a pointer lex_buf to Buffer */

					  /* endless loop broken by token returns it will generate a warning */
	while (1)
	{

		c = b_getc(sc_buf); /*getting the next char from the sc_buffer*/
		printf("***********************value in malar %c\n", c);

							/*reading the character and checking if it is a keyword or not*/
		switch (c)
		{
			case '=': /* Equals to assignment operator */
				if (b_getc(sc_buf) == '=')
				{
					t.code = REL_OP_T;
					t.attribute.rel_op = EQ;
				}
				else
				{
					t.code = ASS_OP_T;
					b_retract(sc_buf);
				}
				return t;

				/*  white space */
			case ' ': break;

				/* left paranthesis*/
			case '(': t.code = LPR_T; return t;

				/* Right paranthesis */
			case ')': t.code = RPR_T;	return t;

			case '{': /* Left braces */
				t.code = LBR_T;
				return t;

			case '}': /* Right braces */
				t.code = RBR_T;
				return t;

			case '>': /* Greater than relational operator */
				t.code = REL_OP_T;
				t.attribute.rel_op = GT;
				return t;

			case '<': /* Less than relational operator */
				if (b_getc(sc_buf) == '>') /* checking if it is not equals to*/
				{
					t.code = REL_OP_T;
					t.attribute.rel_op = NE;
				}
				else
				{
					t.code = REL_OP_T;
					t.attribute.rel_op = LT;

					b_retract(sc_buf);
				}

				return t;

				/* End of Statement */
			case ';': t.code = EOS_T; return t;

				/* horizontal tab*/
			case '\t': break;

				/* new line */
			case '\n': line++; break;

				/* vertical tab */
			case '\v': break;

				/* form feed */
			case '\f': break;

				/* Carriage return */
			case '\r': line++; break;

				/*check for comments */
			case '!':
				c = b_getc(sc_buf);
				if (c == '!') /*check if it is a valid comment*/
				{
					c = b_getc(sc_buf);

					do /*endless loop while the character is not a line terminator*/
					{
						c = b_getc(sc_buf);
					} while (c != '\r' && c != '\n'  && c != '255' && c != '\0');
					line++;
					break;
				}
				else /*returns the error token if comment is not valid*/
				{

					t.code = ERR_T;
					t.attribute.err_lex[0] = '!';
					t.attribute.err_lex[1] = c;
					t.attribute.err_lex[2] = '\0';
					while (c != '\r' && c != '\n'  && c != '255' && c != '\0') /*endless loop to skip the characters while the new line is not detected*/
					{
						c = b_getc(sc_buf);
					}line++;
					return t;
				}

				/* Comma Token */
			case ',': t.code = COM_T; return t;

				/* arithmetic operator for subtraction */
			case '-': t.code = ART_OP_T; t.attribute.arr_op = MINUS; return t;

				/* arithmetic operator for addition */
			case '+': t.code = ART_OP_T; t.attribute.arr_op = PLUS; return t;

				/* arithmetic operator for multiplication */
			case '*': t.code = ART_OP_T; t.attribute.arr_op = MULT; return t;

				/* arithmetic operator for division */
			case '/': t.code = ART_OP_T; t.attribute.arr_op = DIV; return t;

				/*String concatenation operator token*/
			case '#': t.code = SCC_OP_T; return t;

			case '.': /* Logical operator */
				c = b_getc(sc_buf);

				if (c == 'A') /*checking if the next char is A*/
				{
					oCounter++;
					if (b_getc(sc_buf) == 'N') /*checking if the next char is N*/
					{
						oCounter++;
						if (b_getc(sc_buf) == 'D')/*checking if the next char is D*/
						{
							oCounter++;
							if (b_getc(sc_buf) == '.')/*checking if the next char is .*/
							{
								t.code = LOG_OP_T;
								t.attribute.log_op = AND;
								return t;
							}
						}
					}
				}
				if (c == 'O') /*checking if the next char is O */
				{
					oCounter++;
					if (b_getc(sc_buf) == 'R') /*checking if the next char is R*/
					{
						oCounter++;
						if (b_getc(sc_buf) == '.')/*checking if the next char is .*/
						{
							t.code = LOG_OP_T;
							t.attribute.log_op = OR;
							return t;
						}
					}
				}
				/*retracting if the logical opertor is not found and then returning the error token*/
				for (r = 0; r < oCounter; r++)
				{
					b_retract(sc_buf);
				}
				t.code = ERR_T;
				t.attribute.err_lex[0] = '.';
				t.attribute.err_lex[1] = '\0';
				return t;

				/* End of File (SEOF) */
			case '\0':	t.code = SEOF_T; t.attribute.seof = SEOF1; return t;

				/* End of File (SEOF) */
			case '255': t.code = SEOF_T; t.attribute.seof = SEOF2; return t;

				/* End of File (SEOF) */
			case '0xFF': t.code = SEOF_T; t.attribute.seof = SEOF2; return t;

				/* End of File (SEOF) */
			case 'EOF': t.code = SEOF_T; t.attribute.seof = SEOF2; return t;

			case '\\': /*End oF file*/
				c = b_getc(sc_buf);
				if (c == '0')
				{ /*if true, it is EOF*/
					t.code = SEOF_T;
					t.attribute.seof = SEOF1;
				}
				else
				{/*else it is error token*/

					b_retract(sc_buf);
					t.code = ERR_T;

					strcpy(t.attribute.err_lex, "\\");
				}
				return t;


			default:
				printf("33333333333333333333333333333\n")
				/* retracting to read the character again*/
				b_retract(sc_buf);

				/* setting the mark at the begining of the Lexem and save it in lexstart */
				lexstart = b_mark(sc_buf, b_getcoffset(sc_buf));

				/* endless loop while we not reach the Accepting states*/
				while (accept == NOAS)
				{

					c = b_getc(sc_buf);

					/* checking if the next character is \*/
					if (c == '\\')
					{
						c = b_getc(sc_buf);

						/*checking if next char is 0, => its a SEOF*/
						if (c == '0')
						{

							state = 12;
							b_retract(sc_buf);
							b_retract(sc_buf);

							break;

						}
						else
						{ /* else retract and continue*/
							b_retract(sc_buf);
							c = '\\';

						}

					}
					state = get_next_state(state, c, &accept);
				};

				/* Retracting if the state is Accepting with retract*/
				if (accept == ASWR)
				{
					b_retract(sc_buf);
				}

				/* setting the lexend to b_getcoffset*/
				lexend = b_getcoffset(sc_buf);

				/* allocating the space for the lexeme */
				lex_buf = b_allocate(lexend - lexstart + 1, 0, 'f');

				/*checking if the allocation has failed and setting the appropriate token, attributes*/
				if (!lex_buf)
				{
					scerrnum = 324;
					t.code = RTE_T;
					strcpy(t.attribute.err_lex, "RUN TIME ERROR : ");
					return t;
				}

				/* resetting the buffer */
				b_reset(sc_buf);

				/* storing the characters to the newly allocated buffer */
				for (int i = 0; i < (lexend - lexstart); i++)
				{
					b_addc(lex_buf, b_getc(sc_buf));
				}

				/* adding the EOF character */
				b_addc(lex_buf, '\0');

				/* Calling the appropriate accepting function */
				t = aa_table[state](b_location(lex_buf, 0));

				/* freeing the temp allocated buffer */
				free(lex_buf);

				return t;
		}
	}
}

/****************************************************************************************************************
* Purpose: This function is used get character's next state from the scanner.
* Author:  Svillen Ranev
* History/Versions: 1.0 - 1 October 2018
* Called functions: char_class(), assert(), exit()
* Parameters: int, char, int *
* Return Value: int next - next state
* Algorithm: It will perform transition table by getting state of the character using char_class() function
and return its next state.
****************************************************************************************************************/
int get_next_state(int state, char c, int *accept)
{
	int col;
	int next;
	col = char_class(c);
	next = st_table[state][col];
#ifdef DEBUG
	printf("Input symbol: %c Row: %d Column: %d Next: %d \n", c, state, col, next);
#endif
	/*
	The assert(int test) macro can be used to add run-time diagnostic to programs
	and to "defend" from producing unexpected results.
	assert() is a macro that expands to an if statement;
	if test evaluates to false (zero) , assert aborts the program
	(by calling abort()) and sends the following message on stderr:

	Assertion failed: test, file filename, line linenum

	The filename and linenum listed in the message are the source file name
	and line number where the assert macro appears.
	If you place the #define NDEBUG directive ("no debugging")
	in the source code before the #include <assert.h> directive,
	the effect is to comment out the assert statement.
	*/
	assert(next != IS);

	/*
	The other way to include diagnostics in a program is to use
	conditional preprocessing as shown bellow. It allows the programmer
	to send more details describing the run-time problem.
	Once the program is tested thoroughly #define DEBUG is commented out
	or #undef DEBUF is used - see the top of the file.
	*/
#ifdef DEBUG
	if (next == IS)
	{
		printf("Scanner Error: Illegal state:\n");
		printf("Input symbol: %c Row: %d Column: %d\n", c, state, col);
		exit(1);
	}
#endif
	*accept = as_table[next];
	return next;
}

/****************************************************************************************************************
* Purpose: This function is will check the character and return its column index from the transition table .
* Author: Shubham Ujinwal
* History/Versions: 1.0 - 4 November 2018
* Called functions:
* Parameters: char
* Return Value: int val - represents the column number in transition table.
* Algorithm: check the input character and return its column of transition table.
****************************************************************************************************************/
int char_class(char c)
{
	/* int variable to store the column number*/
	int val;

	if ((c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z')) /* Checking the the char is Alphabet */
	{
		val = 0;
	}

	else if (c == '0') /* Checking the the char is 0 */
	{
		val = 1;
	}
	else if (c >= '1' && c <= '9')/* Checking the the char is non zero digit */
	{
		val = 2;
	}
	else if (c == '.')/* Checking the the char is . */
	{
		val = 3;
	}
	else if (c == '$')/* Checking the the char is $ */
	{
		val = 4;
	}
	else if (c == '"')/* Checking the the char is double quote (") */
	{
		val = 6;
	}
	else if (c == '\0' || c == '255' || c == SEOF2)/* Checking the the char is SEOF */
	{
		val = 7;
	}
	else /* for other characters */
	{
		val = 5;
	}
	return val;
}

/****************************************************************************************************************
* Purpose: Accepting function for the arithmentic variable identifier AND keywords(VID - AVID / KW).
* Author:  Shubham Ujinwal
* History/Versions: 1.0 - 4 November 2018
* Called functions: isKeyword(), strlen()
* Parameters: char []
* Return Value: Token t
* Algorithm: CHECK IF THE LEXEME IS A KEYWORD.
* IF THE LEXEME IS NOT A KEYWORD, SET a AVID TOKEN.
* IF THE lexeme IS LONGER than VID_LEN (see token.h) CHARACTERS,
* ONLY FIRST VID_LEN CHARACTERS ARE STORED
* INTO THE VARIABLE ATTRIBUTE ARRAY vid_lex[](see token.h) .
* ADD \0 AT THE END TO MAKE A C-type STRING.
****************************************************************************************************************/
Token aa_func02(char lexeme[])
{
	Token t; /* declaring a variable of type Token */
	int keyNum; /* declaring a variable for storing the value returned from the function isKeyword() */
	int lexLen; /*variable to store the length of the lexeme*/
	int count; /* index for loop */

	keyNum = iskeyword(lexeme); /* storing the value returned from the function*/

								/* Checking if it a keyword and setting the appropriate token, attribute */
	if (keyNum < 10)
	{
		t.code = KW_T;
		t.attribute.kwt_idx = keyNum;
		return t;
	}
	/* If not a keyword, set the token as AVID */
	t.code = AVID_T;

	/*store the length of lexeme*/
	lexLen = strlen(lexeme);

	/*loop to iterate through till the VID_LEN */
	for (count = 0; count < VID_LEN; count++)
	{
		/*checks the character should not be added more than lexLen*/
		if (count < lexLen)
		{
			t.attribute.vid_lex[count] = lexeme[count];
		}
		else break;
	}

	/*adding \0 to make a C-type String */
	t.attribute.vid_lex[count] = '\0';

	return t;
}




/****************************************************************************************************************
* Purpose: Accepting function for the string variable identifier(VID - SVID).
* Author: Shubham Ujinwal
* History/Versions: 1.0 - 4 November 2018
* Called functions: strlen()
* Parameters: char []
* Return Value: Token t
* Algorithm: SET a SVID TOKEN.
* IF THE lexeme IS LONGER than VID_LEN (see token.h) CHARACTERS,
* ONLY FIRST VID_LEN - 1 CHARACTERS ARE STORED
* INTO THE VARIABLE ATTRIBUTE ARRAY vid_lex[](see token.h)
* AND THEN THE $ CHARACTER IS APPENDED TO THE NAME.
* ADD \0 AT THE END TO MAKE A C-type STRING.
****************************************************************************************************************/
Token aa_func03(char lexeme[])
{
	Token t; /* declaring a variable of type Token */
	int lexLen;  /*variable to store the length of the lexeme*/
	int count; /*variable used in for loop*/
	t.code = SVID_T; /*set the token to SVID_T*/

					 /*store the length of lexeme*/
	lexLen = strlen(lexeme);

	/*storing lexeme to variable identifier token attribute (vid_lex[]) */
	for (count = 0; count < VID_LEN - 1; count++)
	{
		/*checks the character should not be added more than lexLen - 1 */
		if (count < lexLen - 1)
		{
			t.attribute.vid_lex[count] = lexeme[count];
		}
		else break;

	}
	t.attribute.vid_lex[count] = '$'; /*adding $ */
	t.attribute.vid_lex[++count] = '\0'; /*adding \0 to make a C-type String */
	return t;
}

/****************************************************************************************************************
* Purpose: ACCEPTING FUNCTION FOR THE integer literal(IL) - decimal constant (DIL)
* Author:  Gurkirat Singh
* History/Versions: 1.0 - 4 November 2018
* Called functions: strlen(), atoi(), aa_func11()
* Parameters: char []
* Return Value: Token t, aa_func11()
* Algorithm:  Convert lexeme to decimal integer value.
* Check if the number is 2 byte integer.
* In case of error, call the error state function else set the appropriate token, attribute
****************************************************************************************************************/
Token aa_func05(char lexeme[])
{
	Token t; /* declaring a variable of type Token */
	int lexLen; /*variable to store the length of the lexeme*/
	long d; /* declaring a short variable to hold the numbers */

	lexLen = strlen(lexeme);/*store length of lexeme */

	d = atol(lexeme); /*converting lexeme into integer type and store it in d variable*/

					  /*CHECKING IF THE VALUE IS NOT IN THE SAME RANGE AS the value of 2 - byte integer in C and the length is > 5 */
	if (d > SHRT_MAX || d <0 || lexLen > INL_LEN)
	{
		return  aa_func11(lexeme); /*if value is out of range then it will call aa_func11()*/
	}
	t.attribute.int_value = d;
	t.code = INL_T;
	return t;
}

/****************************************************************************************************************
* Purpose: ACCEPTING FUNCTION FOR THE floating-point literal (FPL)
* Author:  Gurkirat Singh
* History/Versions: 1.0 - 4 November 2018
* Called functions: atof(), aa_func11()
* Parameters: char []
* Return Value: Token t, aa_func11()
* Algorithm:  Convert lexeme to floating point value
* Check if the number is 4-byte float in C.
* In case of error, call the error state function else set the appropriate token, attribute
****************************************************************************************************************/
Token aa_func08(char lexeme[])
{
	Token t; /* declaring a variable of type Token */
	double num; /*declaring a variable of type double */

				/*converting the lexeme into double and store it in num variable of type double.*/
	num = (double) atof(lexeme);

	/* CHECKING IF THE VALUE IS IN THE SAME RANGE AS the value of 4 - byte float in C.*/
	if ((num >= FLT_MIN && num <= FLT_MAX) || num == 0)
	{
		t.attribute.flt_value = (float) num;
		t.code = FPL_T;
		return t;
	}

	/*IN CASE OF ERROR(OUT OF RANGE) THE FUNCTION MUST CALL aa_func11() and RETURN ERROR TOKEN*/
	return  aa_func11(lexeme);
}

/****************************************************************************************************************
* Purpose: ACCEPTING FUNCTION FOR THE string literal(SL)
* Author:  Gurkirat Singh
* History/Versions: 1.0 - 4 November 2018
* Called functions: strlen(), b_limit(), b_addc()
* Parameters: char []
* Return Value: Token t
* Algorithm:  Store the lexeme into str_LTBL
* Set the token and ahe attribute which is the offset from the beginning of the str_LTBL char buffer to the
* location where the first char of the lexeme contenet will be added to the buffer.
* THE OPENING AND CLOSING " MUST BE IGNORED amd '\0' is added to the end of the string.
* Line counter must be incremented if the lexeme contains the line terminator.
****************************************************************************************************************/
Token aa_func10(char lexeme[])
{
	Token t; /* declaring a variable of type Token */
	int i; /* declaring a variable of type int*/
	int lexLen; /* declaring a variable to store the length of the lexeme */
	lexLen = strlen(lexeme); /* declaring a variable of type int and store the length of lexeme */

	t.attribute.str_offset = b_limit(str_LTBL);

	/*storing lexeme to str_LTBL*/
	for (i = 1; i <= lexLen - 2; i++)
	{
		/*if the lexeme contains line terminator then line get incremented*/
		if (lexeme[i] == '\n' && lexeme[i] == '\r')
		{
			line++;
		}
		b_addc(str_LTBL, lexeme[i]);

	}
	/*adding end of file to str_LTBL*/
	b_addc(str_LTBL, '\0');

	t.code = STR_T;

	return t;
}

/****************************************************************************************************************
* Purpose: ACCEPTING FUNCTION FOR THE ERROR TOKEN
* Author:  Shubham Ujinwal
* History/Versions: 1.0 - 4 November 2018
* Called functions: strlen()
* Parameters: char []
* Return Value: Token t
* Algorithm:  THE FUNCTION SETS THE ERROR TOKEN.
* THE ATTRIBUTE OF THE ERROR TOKEN IS THE lexeme CONTENT ITSELF
* AND IT MUST BE STORED in err_lex. IF THE ERROR lexeme IS LONGER
* than ERR_LEN characters, ONLY THE FIRST ERR_LEN-3 characters ARE
* STORED IN err_lex. THEN THREE DOTS ... ARE ADDED TO THE END OF THE
* err_lex C-type string.
* IF THE ERROR lexeme CONTAINS line terminators THE line COUNTER MUST BE INCTREMENTED.
* BEFORE RETURNING THE FUNCTION MUST SET THE APROPRIATE TOKEN CODE
****************************************************************************************************************/
Token aa_func11(char lexeme[])
{
	Token t; /* declaring a variable of type Token */
	int lexLen; /* declaring a variable of type int to store the lexeme length */
	int count; /* declaring a variable of type int and use it in for loop*/
	t.code = ERR_T;

	lexLen = strlen(lexeme);/*store the length of lexeme*/

							/* looping to get ONLY THE FIRST ERR_LEN - 3 characters in err_lex*/
	for (count = 0; count < ERR_LEN - 3; count++)
	{
		/* Checking if we are not gone out of range of the lenghth of the lexeme */
		if (count <= lexLen - 1)
		{
			/*if the lexeme contains line terminator then line get incremented*/
			if (lexeme[count] == '\n' && lexeme[count] == '\r')
			{
				line++;
			}
			t.attribute.err_lex[count] = lexeme[count];
		}
		else break;
	}
	/*THREE DOTS ...ARE ADDED TO THE END OF THE err_lex C - type string.*/
	if (lexLen > ERR_LEN - 1)
	{
		t.attribute.err_lex[count] = '.';
		t.attribute.err_lex[++count] = '.';
		t.attribute.err_lex[++count] = '.';
		t.attribute.err_lex[++count] = '\0';
	}

	/*end of file added at the err_lex C - type string*/
	t.attribute.err_lex[count] = '\0';
	return t;
}

/****************************************************************************************************************
* Purpose: ACCEPTING FUNCTION FOR THE ERROR TOKEN with retract
* Author:  Gurkirat Singh
* History/Versions: 1.0 - 4 November 2018
* Called functions: strlen()
* Parameters: char []
* Return Value: Token t
* Algorithm:  THE FUNCTION SETS THE ERROR TOKEN.
* THE ATTRIBUTE OF THE ERROR TOKEN IS THE lexeme CONTENT ITSELF
* AND IT MUST BE STORED in err_lex. IF THE ERROR lexeme IS LONGER
* than ERR_LEN characters, ONLY THE FIRST ERR_LEN-3 characters ARE
* STORED IN err_lex. THEN THREE DOTS ... ARE ADDED TO THE END OF THE
* err_lex C-type string.
* IF THE ERROR lexeme CONTAINS line terminators THE line COUNTER MUST BE INCTREMENTED.
* BEFORE RETURNING THE FUNCTION MUST SET THE APROPRIATE TOKEN CODE
****************************************************************************************************************/
Token aa_func12(char lexeme[])
{
	Token t; /* declaring a variable of type Token */
	int lexLen; /* declaring a variable of type int to store the lexeme length */
	int count; /* declaring a variable of type int and use it in for loop*/
	t.code = ERR_T;

	lexLen = strlen(lexeme);/*store the length of lexeme*/

							/* looping to get ONLY THE FIRST ERR_LEN - 3 characters in err_lex*/
	for (count = 0; count < ERR_LEN - 3; count++)
	{
		/* Checking if we are not gone out of range of the lenghth of the lexeme */
		if (count <= lexLen - 1)
		{
			/*if the lexeme contains line terminator then line get incremented*/
			if (lexeme[count] == '\n' && lexeme[count] == '\r')
			{
				line++;
			}
			t.attribute.err_lex[count] = lexeme[count];
		}
		else break;
	}
	/*THREE DOTS ... and '\0 'ARE ADDED TO THE END OF THE err_lex C - type string.*/
	if (lexLen > ERR_LEN - 1)
	{
		t.attribute.err_lex[count] = '.';
		t.attribute.err_lex[++count] = '.';
		t.attribute.err_lex[++count] = '.';
		t.attribute.err_lex[++count] = '\0';
	}
	/*end of file added at the err_lex C - type string*/
	t.attribute.err_lex[count] = '\0';
	return t;
}
/****************************************************************************************************************
* Purpose: Matches if the lexeme is same as the strings in keyword table
* Author:  Shubham Ujinwal
* History/Versions: 1.0 - 4 November 2018
* Called functions: strcmp()
* Parameters: char []
* Return Value: int i - index of the keyword from the keyword table, int 10 - if no match is found
* Algorithm:  Compare the lexeme with the strings store in kw_table[]
****************************************************************************************************************/
int iskeyword(char * kw_lexeme)
{
	int i;  /* declaring a variable of type int and use it in for loop*/

			/* for loop to check if the keyword matches with any element in the kw_table[] */
	for (i = 0; i < KWT_SIZE; i++)
	{
		/*compare the lexeme with the strings stored in kw_table[]*/
		if (strcmp(kw_table[i], kw_lexeme) == 0)
		{
			return i;
		}
	}

	/*if no match is found, it will return 10*/
	return 10;
}
