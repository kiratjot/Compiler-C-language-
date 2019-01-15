/*
* File Name: table.h
* Compiler: MS Visual Studio 2015 Community
* Author: Gurkirat Singh 040876315, Shubham Ujinwal 040885893
* Course: CST 8152  Compilers
* Lab Section: 12
* Assignment: 2
* Date: 8 November 2018
* Professor: Sv. Ranev
* Purpose: Transition Table and function declarations necessary for the scanner implementation
* as required for CST8152 - Assignment #2.
* Function list: aa_func02();
*				 aa_func03();
*				 aa_func05();
*				 aa_func08();
*				 aa_func10();
*				 aa_func11();
*				 aa_func12();
*/

#ifndef  TABLE_H_
#define  TABLE_H_

#ifndef BUFFER_H_
#include "buffer.h"
#endif

#ifndef NULL
#include <_null.h> /* NULL pointer constant is defined there */
#endif

/*   Source end-of-file (SEOF) sentinel symbol
*    '\0' or only one of the folowing constants: 255, 0xFF, EOF
*/

#define ES  11 /* Error state  with no retract */
#define ER  12 /* Error state  with retract */
#define IS -1    /* Inavalid state */

/* State transition table definition */
#define TABLE_COLUMNS 8

/*transition table - type of states defined in separate table */
int  st_table[][TABLE_COLUMNS] = {
	/* State 0 */ { 1,  6,  4, ES, ES, ES,  9, ES },
	/* State 2 */ { 1,  1,  1,  2,  3,  2, ES,  2 },
	/* State 3 */ { IS, IS, IS, IS, IS, IS, IS, IS },
	/* State 4 */ { IS, IS, IS, IS, IS, IS, IS, IS },
	/* State 5 */ { ES,  4,  4,  7, ES,  5, ES,  5 },
	/* State 6 */ { IS, IS, IS, IS, IS, IS, IS, IS },
	/* State 7 */ { ES,  6, ES,  7,  5,  5, ES,  5 },
	/* State 8 */ { 8,  7,  7,  8,  8,  8, ES,  8 },
	/* State 9 */ { IS, IS, IS, IS, IS, IS, IS, IS },
	/* State 10 */ { 9,  9,  9,  9,  9,  9, 10, ER },
	/* State 11 */ { IS, IS, IS, IS, IS, IS, IS, IS },
	/* State 12 */ { IS, IS, IS ,IS, IS, IS, IS, IS },
	/* State 13 */ { IS, IS, IS, IS, IS, IS, IS, IS },
};

/* Accepting state table definition */
#define ASWR     100  /* accepting state with retract */
#define ASNR     101  /* accepting state with no retract */
#define NOAS     102  /* not accepting state */

int as_table[] = {
	/* State 0 */ NOAS,
	/* State 1 */ NOAS,
	/* State 2 */ ASWR,
	/* State 3 */ ASNR,
	/* State 4 */ NOAS,
	/* State 5 */ ASWR,
	/* State 6 */ NOAS,
	/* State 7 */ NOAS,
	/* State 8 */ ASWR,
	/* State 9 */ NOAS,
	/* State 10 */ASNR,
	/* State 11 */ASNR,
	/* State 12*/ ASWR
};

/* Accepting action function declarations */
Token aa_func02(char *lexeme);
Token aa_func03(char *lexeme);
Token aa_func05(char *lexeme);
Token aa_func08(char *lexeme);
Token aa_func10(char *lexeme);
Token aa_func11(char *lexeme);
Token aa_func12(char *lexeme);


/* defining a new type: pointer to function (of one char * argument)
returning Token */
typedef Token(*PTR_AAF)(char *lexeme);

/* Accepting function (action) callback table (array) definition */
PTR_AAF aa_table[] = {

	/* State 1 */ NULL,
	/* State 2 */ NULL,
	/* State 3 */ &aa_func02,
	/* State 4 */ &aa_func03,
	/* State 5 */ NULL,
	/* State 6 */ &aa_func05,
	/* State 7 */ NULL,
	/* State 8 */ NULL,
	/* State 9 */ &aa_func08,
	/* State 10 */NULL,
	/* State 11 */&aa_func10,
	/* State 12 */&aa_func11,
	/* State 13 */&aa_func12
};

/* Keyword lookup table (.AND. and .OR. are not keywords) */
#define KWT_SIZE  10

char * kw_table[] =
{
	"ELSE",
	"FALSE",
	"IF",
	"PLATYPUS",
	"READ",
	"REPEAT",
	"THEN",
	"TRUE",
	"WHILE",
	"WRITE"
};

#endif
