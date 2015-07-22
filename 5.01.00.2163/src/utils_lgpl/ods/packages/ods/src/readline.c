/* readline.c --

   Routine(s) to facilitate reading text files

   Copyright (c) 2002 WL | Delft Hydraulics

   Arjen Markus
*/
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include "readline.h"

/* ------------------------------------------------------------------
    Function: readLine()
    Author:   Arjen Markus
    Purpose:  Read a line like fgets()
    Context:  Used by ODS routines
    Pseudo Code:
              Read a line using fgets(), but then check if we
              have reached the end yet. If not, read until the
              newline character. Keep the newline (but delete
              a carriage return)
    Note:
              If an error occurs the buffer is emptied, this
              is a different behaviour from fgets().
------------------------------------------------------------------ */
char *
readLine( char *buffer, int bufs, FILE *stream )
{
   char  dummy[100] ;
   char *result     ;
   char *pstr       ;

   /* Read the buffer */
   result = fgets( buffer, bufs, stream ) ;

   /* Decide if it is necessary to carry on reading */
   if ( result == NULL )
   {
      buffer[0] = '\0' ;
      return NULL ;
   }
   else
   {
      pstr = strchr( result, '\n' ) ;
      while ( pstr == NULL && ! feof(stream) )
      {
         result = fgets( dummy, sizeof(dummy), stream ) ;
         pstr = strchr( result, '\n' ) ;
      }

      /* Get rid of "\r" if there is any */
      pstr = strchr( buffer, '\r' ) ;
      if ( pstr != NULL ) *pstr = '\n' ;
   }

   if ( feof(stream) )
   {
      strcat( buffer, "\n" ) ;
   }

   if ( ferror(stream) )
   {
      buffer[0] = '\0' ;
      return NULL ;
   }
   else
   {
      return buffer ;
   }
}

/* Test driver */
#if defined(TEST_READLINE)
int main( int argc, char *argv[] )
{
   char  line[20] ;
   FILE *infile   ;

   infile = fopen( "arjen.inp", "r" ) ;

   while( !feof(infile) )
   {
      readLine( line, sizeof(line), infile ) ;
      printf( "line: >%s<\n", line ) ;
   }
   fclose( infile ) ;

   return 0 ;
}
#endif
