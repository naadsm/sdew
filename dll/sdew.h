/********************************************************************************
*  sdew.h
*  -------------------
*  Begin: 2006/09/01
*  Last revision: $Date: 2007-05-29 20:14:56 $ $Author: areeves $
*  Version number: $Revision: 1.2 $
*  Project: Simple Delphi Expat Wrapper
*  Website: http://www.naadsm.org/opensource/delphi
*  Author: Shaun Case <Shaun.Case@colostate.edu>
*  Revision History:
*  $Log: sdew.h,v $
*  Revision 1.2  2007-05-29 20:14:56  areeves
*  Updated SDEW with new functions for:
*    -- Creating a parser from a buffer rather than a file
*    -- Handling element attributes
*
*  Revision 1.1  2007/01/10 22:20:59  areeves
*  Initial commit of the Simple Delphi Expat Wrapper.
*
*  Revision 1.1  2006/10/19 14:05:05  shaun
*  --  Moved to a separate C_DLLs module
*
*  Revision 1.4  2006/09/19 21:42:07  shaun
*  *** empty log message ***
*
*  --------------------------------------------------
*  Copyright (C) 2006 - 2007 Animal Population Health Institute, Colorado State University
*
*  This program is free software; you can redistribute it and/or modify it under the terms of the GNU General
*  Public License as published by the Free Software Foundation; either version 2 of the License, or
*  (at your option) any later version.
*
*********************************************************************************/

#ifndef _SDEW_H_
#define _SDEW_H_

#if BUILDING_DLL
# define DLLIMPORT __declspec (dllexport)
#else /* Not BUILDING_DLL */
# define DLLIMPORT __declspec (dllimport)
#endif /* Not BUILDING_DLL */


//Scew Library Interface
DLLIMPORT void *Create_Scew_Parser_From_File( char *Filename );
DLLIMPORT void *Create_Scew_Parser_From_String( char * str, int strLen );
DLLIMPORT void  Free_Parser( void *_parser );

DLLIMPORT void *Tree_Root_Scew( void *_parser );
DLLIMPORT void  Free_RootTree( void *_rootTree );

DLLIMPORT void  TestParser( char *Filename );

DLLIMPORT int   Element_Count_Scew( void *_element );
DLLIMPORT char *Element_Name_By_Index( void *_element, int Index );
DLLIMPORT void *Element_By_Index( void *_element, int Index );
DLLIMPORT char *Element_Name( void *_element );
DLLIMPORT char *Element_Contents( void *_element );
DLLIMPORT void *Element_By_Name( void *_element, char *_name);
DLLIMPORT void *Element_Next( void *_parent, void *_element );
DLLIMPORT char *Element_Attribute( void *_element, char *_attr );

DLLIMPORT int Attribute_Count_Scew( void *_element );
DLLIMPORT char *Attribute_Name_By_Index( void *_element, int Index );
DLLIMPORT void *Attribute_By_Index( void *_element, int Index );
DLLIMPORT char *Attribute_Name( void *_attribute );
DLLIMPORT char *Attribute_Value( void *_attribute );
DLLIMPORT void *Attribute_By_Name( void *_element, char *_name);
DLLIMPORT void *Attribute_Next( void *_parent, void *_attribute );


#endif /* _SDEW_H_ */
