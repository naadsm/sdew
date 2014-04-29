unit XMLReader;
(*
XMLReader.pas
-------------
Begin: 2006/09/01
Last revision: $Date: 2009-06-07 03:41:41 $ $Author: areeves $
Version number: $Revision: 1.4 $
Project: APHI General Purpose Delphi Library, XML datafile functions
Website: http://www.naadsm.org/opensource/delphi
Author: Shaun Case <Shaun.Case@colostate.edu>
--------------------------------------------------
Copyright (C) 2006 - 2007 Animal Population Health Institute, Colorado State University

This program is free software; you can redistribute it and/or modify it under the terms of the GNU General
Public License as published by the Free Software Foundation; either version 2 of the License, or
(at your option) any later version.
*)

interface

  uses
    Sdew,
    DebugWindow,
    FormProgress
  ;

  type XMLCallback = function( element: Pointer; Sdew: TSdew; extData: Pointer ): TObject;

  type XMLCallbackPtr = ^XMLCallback;

  type TObjectArray = array of TObject;

  type  XMLCallbacks = record
    Tag: String;
    Callback: XMLCallback;
    ObjectList: TObjectArray;
  end;

  type CallbackArray = array of XMLCallbacks;
  type CallbackArrayPtr = ^CallbackArray;

  type IntegerPtr = ^Integer;

    //////////////////////////////////////////////////////////////////////////////
   //  CLASS:  TXMLReader      //
  //////////////////////////////
  type TXMLReader = class( TObject )
    public
      constructor create( CB_Array: CallbackArrayPtr; Filename: String;
                          extData: Pointer; frmProgress: TFormProgress = nil  );overload;
      constructor create( CB_Array: CallbackArrayPtr; Sdew: TSdew; Element: Pointer;
                          extData: Pointer; frmProgress: TFormProgress = nil );overload;
      destructor destroy();override;
      function Run():Boolean;

    protected
      _Sdew: TSdew;
      _Filename: String;
      _RootTree: Pointer;
      _CB_Array: CallbackArrayPtr;
      _extData: Pointer;
      _frmProgress:TFormProgress;

      procedure Initialize();
      procedure EnumerateXML( Element:Pointer );
      function FindCallback( Name: String; Index: IntegerPtr ):XMLCallback;
    end
  ;
    /////////////////////////////
   //  End Class: TXMLReader  //
  ////////////////////////////////////////////////////////////////////////////////


implementation
////////////////////////////////////////////////////////////////////////////////
//
//  TXMLReader basically iterates through all XML elements in a file and calls
//  callback functions associated with those elements, if any.  These callbacks
//  are passed to the create() function in an array of a record, which all
//  contain the names of the elements to handle and an open place for a
//  callback to return an object as a result of its processing.
//
//  Nothing in this class prohibits the use of nesting calls to it from
//  Callback functions.  Thus, a deeply structured XML file can easily be
//  parsed, read, and acted on, by using nested calls to an TXMLReader from
//  within the callback functions called by TXMLReader....To nest a call
//  to this object, call the create() function using the TSdew object
//  already in use by another instance of this object.  Do NOT call the
//  create() function which using the Filename string.
//  See the demo program for more details on how to do this....
//
////////////////////////////////////////////////////////////////////////////////


  constructor TXMLReader.create(CB_Array: CallbackArrayPtr; Filename: String; extData: Pointer; frmProgress: TFormProgress = nil );
    begin
      inherited create();

      _Filename := Filename;
      _extData := extData;
      _CB_Array := CB_Array;
      _frmProgress := frmProgress;

      Initialize();  //This is only called when you want to load a new file....
    end
  ;


  constructor TXMLReader.create( CB_Array: CallbackArrayPtr; Sdew: TSdew; Element: Pointer; extData: Pointer; frmProgress: TFormProgress = nil );
    begin
      inherited create();

      _Filename := '';
      _extData := extData;
      _CB_Array := CB_Array;
      _frmProgress := frmProgress;

      _RootTree := Element;
      _Sdew := Sdew
    end
  ;


  destructor TXMLReader.destroy();
    begin
      if( ( length(_Filename) > 0 ) and assigned(_Sdew) )  then
        _Sdew.destroy()
      ;

      inherited destroy();
    end
  ;


  procedure TXMLReader.Initialize();
    begin
      _Sdew := TSdew.createFromFile( PChar(_Filename) );
      _RootTree := _Sdew.GetRootTree();
    end
  ;


  function TXMLReader.Run():Boolean;
    begin
      EnumerateXML( _RootTree );
      result := true;
    end
  ;


  procedure TXMLReader.EnumerateXML( Element:Pointer );
    var
      Count: Integer;
      ElementName: String;
//    NextElement: Pointer;  //Save this for future use.
      I: Integer;
      CallBack: XMLCallback;
      Index: Integer;
      tempObject: TObject;
    begin
      Count := _Sdew.GetElementCount(Element);

      if ( Assigned( _frmProgress ) ) then
        _frmProgress.setPrimary( 0 )
      ;

      for I:=0 to (Count-1) do
        begin
          if ( Assigned( _frmProgress ) ) then
            _frmProgress.setPrimary( (((I + 1) * 100 )div Count ) )
          ;

          ElementName := _Sdew.GetElementName(_Sdew.GetElementByIndex( Element, I ));
          CallBack := FindCallback( ElementName, @Index );

          if ( Assigned( CallBack ) ) then
            begin
              tempObject := CallBack( _Sdew.GetElementByIndex( Element, I ), _Sdew, _extData );

              if ( assigned( tempObject ) ) then
                begin
                 SetLength( _CB_Array^[Index].ObjectList, length( _CB_Array^[Index].ObjectList ) + 1 );
                  _CB_Array^[Index].ObjectList[ High(_CB_Array^[Index].ObjectList) ] := tempObject;
                end
              ;
            end
//        else  //  No callback is available for this element, just print it out to the debugging screen...
//          dbcout( ElementName + ':  ' + _Sdew.GetElementContents(_Sdew.GetElementByIndex( Element, I )), true)
          ;
        end
      ;
    end
  ;


  function TXMLReader.FindCallback( Name: String; Index: IntegerPtr ):XMLCallback;
    var
      I: Integer;
      ret_val: XMLCallback;
      found:boolean;
    begin;
       ret_val := nil;
       found := false;

       if (( _CB_Array <> nil ) AND ( length(Name) > 0 )) then
         begin
           for I := 0 to High(_CB_Array^) do
             begin
                if ( _CB_Array^[I].Tag = Name ) then
                  begin
                    ret_val := _CB_Array^[I].Callback;
                    Index^ := I;
                    found := true;
                    break;
                  end
                ;
             end
           ;
             
           if ( not found ) then
             begin
               for I := 0 to High(_CB_Array^) do
                 begin
                    if ( _CB_Array^[I].Tag = '*' ) then
                      begin
                        ret_val := _CB_Array^[I].Callback;
                        Index^ := I;
                        //found := true; // Never used??
                        break;
                      end
                    ;
                 end
               ;
             end
           ;
         end
       ;
         
       result := ret_val;
    end
  ;
    
end.
