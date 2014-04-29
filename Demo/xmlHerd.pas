unit xmlHerd;
(*
xmlHerd.pas/dcu
-------------------
Begin: 2006/09/01
Last revision: $Date: 2007-01-10 22:20:59 $ $Author: areeves $
Version number: $Revision: 1.1 $
Project: APHI General Purpose Delphi Library, XML datafile functions
Website: http://www.naadsm.org/opensource/delphi
Author: Shaun Case <Shaun.Case@colostate.edu>
Revision History:
$Log: xmlHerd.pas,v $
Revision 1.1  2007-01-10 22:20:59  areeves
Initial commit of the Simple Delphi Expat Wrapper.

Revision 1.1  2006/10/19 14:33:22  shaun
*** empty log message ***

Revision 1.1  2006/09/28 13:45:39  shaun
*** empty log message ***

Revision 1.2  2006/09/19 22:00:08  shaun
Adding Comments

--------------------------------------------------
Copyright (C) 2005 - 2006 Animal Population Health Institute, Colorado State University

This program is free software; you can redistribute it and/or modify it under the terms of the GNU General
Public License as published by the Free Software Foundation; either version 2 of the License, or
(at your option) any later version.
*)
interface
uses
  Loc,
  SysUtils
;

type TxmlHerd =  class(TObject)
  public
    constructor create(Id:String; ProdType: String; Size: Integer; Location: TLoc; Status: String);overload;
    destructor destroy(); override;

    function GetId():String;
    function GetProdType():String;
    function GetSize():Integer;
    function GeTLocation():TLoc;
    function GetStatus():String;

    procedure SetId(Id:String);
    procedure SetProdType( ProdType:String);
    procedure SetSize(Size:Integer);
    procedure SeTLocation(Location:TLoc);
    procedure SetStatus(Status:String);

    function WriteContents():String;

  protected
  _id: String;
  _prodType: String;
  _size: Integer;
  _location: TLoc;
  _status: String;

end;

implementation
  constructor TxmlHerd.create(Id:String; ProdType: String; Size: Integer; Location: TLoc; Status: String);
    begin
      _id := Id;
      _prodType := ProdType;
      _size := Size;
      _location := Location;
      _status := Status;
    end;

  destructor TxmlHerd.destroy();
    begin
      inherited destroy();
      _location.destroy();
    end;

  function TxmlHerd.GetId():String;
    begin
      result := _id;
    end;

  function TxmlHerd.GetProdType():String;
    begin
      result := _prodType;
    end;

  function TxmlHerd.GetSize():Integer;
    begin
      result := _size;
    end;

  function TxmlHerd.GeTLocation():TLoc;
    begin
      result := _location;
    end;

  function TxmlHerd.GetStatus():String;
    begin
      result := _status;
    end;

  procedure TxmlHerd.SetId(Id:String);
    begin
      _id := Id;
    end;

  procedure TxmlHerd.SetProdType( ProdType:String);
    begin
      _prodType := ProdType;
    end;

  procedure TxmlHerd.SetSize(Size:Integer);
    begin
      _size := Size;
    end;

  procedure TxmlHerd.SeTLocation(Location:TLoc);
    begin
      _location := Location;
    end;

  procedure TxmlHerd.SetStatus(Status:String);
    begin
      _status := Status;
    end;

  function TxmlHerd.WriteContents():String;
    begin
      result := 'xmlHerd:  id = ' + _id + '  production-type = ' + _prodType + '  size = ' + IntToStr(_size) + '  location = [lat(' + FloatToStr(_location.GetLatitude()) + ') lon(' + FloatToStr(_location.GetLongitude()) + ')]'
    end;

end.
