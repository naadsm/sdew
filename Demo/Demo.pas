unit Demo;
(*
Demo.pas/dcu
-------------------
Begin: 2006/09/01
Last revision: $Date: 2007-01-10 22:20:59 $ $Author: areeves $
Version number: $Revision: 1.1 $
Project: Simple Delphi Expat Wrapper
Website: http://www.naadsm.org/opensource/delphi
Author: Shaun Case <Shaun.Case@colostate.edu>
Revision History:
$Log: Demo.pas,v $
Revision 1.1  2007-01-10 22:20:59  areeves
Initial commit of the Simple Delphi Expat Wrapper.

Revision 1.1  2006/10/19 14:33:22  shaun
*** empty log message ***

Revision 1.3  2006/10/19 14:08:50  shaun
*** empty log message ***

Revision 1.2  2006/09/19 22:00:08  shaun
Adding Comments

--------------------------------------------------
Copyright (C) 2005 - 2007 Animal Population Health Institute, Colorado State University

This program is free software; you can redistribute it and/or modify it under the terms of the GNU General
Public License as published by the Free Software Foundation; either version 2 of the License, or
(at your option) any later version.
*)

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, Sdew, XMLReader, Loc, xmlHerd, DiseaseModelStatMethods, TypInfo;

////////////////////////////
//  Button form stuff
//////////////////
type
  TForm1 = class(TForm)
    Button1: TButton;
    Button3: TButton;
    procedure Button1Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////


type Loc = record
  latitude: double;
  longitude: double;
end;

type statMethodFunc = function( Element:Pointer; Sdew:TSdew ):TgenericStatMethod;

type statEquates = record
  name:String;
  sFunc: statMethodFunc;
end;

////////////////////////////////////////////////////////////////////////////////
//  Forward declarations of functions needed by the XML parsing routines.
//  These are callback functions, which called by the XMLReader object as it
//  parses an XML file.
//////////////////////////////////////////////////////////////////////
function ProcessUniformStatMethod     ( Element:Pointer; Sdew:TSdew ):TgenericStatMethod;
function ProcessGammaStatMethod       ( Element:Pointer; Sdew:TSdew ):TgenericStatMethod;
function ProcessBetaPertStatMethod    ( Element:Pointer; Sdew:TSdew ):TgenericStatMethod;
function ProcessPiecewiseStatMethod   ( Element:Pointer; Sdew:TSdew ):TgenericStatMethod;
function ProcessTriangularStatMethod  ( Element:Pointer; Sdew:TSdew ):TgenericStatMethod;
function ProcessLogisticsStatMethod   ( Element:Pointer; Sdew:TSdew ):TgenericStatMethod;
function ProcessWeibullStatMethod     ( Element:Pointer; Sdew:TSdew ):TgenericStatMethod;
function ProcessPointStatMethod       ( Element:Pointer; Sdew:TSdew ):TgenericStatMethod;
function ProcessGaussianStatMethod    ( Element:Pointer; Sdew:TSdew ):TgenericStatMethod;
function ProcessLoglogisticsStatMethod( Element:Pointer; Sdew:TSdew ):TgenericStatMethod;
function ProcessPearson5StatMethod    ( Element:Pointer; Sdew:TSdew ):TgenericStatMethod;
function ProcessLognormalStatMethod   ( Element:Pointer; Sdew:TSdew ):TgenericStatMethod;
function ProcessBetaStatMethod        ( Element:Pointer; Sdew:TSdew ):TgenericStatMethod;
function ProcessExponentialStatMethod ( Element:Pointer; Sdew:TSdew ):TgenericStatMethod;


var
  Form1: TForm1;   //Basic form with two buttons on it....

implementation
  uses
    DebugWindow
;
{$R *.dfm}

/////////////////////////////////////////
// Array of stat method names
const constStatMethods: array[0..13] of String =
(
  'uniform'    ,
  'gamma'      ,
  'beta-pert'  ,
  'piecewise'  ,
  'triangular' ,
  'logistic'   ,
  'weibull'    ,
  'point'      ,
  'gaussian'   ,
  'loglogistic',
  'pearson5'   ,
  'lognormal'  ,
  'beta'       ,
  'exponential'
);

///////////////////////////////////////////////////////////////////////////////
//  Array of stat method names and their associated callback functions.
//  This is used by the XMLReader object as it parses the XML file.  When
//  it encounters one of these stat methods, by name in an element, it calls
//  the associated callback function.
///////////////////////
const constStatMethodTypes:  array[0..13] of statEquates =
(
  ( name: 'uniform';     sFunc:ProcessUniformStatMethod       ),
  ( name: 'gamma';       sFunc: ProcessGammaStatMethod        ),
  ( name: 'beta-pert';   sFunc: ProcessBetaPertStatMethod     ),
  ( name: 'piecewise';   sFunc: ProcessPiecewiseStatMethod    ),
  ( name: 'triangular';  sFunc: ProcessTriangularStatMethod   ),
  ( name: 'logistic';    sFunc: ProcessLogisticsStatMethod    ),
  ( name: 'weibull';     sFunc: ProcessWeibullStatMethod      ),
  ( name: 'point';       sFunc: ProcessPointStatMethod        ),
  ( name: 'gaussian';    sFunc: ProcessGaussianStatMethod     ),
  ( name: 'loglogistic'; sFunc: ProcessLoglogisticsStatMethod ),
  ( name: 'pearson5';    sFunc: ProcessPearson5StatMethod     ),
  ( name: 'lognormal';   sFunc: ProcessLognormalStatMethod    ),
  ( name: 'beta';        sFunc: ProcessBetaStatMethod         ),
  ( name: 'exponential'; sFunc: ProcessExponentialStatMethod  )
);


///////////////////////////////////////////////////////////////////////////////
//  These two functions are the callback functions used when loading a herd
//  XML file.  This process is started by clicking button 1. (See below)
//////////////////////////////////////////////////////////////////////////
   function ProcessHerdLocation( Element: Pointer; Sdew: TSdew; extData:Pointer): TObject;
     var
       latitude: double;
       longitude: double;
       location: TLoc;
     begin
       latitude := StrToFloat(Sdew.GetElementContents(Sdew.GetElementByName(Element, PChar('latitude'))));
       longitude := StrToFloat(Sdew.GetElementContents(Sdew.GetElementByName(Element, PChar('longitude'))));
       location := TLoc.create( latitude, longitude );
       result := location;
     end;

   function ProcessHerd( Element: Pointer; Sdew: TSdew; extData:Pointer ):TObject;
      var
         id: String;
         productionType: String;
         location: TLoc;
         size: Integer;
         status: String;
         tempXMLReader: TXMLReader;
         locationCallbacks : CallbackArray;
         Herd: TxmlHerd;

      begin
         SetLength(locationCallbacks, 1);
         locationCallbacks[0].Tag := 'location';
         locationCallbacks[0].Callback := ProcessHerdLocation;
         SetLength(locationCallbacks[0].ObjectList,0);

         tempXMLReader := TXMLReader.create( @locationCallbacks, Sdew, Element, @location );
         tempXMLReader.Run();
         tempXMLReader.destroy();

         id := Sdew.GetElementContents(Sdew.GetElementByName(Element, PChar('id')));
         productionType := Sdew.GetElementContents(Sdew.GetElementByName(Element, PChar('production-type')));
         size := StrToInt(Sdew.GetElementContents(Sdew.GetElementByName(Element, PChar('size'))));
         status := Sdew.GetElementContents(Sdew.GetElementByName(Element, PChar('status')));

         {*** Instantiate a herd object here and load the above values to save to the database ***}
         if ( length(locationCallbacks[0].ObjectList) > 0 ) then
           Herd := TxmlHerd.create(id, productionType, size, TLoc(locationCallbacks[0].ObjectList[0]), status )
         else
           begin
             Herd := nil;
             Application.MessageBox('This herd file is not valid.  A herd is missing a location.', 'Error');
           end;

         result := Herd;

      end;

      
//////////////////////////////////////////////////////////////////////////////
//  This function is called when the user clicks the top button on the demo
//  form.  This button should say "Press Me To Load the Herds XML File"
//  The callback functions needed for processing this XML file are
//  located just above this function.
//////////////////////////////////////////////////////////////////////
procedure TForm1.Button1Click(Sender: TObject);
    var
       XMLReader: TXMLReader;
       MyCallbacks: CallbackArray;
       I: Integer;
       J: Integer;
  	begin
       SetLength( MyCallbacks, 1);
       MyCallbacks[0].Tag := 'herd';
       MyCallbacks[0].Callback := ProcessHerd;
       SetLength(MyCallbacks[0].ObjectList, 0);

       XMLReader := TXMLReader.create(@MyCallbacks, 'herds11.xml', nil);

       XMLReader.Run();
       XMLReader.destroy();

       for I := 0 to length( MyCallbacks ) - 1 do
         begin
           for J := 0 to length( MyCallbacks[I].ObjectList ) - 1 do
             begin
               dbcout( TxmlHerd(MyCallbacks[I].ObjectList[J]).WriteContents(), true );
             end;
         end;

    end;


////////////////////////////////////////////////////////////////////////////////
//  From here down is the code for handling the functionality of the
//  second button on the page.
//////////////////////////////////////////////////////////////////////////


////////////////////////////////////////////////////////////////////////////////
//  These are the implementations of all of the callback functions seen in the
//  const declaration above of "constStatMethodTypes"
//
//  NOTE:  Anything returned by these callback fuctions is placed, by
//         the XMLReader object, into the array of records passed
//         to it prior to calling this callback.. As a result, the
//         XMLReader returns these updated arrays back to the calling
//         code for it's use.  Notice, in some of these functions, which
//         are nested, how they check this information and add it to their
//         own, creating objects to return.  See the XMLCallbacks record
//         definition in XMLReader for more information about the
//         structure of this data.
///////////////////////////////////////////////////////////////////////
function ProcessUniformStatMethod( Element:Pointer; Sdew:TSdew ):TgenericStatMethod;
  var
    statMethod: TgenericStatMethod;
    a:String;
    b:String;
  begin
    statMethod := nil;

    a := Sdew.GetElementContents(Sdew.GetElementByName( Element, 'a' ));
    b := Sdew.GetElementContents(Sdew.GetElementByName( Element, 'b' ));
    statMethod := TgenericStatMethod( TUniform.create( StrToFloat(a), StrToFloat(b), nil ) );

    result := statMethod;
  end;

function ProcessGammaStatMethod( Element:Pointer; Sdew:TSdew ):TgenericStatMethod;
  var
    statMethod: TgenericStatMethod;
    alpha:String;
    beta:String;
  begin
    statMethod := nil;

    alpha := Sdew.GetElementContents(Sdew.GetElementByName( Element, 'alpha' ));
    beta := Sdew.GetElementContents(Sdew.GetElementByName( Element, 'beta' ));
    statMethod := TgenericStatMethod( TGamma.create( StrToFloat(alpha), StrToFloat(beta), nil ) );

    result := statMethod;
  end;

function ProcessBetaPertStatMethod( Element:Pointer; Sdew:TSdew ):TgenericStatMethod;
  var
    statMethod: TgenericStatMethod;
    min:String;
    max:String;
    mode:String;
  begin
    statMethod := nil;

    min := Sdew.GetElementContents(Sdew.GetElementByName( Element, 'min' ));
    max := Sdew.GetElementContents(Sdew.GetElementByName( Element, 'max' ));
    mode := Sdew.GetElementContents(Sdew.GetElementByName( Element, 'mode' ));
    statMethod := TgenericStatMethod( TBetaPert.create( StrToFloat(min), StrToFloat(max), StrToFloat(mode), nil ) );

    result := statMethod;
  end;

function ProcessPiecewiseStatMethod( Element:Pointer; Sdew:TSdew ):TgenericStatMethod;
  var
    statMethod: TgenericStatMethod;
    Pieces:pieceListArray;
    I:Integer;
    Count:Integer;
    value:String;
    P:String;
    Index:Integer;
    hadError:bool;
    pairs:Integer;
  begin
    statMethod := nil;

    Count := Sdew.GetElementCount( Element);
    if (( Count mod 2 ) = 0 ) then
      begin
        Index := 0;
        pairs := Count div 2;
        SetLength( Pieces, pairs );
        hadError := false;
        For I := 1 to pairs do
          begin
            if ( String(Sdew.GetElementNameByIndex( Element, Index ) ) = 'value' ) then
              begin
                value := Sdew.GetElementContents(Sdew.GetElementByIndex( Element, Index ));
                if ( String( Sdew.GetElementNameByIndex( Element, Index + 1) ) = 'p' ) then
                  begin
                    P := Sdew.GetElementContents(Sdew.GetElementByIndex( Element, Index + 1 ));
                    Index := Index + 2;
                    Pieces[I - 1].value := StrToFloat(value);
                    Pieces[I - 1].p := StrToFloat(P);
                  end
                else
                  begin
                    hadError := true;
                    Application.MessageBox('The piecewise statistics method in this file has invalid parameters','ERROR');
                  end;
              end
            else
              begin
                hadError := true;
                Application.MessageBox('The piecewise statistics method in this file has invalid parameters','ERROR');
              end;
          end;

          if ( not hadError ) then
            begin
              statMethod := TgenericStatMethod( TPiecewise.create(Pieces, nil) );
            end;
      end
    else
      begin
        Application.MessageBox('The piecewise statistics method in this file has an invalid number of parameters','ERROR');
      end;

    result := statMethod;
  end;

function ProcessTriangularStatMethod( Element:Pointer; Sdew:TSdew ):TgenericStatMethod;
  var
    statMethod: TgenericStatMethod;
    a:String;
    b:String;
    c:String;
  begin
    statMethod := nil;

    a := Sdew.GetElementContents(Sdew.GetElementByName( Element, 'a' ));
    b := Sdew.GetElementContents(Sdew.GetElementByName( Element, 'b' ));
    c := Sdew.GetElementContents(Sdew.GetElementByName( Element, 'c' ));
    statMethod := TgenericStatMethod( TTriangular.create( StrToFloat(a), StrToFloat(b), StrToFloat(c), nil ) );

    result := statMethod;
  end;

function ProcessLogisticsStatMethod( Element:Pointer; Sdew:TSdew ):TgenericStatMethod;
  var
    statMethod: TgenericStatMethod;
    location:String;
    scale:String;
  begin
    statMethod := nil;

    location := Sdew.GetElementContents(Sdew.GetElementByName( Element, 'location' ));
    scale := Sdew.GetElementContents(Sdew.GetElementByName( Element, 'scale' ));
    statMethod := TgenericStatMethod( TLogistics.create( StrToFloat(location), StrToFloat(scale), nil ) );

    result := statMethod;
  end;

function ProcessWeibullStatMethod( Element:Pointer; Sdew:TSdew ):TgenericStatMethod;
  var
    statMethod: TgenericStatMethod;
    alpha:String;
    beta:String;
  begin
    statMethod := nil;

    alpha := Sdew.GetElementContents(Sdew.GetElementByName( Element, 'alpha' ));
    beta := Sdew.GetElementContents(Sdew.GetElementByName( Element, 'beta' ));
    statMethod := TgenericStatMethod( TWeibull.create( StrToFloat(alpha), StrToFloat(beta), nil ) );

    result := statMethod;
  end;

function ProcessPointStatMethod( Element:Pointer; Sdew:TSdew ):TgenericStatMethod;
  var
    statMethod: TgenericStatMethod;
    _point:String;
  begin
    statMethod := nil;

    _point := Sdew.GetElementContents( Element );
    statMethod := TgenericStatMethod( TPoint.create( StrToFloat(_point), nil ) );

    result := statMethod;
  end;

function ProcessGaussianStatMethod( Element:Pointer; Sdew:TSdew ):TgenericStatMethod;
  var
    statMethod: TgenericStatMethod;
    mean:String;
    stddev:String;
  begin
    statMethod := nil;

    mean := Sdew.GetElementContents(Sdew.GetElementByName( Element, 'mean' ));
    stddev := Sdew.GetElementContents(Sdew.GetElementByName( Element, 'stddev' ));
    statMethod := TgenericStatMethod( TGaussian.create( StrToFloat(mean), StrToFloat(stddev), nil ) );

    result := statMethod;
  end;

function ProcessLoglogisticsStatMethod( Element:Pointer; Sdew:TSdew ):TgenericStatMethod;
  var
    statMethod: TgenericStatMethod;
    location:String;
    scale:String;
  begin
    statMethod := nil;

    location := Sdew.GetElementContents(Sdew.GetElementByName( Element, 'location' ));
    scale := Sdew.GetElementContents(Sdew.GetElementByName( Element, 'scale' ));
    statMethod := TgenericStatMethod( TLoglogistics.create( StrToFloat(location), StrToFloat(scale), nil ) );

    result := statMethod;
  end;

function ProcessPearson5StatMethod( Element:Pointer; Sdew:TSdew ):TgenericStatMethod;
  var
    statMethod: TgenericStatMethod;
    alpha:String;
    beta:String;
  begin
    statMethod := nil;

    alpha := Sdew.GetElementContents(Sdew.GetElementByName( Element, 'alpha' ));
    beta := Sdew.GetElementContents(Sdew.GetElementByName( Element, 'beta' ));
    statMethod := TgenericStatMethod( TPearson5.create( StrToFloat(alpha), StrToFloat(beta), nil ) );

    result := statMethod;
  end;

function ProcessLognormalStatMethod( Element:Pointer; Sdew:TSdew ):TgenericStatMethod;
  var
    statMethod: TgenericStatMethod;
    zeta:String;
    sigma:String;
  begin
    statMethod := nil;

    zeta := Sdew.GetElementContents(Sdew.GetElementByName( Element, 'zeta' ));
    sigma := Sdew.GetElementContents(Sdew.GetElementByName( Element, 'sigma' ));
    statMethod := TgenericStatMethod( TLognormal.create( StrToFloat(zeta), StrToFloat(sigma), nil ) );

    result := statMethod;
  end;

function ProcessBetaStatMethod( Element:Pointer; Sdew:TSdew ):TgenericStatMethod;
  var
    statMethod: TgenericStatMethod;
    alpha:String;
    beta:String;
    location:String;
    scale:String;
  begin
    statMethod := nil;

    alpha := Sdew.GetElementContents(Sdew.GetElementByName( Element, 'alpha' ));
    beta := Sdew.GetElementContents(Sdew.GetElementByName( Element, 'beta' ));
    location := Sdew.GetElementContents(Sdew.GetElementByName( Element, 'location' ));
    scale := Sdew.GetElementContents(Sdew.GetElementByName( Element, 'scale' ));
    statMethod := TgenericStatMethod( TBeta.create( StrToFloat(alpha), StrToFloat(beta), StrToFloat(location), StrToFloat(scale), nil ) );

    result := statMethod;
  end;

function ProcessExponentialStatMethod( Element:Pointer; Sdew:TSdew ):TgenericStatMethod;
  var
    statMethod: TgenericStatMethod;
    mean:String;
  begin
    statMethod := nil;

    mean := Sdew.GetElementContents(Sdew.GetElementByName( Element, 'mean' ));
    statMethod := TgenericStatMethod( TExponential.create( StrToFloat(mean), nil ) );

    result := statMethod;
  end;

function ProcessStatMethods( Element: Pointer; Sdew: TSdew; extData:Pointer ):TObject;
  var
    statMethod: TgenericStatMethod;
    I:Integer;
    name:String;
    sFunc:statMethodFunc;
  begin
    statMethod := nil;
    sFunc := nil;
    name := Sdew.GetElementName(Element);

    for I := 0 to 13 do
      begin
        if ( constStatMethodTypes[i].name = name ) then
          begin
            sFunc := constStatMethodTypes[i].sFunc;
            break;
          end;
      end;

    if ( Assigned( sFunc ) ) then
      statMethod := sFunc( Element, Sdew );

    result := statMethod;
  end;

function ProcessPeriodModels( Element: Pointer; Sdew: TSdew; extData:Pointer ):TObject;
  var
    statMethodCallbacks : CallbackArray;
    tempXMLReader: TXMLReader;
    I:Integer;
    periodName: String;
    periodObject: TDiseasePeriod;
    units:  TUnits;
    statMethod:TgenericStatMethod;
  begin
    periodObject := nil;
    SetLength( statMethodCallbacks, 14 );
    for I := 0 to 13 do
      begin
        statMethodCallbacks[I].Tag := constStatMethods[I];
        statMethodCallbacks[I].Callback := ProcessStatMethods;
        SetLength(statMethodCallbacks[I].ObjectList,0);
      end;

    periodName := Sdew.GetElementName(Element);
    units := TUnits.create((Sdew.GetElementContents( Sdew.GetElementByName( Sdew.GetElementByName(Element, 'units'), 'xdf:unit') )));

///////////////////////////////////////////////////////////////////////////////
// This is an example of how to nest the XMLReaders on the same file so that
// nested XML tags can be processed and their data kept properly associated.
//////////////
    tempXMLReader := TXMLReader.create( @statMethodCallbacks, Sdew, Element, nil );
    tempXMLReader.Run();
    tempXMLReader.destroy();

    for I := 0 to 13 do
      begin
        if (length(statMethodCallbacks[I].ObjectList) > 0 ) then
          begin
            statMethod := TgenericStatMethod(statMethodCallbacks[I].ObjectList[0]);
            if ( Assigned( statMethod ) ) then
              periodObject := TDiseasePeriod.create( statMethod, statMethod.GetType(), units );
            break;
          end;
      end;
    result := periodObject;

  end;

function ProcessDiseaseModels( Element: Pointer; Sdew: TSdew; extData:Pointer ):TObject;
  var
    periodCallbacks : CallbackArray;
    tempXMLReader: TXMLReader;
    diseaseModel:TDiseaseModel;
    immunityPeriod:TImmunityPeriod;
    latentPeriod:TLatentPeriod;
    infectiousSubClinicalPeriod:TInfectiousSubClinicalPeriod;
    infectiousClinicalPeriod:TInfectiousClinicalPeriod;
    prodType:String;
  begin
    diseaseModel := nil;
    immunityPeriod := nil;
    latentPeriod := nil;
    infectiousSubClinicalPeriod := nil;
    infectiousClinicalPeriod := nil;

    SetLength(periodCallbacks, 4);
    periodCallbacks[0].Tag := 'latent-period';
    periodCallbacks[0].Callback := ProcessPeriodModels;
    SetLength(periodCallbacks[0].ObjectList,0);

    periodCallbacks[1].Tag := 'infectious-subclinical-period';
    periodCallbacks[1].Callback := ProcessPeriodModels;
    SetLength(periodCallbacks[1].ObjectList,0);

    periodCallbacks[2].Tag := 'infectious-clinical-period';
    periodCallbacks[2].Callback := ProcessPeriodModels;
    SetLength(periodCallbacks[2].ObjectList,0);

    periodCallbacks[3].Tag := 'immunity-period';
    periodCallbacks[3].Callback := ProcessPeriodModels;
    SetLength(periodCallbacks[3].ObjectList,0);

    prodType := Sdew.GetElementAttribute( Element, 'production-type' );

    tempXMLReader := TXMLReader.create( @periodCallbacks, Sdew, Element, nil );
    tempXMLReader.Run();
    tempXMLReader.destroy();

    if ( length(periodCallbacks[0].ObjectList) > 0 ) then latentPeriod := TLatentPeriod(periodCallbacks[0].ObjectList[0]);
    if ( length(periodCallbacks[1].ObjectList) > 0 ) then infectiousSubClinicalPeriod := TInfectiousSubClinicalPeriod(periodCallbacks[1].ObjectList[0]);
    if ( length(periodCallbacks[2].ObjectList) > 0 ) then infectiousClinicalPeriod := TInfectiousClinicalPeriod(periodCallbacks[2].ObjectList[0]);
    if ( length(periodCallbacks[3].ObjectList) > 0 ) then immunityPeriod := TImmunityPeriod(periodCallbacks[3].ObjectList[0]);
///////////////
// NOTE
//  Notice here how the results of the callback functions are used to create an
//  object to use that data later.  (some of these results may also be
//  objects created in the nested callbacks....)
////
    if ( Assigned(latentPeriod) AND Assigned(infectiousSubClinicalPeriod) AND Assigned(infectiousClinicalPeriod) AND Assigned(immunityPeriod) ) then
      diseaseModel := TDiseaseModel.create( immunityPeriod, latentPeriod, infectiousSubClinicalPeriod, infectiousClinicalPeriod, prodType )
    else
      Application.MessageBox(PChar('This disease model, ' + prodType = ', does not contain all the necessary period elements'), 'ERROR');

    result := diseaseModel;
  end;

function ProcessModels( Element: Pointer; Sdew: TSdew; extData:Pointer ):TObject;
  var
    diseaseModelCallbacks : CallbackArray;
    tempXMLReader: TXMLReader;
    diseaseModel:TDiseaseModel;
    I:Integer;
  begin
    diseaseModel := nil;
    SetLength(diseaseModelCallbacks, 1);
    diseaseModelCallbacks[0].Tag := 'disease-model';
    diseaseModelCallbacks[0].Callback := ProcessDiseaseModels;
    SetLength(diseaseModelCallbacks[0].ObjectList,0);

    tempXMLReader := TXMLReader.create( @diseaseModelCallbacks, Sdew, Element, nil );
    tempXMLReader.Run();
    tempXMLReader.destroy();

    if ( length(diseaseModelCallbacks[0].ObjectList) > 0 ) then
      for I := 0 to length(diseaseModelCallbacks[0].ObjectList) - 1 do
        begin
          TDiseaseModel(diseaseModelCallbacks[0].ObjectList[I]).WriteContents();
        end;

    result := diseaseModel
  end;

///////////////////////////////////////////////////////////////////////////////
//  This function begins the processing of the disease-model XML file and is
//  called by the clicking of the second button on the demo page.
/////////////////////////////////////////////////////////////////////////
procedure TForm1.Button3Click(Sender: TObject);
    var
       XMLReader: TXMLReader;
       MyCallbacks: CallbackArray;
       I: Integer;
       J: Integer;
  	begin
       SetLength( MyCallbacks, 1);
       MyCallbacks[0].Tag := 'models';
       MyCallbacks[0].Callback := ProcessModels;
       SetLength(MyCallbacks[0].ObjectList, 0);

       XMLReader := TXMLReader.create(@MyCallbacks, 'pdfSamples.xml', nil);

       XMLReader.Run();
       XMLReader.destroy();

       for I := 0 to length( MyCallbacks ) - 1 do
         begin
           for J := 0 to length( MyCallbacks[I].ObjectList ) - 1 do
             begin
               //TDiseaseModel(MyCallbacks[I].ObjectList[J]).WriteContents();
             end;
         end;
end;

initialization
 setDEBUGGING( true );

end.


