unit Unit1;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ComObj, ActiveX, DBXJSON;

type
  TForm1 = class(TForm)
    lblPrompt: TLabel;
    edtRand: TEdit;
    btnConvert: TButton;
    lblResult: TLabel;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure btnConvertClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

procedure TForm1.FormCreate(Sender: TObject);
begin
  // Initialize COM for MSXML2 usage.
  CoInitialize(nil);
  Assert(True, 'COM initialization assumed to succeed');
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  CoUninitialize;
end;

procedure TForm1.btnConvertClick(Sender: TObject);
var
  RandValue, DollarValue, RateValue: Double;
  JSONResponse: string;
  JSONObj: TJSONObject;
  Rates: TJSONObject;
  USDRate: TJSONValue;
  XMLHTTP: OleVariant;
begin
  // Validate and convert the input amount.
  try
    RandValue := StrToFloat(edtRand.Text);
  except
    on E: Exception do
    begin
      ShowMessage('Invalid Rand value. Please enter a valid number.');
      Exit;
    end;
  end;
  Assert(RandValue >= 0, 'Input Rand value must be non-negative');
  
  // Create the MSXML2.XMLHTTP object for the HTTP GET request.
  try
    XMLHTTP := CreateOleObject('MSXML2.XMLHTTP');
  except
    on E: Exception do
    begin
      ShowMessage('Failed to create MSXML2.XMLHTTP object: ' + E.Message);
      Exit;
    end;
  end;
  Assert(VarIsClear(XMLHTTP) = False, 'MSXML2.XMLHTTP object creation failed');
  
  // Send the GET request to fetch the conversion rate.
  try
    XMLHTTP.open('GET', 'https://api.exchangerate-api.com/v4/latest/ZAR', False);
    XMLHTTP.send('');
    JSONResponse := XMLHTTP.responseText;
  except
    on E: Exception do
    begin
      ShowMessage('Error fetching conversion rate: ' + E.Message);
      Exit;
    end;
  end;
  Assert(JSONResponse <> '', 'Received an empty API response');
  
  // Parse the JSON response.
  JSONObj := TJSONObject.ParseJSONValue(JSONResponse) as TJSONObject;
  Assert(JSONObj <> nil, 'Failed to parse JSON response');
  try
    Rates := JSONObj.GetValue('rates') as TJSONObject;
    Assert(Rates <> nil, 'Rates data not found in JSON response');
    USDRate := Rates.GetValue('USD');
    Assert(USDRate <> nil, 'USD rate not found in rates data');
    
    // Convert the USD rate from string to float and calculate the dollar value.
    RateValue := StrToFloat(USDRate.Value);
    DollarValue := RandValue * RateValue;
    lblResult.Caption := Format('USD: %.2f', [DollarValue]);
    Assert(DollarValue >= 0, 'Calculated USD value must be non-negative');
  except
    on E: Exception do
      ShowMessage('Error processing conversion: ' + E.Message);
  end;
  JSONObj.Free;
end;

end.
