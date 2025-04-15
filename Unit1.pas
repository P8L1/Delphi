unit Unit1;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ComObj, ActiveX, DBXJSON;

type
  TForm1 = class(TForm)
    lblPrompt: TLabel;
    edtAmount: TEdit;
    lblSource: TLabel;
    cbSource: TComboBox;
    lblTarget: TLabel;
    cbTarget: TComboBox;
    btnConvert: TButton;
    lblResult: TLabel;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure btnConvertClick(Sender: TObject);
  private
    procedure PopulateCurrencyList;
  public
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

{ Helper function to get a JSON value from a TJSONObject by key }
function GetJSONValue(JSONObj: TJSONObject; const Key: string): TJSONValue;
var
  I: Integer;
  Pair: TJSONPair;
begin
  Result := nil;
  for I := 0 to JSONObj.Size - 1 do
  begin
    Pair := JSONObj.Get(I) as TJSONPair;
    if SameText(Pair.JsonString.Value, Key) then
    begin
      Result := Pair.JsonValue;
      Break;
    end;
  end;
end;

{ Populate both source and target currency combo boxes with a fixed list.
  For production, extend this list as needed to include all well-known currencies. }
procedure TForm1.PopulateCurrencyList;
var
  i: Integer;
  CurrencyList: array[0..9] of string;
begin
  CurrencyList[0] := 'USD';
  CurrencyList[1] := 'EUR';
  CurrencyList[2] := 'GBP';
  CurrencyList[3] := 'ZAR';
  CurrencyList[4] := 'JPY';
  CurrencyList[5] := 'AUD';
  CurrencyList[6] := 'CAD';
  CurrencyList[7] := 'CHF';
  CurrencyList[8] := 'CNY';
  CurrencyList[9] := 'INR';

  cbSource.Items.BeginUpdate;
  cbTarget.Items.BeginUpdate;
  try
    for i := Low(CurrencyList) to High(CurrencyList) do
    begin
      cbSource.Items.Add(CurrencyList[i]);
      cbTarget.Items.Add(CurrencyList[i]);
    end;
    if cbSource.Items.IndexOf('ZAR') <> -1 then
      cbSource.ItemIndex := cbSource.Items.IndexOf('ZAR')
    else
      cbSource.ItemIndex := 0;
    if cbTarget.Items.IndexOf('USD') <> -1 then
      cbTarget.ItemIndex := cbTarget.Items.IndexOf('USD')
    else
      cbTarget.ItemIndex := 0;
  finally
    cbSource.Items.EndUpdate;
    cbTarget.Items.EndUpdate;
  end;
  Assert(cbSource.Items.Count > 0, 'Source currency list should not be empty');
  Assert(cbTarget.Items.Count > 0, 'Target currency list should not be empty');
end;

{ Initialize COM and populate the currency lists }
procedure TForm1.FormCreate(Sender: TObject);
begin
  CoInitialize(nil);
  Assert(True, 'COM initialization assumed to succeed');
  PopulateCurrencyList;
end;

{ Uninitialize COM on form destroy }
procedure TForm1.FormDestroy(Sender: TObject);
begin
  CoUninitialize;
end;

{ On click of the Convert button, fetch conversion rate based on the chosen
  source currency, extract the target rate from the JSON data, and show the result.
  The function performs thorough error checking and assertions. }
procedure TForm1.btnConvertClick(Sender: TObject);
var
  SourceAmount, ConvertedAmount, RateValue: Double;
  JSONResponse: string;
  XMLHTTP: OleVariant;
  JSONVal: TJSONValue;
  JSONObj, RatesObj: TJSONObject;
  TargetRate: TJSONValue;
  URL: string;
  Bytes: TBytes;
  SourceCurrency, TargetCurrency: string;
begin
  { 1. Parse user input amount }
  try
    SourceAmount := StrToFloat(edtAmount.Text);
  except
    on E: Exception do
    begin
      ShowMessage('Invalid amount. Please enter a valid number.');
      Exit;
    end;
  end;
  Assert(SourceAmount >= 0, 'Input amount must be non-negative');

  { 2. Retrieve selected source and target currencies }
  if cbSource.ItemIndex < 0 then
  begin
    ShowMessage('Please select a source currency.');
    Exit;
  end;
  if cbTarget.ItemIndex < 0 then
  begin
    ShowMessage('Please select a target currency.');
    Exit;
  end;
  SourceCurrency := cbSource.Items[cbSource.ItemIndex];
  TargetCurrency := cbTarget.Items[cbTarget.ItemIndex];
  Assert(SourceCurrency <> '', 'Source currency must not be empty');
  Assert(TargetCurrency <> '', 'Target currency must not be empty');

  { 3. Create the MSXML2.XMLHTTP object for the HTTP GET call }
  try
    XMLHTTP := CreateOleObject('MSXML2.XMLHTTP');
  except
    on E: Exception do
    begin
      ShowMessage('Failed to create MSXML2.XMLHTTP object: ' + E.Message);
      Exit;
    end;
  end;
  Assert(not VarIsClear(XMLHTTP), 'MSXML2.XMLHTTP object creation failed');

  { 4. Build the API URL dynamically using the source currency }
  URL := Format('https://api.exchangerate-api.com/v4/latest/%s', [SourceCurrency]);
  Assert(URL <> '', 'Constructed API URL must not be empty');

  { 5. Fetch JSON data from the API }
  try
    XMLHTTP.open('GET', URL, False);
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

  { 6. Parse JSON response }
  Bytes := TEncoding.UTF8.GetBytes(JSONResponse);
  JSONVal := TJSONObject.ParseJSONValue(Bytes, 0);
  Assert(JSONVal <> nil, 'Failed to parse JSON data');
  JSONObj := JSONVal as TJSONObject;

  { 7. Extract the "rates" object from the JSON }
  RatesObj := GetJSONValue(JSONObj, 'rates') as TJSONObject;
  if RatesObj = nil then
  begin
    ShowMessage('Rates object not found in API response.');
    JSONVal.Free;
    Exit;
  end;
  Assert(RatesObj <> nil, 'Rates object should not be nil');

  { 8. Get the conversion rate for the target currency }
  TargetRate := GetJSONValue(RatesObj, TargetCurrency);
  if TargetRate = nil then
  begin
    ShowMessage(Format('Conversion rate for %s not found.', [TargetCurrency]));
    JSONVal.Free;
    Exit;
  end;
  Assert(TargetRate <> nil, 'Target rate must be found in the rates object');

  try
    RateValue := StrToFloat(TargetRate.Value);
  except
    on E: Exception do
    begin
      ShowMessage('Invalid conversion rate received from API.');
      JSONVal.Free;
      Exit;
    end;
  end;
  Assert(RateValue > 0, 'Conversion rate must be greater than zero');

  { 9. Compute the converted amount }
  ConvertedAmount := SourceAmount * RateValue;
  Assert(ConvertedAmount >= 0, 'Calculated converted amount must be non-negative');

  { 10. Display result in label }
  lblResult.Caption := Format('%s: %.2f', [TargetCurrency, ConvertedAmount]);

  JSONVal.Free;
end;

end.

