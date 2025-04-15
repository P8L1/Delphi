unit Unit1;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, IdHTTP, IdSSLOpenSSL, DBXJSON;

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
    FHTTP: TIdHTTP;
    FSSL: TIdSSLIOHandlerSocketOpenSSL;
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

procedure TForm1.FormCreate(Sender: TObject);
begin
  // Create and initialize the HTTP client and SSL handler.
  FHTTP := TIdHTTP.Create(Self);
  Assert(FHTTP <> nil, 'Failed to create HTTP client');
  FSSL := TIdSSLIOHandlerSocketOpenSSL.Create(FHTTP);
  Assert(FSSL <> nil, 'Failed to create SSL IOHandler');
  FHTTP.IOHandler := FSSL;
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  // Clean up allocated resources.
  FreeAndNil(FHTTP);
  // Note: FSSL is owned by FHTTP and is freed automatically.
end;

procedure TForm1.btnConvertClick(Sender: TObject);
var
  RandValue, DollarValue, RateValue: Double;
  JSONResponse: string;
  JSONObject: TJSONObject;
  Rates: TJSONObject;
  USDRate: TJSONValue;
begin
  Assert(FHTTP <> nil, 'HTTP client not initialized');

  // Convert the input string to a floating-point value.
  try
    RandValue := StrToFloat(edtRand.Text);
  except
    on E: Exception do
    begin
      ShowMessage('Invalid Rand value. Please enter a valid number.');
      Exit;
    end;
  end;

  // Fetch real-time conversion rate from the API.
  try
    JSONResponse := FHTTP.Get('https://api.exchangerate-api.com/v4/latest/ZAR');
  except
    on E: Exception do
    begin
      ShowMessage('Error fetching conversion rate: ' + E.Message);
      Exit;
    end;
  end;
  Assert(JSONResponse <> '', 'Received an empty API response');

  // Parse the JSON response.
  JSONObject := TJSONObject.ParseJSONValue(JSONResponse) as TJSONObject;
  Assert(JSONObject <> nil, 'Failed to parse JSON response');
  try
    Rates := JSONObject.GetValue('rates') as TJSONObject;
    Assert(Rates <> nil, 'Rates data not found in JSON response');
    USDRate := Rates.GetValue('USD');
    Assert(USDRate <> nil, 'USD rate not found in rates data');

    // Convert the JSON string value to a floating-point rate.
    RateValue := StrToFloat(USDRate.Value);
    DollarValue := RandValue * RateValue;
    lblResult.Caption := Format('USD: %.2f', [DollarValue]);
  except
    on E: Exception do
      ShowMessage('Error processing conversion: ' + E.Message);
  end;
  JSONObject.Free;
end;

end.
