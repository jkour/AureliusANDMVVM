unit View.Frame.CustomerDetails;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants, 
  FMX.Types, FMX.Graphics, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.StdCtrls,
  FMX.Controls.Presentation, FMX.Edit, FMX.Objects;

type
  TFrameCustomerDetails = class(TFrame)
    ImagePhoto: TImage;
    EditLastName: TEdit;
    LabelLastName: TLabel;
    EditFirstName: TEdit;
    LabelFirstName: TLabel;
    ButtonLoadImage: TButton;
    ButtonCancel: TButton;
    ButtonSave: TButton;
  private
    { Private declarations }
  public
    { Public declarations }
  end;

implementation

{$R *.fmx}

end.
