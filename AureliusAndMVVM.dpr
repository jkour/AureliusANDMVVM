program AureliusAndMVVM;

uses
  System.StartUpCopy,
  FMX.Forms,
  View.Main in 'Views\View.Main.pas' {FormMain},
  View.Frame.CustomerDetails in 'Views\View.Frame.CustomerDetails.pas' {FrameCustomerDetails: TFrame},
  Core.Database.Entities in 'Core\Core.Database.Entities.pas',
  Core.Database.Aurelius.Types in 'Core\Core.Database.Aurelius.Types.pas',
  Core.Database.Aurelius in 'Core\Core.Database.Aurelius.pas',
  Model.Main.Types in 'Models\Model.Main.Types.pas',
  Model.Main in 'Models\Model.Main.pas',
  ViewModel.Main.Types in 'ViewModels\ViewModel.Main.Types.pas',
  ViewModel.Main in 'ViewModels\ViewModel.Main.pas',
  ViewModel.Types in 'ViewModels\ViewModel.Types.pas',
  Model.Database in 'Models\Model.Database.pas',
  Core.Helpers in 'Core\Core.Helpers.pas',
  ViewModel.AddEditCustomer.Types in 'ViewModels\ViewModel.AddEditCustomer.Types.pas',
  ViewModel.AddEditCustomer in 'ViewModels\ViewModel.AddEditCustomer.pas',
  View.AddEditCustomer in 'Views\View.AddEditCustomer.pas' {FormAddEditCustomer};

{$R *.res}

var
  mainModel: IModelMain;
  viewModel: IViewModelMain;
  viewMain: TFormMain;
begin
  mainModel:=createModelMain(database);
  viewModel:=createViewModelMain(mainModel);
  Application.Initialize;
  viewMain:=TFormMain.Create(Application);
  viewMain.setViewModel(viewModel);
  Application.MainForm:=viewMain;
  viewMain.Show;
  Application.Run;
end.
