unit Unit1;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs,
  System.Notification, FMX.Controls.Presentation, FMX.StdCtrls;

type
  TForm1 = class(TForm)
    NotificationCenter1: TNotificationCenter;
    SetNumberButton: TButton;
    ScheduleNotificationButton: TButton;
    procedure SetNumberButtonClick(Sender: TObject);
    procedure ScheduleNotificationButtonClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.fmx}

procedure TForm1.ScheduleNotificationButtonClick(Sender: TObject);
var
  MyNotification: TNotification;
begin
  MyNotification := NotificationCenter1.CreateNotification;
  try
    MyNotification.Name := 'NotificationName';
    MyNotification.AlertBody := 'Delphi Notification fired in 3 seconds.';
    // Fired in 3 seconds
    MyNotification.FireDate := Now + EncodeTime(0, 0, 3, 0);
    // Send notification to the notification center
    NotificationCenter1.ScheduleNotification(MyNotification);
  finally
    MyNotification.Free;
  end;
end;

procedure TForm1.SetNumberButtonClick(Sender: TObject);
var
  MyNotification: TNotification;
begin
  // Create an instance of TNotification
  MyNotification := NotificationCenter1.CreateNotification;
  try
      // --- your code goes here ---
      // Set the icon or notification number
      MyNotification.Number :=18;
      // Set the alert message
      MyNotification.AlertBody := 'Delphi Notification with Number 18.';
      // Note: You must send the notification to the notification center for the Icon Badge Number to be displayed.
      NotificationCenter1.PresentNotification(MyNotification);
  finally
    MyNotification.Free;
  end;
end;

end.
