unit Unit1;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs,
  System.Notification, FMX.Controls.Presentation, FMX.StdCtrls, System.IOUtils;

type
  TForm1 = class(TForm)
    NotificationCenter1: TNotificationCenter;
    SetNumberButton: TButton;
    ScheduleNotificationButton: TButton;
    RepeatedNotificationButton: TButton;
    CancelNotificationButton: TButton;
    PresentNotificationButton: TButton;
    SoundNotificationButton: TButton;
    SoundNotificationButton2: TButton;
    procedure SetNumberButtonClick(Sender: TObject);
    procedure ScheduleNotificationButtonClick(Sender: TObject);
    procedure RepeatedNotificationButtonClick(Sender: TObject);
    procedure CancelNotificationButtonClick(Sender: TObject);
    procedure PresentNotificationButtonClick(Sender: TObject);
    procedure SoundNotificationButtonClick(Sender: TObject);
    procedure SoundNotificationButton2Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.fmx}

procedure TForm1.CancelNotificationButtonClick(Sender: TObject);
begin
  NotificationCenter1.CancelNotification('NotificationName');
end;

procedure TForm1.PresentNotificationButtonClick(Sender: TObject);
var
  MyNotification: TNotification;
begin
  MyNotification := NotificationCenter1.CreateNotification;
  try
    MyNotification.Title := 'NotificationTitle';
    MyNotification.Name := 'NotificationName';
    MyNotification.AlertBody := 'Delphi for your mobile device is here!';
    // Set Icon Badge Number (for iOS) or message number (for Android) as well
    MyNotification.Number := 15;
    MyNotification.EnableSound := False;
    // Send message to the notification center
    NotificationCenter1.PresentNotification(MyNotification);
  finally
    MyNotification.Free;
  end;
end;

procedure TForm1.RepeatedNotificationButtonClick(Sender: TObject);
var
  MyNotification: TNotification;
begin
  MyNotification := NotificationCenter1.CreateNotification;
  try
    MyNotification.Title := 'NotificationTitle';
    MyNotification.Name := 'NotificationName';
    MyNotification.AlertBody := 'Repeating notification each Minute.';
    //Fired in 0 seconds
    MyNotification.FireDate := Now + EncodeTime(0, 0, 0, 0);
    //Fired in 10 seconds
    //MyNotification.FireDate := Now + EncodeTime(0, 0, 10, 0);
    //Repeated each minute
    MyNotification.RepeatInterval := TRepeatInterval.Minute;
    // Send notification to the notification center
    NotificationCenter1.ScheduleNotification(MyNotification);
  finally
    MyNotification.Free;
  end;
end;

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
      MyNotification.Name := 'NotificationName';
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

function GetSoundName: string;
begin
  {$IFDEF IOS}
    Result := 'myiOSSound.caf';
  {$ENDIF}
  {$IFDEF ANDROID}
    // Result := TPath.Combine(TPath.GetSharedDocumentsPath, 'ringtone-020-365650.mp3');
    Result := TPath.Combine(TPath.GetDocumentsPath, 'ringtone-020-365650.mp3'); // https://embt.atlassian.net/browse/RS-116170
                                                                                // https://quality.embarcadero.com/browse/RSP-40419

    // Result := 'android.resource://com.embarcadero.NotificationServiceApp/raw/ringtone-020-365650'; // res/raw/ringtone-020-365650.mp3
  {$ENDIF}
  {$IFDEF MSWINDOWS}
    Result := TPath.Combine(TPath.GetSharedDocumentsPath, 'ringtone-020-365650.mp3');
  {$ENDIF}
end;

procedure TForm1.SoundNotificationButton2Click(Sender: TObject);
begin
  var NotificationChannel := NotificationCenter1.CreateChannel('notification_channel_id_default_1', 'Default notification channel 1', 'Default notification channel 1');
  try
    NotificationChannel.Importance := TImportance.High;
    NotificationChannel.SoundName := 'android.resource://com.embarcadero.NotificationServiceApp/raw/ringtone-020-365650'; // For Android 8.0+
    NotificationCenter1.CreateOrUpdateChannel(NotificationChannel);

    var Notification := NotificationCenter1.CreateNotification;
    try
      Notification.Title := 'NotificationTitle';
      Notification.AlertBody := 'Notification content';
      Notification.ChannelId := 'notification_channel_id_default_1';
      Notification.EnableSound := True;
      Notification.SoundName := 'android.resource://com.embarcadero.NotificationServiceApp/raw/ringtone-020-365650'; // For Android 7.1-
      NotificationCenter1.PresentNotification(Notification);
    finally
      Notification.Free;
    end;
  finally
    NotificationChannel.Free;
  end;
end;

procedure TForm1.SoundNotificationButtonClick(Sender: TObject);
var
  MyNotification: TNotification;
begin
  MyNotification := NotificationCenter1.CreateNotification;
  try
    MyNotification.Name := 'NotificationName';
    MyNotification.AlertBody := 'User''s Sound Notification is here!';
    MyNotification.FireDate := Now;

    // MyNotification.ChannelId := 'notification_channel_id_default_1';
    MyNotification.EnableSound := True;
    MyNotification.SoundName := GetSoundName;

//    if not TFile.Exists(MyNotification.SoundName)
//      then ShowMessage('Sound file not found: ' + MyNotification.SoundName)
//      else
//        // Send message to the notification center
//        // NotificationCenter1.ScheduleNotification(MyNotification);
//        NotificationCenter1.PresentNotification(MyNotification);

    // NotificationCenter1.ScheduleNotification(MyNotification);
    NotificationCenter1.PresentNotification(MyNotification);

  finally
    MyNotification.Free;
  end;
end;

end.
