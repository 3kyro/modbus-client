
; Build Unicode installer
Unicode True

!define APPNAME "Modbus Client"

!define EXECUTABLE_NAME "modbus-client.exe"

!define VERSIONMAJOR 0
!define VERSIONMINOR 2
!define VERSIONPATCH 0

; The name of the installer
Name "Modbus Client"

; The file to write
OutFile "modbus-client-${VERSIONMAJOR}.${VERSIONMINOR}.${VERSIONPATCH}-setup.exe"

; Request application privileges for Windows Vista
RequestExecutionLevel admin 

; The default installation directory
InstallDir "$PROGRAMFILES\Modbus Client"

; Registry key to check for directory (so if you install again, it will 
; overwrite the old one automatically)
InstallDirRegKey HKLM "Software\Modbus Client" "InstallDir"

;--------------------------------

LicenseData "LICENSE"


; Pages
Page license 
Page components
Page directory
Page instfiles

UninstPage uninstConfirm
UninstPage instfiles


;--------------------------------

; The stuff to install
Section "Modbus Client"

  SectionIn RO
  
  ; Set output path to the installation directory.
  SetOutPath $INSTDIR
  
  ; Put file there
  CreateDirectory $INSTDIR\frontend
  File "modbus-client.exe"
  File "LICENSE"
  File "sample.csv"
  File /r "app.js"
  File /r "index.html"
  
  ; Write the installation path into the registry
  WriteRegStr HKLM "SOFTWARE\Modbus Client" "InstallDir" "$INSTDIR"
  
  ; Write the uninstall keys for Windows
  WriteRegStr HKLM "Software\Microsoft\Windows\CurrentVersion\Uninstall\Modbus Client" "DisplayName" "Modbus Client"
  WriteRegStr HKLM "Software\Microsoft\Windows\CurrentVersion\Uninstall\Modbus Client" "UninstallString" '"$INSTDIR\uninstall.exe"'
  WriteRegDWORD HKLM "Software\Microsoft\Windows\CurrentVersion\Uninstall\Modbus Client" "NoModify" 1
  WriteRegDWORD HKLM "Software\Microsoft\Windows\CurrentVersion\Uninstall\Modbus Client" "NoRepair" 1
  WriteUninstaller "$INSTDIR\uninstall.exe"
  
SectionEnd

; Optional section (can be disabled by the user)
Section "Start Menu Shortcuts"

  CreateDirectory "$SMPROGRAMS\Modbus Client"
  CreateShortcut "$SMPROGRAMS\Modbus Client\Uninstall.lnk" "$INSTDIR\uninstall.exe"
  CreateShortcut "$SMPROGRAMS\Modbus Client\Modbus Client.lnk" "$INSTDIR\modbus-client.exe" "-s"

SectionEnd

;--------------------------------


; Uninstaller

Section "Uninstall"
  
  ; Remove registry keys
  DeleteRegKey HKLM "Software\Microsoft\Windows\CurrentVersion\Uninstall\Modbus Client"
  DeleteRegKey HKLM "SOFTWARE\Modbus Client"

  ; Remove files and uninstaller
  Delete $INSTDIR\example2.nsi
  Delete $INSTDIR\uninstall.exe

  ; Remove shortcuts, if any
  Delete "$SMPROGRAMS\Modbus Client\*.lnk"

  ; Remove directories
  RMDir "$SMPROGRAMS\Modbus Client"
  RMDir "$INSTDIR"

SectionEnd

