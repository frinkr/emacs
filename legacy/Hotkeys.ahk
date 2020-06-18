ToggleWindow(WinTitle)
{
    IfWinNotExist % WinTitle
        return

    IfWinActive % WinTitle
    {
        WinGet MMX, MinMax, % WinTitle
        if MMX = -1      ; If window is minimized
            WinRestore
        else
            WinMinimize
    }
    else 
        WinActivate % WinTitle
}

^`::    ; Ctrl + `
    ToggleWindow("ahk_class mintty")
    return

$^9::
    IfWinActive ahk_class Emacs
        Send, {LAlt down}`
    else
        ToggleWindow("ahk_class Emacs")
    return

$F1::    ; F1
    ToggleWindow("ahk_class Emacs")
    return

$F2::    ; F2
    ToggleWindow("ahk_class FaTTY")
    return

$F3::    ; F3
    ToggleWindow("ahk_exe chrome.exe")
    return

$F4::    ; F4
    ToggleWindow("ahk_exe devenv.exe")
    return

