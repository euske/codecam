// -*- tab-width: 4 -*-
// Hookey.cpp

#include <stdio.h>
#include <string.h>
#include <windows.h>

#pragma comment(lib, "user32.lib")

extern "C" {
    __declspec(dllexport) void SetHWND(HWND hWnd);
    __declspec(dllexport) LRESULT KeyboardProcLL(int nCode, WPARAM wParam, LPARAM lParam);
}

static HWND _hWnd = 0;

__declspec(dllexport) void SetHWND(HWND hWnd)
{
    _hWnd = hWnd;
}

__declspec(dllexport) LRESULT KeyboardProcLL(
    int nCode,
    WPARAM wParam,
    LPARAM lParam)
{
    if (nCode == HC_ACTION) {
        KBDLLHOOKSTRUCT* kdb = (KBDLLHOOKSTRUCT*)lParam;
        if (!(kdb->flags & LLKHF_INJECTED)) {
#if !NDEBUG
            fprintf(stderr, "wParam=%p, vkCode=%u, scanCode=%u, flags=0x%x\n", 
                    wParam, kdb->vkCode, kdb->scanCode, kdb->flags);
#endif
            if (_hWnd != NULL) {
                COPYDATASTRUCT cds = {0};
                cds.dwData = wParam;
                cds.cbData = sizeof(KBDLLHOOKSTRUCT);
                cds.lpData = kdb;
                SendMessage(_hWnd, WM_COPYDATA, NULL, (LPARAM)&cds);
            }
        }
    }
    return CallNextHookEx(NULL, nCode, wParam, lParam);
}
