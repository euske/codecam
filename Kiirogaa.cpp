//  Kiirogaa.cpp
//

#include <stdio.h>
#include <stdlib.h>
#include <Windows.h>
#include <StrSafe.h>
#include "Resource.h"

#pragma comment(lib, "user32.lib")
#pragma comment(lib, "shell32.lib")
#pragma comment(lib, "comdlg32.lib")

const LPCWSTR KIIROGAA_NAME = L"Kiirogaa";
const LPCWSTR KIIROGAA_MUTEX = L"Local\\Kiirogaa";
const UINT KIIROGAA_ICON_ID = 1;

enum {
    WM_USER_ICON_EVENT = WM_USER+1,
    WM_USER_STATE_CHANGED,
    WM_USER_ICON_CHANGED,
};


//  Kiirogaa
// 
typedef struct _Kiirogaa
{
    FILE* logfp;
    HMODULE hModule;
    HHOOK hHook;
    HICON iconKiirogaaOn;
    HICON iconKiirogaaOff;
    HWND focusHWnd;
    BOOL enabled;
} Kiirogaa;

static int openLogFile(Kiirogaa* self, LPCWSTR path)
{
    self->focusHWnd = NULL;
    return _wfopen_s(&self->logfp, path, L"a");
}

// writeToLog(self, c)
static void writeToLog(Kiirogaa* self, int c)
{
    HWND focusHWnd = GetForegroundWindow();
    if (self->focusHWnd != focusHWnd) {
        self->focusHWnd = focusHWnd;
        WCHAR name[256];
        if (GetClassName(focusHWnd, name, 255)) {
            fwprintf(self->logfp, L"# %s\n", name);
        }
    }
    
    SYSTEMTIME st;
    GetLocalTime(&st);
    fwprintf(
        self->logfp, L"%04d-%02d-%02d %02d:%02d:%02d.%03d %04x\n",
        st.wYear, st.wMonth, st.wDay,
        st.wHour, st.wMinute, st.wSecond, st.wMilliseconds, c);
    fflush(self->logfp);
}


//  kiirogaaTrayWndProc
//    Handles the tray icon.
//
static LRESULT CALLBACK kiirogaaTrayWndProc(
    HWND hWnd,
    UINT uMsg,
    WPARAM wParam,
    LPARAM lParam)
{
    //fwprintf(stderr, L"msg: %x, hWnd=%p, wParam=%p\n", uMsg, hWnd, wParam);

    switch (uMsg) {
    case WM_CREATE:
    {
        // Initialization.
	CREATESTRUCT* cs = (CREATESTRUCT*)lParam;
        Kiirogaa* self = (Kiirogaa*)cs->lpCreateParams;
        {
            // Set the default item.
            HMENU menu = GetMenu(hWnd);
            if (menu != NULL) {
                menu = GetSubMenu(menu, 0);
                if (menu != NULL) {
                    SetMenuDefaultItem(menu, IDM_TOGGLE, FALSE);
                }
            }
        }
        SetWindowLongPtr(hWnd, GWLP_USERDATA, (LONG_PTR)self);

        // Set the tray icon.
        NOTIFYICONDATA nidata = {0};
        nidata.cbSize = sizeof(nidata);
        nidata.hWnd = hWnd;
        nidata.uID = KIIROGAA_ICON_ID;
        nidata.uFlags = NIF_MESSAGE;
        nidata.uCallbackMessage = WM_USER_ICON_EVENT;
        Shell_NotifyIcon(NIM_ADD, &nidata);
	return FALSE;
    }
    
    case WM_DESTROY:
    {
        // Clean up.
	LONG_PTR lp = GetWindowLongPtr(hWnd, GWLP_USERDATA);
        Kiirogaa* self = (Kiirogaa*)lp;
        if (self != NULL) {
	    // Unregister the icon.
            NOTIFYICONDATA nidata = {0};
            nidata.cbSize = sizeof(nidata);
            nidata.hWnd = hWnd;
            nidata.uID = KIIROGAA_ICON_ID;
            Shell_NotifyIcon(NIM_DELETE, &nidata);

            // Destroy the data structure.
	    SetWindowLongPtr(hWnd, GWLP_USERDATA, (LONG_PTR)NULL);
        }
        // Exit the program.
	PostQuitMessage(0);
	return FALSE;
    }

    case WM_CLOSE:
	DestroyWindow(hWnd);
	return FALSE;

    case WM_COMMAND:
    {
        // Respond to menu choices.
	LONG_PTR lp = GetWindowLongPtr(hWnd, GWLP_USERDATA);
        Kiirogaa* self = (Kiirogaa*)lp;

	switch (LOWORD(wParam)) {
        case IDM_TOGGLE:
            // Toggle enable/disable the function.
            if (self != NULL) {
                self->enabled = !(self->enabled);
                SendMessage(hWnd, WM_USER_STATE_CHANGED, 0, 0);
            }
            break;

        case IDM_SAVEAS:
            // Open a new log file.
            if (self != NULL) {
                WCHAR path[MAX_PATH] = {0};
                OPENFILENAME ofn = {0};
                ofn.lStructSize = sizeof(ofn);
                ofn.hwndOwner = hWnd;
                ofn.lpstrTitle = L"Save As";
                ofn.lpstrFilter = L"Text Files (*.log)\0*.log\0All Files (*.*)\0*.*\0\0";
                ofn.lpstrFile = path;
                ofn.nMaxFile = MAX_PATH-1;
                ofn.lpstrDefExt = L"log";
                ofn.Flags = OFN_EXPLORER;
                if (GetSaveFileName(&ofn)) {
                    openLogFile(self, path);
                }
            }
            break;

	case IDM_EXIT:
            // Exiting.
	    SendMessage(hWnd, WM_CLOSE, 0, 0);
	    break;
	}
	return FALSE;
    }

    case WM_USER_STATE_CHANGED:
    {
        // Respond to a status change.
	LONG_PTR lp = GetWindowLongPtr(hWnd, GWLP_USERDATA);
        Kiirogaa* self = (Kiirogaa*)lp;
        if (self != NULL) {
            HMENU menu = GetMenu(hWnd);
            if (menu != NULL) {
                menu = GetSubMenu(menu, 0);
                if (menu != NULL) {
                    // Check/uncheck the menu item.
                    MENUITEMINFO info = {0};
                    info.cbSize = sizeof(info);
                    info.fMask = MIIM_STATE;
                    info.fState = ((self->enabled)?
                                   MFS_CHECKED : 
                                   MFS_UNCHECKED);
                    info.fState |= MFS_DEFAULT;
                    SetMenuItemInfo(menu, IDM_TOGGLE, FALSE, &info);
                }
            }
            SendMessage(hWnd, WM_USER_ICON_CHANGED, 0, 0);
        }
        return FALSE;
    }

    case WM_USER_ICON_CHANGED:
    {
        // Change the icon and status text.
	LONG_PTR lp = GetWindowLongPtr(hWnd, GWLP_USERDATA);
        Kiirogaa* self = (Kiirogaa*)lp;
        if (self != NULL) {
            // LPARAM: active hook.
            NOTIFYICONDATA nidata = {0};
            nidata.cbSize = sizeof(nidata);
            nidata.hWnd = hWnd;
            nidata.uID = KIIROGAA_ICON_ID;
            nidata.uFlags = NIF_ICON | NIF_TIP;
            // Set the icon and status text.
            nidata.hIcon = (self->enabled)? self->iconKiirogaaOn : self->iconKiirogaaOff;
            StringCchCopy(nidata.szTip, _countof(nidata.szTip), KIIROGAA_NAME);
            Shell_NotifyIcon(NIM_MODIFY, &nidata);
        }
        return FALSE;
    }
    
    case WM_USER_ICON_EVENT:
    {
        // Respond to tray icon events.
	POINT pt;
        HMENU menu = GetMenu(hWnd);
        if (menu != NULL) {
            menu = GetSubMenu(menu, 0);
        }
	switch (lParam) {
	case WM_LBUTTONDBLCLK:
            // Double click - choose the default item.
            if (menu != NULL) {
                UINT item = GetMenuDefaultItem(menu, FALSE, 0);
                SendMessage(hWnd, WM_COMMAND, MAKEWPARAM(item, 1), NULL);
            }
	    break;
	case WM_LBUTTONUP:
	    break;
	case WM_RBUTTONUP:
            // Right click - open the popup menu.
	    if (GetCursorPos(&pt)) {
                SetForegroundWindow(hWnd);
                if (menu != NULL) {
                    TrackPopupMenu(menu, TPM_LEFTALIGN, 
                                   pt.x, pt.y, 0, hWnd, NULL);
                }
		PostMessage(hWnd, WM_NULL, 0, 0);
	    }
	    break;
	}
	return FALSE;
    }

    case WM_COPYDATA:
    {
        // Respond to a status change.
	LONG_PTR lp = GetWindowLongPtr(hWnd, GWLP_USERDATA);
        Kiirogaa* self = (Kiirogaa*)lp;
        if (self != NULL && self->enabled) {
            COPYDATASTRUCT* cds = (COPYDATASTRUCT*)lParam;
            if (cds->cbData == sizeof(KBDLLHOOKSTRUCT) &&
                cds->dwData == WM_KEYDOWN) {
                KBDLLHOOKSTRUCT* kdb = (KBDLLHOOKSTRUCT*)cds->lpData;
                BYTE keys[256] = {0};
                for (int i = 0; i < 256; i++) {
                    keys[i] = (GetAsyncKeyState(i) & 0x8000)? 0x80 : 0x00;
                }
                WCHAR c;
                if (0 < ToUnicode(kdb->vkCode, kdb->scanCode, keys, &c, 1, 0)) {
                    writeToLog(self, c);
                }
            }
        }
        return FALSE;
    }
        
    default:
	return DefWindowProc(hWnd, uMsg, wParam, lParam);
    }
}


//  KiirogaaMain
// 
int KiirogaaMain(
    HINSTANCE hInstance, 
    HINSTANCE hPrevInstance, 
    int nCmdShow,
    int argc, LPWSTR* argv)
{
    // Prevent a duplicate process.
    HANDLE mutex = CreateMutex(NULL, TRUE, KIIROGAA_MUTEX);
    if (GetLastError() == ERROR_ALREADY_EXISTS) {
	CloseHandle(mutex);
	return 0;
    }

    // Create a structure.
    Kiirogaa* kiirogaa = (Kiirogaa*) calloc(1, sizeof(Kiirogaa));
    if (kiirogaa == NULL) return 111;
    kiirogaa->logfp = stderr;

    // Parse the command line options.
    for (int i = 1; i < argc; i++) {
        if (wcscmp(argv[i], L"-l") == 0 && (i+1) < argc) {
            i++;
            openLogFile(kiirogaa, argv[i]);
        }
    }

    // Load a DLL.
    kiirogaa->hModule = LoadLibrary(L"hookey.dll");
    if (kiirogaa->hModule == NULL) {
        MessageBox(NULL, 
                   L"hookey.dll is not found.", 
                   L"Kiirogaa", 
                   MB_ICONERROR | MB_OK);
        return 111;
    }
    void (*SetHWND)(HWND hWnd) =
        (void (*)(HWND)) GetProcAddress(kiirogaa->hModule, "SetHWND");

    // Hook the thing.
    HOOKPROC hookProc = (HOOKPROC) GetProcAddress(kiirogaa->hModule, "KeyboardProcLL");
    kiirogaa->hHook = SetWindowsHookEx(WH_KEYBOARD_LL, hookProc, kiirogaa->hModule, 0);
    if (kiirogaa->hHook == NULL) {
        MessageBox(NULL, 
                   L"SetWindowsHookEx failed.", 
                   L"Kiirogaa", 
                   MB_ICONERROR | MB_OK);
    }

    // Load resources.
    kiirogaa->iconKiirogaaOn = LoadIcon(hInstance, MAKEINTRESOURCE(IDI_KIIROGAA_ON));
    if (kiirogaa->iconKiirogaaOn == NULL) return 111;
    kiirogaa->iconKiirogaaOff = LoadIcon(hInstance, MAKEINTRESOURCE(IDI_KIIROGAA_OFF));
    if (kiirogaa->iconKiirogaaOff == NULL) return 111;
    
    // Register the window class.
    ATOM atom;
    {
	WNDCLASS wc;
	ZeroMemory(&wc, sizeof(wc));
	wc.lpfnWndProc = kiirogaaTrayWndProc;
	wc.hInstance = hInstance;
	wc.hIcon = LoadIcon(NULL, IDI_APPLICATION);
	wc.hCursor = LoadCursor(NULL, IDC_ARROW);
	wc.hbrBackground = (HBRUSH)(COLOR_WINDOW+1);
        wc.lpszMenuName = MAKEINTRESOURCE(IDM_POPUPMENU);
	wc.lpszClassName = L"KiirogaaTrayWindowClass";
	atom = RegisterClass(&wc);
    }

    // Create a SysTray window.
    HWND hWnd = CreateWindowEx(
        WS_EX_NOACTIVATE,
	(LPCWSTR)atom,
	KIIROGAA_NAME,
	WS_POPUP,
	CW_USEDEFAULT, CW_USEDEFAULT,
	CW_USEDEFAULT, CW_USEDEFAULT,
	NULL, NULL, hInstance, kiirogaa);
    SetHWND(hWnd);
    UpdateWindow(hWnd);
    SendMessage(hWnd, WM_USER_STATE_CHANGED, 0, 0);
    
    // Event loop.
    MSG msg;
    while (0 < GetMessage(&msg, NULL, 0, 0)) {
        TranslateMessage(&msg);
        DispatchMessage(&msg);
    }

    // Cleanup.
    if (kiirogaa->logfp != NULL) {
        fclose(kiirogaa->logfp);
        kiirogaa->logfp = NULL;
    }
    if (kiirogaa->hHook != NULL) {
        UnhookWindowsHookEx(kiirogaa->hHook);
        kiirogaa->hHook = NULL;
    }
    FreeLibrary(kiirogaa->hModule);
    free(kiirogaa);

    return (int)msg.wParam;
}


// WinMain and wmain
#ifdef WINDOWS
int WinMain(HINSTANCE hInstance, 
	    HINSTANCE hPrevInstance, 
	    LPSTR lpCmdLine,
	    int nCmdShow)
{
    int argc;
    LPWSTR* argv = CommandLineToArgvW(GetCommandLineW(), &argc);
    return KiirogaaMain(hInstance, hPrevInstance, nCmdShow, argc, argv);
}
#else
int wmain(int argc, wchar_t* argv[])
{
    return KiirogaaMain(GetModuleHandle(NULL), NULL, 0, argc, argv);
}
#endif
