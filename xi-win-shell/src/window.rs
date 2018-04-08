// Copyright 2018 Google LLC
//
// Licensed under the Apache License, Version 2.0 (the "License");
// you may not use this file except in compliance with the License.
// You may obtain a copy of the License at
//
//     http://www.apache.org/licenses/LICENSE-2.0
//
// Unless required by applicable law or agreed to in writing, software
// distributed under the License is distributed on an "AS IS" BASIS,
// WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
// See the License for the specific language governing permissions and
// limitations under the License.

//! Creation and management of windows.

#![allow(non_snake_case)]

use std::borrow::Borrow;
use std::cell::{Cell, RefCell};
use std::mem;
use std::ptr::{null, null_mut};
use std::rc::{Rc, Weak};

use winapi::ctypes::{c_int, c_void};
use winapi::shared::basetsd::*;
use winapi::shared::dxgi::*;
use winapi::shared::dxgi1_2::*;
use winapi::shared::dxgi1_3::*;
use winapi::shared::dxgitype::*;
use winapi::shared::dxgiformat::*;
use winapi::shared::minwindef::*;
use winapi::shared::windef::*;
use winapi::shared::winerror::*;
use winapi::um::d3d11::*;
use winapi::um::d3dcommon::*;
use winapi::um::unknwnbase::*;
use winapi::um::wingdi::*;
use winapi::um::winnt::*;
use winapi::um::winuser::*;

use direct2d;
use direct2d::render_target::RenderTarget;

use Error;
use menu::Menu;
use paint::{self, PaintCtx};
use util::{OPTIONAL_FUNCTIONS, FromWide, ToWide};
use win_main::RunLoopHandle;
use win_main::XI_RUN_IDLE;

extern "system" {
    pub fn DwmFlush();
}

/// Builder abstraction for creating new windows.
pub struct WindowBuilder {
    handler: Option<Box<WinHandler>>,
    runloop: RunLoopHandle,
    dwStyle: DWORD,
    title: String,
    menu: Option<Menu>,
    present_strategy: PresentStrategy,
}

#[derive(Clone, Copy, PartialEq, Eq, Debug)]
/// It's very tricky to get smooth dynamics (especially resizing) and
/// good performance on Windows. This setting lets clients experiment
/// with different strategies.
pub enum PresentStrategy {
    /// Corresponds to the swap effect DXGI_SWAP_EFFECT_SEQUENTIAL. In
    /// testing, it causes diagonal banding artifacts with Nvidia
    /// adapters, and incremental present doesn't work. However, it
    /// is compatible with GDI (such as menus).
    Sequential,

    /// Corresponds to the swap effect DXGI_SWAP_EFFECT_FLIP_SEQUENTIAL.
    /// In testing, it seems to perform well (including allowing smooth
    /// resizing when the frame can be rendered quickly), but isn't
    /// compatible with GDI.
    Flip,

    /// Corresponds to the swap effect DXGI_SWAP_EFFECT_FLIP_SEQUENTIAL
    /// but with a redirection surface for GDI compatibility. Resize is
    /// very laggy and artifacty.
    FlipRedirect,
}

#[derive(Clone, Default)]
pub struct WindowHandle(Weak<WindowState>);

struct WindowState {
    hwnd: Cell<HWND>,
    wndproc: Box<WndProc>,
}

/// App behavior, supplied by the app. Many of the "window procedure"
/// messages map to calls to this trait. The methods are non-mut because
/// the window procedure can be called recursively; implementers are
/// expected to use `RefCell` or the like, but should be careful to keep
/// the lifetime of the borrow short.
pub trait WinHandler {
    /// Provide the handler with a handle to the window so that it can
    /// invalidate or make other requests.
    fn connect(&self, handle: &WindowHandle);

    /// Request the handler to paint the window contents. Return value
    /// indicates whether window is animating, i.e. whether another paint
    /// should be scheduled for the next animation frame.
    fn paint(&self, ctx: &mut PaintCtx) -> bool;

    #[allow(unused_variables)]
    /// Called when a menu item is selected.
    fn command(&self, id: u32) {}

    /// Called on keyboard input of a single character. This corresponds
    /// to the WM_CHAR message. Handling of text input will continue to
    /// evolve, we need to handle input methods and more.
    #[allow(unused_variables)]
    fn char(&self, ch: u32) {}

    /// Called on a key down event. This corresponds to the WM_KEYDOWN
    /// message. The key code is as WM_KEYDOWN. We'll want to add stuff
    /// like the modifier state.
    #[allow(unused_variables)]
    fn keydown(&self, vkey_code: i32) {}

    /// Called when the window is being destroyed. Note that this happens
    /// earlier in the sequence than drop (at WM_DESTROY, while the latter is
    /// WM_NCDESTROY).
    fn destroy(&self) {}
}

/// Generic handler trait for the winapi window procedure entry point.
trait WndProc {
    fn connect(&self, handle: &WindowHandle, *mut IDXGISwapChain1);

    fn window_proc(&self, hwnd: HWND, msg: UINT, wparam: WPARAM, lparam: LPARAM)
        -> Option<LRESULT>;
}

// State and logic for the winapi window procedure entry point. Note that this level
// implements policies such as the use of Direct2D for painting.
struct MyWndProc {
    handler: Box<WinHandler>,
    handle: RefCell<WindowHandle>,
    runloop: RunLoopHandle,
    d2d_factory: direct2d::Factory,
    swap_chain: Cell<*mut IDXGISwapChain1>,
    render_target: RefCell<Option<RenderTarget>>,
    present_strategy: PresentStrategy,
}

impl Default for PresentStrategy {
    fn default() -> PresentStrategy {
        // We probably want to change this, but we need GDI to work. Too bad about
        // the artifacty resizing.
        PresentStrategy::FlipRedirect
    }
}

impl MyWndProc {
    fn rebuild_render_target(&self) {
        unsafe {
            *self.render_target.borrow_mut() =
                paint::create_render_target_dxgi(&self.d2d_factory, self.swap_chain.get()).ok();
        }
    }

    // Renders but does not present.
    fn render(&self) {
        let mut tmp = self.render_target.borrow_mut();
        let rt = tmp.as_mut().unwrap();
        rt.begin_draw();
        let anim = self.handler.paint(&mut PaintCtx {
            d2d_factory: &self.d2d_factory,
            render_target: rt,
        });
        // Maybe should deal with lost device here...
        let res = rt.end_draw();
        if let Err(e) = res {
            println!("EndDraw error: {:?}", e);
        }
        if anim {
            let handle = self.handle.borrow().clone();
            self.runloop.borrow().add_idle(&handle.clone(), move || handle.invalidate());
        }
    }
}

impl WndProc for MyWndProc {
    fn connect(&self, handle: &WindowHandle, swap_chain: *mut IDXGISwapChain1) {
        *self.handle.borrow_mut() = handle.clone();
        self.handler.connect(handle);
        self.swap_chain.set(swap_chain);
    }

    fn window_proc(&self, hwnd: HWND, msg: UINT, wparam: WPARAM, lparam: LPARAM)
        -> Option<LRESULT>
    {
        //println!("wndproc msg: {}", msg);
        match msg {
            WM_ERASEBKGND => {
                Some(0)
            }
            WM_PAINT => unsafe {
                if self.render_target.borrow_mut().is_none() {
                    let rt = paint::create_render_target(&self.d2d_factory, hwnd);
                    *self.render_target.borrow_mut() = rt.ok();
                }
                self.render();
                (*self.swap_chain.get()).Present(1, 0);
                ValidateRect(hwnd, null_mut());
                Some(0)
            },
            WM_SIZE => unsafe {
                *self.render_target.borrow_mut() = None;
                let res = (*self.swap_chain.get()).ResizeBuffers(0, 0, 0, DXGI_FORMAT_UNKNOWN, 0);
                if SUCCEEDED(res) {
                    self.rebuild_render_target();
                    if self.present_strategy == PresentStrategy::Flip {
                        self.render();
                        (*self.swap_chain.get()).Present(0, 0);
                        DwmFlush();
                        ValidateRect(hwnd, null_mut());
                    } else {
                        InvalidateRect(hwnd, null_mut(), FALSE);
                    }
                } else {
                    println!("ResizeBuffers failed: 0x{:x}", res);
                }
                Some(0)
            },
            WM_COMMAND => {
                self.handler.command(wparam as u32);
                Some(0)
            }
            WM_CHAR => {
                self.handler.char(wparam as u32);
                Some(0)
            }
            WM_KEYDOWN => {
                self.handler.keydown(wparam as i32);
                Some(0)
            }
            WM_DESTROY => {
                self.handler.destroy();
                None
            }
            XI_RUN_IDLE => {
                self.runloop.borrow().run_idle();
                Some(0)
            }
            _ => None
        }
    }
}

impl WindowBuilder {
    pub fn new(runloop: RunLoopHandle) -> WindowBuilder {
        WindowBuilder {
            handler: None,
            runloop,
            dwStyle: WS_OVERLAPPEDWINDOW,
            title: String::new(),
            menu: None,
            present_strategy: Default::default(),
        }
    }

    pub fn set_handler(&mut self, handler: Box<WinHandler>) {
        self.handler = Some(handler);
    }

    pub fn set_scroll(&mut self, hscroll: bool, vscroll: bool) {
        self.dwStyle &= !(WS_HSCROLL | WS_VSCROLL);
        if hscroll {
            self.dwStyle |= WS_HSCROLL;
        }
        if vscroll {
            self.dwStyle |= WS_VSCROLL;
        }
    }

    pub fn set_title<S: Into<String>>(&mut self, title: S) {
        self.title = title.into();
    }

    pub fn set_menu(&mut self, menu: Menu) {
        self.menu = Some(menu);
    }

    pub fn set_present_strategy(&mut self, present_strategy: PresentStrategy) {
        self.present_strategy = present_strategy;
    }

    pub fn build(self)
        -> Result<WindowHandle, Error>
    {
        unsafe {
            // Maybe separate registration in build api? Probably only need to
            // register once even for multiple window creation.

            // TODO: probably want configurable class name.
            let class_name = "Xi Editor".to_wide();
            let icon = LoadIconW(0 as HINSTANCE, IDI_APPLICATION);
            let cursor = LoadCursorW(0 as HINSTANCE, IDC_IBEAM);
            let brush = CreateSolidBrush(0xffffff);
            let wnd = WNDCLASSW {
                style: 0,
                lpfnWndProc: Some(win_proc_dispatch),
                cbClsExtra: 0,
                cbWndExtra: 0,
                hInstance: 0 as HINSTANCE,
                hIcon: icon,
                hCursor: cursor,
                hbrBackground: brush,
                lpszMenuName: 0 as LPCWSTR,
                lpszClassName: class_name.as_ptr(),
            };
            let class_atom = RegisterClassW(&wnd);
            if class_atom == 0 {
                return Err(Error::Null);
            }
            let wndproc = MyWndProc {
                handler: self.handler.unwrap(),
                handle: Default::default(),
                runloop: self.runloop,
                d2d_factory: direct2d::Factory::new().unwrap(),
                swap_chain: Cell::new(null_mut()),
                render_target: RefCell::new(None),
                present_strategy: self.present_strategy,
            };

            let window = WindowState {
                hwnd: Cell::new(0 as HWND),
                wndproc: Box::new(wndproc),
            };
            let win = Rc::new(window);
            let handle = WindowHandle(Rc::downgrade(&win));

            // Simple scaling based on System Dpi (96 is equivalent to 100%)
            let dpi = if let Some(func) = OPTIONAL_FUNCTIONS.GetDpiForSystem {
                // Only supported on windows 10
                func() as f32
            } else {
                // TODO GetDpiForMonitor is supported on windows 8.1, try falling back to that here
                96.0
            };
            let width = (500.0 * (dpi/96.0)) as i32;
            let height = (400.0 * (dpi/96.0)) as i32;

            let hmenu = match self.menu {
                Some(menu) => menu.into_hmenu(),
                None => 0 as HMENU,
            };
            let mut dwExStyle = WS_EX_OVERLAPPEDWINDOW;
            if self.present_strategy == PresentStrategy::Flip {
                dwExStyle |= WS_EX_NOREDIRECTIONBITMAP;
            }
            let hwnd = create_window(dwExStyle, class_name.as_ptr(),
                self.title.to_wide().as_ptr(), self.dwStyle,
                CW_USEDEFAULT, CW_USEDEFAULT, width, height, 0 as HWND, hmenu, 0 as HINSTANCE,
                win.clone());
            if hwnd.is_null() {
                return Err(Error::Null);
            }

            let mut factory: *mut IDXGIFactory2 = null_mut();
            let hres = CreateDXGIFactory2(0, &IID_IDXGIFactory2,
                &mut factory as *mut *mut IDXGIFactory2 as *mut *mut c_void);
            println!("dxgi factory pointer = {:?}", factory);
            let adapter = choose_adapter(factory);
            println!("adapter = {:?}", adapter);
            let mut d3d11_device: *mut ID3D11Device = null_mut();
            let flags = D3D11_CREATE_DEVICE_BGRA_SUPPORT;  // could probably set single threaded
            D3D11CreateDevice(adapter, D3D_DRIVER_TYPE_UNKNOWN, null_mut(), flags,
                null(), 0, D3D11_SDK_VERSION, &mut d3d11_device, null_mut(), null_mut());
            println!("d3d11 device pointer = {:?}", d3d11_device);
            let mut swap_chain: *mut IDXGISwapChain1 = null_mut();
            let (swap_effect, scaling, bufs) = match self.present_strategy {
                PresentStrategy::Sequential =>
                    (DXGI_SWAP_EFFECT_SEQUENTIAL, DXGI_SCALING_STRETCH, 1),
                PresentStrategy::Flip | PresentStrategy::FlipRedirect =>
                    (DXGI_SWAP_EFFECT_FLIP_SEQUENTIAL, DXGI_SCALING_NONE, 2),
            };
            let desc = DXGI_SWAP_CHAIN_DESC1 {
                Width: 0,
                Height: 0,
                Format: DXGI_FORMAT_B8G8R8A8_UNORM,
                Stereo: FALSE,
                SampleDesc: DXGI_SAMPLE_DESC { Count: 1, Quality: 0},
                BufferUsage: DXGI_USAGE_RENDER_TARGET_OUTPUT,
                BufferCount: bufs,
                Scaling: scaling,
                SwapEffect: swap_effect,
                AlphaMode: DXGI_ALPHA_MODE_UNSPECIFIED,
                Flags: 0,
            };
            let res = (*factory).CreateSwapChainForHwnd(d3d11_device as *mut IUnknown, hwnd, &desc,
                null(), null_mut(), &mut swap_chain);
            println!("swap chain res = 0x{:x}, pointer = {:?}", res, swap_chain);

            win.hwnd.set(hwnd);
            win.wndproc.connect(&handle, swap_chain);
            mem::drop(win);
            Ok(handle)
        }
    }
}

/// Choose an adapter. Here the heuristic is to choose the adapter with the
/// largest video memory, which will generally be the discrete adapter. It's
/// possible that on some systems the integrated adapter might be a better
/// choice, but that probably depends on usage.
unsafe fn choose_adapter(factory: *mut IDXGIFactory2) -> *mut IDXGIAdapter {
    let mut i = 0;
    let mut best_adapter = null_mut();
    let mut best_vram = 0;
    loop {
        let mut adapter: *mut IDXGIAdapter = null_mut();
        if !SUCCEEDED((*factory).EnumAdapters(i, &mut adapter)) {
            break;
        }
        let mut desc: DXGI_ADAPTER_DESC = mem::uninitialized();
        (*adapter).GetDesc(&mut desc);
        let vram = desc.DedicatedVideoMemory;
        if i == 0 || vram > best_vram {
            best_vram = vram;
            best_adapter = adapter;
        }
        println!("{:?}: desc = {:?}, vram = {}",
            adapter,
            (&mut desc.Description[0] as LPWSTR).from_wide(),
            desc.DedicatedVideoMemory);
        i += 1;
    }
    best_adapter
}


unsafe extern "system" fn win_proc_dispatch(hwnd: HWND, msg: UINT, wparam: WPARAM, lparam: LPARAM)
    -> LRESULT
{
    if msg == WM_CREATE {
        let create_struct = &*(lparam as *const CREATESTRUCTW);
        let wndproc_ptr = create_struct.lpCreateParams;
        SetWindowLongPtrW(hwnd, GWLP_USERDATA, wndproc_ptr as LONG_PTR);
    }
    let window_ptr = GetWindowLongPtrW(hwnd, GWLP_USERDATA) as *const WindowState;
    let result = {
        if window_ptr.is_null() {
            None
        } else {
            (*window_ptr).wndproc.window_proc(hwnd, msg, wparam, lparam)
        }
    };
    if msg == WM_NCDESTROY {
        if !window_ptr.is_null() {
            SetWindowLongPtrW(hwnd, GWLP_USERDATA, 0);
            mem::drop(Rc::from_raw(window_ptr));
        }
    }
    match result {
        Some(lresult) => lresult,
        None => DefWindowProcW(hwnd, msg, wparam, lparam),
    }
}

/// Create a window (same parameters as CreateWindowExW) with associated WndProc.
unsafe fn create_window(
        dwExStyle: DWORD, lpClassName: LPCWSTR, lpWindowName: LPCWSTR, dwStyle: DWORD, x: c_int,
        y: c_int, nWidth: c_int, nHeight: c_int, hWndParent: HWND, hMenu: HMENU,
        hInstance: HINSTANCE, wndproc: Rc<WindowState>) -> HWND
{
    CreateWindowExW(dwExStyle, lpClassName, lpWindowName, dwStyle, x, y,
        nWidth, nHeight, hWndParent, hMenu, hInstance, Rc::into_raw(wndproc) as LPVOID)
}

impl WindowHandle {
    pub fn show(&self) {
        if let Some(w) = self.0.upgrade() {
            let hwnd = w.hwnd.get();
            unsafe {
                ShowWindow(hwnd, SW_SHOWNORMAL);
                UpdateWindow(hwnd);
            }
        }
    }

    pub fn close(&self) {
        if let Some(w) = self.0.upgrade() {
            let hwnd = w.hwnd.get();
            unsafe {
                DestroyWindow(hwnd);
            }
        }
    }

    pub fn invalidate(&self) {
        if let Some(w) = self.0.upgrade() {
            let hwnd = w.hwnd.get();
            unsafe {
                InvalidateRect(hwnd, null(), FALSE);
            }
        }
    }

    /// Get the raw HWND handle, for uses that are not wrapped in
    /// xi_win_shell.
    pub fn get_hwnd(&self) -> Option<HWND> {
        self.0.upgrade().map(|w| w.hwnd.get())
    }
}
