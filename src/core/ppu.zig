const std = @import("std");

// DMG ONLY -- Basically everything, some other more specific stuff

const MemoryFlag = @import("utils/MemoryFlag.zig");
const TrackedValue = @import("utils/tracked_value.zig").TrackedValue;
const InterruptKind = @import("interrupt.zig").InterruptKind;

const mmio_register = @import("utils/mmio_register.zig");
const LCDC = mmio_register.LCDC;
const STAT = mmio_register.STAT;
const BGP = mmio_register.BGP;
const OBP0 = mmio_register.OBP0;
const OBP1 = mmio_register.OBP1;

const logger = std.log.scoped(.ppu);

// TODO: this may need to be non global
var static_vram: [0x2000]u8 = undefined;
var static_init_vram: [0x2000]bool = @splat(false);

var static_oam: [0xA0]u8 = undefined;
var static_init_oam: [0xA0]bool = @splat(false);

pub fn Ppu(VideoBackend: type, Interrupt: type, warn: bool) type {
    return struct {
        const This = @This();

        pub const Vram = struct {
            pub fn read(_: *This, addr: u16) struct { MemoryFlag, u8 } {
                const val = static_vram[addr];
                const flags = MemoryFlag{ .uninitialized = !static_init_vram[addr] };
                return .{ flags, val };
            }

            pub fn write(_: *This, addr: u16, val: u8) MemoryFlag {
                static_vram[addr] = val;
                static_init_vram[addr] = true;
                return .{};
            }

            pub fn peek(_: *This, addr: u16) u8 {
                return static_vram[addr];
            }

            pub fn poke(_: *This, addr: u16, val: u8) void {
                static_vram[addr] = val;
            }
        };

        pub const Oam = struct {
            const ObjectAttrs = packed struct(u8) {
                const Palette = enum(u1) {
                    obp0 = 0,
                    obp1 = 1,
                };

                const Flipped = enum(u1) {
                    normal = 0,
                    flipped = 1,
                };

                const Priority = enum(u1) {
                    none = 0,
                    bg_window_drawn_over = 1,
                };

                _: u3 = 0b111, // DMG only -- palette for CGB
                __: u1 = 1, // DMG only -- VRAM bank for CGB
                palette: Palette, // DMG only -- Unused on CGB
                xflip: Flipped,
                yflip: Flipped,
                priority: Priority,
            };

            pub fn read(_: *This, addr: u16) struct { MemoryFlag, u8 } {
                const val = static_oam[addr];
                const flags = MemoryFlag{ .uninitialized = !static_init_oam[addr] };
                return .{ flags, val };
            }

            pub fn write(_: *This, addr: u16, val: u8) MemoryFlag {
                static_oam[addr] = val;
                static_init_oam[addr] = true;
                return .{};
            }

            pub fn peek(_: *This, addr: u16) u8 {
                return static_oam[addr];
            }

            pub fn poke(_: *This, addr: u16, val: u8) void {
                static_oam[addr] = val;
            }
        };

        pub const Forbidden = struct {
            pub fn read(_: *This, _: u16) struct { MemoryFlag, u8 } {
                return .{ MemoryFlag{ .illegal = true }, 0xFF };
            }

            pub fn write(_: *This, _: u16, _: u8) MemoryFlag {
                return .{ .illegal = true };
            }

            pub fn peek(_: *This, _: u16) u8 {
                return 0xFF;
            }

            pub fn poke(_: *This, _: u16, _: u8) void {}
        };

        const Mode = union(STAT.PPUMode) {
            Mode0: Mode0,
            Mode1: Mode1,
            Mode2: Mode2,
            Mode3: Mode3,

            fn tick(self: *@This(), ppu: *This) void {
                if (ppu.ly == ppu.win_viewport.y.resolve(null)) {
                    ppu.wy_eq_ly = true;
                }

                const next = switch (self.*) {
                    inline else => |*mode| mode.tick(ppu),
                };

                if (next) |new_mode| {
                    self.* = new_mode;
                }
            }
        };

        const Mode0 = struct {
            fn init() Mode0 {
                return .{};
            }

            fn tick(_: *Mode0, ppu: *This) ?Mode {
                if (ppu.scanline_duration == 455) {
                    ppu.scanline_duration = 0;
                    if (ppu.ly == 143) {
                        ppu.ly += 1; // we do keep increasing ly in vblank
                        ppu.window_line_counter = 0; // reset this mode3 register when entering vblank
                        return .{
                            .Mode1 = .init(),
                        };
                    } else {
                        ppu.ly += 1;
                        return .{
                            .Mode2 = .init(),
                        };
                    }
                } else {
                    ppu.scanline_duration += 1;
                    return null;
                }
            }
        };

        const Mode1 = struct {
            fn init() Mode1 {
                return .{};
            }

            fn tick(_: *Mode1, ppu: *This) ?Mode {
                if (ppu.scanline_duration == 455) {
                    ppu.scanline_duration = 0;
                    if (ppu.ly == 153) {
                        ppu.ly = 0;
                        ppu.wy_eq_ly = false;
                        return .{
                            .Mode2 = .init(),
                        };
                    } else {
                        ppu.ly += 1;
                        return null;
                    }
                } else {
                    ppu.scanline_duration += 1;
                    return null;
                }
            }
        };

        const Mode2 = struct {
            objs_idx: u8,
            oam_addr: u8,
            current: struct { x: u8, y: u8 },
            state: enum { read_next, write_next },

            fn init() Mode2 {
                return .{
                    .objs_idx = 0,
                    .oam_addr = 0,
                    .current = .{
                        .x = 0,
                        .y = 0,
                    },
                    .state = .read_next,
                };
            }

            fn tick(self: *Mode2, ppu: *This) ?Mode {
                ppu.scanline_duration += 1;

                switch (self.state) {
                    .read_next => {
                        _, self.current.y = Oam.read(ppu, self.oam_addr);
                        _, self.current.x = Oam.read(ppu, self.oam_addr + 1);
                        self.state = .write_next;
                    },
                    .write_next => {
                        // object's y coordinate is offset by 16 from the actual screen coordinate
                        const line = ppu.ly + 16;
                        const height = ppu.obj_size.resolve(null).height();
                        if (self.objs_idx < 10 and line >= self.current.y and line < @as(u16, self.current.y) + height) {
                            const entry: This.ScannedOamEntry = .{
                                .x = self.current.x,
                                .y = self.current.y,
                                .addr = self.oam_addr,
                            };

                            // stable insertion sort by x position
                            var pos = self.objs_idx;
                            while (pos > 0 and ppu.objects_for_this_scanline[pos - 1].x > entry.x) {
                                ppu.objects_for_this_scanline[pos] = ppu.objects_for_this_scanline[pos - 1];
                                pos -= 1;
                            }

                            ppu.objects_for_this_scanline[pos] = entry;
                            ppu.objects_fetched_this_scanline[self.objs_idx] = false;
                            self.objs_idx += 1;
                            ppu.total_objects_for_this_scanline = self.objs_idx;
                        }

                        self.oam_addr += 4;
                        self.state = .read_next;
                    },
                }

                if (self.oam_addr >= 4 * 40) {
                    return .{
                        .Mode3 = .init(),
                    };
                }
                return null;
            }
        };

        const Mode3 = struct {
            const Pixel = packed struct(u4) {
                color_idx: u2,
                palette: Oam.ObjectAttrs.Palette,
                priority: Oam.ObjectAttrs.Priority,
            };

            const FIFO = struct {
                const SIZE: u8 = 8;
                const MASK: u8 = SIZE - 1;

                elements: [SIZE]Pixel,
                head: u8,
                tail: u8,

                inline fn init() FIFO {
                    var self: FIFO = undefined;
                    self.reset();
                    return self;
                }

                inline fn reset(self: *FIFO) void {
                    self.head = 0;
                    self.tail = 0;
                }

                inline fn size(self: FIFO) u8 {
                    return self.tail -% self.head;
                }

                inline fn empty(self: FIFO) bool {
                    return self.tail == self.head;
                }

                inline fn full(self: FIFO) bool {
                    return self.size() == SIZE;
                }

                inline fn push(self: *FIFO, val: Pixel) void {
                    if (self.full()) {
                        @panic("attempt to push to full fifo");
                    }

                    self.elements[self.tail & MASK] = val;
                    self.tail +%= 1;
                }

                inline fn at(self: *FIFO, idx: u8) *Pixel {
                    if (idx >= self.size()) {
                        @panic("attempt to index fifo with invalidm size");
                    }

                    return &self.elements[(self.head +% idx) & MASK];
                }

                inline fn pop(self: *FIFO) Pixel {
                    if (self.empty()) {
                        @panic("attempt to pop from empty fifo");
                    }

                    const val = self.elements[self.head & MASK];
                    self.head +%= 1;
                    return val;
                }
            };

            fn Fetcher(which: enum { win_bg, obj }) type {
                return struct {
                    const ThisFetcher = @This();

                    const State = enum {
                        fetch_tile_1,
                        fetch_tile_2,
                        fetch_data_low_1,
                        fetch_data_low_2,
                        fetch_data_high_1,
                        fetch_data_high_2,
                        push,
                    };

                    const FetchMode = enum {
                        background,
                        window,
                    };

                    fifo: *FIFO,

                    state: State,
                    mode: if (which == .win_bg) FetchMode else void,

                    xpos: u5,
                    addr: u16,
                    tilenum: u8,
                    data_high: u8,
                    data_low: u8,

                    pixels: if (which == .win_bg) [8]Pixel else void,
                    attrs: if (which == .obj) Oam.ObjectAttrs else void,
                    virgin: if (which == .win_bg) bool else void,
                    obj_idx: if (which == .obj) u8 else void,

                    fn init() ThisFetcher {
                        return .{
                            .fifo = undefined,

                            .state = .fetch_tile_1,
                            .mode = if (which == .win_bg) .background else {},

                            .xpos = 0,
                            .addr = 0,
                            .tilenum = 0,
                            .data_high = 0,
                            .data_low = 0,

                            .pixels = if (which == .win_bg) @splat(@bitCast(@as(u4, 0))) else {},
                            .attrs = if (which == .obj) @bitCast(@as(u8, 0)) else {},
                            .virgin = if (which == .win_bg) true else {},
                            .obj_idx = if (which == .obj) 0 else {},
                        };
                    }

                    fn switchToWinMode(self: *ThisFetcher, fifo: *FIFO) void {
                        if (self.mode == .window) {
                            return;
                        }

                        self.state = .fetch_tile_1;
                        self.mode = .window;
                        self.xpos = 0;
                        fifo.reset();
                    }

                    fn tick(self: *ThisFetcher, ppu: *This, fifo: *FIFO) void {
                        self.fifo = fifo;

                        inline for (@typeInfo(State).@"enum".field_names) |state| {
                            if (self.state == @field(State, state)) {
                                if (which == .obj) {
                                    const name = "tick_objects_" ++ state;
                                    if (@hasDecl(ThisFetcher, name)) {
                                        self.state = @field(ThisFetcher, name)(self, ppu);
                                        return;
                                    }
                                } else {
                                    inline for (@typeInfo(FetchMode).@"enum".field_names) |mode| {
                                        if (self.mode == @field(FetchMode, mode)) {
                                            const name = "tick_" ++ mode ++ "_" ++ state;
                                            if (@hasDecl(ThisFetcher, name)) {
                                                self.state = @field(ThisFetcher, name)(self, ppu);
                                                return;
                                            }
                                        }
                                    }
                                }

                                const name = "tick_" ++ state;
                                if (@hasDecl(ThisFetcher, name)) {
                                    self.state = @field(ThisFetcher, name)(self, ppu);
                                    return;
                                } else {
                                    @panic("missing comptime definitions");
                                }
                            }
                        }
                    }

                    fn tick_background_fetch_tile_1(self: *ThisFetcher, ppu: *const This) State {
                        const base_addr: u16 = ppu.bg_tile_map_area.resolve(null).baseAddr();
                        const x_offset = self.xpos +% @as(u5, @intCast(ppu.bg_viewport.x.resolve(null) / 8));
                        const y_offset = 32 * (ppu.ly +% @as(u5, @intCast(ppu.bg_viewport.y.resolve(null) / 8)));
                        self.addr = base_addr + x_offset + y_offset;
                        return .fetch_tile_2;
                    }

                    fn tick_window_fetch_tile_1(self: *ThisFetcher, ppu: *const This) State {
                        const base_addr: u16 = ppu.window_tile_map_area.resolve(null).baseAddr();
                        const x_offset = self.xpos;
                        const y_offset = 32 * ((ppu.window_line_counter / 8) & 0x1F);
                        self.addr = base_addr + x_offset + y_offset;
                        return .fetch_tile_2;
                    }

                    fn tick_objects_fetch_tile_1(self: *ThisFetcher, ppu: *const This) State {
                        self.addr = ppu.objects_for_this_scanline[self.obj_idx].addr + 2;
                        return .fetch_tile_2;
                    }

                    fn tick_objects_fetch_tile_2(self: *ThisFetcher, ppu: *const This) State {
                        _, self.tilenum = Oam.read(ppu, self.addr);
                        _, const attrs = Oam.read(ppu, self.addr + 1);
                        self.attrs = @bitCast(attrs);
                        return .fetch_data_low_1;
                    }

                    fn tick_fetch_tile_2(self: *ThisFetcher, ppu: *const This) State {
                        _, self.tilenum = Vram.read(ppu, self.addr);
                        return .fetch_data_low_1;
                    }

                    fn tick_objects_fetch_data_low_1(self: *ThisFetcher, ppu: *const This) State {
                        const height_enum = ppu.obj_size.resolve(null);
                        const height = height_enum.height();

                        const tilenum = if (height_enum == .size_8x16)
                            self.tilenum & 0xFE
                        else
                            self.tilenum;
                        const tile_offset = @as(u16, tilenum) * 16;

                        var row = ppu.ly + 16 - ppu.objects_for_this_scanline[self.obj_idx].y;
                        if (self.attrs.yflip == .flipped) {
                            row = height - 1 - row;
                        }

                        self.addr = tile_offset + row * 2;

                        return .fetch_data_low_2;
                    }

                    fn tick_fetch_data_low_1(self: *ThisFetcher, ppu: *const This) State {
                        const tile_offset_method: LCDC.TileDataMethod = ppu.bg_window_tile_data_area.resolve(null);

                        const tile_offset: u16 = switch (tile_offset_method) {
                            .method_8000 => @as(u16, self.tilenum) * 16,
                            .method_8800 => blk: {
                                const signed_tile: i8 = @bitCast(self.tilenum);
                                const offset: i16 = 0x1000 + @as(i16, signed_tile) * 16;
                                break :blk @intCast(offset);
                            },
                        };

                        const row: u16 = switch (self.mode) {
                            .background => (@as(u16, ppu.ly) + ppu.bg_viewport.y.resolve(null)) & 7,
                            .window => ppu.window_line_counter & 7,
                        };

                        self.addr = tile_offset + row * 2;

                        return .fetch_data_low_2;
                    }

                    fn tick_fetch_data_low_2(self: *ThisFetcher, ppu: *const This) State {
                        _, self.data_low = Vram.read(ppu, self.addr);
                        return .fetch_data_high_1;
                    }

                    fn tick_fetch_data_high_1(self: *ThisFetcher, _: *const This) State {
                        self.addr += 1;
                        return .fetch_data_high_2;
                    }

                    fn tick_fetch_data_high_2(self: *ThisFetcher, ppu: *const This) State {
                        _, self.data_high = Vram.read(ppu, self.addr);

                        if (which == .win_bg and self.virgin) {
                            self.virgin = false;
                            return .fetch_tile_1;
                        }

                        return self.attemptImmediatePush();
                    }

                    fn attemptImmediatePush(self: *ThisFetcher, ppu: *This) State {
                        if (which == .obj) {
                            self.pushObjects(ppu);
                            return .fetch_tile_1;
                        } else {
                            self.pixels = self.calculatePixels();
                            return self.tick_push(ppu);
                        }
                    }

                    fn pushObjects(self: *ThisFetcher, ppu: *This) void {
                        const pixels = self.calculatePixels();
                        const objx = ppu.objects_for_this_scanline[self.obj_idx].x;

                        const shifted = if (objx < 8)
                            pixels[8 - objx ..]
                        else
                            &pixels;

                        var counter: u8 = 0;
                        while (counter < self.fifo.size() and counter < shifted.len) {
                            const pix = self.fifo.at(counter);
                            pix.* = mergePixels(pix.*, shifted[counter]);
                            counter += 1;
                        }

                        for (shifted[counter..]) |pixel| {
                            self.fifo.push(pixel);
                        }

                        ppu.objects_fetched_this_scanline[self.obj_idx] = true;
                    }

                    fn mergePixels(curr: Pixel, next: Pixel) Pixel {
                        if (curr.color_idx == 0) {
                            return next;
                        }
                        return curr;
                    }

                    fn tick_push(self: *ThisFetcher, _: *const This) State {
                        if (!self.fifo.empty()) {
                            return .push;
                        }

                        for (self.pixels) |pixel| {
                            self.fifo.push(pixel);
                        }
                        self.xpos +%= 1;

                        return .fetch_tile_1;
                    }

                    fn calculatePixels(self: *ThisFetcher) [8]Pixel {
                        var pixels: [8]Pixel = undefined;
                        for (0..8) |i| {
                            const bit = if (which == .obj and self.attrs.xflip == .flipped) i else (7 - i);

                            const color_idx_low: u1 = (self.data_low & 1 << bit) >> bit;
                            const color_idx_high: u1 = (self.data_high & 1 << bit) >> bit;
                            const color_idx: u2 = (@as(u2, color_idx_high) << 1) | color_idx_low;

                            pixels[i] = .{
                                .color_idx = color_idx,
                                .palette = if (which == .obj) self.attrs.palette else .obp0,
                                .priority = if (which == .obj) self.attrs.priority else .none,
                            };
                        }
                        return pixels;
                    }
                };
            }

            bg_win_fetcher: Fetcher(.win_bg),
            obj_fetcher: Fetcher(.obj),
            active_fetcher: enum { win_bg, obj },

            bg_win_fifo: FIFO,
            obj_fifo: FIFO,

            x: u8,
            had_window: bool,
            pixels_to_discard: ?u8,
            discarded_pixels: u8,

            fn init() Mode3 {
                return .{
                    .bg_win_fetcher = .init(),
                    .obj_fetcher = .init(),
                    .active_fetcher = .win_bg,

                    .bg_win_fifo = .init(),
                    .obj_fifo = .init(),

                    .x = 0,
                    .had_window = false,
                    .pixels_to_discard = null,
                    .discarded_pixels = 0,
                };
            }

            fn tick(self: *Mode3, ppu: *This) ?Mode {
                ppu.scanline_duration += 1;

                if (self.pixels_to_discard == null) {
                    self.pixels_to_discard = ppu.bg_viewport.x.resolve(null) % 8;
                }

                self.setActiveFetcher(ppu);
                switch (self.active_fetcher) {
                    .win_bg => self.bg_win_fetcher.tick(ppu, &self.bg_win_fifo),
                    .obj => self.obj_fetcher.tick(ppu, &self.obj_fifo),
                }
                self.setActiveFetcher(ppu);

                if (self.active_fetcher != .obj) {
                    const pixel = self.renderPixel(ppu);
                    if (pixel) |p| {
                        if (self.discarded_pixels < self.pixels_to_discard.?) {
                            self.discarded_pixels += 1;
                        } else {
                            ppu.video_backend.submit(p);
                            self.x += 1;
                        }
                    }
                }

                if (self.x >= 160) {
                    if (self.had_window) {
                        ppu.window_line_counter += 1;
                    }
                    return .{
                        .Mode0 = .init(),
                    };
                }

                if (self.shouldBeInWinMode(ppu)) {
                    self.bg_win_fetcher.switchToWinMode(&self.bg_win_fifo);
                    self.had_window = true;
                }

                return null;
            }

            fn shouldBeInWinMode(self: Mode3, ppu: *const This) bool {
                if (!ppu.bg_window_enable.resolve(null)) {
                    return false;
                }
                if (!ppu.window_enable.resolve(null)) {
                    return false;
                }
                if (!ppu.wy_eq_ly) {
                    return false;
                }
                if (self.x + 7 < ppu.win_viewport.x.resolve(null)) {
                    return false;
                }
                return true;
            }

            fn setActiveFetcher(self: *Mode3, ppu: *const This) void {
                if (self.shouldFetchObject(ppu)) |idx| {
                    self.active_fetcher = .obj;
                    self.obj_fetcher.obj_idx = idx;
                } else {
                    self.active_fetcher = .win_bg;
                }
            }

            fn shouldFetchObject(self: *Mode3, ppu: *const This) ?u8 {
                for (0..ppu.total_objects_for_this_scanline) |i| {
                    if (ppu.objects_for_this_scanline[i].x <= self.x + 8 and
                        !ppu.objects_fetched_this_scanline[i])
                    {
                        return @intCast(i);
                    }
                }
                return null;
            }

            fn renderPixel(self: *Mode3, ppu: *This) ?u2 {
                const pixel, const which = self.resolvePixel(ppu) orelse return null;

                const palette = switch (which) {
                    .bg_win => ppu.bg_win_palette,
                    .obj => switch (pixel.palette) {
                        .obp0 => ppu.obj_palette_1,
                        .obp1 => ppu.obj_palette_2,
                    },
                };

                return @backingInt(palette[pixel.color_idx].resolve(null));
            }

            fn resolvePixel(self: *Mode3, ppu: *This) ?struct { Pixel, enum { bg_win, obj } } {
                if (self.bg_win_fifo.empty()) {
                    return null;
                }

                var bg_win_pixel = self.bg_win_fifo.pop();

                if (!ppu.bg_window_enable.resolve(null)) {
                    bg_win_pixel.color_idx = 0;
                }

                if (self.obj_fifo.empty()) {
                    return .{ bg_win_pixel, .bg_win };
                }

                const obj_pixel = self.obj_fifo.pop();

                if (obj_pixel.color_idx == 0) {
                    return .{ bg_win_pixel, .bg_win };
                }

                if (obj_pixel.priority == .bg_window_drawn_over and bg_win_pixel.color_idx != 0) {
                    return .{ bg_win_pixel, .bg_win };
                }

                return .{ obj_pixel, .obj };
            }
        };

        const IntSelect = struct {
            mode0: bool,
            mode1: bool,
            mode2: bool,
            lyc: bool,
        };

        const ScannedOamEntry = struct {
            x: u8,
            y: u8,
            addr: u8,
        };

        enabled: TrackedValue(bool),

        bg_tile_map_area: TrackedValue(LCDC.TileMapArea),
        window_tile_map_area: TrackedValue(LCDC.TileMapArea),
        bg_window_tile_data_area: TrackedValue(LCDC.TileDataMethod),
        obj_size: TrackedValue(LCDC.ObjSize),

        window_enable: TrackedValue(bool),
        obj_enable: TrackedValue(bool),
        bg_window_enable: TrackedValue(bool), // DMG ONLY -- CGB instead does something with priority

        int_select: struct {
            mode0: TrackedValue(bool),
            mode1: TrackedValue(bool),
            mode2: TrackedValue(bool),
            lyc: TrackedValue(bool),

            fn as_values(self: @This(), flag: ?*MemoryFlag) IntSelect {
                return .{
                    .mode0 = self.mode0.resolve(flag),
                    .mode1 = self.mode1.resolve(flag),
                    .mode2 = self.mode2.resolve(flag),
                    .lyc = self.lyc.resolve(flag),
                };
            }
        },

        bg_viewport: struct {
            x: TrackedValue(u8),
            y: TrackedValue(u8),
        },
        win_viewport: struct {
            x: TrackedValue(u8),
            y: TrackedValue(u8),
        },

        lyc: TrackedValue(u8),

        bg_win_palette: [4]TrackedValue(BGP.Color),
        obj_palette_1: [4]TrackedValue(BGP.Color),
        obj_palette_2: [4]TrackedValue(BGP.Color),

        current_oam_dma_addr: TrackedValue(u8),

        mode: Mode,
        ly: u8,
        objects_for_this_scanline: [10]ScannedOamEntry,
        objects_fetched_this_scanline: [10]bool,
        total_objects_for_this_scanline: u8,
        scanline_duration: u16,
        window_line_counter: u8,
        wy_eq_ly: bool,

        oam_dma_progress: u8,
        must_write_to_stat_next_mcycle: ?u8,

        video_backend: *VideoBackend,
        interrupt: *Interrupt,

        pub fn init(video_backend: *VideoBackend, interrupt: *Interrupt) This {
            return This{
                .enabled = .init(false),

                .bg_tile_map_area = .init(.addr_9C00_to_9FFF),
                .window_tile_map_area = .init(.addr_9C00_to_9FFF),
                .bg_window_tile_data_area = .init(.method_8800),
                .obj_size = .init(.size_8x16),

                .window_enable = .init(false),
                .obj_enable = .init(false),
                .bg_window_enable = .init(false),

                .int_select = .{
                    .mode0 = .init(true),
                    .mode1 = .init(false),
                    .mode2 = .init(true),
                    .lyc = .init(false),
                },

                .bg_viewport = .{
                    .x = .init(0xAA),
                    .y = .init(0xAA),
                },
                .win_viewport = .{
                    .x = .init(0xAA),
                    .y = .init(0xAA),
                },

                .lyc = .init(0xAA),

                .bg_win_palette = .{
                    .init(.white),
                    .init(.black),
                    .init(.white),
                    .init(.black),
                },
                .obj_palette_1 = .{
                    .init(.white),
                    .init(.black),
                    .init(.white),
                    .init(.black),
                },
                .obj_palette_2 = .{
                    .init(.white),
                    .init(.black),
                    .init(.white),
                    .init(.black),
                },

                .current_oam_dma_addr = .init(0xAA),

                .mode = .{
                    .Mode0 = .init(),
                },
                .ly = 0,

                .oam_dma_progress = 0xFF,
                .must_write_to_stat_next_mcycle = null,
                .objects_for_this_scanline = @splat(.{ .x = 0, .y = 0, .addr = 0 }),
                .objects_fetched_this_scanline = @splat(false),
                .total_objects_for_this_scanline = 0,
                .scanline_duration = 0,
                .window_line_counter = 0,
                .wy_eq_ly = false,

                .video_backend = video_backend,
                .interrupt = interrupt,
            };
        }

        pub fn tick(self: *This, m_cycle_tick: u2) void {
            if (m_cycle_tick == 0) {
                if (self.must_write_to_stat_next_mcycle) |val| {
                    _ = self.write(0x1, val);
                    self.must_write_to_stat_next_mcycle = null;
                }
                self.tick_oam_dma();
            }

            if (!self.enabled.resolve(null)) {
                return;
            }

            const prev_ly = self.ly;
            const prev_mode = std.meta.activeTag(self.mode);

            // TODO: restrict memory access during ppu execution (related to OAM DMA, need changes in MMU)
            self.mode.tick(self);

            if (prev_ly != self.ly or prev_mode != self.mode) {
                self.check_trigger_interrupt(self.int_select.as_values(null), prev_mode, prev_ly, self.lyc.resolve(null), null);
            }
        }

        fn tick_oam_dma(self: *This) void {
            if (self.oam_dma_progress >= 0xA0) {
                return;
            }

            // TODO: implement transfer itself

            self.oam_dma_progress += 1;
        }

        fn inner_read(self: *This, addr: u16) struct { MemoryFlag, u8 } {
            var flag: MemoryFlag = .{};
            const val: u8 = switch (addr) {
                0x00 => @bitCast(LCDC{
                    .bg_tile_map_area = self.bg_tile_map_area.resolve(&flag),
                    .bg_window_enable = self.bg_window_enable.resolve(&flag),
                    .bg_window_tile_data_area = self.bg_window_tile_data_area.resolve(&flag),
                    .obj_enable = self.obj_enable.resolve(&flag),
                    .obj_size = self.obj_size.resolve(&flag),
                    .ppu_enable = self.enabled.resolve(&flag),
                    .window_enable = self.window_enable.resolve(&flag),
                    .window_tile_map_area = self.window_tile_map_area.resolve(&flag),
                }),
                0x01 => @bitCast(STAT{
                    .ppu_mode = std.meta.activeTag(self.mode),
                    .lyc_eq_ly = self.lyc.resolve(&flag) == self.ly,
                    .mode_0_int_select = self.int_select.mode0.resolve(&flag),
                    .mode_1_int_select = self.int_select.mode1.resolve(&flag),
                    .mode_2_int_select = self.int_select.mode2.resolve(&flag),
                    .lyc_int_select = self.int_select.lyc.resolve(&flag),
                }),
                0x02 => self.bg_viewport.y.resolve(&flag),
                0x03 => self.bg_viewport.x.resolve(&flag),
                0x04 => self.ly,
                0x05 => self.lyc.resolve(&flag),
                0x06 => self.current_oam_dma_addr.resolve(&flag),
                0x07 => @bitCast(BGP{ // DMG ONLY -- CGB takes its palettes from elsewhere
                    .id0 = self.bg_win_palette[0].resolve(&flag),
                    .id1 = self.bg_win_palette[1].resolve(&flag),
                    .id2 = self.bg_win_palette[2].resolve(&flag),
                    .id3 = self.bg_win_palette[3].resolve(&flag),
                }),
                0x08 => @bitCast(OBP0{ // DMG ONLY -- CGB takes its palettes from elsewhere
                    .id0 = self.obj_palette_1[0].resolve(&flag),
                    .id1 = self.obj_palette_1[1].resolve(&flag),
                    .id2 = self.obj_palette_1[2].resolve(&flag),
                    .id3 = self.obj_palette_1[3].resolve(&flag),
                }),
                0x09 => @bitCast(OBP1{ // DMG ONLY -- CGB takes its palettes from elsewhere
                    .id0 = self.obj_palette_2[0].resolve(&flag),
                    .id1 = self.obj_palette_2[1].resolve(&flag),
                    .id2 = self.obj_palette_2[2].resolve(&flag),
                    .id3 = self.obj_palette_2[3].resolve(&flag),
                }),
                0x0A => self.win_viewport.y.resolve(&flag),
                0x0B => self.win_viewport.x.resolve(&flag),
                else => unreachable,
            };

            return .{ flag, val };
        }

        fn inner_write(self: *This, addr: u16, val: u8, real: bool) void {
            switch (addr) {
                0x00 => {
                    const reg: LCDC = @bitCast(val);
                    self.bg_tile_map_area.set(reg.bg_tile_map_area, real);
                    self.bg_window_enable.set(reg.bg_window_enable, real);
                    self.bg_window_tile_data_area.set(reg.bg_window_tile_data_area, real);
                    self.obj_enable.set(reg.obj_enable, real);
                    self.obj_size.set(reg.obj_size, real);
                    self.enabled.set(reg.ppu_enable, real);
                    self.window_enable.set(reg.window_enable, real);
                    self.window_tile_map_area.set(reg.window_tile_map_area, real);
                },
                0x01 => {
                    const reg: STAT = @bitCast(val);
                    self.int_select.mode0.set(reg.mode_0_int_select, real);
                    self.int_select.mode1.set(reg.mode_1_int_select, real);
                    self.int_select.mode2.set(reg.mode_2_int_select, real);
                    self.int_select.lyc.set(reg.lyc_int_select, real);
                },
                0x02 => self.bg_viewport.y.set(val, real),
                0x03 => self.bg_viewport.x.set(val, real),
                0x04 => self.ly = val, // allowed for debugging, write() should forbid it directly
                0x05 => self.lyc.set(val, real),
                0x06 => self.current_oam_dma_addr.set(val, real),
                0x07 => {
                    const reg: BGP = @bitCast(val);
                    self.bg_win_palette[0].set(reg.id0, real);
                    self.bg_win_palette[1].set(reg.id1, real);
                    self.bg_win_palette[2].set(reg.id2, real);
                    self.bg_win_palette[3].set(reg.id3, real);
                },
                0x08 => {
                    const reg: OBP0 = @bitCast(val);
                    self.obj_palette_1[0].set(reg.id0, real);
                    self.obj_palette_1[1].set(reg.id1, real);
                    self.obj_palette_1[2].set(reg.id2, real);
                    self.obj_palette_1[3].set(reg.id3, real);
                },
                0x09 => {
                    const reg: OBP1 = @bitCast(val);
                    self.obj_palette_2[0].set(reg.id0, real);
                    self.obj_palette_2[1].set(reg.id1, real);
                    self.obj_palette_2[2].set(reg.id2, real);
                    self.obj_palette_2[3].set(reg.id3, real);
                },
                0x0A => self.win_viewport.y.set(val, real),
                0x0B => self.win_viewport.x.set(val, real),
                else => unreachable,
            }
        }

        pub fn peek(self: *This, addr: u16) u8 {
            return self.inner_read(addr).@"1";
        }

        pub fn poke(self: *This, addr: u16, val: u8) void {
            return self.inner_write(addr, val, false);
        }

        pub fn read(self: *This, addr: u16) struct { MemoryFlag, u8 } {
            return self.inner_read(addr);
        }

        pub fn write(self: *This, addr: u16, in_val: u8) MemoryFlag {
            var flag: MemoryFlag = .{};
            var val = in_val;

            if (addr == 0x00 and val & 0x80 == 0) {
                if (self.enabled.resolve(&flag) and self.mode != .Mode1) {
                    if (warn) {
                        logger.warn("ppu turned off outside of mode 1 (vblank) will damage the hardware", .{});
                    }
                    flag.illegal = true;
                }
                self.poweroff();
            }

            if (addr == 0x01) {
                // DMG ONLY -- Hardware bug, from pandocs:
                // A hardware quirk in the monochrome Game Boy makes the LCD interrupt sometimes trigger when writing to
                // STAT (including writing $00) during OAM scan, HBlank, VBlank, or LY=LYC. It behaves as if $FF were
                // written for one M-cycle, and then the written value were written the next M-cycle.
                if (self.must_write_to_stat_next_mcycle == null and (self.mode != .Mode3 or self.ly == self.lyc.resolve(&flag))) {
                    self.must_write_to_stat_next_mcycle = val;
                    val = 0xFF;
                }
            }

            const prev_int_select = self.int_select.as_values(&flag);
            const prev_lyc = self.lyc.resolve(&flag);

            if (addr == 0x04) {
                flag.illegal = true;
            } else {
                self.inner_write(addr, val, true);
            }

            if (addr == 0x01 or addr == 0x05) {
                self.check_trigger_interrupt(prev_int_select, self.mode, self.ly, prev_lyc, &flag);
            }

            if (addr == 0x06) {
                if (val > 0xDF) {
                    if (warn) {
                        logger.warn("dma over invalid range will not behave as expected", .{});
                    }
                    flag.illegal = true;
                }
                self.oam_dma_progress = 0x00;
            }

            return flag;
        }

        fn check_trigger_interrupt(
            self: *This,
            prev_int_select: IntSelect,
            prev_mode: STAT.PPUMode,
            prev_ly: u8,
            prev_lyc: u8,
            flag: ?*MemoryFlag,
        ) void {
            if (!self.enabled.resolve(flag)) {
                return;
            }

            const before = This.get_interrupt_state(prev_int_select, prev_mode, prev_ly == prev_lyc);
            const after = This.get_interrupt_state(self.int_select.as_values(flag), self.mode, self.ly == self.lyc.resolve(flag));

            if (!before and after) {
                self.interrupt.request(InterruptKind.ppu);
            }
        }

        fn get_interrupt_state(int_select: IntSelect, mode: STAT.PPUMode, ly_eq_lyc: bool) bool {
            return (int_select.mode0 and mode == .Mode0) or
                (int_select.mode1 and mode == .Mode1) or
                (int_select.mode2 and mode == .Mode2) or
                (int_select.lyc and ly_eq_lyc);
        }

        fn poweroff(self: *This) void {
            self.video_backend.poweroff();
            self.enabled.set(false, true);
            self.mode = .{
                .Mode0 = .init(),
            };
            self.ly = 0;
        }
    };
}
