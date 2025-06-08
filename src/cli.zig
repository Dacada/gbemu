const std = @import("std");

const logger = std.log.scoped(.cliargs);

const ArgParserError = error{
    ArgumentWithoutValue,
    UnknownArgument,
    InvalidValue,
};

pub const ArgParserParamDefinition = struct {
    name: []const u8, // TODO: allow to use _ as separator in the name in the field and - in the name in the cli argument
    type: type,
    default: *const anyopaque,
    help: []const u8,

    pub fn init(comptime name: []const u8, comptime help: []const u8, comptime T: type, comptime default: T) ArgParserParamDefinition {
        return .{
            .name = name,
            .type = T,
            .default = &default,
            .help = help,
        };
    }
};

pub const ArgParserParams = struct {
    error_exit_code: u8,
    optional: []const ArgParserParamDefinition,
};

pub fn ArgParser(comptime params: ArgParserParams) type {
    var fields: [params.optional.len]std.builtin.Type.StructField = undefined;
    comptime for (params.optional, 0..) |opt, i| {
        fields[i] = .{
            .name = opt.name[0.. :0],
            .type = opt.type,
            .default_value_ptr = opt.default,
            .is_comptime = false,
            .alignment = @alignOf(opt.type),
        };
    };

    const Args = @Type(.{
        .@"struct" = .{
            .layout = .auto,
            .fields = &fields,
            .decls = &.{},
            .is_tuple = false,
        },
    });

    return struct {
        fn parse_value(comptime T: type, value: []const u8) ArgParserError!T {
            comptime var info = @typeInfo(T);
            comptime var TT = T;
            if (info == .optional) {
                TT = info.optional.child;
                info = @typeInfo(TT);
            }

            return switch (info) {
                .int => std.fmt.parseInt(TT, value, 0) catch |e| {
                    switch (e) {
                        std.fmt.ParseIntError.Overflow => {
                            logger.err("Overflow when parsing {s} for type {}", .{ value, TT });
                        },
                        std.fmt.ParseIntError.InvalidCharacter => {
                            logger.err("Invalid character when parsing {s} for type {}", .{ value, TT });
                        },
                    }
                    return ArgParserError.InvalidValue;
                },
                else => @panic("haven't fully implemented parser yet!"),
            };
        }

        fn parse_and_assign_value(value: []const u8, comptime name: []const u8, comptime T: type, args: *Args) !void {
            @field(args, name) = try parse_value(T, value);
        }

        fn parse_long_argument(arg: []const u8, iter: *std.process.ArgIterator, args: *Args) ArgParserError!void {
            inline for (params.optional) |opt| {
                if (std.mem.eql(u8, arg, opt.name)) {
                    const value = iter.next();
                    if (value == null) {
                        logger.err("Argument without value: {s}", .{arg});
                        return ArgParserError.ArgumentWithoutValue;
                    }
                    return parse_and_assign_value(value.?, opt.name, opt.type, args);
                }
            }

            logger.err("Unknown argument: {s}", .{arg});
            return ArgParserError.UnknownArgument;
        }

        fn parse_inner(iter: *std.process.ArgIterator) ArgParserError!Args {
            var args = Args{};
            while (iter.next()) |arg| {
                if (arg[0] == '-') {
                    if (arg[1] == '-') {
                        try parse_long_argument(arg[2..], iter, &args);
                    } else {
                        @panic("I haven't implemented short optional arguments yet!");
                    }
                } else {
                    @panic("I haven't implemented positional arguments yet!");
                }
            }
            return args;
        }

        fn print_usage_and_exit(program_name: []const u8, exit_code: u8) noreturn {
            const stderr = std.io.getStdErr();
            const writer = stderr.writer();
            const tty = std.io.tty.detectConfig(stderr);

            tty.setColor(writer, .blue) catch {};
            writer.writeAll("Usage:") catch {};
            tty.setColor(writer, .reset) catch {};
            std.fmt.format(writer, " {s}", .{program_name}) catch {};
            if (params.optional.len > 0) {
                writer.writeAll(" [options]") catch {};
            }
            writer.writeAll("\n\n") catch {};

            tty.setColor(writer, .blue) catch {};
            writer.writeAll("Options:") catch {};
            tty.setColor(writer, .reset) catch {};

            comptime var option_lines: [params.optional.len]struct {
                before: []const u8,
                option: []const u8,
                after: []const u8,
                description: []const u8,
                before_padding_len: usize,
                default: []const u8,
            } = undefined;
            comptime var maxlen = 0;
            inline for (params.optional, 0..) |option, i| {
                const before = "  --";
                const after = std.fmt.comptimePrint(" <{}>", .{option.type});
                const len = before.len + option.name.len + after.len;

                const casted_default_value: *const option.type = @ptrCast(option.default);
                const default = std.fmt.comptimePrint(" (default: {any})", .{casted_default_value.*});

                option_lines[i] = .{
                    .before = before,
                    .option = option.name,
                    .after = after,
                    .description = option.help,
                    .before_padding_len = len,
                    .default = default,
                };
                if (len > maxlen) {
                    maxlen = len;
                }
            }

            const padding = [_]u8{' '} ** (maxlen + 2);

            inline for (option_lines) |segments| {
                writer.writeByte('\n') catch {};
                writer.writeAll(segments.before) catch {};
                tty.setColor(writer, .bold) catch {};
                writer.writeAll(segments.option) catch {};
                tty.setColor(writer, .reset) catch {};
                writer.writeAll(segments.after) catch {};
                writer.writeAll(padding[0..(padding.len - segments.before_padding_len)]) catch {};
                writer.writeAll(segments.description) catch {};
                writer.writeAll(segments.default) catch {};
            }
            writer.writeByte('\n') catch {};

            std.process.exit(exit_code);
        }

        pub fn parse(iter: *std.process.ArgIterator) Args {
            const program_name = iter.next() orelse "program";

            return parse_inner(iter) catch {
                print_usage_and_exit(program_name, params.error_exit_code);
            };
        }
    };
}
