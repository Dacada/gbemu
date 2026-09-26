const std = @import("std");
const builtin = @import("builtin");

const RegisterPackerDefinitionItem = struct {
    type: enum {
        unused,
        default,
    } = .default,
    name: ?[]const u8 = null,
    bits: comptime_int,
};

fn process_definition(definition: anytype) struct {type, []const RegisterPackerDefinitionItem} {
    const info = @typeInfo(@TypeOf(definition));
    if (info != .@"struct") {
        @compileError("invalid definition");
    }

    const strct = info.@"struct";

    if (!strct.is_tuple) {
        @compileError("invalid definition");
    }

    const len = strct.fields.len;
    var resolved_definition: [len]RegisterPackerDefinitionItem = undefined;
    var field_names: [len][]const u8 = undefined;
    var field_types: [len]type = undefined;
    var field_attrs: [len]builtin.Type.StructField.Attributes = undefined;

    inline for (strct.fields, 0..) |field, i| {
        const item =
    }

    return .{
        @Struct(.auto, null, field_names, field_types, field_attrs),
        resolved_definition,
    };
// +fn MaybeAllType(T: type) type {
// +    const info = @typeInfo(T);
// +
// +    if (info != .@"struct") {
// +        @compileError("input must be a tuple of TrackedValue");
// +    }
// +
// +    const strct = info.@"struct";
// +
// +    if (!strct.is_tuple) {
// +        @compileError("input must be a tuple of TrackedValue");
// +    }
// +
// +    var types: [strct.fields.len]type = undefined;
// +    inline for (strct.fields, 0..) |field, i| {
// +        const field_info = @typeInfo(field.type);
// +
// +        if (field_info != .@"struct") {
// +            @compileError("input must be a tuple of TrackedValue");
// +        }
// +
// +        if (!@hasDecl(field.type, "maybe")) {
// +            @compileError("input must be a tuple of TrackedValue");
// +        }
// +
// +        if (!@hasDecl(field.type, "InnerType")) {
// +            @compileError("input must be a tuple of TrackedValue");
// +        }
// +
// +        const inner_type = @field(field.type, "InnerType");
// +
// +        if (@TypeOf(inner_type) != type) {
// +            @compileError("input must be a tuple of TrackedValue");
// +        }
// +
// +        types[i] = inner_type;
// +    }
// +
// +    return @Tuple(&types);
// +}
// +
// +pub fn maybeAll(values: anytype) ?MaybeAllType(@TypeOf(values)) {
// +    var ret: MaybeAllType(@TypeOf(values)) = undefined;
// +
// +    inline for (values, 0..) |value, i| {
// +        ret[i] = value.maybe() orelse return null;
// +    }
// +
// +    return ret;
// +}
}

/// Definition is from LSB to MSB
pub fn RegisterPacker(InnerType: type, definition: anytype) type {
    // RegisterPacker(u8, .{
    //   .{ .type=.unused, .bits=2 },
    //   .{ .name="select_buttons", .bits=1},
    //   .{ .name="select_dpad", .bits=1 },
    //   .{ .name="button_state", bits=4 },
    // })

    const PackTupleType, const packing_info = process_definition(definition);

    return struct {
        pub fn pack(input: PackTupleType) InnerType {
            var output= 0;

            // This should be reversed in order to go from MSB to LSB instead
            for (packing_info) |info| {
                const val = if (info.type == .unused)
                    std.math.maxInt(std.meta.Int(.unsigned,info.bits))
                else
                    @field(input, info.name);

                const offset = info.bits;

                output |= val;
                output <<= offset;
            }

            return output;
        }

        pub fn unpack(input: InnerType) PackTupleType {
            var output: PackTupleType = undefined;

            for (packing_info) |info| {
                const mask = ~(1 << info.bits);
                const offset = info.bits;
                const name = info.name;

                input >>= offset;
                if (info.type == .default) {
                    @field(output, name) = input & mask;
                }
            }

            return output;
        }
    };
}
