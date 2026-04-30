const std = @import("std");
const builtin = @import("builtin");
const interp_mod = @import("../interpreter.zig");
const Value = interp_mod.Value;
const EvalError = interp_mod.EvalError;
const Interpreter = interp_mod.Interpreter;
const ModuleMember = interp_mod.ModuleMember;

pub fn make(alloc: std.mem.Allocator) EvalError!Value {
    if (builtin.cpu.arch.isWasm()) {
        const members = try alloc.dupe(ModuleMember, &[_]ModuleMember{ });
        return Value{ .module = members };
    }

    const members = try alloc.dupe(ModuleMember, &[_]ModuleMember{
        .{ .name = "lytt", .value = .{ .builtin_fn = NetworkImpl.lytt } },
    });
    return Value{ .module = members };
}

const NetworkImpl = struct {
    pub fn lytt(args: []const Value, interp: *Interpreter) EvalError!Value {
        if (builtin.os.tag == .freestanding) return error.NetworkUnsupported;

        if (args.len != 2) return EvalError.TypeError;
        const port = try args[0].as_int();
        const callback = args[1];

        const address = std.net.Address.parseIp("127.0.0.1", @intCast(port)) catch return EvalError.RuntimeError;
        var server = address.listen(.{ .reuse_address = true }) catch return EvalError.RuntimeError;

        var buffer: [4096]u8 = undefined;

        while (true) {
            var conn = server.accept() catch continue;
            defer conn.stream.close();

            const bytes_read = conn.stream.read(&buffer) catch continue;
            if (bytes_read == 0) continue;

            var lines = std.mem.tokenizeSequence(u8, buffer[0..bytes_read], "\r\n");
            const first_line = lines.next() orelse continue;
            var parts = std.mem.tokenizeScalar(u8, first_line, ' ');

            const method = parts.next() orelse "GET";
            const path = parts.next() orelse "/";

            const method_val = Value{ .string = try interp.allocator.dupe(u8, method) };
            const path_val = Value{ .string = try interp.allocator.dupe(u8, path) };

            const callback_args = &[_]Value{ method_val, path_val };
            const respons_obj = interp.callFunction(callback, callback_args) catch {
                _ = conn.stream.writeAll("HTTP/1.1 500 Intern feil\r\n\r\n") catch {};
                continue;
            };

            if (respons_obj == .instance) {
                const innhald_val = respons_obj.get_field("innhald") catch continue;
                const type_val = respons_obj.get_field("type") catch continue;
                const status_val = respons_obj.get_field("status") catch continue;

                const innhald = innhald_val.string;
                const type_str = type_val.string;
                const status = status_val.integer;

                var header_buf: [512]u8 = undefined;
                const header = try std.fmt.bufPrint(&header_buf,
                    "HTTP/1.1 {d} OK\r\nContent-Type: {s}\r\nContent-Length: {d}\r\nConnection: close\r\n\r\n",
                    .{ status, type_str, innhald.len }
                );

                _ = conn.stream.writeAll(header) catch {};
                _ = conn.stream.writeAll(innhald) catch {};
            }
        }
    }
};
